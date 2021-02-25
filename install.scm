#!/bin/sh
# -*- scheme -*-
exec guile -e main -s "$0" "$@"
!#

(add-to-load-path
 (dirname (current-filename)))

(use-modules
 ((common utils) #:prefix utils:)
 ((common deps) #:prefix deps:)
 ((ice-9 readline))
 ((ice-9 format))
 ((ice-9 regex) #:prefix regex:)
 ((ice-9 rdelim) #:prefix rdelim:)
 ((ice-9 popen) #:prefix popen:)
 ((ice-9 pretty-print) #:prefix pp:)
 ((srfi srfi-1) #:prefix srfi1:))

;;; CONFIGURE

(define (init-apt)
  (let ((aptconf-dir (utils:path "" "etc" "apt" "apt.conf.d")))
    (with-output-to-file (utils:path aptconf-dir "norecommends")
      (lambda ()
	(utils:println "APT::Get::Install-Recommends \"false\";")
	(utils:println "APT::Get::Install-Suggests \"false\";"))))
  (system* "apt" "update")
  (system* "apt" "full-upgrade" "-y"))

(define (init-network)
  (let ((interfaces-dir (utils:path "" "etc" "network" "interfaces.d")))
    (with-output-to-file (utils:path interfaces-dir "lo")
      (lambda ()
	(utils:println "auto" "lo")
	(utils:println "iface" "lo" "inet" "loopback")))
    (map
     (lambda (dev)
       (with-output-to-file (utils:path interfaces-dir dev)
	 (lambda ()
	   (utils:println "auto" dev)
	   (utils:println "iface" dev "inet" "dhcp"))))
     (filter
      (lambda (dev) (regex:string-match "en[a-z0-9]+" dev))
      (string-split (utils:system->string* "ls" (utils:path "" "sys" "class" "net")) #\newline)))))

(define (configure-locale locale)
  (system* "apt" "install" "-y" "locales")
  (let* ((locales-file (utils:path "" "etc" "locale.gen"))
	 (bak-file (string-append locales-file ".bak")))
    (when (not (file-exists? bak-file))
      (copy-file locales-file bak-file))
    (when (file-exists? bak-file)
      (call-with-input-file bak-file
	(lambda (input-port)
	  (let* ((content (rdelim:read-string input-port))
		 (pattern (string-append "# (" locale " .*)$")))
	    (call-with-output-file locales-file
	      (lambda (output-port)
		(regex:regexp-substitute/global
		 output-port pattern content
		 'pre 1 'post))))))
      (delete-file bak-file)))
  (system "locale-gen"))

(define (configure-timezone timezone)
  (putenv "DEBIAN_FRONTEND=noninteractive")
  (system* "apt" "install" "-y" "tzdata")
  (unsetenv "DEBIAN_FRONTEND")
  (utils:println "Configuring timezone:" timezone "...")
  (let ((zone-file (utils:path "" "usr" "share" "zoneinfo" timezone))
	(localtime-file (utils:path "" "etc" "localtime")))
    (cond
     ((file-exists? zone-file)
      (when (file-exists? localtime-file)
	(delete-file localtime-file))
      (symlink zone-file localtime-file)
      (system* "dpkg-reconfigure" "-f" "noninteractive" "tzdata"))
     (else
      (error "Zonefile does not exist!" zone-file)))))

(define* (configure-keyboard #:key layout variant model options)
  (putenv "DEBIAN_FRONTEND=noninteractive")
  (system* "apt" "install" "-y" "console-setup")
  (unsetenv "DEBIAN_FRONTEND")
  (with-output-to-file (utils:path "" "etc" "default" "keyboard")
    (lambda ()
      (display (string-append "XKBLAYOUT=" layout))
      (newline)
      (display (string-append "XKBVARIANT=" variant))
      (newline)
      (display (string-append "XKBOPTIONS=" options))
      (newline)
      (display (string-append "XKBMODEL=" model))
      (newline)
      (display "BACKSPACE=guess")
      (newline)))
  (utils:println "Verifying keyboard layout...")
  (system "setupcon")
  (let ((resp (readline "Please type \"Hello#123\" here: ")))
    (when (not (string= resp "Hello#123"))
      (utils:println "CHECK FAILED!!!")
      (utils:println "Falling back to manual keyboard configuration...")
      (system* "dpkg-reconfigure" "keyboard-configuration"))))

(define (read-sudouser)
  (utils:println "You can disable root user acount by creating a sudo user instead.")
  (readline "Type a name for sudo user (or leave it empty to keep the root account enabled): "))

(define (init-sudouser sudouser)
  (let ((username (or sudouser (read-sudouser))))
    (cond
     ((not (string-null? username))
      (system* "apt" "install" "-y" "sudo")
      (system* "useradd" "-m" "-G" "sudo" username "-s" "/bin/bash")
      (utils:println "Set password for sudo user" username "!")
      (while (not (zero? (system* "passwd" username)))
	(utils:println "Passwords don't match! Please try again!"))
      (system* "passwd" "-l" "root")
      (system* "usermod" "-s" "/sbin/nologin" "root"))
     (else
      (utils:println "Set password for root user!")
      (while (not (zero? (system "passwd")))
	(utils:println "Passwords don't match! Please try again!"))))))

(define* (configure-grub modules #:key zpool rootfs)
  (with-output-to-file (utils:path "" "etc" "default" "grub")
    (lambda ()
      (display "GRUB_CMDLINE_LINUX_DEFAULT=\"quiet\"")
      (newline)
      (display "GRUB_TERMINAL=\"console\"")
      (newline)
      (display (string-append "GRUB_PRELOAD_MODULES=\"" (string-join modules ",") "\""))
      (newline)
      (when (member "cryptodisk" modules)
	(display "GRUB_CRYPTODISK_ENABLE=y")
	(newline))
      (when (member "zfs" modules)
	(display (string-append "GRUB_CMDLINE_LINUX=root=ZFS=" zpool "/" rootfs))
	(newline)))))

(define* (install-grub bootdev grub-modules #:key uefiboot zpool rootfs)
  (putenv "DEBIAN_FRONTEND=noninteractive")
  (cond
   (uefiboot
    (system* "apt" "install" "-y" "grub-efi" "xz-utils")
    (unsetenv "DEBIAN_FRONTEND")
    (configure-grub
     grub-modules
     #:zpool zpool
     #:rootfs rootfs)
    (system*
     "grub-install"
     "--target=x86_64-efi"
     "--efi-directory=/boot/efi"
     "--bootloader-id=debian"
     "--compress=xz"
     "--recheck"
     bootdev))
   (else
    (system* "apt" "install" "-y" "grub-pc")
    (unsetenv "DEBIAN_FRONTEND")
    (configure-grub
     grub-modules
     #:zpool zpool)
    (system* "grub-install" bootdev))))

(define (install-kernel-image-zfs arch)
  (let ((release (deps:read-debian-version))
	(package (string-append "linux-image-" arch)))
    (cond
     ((= 8 release)
      (system* "apt" "install" "-y" "-t" "jessie-backports" package))
     ((= 10 release)
      (system* "apt" "install" "-y" "-t" "buster-backports" package))
     ((<= 9 release)
      (system* "apt" "install" "-y" package)))))

;; BOOTSTRAP

(define bootstrap-include-list
  (list "lsb-release" "info" "xz-utils" "dirmngr" "ca-certificates"))

(define (bootstrap target arch release mirror)
  (system* "apt" "install" "-y" "debootstrap")
  (utils:println "Bootstrapping Debian release" release "for archictecture" arch "...")
  (when (not (zero?
	(system* "debootstrap" "--arch" arch "--include"
		 (string-join bootstrap-include-list ",")
		 release target)))
    (error "Failed to bootstrap the Debian system!" target arch release mirror)))

;;; MAIN FUNCTION

(define archictecture-list
  (list "amd64" "arm64"  "armel" "armhf" "i368" "mips" "mips64el" "mipsel" "powerpc" "ppc64el" "s390x"))

(define options-spec
  `((target
     (description
      "Installation target as root directory")
     (single-char #\t)
     (default "/mnt/instroot")
     (value-arg "path")
     (value #t))
    (release
     (description
      "Debian release to install")
     (single-char #\r)
     (default "buster")
     (value-arg "name")
     (value #t))
    (arch
     (description
      ,(string-append
	"The target architecture of the new system. Has to be of either: "
	(string-join archictecture-list ", ") "."))
     (single-char #\a)
     (predicate ,(lambda (arch) (member arch archictecture-list)))
     (default "amd64")
     (value-arg "arch")
     (value #t))
    (mirror
     (description
      "Debian mirror URL to bootstrap from. See the list of mirrors at http://www.debian.org/mirror/list")
     (single-char #\m)
     (default "ftp.uk.debian.org/debian/")
     (value-arg "url")
     (value #t))
    (locale
     (single-char #\l)
     (description "Locale of the new system")
     (value #t)
     (value-arg "locale")
     (default "en_US.UTF-8"))
    (keymap
     (single-char #\k)
     (description "Default keymap of the new system")
     (default "us:dvorak")
     (predicate ,(lambda (keymap) (regex:string-match "[a-z]+:[a-z]+" keymap)))
     (value-arg "keymap")
     (value #t))
    (timezone
     (single-char #\t)
     (description "Timezone of the new system")
     (default "Europe/London")
     (value-arg "timezone")
     (value #t))
    (hostname
     (single-char #\n)
     (description "Hostname of the new system. Should conform to RFC 1123.")
     (value-arg "hostname")
     (value #t))
    (sudouser
     (single-char #\s)
     (description "Name for the sudo user to be used instead of root")
     (value-arg "username")
     (value #t))
    (execute-only
     (description
      "Skip bootstrapping new system, and only execute COMMAND in chroot environment")
     (single-char #\X))
    (help
     (description
      "This usage help...")
     (single-char #\h))))

(define pseudofs-dirs
  (list "dev" "dev/pts" "sys" "proc" "run"))

(define (main args)
  (let* ((options (utils:getopt-extra args options-spec))
	 (target (hash-ref options 'target))
	 (release (hash-ref options 'release))
	 (arch (hash-ref options 'arch))
	 (mirror (hash-ref options 'mirror))
	 (locale (hash-ref options 'locale))
	 (keymap (hash-ref options 'keymap))
	 (keymap-parts (string-split keymap #\:))
	 (layout (car keymap-parts))
	 (variant (cadr keymap-parts))
	 (timezone (hash-ref options 'timezone))
	 (hostname (hash-ref options 'hostname))
	 (sudouser (hash-ref options 'sudouser))
	 (execute-only? (hash-ref options 'execute-only))
	 (help? (hash-ref options 'help)))
    (cond
     (help?
      (utils:println
       (string-append "USAGE:

" (basename (car args)) " [OPTION...]

Bootstraps Debian in the target directory, and then configures the new system under a chroot environment.

Valid options are:
"))
      (utils:println (utils:usage options-spec)))
     ((not (utils:root-user?))
      (error "This script must be run as root!"))
     ((not (utils:directory? target))
      (error "Installation target directory doesn't exist!" target))
     ((not hostname)
      (error "Hostname must be specified for the new system!"))
     (else
      (when (not execute-only?)
	(bootstrap target arch release mirror))
      (map
       (lambda (dir)
	 (let ((target-path (utils:path target dir)))
	   (when (not (file-exists? target-path)) (mkdir target-path))
	   (system* "mount" "--rbind" (utils:path "" dir) target-path)))
       pseudofs-dirs)
      (let* ((config-file (utils:path target utils:config-filename))
	     (config (utils:read-config config-file))
	     (rootdev (hash-ref config 'rootdev))
	     (luks-v2? (hash-ref config 'luksv2))
	     (bootdev (hash-ref config 'bootdev rootdev))
	     (uefiboot (hash-ref config 'uefiboot))
	     (swapsize (hash-ref config 'swapsize))
	     (swapfiles (hash-ref config 'swapfiles))
	     (swapfiles (and swapfiles (string->number swapfiles)))
	     (zpool (hash-ref config 'zpool))
	     (rootfs (hash-ref config 'rootfs))
	     (grub-module-store (make-hash-table 1))
	     (get-grub-modules
	      (lambda () (hash-ref grub-module-store #:value '())))
	     (add-grub-module
	      (lambda (module)
		(let ((curr (get-grub-modules)))
		  (hash-set! grub-module-store #:value (cons module curr)))))
	     (pid (primitive-fork)))
	(cond
	 ((zero? pid)
	  (chroot target)
	  (cond
	   ((not bootdev)
	    (error "boot device has to be specified!"))
	   ((not (utils:block-device? bootdev))
	    (error "boot device has to be a block device!" bootdev))
	   ((and luks-v2? (<= 10 (or (deps:read-debian-version) 0)))
	    (error "LUKS format version 2 is only supported in Debian Buster or later!"))
	   (else
	    (init-apt)
	    (init-network)
	    (configure-locale locale)
	    (configure-timezone timezone)
	    (configure-keyboard
	     #:layout layout
	     #:variant variant
	     #:model "pc105"
	     #:options (or options "ctrl:nocaps"))
	    (init-sudouser sudouser)
	    (when rootdev
	      (system* "apt" "install" "-y" "cryptsetup")
	      (add-grub-module "cryptodisk"))
	    (cond
	     (zpool
	      (deps:install-deps-zfs)
	      (add-grub-module "zfs")
	      (system* "systemctl" "enable" "zfs-import-cache.service")
	      (system* "systemctl" "enable" "zfs-import-cache.target")
	      (system* "systemctl" "enable" "zfs-mount.service")
	      (system* "systemctl" "enable" "zfs-mount.target"))
	     ((zero? swapfiles)
	      (deps:install-deps-lvm)
	      (add-grub-module "lvm")
	      (let* ((lvm-dir (utils:path "" "etc" "lvm"))
		     (lvm-file (utils:path lvm-dir "lvm.conf"))
		     (lvmbak-file (string-append lvm-file ".bak")))
		(utils:move-file lvm-file lvmbak-file)
		(system* "sed" "-ire" "s|(multipath_component_detection =) [0-9]+|\\1 0|" lvm-file)
		(system* "sed" "-ire" "s|(md_component_detection =) [0-9]+|\\1 0|" lvm-file)
		(system* "sed" "-ire" "s|(udev_sync =) [0-9]+|\\1 0|" lvm-file)
		(system* "sed" "-ire" "s|(udev_rules =) [0-9]+|\\1 0|" lvm-file))))
	    (cond
	     (zpool
	      (install-kernel-image-zfs arch)
	      (install-grub
	       bootdev (get-grub-modules)
	       #:uefiboot uefiboot
	       #:zpool zpool
	       #:rootfs rootfs))
	     (else
	      (system* "apt" "install" "-y" (string-append "linux-image-" arch))
	      (install-grub
	       bootdev (get-grub-modules)
	       #:uefiboot uefiboot
	       #:zpool zpool
	       #:rootfs rootfs)))))
	  (primitive-exit 0))
	 (else
	  (waitpid pid)
	  (map
	   (lambda (dir)
	     (system* "umount" "-Rlf" (utils:path target dir)))
	   (reverse pseudofs-dirs)))))))))
