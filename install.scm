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
  (readline "Type a name for sudo user (or leave empty to keep the root account enabled): "))

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

(define* (grub-modules #:key rootdev zpool swapfiles))


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
    (lang
     (description "Value to set LANG environment variable inside the chroot environment")
     (default "C.UTF-8")
     (value-arg "LANG")
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
  (list "dev" "sys" "proc" "run"))

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

Bootstraps Debian in target directory, then chroots into it and executes COMMAND in the freshly bootstrapped Debian environment.

Also places configuration script debconf.sh in the target directory to help with automating the configuration.

Valid options are:
"))
      (utils:println (utils:usage options-spec)))
     ((not (utils:root-user?))
      (error "This script must be run as root!"))
     ((not (utils:directory? target))
      (error "Installation target directory doesn't exist!" target))
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
	      (system* "apt" "install" "-y" "cryptsetup"))
	    (cond
	     (zpool
	      (deps:install-deps-zfs)
	      (system* "systemctl" "enable" "zfs-import-cache.service")
	      (system* "systemctl" "enable" "zfs-import-cache.target")
	      (system* "systemctl" "enable" "zfs-mount.service")
	      (system* "systemctl" "enable" "zfs-mount.target"))
	     ((zero? swapfiles)
	      (deps:install-deps-lvm)
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
	      (deps:install-deps-zfs)
	      (install-grub bootdev uefiboot arch grub-modules zpool rootfs))
	     (else
	      (system* "apt" "install" "-y" (string-append "linux-image-" arch))))))
	  (primitive-exit 0))
	 (else
	  (waitpid pid)
	  (map
	   (lambda (dir)
	     (system* "umount" "-Rlf" (utils:path target dir)))
	   pseudofs-dirs))))))))
