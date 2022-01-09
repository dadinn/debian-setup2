#!/bin/sh
# -*- scheme -*-
exec guile -e main -s "$0" "$@"
!#

(add-to-load-path
 (dirname (current-filename)))

(use-modules
 ((common utils) #:prefix utils:)
 ((common deps) #:prefix deps:)
 ((ice-9 readline) #:select (readline))
 ((ice-9 ftw) #:select (ftw))
 ((ice-9 regex) #:prefix regex:)
 ((ice-9 rdelim) #:prefix rdelim:)
 ((ice-9 popen) #:prefix popen:)
 ((ice-9 pretty-print) #:prefix pp:)
 ((srfi srfi-1) #:prefix srfi1:)
 ((web uri) #:select (string->uri uri-host)))

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
    (for-each
     (lambda (dev)
       (with-output-to-file (utils:path interfaces-dir dev)
	 (lambda ()
	   (utils:println "auto" dev)
	   (utils:println "iface" dev "inet" "dhcp"))))
     (filter
      (lambda (dev) (regex:string-match "en[a-z0-9]+" dev))
      (string-split (utils:system->string* "ls" (utils:path "" "sys" "class" "net")) #\newline)))))

(define (init-hostname hostname)
  (with-output-to-file (utils:path "" "etc" "hostname")
    (lambda ()
      (utils:println hostname))))

(define (configure-hosts hostname)
  (with-output-to-file (utils:path "" "etc" "hosts")
    (lambda ()
      (utils:println "127.0.0.1 localhost")
      (utils:println "127.0.1.1" hostname)
      (utils:println "::1 localhost"))))

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
  (setenv "DEBIAN_FRONTEND" "noninteractive")
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

(define* (configure-keyboard #:key layout variant model options skip-check?)
  (setenv "DEBIAN_FRONTEND" "noninteractive")
  (system* "apt" "install" "-y" "console-setup")
  (unsetenv "DEBIAN_FRONTEND")
  (with-output-to-file (utils:path "" "etc" "default" "keyboard")
    (lambda ()
      (display (string-append "XKBLAYOUT=" (or layout "us")))
      (newline)
      (display (string-append "XKBVARIANT=" (or variant "dvorak")))
      (newline)
      (display (string-append "XKBOPTIONS=" (or options "ctrl:nocaps")))
      (newline)
      (display (string-append "XKBMODEL=" (or model "pc105")))
      (newline)
      (display "BACKSPACE=guess")
      (newline)))
  (system "setupcon")
  (utils:println "Verifying keyboard layout...")
  (let ((resp
	 (if skip-check? "Hello#123"
	  (readline "Please type \"Hello#123\" here: "))))
    (when (not (string= resp "Hello#123"))
      (utils:println "CHECK FAILED!!!")
      (utils:println "Falling back to manual keyboard configuration...")
      (system* "dpkg-reconfigure" "keyboard-configuration"))))

(define (read-sudouser)
  (utils:println "You can disable root user account by creating a sudo user instead.")
  (readline "Type a name for sudo user (or leave it empty to keep the root account enabled): "))

(define* (set-password username #:optional password)
  (format #t "Setting password for user ~A..." username)
  (if password
   (let ((output-port (popen:open-pipe* OPEN_WRITE "passwd" "-q" username)))
     (format output-port "~A\n~A" password password)
     (popen:close-pipe output-port))
   (while (not (zero? (system* "passwd" username)))
     (utils:println "Passwords don't match! Please try again!"))))

(define* (init-sudouser sudouser #:key password skip-prompt?)
  (let ((username
	 (or sudouser
	  (if (not skip-prompt?)
	   (read-sudouser)
	   ""))))
    (cond
     ((not (string-null? username))
      (system* "apt" "install" "-y" "sudo")
      (system* "useradd" "-m" "-G" "sudo" username "-s" "/bin/bash")
      (set-password username password)
      (system* "passwd" "-l" "root")
      (system* "usermod" "-s" "/sbin/nologin" "root"))
     (else
      (set-password "root" password)))))

(define (update-lvm-config input-port output-port)
  "Update LVM configuration to disable udev synchronisation"
  (let* ((content (rdelim:read-string input-port))
	 (content
	  (regex:regexp-substitute/global
	   #f "(multipath_component_detection =) [0-9]+" content
	   'pre 1 " 0" 'post))
	 (content
	  (regex:regexp-substitute/global
	   #f "(md_component_detection =) [0-9]+" content
	   'pre 1 " 0" 'post))
	 (content
	  (regex:regexp-substitute/global
	   #f "(udev_sync =) [0-9]+" content
	   'pre 1 " 0" 'post)))
    (regex:regexp-substitute/global
     output-port "(udev_rules =) [0-9]+" content
     'pre 1 " 0" 'post)))

(define* (configure-grub modules #:key zpool zroot serial-only?)
  (with-output-to-file (utils:path "" "etc" "default" "grub")
    (lambda ()
      (display "GRUB_CMDLINE_LINUX_DEFAULT=\"quiet\"")
      (newline)
      (let ((cmdline-linux-args
	     (append
	      (if (not (and (member "zfs" modules) zpool zroot)) '()
	       (list (string-append "root=ZFS=" zpool "/" zroot)))
	      (if (not serial-only?) '()
	       (list "console=ttyS0,115200")))))
	(when (not (null? cmdline-linux-args))
	  (format #t "GRUB_CMDLINE_LINUX=\"~A\""
	   (string-join cmdline-linux-args " "))
	  (newline)))
      (when serial-only?
	(display "GRUB_TERMINAL=\"console serial\"")
	(newline))
      (display (string-append "GRUB_PRELOAD_MODULES=\"" (string-join modules ",") "\""))
      (newline)
      (when (member "cryptodisk" modules)
	(display "GRUB_CRYPTODISK_ENABLE=y")
	(newline)))))

(define (file-tree-missing? root-dir filename)
  (ftw root-dir (lambda (path info flag)
    (not (and (eqv? flag 'regular) (string= (basename path) filename))))))

(define* (install-kernel-and-grub arch bootdev grub-modules #:key uefiboot? zpool zroot serial-only?)
  ;; KERNEL
  (let ((release (deps:read-debian-version))
	(package (string-append "linux-image-" arch)))
    (cond
     ((and zpool (= 8 release))
      (system* "apt" "install" "-y" "-t" "jessie-backports" package))
     ((and zpool (= 10 release))
      (system* "apt" "install" "-y" "-t" "buster-backports" package))
     ((or (and zpool (<= 9 release)) (not zpool))
      (system* "apt" "install" "-y" package)))
    (when zpool
      (system* "apt" "install" "-y" "zfs-initramfs")))
  ;; GRUB
  (setenv "DEBIAN_FRONTEND" "noninteractive")
  (cond
   (uefiboot?
    (system* "apt" "install" "-y" "grub-efi" "xz-utils")
    (unsetenv "DEBIAN_FRONTEND")
    (configure-grub
     grub-modules
     #:zpool zpool
     #:zroot zroot
     #:serial-only?
     serial-only?)
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
     #:zpool zpool
     #:zroot zroot
     #:serial-only?
     serial-only?)
    (system* "grub-install" bootdev)))
  (system "update-grub")
  (when (and zpool (file-tree-missing? (utils:path "" "boot" "grub") "zfs.mod"))
    (error "Failed installing ZFS module for GRUB!")))

;; BOOTSTRAP

(define bootstrap-include-list
  (list "lsb-release" "info" "xz-utils" "dirmngr" "ca-certificates"))

(define* (bootstrap target arch release mirror #:optional no-check-gpg?)
  (system* "apt" "install" "-y" "debootstrap")
  (utils:println "Bootstrapping Debian release" release "for archictecture" arch "...")
  (let ((command
	 `("debootstrap" "--arch" ,arch "--include"
	   ,(string-join bootstrap-include-list ",")
	   ,@(if no-check-gpg? (list "--no-check-gpg") '())
	   ,release ,target ,mirror)))
    (when (not (zero? (apply system* command)))
      (error "Failed to bootstrap the Debian system!" target arch release mirror))))

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
     (default "http://deb.debian.org/debian")
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
    (serial-only
     (single-char #\S)
     (description
      "Disable graphics, and only configure serial console in Linux kernel and GRUB."))
    (hostname
     (single-char #\n)
     (description "Hostname of the new system. Should conform to RFC 1123.")
     (value-arg "hostname")
     (value #t))
    (password
     (single-char #\p)
     (description "Password for root, or sudouser (if specified).")
     (value-arg "PASSWORD")
     (value #t))
    (sudouser
     (single-char #\s)
     (description "Name for the sudo user to be used instead of root")
     (value-arg "username")
     (value #t))
    (skip-sudouser-prompt
     (description
      "When sudouser option is not specified, skip prompt asking for sudo username and automatically configure the root user password instead."))
    (accept-openzfs-license
     (description "Confirm OpenZFS License (CDDL) automatically:
https://github.com/openzfs/zfs/blob/master/LICENSE"))
    (finalise
     (description
      "Prepare the new system to be ready for reboot, by removing temporary files, unmounting filesystems, and executing finishing steps."))
    (unattended
     (description
      "Runs script in unattended mode. Requires password to be specified.
Toggles options skip-sudouser-prompt, accept-openzfs-license, finalise.")
     (single-char #\A))
    (bootstrap-only
     (description
      "Skip configuring bootstrapped installation, and only do the bootstrapping of the new system.")
     (single-char #\B))
    (configure-only
     (description
      "Skip bootstrapping new system, and only execute configuration steps in chroot environment.")
     (single-char #\C))
    (finalise-only
     (description
      "Skip bootstrapping or configuring installation, and only execute finishing steps.
Toggles the finalise option.")
     (single-char #\D))
    (help
     (description
      "This usage help...")
     (single-char #\h))))

(define pseudofs-dirs
  (list "dev" "sys" "proc" "run"))

(define (main args)
  (let* ((options (utils:getopt-extra args options-spec))
	 (target (hash-ref options 'target))
	 (config-file (utils:path target utils:config-filename))
	 (release (hash-ref options 'release))
	 (arch (hash-ref options 'arch))
	 (mirror (hash-ref options 'mirror))
	 (locale (hash-ref options 'locale))
	 (keymap (hash-ref options 'keymap))
	 (keymap-parts (string-split keymap #\:))
	 (keyboard-layout (car keymap-parts))
	 (keyboard-variant (cadr keymap-parts))
	 (timezone (hash-ref options 'timezone))
	 (serial-only? (hash-ref options 'serial-only))
	 (hostname (hash-ref options 'hostname))
	 (password (hash-ref options 'password))
	 (sudouser (hash-ref options 'sudouser))
	 (unattended? (hash-ref options 'unattended))
	 (skip-sudouser-prompt? (hash-ref options 'skip-sudouser-prompt))
	 (skip-sudouser-prompt? (not (equal? skip-sudouser-prompt? unattended?)))
	 (accept-openzfs-license? (hash-ref options 'accept-openzfs-license))
	 (accept-openzfs-license? (not (equal? accept-openzfs-license? unattended?)))
	 (finalise? (hash-ref options 'finalise))
	 (finalise? (not (equal? finalise? unattended?)))
	 (bootstrap-only? (hash-ref options 'bootstrap-only))
	 (configure-only? (hash-ref options 'configure-only))
	 (finalise-only? (hash-ref options 'finalise-only))
	 (finalise? (or finalise? finalise-only?))
	 (help? (hash-ref options 'help)))
    (cond
     (help?
      (display
       (string-append "USAGE:

" (basename (car args)) " [OPTION...]

Bootstraps Debian in the target directory, and then configures the new system under a chroot environment.

Valid options are:

" (utils:usage options-spec)))
      (newline)
      (newline))
     ((not (utils:directory? target))
      (error "Installation target directory doesn't exist!" target))
     ((not (file-exists? config-file))
      (error "Configuration file doesn't exist!" config-file))
     ((or (and bootstrap-only? configure-only?)
       (and bootstrap-only? finalise-only?)
       (and configure-only? finalise-only?))
      (error "The bootstrap-only, configure-only, and finalise-only options cannot be used together!"))
     ((and (not (or bootstrap-only? finalise-only?)) (not hostname))
      (error "Hostname must be specified to configure the new system!"))
     ((and unattended? (not password))
      (error "Password must be specified when using unattended mode!"))
     ((not (utils:root-user?))
      (error "This script must be run as root!"))
     (else
      (when (not (or configure-only? finalise-only?))
	(bootstrap target arch release mirror)
	(utils:println "FINISHED BOOTSTRAPPING NEW DEBIAN SYSTEM!"))
      (let* ((config (utils:read-config config-file))
	     (rootdev (hash-ref config 'rootdev))
	     (luks-v2? (hash-ref config 'luksv2))
	     (bootdev (hash-ref config 'bootdev rootdev))
	     (uefiboot? (hash-ref config 'uefiboot))
	     (swapfiles (hash-ref config 'swapfiles))
	     (swapfiles (and swapfiles (string->number swapfiles)))
	     (zpool (hash-ref config 'zpool))
	     (zroot (hash-ref config 'zroot))
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
	  (when (not (or bootstrap-only? finalise-only?))
	    (utils:println "Configuring new Debian system...")
	    (for-each
	     (lambda (dir)
	       (let ((target-path (utils:path target dir)))
		 (when (not (file-exists? target-path))
		   (mkdir target-path))
		 (when (not (zero? (system* "mountpoint" "-q" target-path)))
		   (system* "mount" "--rbind" (utils:path "" dir) target-path))))
	     pseudofs-dirs)
	    (chroot target)
	    (chdir "/")
	    (setenv "LANG" "C.UTF-8")
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
	      (init-hostname hostname)
	      (configure-hosts hostname)
	      (configure-locale locale)
	      (configure-timezone timezone)
	      (configure-keyboard
	       #:skip-check? password
	       #:layout keyboard-layout
	       #:variant keyboard-variant)
	      (init-sudouser sudouser
	       #:skip-prompt? skip-sudouser-prompt?
	       #:password password)
	      (when rootdev
		(system* "apt" "install" "-y" "cryptsetup")
		(add-grub-module "cryptodisk"))
	      (cond
	       (zpool
		(deps:install-deps-zfs accept-openzfs-license?)
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
		  (when (not (file-exists? lvmbak-file))
		    (copy-file lvm-file lvmbak-file))
		  (call-with-input-file lvmbak-file
		    (lambda (input-port)
		      (call-with-output-file lvm-file
			(lambda (output-port)
			  (update-lvm-config input-port output-port)))))
		  (delete-file lvmbak-file))))
	      (install-kernel-and-grub
	       arch bootdev (get-grub-modules)
	       #:uefiboot? uefiboot?
	       #:zpool zpool
	       #:zroot zroot
	       #:serial-only?
	       serial-only?)))
	    (when (equal? "localhost" (uri-host (string->uri mirror)))
	      (let* ((new-file "/etc/apt/sources.list")
		     (old-file (string-append new-file ".old")))
		(utils:move-file new-file old-file)
		(call-with-input-file old-file
		  (lambda (input-port)
		    (call-with-output-file new-file
		      (lambda (output-port)
			(let* ((pattern
				(make-regexp
				 "^deb ([^ ]+) ([^ ]+) (.+)$"
				 regexp/newline))
			       (result (rdelim:read-string input-port))
			       (result (regexp-exec pattern result))
			       (components (regex:match:substring result 3)))
			  (regex:regexp-substitute output-port result "#
# This system was installed using a locally hosted apt mirror.
# The below source entry was disabled at the end of the installation
# process. For information about how to configure apt package sources,
# see the sources.list(5) manual.

# " 0 'post)
			  (newline output-port)
			  (format output-port "deb http://deb.debian.org/debian ~A ~A\n"
				  release components)
			  (format output-port "deb-src http://deb.debian.org/debian ~A ~A\n"
				  release components)
			  (format output-port "deb http://security.debian.org/debian-security ~A ~A\n"
				  (string-append release "-security") components)
			  (format output-port "deb-src http://security.debian.org/debian-security ~A ~A\n"
				  (string-append release "-security") components)
			  (format output-port "deb http://deb.debian.org/debian ~A ~A\n"
				  (string-append release "-updates") components)
			  (format output-port "deb-src http://deb.debian.org/debian ~A ~A\n"
				  (string-append release "-updates") components))))))
		(delete-file old-file)))
	    (utils:println "FINISHED CONFIGURING NEW DEBIAN SYSTEM!"))
	  (primitive-exit 0))
	 (else
	  (waitpid pid)
	  (let ((resp
		 (cond
		  (bootstrap-only? "N")
		  (finalise? "Y")
		  (unattended? "N")
		  (else (readline "Ready to finalise installation? [Y/n]")))))
	    (cond
	     ((regex:string-match "[nN]" resp)
	      (utils:println "Skipped executing finishing steps!"))
	     (else
	      (delete-file config-file)
	      (utils:println "Removed" config-file "!")
	      (utils:println "Unmounting installation directories...")
	      (for-each
	       (lambda (dir)
		 (system* "umount" "-Rlf" (utils:path target dir)))
	       (reverse pseudofs-dirs))
	      (when uefiboot?
		(system* "umount" (utils:path target "boot" "efi")))
	      (system* "umount" (utils:path target "boot"))
	      (cond
	       (zpool
		(system* "zfs" "umount" "-a")
		(let ((root-dataset (utils:path zpool zroot)))
		  (system* "zfs" "set" "mountpoint=/" root-dataset)
		  (system* "zfs" "snapshot" (string-append root-dataset "@install")))
		(system* "zpool" "export" zpool))
	       (else (system* "umount" target)))
	      (utils:println "FINISHED INSTALLING NEW DEBIAN SYSTEM!")))))))))))
