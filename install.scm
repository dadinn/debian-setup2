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

(define* (configure-keyboard #:key layout variant model options)
  (setenv "DEBIAN_FRONTEND" "noninteractive")
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
  (system "setupcon")
  (utils:println "Verifying keyboard layout...")
  (let ((resp (readline "Please type \"Hello#123\" here: ")))
    (when (not (string= resp "Hello#123"))
      (utils:println "CHECK FAILED!!!")
      (utils:println "Falling back to manual keyboard configuration...")
      (system* "dpkg-reconfigure" "keyboard-configuration"))))

(define (read-sudouser)
  (utils:println "You can disable root user account by creating a sudo user instead.")
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


;; BOOTSTRAP

(define bootstrap-include-list
  (list "lsb-release" "info" "xz-utils" "dirmngr" "ca-certificates"))

(define (bootstrap target arch release mirror)
  (system* "apt" "install" "-y" "debootstrap")
  (utils:println "Bootstrapping Debian release" release "for archictecture" arch "...")
  (when (not (zero?
	(system* "debootstrap" "--arch" arch "--include"
		 (string-join bootstrap-include-list ",")
		 release target mirror)))
    (error "Failed to bootstrap the Debian system!" target arch release mirror)))

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

(define (main args)
  (let* ((options (utils:getopt-extra args options-spec))
	 (target (hash-ref options 'target))
	 (release (hash-ref options 'release))
	 (arch (hash-ref options 'arch))
	 (mirror (hash-ref options 'mirror))
	 (execute-only? (hash-ref options 'execute-only))
	 (help? (hash-ref options 'help))
	 (command (hash-ref options '() (list "/bin/sh"))))
    (cond
     (help?
      (display
       (string-append "USAGE:

" (basename (car args)) " [OPTION...] [COMMAND...]

Bootstraps Debian in the target directory, then chroots into it and executes COMMAND in the freshly bootstrapped Debian environment.

Valid options are:

" (utils:usage options-spec)))
      (newline))
     ((not (utils:directory? target))
      (error "Installation target directory doesn't exist!" target))
     ((not (utils:root-user?))
      (error "This script must be run as root!"))
     (else
      (when (not execute-only?)
	(bootstrap target arch release mirror))
      (map
       (lambda (dir)
	 (system* "mount" "--rbind" (utils:path "" dir) (utils:path target dir)))
       (list "dev" "sys" "proc" "run"))
      (apply system* "chroot" target command)
      (map
       (lambda (dir)
	 (system* "umount" "-Rlf" (utils:path target dir)))
       (list "dev" "sys" "proc" "run"))))))
