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

(define (bootstrap target arch release mirror)
  (system* "apt" "install" "-y" "debootstrap")
  (utils:println "Bootstrapping Debian release" release "for archictecture" arch "...")
  (when (not (zero? (system* "debootstrap" "--arch" arch "--include" "lsb-release,info,xz-utils,dirmngr,ca-certificates" release target mirror)))
    (error "Failed to bootstrap the Debian system!" target arch release mirror)))

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
      "The target architecture of the new system. Has to be of either: amd64, arm64, armel, armhf, i368, mips, mips64el, mipsel, powerpc, ppc64el, s390x")
     (single-char #\a)
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
      (utils:println
       (string-append "USAGE:

" (basename (car args)) " [OPTION...] [COMMAND...]

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
      (let* ((current-dir (dirname (current-filename)))
	     (conf-file "debconf.scm")
	     (target-conf-file (utils:path target conf-file)))
	(copy-file (utils:path current-dir conf-file) target-conf-file)
	(chmod target-conf-file #o755)
	(map
	 (lambda (dir)
	   (system* "mount" "--rbind" (utils:path "" dir) (utils:path target dir)))
	 (list "dev" "sys" "proc" "run"))
	(apply system* "chroot" target command)
	(map
	 (lambda (dir)
	   (system* "umount" "-Rlf" (utils:path target dir)))
	 (list "dev" "sys" "proc" "run")))))))
