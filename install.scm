#!/bin/sh
# -*- scheme -*-
exec guile -e main -s "$0" "$@"
!#

(add-to-load-path
 (dirname (current-filename)))

(use-modules
 ((common utils) #:prefix utils:)
 ((common deps) #:prefix deps:)
 ((ice-9 regex) #:prefix regex:)
 ((ice-9 rdelim) #:prefix rdelim:)
 ((ice-9 popen) #:prefix popen:)
 ((ice-9 pretty-print) #:prefix pp:)
 ((srfi srfi-1) #:prefix srfi1:))

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
