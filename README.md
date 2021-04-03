# debian-setup

Scripts to bootstrap Debian system on an initialized root filesystem structure. It is expected that the root filesystem structure be initialized using the **init-instroot** script.

It is expected that the filesystem structure contains `/etc/fstab`, `/etc/crypttab` entries, and `/CONFIG_VARS.sh` with environmental variables used to initialise the structure.

## install.scm

Bootstraps Debian system under target directory (default `/mnt/instroot`), mounts `dev`, `sys`, `run` filesystems, and configures the freshly bootstrapped system under chroot environment.

### Example

To configure a bootable system under chroot, setting hostname, and creating a sudo enabled user (root user will be disabled), run the following command:

	./install.scm -n besenczy -s dadinn

This also installs all the necessary packages for LVM, LUKS, ZFS, Grub, and kernel images, and configures keyboard layouts and networks.
