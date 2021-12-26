# debian-setup

Script to bootstrap Debian system on an initialized root filesystem structure.

It is expected that the root filesystem structure be initialized using the **init-instroot** script.

Also, the filesystem structure has to contain the so generated `/etc/fstab`, `/etc/crypttab` entries, and `/INSTROOT_VARS.scm` with the options used to initialise the structure.

## install.scm

Bootstraps Debian system under target directory (default `/mnt/instroot`), mounts `dev`, `sys`, `proc`, `run` filesystems, and configures a bootable new system under a chroot environment.

Supports unattended execution by providing sudo-user password as command-line argument.

### Examples

To configure a bootable system under chroot, set the hostname, and create a sudo-enabled user, run the following command:

	./install.scm -n host.example.com -u admin

This installs all the necessary packages for LVM, LUKS, ZFS, GRUB, and kernel images, and configures specified keyboard layout, and basic network interfaces.

It is possible to run the same command in unattended mode, where all prompts are answered, and sudo user or root password can be set as a command line argument:

	./install.scm -A -n hostname -u user -p 'Password123!'

Alternatively, it is possible to run the installation in multiple steps:

    # bootstrap Debian only
    ./install.scm -B

    # then configure the new system
    ./install.scm -n hostname -u user -C

    # then finalize the installation (prepares new system for reboot)
    ./install.scm -D

    # then finally reboot the new system manually
    reboot 0

Alternatively, since the unattended mode only toggles specific options, it is possible to un-toggle those options.

For example the exact same end-result as the 3-step execution before can be achieved this way:

    # bootstap and configure Debian, and leave everything mounted for inspecting the installation manually
    ./install.scm -n hostname -u user -A --finalise

    # once finished with inspecting the installation, execute finishing steps
    ./install.scm -D

    # then reboot the new system manually
    reboot 0

The benefit of running installation in multiple steps is to make it possible to backup/snapshot in-between the individual steps, so as to save network bandwidth and processing while testing and re-running steps inside a VM.
