# Developing linux modules

## First step

Although it is not necessary to compile the kernel yourself to develop modules, I think it is a very good first step to introduce you to the linux module development environment.

The compilation process today is much easier that once was. We will follow a very easy set of commands.

But first let us start a virtual machine so we do not accidentally destroy our computer. For this we will use vagrant just because it is very easy to use it. 

On Ubuntu:

    sudo apt-get install virtualbox
    sudo apt-get install vagrant
    cd ~
    mkdir vagrantmachines
    cd vagrantmachines
    mkdir ubuntu.trusty64
    cd ubuntu.trusty64
    vagrant init ubuntu/trusty64
    vagrant up
    vagrant ssh

On Windows

    iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))    
    choco install virtualbox -y
    choco install vagrant -y
    cd ~
    mkdir vagrantmachines
    cd vagrantmachines
    mkdir ubuntu.trusty64
    cd ubuntu.trusty64
    vagrant init ubuntu/trusty64
    vagrant up
    vagrant ssh

Please note that the majority of the script is exactly the same for Windows and Linux.

Once inside the machine we need to prepare the machine, download the source code, compile the kernel, install it and reboot. All of this can be easily be done as:

Preparing the machine:

    sudo apt-get update
    sudo apt-get install libssl-dev
    sudo apt-get install git
    sudo apt-get install fakeroot
    sudo apt-get install build-essential
    sudo apt-get install ncurses-dev
    sudo apt-get install xz-utils
    sudo apt-get install kernel-package

Downloading the source code:

    wget https://cdn.kernel.org/pub/linux/kernel/v4.x/linux-4.4.tar.xz
    tar xvf linux-4.4.tar.xz

Compiling the kernel:

    cd linux-4.4
    cp /boot/config-$(uname -r) .config
    make menuconfig
    make-kpkg clean
    sudo make-kpkg --initrd --revision=1.0.NAS kernel_image kernel_headers

Installing the new kernel

    cd ..
    sudo dpkg -i linux-image-4.4.0_1.0.NAS_amd64.deb
    sudo dpkg -i linux-headers-4.4.0_1.0.NAS_amd64.deb

Reboot:

    sudo reboot

Test it:

    uname -a

## Second Step

Now we can actually develop our first linux module. This first module will only print a start and a end message. The code is self-explanatory.

firstmodule.c

    #include <linux/module.h>
    #include <linux/kernel.h>

    int init_module(void)
    {
        printk(KERN_INFO "FirstModule - Init Module.\n");
        return 0;
    }

    void cleanup_module(void)
    {
        printk(KERN_INFO "FirstModule - Cleanup.\n");
    }

To compile it, we need a simple Makefile:

    obj−m += firstmodule.o
    all:
        make -C /lib/modules/$(shell uname -r)/build M=$(PWD) modules
    clean:
        make -C /lib/modules/$(shell uname -r)/build M=$(PWD) clean

To test this new module you just need to run:

    sudo insmod ./firstmodule.ko
    sudo rmmod firstmodule
    sudo cat /var/log/syslog | grep FirstModule

Congratulations! We have developed your first linux module!
