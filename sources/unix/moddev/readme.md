# Developing linux modules/devices/drivers

Pay special attention for special characters (tab, space, dash) when copying the code. Sometimes a simple control+c e control-v does not work.

## First step

Although it is not necessary to compile the kernel yourself to develop modules, I think it is a very good first step to introduce you to the linux module development environment.

The compilation process today is much easier that once was. We will follow a very easy set of commands.

But first let us start a virtual machine so we do not accidentally destroy our computer. For this we will use vagrant just because it is very easy to use it. 

On Ubuntu:

    > sudo apt-get install virtualbox
    > sudo apt-get install vagrant

On Windows

    > iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))    
    > choco install virtualbox -y
    > choco install vagrant -y

From now on, both windows and linux, have the same script:

    > cd ~
    > mkdir vagrantmachines
    > cd vagrantmachines
    > mkdir ubuntu.trusty64
    > cd ubuntu.trusty64
    > vagrant init ubuntu/trusty64
    > vagrant up
    > vagrant ssh

Please note that the majority of the script is exactly the same for Windows and Linux.

Once inside the machine we need to prepare the machine, download the source code, compile the kernel, install it and reboot. All of this can be easily be done as:

Preparing the machine:

    > sudo apt-get update
    > sudo apt-get install libssl-dev
    > sudo apt-get install git
    > sudo apt-get install fakeroot
    > sudo apt-get install build-essential
    > sudo apt-get install ncurses-dev
    > sudo apt-get install xz-utils
    > sudo apt-get install kernel-package

Downloading the source code:

    > wget https://cdn.kernel.org/pub/linux/kernel/v4.x/linux-4.4.tar.xz
    > tar xvf linux-4.4.tar.xz

Compiling the kernel (this can take a lot of time). "make menuconfig" will open a menu to allow you to customize the kernel compilation. When in doubt, just save it without changing anything.

    > cd linux-4.4
    > cp /boot/config-$(uname -r) .config
    > make menuconfig
    > make-kpkg clean
    > sudo make-kpkg --initrd --revision=1.0.NAS kernel_image kernel_headers

Installing the new kernel

    > cd ..
    > sudo dpkg -i linux-image-4.4.0_1.0.NAS_amd64.deb
    > sudo dpkg -i linux-headers-4.4.0_1.0.NAS_amd64.deb

Reboot:

    > sudo reboot

Test it:

    > uname -a
    
And you should see the kernel version that you have just compiled.

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

    > sudo make
    > sudo insmod ./firstmodule.ko
    > sudo rmmod firstmodule
    > sudo cat /var/log/syslog | grep FirstModule

Congratulations! We have developed your first linux module!

## Simple echo device

Now we are going to create a very simple char device, one that communicates characters. This device will contain a memory of 100 bytes and will allow us to store whatever we want in it.

### Registering the device

The first step is to register the device:

    int init_module(void)
    {
        Major = register_chrdev(0, DEVICE_NAME, &fops);
        if (Major < 0) {
            printk(KERN_ALERT "%s: Registering device failed with %d\n", DEVICE_NAME, Major);
            return Major;
        }
        printk(KERN_INFO "%s: mknod /dev/%s c %d 0", DEVICE_NAME, DEVICE_NAME, Major);
        return SUCCESS;
    }

With this code we will be able to mount the device like this:

    > sudo mknod /dev/echodevice c 250 0
    > ls /dev/echodevice -la

and see a result like this:

    > ls /dev/echodevice -la
    crwxrwxrwx 1 root root 250, 0 Aug 27 13:56 /dev/echodevice         
We still cannot nor read nor write to the device. But it is ready. Ignore the "&fops" for now.

Our next step is to unregister the device when the module is unloaded.

    void cleanup_module(void)
    {
        unregister_chrdev(Major, DEVICE_NAME);
    }

Old guides will guide you to check the return. It is no longer the case necessary. Since this commit: https://github.com/torvalds/linux/commit/e53252d97e670a38b1d2e9723b48077bba11ddda

One can see that now "unregister_chrdev" returns void.

Now we can:

    > sudo rmmod /dev/echodevice
    > cat /dev/echodevice
    cat: /dev/echodevice: No such device or address
    > ls /dev/echodevice -la
    crwxrwxrwx 1 root root 250, 0 Aug 27 13:56 /dev/echodevice
    > sudo rm /dev/echodevice
    > ls /dev/echodevice -la
    ls: cannot access /dev/echodevice: No such file or directory

Unfortunately, even after removing the module, the driver still exists although it is not functional. We can deal with this later. 

### Accepting write

Now we are ready to allow the user to write the information that he wants at our device.

    static ssize_t device_write(struct file *filp, const char *buffer, size_t length, loff_t * offset)
    {
        int i = 0;

        for (i = 0; i < length && i < BUF_LEN; ++i)
        {
            get_user(msg[i], buffer + i);
        }
        
        if(i < BUF_LEN) msg[i] = 0;
        else msg[BUF_LEN-1] = 0;

        msg_Ptr = msg;

        return i;
    }                

This code is very simple:  

    * First we copy the buffer from "user space" to the "kernel space" one character at a time using the macro "get_user";
    * Second, we append (or truncate) the last character to be the zero character;
    * Third, we return how many characters were written.

Although a simple example we are already facing a very complex problem in device programming. We need to move memory between kernel space (where we are) and user space (process memory mapping).

![](https://www.ibm.com/developerworks/library/l-kernel-memory-access/image004.gif)

And for this task we have four functions. Here we are using the simplest, "get_user" that will "download" a byte from "user space".

Now we are ready to store whatever-100-bytes we want at the device.

    > sudo chmod 777 /dev/echodevice
    > echo "hello word" > /dev/echodevice 

### Accepting read

Read is as simple as write in our case. We just need the use "put_user" macro to "upload" each byte back to the "user space".

    static ssize_t device_read(struct file *filp, char *buffer, size_t length, loff_t * offset)
    {
        int bytes_read = 0;
        
        if (*msg_Ptr == 0) return 0;
        msg_Ptr = msg;

        while (length && *msg_Ptr) {
            put_user(*(msg_Ptr++), buffer++);
            length--;
            bytes_read++;
        }

        return bytes_read;
    }

And now we can:

    > cat /dev/echodevice
    hello word

### Wiring the callbacks

Before finishing, we need to go back to the "&fops" of the device registration method. In that call we tell the kernel that we are registering a new device. And the "fops" variable is used to specify the callbacks used when interact with the kernel.

    static int device_open(struct inode *, struct file *);
    static int device_release(struct inode *, struct file *);
    static ssize_t device_read(struct file *, char *, size_t, loff_t *);
    static ssize_t device_write(struct file *, const char *, size_t, loff_t *);
    static struct file_operations fops = {
        .read = device_read,
        .write = device_write,
        .open = device_open,
        .release = device_release
    };

This code fragment is self-explanatory.

### Reference Count

Every time a process open and close a connection to your device the kernel will call two callback that will allow you to increment the reference count to your devices.

"try_module_get" will increment the reference and "module_put" will decrement the reference. This will allow the kernel to know that it is safe the allow a "rmmod" and unload your module.

If you forget to release your device, the kernel will never allow you to release your device. The solution? Restart. I bet you thought that this was a Windows-only problem!  

    static int device_open(struct inode *inode, struct file *file)
    {
        try_module_get(THIS_MODULE);
        return SUCCESS;
    }

    static int device_release(struct inode *inode, struct file *file)
    {
        module_put(THIS_MODULE);
        return SUCCESS;
    }

You can see how many references exists currently calling:

    > sudo cat /proc/modules | grep echodevice
    echodevice 16384 0 - Live 0xffffffffa00e2000 (POE)                                      
The third value, zero in this case, means no connections.

# Finish

Congratulations! You have completed your first device. See the file echodevice.c fot the complete code.
