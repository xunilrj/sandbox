#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/fs.h>
#include <asm/uaccess.h>

int init_module(void);
void cleanup_module(void);
static int device_open(struct inode *, struct file *);
static int device_release(struct inode *, struct file *);
static ssize_t device_read(struct file *, char *, size_t, loff_t *);
static ssize_t device_write(struct file *, const char *, size_t, loff_t *);
#define SUCCESS 0
#define DEVICE_NAME "echodevice"

static int Major;

#define BUF_LEN 100
static char msg[BUF_LEN];
static char *msg_Ptr;
static struct file_operations fops = {
    .read = device_read,
    .write = device_write,
    .open = device_open,
    .release = device_release
};

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

void cleanup_module(void)
{
    unregister_chrdev(Major, DEVICE_NAME);
}

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