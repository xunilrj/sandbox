sudo apt-get update
sudo apt-get install libssl-dev
sudo apt-get install git fakeroot build-essential ncurses-dev xz-utils
sudo apt-get install kernel-package
wget https://cdn.kernel.org/pub/linux/kernel/v4.x/linux-4.4.tar.xz
tar xvf linux-4.4.tar.xz
cd linux-4.4
cp /boot/config-$(uname -r) .config
make menuconfig
make-kpkg clean
sudo make-kpkg --initrd --revision=1.0.NAS kernel_image kernel_headers
cd ..
sudo dpkg -i linux-image-4.4.0_1.0.NAS_amd64.deb
sudo dpkg -i linux-headers-4.4.0_1.0.NAS_amd64.deb

sudo reboot
