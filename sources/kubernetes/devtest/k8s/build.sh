#! /bin/bash

mkdir root -p
pushd root

ln -s /realroot/bin bin -f
ln -s /realroot/boot boot -f
ln -s /realroot/home home -f
ln -s /realroot/init init -f
ln -s /realroot/lib lib -f
ln -s /realroot/lib32 lib32 -f
ln -s /realroot/lib64 lib64 -f
ln -s /realroot/libx32 libx32 -f
ln -s /realroot/media media -f
ln -s /realroot/mnt mnt -f
ln -s /realroot/opt opt -f
ln -s /realroot/root root -f
ln -s /realroot/run run -f
ln -s /realroot/sbin sbin -f
ln -s /realroot/snap snap -f
ln -s /realroot/srv srv -f
ln -s /realroot/tmp tmp -f
ln -s /realroot/usr usr -f
ln -s /realroot/var var -f

rm etc -rf
mkdir etc -p
cat /etc/environment > etc/environment
echo "root:x:0:0:root:/root:/bin/bash" > etc/passwd
echo "Defaults    env_reset" > etc/sudoers
echo "Defaults    mail_badpass" >> etc/sudoers
echo "Defaults    secure_path='/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/snap/bin'" >> etc/sudoers
echo "root    ALL=(ALL) ALL" >> etc/sudoers
echo "%sudo ALL=(ALL) ALL" >> etc/sudoers
cat /etc/bash.bashrc > /etc/bash.bashrc

tar -cf ../root.tar .

popd