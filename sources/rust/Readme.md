

https://danielkeep.github.io/tlborm/book/pim-README.html  
https://nick.groenen.me/posts/rust-error-handling  
https://nickwilcox.github.io/blog/autovec  

# Setting up VIM

Install: 

Choose a font
https://www.nerdfonts.com/font-downloads

For Windows Terminal change your font:

```
 "profiles": {
        "defaults": {
            "fontFace": "FiraMono NF"
        },
```

https://github.com/neovim/neovim/wiki/Installing-Neovim
https://github.com/junegunn/vim-plug
https://github.com/autozimu/LanguageClient-neovim
https://github.com/junegunn/fzf
https://github.com/neoclide/coc.nvim
https://github.com/neoclide/coc.nvim/wiki/Using-coc-extensions
https://github.com/fannheyward/coc-rust-analyzer
https://github.com/ctrlpvim/ctrlp.vim
https://github.com/numToStr/FTerm.nvim
https://github.com/t9md/vim-choosewin.git

Or

```
curl -LO https://github.com/neovim/neovim/releases/latest/download/nvim.appimage
chmod u+x nvim.appimage
mv ./nvim.appimage /usr/bin/nvim

sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

apt install python3-pip
pip3 install --user pynvim

git clone https://github.com/ctrlpvim/ctrlp.vim.git ~/.local/share/nvim/bundle/ctrlp.vim
git clone https://github.com/t9md/vim-choosewin.git ~/.local/share/nvim/bundle/vim-choosewin.git
```

Run

```
nvim .
```

Inside of it run

```
:PlugInstall
:CocInstall coc-rust-analyzer
:UpdateRemotePlugins
```