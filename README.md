# dotfiles
My dotfiles repo

Managed with [rcm](http://thoughtbot.github.io/rcm/rcm.7.html)

## Tools you need

### vim
You need vim 8.0 or newer version

or 

### NeoVim


### vim-plug
For managing vim plugins

Install vim-plug using :

curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim



### tmux
https://github.com/tmux/tmux/wiki

### tumxinator
https://github.com/tmuxinator/tmuxinator

Install tmux through ruby, You need ruby 2.4.5 or higher version

```sh
gem install tmuxinator
```
#### bash completion

Add the following to your ~/.bashrc:

```sh
source ~/.bin/tmuxinator.bash
```


### fzf
https://github.com/junegunn/fzf

Install fzf through git

```sh
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install
```

