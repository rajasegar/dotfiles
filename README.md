# dotfiles
My dotfiles repo

Managed with [rcm](http://thoughtbot.github.io/rcm/rcm.7.html)

Install rcm using https://github.com/thoughtbot/rcm
```
brew tap thoughtbot/formulae
brew install rcm
```

## Installation
Before doing the below steps, install the tools you need FIRST.

1. Go to the root
```
$ cd
```
2. Checkout this repo into `.dotfiles` directory
```
$ git clone https://github.com/rajasegar/dotfiles .dotfiles
```

3. Run `rcup` to install the dotfiles
```
$ rcup -v
```

## Tools you need

### Emacs
Install [Emacs](https://www.gnu.org/software/emacs/)

### NeoVim
Install [neovim](https://github.com/neovim/neovim/wiki/Installing-Neovim)


### packer.nvim
For managing Neovim plugins


To get started, first clone this [repository](https://github.com/wbthomason/packer.nvim) to somewhere on your `packpath`, e.g.:

> Unix, Linux Installation
```shell
git clone --depth 1 https://github.com/wbthomason/packer.nvim\
 ~/.local/share/nvim/site/pack/packer/start/packer.nvim
```


### tmux
https://github.com/tmux/tmux/wiki

### tpm
Tmux plugin manager
https://github.com/tmux-plugins/tpm

### tmuxinator
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

## Fonts
Download Hack Nerd Font from here
[Nerd fonts](https://www.nerdfonts.com/font-downloads)


