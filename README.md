# My dotfiles

## Description
These are my personal dotfiles, which I manage with the help of git and a nice tool called [dotfiles]. 

## Installation 

Install the [dotfiles] package, either using `pip` (recommended) or `easy_install`. Maybe with some help of `sudo`.

    pip install dotfiles

Create some directory where to store multiple dotfiles repositories.
   
    mkdir -p ~/repos/dotfiles
   
Clone this repository into that directory.
   
    git clone https://github.com/thomasf/dotfiles-thomasf-emacs ~/repos/dotfiles/emacs
   
And symlink it's contents into your home directory.

    dotfiles -s -R ~/repos/dotfiles/emacs
     
Also check out `dotfiles -h` or the [dotfiles](https://github.com/jbernard/dotfiles) manual for more information on the hows and whats of that tool.


[dotfiles]: https://github.com/jbernard/dotfiles "dotfiles"
