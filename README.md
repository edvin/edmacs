# EdMacs

## Installation

```bash
git clone https://github.com/edvin/edmacs ~/.emacs.d
```

## Prerequisites

- go: https://go.dev/doc/install
- godef: https://github.com/rogpeppe/godef
- dlv: https://github.com/go-delve/delve/tree/master/Documentation/installation
- gopls: https://pkg.go.dev/golang.org/x/tools/gopls#readme-installation
- libtree-sitter-dev: https://packages.debian.org/bookworm/main/libtree-sitter-dev

```bash
go install github.com/rogpeppe/godef@master
go install github.com/go-delve/delve/cmd/dlv@master
go install golang.org/x/tools/gopls@latest

# Ubuntu
sudo apt-get install libtree-sitter-dev

# Arch
yay libgccjit
yay libtree
```

Fonts: https://www.jetbrains.com/lp/mono/#how-to-install'

## Compile Emacs from source

Download: https://www.gnu.org/software/emacs/manual/html_node/efaq/Installing-Emacs.html

### Configure

```bash
./configure --with-pgtk --with-native-compilation
make -j 8
sudo make install
```

### First startup

```bash
/usr/local/bin/emacs

# icons:
M-x all-the-icons-install-fonts
M-x nerd-icons-install-fonts
M-x restart-emacs
```

### Refcard

https://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf
