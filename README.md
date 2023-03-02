# Coordiante35's Emacs Config

Support:
* Golang IDE

# Golang Dependency

```shell
go install golang.org/x/tools/gopls@latest
go get -u github.com/nsf/gocode
```

Need to add gopls to $PATH. Exampe: 

```shell
export PATH=$PATH:$HOME/go/bin
```

# C/C++ Dependency

Referenced by: https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/
clangd, bear, mono is needed for lsp

```shell
sudo apt-get install clangd
sudo apt-get install clang
sudo apt-get install bear

sudo apt install gnupg ca-certificates
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
echo "deb https://download.mono-project.com/repo/ubuntu stable-bionic main" | sudo tee /etc/apt/sources.list.d/mono-official-stable.list
sudo apt update
sudo apt install mono-devel

if running M-x dap-cpptools-setup failed for "dap-utils--get-extension: Wrong type argument: stringp, nil", try to find the zip package
in /tmp/ (size: 80M), unzip the package to ~/.emacs.d/.extension/vscode/cpptools
```

if #include file not found, running make with bear command will generate the
compile_commands.json, which later will be used by clangd
