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
clangd, bear is needed for lsp

```shell
sudo apt-get install clangd
sudo apt-get install bear
```

if #include file not found, running make with bear command will generate the
compile_commands.json, which later will be used by clangd
