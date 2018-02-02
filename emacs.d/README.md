# .emacs.d

## GUI无法输入中文：
http://wangzhe3224.github.io/emacs/2015/08/31/fcitx-emacs.html

https://gist.github.com/blindFS/c258b0c92008c5f8678a

先检查是否有zh_CN.UTF-8库，如果有，在/usr/local/share/application/emacs.desktop中修改：

```
Exec=env LC_CType=zh_CN.UTF-8 emacs %F
```
