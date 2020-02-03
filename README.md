# dotemacs ![CI](https://github.com/furushchev/dotemacs/workflows/CI/badge.svg)

**This is an Emacs Settings**

### How to use

1. download this repository and set on appropriate directory

  ``` bash
git clone https://github.com/furushchev/dotemacs.git ~/.emacs.d
```

2. launch `emacs`

  **NOTE** On first startup, dependencies are automatically downloaded and setup, so it may take some time.
  
3. Modify your name and email address

  ```bash
emacs ~/.emacs.d/init-common.el
# ;; author
# (setq user-full-name "Yuki Furuta")  ;; change this
# (setq user-mail-address "me@furushchev.ru") ;; also change this
```

### Author

Yuki Furuta (me@furushchev.ru)
