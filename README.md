# dotemacs [![Build Status](https://travis-ci.org/furushchev/dotemacs.svg)](https://travis-ci.org/furushchev/dotemacs)

**This is an Emacs Settings**

### Install

``` bash
curl -sfL https://raw.github.com/furushchev/dotemacs/master/install.sh | bash
```

### Step-by-Step Install

1. download this repository and set on appropriate directory

  ``` bash
git clone https://github.com/furushchev/dotemacs.git ~/.emacs.d
cd ~/.emacs.d && git submodule update --init
```

2. launch `emacs`

  **NOTE** On first startup, dependencies are automatically downloaded and setup, so it may take some time.
  
3. Modify your name and email address

  ```bash
emacs ~/.emacs.d/init/00-settings.el
# ;; author
# (setq user-full-name "Yuki Furuta")  ;; change this
# (setq user-mail-address "furushchev@jsk.imi.i.u-tokyo.ac.jp") ;; change also this
```

### Profiling

After launching `emacs`, execute `M-x init-loader-show-log`.  
This command shows how many time it takes to execute each startup scripts.

### Author

Yuki Furuta (furushchev@mail.ru)
