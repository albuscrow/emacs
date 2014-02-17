((dired-isearch status "installed" recipe
                (:name dired-isearch :auto-generated t :type emacswiki :description "isearch in Dired" :website "https://raw.github.com/emacsmirror/emacswiki.org/master/dired-isearch.el"))
 (dired-view status "installed" recipe
             (:name dired-view :description "Dired view mode" :type emacswiki :features dired-view))
 (doxymacs status "installed" recipe
           (:name doxymacs :website "http://doxymacs.sourceforge.net/" :description "Doxymacs is Doxygen + {X}Emacs." :type git :url "git://doxymacs.git.sourceforge.net/gitroot/doxymacs/doxymacs" :load-path
                  ("./lisp")
                  :build
                  ("./bootstrap" "./configure" "make")
                  :build/darwin
                  ("sed -i -e 's/-fexpensive-optimizations//' ./c/Makefile.am" "sed -i -e 's/inline/static inline/' ./c/doxymacs_parser.c" "./bootstrap" "./configure" "make")
                  :features doxymacs))
 (el-get status "installed" recipe
         (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "master" :pkgname "dimitri/el-get" :info "." :load "el-get.el"))
 (linum+ status "installed" recipe
         (:name linum+ :auto-generated t :type emacswiki :description "Extension of linum" :website "https://raw.github.com/emacsmirror/emacswiki.org/master/linum+.el")))
