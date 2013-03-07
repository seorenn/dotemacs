Major mode for editing Python files with some fontification and
indentation bits extracted from original Dave Love's python.el
found in GNU/Emacs.

Implements Syntax highlighting, Indentation, Movement, Shell
interaction, Shell completion, Shell virtualenv support, Pdb
tracking, Symbol completion, Skeletons, FFAP, Code Check, Eldoc,
Imenu.

Syntax highlighting: Fontification of code is provided and supports
python's triple quoted strings properly.

Indentation: Automatic indentation with indentation cycling is
provided, it allows you to navigate different available levels of
indentation by hitting <tab> several times.  Also when inserting a
colon the `python-indent-electric-colon' command is invoked and
causes the current line to be dedented automatically if needed.

Movement: `beginning-of-defun' and `end-of-defun' functions are
properly implemented.  There are also specialized
`forward-sentence' and `backward-sentence' replacements called
`python-nav-forward-block', `python-nav-backward-block'
respectively which navigate between beginning of blocks of code.
Extra functions `python-nav-forward-statement',
`python-nav-backward-statement',
`python-nav-beginning-of-statement', `python-nav-end-of-statement',
`python-nav-beginning-of-block' and `python-nav-end-of-block' are
included but no bound to any key.  At last but not least the
specialized `python-nav-forward-sexp' allows easy navigation
between code blocks.  If you prefer `cc-mode'-like `forward-sexp'
movement, setting `forward-sexp-function' to nil is enough, You can
do that using the `python-mode-hook':

(add-hook 'python-mode-hook
          (lambda () (setq forward-sexp-function nil)))

Shell interaction: is provided and allows you to execute easily any
block of code of your current buffer in an inferior Python process.

Shell completion: hitting tab will try to complete the current
word.  Shell completion is implemented in a manner that if you
change the `python-shell-interpreter' to any other (for example
IPython) it should be easy to integrate another way to calculate
completions.  You just need to specify your custom
`python-shell-completion-setup-code' and
`python-shell-completion-string-code'.

Here is a complete example of the settings you would use for
iPython 0.11:

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

For iPython 0.10 everything would be the same except for
`python-shell-completion-string-code' and
`python-shell-completion-module-string-code':

(setq python-shell-completion-string-code
      "';'.join(__IP.complete('''%s'''))\n"
      python-shell-completion-module-string-code "")

Unfortunately running iPython on Windows needs some more tweaking.
The way you must set `python-shell-interpreter' and
`python-shell-interpreter-args' is as follows:

(setq
 python-shell-interpreter "C:\\Python27\\python.exe"
 python-shell-interpreter-args
 "-i C:\\Python27\\Scripts\\ipython-script.py")

That will spawn the iPython process correctly (Of course you need
to modify the paths according to your system).

Please note that the default completion system depends on the
readline module, so if you are using some Operating System that
bundles Python without it (like Windows) just install the
pyreadline from http://ipython.scipy.org/moin/PyReadline/Intro and
you should be good to go.

Shell virtualenv support: The shell also contains support for
virtualenvs and other special environment modifications thanks to
`python-shell-process-environment' and `python-shell-exec-path'.
These two variables allows you to modify execution paths and
environment variables to make easy for you to setup virtualenv rules
or behavior modifications when running shells.  Here is an example
of how to make shell processes to be run using the /path/to/env/
virtualenv:

(setq python-shell-process-environment
      (list
       (format "PATH=%s" (mapconcat
                          'identity
                          (reverse
                           (cons (getenv "PATH")
                                 '("/path/to/env/bin/")))
                          ":"))
       "VIRTUAL_ENV=/path/to/env/"))
(python-shell-exec-path . ("/path/to/env/bin/"))

Since the above is cumbersome and can be programmatically
calculated, the variable `python-shell-virtualenv-path' is
provided.  When this variable is set with the path of the
virtualenv to use, `process-environment' and `exec-path' get proper
values in order to run shells inside the specified virtualenv.  So
the following will achieve the same as the previous example:

(setq python-shell-virtualenv-path "/path/to/env/")

Also the `python-shell-extra-pythonpaths' variable have been
introduced as simple way of adding paths to the PYTHONPATH without
affecting existing values.

Pdb tracking: when you execute a block of code that contains some
call to pdb (or ipdb) it will prompt the block of code and will
follow the execution of pdb marking the current line with an arrow.

Symbol completion: you can complete the symbol at point.  It uses
the shell completion in background so you should run
`python-shell-send-buffer' from time to time to get better results.

Skeletons: 6 skeletons are provided for simple inserting of class,
def, for, if, try and while.  These skeletons are integrated with
dabbrev.  If you have `dabbrev-mode' activated and
`python-skeleton-autoinsert' is set to t, then whenever you type
the name of any of those defined and hit SPC, they will be
automatically expanded.  As an alternative you can use the defined
skeleton commands: `python-skeleton-class', `python-skeleton-def'
`python-skeleton-for', `python-skeleton-if', `python-skeleton-try'
and `python-skeleton-while'.

FFAP: You can find the filename for a given module when using ffap
out of the box.  This feature needs an inferior python shell
running.

Code check: Check the current file for errors with `python-check'
using the program defined in `python-check-command'.

Eldoc: returns documentation for object at point by using the
inferior python subprocess to inspect its documentation.  As you
might guessed you should run `python-shell-send-buffer' from time
to time to get better results too.

Imenu: This mode supports Imenu in its most basic form, letting it
build the necessary alist via `imenu-default-create-index-function'
by having set `imenu-extract-index-name-function' to
`python-info-current-defun' and
`imenu-prev-index-position-function' to
`python-imenu-prev-index-position'.

If you used python-mode.el you probably will miss auto-indentation
when inserting newlines.  To achieve the same behavior you have
two options:

1) Use GNU/Emacs' standard binding for `newline-and-indent': C-j.

2) Add the following hook in your .emacs:

(add-hook 'python-mode-hook
  #'(lambda ()
      (define-key python-mode-map "\C-m" 'newline-and-indent)))

I'd recommend the first one since you'll get the same behavior for
all modes out-of-the-box.

Installation:

Add this to your .emacs:

(add-to-list 'load-path "/folder/containing/file")
(require 'python)
