;; Added early-init.el
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
;; (setq straight-use-package-by-default t)
(setq use-package-always-demand t)

(use-package kixi-emacs
  :straight (kixi-emacs :type git :repo "https://github.com/MastodonC/kixi-emacs.git")
  :demand t)

(use-package evil-tutor
  :straight t)

(require 'kixi-emacs)

(use-package magit
  :init
  (setq magit-diff-refine-hunk t)
  (setq magit-repository-directories
        '(("~/wip" . 2)
          ("~/src" . 1)))
  (setq magit-repolist-columns
        '(("F" 1 magit-repolist-column-flag nil)
          ("Name" 25 magit-repolist-column-ident nil)
          ("Sta" 3 magit-repolist-column-stashes nil)
          ("Bra" 3 magit-repolist-column-branches nil)
          ("Branch" 25 magit-repolist-column-branch nil)
          ("Upstream" 25 magit-repolist-column-upstream nil)
          ("Version" 25 magit-repolist-column-version nil)
          ("B<U" 3 magit-repolist-column-unpulled-from-upstream
           ((:right-align t)
            (:help-echo "Upstream changes not in branch")))
          ("B<R" 3 magit-repolist-column-unpulled-from-pushremote
           ((:right-align t)
            (:help-echo "Remote changes not in branch")))
          ("B>U" 3 magit-repolist-column-unpushed-to-upstream
           ((:right-align t)
            (:help-echo "Local changes not in upstream")))
          ("B>R" 3 magit-repolist-column-unpushed-to-pushremote
           ((:right-align t)
            (:help-echo "Local changes not in remote")))
          ("Path" 99 magit-repolist-column-path nil))))

(setq auth-sources '("~/.authinfo"))
(use-package forge
  :after magit)

(use-package blamer
  ;; :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 100
                   :italic t)))
  ;; :config
  ;; (global-blamer-mode 1)
  )

(use-package git-auto-commit-mode
  :diminish "🦾")

