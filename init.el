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

;; Get the version of org from the repo
(straight-use-package 'org)

(use-package kixi-emacs
  :straight (kixi-emacs :type git :repo "https://github.com/MastodonC/kixi-emacs.git")
  :demand t)

(use-package evil-tutor
  :straight t)

(require 'kixi-emacs)
(require 'general)

(use-package magit
  :straight t
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
  :straight t
  :after magit)

(use-package blamer
  :straight t
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
  :straight t
  :diminish "🦾")

;; Language-specific packages
(use-package org
  :config
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (require 'org-protocol)
  (setq org-directory "~/org/")
  (setq org-id-link-to-org-use-id 'use-existing)
  (setq org-log-done t)
  (setq org-capture-templates
        `(("p" "Protocol" entry (file+headline ,(concat org-directory "/notes.org") "Inbox")
           "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"
           :empty-lines-before 1)
          ("L" "Protocol Link" entry (file+headline ,(concat org-directory "/notes.org") "Inbox")
           "* [[%:link][%:description]] \nCaptured On: %U\n\n%?"
           :empty-lines-before 1)))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WIP(w)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)")
          (sequence "DELEGATED(g)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)")
          (sequence "MAYBE(m)" "|" "CANCELLED(c)")))
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "DarkOrange1" :weight bold))
          ("DELEGATED" . (:foreground "DarkOrange1"))
          ("WIP" . (:foreground "blue" :background "gold" :weight bold))
          ("BLOCKED" . (:background "dark orange" :foreground "white"))
          ("MAYBE" . (:foreground "sea green"))
          ("DONE" . (:foreground "light sea green"))
          ("CANCELLED" . (:foreground "forest green")))))

(use-package org-roam
  :straight t
  :after org
  :init
  (setq org-roam-v2-ack t)
  (setq dw/daily-note-filename "%<%Y-%m-%d>.org"
        dw/daily-note-header "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
  :custom
  (org-roam-directory "~/RoamNotes")
  (org-roam-dailies-directory "daily/")
  (org-roam-completion-everywhere t)
  (org-roam-mode-section-functions
   (list #'org-roam-backlinks-section
         #'org-roam-reflinks-section
         #'org-roam-unlinked-references-section))

  ;; initial config
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%H:%M >: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
      :empty-lines-before 1)))

  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ("P" "Project" plain "* Goals\n\n%?\n\n* People\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)))
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))
  :general 
  (kixi-leader-def
    :infix "r"
    "" '(:ignore t :wk "org")
    "t" 'org-roam-dailies-goto-today
    "T" 'org-roam-dailies-capture-today
    "d" 'org-roam-dailies-goto-date
    "D" 'org-roam-dailies-capture-date
    "f" 'org-roam-node-find
    "s" 'org-store-link)

  (kixi-mode-leader-def
    :keymaps 'org-mode-map
    "TAB" 'org-cycle
    "o" 'org-open-at-point
    "r" '(:ignore t :wk "roam")
    "ri" 'org-roam-node-insert
    "l" '(:ignore t :wk "link")
    "li" 'org-insert-link
    "ls" 'org-store-link))

(use-package org-transclusion
  :straight t
  :after org
  :init
  (setq org-transclusion-open-source-display-action-list '(display-buffer-same-window))
  (setq org-transclusion-add-all-on-activate t))

(use-package org-modern
  :straight t
  :after org
  :config (global-org-modern-mode))

(use-package 2048-game
  :straight t)

(use-package ess
  :straight t
  :defer t)

(use-package helpful
  :straight t)

(setq org-appear-trigger 'manual)
(add-hook 'org-mode-hook (lambda ()
                           (add-hook 'evil-insert-state-entry-hook
                                     #'org-appear-manual-start
                                     nil
                                     t)
                           (add-hook 'evil-insert-state-exit-hook
                                     #'org-appear-manual-stop
                                     nil
                                     t)))
(customize-set-variable 'org-return-follows-link t)
(customize-set-variable 'org-mouse-1-follows-link t)

;; Display links as the description provided
(customize-set-variable 'org-link-descriptive t)

;; Hide markup markers
(customize-set-variable 'org-hide-emphasis-markers t)

(straight-use-package '(org-appear :type git :host github :repo "awth13/org-appear"))
(add-hook 'org-mode-hook 'org-appear-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stupid window tricks
(defun delete-window-below ()
  (interactive)
  (save-excursion
    (windmove-down)
    (delete-window)))

(defun delete-window-right ()
  (interactive)
  (save-excursion
    (windmove-right)
    (delete-window)))

(kixi-leader-def
  :infix "w"
  "x" '(:ignore t :wk "delete windows")
  "xb" 'delete-window-below
  "xr" 'delete-window-right)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stuff that might migrate to kixi-emacs


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enabling desktop-save-mode will save and restore all buffers between sessions
(setq desktop-restore-frames 't)
(desktop-save-mode 1)
(savehist-mode 1)
(server-start)

(provide 'init)
