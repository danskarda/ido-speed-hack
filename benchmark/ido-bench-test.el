;;; For easiest setup, make in current directory following symlinks:
;;;
;;; link-ido-emacs-23.el.gz	- original ido.el from emacs 23.4
;;; link-ido-emacs-24.el.gz	- original ido.el from emacs 24
;;; link-ido-patched.el		- patched ido.el
;;;
;;; link-ido-speed-hack.el	- link to ../ido-speed-hack.el
;;;
;;; link-ido-better-flex-original.el  - original ido.el
;;; link-ido-better-flex-patched.el   - original ido.el

;;; cleanup
(shell-command "rm -v *.elc *~")

;;; configuration
(setq ido-bench-iterations	10
      ido-bench-buffer		"*ido-bench-results*"
      ido-bench-texts		'("abcdefghijklmnopqrstuvwxyz" "-etaimn"
				  "bcfile" "grpdir")

      ;; define variables for tests
      ido-bench-common-completion   nil
      ido-test-bitmap-cache	    nil
      ido-test-bitmap-cache-size    nil)

;;; load benchmark lib
(byte-compile-file "ido-bench.el" t)

;;; prepare bench data - only once to have comparable dataset!
(and nil (ido-bench-prepare-data))

;;; 0) load bench data
(load-file "bench-data.el")

;;; 1) choose ido.el:
(byte-compile-file (setq ido-bench-ido "link-ido-emacs-23.el.gz") t)
(byte-compile-file (setq ido-bench-ido "link-ido-emacs-24.el.gz") t)
(byte-compile-file (setq ido-bench-ido "link-ido-patched.el")     t)

;;; 2) (optionally) load ido-speed-hack
(progn (byte-compile-file (setq ido-bench-ido "link-ido-speed-hack.el")  t))




;;; 3) (optionally) load ido-better-flex
(byte-compile-file (setq ido-bench-ido "link-ido-better-flex-original.el") t)
(byte-compile-file (setq ido-bench-ido "link-ido-better-flex-patched.el")  t)

(ido-better-flex/enable)

;;; define tests

(setq ido-bench-test-vars
      '(ido-bench-common-completion
	ido-enable-flex-matching
	ido-case-fold
	ido-enable-regexp
	ido-test-bitmap-cache
	ido-test-bitmap-cache-size)

      ido-bench-test-list-better-flex
      '(;; only better flex
	((ido-bench-common-completion	  nil)
	 (ido-enable-flex-matching	  nil)
	 (ido-case-fold			  nil)
	 (ido-enable-regexp		  nil))
	;; only better case senstive
	((ido-bench-common-completion	  nil)
	 (ido-enable-flex-matching	  nil)
	 (ido-case-fold			  t)
	 (ido-enable-regexp		  nil)))

      ido-bench-test-list
      '( ;; nothing, not even common completion
	((ido-bench-common-completion	  nil)
	 (ido-enable-flex-matching	  nil)
	 (ido-case-fold			  nil)
	 (ido-enable-regexp		  nil))
	;; common completion
	((ido-bench-common-completion	  t)
	 (ido-enable-flex-matching	  nil)
	 (ido-case-fold			  nil)
	 (ido-enable-regexp		  nil))
	;; flex
	((ido-bench-common-completion	  nil)
	 (ido-enable-flex-matching	  t)
	 (ido-case-fold			  nil)
	 (ido-enable-regexp		  nil))
	;; flex + case-fold
	((ido-bench-common-completion	  nil)
	 (ido-enable-flex-matching	  t)
	 (ido-case-fold			  t)
	 (ido-enable-regexp		  nil))
	;; regexp
	((ido-bench-common-completion	  nil)
	 (ido-enable-flex-matching	  nil)
	 (ido-case-fold			  nil)
	 (ido-enable-regexp		  t))))


(defun ido-bench-batch (CFG)
  (kill-buffer (get-buffer-create ido-bench-buffer))
  (with-current-buffer (get-buffer-create ido-bench-buffer)
    (display-buffer ido-bench-buffer)
    (ido-bench-insert "Version"    ido-bench-ido)
    (ido-bench-insert "Iterations" ido-bench-iterations)
    (ido-bench-insert "List size"  (length ido-bench-list))

    (dolist (cfg CFG)
      (insert "\n\nConfiguration\n=============\n")
      (ido-bench-set-vars cfg)
      (ido-bench-insert-vars ido-bench-test-vars)

      (dolist (txt ido-bench-texts)
	(newline)
	(ido-bench-run-insert ido-bench-list
			      txt
			      ido-bench-common-completion
			      ido-bench-iterations)
	(redisplay t))))
  (ding))


;;;  test for all except caches
(ido-bench-batch  ido-bench-test-list)

;;; test for better-flex only
(ido-bench-batch ido-bench-test-list-better-flex)

;;; for caches ...
(setq ido-test-bitmap-cache	  t
      ido-test-bitmap-cache-size  65537)

(setq ido-test-bitmap-cache	  'shared
      ido-test-bitmap-cache-size  65537)

(ido-bench-batch (append ido-bench-test-list
			 ido-bench-test-list-cache))
