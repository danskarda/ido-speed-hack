;;; benchmark helping library
;;; see file ido-bench-tests.el for actual

(defun ido-bench-make-input (STR &optional LIST)
  (if (string= STR "")
      LIST
    (ido-bench-make-input (substring STR 0 -1) (cons STR LIST))))

(defun ido-bench-make-cache ()
  (make-hash-table :test 'equal :size ido-test-bitmap-cache-size))

(defun ido-bench-run (LIST STR COMMON-COMPLETION-P N)
  (let* (;; configuration
	 (ido-matches			nil)
	 (ido-cur-item			'list)
	 (ido-cur-list			LIST)
	 ;; setup ido-speed hint variables
	 ;; (usually performed by ido-read-internal)
	 (ido-cache-cur-list		nil)
	 (ido-cache-cur-list-reverse	nil)
	 (*ido-speed-text*		"")
	 (*ido-speed-orig*		nil)
	 (*ido-speed-cache*		nil)
	 (*ido-speed-bitcache*		(when (eq 'shared ido-test-bitmap-cache)
					  (ido-bench-make-cache)))
	 (ido-common-match-string nil)
	 (inputs			(ido-bench-make-input STR)))

    (garbage-collect)
    (benchmark-run 1
      (dotimes (n (or N 1))
	(let ((*ido-speed-bitcache* (or *ido-speed-bitcache*
					(when ido-test-bitmap-cache
					  (ido-bench-make-cache)))))
	  (dolist (ido-text inputs)
	    (let ((ido-rescan t))
	      (ido-set-matches)
	      (when COMMON-COMPLETION-P
		(ido-set-common-completion)))))))))

(defun ido-bench-insert (KEY VAL)
  (insert (format "%-30s%s\n" (format "%s:" KEY) VAL)))

(defun ido-bench-insert-vars (VARS)
  (dolist (v VARS)
    (ido-bench-insert v (symbol-value v))))

(defun ido-bench-set-vars (VARS)
  (dolist (v VARS)
    (set (car v) (cadr v))))

(defun ido-bench-run-insert (LIST STR COMMON-COMPLETION-P N)
  (let ((r (ido-bench-run LIST STR COMMON-COMPLETION-P N)))
    (ido-bench-insert "Text" STR)
    (ido-bench-insert "Elapsed" (car r))
    (ido-bench-insert "GC"  	 (cadr r))
    (ido-bench-insert "GC time" (caddr r))))

(defun ido-bench-prepare-data ()
  (with-temp-buffer
    (insert "(setq ido-bench-list '")
    (let* (r)
      (mapatoms (lambda (x) (push x r)) obarray)
      (insert (format "%S" (mapcar (lambda (x) (cons (symbol-name x) x)) r))))
    (insert ")\n")
    (write-region nil nil "bench-data.el")))

(provide 'ido-bench)
