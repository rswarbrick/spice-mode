;;; test-spice-mode.el --- Code to test whether spice-mode.el is working properly.

(defun spice-test-file-name ()
  "Return the file name of the spice test file."
  (let ((fname (concat (file-name-directory
                        (locate-library "spice-mode"))
                       (file-relative-name "spice-test.cir"))))
    (unless (file-readable-p fname)
      (error "Couldn't read spice test file: %s" fname))
    fname))

(defmacro with-spice-test-buffer (&rest body)
  "Run `BODY' with a buffer pointing at the spice test file,
spice-test.cir."
  `(let ((buffer (find-file-noselect (spice-test-file-name))))
     (unwind-protect
         (with-current-buffer buffer
           ;; (Force spice mode)
           (spice-mode)
           ,@body)
       (kill-buffer buffer))))

(defun ensure-buffer-contains (string error-msg &rest args)
  "Check that the current buffer contains the given string."
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward string nil t)
      (apply #'error error-msg args))))

(defun test-spice-load-funcs ()
  "Test things that are supposed to happen when loading any old
spice file (such as init hooks etc)."
  (let* ((smh-has-run nil)
         (spice-mode-hook (list (lambda () (setq smh-has-run t)))))
    (with-spice-test-buffer nil)

    (unless (eq t smh-has-run)
      (error "Spice mode hook didn't run."))))

(defun test-spice-initialize-file ()
  "Test `spice-initialize-empty-file'."
  (let ((spice-initialize-empty-file t)
        (spice-initialize-template-file (spice-test-file-name)))
    (with-temp-buffer
      (spice-mode)
      (ensure-buffer-contains "* Spice test file"
                              "spice-initialize-empty-file failed."))))
