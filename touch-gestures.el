;;; custom/touch-gestures.el -*- lexical-binding: t; -*-
;;;

(defvar touchscreen-touch-current '()
  "Current touch events")

(setq touchscreen-touch-current '())

(defun touchscreen-parse-touch-event (events &optional update-p)
  (let ((events-type (car events))
        (events-data
         (if update-p
             (car (cdr events))
           (cdr events))))
    ;; (message "events-data in parsing: %s" events-data)
    (mapcar (lambda (event-data)
              (let ((tap-id-val (nth 0 event-data))
                    (event-type-val
                     (cond
                      ((eql events-type 'touchscreen-begin) 0)
                      ((eql events-type 'touchscreen-update) 1)
                      ((eql events-type 'touchscreen-end) 2)))
                    (window-target-val (nth 1 event-data))
                    (position-or-area-val (nth 2 event-data))
                    (x-y-val (nth 3 event-data))
                    (timestamp-val (nth 4 event-data))
                    (object-val (nth 5 event-data))
                    (text-pos-val (nth 6 event-data))
                    (col-row-val (nth 7 event-data))
                    (image-val (nth 8 event-data))
                    (dx-dy-val (nth 9 event-data))
                    (width-height-val (nth 10 event-data)))
                (list tap-id-val
                      (list 'event-type event-type-val
                            'window-target window-target-val
                            'position-or-area position-or-area-val
                            'x-y x-y-val
                            'timestamp timestamp-val
                            'object object-val
                            'text-pos text-pos-val
                            'col-row col-row-val
                            'image image-val
                            'dx-dy dx-dy-val
                            'width-height width-height-val))))
            events-data)))

(defun touchscreen-get-event-details (event)
  (car (cdr event)))

(defun touchscreen-process-touch-begin (input)
  "Test touchscreen"
  (interactive "e")
  (message "begin")
  ;; (message "plain begin: %s" input)
  (let ((event-details (touchscreen-parse-touch-event input)))
    ;; (message "%s" event-details)
    (when (not (assoc (car (nth 0 event-details)) touch-current))
      (setq touch-current
            (append event-details touch-current)))))

(defun touchscreen-process-touch-update (input)
  "Test touchscreen"
  (interactive "e")
  (let ((events (touchscreen-parse-touch-event input t)))
    (when (eq 1 (length events)) ;; simple scrolling
      ;; (message "event info from update: %s" (car (cdr (nth 0 events))))
      (let ((col-row (plist-get (touchscreen-get-event-details (nth 0 events)) 'col-row)))
        ;; (message "%s" col-row)
        ))
    (setq touch-current events)))

(defun touchscreen-process-touch-end (input)
  "Test touchscreen"
  (interactive "e")
  (message (format "%s" input))
  (let* ((event-details (touchscreen-parse-touch-event input))
         (tap-id (car (nth 0 event-details))))
    (setq touch-current (delq (assoc tap-id touch-current) touch-current))))


(global-set-key [touchscreen-begin]  #'touchscreen-process-touch-begin)
(global-set-key [touchscreen-update] #'touchscreen-process-touch-update)
(global-set-key [touchscreen-end]    #'touchscreen-process-touch-end)
