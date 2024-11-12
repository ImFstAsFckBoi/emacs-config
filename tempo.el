; Tempo/BPM tap pad

(defun get-time-at-input (p)
  (interactive)
  (read-from-minibuffer p )
  (float-time))

(defun bpm-from-delta-time (dt)
  (* 60 (/ 1 dt)))

(defun mean (seq)
  (setq l (length seq))
  (/ (apply '+ seq) l))

(defun tempo-pad ()
  "Tap enter in time with a song to estimate its tempo in BPMs."
  (interactive)
  (setq sma-len 50)
  (setq bpms (make-list 1 120))
  (setq p "-")
  (setq t1 (get-time-at-input p))
  (while t
    (setq t2 (get-time-at-input p))
    (setq p (format "(SMA-len=%3d) BPM: %d " (length bpms) (mean bpms)))
    (add-to-list 'bpms  (bpm-from-delta-time (- t2 t1)))
    (if (> (length bpms) sma-len)
        (setq bpms (butlast bpms 1)))
    (setq t1 t2)))
