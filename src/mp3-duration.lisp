(in-package :cl-user)
(defpackage mp3-duration
  (:use #:cl)
  (:export #:estimate-duration))
(in-package :mp3-duration)

(defstruct peekable-stream
  (peek nil :type (or null (unsigned-byte 8)))
  (real-stream nil :type stream))

(declaim (ftype (function (peekable-stream) (unsigned-byte 8)) read-byte*))
(defun read-byte* (stream)
  (declare (type peekable-stream stream))
  (with-slots (peek real-stream) stream
    (if peek
        (prog1 peek
          (setf peek nil))
        (read-byte real-stream))))

(declaim (ftype (function (peekable-stream) (unsigned-byte 8)) peek-byte))
(defun peek-byte (stream)
  (declare (type peekable-stream stream))
  (with-slots (peek real-stream) stream
    (or peek
        (let ((next (read-byte real-stream)))
          (setf peek next)
          next))))

(defun unread-byte (stream byte)
  (declare (type peekable-stream stream)
           (type (unsigned-byte 8) byte))
  (with-slots (peek) stream
    (when peek
      (error "Cannot unread-byte twice."))
    (setf peek byte)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar *skip-buffer* (make-array 1024 :element-type '(unsigned-byte 8))))
(defun skip-bytes (in count)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type peekable-stream in)
           (type fixnum count))
  (let ((in (peekable-stream-real-stream in)))
    (multiple-value-bind (times mod)
        (floor count #.(length *skip-buffer*))
      (dotimes (i times)
        (read-sequence *skip-buffer* in))
      (read-sequence *skip-buffer* in :end mod))))

(defun skip-id3-tag (in)
  (declare (type peekable-stream in))
  (block nil
    (unless (= (peek-byte in) (char-code #\I))
      (return nil))
    (read-byte* in)
    (unless (= (peek-byte in) (char-code #\D))
      (return nil))
    (read-byte* in)
    (unless (= (peek-byte in) (char-code #\3))
      (return nil))
    (read-byte* in)

    ;; Skip version and revision
    (skip-bytes in 2)

    (let ((has-footer (/= (logand (read-byte* in) #x10) 0))
          (size (+ (ash (read-byte* in) 21)
                   (ash (read-byte* in) 14)
                   (ash (read-byte* in) 7)
                   (read-byte* in))))
      (skip-bytes in (if has-footer
                         (+ size 10)
                         size)))
    t))

(defun parse-frame (in)
  (declare (optimize (speed 3) (safety 2))
           (type peekable-stream in))
  (let ((1st-byte (read-byte* in)))
    (declare (type (unsigned-byte 8) 1st-byte))
    (cond
      ((and (= 1st-byte #xff)
            (= (logand (peek-byte in) #xe0) #xe0))
       (let* ((b1 (read-byte* in))
              (b2 (read-byte* in))
              (version (svref #(3  ;; MPEGv2.5
                                0
                                2  ;; MPEGv2
                                1) ;; MPEGv1
                              (ash (logand b1 #x18) -3)))
              (layer (svref #(0 3 2 1)
                            (ash (logand b1 #x06) -1)))
              (bitrate
                (aref
                 #3A(((0  0  0  0  0  0  0  0  0  0  0  0  0  0  0)
                      (0  0  0  0  0  0  0  0  0  0  0  0  0  0  0)
                      (0  0  0  0  0  0  0  0  0  0  0  0  0  0  0)
                      (0  0  0  0  0  0  0  0  0  0  0  0  0  0  0))
                     ((0  0  0  0  0  0  0  0  0  0  0  0  0  0  0)
                      (0 32 64 96 128 160 192 224 256 288 320 352 384 416 448)
                      (0 32 48 56  64  80  96 112 128 160 192 224 256 320 384)
                      (0 32 40 48  56  64  80  96 112 128 160 192 224 256 320))
                     ((0  0  0  0  0  0  0   0   0   0   0   0   0   0   0)
                      (0 32 48 56 64 80 96 112 128 144 160 176 192 224 256)
                      (0  8 16 24 32 40 48  56  64  80  96 112 128 144 160)
                      (0  8 16 24 32 40 48  56  64  80  96 112 128 144 160))
                     ((0  0  0  0  0  0  0   0   0   0   0   0   0   0   0)
                      (0 32 48 56 64 80 96 112 128 144 160 176 192 224 256)
                      (0  8 16 24 32 40 48  56  64  80  96 112 128 144 160)
                      (0  8 16 24 32 40 48  56  64  80  96 112 128 144 160)))
                 version
                 layer
                 (ash (logand b2 #xf0) -4)))
              (sample-rate (aref
                            #2A((    0     0     0)
                                (44100 48000 32000)
                                (22050 24000 16000)
                                (11025 12000  8000))
                            version
                            (ash (logand b2 #x0c) -2)))
              (sample (aref
                       #2A((0   0    0    0)
                           (0 384 1152 1152)
                           (0 384 1152  576)
                           (0 384 1152  576))
                       version
                       layer))
              (padding-bit (ash (logand b2 #x02) -1))
              (frame-size (truncate
                           (if (= layer 1)
                               (+ (/ (* sample bitrate 125) sample-rate)
                                   (* padding-bit 4))
                               (+ (/ (* sample bitrate 125) sample-rate)
                                   padding-bit)))))
         (declare (type fixnum version layer sample-rate sample bitrate))
         (if (and (/= 0 sample)
                  (/= 0 frame-size))
             (progn
               (skip-bytes in (- frame-size 3))
               (/ sample sample-rate))
             (progn
               (warn "Corrupted?")
               0))))
      ((and (= 1st-byte (char-code #\T))
            (= (peek-byte in) (char-code #\A))
            (progn
              (read-byte* in)
              (= (peek-byte in) (char-code #\G))))
       ;; Skip ID3v1 tag
       (skip-bytes in (- 128 2))
       0)
      (t (warn "Corrupted?") 0))))

(defun estimate-duration (input)
  (flet ((parse-frames (in)
           (let ((duration 0))
             (handler-case
                 (loop
                   (incf duration (parse-frame in)))
               (end-of-file ()
                 (float duration))))))
    (etypecase input
      (stream
       (let ((in (make-peekable-stream :real-stream input)))
         (skip-id3-tag in)
         (parse-frames in)))
      ((or pathname string)
       (with-open-file (in input :element-type '(unsigned-byte 8))
         (let ((in (make-peekable-stream :real-stream in)))
           (skip-id3-tag in)
           (parse-frames in)))))))
