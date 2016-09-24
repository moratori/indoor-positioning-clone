
(in-package :cl-user)
(defpackage 
  :plotter.core.indoor-positioning
  (:use :cl 
        :cl-annot
        :types.core.indoor-positioning
        )
  (:import-from 
    :trivial-shell
    :shell-command
    )
  )
(in-package :plotter.core.indoor-positioning)



(enable-annot-syntax)


(defun exists-shell-command (command)
  "command が存在するか確かめ、存在する場合は
   commandおフルパスを返す
   存在しないならばnilを返す"
  (multiple-value-bind 
    (res _ code)
    (shell-command (format nil "which ~A" command))
    (declare (ignore _))
    (when (zerop code) (string-trim '(#\newline #\return) res))))

(defun plot-with-gnuplot (gnuplot-command)
  "gnuplotがインストールされているか確認し、
   されていればtrivial-shellで叩く
   入ってなければ、そのままコマンドを表示する"

  (let ((gpcheck (exists-shell-command "gnuplot")))
    (if gpcheck 
      (shell-command gnuplot-command)
      (progn 
        (format *standard-output* gnuplot-command)
        (force-output *standard-output*)))))


@export
(defun plot (path fingerprint place bssid)
  "gnuplotでplotできる形式のファイルを生成する 
   fingerprintは位置指紋を表すハッシュテーブル
   placeは場所名を表すキーワードシンボル
   bssidはプロットしたい特定のAP,又はtで
   全てのAPをプロットする
   これらをpathディレクトリのファイルに作る" 
  (if (stringp bssid)
    (multiple-value-bind
      (apdatalist flag) (gethash place fingerprint)
      (when flag 
        (let* ((apdata (find-if (lambda (x) (string= (string-trim '(#\") (ap.bssid x)) bssid)) apdatalist))
               (name (merge-pathnames path (ap.bssid apdata))))
          (when apdata 
            (with-open-file 
              (out name :direction :output :if-exists :supersede :if-does-not-exist :create)
              (loop for (rssi . f) in (ap.hist apdata)
                    do (format out "~A ~A~%" rssi f))))

          (plot-with-gnuplot
            (format nil "gnuplot -p -e 'plot \"~A\" title \"~A\" with linespoint'" name (pathname-name name))))))

    (multiple-value-bind 
      (apdatalist flag) (gethash place fingerprint)
      (when flag 
        (let (nl)
          (loop 
            for ap in apdatalist 
            for name = (merge-pathnames path (ap.bssid ap)) do
            (push name nl)
            (with-open-file 
              (out name  :direction :output :if-exists :supersede :if-does-not-exist :create)
              (loop 
                for (rssi . f) in (ap.hist ap) do 
                (format out "~A ~A~%" rssi f))))

          (plot-with-gnuplot 
            (let ((cmd "gnuplot -p -e 'set xlabel \"Received Signal Strength Indicator\"; set ylabel \"Frequency (times)\"; set title \"Distribution\"; plot "))
              (loop 
                for p in nl
                for name = (pathname-name p)
                do (setf cmd (concatenate 'string cmd 
                                          (format nil "\"~A\" title \"~A\" with linespoint, " p name))))
              (concatenate 'string cmd "'"))))))))
