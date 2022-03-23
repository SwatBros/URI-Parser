; Alessio Disaro' 866247


(defun uri-parse (uri)
  (let ((found (recognize (coerce uri 'list))))
    (if (not (null found))
        (cons 'uri (convert-port (convert found))))))


(defun uri-scheme (uri)
  (cond ((null uri) nil)
        ((urip uri) (second uri))))

(defun uri-userinfo (uri)
  (cond ((null uri) nil)
        ((urip uri) (third uri))))

(defun uri-host (uri)
  (cond ((null uri) nil)
        ((urip uri) (fourth uri))))

(defun uri-port (uri)
  (cond ((null uri) nil)
        ((urip uri) (fifth uri))))

(defun uri-path (uri)
  (cond ((null uri) nil)
        ((urip uri) (sixth uri))))

(defun uri-query (uri)
  (cond ((null uri) nil)
        ((urip uri) (seventh uri))))

(defun uri-fragment (uri)
  (cond ((null uri) nil)
        ((urip uri) (eighth uri))))


(defun uri-display (uri &optional (stream t))
  (cond ((null uri) nil)
        ((urip uri) (format stream
                   "Scheme: ~S~%Userinfo: ~S~%Host: ~S
Port: ~S~%Path: ~S~%Query: ~S~%Fragment: ~S"
                   (uri-scheme uri)
                   (uri-userinfo uri)
                   (uri-host uri)
                   (uri-port uri)
                   (uri-path uri)
                   (uri-query uri)
                   (uri-fragment uri)))
        (t nil)))


(defun urip (uri)
  (cond ((null uri) t)
        (t (and (listp uri)
                (= (length uri) 8)
                (eq (first uri) 'uri)
                (mapcar 'stringp (rest uri))))))


(defun recognize (text)
  (accept text '(start other) '(() () () () () ()()) 0 0))


(defun accept (chars state parsed id44 id8)
  (cond ((and (= (length chars) 0)
              (final-p (first state)))
	 parsed)
	((= (length chars) 0) nil)
	((or (> id44 44) (> id8 8)) nil)
        (t (let ((next (delta (first state) (first chars))))
             (cond ((null next) nil)
                   ((= (length next) 1)
		    (let ((new (first next)))
		      (let ((nid44 (add-id44 id44 (second new)))
			    (nid8 (add-id8 id8 (second new))))
			(accept (rest chars) new
				(add-parsed parsed
					    (first chars)
					    (second new))
				nid44
				nid8))))
                   (t (let ((nid44-s (add-id44-s id44 (mapcar 'second next)))
			    (nid8-s (add-id8-s id8 (mapcar 'second next))))
			(accept-s (rest chars) next
				  (add-parsed-s parsed
						(first chars)
						(mapcar 'second next))
				  nid44-s
				  nid8-s))))))))

(defun accept-s (chars states parsed-l id44-s id8-s)
  (cond ((= (length states) 1)
	 (accept chars (first states) (first parsed-l)
		 (first id44-s) (first id8-s)))
        (t (let ((next
		  (accept chars (first states) (first parsed-l)
			  (first id44-s) (first id8-s))))
             (cond ((not (null next)) next)
                   (t (accept-s
		       chars
		       (rest states)
		       (rest parsed-l)
		       (rest id44-s)
		       (rest id8-s))))))))


(defun add-id44 (id44 type)
  (cond ((eq type 'zpath44) (+ id44 1))
	(t id44)))

(defun add-id8 (id8 type)
  (cond ((eq type 'zpath8) (+ id8 1))
	(t id8)))

(defun add-id44-s (id44 type-s)
  (cond ((null type-s) ())
	(t (cons (add-id44 id44 (first type-s))
		 (add-id44-s id44 (rest type-s))))))

(defun add-id8-s (id8 type-s)
  (cond ((null type-s) ())
	(t (cons (add-id8 id8 (first type-s))
		 (add-id8-s id8 (rest type-s))))))


(defun add-parsed (parsed char type)
  (cond ((eq type 'other) parsed)
        ((eq type 'scheme)
	 (change-pos parsed 1 (append (first parsed) (list char))))
        ((eq type 'userinfo)
	 (change-pos parsed 2 (append (second parsed) (list char))))
        ((eq type 'host)
	 (change-pos parsed 3 (append (third parsed) (list char))))
        ((eq type 'port)
	 (change-pos parsed 4 (append (fourth parsed) (list char))))
        ((eq type 'path)
	 (change-pos parsed 5 (append (fifth parsed) (list char))))
	((eq type 'zpath44)
	 (add-parsed parsed char 'path))
	((eq type 'zpath8)
	 (add-parsed parsed char 'path))
        ((eq type 'query)
	 (change-pos parsed 6 (append (sixth parsed) (list char))))
        ((eq type 'fragment)
	 (change-pos parsed 7 (append (seventh parsed) (list char))))))



(defun add-parsed-s (parsed char types)
  (cond ((= (length types) 1)
	 (list (add-parsed parsed char (first types))))
        (t (append (list (add-parsed parsed char (first types)))
		   (add-parsed-s parsed char (rest types))))))


(defun change-pos (list pos value)
  (cond ((null list) ())
        ((= pos 1) (cons value (rest list)))
        (t (cons (first list) (change-pos (rest list) (- pos 1) value)))))


(defun convert (list)
  (cond ((null list) ())
        (t (cons (cond ((null (first list)) nil)
                       (t (coerce (first list) 'string)))
		 (convert (rest list))))))

(defun convert-port (list)
  (cond ((null (fourth list)) (change-pos list 4 80))
        (t (change-pos list 4 (parse-integer (fourth list))))))


(defun delta (state char)
  (cond
   ; URI2
   ; Tel
   ((and (eq state 'start) (eq char #\t)) '((tels0 scheme)))
   ((and (eq state 'tels0) (eq char #\e)) '((tels1 scheme)))
   ((and (eq state 'tels1) (eq char #\l)) '((tels2 scheme)))
   ((and (eq state 'start) (eq char #\T)) '((tels0 scheme)))
   ((and (eq state 'tels0) (eq char #\E)) '((tels1 scheme)))
   ((and (eq state 'tels1) (eq char #\L)) '((tels2 scheme)))

   ; URI1?
   ((and (eq state 'tels0)
	 (not (or (eq char #\e) (eq char #\E)))
	 (identificatore-p char))
    '((s0 scheme)))
   ((and (eq state 'tels1)
	 (not (or (eq char #\L) (eq char #\L)))
	 (identificatore-p char))
    '((s0 scheme)))
   ((and (eq state 'tels2)
	 (not (eq char #\:))
	 (identificatore-p char))
    '((s0 scheme)))

   ; :
   ((and (eq state 'tels2) (eq char #\:)) '((tel0 other)))

   ; Userinfo
   ((and (eq state 'tel0) (identificatore-p char)) '((telu0 userinfo)))
   ((and (eq state 'telu0) (identificatore-p char)) '((telu0 userinfo)))

   ; Fax
   ((and (eq state 'start) (eq char #\f)) '((faxs0 scheme)))
   ((and (eq state 'faxs0) (eq char #\a)) '((faxs1 scheme)))
   ((and (eq state 'faxs1) (eq char #\x)) '((faxs2 scheme)))
   ((and (eq state 'start) (eq char #\F)) '((faxs0 scheme)))
   ((and (eq state 'faxs0) (eq char #\A)) '((faxs1 scheme)))
   ((and (eq state 'faxs1) (eq char #\X)) '((faxs2 scheme)))

   ; UR1?
   ((and (eq state 'faxs0)
	 (not (or (eq char #\a)(eq char #\A)))
	 (identificatore-p char))
    '((s0 scheme)))
   ((and (eq state 'faxs1)
	 (not (or (eq char #\x) (eq char #\X)))
	 (identificatore-p char))
    '((s0 scheme)))
   ((and (eq state 'faxs2)
	 (not (eq char #\:))
	 (identificatore-p char))
    '((s0 scheme)))

   ; :
   ((and (eq state 'faxs2) (eq char #\:)) '((tel0 other)))

   ; Mailto
   ((and (eq state 'start) (eq char #\m)) '((mails0 scheme)))
   ((and (eq state 'mails0) (eq char #\a)) '((mails1 scheme)))
   ((and (eq state 'mails1) (eq char #\i)) '((mails2 scheme)))
   ((and (eq state 'mails2) (eq char #\l)) '((mails3 scheme)))
   ((and (eq state 'mails3) (eq char #\t)) '((mails4 scheme)))
   ((and (eq state 'mails4) (eq char #\o)) '((mails5 scheme)))
   ((and (eq state 'start) (eq char #\M)) '((mails0 scheme)))
   ((and (eq state 'mails0) (eq char #\A)) '((mails1 scheme)))
   ((and (eq state 'mails1) (eq char #\I)) '((mails2 scheme)))
   ((and (eq state 'mails2) (eq char #\L)) '((mails3 scheme)))
   ((and (eq state 'mails3) (eq char #\T)) '((mails4 scheme)))
   ((and (eq state 'mails4) (eq char #\O)) '((mails5 scheme)))

   ; URI1?
   ((and (eq state 'mails0)
	 (not (or (eq char #\a) (eq char #\A)))
	 (identificatore-p char))
    '((s0 scheme)))
   ((and (eq state 'mails1)
	 (not (or (eq char #\i) (eq char #\I)))
	 (identificatore-p char))
    '((s0 scheme)))
   ((and (eq state 'mails2)
	 (not (or (eq char #\l) (eq char #\L)))
	 (identificatore-p char))
    '((s0 scheme)))
   ((and (eq state 'mails3)
	 (not (or (eq char #\t) (eq char #\T)))
	 (identificatore-p char))
    '((s0 scheme)))
   ((and (eq state 'mails4)
	 (not (or (eq char #\o) (eq char #\O)))
	 (identificatore-p char))
    '((s0 scheme)))
   ((and (eq state 'mails5)
	 (not (eq char #\:))
	 (identificatore-p char))
    '((s0 scheme)))

   ; :
   ((and (eq state 'mails5) (eq char #\:)) '((mailto0 other)))

   ; Userinfo
   ((and (eq state 'mailto0) (identificatore-p char)) '((mailu0 userinfo)))
   ((and (eq state 'mailu0) (identificatore-p char)) '((mailu0 userinfo)))

   ; Host
   ((and (eq state 'mailu0) (eq char #\@)) '((mailh0 other)))

   ; IP
   ; N.
   ((and (eq state 'mailh0) (digit-char-between-p char 0 1))
    '((mailh2 host) (mailh4 host) (mailh5 host) (mailh1 host)))
   ((and (eq state 'mailh2) (digit-char-p char)) '((mailh4 host)))
   ((and (eq state 'mailh4) (digit-char-p char)) '((mailh5 host)))
   ((and (eq state 'mailh0) (eq char #\2))
    '((mailh3 host) (mailh4 host) (mailh5 host) (mailh1 host)))
   ((and (eq state 'mailh3) (digit-char-between-p char 0 4))
    '((mailh4 host)))
   ((and (eq state 'mailh3) (eq char #\5)) '((mailh6 host)))
   ((and (eq state 'mailh6) (digit-char-between-p char 0 5))
    '((mailh5 host)))
   ((and (eq state 'mailh0) (digit-char-p char))
    '((mailh4 host) (mailh5 host) (mailh1 host)))
   ((and (eq state 'mailh5) (eq char #\.)) '((mailh7 host)))

   ; N.
   ((and (eq state 'mailh7) (digit-char-between-p char 0 1))
    '((mailh8 host) (mailh9 host) (mailh10 host)))
   ((and (eq state 'mailh8) (digit-char-p char)) '((mailh9 host)))
   ((and (eq state 'mailh9) (digit-char-p char)) '((mailh10 host)))
   ((and (eq state 'mailh7) (eq char #\2))
    '((mailh11 host) (mailh9 host) (mailh10 host)))
   ((and (eq state 'mailh11) (digit-char-between-p char 0 4))
    '((mailh9 host)))
   ((and (eq state 'mailh11) (eq char #\5)) '((mailh12 host)))
   ((and (eq state 'mailh12) (digit-char-between-p char 0 5))
    '((mailh10 host)))
   ((and (eq state 'mailh7) (digit-char-p char))
    '((mailh9 host) (mailh10 host)))
   ((and (eq state 'mailh10) (eq char #\.)) '((mailh13 host)))

   ; N.
   ((and (eq state 'mailh13) (digit-char-between-p char 0 1))
    '((mailh14 host) (mailh15 host) (mailh16 host)))
   ((and (eq state 'mailh14) (digit-char-p char)) '((mailh15 host)))
   ((and (eq state 'mailh15) (digit-char-p char)) '((mailh16 host)))
   ((and (eq state 'mailh13) (eq char #\2))
    '((mailh17 host) (mailh15 host) (mailh16 host)))
   ((and (eq state 'mailh17) (digit-char-between-p char 0 4))
    '((mailh15 host)))
   ((and (eq state 'mailh17) (eq char #\5)) '((mailh18 host)))
   ((and (eq state 'mailh18) (digit-char-between-p char 0 5))
    '((mailh16 host)))
   ((and (eq state 'mailh13) (digit-char-p char))
    '((mailh15 host) (mailh16 host)))
   ((and (eq state 'mailh16) (eq char #\.)) '((mailh19 host)))

   ; N
   ((and (eq state 'mailh19) (digit-char-between-p char 0 1))
    '((mailh20 host) (mailh21 host) (mailh22 host)))
   ((and (eq state 'mailh20) (digit-char-p char)) '((mailh21 host)))
   ((and (eq state 'mailh21) (digit-char-p char)) '((mailh22 host)))
   ((and (eq state 'mailh19) (eq char #\2))
    '((mailh23 host) (mailh21 host) (mailh22 host)))
   ((and (eq state 'mailh23) (digit-char-between-p char 0 4))
    '((mailh21 host)))
   ((and (eq state 'mailh23) (eq char #\5)) '((mailh24 host)))
   ((and (eq state 'mailh24) (digit-char-between-p char 0 5))
    '((mailh22 host)))
   ((and (eq state 'mailh19) (digit-char-p char))
    '((mailh21 host) (mailh22 host)))

   ; Id
   ((and (eq state 'mailh0) (id-host-p char)) '((mailh1 host)))
   ((and (eq state 'mailh1) (id-host-p char)) '((mailh1 host)))
   ((and (eq state 'mailh1) (eq char #\.)) '((mailh25 host)))
   ((and (eq state 'mailh25) (id-host-p char)) '((mailh1 host)))

   ; News
   ((and (eq state 'start) (eq char #\n)) '((newss0 scheme)))
   ((and (eq state 'newss0) (eq char #\e)) '((newss1 scheme)))
   ((and (eq state 'newss1) (eq char #\w)) '((newss2 scheme)))
   ((and (eq state 'newss2) (eq char #\s)) '((newss3 scheme)))
   ((and (eq state 'start) (eq char #\N)) '((newss0 scheme)))
   ((and (eq state 'newss0) (eq char #\E)) '((newss1 scheme)))
   ((and (eq state 'newss1) (eq char #\W)) '((newss2 scheme)))
   ((and (eq state 'newss2) (eq char #\S)) '((newss3 scheme)))

   ; URI?
   ((and (eq state 'newss0)
	 (not (or (eq char #\e) (eq char #\E)))
	 (identificatore-p char))
    '((s0 scheme)))
   ((and (eq state 'newss1)
	 (not (or (eq char #\w) (eq char #\W)))
	 (identificatore-p char))
    '((s0 scheme)))
   ((and (eq state 'newss2)
	 (not (or (eq char #\s) (eq char #\S)))
	 (identificatore-p char))
    '((s0 scheme)))
   ((and (eq state 'newss3)
	 (not (eq char #\:))
	 (identificatore-p char))
    '((s0 scheme)))

   ; :
   ((and (eq state 'newss3) (eq char #\:)) '((news0 other)))

   ; Host
   ((and (eq state 'news0) (digit-char-between-p char 0 1))
    '((mailh2 host) (mailh4 host) (mailh5 host) (mailh1 host)))
   ((and (eq state 'news0) (eq char #\2))
    '((mailh3 host) (mailh4 host) (mailh5 host) (mailh1 host)))
   ((and (eq state 'news0) (digit-char-p char))
    '((mailh4 host) (mailh5 host) (mailh1 host)))
   ((and (eq state 'news0) (id-host-p char)) '((mailh1 host)))

   ; Zos
   ((and (eq state 'start) (eq char #\z)) '((zoss0 scheme)))
   ((and (eq state 'zoss0) (eq char #\o)) '((zoss1 scheme)))
   ((and (eq state 'zoss1) (eq char #\s)) '((zoss2 scheme)))
   ((and (eq state 'start) (eq char #\Z)) '((zoss0 scheme)))
   ((and (eq state 'zoss0) (eq char #\O)) '((zoss1 scheme)))
   ((and (eq state 'zoss1) (eq char #\S)) '((zoss2 scheme)))

   ; URI1?
   ((and (eq state 'zoss0)
	 (not (or (eq char #\o) (eq char #\O)))
	 (identificatore-p char))
    '((s0 scheme)))
   ((and (eq state 'zoss1)
	 (not (or (eq char #\s) (eq char #\S)))
	 (identificatore-p char))
    '((s0 scheme)))
   ((and (eq state 'zoss2)
	 (not (eq char #\:))
	 (identificatore-p char))
    '((s0 scheme)))

   ; :
   ((and (eq state 'zoss2) (eq char #\:)) '((zos0 other)))

   ; Authorithy
   ((and (eq state 'zos0) (eq char #\/))
    '((zosa0 other) (zos1 other)))
   ((and (eq state 'zosa0) (eq char #\/)) '((zosa1 other)))

   ; Userinfo
   ((and (eq state 'zosa1) (digit-char-between-p char 0 1))
    '((zosah2 host) (zosah4 host) (zosah5 host)
      (zosau0 userinfo) (zosah0 host)))
   ((and (eq state 'zosa1) (eq char #\2))
    '((zosah3 host) (zosah4 host) (zosah5 host)
      (zosau0 userinfo) (zosah0 host)))
   ((and (eq state 'zosa1) (digit-char-p char))
    '((zosah4 host) (zosah5 host)
      (zosau0 userinfo) (zosah0 host)))
   ((and (eq state 'zosa1) (id-host-p char))
    '((zosau0 userinfo) (zosah0 host)))
   ((and (eq state 'zosa1) (eq char #\.)) '((zosau0 userinfo)))
   ((and (eq state 'zosau0) (identificatore-p char))
    '((zosau0 userinfo)))
   ((and (eq state 'zosau0) (eq char #\@)) '((zosau1 other)))

   ; Host
   ; IP
   ; N.
   ((and (eq state 'zosau1) (digit-char-between-p char 0 1))
    '((zosah2 host) (zosah4 host) (zosah5 host) (zosah0 host)))
   ((and (eq state 'zosah2) (digit-char-p char)) '((zosah4 host)))
   ((and (eq state 'zosah4) (digit-char-p char)) '((zosah5 host)))
   ((and (eq state 'zosau1) (eq char #\2))
    '((zosah3 host) (zosah4 host) (zosah5 host) (zosah0 host)))
   ((and (eq state 'zosah3) (digit-char-between-p char 0 4))
    '((zosah4 host)))
   ((and (eq state 'zosah3) (eq char #\5)) '((zosah6 host)))
   ((and (eq state 'zosah6) (digit-char-between-p char 0 5))
    '((zosah5 host)))
   ((and (eq state 'zosau1) (digit-char-p char))
    '((zosah4 host) (zosah5 host) (zosah0 host)))
   ((and (eq state 'zosah5) (eq char #\.)) '((zosah7 host)))

   ; N.
   ((and (eq state 'zosah7) (digit-char-between-p char 0 1))
    '((zosah8 host) (zosah9 host) (zosah10 host)))
   ((and (eq state 'zosah8) (digit-char-p char)) '((zosah9 host)))
   ((and (eq state 'zosah9) (digit-char-p char)) '((zosah10 host)))
   ((and (eq state 'zosah7) (eq char #\2))
    '((zosah11 host) (zosah9 host) (zosah10 host)))
   ((and (eq state 'zosah11) (digit-char-between-p char 0 4))
    '((zosah9 host)))
   ((and (eq state 'zosah11) (eq char #\5)) '((zosah12 host)))
   ((and (eq state 'zosah12) (digit-char-between-p char 0 5))
    '((zosah10 host)))
   ((and (eq state 'zosah7) (digit-char-p char))
    '((zosah9 host) (zosah10 host)))
   ((and (eq state 'zosah10) (eq char #\.)) '((zosah13 host)))

   ; N.
   ((and (eq state 'zosah13) (digit-char-between-p char 0 1))
    '((zosah14 host) (zosah15 host) (zosah16 host)))
   ((and (eq state 'zosah14) (digit-char-p char)) '((zosah15 host)))
   ((and (eq state 'zosah15) (digit-char-p char)) '((zosah16 host)))
   ((and (eq state 'zosah13) (eq char #\2))
    '((zosah17 host) (zosah15 host) (zosah16 host)))
   ((and (eq state 'zosah17) (digit-char-between-p char 0 4))
    '((zosah15 host)))
   ((and (eq state 'zosah17) (eq char #\5)) '((zosah18 host)))
   ((and (eq state 'zosah18) (digit-char-between-p char 0 5))
    '((zosah16 host)))
   ((and (eq state 'zosah13) (digit-char-p char))
    '((zosah15 host) (zosah16 host)))
   ((and (eq state 'zosah16) (eq char #\.)) '((zosah19 host)))

   ; N
   ((and (eq state 'zosah19) (digit-char-between-p char 0 1))
    '((zosah20 host) (zosah21 host) (zosah22 host)))
   ((and (eq state 'zosah20) (digit-char-p char)) '((zosah21 host)))
   ((and (eq state 'zosah21) (digit-char-p char)) '((zosah22 host)))
   ((and (eq state 'zosah19) (eq char #\2))
    '((zosah23 host) (zosah21 host) (zosah22 host)))
   ((and (eq state 'zosah23) (digit-char-between-p char 0 4))
    '((zosah21 host)))
   ((and (eq state 'zosah23) (eq char #\5)) '((zosah24 host)))
   ((and (eq state 'zosah24) (digit-char-between-p char 0 5))
    '((zosah22 host)))
   ((and (eq state 'zosah19) (digit-char-p char))
    '((zosah21 host) (zosah22 host)))

   ; Id
   ((and (eq state 'zosau1) (id-host-p char)) '((zosah0 host)))
   ((and (eq state 'zosah0) (id-host-p char)) '((zosah0 host)))
   ((and (eq state 'zosah0) (eq char #\.)) '((zosah1 host)))
   ((and (eq state 'zosah1) (id-host-p char)) '((zosah0 host)))

   ; Port
   ((and (eq state 'zosah0) (eq char #\:)) '((zosap0 other)))
   ((and (eq state 'zosah22) (eq char #\:)) '((zosap0 other)))
   ((and (eq state 'zosap0) (digit-char-p char)) '((zosap1 port)))
   ((and (eq state 'zosap1) (digit-char-p char)) '((zosap1 port)))

   ; Path
   ; Id44
   ((and (eq state 'zosah0) (eq char #\/)) '((zos1 other)))
   ((and (eq state 'zosah22) (eq char #\/)) '((zos1 other)))
   ((and (eq state 'zosap1) (eq char #\/)) '((zos1 other)))
   ((and (eq state 'zos1) (alpha-char-p char))
    '((zospath0 zpath44) (zospath1 zpath44)))
   ((and (eq state 'zospath0) (alpha-char-p char))
    '((zospath0 zpath44) (zospath1 zpath44)))
   ((and (eq state 'zospath0) (digit-char-p char))
    '((zospath0 zpath44) (zospath1 zpath44)))
   ((and (eq state 'zospath0) (eq char #\.)) '((zospath0 zpath44)))

   ; Id8
   ((and (eq state 'zospath1) (eq char #\()) '((zospath2 path)))
   ((and (eq state 'zospath2) (alpha-char-p char)) '((zospath3 zpath8)))
   ((and (eq state 'zospath3) (alpha-char-p char)) '((zospath3 zpath8)))
   ((and (eq state 'zospath3) (digit-char-p char)) '((zospath3 zpath8)))
   ((and (eq state 'zospath3) (eq char #\))) '((zospath4 path)))

   ; Query
   ((and (eq state 'zospath1) (eq char #\?)) '((zosquery0 other)))
   ((and (eq state 'zospath4) (eq char #\?)) '((zosquery0 other)))
   ((and (eq state 'zosquery0) (id-query-p char)) '((zosquery1 query)))
   ((and (eq state 'zosquery1) (id-query-p char)) '((zosquery1 query)))

   ; Fragment
   ((and (eq state 'zosquery1) (eq char #\#)) '((zosfrag0 other)))
   ((and (eq state 'zospath1) (eq char #\#)) '((zosfrag0 other)))
   ((and (eq state 'zospath4) (eq char #\#)) '((zosfrag0 other)))
   ((and (eq state 'zosfrag0)) '((zosfrag1 fragment)))
   ((and (eq state 'zosfrag1)) '((zosfrag1 fragment)))


   ; URI1
   ; Scheme
   ((and (eq state 'start) (identificatore-p char)) '((s0 scheme)))
   ((and (eq state 's0) (identificatore-p char)) '((s0 scheme)))

   ; :
   ((and (eq state 's0) (eq char #\:)) '((q0 other)))

   ; Authorithy
   ((and (eq state 'q0) (eq char #\/)) '((a0 other) (q1 other)))
   ((and (eq state 'a0) (eq char #\/)) '((a1 other)))

   ; Userinfo
   ((and (eq state 'a1) (digit-char-between-p char 0 1))
    '((au0 userinfo) (ah2 host) (ah4 host) (ah5 host) (ah0 host)))
   ((and (eq state 'a1) (eq char #\2))
    '((au0 userinfo) (ah3 host) (ah4 host) (ah5 host) (ah0 host)))
   ((and (eq state 'a1) (digit-char-p char))
    '((au0 userinfo) (ah4 host) (ah5 host) (ah0 host)))
   ((and (eq state 'a1) (id-host-p char)) '((au0 userinfo) (ah0 host)))
   ((and (eq state 'a1) (eq char #\.)) '((au0 userinfo)))
   ((and (eq state 'au0) (identificatore-p char)) '((au0 userinfo)))
   ((and (eq state 'au0) (eq char #\@)) '((au1 other)))

   ; Host
   ; IP
   ; N.
   ((and (eq state 'au1) (digit-char-between-p char 0 1))
    '((ah2 host) (ah4 host) (ah5 host) (ah0 host)))
   ((and (eq state 'au1) (eq char #\2))
    '((ah3 host) (ah4 host) (ah5 host) (ah0 host)))
   ((and (eq state 'ah2) (digit-char-p char)) '((ah4 host)))
   ((and (eq state 'ah4) (digit-char-p char)) '((ah5 host)))
   ((and (eq state 'ah3) (digit-char-between-p char 0 4))
    '((ah4 host)))
   ((and (eq state 'ah3) (eq char #\5)) '((ah6 host)))
   ((and (eq state 'ah6) (digit-char-between-p char 0 5))
    '((ah5 host)))
   ((and (eq state 'au1) (digit-char-p char))
    '((ah4 host) (ah5 host)))
   ((and (eq state 'ah5) (eq char #\.)) '((ah7 host)))

   ; N.
   ((and (eq state 'ah7) (digit-char-between-p char 0 1))
    '((ah8 host) (ah9 host) (ah10 host)))
   ((and (eq state 'ah7) (eq char #\2))
    '((ah11 host) (ah9 host) (ah10 host)))
   ((and (eq state 'ah8) (digit-char-p char)) '((ah9 host)))
   ((and (eq state 'ah9) (digit-char-p char)) '((ah10 host)))
   ((and (eq state 'ah11) (digit-char-between-p char 0 4))
    '((ah9 host)))
   ((and (eq state 'ah11) (eq char #\5)) '((ah12 host)))
   ((and (eq state 'ah12) (digit-char-between-p char 0 5))
    '((ah10 host)))
   ((and (eq state 'ah7) (digit-char-p char))
    '((ah9 host) (ah10 host)))
   ((and (eq state 'ah10) (eq char #\.)) '((ah13 host)))

   ; N.
   ((and (eq state 'ah13) (digit-char-between-p char 0 1))
    '((ah14 host) (ah15 host) (ah16 host)))
   ((and (eq state 'ah13) (eq char #\2))
    '((ah17 host) (ah15 host) (ah16 host)))
   ((and (eq state 'ah14) (digit-char-p char)) '((ah15 host)))
   ((and (eq state 'ah15) (digit-char-p char)) '((ah16 host)))
   ((and (eq state 'ah17) (digit-char-between-p char 0 4))
    '((ah15 host)))
   ((and (eq state 'ah17) (eq char #\5)) '((ah18 host)))
   ((and (eq state 'ah18) (digit-char-between-p char 0 5))
    '((ah16 host)))
   ((and (eq state 'ah13) (digit-char-p char))
    '((ah15 host) (ah16 host)))
   ((and (eq state 'ah16) (eq char #\.)) '((ah19 host)))

   ; N
   ((and (eq state 'ah19) (digit-char-between-p char 0 1))
    '((ah20 host) (ah21 host) (ah22 host)))
   ((and (eq state 'ah7) (eq char #\2))
    '((ah23 host) (ah21 host) (ah22 host)))
   ((and (eq state 'ah20) (digit-char-p char)) '((ah21 host)))
   ((and (eq state 'ah21) (digit-char-p char)) '((ah22 host)))
   ((and (eq state 'ah23) (digit-char-between-p char 0 4))
    '((ah21 host)))
   ((and (eq state 'ah23) (eq char #\5)) '((ah24 host)))
   ((and (eq state 'ah24) (digit-char-between-p char 0 5))
    '((ah22 host)))
   ((and (eq state 'ah19) (digit-char-p char))
    '((ah21 host) (ah22 host)))

   ; Id
   ((and (eq state 'au1) (id-host-p char)) '((ah0 host)))
   ((and (eq state 'ah0) (id-host-p char)) '((ah0 host)))
   ((and (eq state 'ah0) (eq char #\.)) '((ah1 host)))
   ((and (eq state 'ah1) (id-host-p char)) '((ah0 host)))

   ; Port
   ((and (eq state 'ah0) (eq char #\:)) '((ap0 other)))
   ((and (eq state 'ah22) (eq char #\:)) '((ap0 other)))
   ((and (eq state 'ap0) (digit-char-p char)) '((ap1 port)))
   ((and (eq state 'ap1) (digit-char-p char)) '((ap1 port)))

   ; /
   ((and (eq state 'ap1) (eq char #\/)) '((q1 other)))
   ((and (eq state 'ah0) (eq char #\/)) '((q1 other)))
   ((and (eq state 'ah22) (eq char #\/)) '((q1 other)))

   ; Path
   ((and (eq state 'q1) (identificatore-p char)) '((path0 path)))
   ((and (eq state 'path0) (identificatore-p char)) '((path0 path)))
   ((and (eq state 'path0) (eq char #\/)) '((path1 path)))
   ((and (eq state 'path1) (identificatore-p char)) '((path0 path)))

   ; Query
   ((and (eq state 'q1) (eq char #\?)) '((query0 other)))
   ((and (eq state 'path0) (eq char #\?)) '((query0 other)))
   ((and (eq state 'query0) (id-query-p char)) '((query1 query)))
   ((and (eq state 'query1) (id-query-p char)) '((query1 query)))

   ; Fragment
   ((and (eq state 'q1) (eq char #\#)) '((frag0 other)))
   ((and (eq state 'query1) (eq char #\#)) '((frag0 other)))
   ((and (eq state 'path0) (eq char #\#)) '((frag0 other)))
   ((and (eq state 'frag0)) '((frag1 fragment)))
   ((and (eq state 'frag1)) '((frag1 fragment)))
   ))


(defun final-p (state)
  (or (eq state 'q0)
      (eq state 'q1)
      (eq state 'path0)
      (eq state 'ah0)
      (eq state 'ah22)
      (eq state 'ap1)
      (eq state 'query1)
      (eq state 'frag1)
      (eq state 'tel0)
      (eq state 'telu0)
      (eq state 'mailto0)
      (eq state 'mailu0)
      (eq state 'mailh22)
      (eq state 'mailh1)
      (eq state 'news0)
      (eq state 'zos0)
      (eq state 'zosah22)
      (eq state 'zosah0)
      (eq state 'zosap1)
      (eq state 'zospath1)
      (eq state 'zospath4)
      (eq state 'zosquery1)
      (eq state 'zosfrag1)))



(defun identificatore-p (x)
  (not (or (eq x #\/)
           (eq x #\?)
           (eq x #\#)
           (eq x #\@)
           (eq x #\:))))


(defun id-host-p (x)
  (and (identificatore-p x)
       (not (eq x #\.))))


(defun id-query-p (x)
  (not (eq x #\#)))


(defun digit-char-between-p (x l h)
  (let ((n (digit-char-p x)))
    (cond ((null n) nil)
	  (t (and (>= n l)
		  (<= n h))))))
