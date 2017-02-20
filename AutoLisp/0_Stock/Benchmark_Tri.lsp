'("$Id: sort.lsp 1.11 1996/07/18 15:14:07 Reini Exp $")
;;; SORT.LSP
;;;+------------------------------------------------------------------------+
;;;| URBAN X Utilities for AutoCAD                                   Ver 2.9|
;;;| (c) 1991-96 Reinhard URBAN. All rights reserved                        |
;;;| X-Ray, Rechbauerstr. 38, 8010 Graz, Austria, Tel: ++43-316-812646      |
;;;| <rurban@sbox.tu-graz.ac.at>  http://xarch.tu-graz.ac.at/autocad/urban/ |
;;;+------------------------------------------------------------------------+
;;;
;;; Optimized and Benched General Sort Routines:
;;;
;;; MERGE SORT O(n logn): fastest but stack-costly
;;;   merge-sort
;;;   str-sort         from TABLES.LSP (adesk sample)
;;;
;;; QUICK SORT O(n logn): almost as fast as merge-sort
;;;   qsort 	       from lisp2c demo
;;;
;;; BUBBLE SORT O(n^2):
;;;   bub-sort         (cbsort, sort1   from CAD-User 4/92)
;;;
;;; INSERTION SORT O(n^2):
;;;   ins-sort          by Peter Szammer <szammer@sime.com>
;;;
;;; default sort predicate is '<
;;;
;;; no shell or heap sorts so far
;;;
;;; YOU MAY USE THIS FUNCTION AS IT IS FOR ANY PURPOSE
;;; AT YOUR OWN RISK IF YOU RETAIN THIS NOTICE COMPLETE
;;; AND UNALTERED. NO WARRANTIES GIVEN WHATSOEVER.
;;;
;;; sorting 100 elements:
;;;   insertion sort: 20.430023 sec/ 50.06%
;;;   bubble sort   : 15.722076 sec/ 38.53%
;;;   merge sort    :  2.252014 sec/  5.52%
;;;   quick sort    :  2.292969 sec/  5.62%
;;;   vlx-sort      :  0.110016 sec/  0.27% (Vital Lisp internal)

;;;************************************************************************
;;; Merge Sort  O(n logn)
;;;   overall the fastest, but needs a lot of stack
(defun merge-sort (x cmp)
  (cond ((null (cdr x)) x)
        (T (merge cmp
	           (merge-sort (first-half x) cmp)
                   (merge-sort (last-half x) cmp)))))

(defun merge (cmp a b)
  (cond	((null a) b)
	((null b) a)
	((apply cmp (list (car a) (car b)))
	 (cons (car a) (merge cmp (cdr a) b))
	)
	(t
	 (cons (car b) (merge cmp a (cdr b)))
	)
  )
)

;;;************************************************************************
;;; Quick Sort  O(n logn)
;;;   slightly slower than quick sort
;;;   derived from the lisp2c demo version

(defun qsort (lst cmp / x y l e g)
  (if lst
    (progn
      (setq x (nth (/ (length lst) 2) lst))
      (foreach y lst
	(cond
	  ((equal y x)
	    (setq e (cons y e)))
	  ((apply cmp (list y x))
	    (setq l (cons y l)))
	  (T (setq g (cons y g)))
	)
      )
      (append (qsort l cmp) e (qsort g cmp))
    )
  )
)


;;; from  ftp://ftp.netcom.com/pub/hb/hbaker/Share-Unify.html
;|
(defun qs(x)
  (if (null x) nil
    (append (qs1 (low (cdr x) (car x)))
            (cons (car x)
                  (qs2 (high (cdr x) (car x)))))))

(defun high(x i)
  (cond ((null x) nil)
        ((< (car x) i) (high (cdr x) i))
        (t (cons (car x) (high (cdr x) i)))))

(defun low(x i)
  (cond ((null x) nil)
        ((>= (car x) i) (low (cdr x) i))
        (t (cons (car x) (low (cdr x) i)))))
|;

(defun qsort-1 (x cmp)
  (if (null x) nil
    (append (qsort-1 (low (cdr x) (car x) cmp) cmp)
            (cons (car x)
                  (qsort-1 (high (cdr x) (car x) cmp) cmp)))))

(defun high (x i cmp)
  (cond ((null x) nil)
        ((apply cmp (list (car x) i)) (high (cdr x) i cmp))
        (t (cons (car x) (high (cdr x) i cmp)))))

(defun low (x i cmp)
  (cond ((null x) nil)
        ((not (apply cmp (list (car x) i))) (low (cdr x) i cmp))
        (t (cons (car x) (low (cdr x) i cmp)))))



;;;************************************************************************
;;; Insertion Sort  O(n^2)
;;;   fast for already sorted (reversed) lists
(defun ins-sort (L cmp / M N O)
  (setq O L L (list (car O)))
  (while (setq M nil N L O (cdr O))
    (while (and O N (apply cmp (list (car N) (car O))))
      (setq M (append M (list (car N))) N (cdr N))
    )
    (setq N (cons (car O) N) L (append M N))
  )
  L
)

;;;************************************************************************
;;; Bubble Sort O(n^2), iterative
;;;  slowest sort method
(defun bub-sort (lst cmp / new n x)
  (repeat (length lst)
    (setq n (car lst))
    (foreach x lst
      (if (apply cmp (list n x)) (setq n x))	;swap it
    )
    (setq lst (urx-remove n lst)	; this removes an item from a list
          new (cons n new)
    )
  )
  new
)

;;; Bubble Sort O(n^2), recursive
;;;  !! no double elements in lst allowed !!
;;;  slowest sort method
(defun bub-sort-r (lst cmp / x y)
  (setq x (car lst))
  (foreach y (cdr lst)
    (if (apply cmp (list x y)) (setq x y)))
  (if lst
    (append
      (bub-sort-r
        (append
          (cdr (member x lst))
          (cdr (member x (reverse lst)))
        )
        cmp
      )
      (list x)
    )
  )
)

;;;************************************************************************
;;; Helper functions

(defun first-half (l)
  (ur_head l (/ (1- (length l)) 2)))

(defun ur_head (l n)
  (cond ((minusp n) nil)
    (t (cons (car l) (ur_head (cdr l) (1- n))))))

(defun last-half (l)
  (ur_tail l (/ (1- (length l)) 2)))

(defun ur_tail (l n)
  (cond ((minusp n) l)
	(t (ur_tail (cdr l) (1- n)))))

;;;************************************************************************
;;; some examples of compare functions

;;;  STRCMP compares two strings like the clib func
(defun strcmp (a b)
  (cond ((= a b) 0)
        (T (cond ((< (ascii a) (ascii b)) -1)
                 ((> (ascii a) (ascii b))  1)
                 (t (strcmp (substr a 2) (substr b 2)))))))

(defun cmpstr (x1 x2) (> (strcmp x1 x2) 0))
(defun cmpnum (x1 x2) (> x1 x2))
(defun cmppts (x1 x2) (> (car x1) (car x2)))

;;;************************************************************************
;;; Detailed benchmarks:

(defun c:bench10 (/ start overall sum n l r)
  (setq l '(0 1 2 3 4 5 6 7 8 9))	;sorted data
  (setq r '(4 6 0 7 9 2 3 5 7 8))	;random data
  (setq start (ur_start) *bench* nil n 200)
  (ur_benchfunc '(ins-sort l '<) n)	;sorted
  (ur_benchfunc '(ins-sort l '>) n)	;reverse
  (ur_benchfunc '(ins-sort r '<) n)	;random
  (ur_benchfunc '(bub-sort l '<) n)
  (ur_benchfunc '(bub-sort l '>) n)
  (ur_benchfunc '(bub-sort r '<) n)
  (ur_benchfunc '(merge-sort l '<) n)
  (ur_benchfunc '(merge-sort l '>) n)
  (ur_benchfunc '(merge-sort r '<) n)
  (ur_benchfunc '(qsort l '<) n)
  (ur_benchfunc '(qsort l '>) n)
  (ur_benchfunc '(qsort r '<) n)
  (ur_benchfunc '(vlx-sort l '<) n)
  (ur_benchfunc '(vlx-sort l '>) n)
  (ur_benchfunc '(vlx-sort r '<) n)
  (setq *bench* (reverse *bench*))
  (result (- (* 86400.0 (getvar "DATE")) start))
)

;|
->
Benchmark results for 10 elements:
(INS-SORT L '<) 200 times: 3.27502 sec  	;sorted
(INS-SORT L '>) 200 times: 0.701019 sec  	;reverse
(INS-SORT R '<) 200 times: 2.47403 sec  	;random

(BUB-SORT L '<) 200 times: 2.79398 sec
(BUB-SORT L '>) 200 times: 2.664 sec
(BUB-SORT R '<) 200 times: 2.50397 sec

(MERGE-SORT L '<) 200 times: 1.91299 sec
(MERGE-SORT L '>) 200 times: 1.71295 sec
(MERGE-SORT R '<) 200 times: 2.08298 sec

(QSORT L '<) 200 times: 2.09302 sec
(QSORT L '>) 200 times: 2.06299 sec
(QSORT R '<) 200 times: 2.45297 sec

(VLX-SORT L '<) 200 times: 0.230957 sec
(VLX-SORT L '>) 200 times: 0.230011 sec
(VLX-SORT R '<) 200 times: 0.230988 sec

Benchmark results
Weighted sums:
  insertion sort: 7.099976 sec/ 24.52%
  bubble sort   : 8.422028 sec/ 29.08%
  merge sort    : 6.119080 sec/ 21.13%
  quick sort    : 6.608978 sec/ 22.82%
  internal qsort: 0.710999 sec/ 2.46%
|;

(defun c:bench100 (/ start overall sum n l r)
  (setq l (ur_intlst nil 100))		;sorted data
  (setq r (ur_randomize l))		;random data
  (setq start (ur_start) *bench* nil n 5)
  (ur_benchfunc '(ins-sort l '<) n)	;sorted
  (ur_benchfunc '(ins-sort l '>) n)	;reverse
  (ur_benchfunc '(ins-sort r '<) n)	;random
  (ur_benchfunc '(bub-sort l '<) n)
  (ur_benchfunc '(bub-sort l '>) n)
  (ur_benchfunc '(bub-sort r '<) n)
  (ur_benchfunc '(merge-sort l '<) n)
  (ur_benchfunc '(merge-sort l '>) n)
  (ur_benchfunc '(merge-sort r '<) n)
  (ur_benchfunc '(qsort l '<) n)
  (ur_benchfunc '(qsort l '>) n)
  (ur_benchfunc '(qsort r '<) n)
  (ur_benchfunc '(vlx-sort l '<) n)
  (ur_benchfunc '(vlx-sort l '>) n)
  (ur_benchfunc '(vlx-sort r '<) n)
  (setq *bench* (reverse *bench*))
  (result (- (* 86400.0 (getvar "DATE")) start))
)

;|
->
Benchmark results for 100 elements:

(INS-SORT L (QUOTE <)) 5 times: 14.06 sec  	;sorted
(INS-SORT L (QUOTE >)) 5 times: 0.281036 sec   ;reverse
(INS-SORT R (QUOTE <)) 5 times: 6.08899 sec  	;random

(BUB-SORT L (QUOTE <)) 5 times: 5.74802 sec
(BUB-SORT L (QUOTE >)) 5 times: 4.88702 sec
(BUB-SORT R (QUOTE <)) 5 times: 5.08704 sec

(MERGE-SORT L (QUOTE <)) 5 times: 0.681 sec
(MERGE-SORT L (QUOTE >)) 5 times: 0.630005 sec
(MERGE-SORT R (QUOTE <)) 5 times: 0.94101 sec

(QSORT L (QUOTE <)) 5 times: 0.740967 sec
(QSORT L (QUOTE >)) 5 times: 0.730988 sec
(QSORT R (QUOTE <)) 5 times: 0.821014 sec

(VLX-SORT L (QUOTE <)) 5 times: 0.0299683 sec
(VLX-SORT L (QUOTE >)) 5 times: 0.0300293 sec
(VLX-SORT R (QUOTE <)) 5 times: 0.0500183 sec

Benchmark results
Weighted sums:
  insertion sort: 20.430023 sec/ 50.06%
  bubble sort   : 15.722076 sec/ 38.53%
  merge sort    :  2.252014 sec/  5.52%
  quick sort    :  2.292969 sec/  5.62%
  internal qsort:  0.110016 sec/  0.27%

Sum     : 40.8071 sec
Overall : 43.563 sec
Overhead: 2.75589 sec   6.32622% mostly (gc)
|;

(defun result (overall / sum times)
  (princ "\nBenchmark results")
  (setq times (mapcar 'cdr *bench*))
  (setq sum (apply '+ times))
  (princ "\nWeighted sums:")
  (princ "\n  insertion sort: ") (rslt (apply '+ (urx-head times 3)) sum)
  (princ "\n  bubble sort   : ") (rslt (apply '+ (urx-head (urx-tail times 3) 3)) sum)
  (princ "\n  merge sort    : ") (rslt (apply '+ (urx-head (urx-tail times 6) 3)) sum)
  (princ "\n  quick sort    : ") (rslt (apply '+ (urx-head (urx-tail times 9) 3)) sum)
  (princ "\n  internal qsort: ") (rslt (apply '+ (urx-tail times 12)) sum)
  (urx-write
    "\nSum     : " sum " sec"
    "\nOverall : " overall " sec"
    "\nOverhead: " (- overall sum) " sec   "
                  (* 100.0 (/ (- overall sum) overall))"%"
                  " mostly (gc)"
                  )
  (princ)
)

(defun percent (x sum)
  (* 100.0 (/ x sum)))
(defun rslt (x sum)
  (princ (strcat (rtos x 2 6) " sec/ " (rtos (percent x sum) 2 2) "%")))