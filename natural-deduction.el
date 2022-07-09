(defcustom tlt-nd-stars "Sterne"
  "Name of the column to be numbered.")

(defcustom tlt-nd-rowname "Zeile"
  "Name of the column to be numbered.")

(defcustom tlt-nd-reference "Bezug"
  "Name of the column containing numbered tlt-nd-references.")

(defcustom tlt-nd-rule "Regel"
  "Name of the column to be numbered.")

(defcustom tlt-nd-formulas "Formel"
  "Name of the column containing the actual tlt-nd-formulas.")

(defcustom tlt-nd-hyp "Hyp"
  "Name of the assertion tlt-nd-rule.")

(defcustom tlt-nd-capitalize t
  "Whether to capitalize rules automatically when aligning the table.")

(defvar tlt-nd-colnames nil
  "Column names of table last analyzed.")

(defun get-colnames ()
  "Return an alist whose members are of the form (COLNUMNAME . STRING).
              STRING is the content of a cell, whitespaces trimmed off."
  (org-table-analyze)
  (let ((beg (org-table-begin))
        (end (org-table-end))
        (counter 1)
        (colnums org-table-current-ncol)
        (org-table-allow-automatic-line-recalculation nil) ; because it is done with the ctrl-c-ctrl-c-hook later anyway
        (org-table-automatic-realign nil))
    (setq tlt-nd-colnames nil)
    (save-excursion
      (goto-char beg)
      (while (<= counter colnums)
        (org-table-next-field)
        (push (cons counter
                    (string-trim               ; remove trailing/leading whitespace and
                     (substring-no-properties  ; remove org-properties
                      (org-table-get-field)))) ; of the current cell's content
              tlt-nd-colnames)
        (setq counter (1+ counter))))
    (reverse tlt-nd-colnames)))

(defun get-nth-char-in-string (n string)
  "Return Nth char in STRING."
  (substring string (1- n) n))


(defun org-table-get-notrim (row column)
  "Works just like `org-table-get' but does not trim off whitespace.
                    Needed for whitespaces to later be converted to '-'."
  (setq column (or column (org-table-current-column)))
  (save-excursion
    (and (or (not row) (org-table-goto-line row))
         (org-table-get-field column))))

(defun capitalize-first-char (string)
  "Return STRING, first char capitalized.
  Return zero-length strings as-is."
  (if (equal string "") ""
    (concat (upcase (substring string 0 1)) (substring string 1))))

(defun get-column (column lines &optional trim)
  "From the table at hand, return an alist whose elements are of the form
                    (ROWNUMBER . FIELD) for column no. COLUMN until LINES. If TRIM is 'left,
                  'right or 'both, trim respectively. If TRIM is 'stars,
              remove the first whitespace left and replace the remaining whitespaces by '-'.
        Else do not trim at all. If `tlt-nd-capitalize' is t, also upcase the first letter
        of each field."
  (let ((row 2) ; row counter starts at 2
        (lineno (+ lines 2)) ; actual lines start here
        (fields))     ; empty list
    (while (< row lineno)
      (let* ((field-bare (substring-no-properties (org-table-get-notrim row column)))
             (rownumber (1- row))
             (field-processed

              ;; value of field depending on trim argument ;;

              (cond  ((equal (string-trim field-bare) "") (string-trim field-bare)) ; contract all whitespace-only strings to zero-length strings
                     ((equal trim 'stars)
                      (s-replace-regexp " " "-"
                                        (string-remove-prefix " "
                                                              (string-trim-right field-bare)))) ; make "   +" "--+"
                     ((equal trim 'both) (string-trim field-bare))
                     ((equal trim 'left) (string-trim-left field-bare))
                     ((equal trim 'right) (string-trim-right field-bare))
                     (t field-bare)))

             (field (if tlt-nd-capitalize
                        (capitalize-first-char field-processed)
                      (field-processed)))

             (element (list rownumber field)))
        (push element fields))
      (setq row (1+ row)))

    (reverse fields))) ; because the first field is pushed first and thus the last element

(defun abstract-column (col1 col2 string)
  "Return the subset of COL1 consisting of (ROWNUM FIELDCOL1) such that
        FIELDCOL2 in (ROWNUM FIELDCOL2) matches STRING. Return an error message
      if FIELDCOL1 does not contain exactly one occurrence of '+'."
  (let ((counter 1)
        (max (length col1)))
    (let ((list))
      (while (<= counter max)
        (let* ((starlist (nth (1- counter) col1))       ; for example (2 "-+")
               (starstring (nth 1 starlist))                  ; for example "Iv"
               (starnumber (nth 0 starlist))
               (rulelist (nth (1- counter) col2))            ; for example (1 "Iv")
               (rulestring (nth 1 rulelist)))                       ; for example "Iv"

          (when (equal rulestring string)
            (let ((how-many-stars (cl-count ?+ starstring))) ; how many occurrences of "+"
              (if (= how-many-stars 1)
                  (push starlist list)
                (error "The hypothesis in row %s has %s star%s in it. Fix before re-aligning, please"
                       starnumber
                       (if (equal how-many-stars 0) "no" how-many-stars)
                       (if (equal how-many-stars 0) "" "s"))))))
        (setq counter (1+ counter)))
      (reverse list))))

(defun replace-string-in-list (old new list)
  "Return the result of replacing OLD by NEW in LIST."
  (let ((outlist))
    (dolist (element list outlist)
      (if (equal element old)
          (push new outlist)
        (push element outlist)))
    (reverse outlist)))

(defun order-list-string-int (list)
  "RETURN LIST ordered by the numbers as strings which are its members."
  (let* ((unquoted (mapcar #'string-to-number list)) ; important: (string-to-number "m3") returns 0!
         (sorted (sort unquoted #'<=)))
    (mapcar #'int-to-string sorted)))

(defun order-string (string)
  "RETURN the STRING containing numbers ordered by these numbers."
  (let* ((clean (gnus-strip-whitespace                     ; raw numstring
                 (substring-no-properties string)))                 ; with properties removed
         (list (split-string clean ","))
         (sorted (order-list-string-int list))
         (sorted-string (string-join sorted ",")))
    (if (equal sorted-string "0") "-"
      sorted-string)))

(defun adjust-numbers ()
  "Adjust numbers."
  (let* ((cols (get-colnames))                                               ; (... (2. "Zeile") ... (4. "Bezug"))
         (ref (car (rassoc tlt-nd-reference cols)))                             ; 2 aus (2 . "Zeile")
         (row (car (rassoc tlt-nd-rowname cols)))                               ; 4 aus (4 . "Bezug")
         (lines (length org-table-dlines))                               ; Anzahl Zeilen (+1)
         (org-table-allow-automatic-line-recalculation nil) ; because it is done with the ctrl-c-ctrl-c-hook later anyway
         (org-table-automatic-realign nil)
         (counter 2))                                                    ; first counter to loop over row name; starts with two because of the headline

    ;; Looping over the ROW numbers ;;

    (while (< counter lines)
      (let* ((rownum (1- counter))                                       ; -1 becaues of the headline
             (rownum-str (int-to-string rownum))                         ; 1 → "1"
             (actnum (string-trim
                      (substring-no-properties
                       (org-table-get counter row))))                    ; the actual (possibly wrong) row number (trimmed off whitespaces)
             (counter2 2))                                               ; second counter to loop over tlt-nd-references list



        (unless (equal (int-to-string rownum) actnum)                    ; if the row number is a wrong one (convert counter bc actnum is a string)
          (org-table-put counter row rownum-str)                         ; set it to the right one (-1 because of headline)

          ;; Looping over the TLT-ND-REFERENCE numbers ;;                       ; change all the wrong numbers to the right ones in the tlt-nd-reference column

          (while (< counter2 lines)
            (let* ((numstring (gnus-strip-whitespace                     ; raw numstring
                               (substring-no-properties                   ; with properties removed
                                (org-table-get counter2 ref))))            ; of tlt-nd-reference cell in string from: "REF1,REF2,REF3"
                   (refnums (split-string numstring ","))                ; tlt-nd-reference cell in form ("REF1" "REF2" "REF3")

                   (wrongnum actnum)                                     ; the wrong number is the actual number in string form (under the above condition)
                   (rightnum rownum-str)                                 ; the right number is the rownumber
                   (rightnum-marked (concat "m" rightnum))               ; the right number marked with "m", e.g. "m2"

                   (refnums-right-list
                    (replace-string-in-list wrongnum rightnum-marked refnums))      ; replace all occurrences of the wrongnum in refnums by the marked rightnum

                   (refnums-right-str (string-join refnums-right-list "," )))       ; and make a string out of it

              (unless (or                                               ; unless the ref field is empty (else the ref field would get the row number of the empty field)
                       (equal numstring "")                             ; that is the empty string
                       (equal numstring "-"))                           ; or "-"
                (org-table-put counter2 ref refnums-right-str)          ; put the new string back in the field
                (message "counter2: %s, refnums: %s, refnums-right-list: %s, refnums-right-str: %s" counter2 refnums refnums-right-list refnums-right-str))

              (setq counter2 (1+ counter2)))))                           ; in any case, whether changed or not, increment the counter by 1


        (setq counter (1+ counter))))   ; whether the tlt-nd-reference numbers were changed or not: increment the counter by 1

    ;; Lastly, after every tlt-nd-reference cell has been changed, remove the marks and sort them ;;

    (let ((counter3 2))                                                   ; create a third and last counter
      (while (< counter3 lines)                                           ; and loop over every row
        (let* ((marked (substring-no-properties
                        (org-table-get counter3 ref)))
               (unmarked (replace-regexp-in-string "m" "" marked))
               (sorted (order-string unmarked))
               (forms (car (rassoc tlt-nd-formulas cols))))

          (unless (equal (org-table-get counter3 forms) "")  ; unless the formula cell is empty
            (org-table-put counter3 ref sorted)))  ; put the new number in there

        (setq counter3 (1+ counter3))))))

(add-hook 'org-ctrl-c-ctrl-c-hook #'adjust-numbers)

(defun get-by-nth (list n value)
  "Get member of LIST whose Nth member is VALUE.
  N is 1-based."
  (let ((max (length list))
        (counter 1)
        (result))
    (while (not (or result (> counter max))) ; until we have a result or the counter is at max
      (let ((element (nth (1- counter) list))) ; -1 because `nth' is 0-based
        (if (equal (nth (1- n) element) value)
            ;; if the nth value of element is the value we're looking for ;;

            ;; set the counter to max so that it stops ;;
            ;; and return the element                  ;;
            (progn
              (setq result element)
              (setq counter (1+ max)))
          (setq counter (1+ counter)))))

    ;; if it is not, increment the counter by 1 ;;
    (if result result
      ;; If it is at max, return an error message ;;
      (error "No member of %s whose %s%s value is %s"
             list n
             (let* ((num (int-to-string n))
                    (digits (length num))
                    (lastdigit (get-nth-char-in-string digits num)))
               (cond ((equal lastdigit "1") "st")
                     ((equal lastdigit "2") "nd")
                     ((equal lastdigit "3") "rd")
                     (t "th")))
             value))))

;;; Example
(setq a '((1 2 3) (a 5 7) (b 3 9)))
(setq n 1)
(get-by-nth a 2 5)

(defun fill-stars (starlist)
  "Fill the stars fields of STARLIST after their last star with occurrences of '-' until the maximum star level.
For example, if the maximum level is 4 and the field string is '-+', fill to '-+--'."
  (let ((value 1)
        (newlist nil))
    ;; get length ;;
    (dolist (element starlist value)
      (let* ((star (nth 1 element))
             (length (length star)))
        (when (> length value)
          (setq value length))))

    (dolist (element starlist newlist)
      (let* ((row (nth 0 element))
             (star (nth 1 element))
             (length (length star))
             (diff (- value length))
             (changed (concat star (make-string diff ?-))))
        (push (list row changed) newlist))
      newlist)))

(defun get-hypcorrects ()
  "For the table at point, return a list whose members are of the form (ROW OLD NEW).
    ROW is the row in which the hypothesis is, OLD the position of the star in that hypothesis
    and NEW the position where the star should be."
  (let* ((cols (get-colnames))                         ; (... (2. "Zeile") ... (4. "Bezug"))
         (rulecol (car (rassoc tlt-nd-rule cols)))     ; rulename column, returns 5
         (lines (length org-table-dlines))             ; number of lines +1
         (row (- lines 2))                             ; because the first star row is line 2
         (starcol (car (rassoc tlt-nd-stars cols)))    ; star column, returns 1
         (stars (get-column starcol row 'stars))       ; stars
         (starcounter (length stars))                  ; number of stars for looping
         (rules (get-column rulecol row 'both))        ; rules
         (case-fold-search nil)                        ; relevant if tlt-nd-capitalize is nil
         (hyps (abstract-column stars rules tlt-nd-hyp))
         (star-level-right 1)
         (hypcorrects nil)                             ; info about how the hypotheses got replaced
         (lockedrows nil)                             ; info about how the rows not to be touched
         (changed-stars stars)
         (how-many-hyps (length hyps)))                ; the number of hypotheses - where the loop should stop


    ;;  At this point, we have three important lists. An example:  ;;
    ;;  STARS: (setq stars '((1 "-+")  (2 "++") (3 "+")))          ;;
    ;;  RULES: (setq rules '((1 "Hyp") (2 "$E\\to$") (3 "Hyp")))   ;;
    ;;  HYPS:  (setq hyps '((1 "-+") (3 "+")))                     ;;

    ;; First step: Check whether the hypothesis in question is the right level.
    ;; Since we have trimmed the hypothesis strings correctly, its length corresponds
    ;; to its level. Since we ordered the hypothesis list, too, the index says of which
    ;; level the hypothesis should be: (1 HYP) must be of length 1, (2 HYP) of length 2 etc. ;;
    ;; This is why `star-level-right' is both the index and the star level we need  ;;


    (while (<= star-level-right how-many-hyps)
      (let* ((star-level-sub (1- star-level-right)) ; because we use `nth' and `nth' is zero-based
             (hypnum (nth star-level-sub hyps))     ; the hypothesis of number `star-level-right'; e.g. (1 "+"), (2 "-+") etc.;
             (hyprow (nth 0 hypnum))                ; the hypothesis row number; e.g. 1, 2, etc.
             (hypstring (nth 1 hypnum))             ; the star string of the hypothesis, e.g. "+" from (1 "+")
             (star-level-act (length hypstring)))   ; the ACTUAL star-level; possibly wrong; explanation above

        ;; If the number of characters in the hypothesis string is identical ;;
        ;; with the number of the counter `star-level-right', move on.       ;;

        (unless (equal star-level-act star-level-right)

          ;; If that is not the case, make a hypothesis correction list ;;
          ;; Form: (HYPNUMBER OLD NEW). Example: (2 3 2) ;;
          ;; hypothesis in row 2, is level 3, should be level 2

          (let ((wrong-star (1+ (string-match "+" hypstring))) ; +1 because string-match is zero-based
                (right-star star-level-right)
                (minuses (1- star-level-right))) ; because we need one "-" less than level; e.g. "-+", one minus, level 2!

            ;; now create the hypothesis correction list            ;;
            ;; and change the hypotheses to be of the correct level ;;
            ;; (this can be done directly)                          ;;

            (push (list wrong-star right-star) hypcorrects)
            ;; try: do not change the hypothesis correction list ;;
            (setf (nth 1 (assoc hyprow changed-stars))
                  (format "%s+" (make-string minuses ?-)))))
        (push hyprow lockedrows))

      (setq star-level-right (1+ star-level-right)))

    (list (fill-stars changed-stars)
          (reverse hypcorrects)
          (reverse lockedrows)
          ))) ; reverse because first item is

(defun replace-stars (wrong-star right-star str rightplus)
  "Replace '+' at position WRONG-STAR in STR by '-' and, if RIGHTPLUS it non-nil,
  replace '-' at position RIGHT-STAR in STR by '+'."
  ;; replace the wrong-star by a "-"
  (let ((out (replace-regexp-in-string  ; replace any "+" at level star-level-act
              (format "\\(^[\\+\\-]\\{%d\\}\\)\\(\\+\\)\\([\\+\\|\\-]*\\)" (1- wrong-star)) ; 1- because we exclude them so the first char is the star level

              ;;  first group: all occurrences of "+" or "-" until, excluding, `right-star' - anything before
              ;;  second group: exactly one occurrence of "+" or "-": the one to replace  ;;
              ;;  third group: zero or more occurrences of "+" or "-"  - anything after              ;;
              "\\1-\\3" ; the wrong star is replaced by a "-"
              ;;  first group, second group replaced by ";%d;", third group          ;;
              str)))
    ;; set the right-star to be "+" if that is not already the case ;;
    (if rightplus out
      (replace-regexp-in-string  ; replace any "+" at level star-level-act by ";star-amount;" if it exists
       (format "\\(^[\\+\\-]\\{%d\\}\\)\\([\\+\\|\\-]\\)\\([\\+\\|\\-]*\\)" (1- right-star)) ; 1- because we exclude them so the first char is the star level

       ;;  first group: all occurrences of "+" or "-" until, excluding, `right-star' - anything before
       ;;  second group: exactly one occurrence of "+" or "-": the one to replace  ;;
       ;;  third group: zero or more occurrences of "+" or "-"  - anything after              ;;
       "\\1+\\3" ; the wrong star is replaced by a "-"
       ;;  first group, second group replaced by ";%d;", third group          ;;
       out))))

(defun delete-star (wrong-star str)
  "Replace '+' at position WRONG-STAR in STR by '-' and, if RIGHTPLUS it non-nil,
  replace '-' at position RIGHT-STAR in STR by '+'."
  ;; replace the wrong-star by a "-"
  (replace-regexp-in-string  ; replace any "+" at level star-level-act
   (format "\\(^[\\+\\-]\\{%d\\}\\)\\(\\+\\)\\([\\+\\|\\-]*\\)" (1- wrong-star)) ; 1- because we exclude them so the first char is the star level

   ;;  first group: all occurrences of "+" or "-" until, excluding, `right-star' - anything before
   ;;  second group: exactly one occurrence of "+" or "-": the one to replace  ;;
   ;;  third group: zero or more occurrences of "+" or "-"  - anything after              ;;
   "\\1-\\3" ; the wrong star is replaced by a "-"
   ;;  first group, second group replaced by ";%d;", third group          ;;
   str))

(defun add-star (right-star str)
  "Replace '+' at position WRONG-STAR in STR by '-' and, if RIGHTPLUS it non-nil,
  replace '-' at position RIGHT-STAR in STR by '+'."
  (replace-regexp-in-string  ; replace any "+" at level star-level-act by ";star-amount;" if it exists
   (format "\\(^[\\+\\-]\\{%d\\}\\)\\([\\+\\|\\-]\\)\\([\\+\\|\\-]*\\)" (1- right-star)) ; 1- because we exclude them so the first char is the star level

   ;;  first group: all occurrences of "+" or "-" until, excluding, `right-star' - anything before
   ;;  second group: exactly one occurrence of "+" or "-": the one to replace  ;;
   ;;  third group: zero or more occurrences of "+" or "-"  - anything after              ;;
   "\\1+\\3" ; the wrong star is replaced by a "-"
   ;;  first group, second group replaced by ";%d;", third group          ;;
   str))

(replace-stars 2 3 "++-+" nil) ; remove star at pos 2, add star at pos 3

(defun correct-stars (hypinfo)
  "Returns the correct list of stars for every line based on HYPINFO.
          HYPINFO is a list containing the lists CHANGESTARS and HYPCORRECTS. CHANGESSTARS's members are of the form
          (ROWNUMBER STARSRING), where RUWONUMBER is the rownumber the hypothesis appears
          and STARSTRING is the star field ocurring in that line. An example is (3 '-+').
          The hypotheses are already corrected, but the rest of the star fields is not.
          HYPCORRECTS's members are of the form (ROWNUMBER OLD NEW) where ROWNUMBER is the number the
          hypothesis to be changed is in, OLD the current star level and NEW the correct star level.

        Meant to take the value of `get-hypcorrects' as its argument."
  (let* ((starlist (nth 0 hypinfo))         ; the list of stars, hypotheses already corrected
         (hypcorrections (nth 1 hypinfo))   ; the list of hypothesis corrections
         (corrections hypcorrections)       ; the correction list starts with the hypotheses
         (locked-rows (nth 2 hypinfo))      ; the list of stars not to be altered anymore
         (rowamount (1+ (length starlist))) ; the number of star rows
         (changed-stars starlist)
         (corrected-stars)
         (row 1))

    (while (< row rowamount)  ; for every row

      (dolist (correct corrections)
        (let* ((wrong-star (nth 0 correct))   ; the wrong position of the star, e.g. 2 for "-+-"
               (right-star (nth 1 correct))  ; the correct position of the star, e.g. 3 for "-+-"
               (star (nth (- rowamount row 1) changed-stars))       ; take its star info e.g. (4 "-+-") | since the stars are orderd by row
               (starstring (nth 1 star))                ; and extract the star string, e.g. "-+-"
               (replace (equal (get-nth-char-in-string wrong-star starstring) "+")) ; whether at the position to replace, there already is a star
               (starfield (assoc row corrected-stars)))

          (cond ((member row locked-rows) nil)
                ((not replace) (setf (nth (- rowamount row 1) changed-stars) (list row (delete-star wrong-star starstring))))

                (t (let* ((del-star (delete-star wrong-star starstring))
                          (finish-star (add-star right-star del-star)))
                     (setf (nth (- rowamount row 1) changed-stars) (list row (add-star right-star starstring)
                                                                         )))))))

      (setq row (1+ row)))

    changed-stars)) ; return the changed starlis
