;;; cal-korea.el --- calendar functions for the Korean Lunar calendar

;; Copyright (C) 1995, 1997, 2001, 2002, 2003, 2004, 2005, 2006, 2007
;;   Free Software Foundation, Inc.

;; Author: Jaemok Jeong <jmjeong@gmail.com> [2007-12-07]
;; Based on cal-china.el (Modified for Korean Lunar Calendar)
;; Keywords: calendar
;; Human-Keywords: Korean Lunar calendar, calendar, holidays, diary
;; -----------------------------------------------------------------
;; cal-china.el
;;
;; Original Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Original Maintainer: Glenn Morris <rgm@gnu.org>

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This collection of functions implements the features of calendar.el,
;; diary.el, and holidays.el that deal with the Korean calendar.  The rules
;; used for the Korean calendar are those of Baolin Liu (see L. E. Doggett's
;; article "Calendars" in the Explanatory Supplement to the Astronomical
;; Almanac, second edition, 1992) for the calendar as revised at the beginning
;; of the Qing dynasty in 1644.  The nature of the astronomical calculations
;; is such that precise calculations cannot be made without great expense in
;; time, so that the calendars produced may not agree perfectly with published
;; tables--but no two pairs of published tables agree perfectly either!  Liu's
;; rules produce a calendar for 2033 which is not accepted by all authorities.
;; The date of Korean New Year is correct from 1644-2051.

;; Technical details of all the calendrical calculations can be found in
;; ``Calendrical Calculations: The Millennium Edition'' by Edward M. Reingold
;; and Nachum Dershowitz, Cambridge University Press (2001).

;;; Code:

(defvar date)
(defvar displayed-month)
(defvar displayed-year)

(require 'lunar)

(defvar korean-calendar-celestial-stem
  ["갑" "을" "병" "정" "무" "기" "경" "신" "임" "계"])

(defvar korean-calendar-terrestrial-branch
  ["자" "축" "인" "묘" "진" "사" "오" "미" "신" "유" "술" "해"])

(defcustom korean-calendar-time-zone
  '(if (< year 1928)
       (+ 465 (/ 40.0 60.0))
     540)
  "*Number of minutes difference between local standard time for Korean Lunar
calendar and Coordinated Universal (Greenwich) Time.  Default is for ㄶㅕㅊㄷㅈ.
This is an expression in `year' since it changed at 1928-01-01 00:00:00 from
UT+7:45:40 to UT+9."
  :type 'sexp
  :group 'korean-calendar)

(defcustom korean-calendar-location-name "Seoul"
  "*Name of location used for calculation of Korean Lunar calendar."
  :type 'string
  :group 'korean-calendar)

(defcustom korean-calendar-daylight-time-offset 0
										; The correct value is as follows, but the Korean Lunar calendrical
										; authorities do NOT use DST in determining astronomical events:
										;  60
  "*Number of minutes difference between daylight saving and standard time
for Korean Lunar calendar.  Default is for no daylight saving time."
  :type 'integer
  :group 'korean-calendar)

(defcustom korean-calendar-standard-time-zone-name
  '(if (< year 1928)
       "PMT"
     "KST")
  "*Abbreviated name of standard time zone used for Korean Lunar calendar.
This is an expression depending on `year' because it changed
at 1928-01-01 00:00:00 from `PMT' to `KST'."
  :type 'sexp
  :group 'korean-calendar)

(defcustom korean-calendar-daylight-time-zone-name "KDT"
  "*Abbreviated name of daylight saving time zone used for Korean Lunar calendar."
  :type 'string
  :group 'korean-calendar)

(defcustom korean-calendar-daylight-savings-starts nil
										; The correct value is as follows, but the Korean Lunar calendrical
										; authorities do NOT use DST in determining astronomical events:
										;  '(cond ((< 1986 year) (calendar-nth-named-day 1 0 4 year 10))
										;         ((= 1986 year) '(5 4 1986))
										;         (t nil))
  "*Sexp giving the date on which daylight saving time starts for Korean Lunar
calendar.  Default is for no daylight saving time.  See documentation of
`calendar-daylight-savings-starts'."
  :type 'sexp
  :group 'korean-calendar)

(defcustom korean-calendar-daylight-savings-ends nil
										; The correct value is as follows, but the Korean Lunar calendrical
										; authorities do NOT use DST in determining astronomical events:
										;  '(if (<= 1986 year) (calendar-nth-named-day 1 0 9 year 11))
  "*Sexp giving the date on which daylight saving time ends for Korean Lunar
calendar.  Default is for no daylight saving time.  See documentation of
`calendar-daylight-savings-ends'."
  :type 'sexp
  :group 'korean-calendar)

(defcustom korean-calendar-daylight-savings-starts-time 0
  "*Number of minutes after midnight that daylight saving time starts for
Korean Lunar calendar.  Default is for no daylight saving time."
  :type 'integer
  :group 'korean-calendar)

(defcustom korean-calendar-daylight-savings-ends-time 0
  "*Number of minutes after midnight that daylight saving time ends for
Korean Lunar calendar.  Default is for no daylight saving time."
  :type 'integer
  :group 'korean-calendar)

(defcustom korean-calendar-print-long-description nil
  "*If this variable set to t, long description will be displayed"
  :type 'boolean
  :group 'korean-calendar)

(defun korean-zodiac-sign-on-or-after (d)
  "Absolute date of first new Zodiac sign on or after absolute date d.
The Zodiac signs begin when the sun's longitude is a multiple of 30 degrees."
  (let* ((year (extract-calendar-year
				(calendar-gregorian-from-absolute d)))
		 (calendar-time-zone (eval korean-calendar-time-zone))
		 (calendar-daylight-time-offset
		  korean-calendar-daylight-time-offset)
         (calendar-standard-time-zone-name
          korean-calendar-standard-time-zone-name)
         (calendar-daylight-time-zone-name
          korean-calendar-daylight-time-zone-name)
         (calendar-calendar-daylight-savings-starts
          korean-calendar-daylight-savings-starts)
         (calendar-daylight-savings-ends
          korean-calendar-daylight-savings-ends)
         (calendar-daylight-savings-starts-time
          korean-calendar-daylight-savings-starts-time)
         (calendar-daylight-savings-ends-time
          korean-calendar-daylight-savings-ends-time))
	(floor
	 (calendar-absolute-from-astro
	  (solar-date-next-longitude
	   (calendar-astro-from-absolute d)
	   30)))))

(defun korean-new-moon-on-or-after (d)
  "Absolute date of first new moon on or after absolute date d."
  (let* ((year (extract-calendar-year
                (calendar-gregorian-from-absolute d)))
         (calendar-time-zone (eval korean-calendar-time-zone))
         (calendar-daylight-time-offset
          korean-calendar-daylight-time-offset)
         (calendar-standard-time-zone-name
          korean-calendar-standard-time-zone-name)
         (calendar-daylight-time-zone-name
          korean-calendar-daylight-time-zone-name)
         (calendar-calendar-daylight-savings-starts
          korean-calendar-daylight-savings-starts)
         (calendar-daylight-savings-ends
          korean-calendar-daylight-savings-ends)
         (calendar-daylight-savings-starts-time
          korean-calendar-daylight-savings-starts-time)
         (calendar-daylight-savings-ends-time
          korean-calendar-daylight-savings-ends-time))
    (floor
     (calendar-absolute-from-astro
      (lunar-new-moon-on-or-after
       (calendar-astro-from-absolute d))))))

(defvar korean-year-cache
  '((1990 (12 726464) (1 726494) (2 726523) (3 726553) (4 726582) (5 726611)
		  (5.5 726641) (6 726670) (7 726699) (8 726729) (9 726759) (10 726788)
		  (11 726818))
	(1991 (12 726848) (1 726878) (2 726907) (3 726937) (4 726966) (5 726995)
		  (6 727025) (7 727054) (8 727083) (9 727113) (10 727142) (11 727172))
	(1992 (12 727202) (1 727232) (2 727261) (3 727291) (4 727321) (5 727350)
		  (6 727379) (7 727409) (8 727438) (9 727467) (10 727497) (11 727526))
	(1993 (12 727556) (1 727586) (2 727615) (3 727645) (3.5 727675) (4 727704)
		  (5 727734) (6 727763) (7 727793) (8 727822) (9 727851) (10 727881)
		  (11 727910))
	(1994 (12 727940) (1 727969) (2 727999) (3 728029) (4 728059) (5 728088)
		  (6 728118) (7 728147) (8 728177) (9 728206) (10 728235) (11 728265))
	(1995 (12 728294) (1 728324) (2 728353) (3 728383) (4 728413) (5 728442)
		  (6 728472) (7 728502) (8 728531) (8.5 728561) (9 728590) (10 728620)
		  (11 728649))
	(1996 (12 728678) (1 728708) (2 728737) (3 728767) (4 728796) (5 728826)
		  (6 728856) (7 728885) (8 728915) (9 728944) (10 728974) (11 729004))
	(1997 (12 729033) (1 729063) (2 729092) (3 729121) (4 729151) (5 729180)
		  (6 729210) (7 729239) (8 729269) (9 729299) (10 729328) (11 729358))
	(1998 (12 729388) (1 729417) (2 729447) (3 729476) (4 729505) (5 729535)
		  (5.5 729564) (6 729593) (7 729623) (8 729653) (9 729682) (10 729712)
		  (11 729742))
	(1999 (12 729772) (1 729801) (2 729831) (3 729860) (4 729889) (5 729919)
		  (6 729948) (7 729977) (8 730007) (9 730036) (10 730066) (11 730096))
	(2000 (12 730126) (1 730155) (2 730185) (3 730215) (4 730244) (5 730273)
		  (6 730303) (7 730332) (8 730361) (9 730391) (10 730420) (11 730450))
	(2001 (12 730480) (1 730509) (2 730539) (3 730569) (4 730599) (4.5 730628)
		  (5 730657) (6 730687) (7 730716) (8 730745) (9 730775) (10 730804)
		  (11 730834))
	(2002 (12 730863) (1 730893) (2 730923) (3 730953) (4 730982) (5 731012)
		  (6 731041) (7 731071) (8 731100) (9 731129) (10 731159) (11 731188))
	(2003 (12 731218) (1 731247) (2 731277) (3 731307) (4 731336) (5 731366)
		  (6 731396) (7 731425) (8 731455) (9 731484) (10 731513) (11 731543))
	(2004 (12 731572) (1 731602) (2 731631) (2.5 731661) (3 731690) (4 731720)
		  (5 731750) (6 731779) (7 731809) (8 731838) (9 731868) (10 731897)
		  (11 731927))
	(2005 (12 731956) (1 731986) (2 732015) (3 732045) (4 732074) (5 732104)
		  (6 732133) (7 732163) (8 732193) (9 732222) (10 732252) (11 732282))
	(2006 (12 732311) (1 732340) (2 732370) (3 732399) (4 732429) (5 732458)
		  (6 732488) (7 732517) (7.5 732547) (8 732576) (9 732606) (10 732636)
		  (11 732665))
	(2007 (12 732695) (1 732725) (2 732754) (3 732783) (4 732813) (5 732842)
		  (6 732871) (7 732901) (8 732930) (9 732960) (10 732990) (11 733020))
	(2008 (12 733049) (1 733079) (2 733109) (3 733138) (4 733167) (5 733197)
		  (6 733226) (7 733255) (8 733285) (9 733314) (10 733344) (11 733374))
	(2009 (12 733403) (1 733433) (2 733463) (3 733493) (4 733522) (5 733551)
		  (5.5 733581) (6 733610) (7 733639) (8 733669) (9 733698) (10 733728)
		  (11 733757))
	(2010 (12 733787) (1 733817) (2 733847) (3 733876) (4 733906) (5 733935)
		  (6 733965) (7 733994) (8 734023) (9 734053) (10 734082) (11 734112))
	(2011 (12 734141) (1 734171) (2 734201) (3 734230) (4 734260) (5 734290)
		  (6 734319) (7 734349) (8 734378) (9 734407) (10 734437) (11 734466))
	(2012 (12 734496) (1 734525) (2 734555) (3 734584) (3.5 734614) (4 734644)
		  (5 734674) (6 734703) (7 734733) (8 734762) (9 734791) (10 734821)
		  (11 734850))
	(2013 (12 734880) (1 734909) (2 734939) (3 734968) (4 734998) (5 735028)
		  (6 735057) (7 735087) (8 735116) (9 735146) (10 735175) (11 735205))
	(2014 (12 735234) (1 735264) (2 735293) (3 735323) (4 735352) (5 735382)
		  (6 735411) (7 735441) (8 735470) (9 735500) (9.5 735530) (10 735559)
		  (11 735589)))
  "An assoc list of Korean Lunar year structures as determined by `korean-year'.

Values are computed as needed, but to save time, the initial value consists
of the precomputed years 1990-2014.  The code works just as well with this
set to nil initially (which is how the value for 1990-2014 was computed).")

(defun korean-year (y)
  "The structure of the Korean Lunar year for Gregorian year Y.
The result is a list of pairs (i d), where month i begins on absolute date d,
of the Korean Lunar months from the Korean Lunar month following the solstice in
Gregorian year Y-1 to the Korean Lunar month of the solstice of Gregorian year Y.

The list is cached for further use."
  (let ((list (cdr (assoc y korean-year-cache))))
    (if (not list)
        (progn
          (setq list (compute-korean-year y))
          (setq korean-year-cache
                (append korean-year-cache (list (cons y list))))))
    list))

(defun number-korean-months (list start)
  "Assign month numbers to the lunar months in LIST, starting with START.
Numbers are assigned sequentially, START, START+1, ..., 11, with half
numbers used for leap months.

First month of list will never be a leap month, nor will the last."
  (if list
      (if (zerop (- 12 start (length list)))
          ;; List is too short for a leap month
          (cons (list start (car list))
                (number-korean-months (cdr list) (1+ start)))
        (cons
         ;; First month
         (list start (car list))
         ;; Remaining months
         (if (and (cdr (cdr list)) ;; at least two more months...
                  (<= (car (cdr (cdr list)))
                      (korean-zodiac-sign-on-or-after (car (cdr list)))))
             ;; Next month is a leap month
             (cons (list (+ start 0.5) (car (cdr list)))
                   (number-korean-months (cdr (cdr list)) (1+ start)))
           ;; Next month is not a leap month
           (number-korean-months (cdr list) (1+ start)))))))

(defun korean-month-list (start end)
  "List of starting dates of Korean Lunar months from START to END."
  (if (<= start end)
      (let ((new-moon (korean-new-moon-on-or-after start)))
        (if (<= new-moon end)
            (cons new-moon
                  (korean-month-list (1+ new-moon) end))))))

(defun compute-korean-year (y)
  "Compute the structure of the Korean Lunar year for Gregorian year Y.
The result is a list of pairs (i d), where month i begins on absolute date d,
of the Korean Lunar months from the Korean Lunar month following the solstice in
Gregorian year Y-1 to the Korean Lunar month of the solstice of Gregorian year Y."
  (let* ((next-solstice (korean-zodiac-sign-on-or-after
                         (calendar-absolute-from-gregorian
                          (list 12 15 y))))
         (list (korean-month-list (1+ (korean-zodiac-sign-on-or-after
									   (calendar-absolute-from-gregorian
										(list 12 15 (1- y)))))
								  next-solstice))
         (next-sign (korean-zodiac-sign-on-or-after (car list))))
    (if (= (length list) 12)
        ;; No room for a leap month, just number them 12, 1, 2, ..., 11
        (cons (list 12 (car list))
              (number-korean-months (cdr list) 1))
      ;; Now we can assign numbers to the list for y
      ;; The first month or two are special
      (if (or (> (car list) next-sign) (>= next-sign (car (cdr list))))
          ;; First month on list is a leap month, second is not
          (append (list (list 11.5 (car list))
                        (list 12 (car (cdr list))))
                  (number-korean-months (cdr (cdr list)) 1))
        ;; First month on list is not a leap month
        (append (list (list 12 (car list)))
                (if (>= (korean-zodiac-sign-on-or-after (car (cdr list)))
                        (car (cdr (cdr list))))
                    ;; Second month on list is a leap month
                    (cons (list 12.5 (car (cdr list)))
                          (number-korean-months (cdr (cdr list)) 1))
                  ;; Second month on list is not a leap month
                  (number-korean-months (cdr list) 1)))))))

(defun calendar-absolute-from-korean (date)
  "The number of days elapsed between the Gregorian date 12/31/1 BC and DATE.
The Gregorian date Sunday, December 31, 1 BC is imaginary."
  (let* ((cycle (car date))
         (year (car (cdr date)))
         (month (car (cdr (cdr date))))
         (day (car (cdr (cdr (cdr date)))))
         (g-year (+ (* (1- cycle) 60) ;; years in prior cycles
                    (1- year)		  ;; prior years this cycle
                    -2636)))		  ;; years before absolute date 0
    (+ (1- day)						  ;; prior days this month
       (car
        (cdr ;; absolute date of start of this month
         (assoc month (append (memq (assoc 1 (korean-year g-year))
                                    (korean-year g-year))
                              (korean-year (1+ g-year)))))))))

(defun calendar-korean-from-absolute (date)
  "Compute Korean Lunar date (cycle year month day) corresponding to absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((g-year (extract-calendar-year
                  (calendar-gregorian-from-absolute date)))
         (c-year (+ g-year 2695))
         (list (append (korean-year (1- g-year))
                       (korean-year g-year)
                       (korean-year (1+ g-year)))))
    (while (<= (car (cdr (car (cdr list)))) date)
      ;; the first month on the list is in Korean Lunar year c-year
      ;; date is on or after start of second month on list...
      (if (= 1 (car (car (cdr list))))
          ;; second month on list is a new Korean Lunar year
          (setq c-year (1+ c-year)))
      ;; ...so first month on list is of no interest
      (setq list (cdr list)))
    (list (/ (1- c-year) 60)
		  (calendar-mod c-year 60)
          (car (car list))
          (1+ (- date (car (cdr (car list))))))))

(defun holiday-korean-new-year ()
  "Date of Korean Lunar New Year."
  (let ((m displayed-month)
        (y displayed-year))
    (increment-calendar-month m y 1)
    (if (< m 5)
        (let ((korean-new-year
               (calendar-gregorian-from-absolute
                (car (cdr (assoc 1 (korean-year y)))))))
          (if (calendar-date-is-visible-p korean-new-year)
			  (list
			   (list korean-new-year
					 (format "Korean Lunar New Year (%s)"
							 (calendar-korean-sexagesimal-name (+ y 57))))))))))

(defun calendar-korean-date-string (&optional date)
  "String of Korean Lunar date of Gregorian DATE.
Defaults to today's date if DATE is not given."
  (let* ((a-date (calendar-absolute-from-gregorian
                  (or date (calendar-current-date))))
         (c-date (calendar-korean-from-absolute a-date))
         (cycle (car c-date))
         (year (car (cdr c-date)))
         (month (car (cdr (cdr c-date))))
         (day (car (cdr (cdr (cdr c-date)))))
         (this-month (calendar-absolute-from-korean
                      (list cycle year month 1)))
         (next-month (calendar-absolute-from-korean
                      (list (if (= year 60) (1+ cycle) cycle)
                            (if (= (floor month) 12) (1+ year) year)
                            (calendar-mod (1+ (floor month)) 12)
                            1)))
         (m-cycle (% (+ (* year 5) (floor month)) 60)))
	(if korean-calendar-print-long-description
		(format "%s년 %02d월 %02d일 (%s) %s년 %s %s일"
				(+ (* (1- cycle) 60) ;; years in prior cycles
				   (1- year)		 ;; prior years this cycle
				   -2636)			 ;; years before absolute date 0
				(floor month)
				day
				(if (not (integerp month))
					"윤달"
				  "평달")
				(calendar-korean-sexagesimal-name year)
				(if (integerp month)
					(format "%s월" (calendar-korean-sexagesimal-name
									(+ (* 12 year) month 50)))
				  "")
				(calendar-korean-sexagesimal-name (+ a-date 15)))
	  (format "%s%s/%02d/%02d"
			  (if (not (integerp month))
				  "윤) "
				"")
			  (+ (* (1- cycle) 60)	 ;; years in prior cycles
				 (1- year)			 ;; prior years this cycle
				 -2636)				 ;; years before absolute date 0
			  (floor month)
			  day ))))
	
(defun calendar-korean-sexagesimal-name (n)
  "The N-th name of the Korean Lunar sexagesimal cycle.
N congruent to 1 gives the first name, N congruent to 2 gives the second name,
..., N congruent to 60 gives the sixtieth name."
  (format "%s%s"
          (aref korean-calendar-celestial-stem (% (1- n) 10))
          (aref korean-calendar-terrestrial-branch (% (1- n) 12))))

(defun calendar-print-korean-date ()
  "Show the Korean Lunar date equivalents of date."
  (interactive)
  (message "Computing Korean Lunar date...")
  (message "Korean Lunar date: %s"
           (calendar-korean-date-string (calendar-cursor-to-date t))))

(defun calendar-goto-korean-date (date &optional noecho)
  "Move cursor to Korean date DATE.
Echo Korean date unless NOECHO is t."
  (interactive
   (let* ((c (jm-calendar-korean-from-absolute
              (calendar-absolute-from-gregorian
               (calendar-current-date))))
          (input-year (calendar-read
					   "Year in Korean (>0): "
					   '(lambda (x) (< 0 x) )
					   (int-to-string (extract-calendar-year c))))
		  (c-year (+ input-year 2636))
		  (cycle (1+ (/ c-year 60)))
		  (year (calendar-mod (1+ c-year) 60))
		  (month-list (make-korean-month-assoc-list
					   (korean-months cycle year)))
		  (month (cdr (assoc
					   (completing-read "Korean calendar month: "
										month-list nil t)
					   month-list)))
		  (last (if (= month
					   (car (cdr (cdr
								  (calendar-korean-from-absolute
								   (+ 29
									  (calendar-absolute-from-korean
									   (list cycle year month 1))))))))
					30
				  29))
		  (day (calendar-read
				(format "Korean calendar day (1-%d): " last)
				'(lambda (x) (and (<= 1 x) (<= x last))))))
	 (list (list cycle year month day))))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-korean date)))
  (or noecho (calendar-print-korean-date)))

(defun korean-months (c y)
  "A list of the months in cycle C, year Y of the Korean Lunar calendar."
  (let* ((l (memq 1 (append
                     (mapcar '(lambda (x)
                                (car x))
                             (korean-year (extract-calendar-year
										   (calendar-gregorian-from-absolute
											(calendar-absolute-from-korean
											 (list c y 1 1))))))
                     (mapcar '(lambda (x)
                                (if (> (car x) 11) (car x)))
                             (korean-year (extract-calendar-year
										   (calendar-gregorian-from-absolute
											(calendar-absolute-from-korean
											 (list (if (= y 60) (1+ c) c)
												   (if (= y 60) 1 y)
												   1 1))))))))))
    l))

(defun make-korean-month-assoc-list (l)
  "Make list of months L into an assoc list."
  (if (and l (car l))
      (if (and (cdr l) (car (cdr l)))
          (if (= (car l) (floor (car (cdr l))))
              (append
               (list (cons (format "%s (평달)" (car l)) (car l))
                     (cons (format "%s (윤달)" (car l)) (car (cdr l))))
               (make-korean-month-assoc-list (cdr (cdr l))))
            (append
             (list (cons (int-to-string (car l)) (car l)))
             (make-korean-month-assoc-list (cdr l))))
        (list (cons (int-to-string (car l)) (car l))))))

(defun diary-korean-date ()
  "Korean Lunar calendar equivalent of date diary entry."
  (format "음력: %s" (calendar-korean-date-string date)))


(defun jm-calendar-absolute-from-korean (date)
  "The number of days elapsed between the Gregorian date 12/31/1 BC and DATE.
The Gregorian date Sunday, December 31, 1 BC is imaginary."
  (let* ((month (car date))
         (day (car (cdr date)))
         (year (car (cdr (cdr date))))
         (g-year year))	;; years before absolute date 0
    (+ (1- day)			;; prior days this month
       (car
        (cdr ;; absolute date of start of this month
         (assoc month (append (memq (assoc 1 (korean-year g-year))
                                    (korean-year g-year))
                              (korean-year (1+ g-year)))))))))

(defun jm-calendar-korean-from-absolute (l-date)
  "Compute Korean Lunar date (cycle year month day) corresponding to absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((g-year (extract-calendar-year
                  (calendar-gregorian-from-absolute l-date)))
         (c-year (+ g-year 2695))
         (list (append (korean-year (1- g-year))
                       (korean-year g-year)
                       (korean-year (1+ g-year))))
		 cycle mod-year)
    (while (<= (car (cdr (car (cdr list)))) l-date)
      ;; the first month on the list is in Korean Lunar year c-year
      ;; date is on or after start of second month on list...
      (if (= 1 (car (car (cdr list))))
          ;; second month on list is a new Korean Lunar year
          (setq c-year (1+ c-year)))
      ;; ...so first month on list is of no interest
      (setq list (cdr list)))
	(setq cycle (/ (1- c-year) 60))
	(setq mod-year (calendar-mod c-year 60))
    (list
	 (car (car list))
	 (1+ (- l-date (car (cdr (car list)))))
	 (+ (* (1- cycle) 60)		 ;; years in prior cycles
		(1- mod-year)			 ;; prior years this cycle
		-2636)					 ;; years before absolute date 0
	 )))

(defun diary-lunar-date (month day year &optional leap mark)
  "Specific date(s) diary entry.
Entry applies if date is MONTH, DAY, YEAR if `european-calendar-style' is nil,
and DAY, MONTH, YEAR if `european-calendar-style' is t.  DAY, MONTH, and YEAR
can be lists of integers, the constant t, or an integer.  The constant t means
all values.

An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar."
  (let* ((dd (if european-calendar-style
				 month
			   day))
		 (mm (if european-calendar-style
				 day
			   month))
		 (mm (+ mm (if leap 0.5 0)))
		 (c-date (calendar-absolute-from-gregorian
				  (or date (calendar-current-date))))
		 (a-date (jm-calendar-korean-from-absolute c-date))
		 
		 (m (extract-calendar-month a-date))
		 (y (extract-calendar-year a-date))
		 (d (extract-calendar-day a-date)))
	(if (and
		 (or (and (listp dd) (memq d dd))
			 (equal d dd)
			 (eq dd t))
		 (or (and (listp mm) (memq m mm))
			 (equal m mm)
			 (eq mm t))
 		 (or (and (listp year) (memq y year))
 			 (equal y year)
 			 (eq year t)))
        (cons mark (format entry (format "음력 %d월 %d일" m d))))))


(defun diary-lunar-anniversary (month day &optional year leap mark)
  "Anniversary diary entry.
Entry applies if date is the anniversary of MONTH, DAY, YEAR if
`european-calendar-style' is nil, and DAY, MONTH, YEAR if
`european-calendar-style' is t.  Diary entry can contain `%s' or `%s%d'; the
%d will be replaced by the number of years since the MONTH DAY, YEAR and the
%s will be replaced by lunar date.

An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar."
  (let* ((d (if european-calendar-style
                month
              day))
         (m (if european-calendar-style
                day
              month))
		 (m (+ m (if leap 0.5 0)))
		 (c-date (calendar-absolute-from-gregorian
				  (or date (calendar-current-date))))
		 (a-date (jm-calendar-korean-from-absolute c-date))
		 (mm (extract-calendar-month a-date))
		 (yy (extract-calendar-year a-date))
		 (dd (extract-calendar-day a-date))
         (diff (if (not (booleanp year)) (- yy year) 100)))
	(if (and (> diff 0) (calendar-date-equal (list m d yy) (list mm dd yy)))
		(cons mark (format entry (format "음력 %d월 %d일" m d) diff)))))

(eval-after-load "calendar"
  '(progn
	 (define-key calendar-mode-map "pl" 'calendar-print-korean-date)
	 (define-key calendar-mode-map "gl" 'calendar-goto-korean-date)
	 ))

(provide 'cal-korea)

;;; cal-korea.el ends here
