
(progn
    (setq *halfSnap*** 2.5)
    (setq *Snap*** 5.0)
   )
;ME_Loader Insert Block by Tony Masutti, bonus lisp files by T. Harman 2019-08-12 
;=============================================
;these InsBLK and InsLftRgt came from Tony Masutti
; version 2019-01-31
;=============================================
;
(Defun **InsBLK** (BKName  Layer   snap      /
		      p1	p2	  elst	    e2	      oldSnap
		      *prevError* osf3 *error*
		     )
;;;Program to Insert 1 block 
;;;Designed By "Anthony Masutti for Diversified Systems 03-2018
;;;Updated 06-07-2018 -Fixed Onsnap always on Bug
;;;Note this is not a command. Its a function
;;;Typical Calling Macro = (**InsBlk** "DIV_DEV-TOPHAT" "TA-DIAG-DEVC" 0.125)
(VL-LOAD-COM)

  
  (Defun chlay (Elst Layer)
    (if (not (eq elst (entlast)))
		(progn
		  (setq e2  (entlast))
		  (vlax-put (vlax-ename->vla-object e2) 'layer layer)
		)
	      )
   )
  
  (if (< (getvar "OSMODE") 16383)
    (PROGN
      (SETQ  osf3 "ON")
      (setvar "OSMODE" (+ (getvar "OSMODE") 16384))
    )
  )
  (setq p2 (list 0.0 0.0))
  (setq elst (entlast))
  (if (not (tblsearch "Layer" layer))
    (alert (strcat "Layer " layer " does not Exist! Exiting"))
    (progn
      (setq oldSnapUnit (getvar "snapunit"))
      (setq oldCmdEcho (getvar "cmdecho"))
      (setq attReq (getvar "attreq"))
      (setq oldsnapMode (getvar "snapmode"))
      (setvar "snapmode" 1)
      (setvar "attreq" 0)
      (setvar "cmdecho" 0)
      
      (setvar "SnapUnit" (list snap snap))
      
      (setq *prevError* *error*)
      (defun *error* (msg)
	(setvar "attreq" attreq)
	(setvar "snapUnit" oldSnapUnit)
	(setvar "cmdecho" oldCmdEcho)
	(if (= osf3 "ON")
	 (setvar "osmode" (- (getvar "osmode") 16384))
	)
	(setvar "snapmode" oldsnapMode)
	(IF (not (EQ elst (entlast)))
	  (entdel (entlast))
	)
	(setq *error* *prevError*)
	(princ msg)
      )

             (command "._insert" BKName "s" "1" "R" 0 PAUSE)
	
	      (chlay elst layer)
	    
      (setvar "snapUnit" oldSnapUnit)
      (setvar "snapmode" oldsnapMode)
      (if (= osf3 "ON")
	 (setvar "osmode" (- (getvar "osmode") 16384))
	)
      (setvar 'insname "")
      (setvar "cmdecho" oldCmdEcho)
      (setvar "attreq" attreq)
      (setq *error* *prevError*)
      
 
  )
  )
  )
;************************************************************************************
;*                        Insert Left and Right                                     *
;************************************************************************************

(Defun **InsLftRgt** (lftBKName	rgtBkName Layer	    snap      /
		      p1	p2	  elst	    e2	      oldSnap
		      *prevError* osf3 *error*
		     )
;;;Program to Insert 1 block from 2 choices determined by a Directional second point
;;;Designed By "Anthony Masutti for Diversified Systems 02-2018
;;;Updated 06-07-2018 -Fixed Onsnap always on Bug
;;;Updated 06-26-2018 Commented out recursion
;;;Note: This is Not a Command its just a function.
;;;Typical Calling Macro = ^C^C(defun c:*L1*()(**InsLftRgt** "DIV_IO-CONN-IN" "DIV_IO-CONN-OUT" "TA-DIAG-CABL" 0.125));*L1*;
(VL-LOAD-COM)

  (Defun chlay (Elst Layer)
    (if (not (eq elst (entlast)))
		(progn
		  (setq e2 (entlast))
		 (vlax-put (vlax-ename->vla-object e2) 'layer layer)
		)
	      )
   )
  
  (if (< (getvar "OSMODE") 16383)
    (progn
      (setq osf3 "ON")
      (setvar "OSMODE" (+ (getvar "OSMODE") 16384))
    )
  )
  (setq p2 (list 0.0 0.0))
  (setq elst (entlast))
  (if (not (tblsearch "Layer" layer))
    (alert (strcat "Layer " layer " does not Exist! Exiting"))
    (progn
      (setq oldSnapUnit (getvar "snapunit"))
      (setq oldCmdEcho (getvar "cmdecho"))
      (setq attReq (getvar "attreq"))
      (setq oldsnapMode (getvar "snapmode"))
      (setvar "snapmode" 1)
      (setvar "attreq" 0)
      (setvar "cmdecho" 0)
      (setvar "SnapUnit" (list snap snap))
      (setq *prevError* *error*)
      (defun *error* (msg)
	(setvar "attreq" attreq)
	(setvar "snapUnit" oldSnapUnit)
	(setvar "cmdecho" oldCmdEcho)
	(if (= osf3 "ON")
	 (setvar "osmode" (- (getvar "osmode") 16384))
	)
	(setvar "snapmode" oldsnapMode)
	(IF (not (EQ elst (entlast)))
	  (entdel (entlast))
	)
	
	(setq *error* *prevError*)
	(princ msg)
      )
      (if (= (strcase lftBKName) (strcase rgtBkName))
	(progn
	  (setq p1 (cadr (grread t)))
	  (command "insert" lftBKName PAUSE "" "")
	  
	  (chlay elst layer)
	)
	(progn
       
	  (setq p1 (getpoint "\nEnter Origin point for Block "))
	  
       	  (setq p2 (getpoint "\nEnter point to Show Direction "))
				
       ;;;If left and right block names are the same it doesn't matter which is chosen
	  (if (not (equal p1 p2 0.0001))
	    (progn
	      (if (> (car p1) (car p2))
		(progn
		  (if (= (substr lftBKName 1 1) "*")
                    (command "._insert" lftBKName P1 "" "")
		    (command "._insert" lftBKName "s" "1" "R" 0 P1  )
                   )
		)
		(progn
		   (if (= (substr rgtBkName 1 1) "*")
                     (command "._insert" rgtBkName  p1 "" "")
		     (command "._insert" rgtBkName "s" "1" "R" 0 p1 )
                   )
		)
	      )
	      (chlay elst layer)
	    )
	    (alert "you can't pick the same point to show direction")
	  )
	)
      )
      
    
    (setvar "snapUnit" oldSnapUnit)
      (setvar "cmdecho" oldCmdEcho)
      (setvar "attreq" attreq)
      (if (= osf3 "ON")
	 (setvar "osmode" (- (getvar "osmode") 16384))
      )
      (setvar "snapmode" oldsnapMode)
     (setq *error* *prevError*)
  
;;;  (initget 128
;;;	   "Move Copy Line PLine CIrcle Stretch Insert Hatch Bhatch Dist DImension 
;;;	   DText Text MText XRef MAtchprop Erase eXplode BReak BSave CHange CHAmfer
;;;	   STAndards DIst MEasure MLeader MLEadeStyle MView Offset Fillet OPtions
;;;	   OSnap PRoperties Pan Rotate ARray ATtEdit BLock BEdit textEDit ELlipse
;;;	   LAyer LISt LineType"
;;;  )
;;;  (setq answ (getkword "\nCommand:"))
;;;  (if (not answ)
;;;    (**INSLFTRGT** lftBKName rgtBkName layer snap)
;;;    (COMMAND ANSW)
;;;    )
  
  )
  )
  )
;
;
;===========================================
;Changes dynamic block paramter of last block inserted
;===========================================
;
(defun chvis3 (e1 propname Vstate / obj I v varval safelist tot)
  ;;;Program to change visibility State Or Any Property in a dynamic block
  ;;;Anthony Masutti 06-20-2019
;
;=========================================================
;Example syntax to use this program
;(chvis3 (ENTLAST) "[parameter name]" "[parameter value]")
;(chvis3 (ENTLAST) "Shows Rack & Break" "5. No Location / No Break")
;
;========================================================
;
  (vl-Load-com)
  (setq	obj  (vlax-ename->vla-object e1))
  (setq v (vla-getdynamicblockproperties obj))
  (setq varVal (vlax-variant-value v))
  
    (if (>= (vlax-safearray-get-U-Bound VarVal 1) 0)
      (progn
      (setq safeList  (vlax-safearray->list varVal))
      (setq tot  (length safeList))
      (setq i 0)
      
      (while (< i tot)
        (if	(= (strcase (vlax-get-property (nth i safeList) "PropertyName")) (strcase propname))
          (progn
   	    (vlax-put-property (nth i safeList) "Value" vState)
          )
        )
    (setq i (1+ i))
  )
  )
 )
(princ)  
)
;
;
;==========================
; Utility Snap-Ortho On Off Gizmos
;==========================
;
;++++++++++++++++++++++++++++
;Sets ortho on, snap on, snap = .125
;++++++++++++++++++++++++++++
;
(defun snapmeo ()
    (setvar "cmdecho" 1)
            (setq t-ortho (getvar "orthomode"))
            (setq t-smode (getvar "snapmode"))
            (setq t-s (getvar "snapunit"))
                 (setvar "orthomode" 1)
                 (setvar "snapmode" 1)
                 (princ (strcat "Snap Set To: " (rtos *snap*** 2 3) "," (rtos *snap*** 2 3)))
                 (setvar "snapunit" (list *Snap*** *Snap***))
           ;*XXX*** Variables are defined in Div_aids.lsp
    (princ))
;
;++++++++++++++++++++++++++++++++++++++++++++++
;Resets ortho on/off, snap on/off snap value to previous state
;++++++++++++++++++++++++++++++++++++++++++++++
;
(defun snapmeo-r ()
     (setvar "cmdecho" 1)
     (setvar "orthomode" t-ortho)
     (setvar "snapmode" t-smode)
     (princ "Snap Reset To User Pref ")(setvar "snapunit" t-s)
(princ))
;
;
;
;++++++++++++++++++++++++++++++++++++++
;Sets ortho on, snap on, snap = .125, osnaps off
;++++++++++++++++++++++++++++++++++++++
;
(defun snapmeoo ()
    (setvar "cmdecho" 1)
            (setq t-ortho (getvar "orthomode"))
            (setq t-smode (getvar "snapmode"))
            (setq t-s (getvar "snapunit"))
            (setq t-ox (getvar "osmode"))
                 (setvar "orthomode" 1)
                 (setvar "snapmode" 1)
                 (princ (strcat "Snap Set To: " (rtos *snap*** 2 3) "," (rtos *snap*** 2 3)));(princ "Snap Set To: 0.125,0.125 ")
                 (setvar "snapunit" (list *Snap*** *Snap***))
                 (setvar "osmode" 0)
    (princ))
;
;
;++++++++++++++++++++++++++++++++++++++++++++++++++++++
;Resets ortho on/off, snap on/off restores snap value & retores osnaps
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
(defun snapmeoo-r ()
     (setvar "cmdecho" 1)
     (setvar "orthomode" t-ortho)
     (setvar "snapmode" t-smode)
     (setvar "osmode" t-ox)
     (princ "Snap Reset To User Pref ")(setvar "snapunit" t-s)
(princ))
;
;
;
;
;++++++++++++++++++++++++++++++
;Sets snap on, snap = .125 & Osnap off
;++++++++++++++++++++++++++++++
;
(defun snapme ()
    (setvar "cmdecho" 1)
            (setq t-om (getvar "osmode"))
            (setq t-smode (getvar "snapmode"))
            (setq t-s (getvar "snapunit"))
                 (setvar "osmode" 0)
                 (setvar "snapmode" 1)
                 (princ (strcat "Snap Set To: " (rtos *snap*** 2 3) "," (rtos *snap*** 2 3)));(princ "Snap Set To: 0.125,0.125 ")
                 (setvar "snapunit" (list *Snap*** *Snap***))
    (princ))
;
;+++++++++++++++++++++++++++++++++++++++++++++
; resets Osnap, snap on/off and snap value to previous state
;+++++++++++++++++++++++++++++++++++++++++++++
; 
(defun snapme-r ()
    (setvar "cmdecho" 1)
     (setvar "osmode" t-om)
     (setvar "snapmode" t-smode)
     (princ "Snap Reset To User Pref ")(setvar "snapunit" t-s)
(princ))
;
;
;+++++++++++++++++++++++++++++++++++++++++++++
; resets Osnap, Ortho, snap on/off and snap value to previous state
;+++++++++++++++++++++++++++++++++++++++++++++
;
;=-=-=-=-=-=-=-=-=-=-=-=
;not used at this time 
;=-=-=-=-=-=-=-=-=-=-=-=
;(defun C:FIX-SNAP ()
;     (setvar "cmdecho" 1)
;     (setvar "osmode" t-om)
;     (setvar "orthomode" t-ortho)
;     (setvar "snapmode" t-smode)
;     (setvar "snapunit" t-s)
;(princ))
;
;
;++++++++++++++++++++++++++++++
;Turns Osnap off
;++++++++++++++++++++++++++++++
;
(defun snapo ()
    (setvar "cmdecho" 1)
            (setq t-ox (getvar "osmode"))
            (setvar "osmode" 0)
    (princ))
;
;
;+++++++++++++++++++++++++++++++++++++++++++++
; resets Osnap to previous state
;+++++++++++++++++++++++++++++++++++++++++++++
; 
(defun snapo-r ()
    (setvar "cmdecho" 1)
     (setvar "osmode" t-ox)
(princ))WB
;
;
;++++++++++++++++
;Turns AttReq off
;++++++++++++++++
;
(defun AttReqX ()
    (setvar "cmdecho" 1)
            (setq att-mode (getvar "attreq"))
            (setvar "attreq" 0)
(princ))
;
;
;+++++++++++++++++++++++++
; resets AttReq to previous state
;+++++++++++++++++++++++++
; 
(defun AttReqX-r ()
    (setvar "cmdecho" 1)
    (setvar "attreq" att-mode)   
(princ))
;
;;
(defun C:DIV-LISP-REV ()(alert "DIV Lisp Loader Ver 2020-04-20aTH-AM"))
(load "RvCld.lsp")
(LOAD "DRPL.LSP")
;(load "ViewPortRotation.lsp")
(load "DIV_AIDS.lsp") ;*XXX*** Variables are defined in Div_aids.lsp *Snap*** etc.
(load "DIV_Panel.lsp")
(load "LayrTA.lsp")
(load "wblockm.lsp")
(load "RevGridShift.vlx")
;(load "labelline.lsp")
