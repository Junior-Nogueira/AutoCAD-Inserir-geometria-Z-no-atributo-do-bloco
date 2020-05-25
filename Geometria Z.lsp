
;;----------------------------------------------------------------;;
(defun c:ATZ ( / select value onblk ptZ setatt)  
  (if 
    (and
      (setq select (LM:ssget "\nSelecione bloco com atributo: " '(((0 . "INSERT")(66 . 1)))))
      (setq value (getstring "\nDigite o nome do atributo para inserir a elevacao: "))
    )
    (foreach x (vl-remove-if 'listp (mapcar 'cadr (ssnamex select)))
      (setq onblk (vlax-ename->vla-object x))
      (setq blkba (vlax-get onblk 'insertionpoint))
      (if (and (setq ptZ (nth 2 blkba)) (not (null value)))
        (setq setatt (LM:vl-setattributevalue onblk value (rtos ptZ 2 2)))
        (prompt "\nElevacao Z nula ou 0.00 ou atributo inexistente.")
      )
      (prompt (strcat "\nValor adicionado em " (rtos (sslength select) 2 0 ) " Bloco(s)."))
    )
    (prompt "Oops!!Bloco sem atributo ou nenhuma selacao!")
  )
  (princ)
)
(vl-load-com)
(prompt "\nPrograma feito para Gustavo Iatalesi \nDigite :: ATZ :: para iniciar!" )
;;----------------------------------------------------------------;;
;; ssget - Lee Mac
;; A wrapper for the ssget function to permit the use of a custom selection prompt
;; msg - [str] selection prompt
;; arg - [lst] list of ssget arguments

(defun LM:ssget ( msg arg / sel )
  (princ msg)
  (setvar 'nomutt 1)
  (setq sel (vl-catch-all-apply 'ssget arg))
  (setvar 'nomutt 0)
  (if (not (vl-catch-all-error-p sel)) sel)
)
;;----------------------------------------------------------------;;

;; Set Attribute Values  -  Lee Mac
;; Sets attributes with tags found in the association list to their associated values.
;; blk - [ent] Block (Insert) Entity Name
;; lst - [lst] Association list of ((<tag> . <value>) ... )
;; Returns: nil

(defun LM:vl-setattributevalue ( blk tag val )
  (setq tag (strcase tag))
  (vl-some '(lambda ( att )(if (= tag (strcase (vla-get-tagstring att))) (progn (vla-put-textstring att val) val)))(vlax-invoke blk 'getattributes))
)