(defun LM:vl-setattributevalue ( blk tag val )
  (setq tag (strcase tag))
  (vl-some '(lambda ( att )(if (= tag (strcase (vla-get-tagstring att))) (progn (vla-put-textstring att val) val)))(vlax-invoke blk 'getattributes))
)










(defun c:DEMO ( / *error* JR:Initget )
  (vl-load-com)
  
  (defun *error* ( errmsg )
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    )
    (prompt "Oopos algo deu errado!")
    (princ)
  )
  
  (defun JR:Initget ( parametos mensagem itens)
    (initget parametos )
    (setq valor (getkword (strcat mensagem "[" itens "]")))
  )
  
  (defun LM:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
  )
  
  (defun LM:lst->str ( lst del / str )
    (setq str (car lst))
    (foreach itm (cdr lst) (setq str (strcat str del itm)))
    str
  )
  
  
  
  
  (if (setq select (LM:ssget "\nSelecione bloco com atributo: " '(((0 . "INSERT")(66 . 1)))))
    (foreach x (vl-remove-if 'listp (mapcar 'cadr (ssnamex select)))
      (vlax-property-available-p (setq onblk (vlax-ename->vla-object X)) 'hasattributes)
      (setq listatt (mapcar '(lambda (x) (vla-get-tagstring x)) (vlax-invoke onblk 'getattributes)))
      (setq escolha (JR:Initget (LM:lst->str listatt " ")  "\nAtributo para inserir o valor " (LM:lst->str listatt "/")))
      (setq attlist (mapcar 'vla-get-textstring (vlax-invoke onblk 'getattributes)))
      (setq baseblkselection (vlax-get onblk 'insertionpoint))
      (setq ptZ (nth 2 baseblkselection))
      (setq setatt (LM:vl-setattributevalue onblk escolha (rtos ptZ)))
    )
    (prompt "Oops!!Bloco sem atributo ou nenhuma selacao!")
  )
  (princ)
)