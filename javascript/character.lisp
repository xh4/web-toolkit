(in-package :javascript)

(defun whitespace-p (char)
  (or (eq #\Space char)
      (eq #\Tab char)
      (eq #\VT char)
      (eq #\Page char)
      #+lispworks
      (eq #\No-Break-Space char)
      #-lispworks
      (eq #\No-Break_Space char)
      (and (>= (char-code char) #x1680)
           (member (char-code char)
                   '(#x1680 #x2000 #x2001 #x2002 #x2003 #x2004 #x2005 #x2006 #x2007 #x2008 #x2009 #x200A #x202F #x205F #x3000 #xFEFF)))))

(defun line-terminator-p (char)
  (or (eq #\Newline char)
      (eq #\Return char)
      #+lispworks (eq #\Line-Separator char) #-lispworks (eq #\Line_Separator char)
      #+lispworks (eq #\Paragraph-Separator char) #-lispworks (eq #\Paragraph_Separator char)))

(defun identifier-start-p (char)
  (or (eq #\$ char)
      (eq #\_ char)
      (char<= #\A char #\Z)
      (char<= #\a char #\z)
      (eq #\\ char)
      ;; TODO
      (and (char>= char #\U+0080))))

(defun identifier-part-p (char)
  (or (eq #\$ char)
      (eq #\_ char)
      (char<= #\A char #\Z)
      (char<= #\a char #\z)
      (char<= #\0 char #\9)
      (eq #\\ char)
      ;; TODO
      (and (char>= char #\U+0080))))

(defun decimal-digit-p (char)
  (char<= #\0 char #\9))

(defun hex-digit-p (char)
  (or (char<= #\0 char #\9)
      (char<= #\A char #\F)
      (char<= #\a char #\f)))

(defun octal-digit-p (char)
  (char<= #\0 char #\7))

(defun hex-value (char)
  (cl:position (char-downcase char) "0123456789abcdef"))

(defun octal-value (char)
  (cl:position char "01234567"))