#+nil
(format t "~s" (cxml-pt-to-lhtml
                 (parse-html "<br/>ėšį<br> <div>fdsaa <span>fdsadfafdsa</span> <booo> <div>fdsafdsafdsa </div>")))

(assert (equal
(cxml-pt-to-lhtml
 (parse-html
"
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
          \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">
  <head>
<base href=\"http://licejus.pov.lt/calendar/daily.html\" />
"
))
'(:HTML NIL
  (:HEAD NIL (:BASE ((:HREF "http://licejus.pov.lt/calendar/daily.html"))))
  (:BODY NIL))))


(assert (equal
(cxml-pt-to-lhtml
 (parse-html
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html dir=\"ltr\">
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=windows-1257\">
<meta http-equiv=\"Content-Style-Type\" content=\"text/css\">

<title>GameDev.LT - Žaidimų kūrimas Lietuvoje :: Index</title>
<!-- link rel=\"stylesheet\" href=\"templates/DustyGreen/DustyGreen.css\" type=\"text/css\" -->
<link rel=\"stylesheet\" href=\"templates/DustyGreen/ssmitems.css\" type=\"text/css\">
</head>
<script>
<!--

/*
Copyright © MaXimuS 2002, All Rights Reserved.
Site: http://maximus.ravecore.com
E-mail: maximusforever@hotmail.com
Script: Static Slide Menu
Version: 6.6 Build 34
*/

NS6=(document.getElementById&&!document.all)
IE=(document.all);IE4=(document.all&&!document.getElementById)
NS=(navigator.appName==\"Netscape\" && navigator.appVersion.charAt(0)==\"4\")
OP=(navigator.userAgent.indexOf('Opera')>-1)
-->
</script>
"
))
'(:HTML NIL
 (:HEAD NIL
  (:META
   ((:HTTP-EQUIV "Content-Type")
    (:CONTENT "text/html; charset=windows-1257")))
  (:META ((:HTTP-EQUIV "Content-Style-Type") (:CONTENT "text/css")))
  (:TITLE NIL "GameDev.LT - Žaidimų kūrimas Lietuvoje :: Index")
  (:LINK
   ((:REL "stylesheet") (:HREF "templates/DustyGreen/ssmitems.css")
    (:TYPE "text/css"))))
 (:BODY NIL))))
