#!/bin/sh -e
set -x

cd $(dirname $0)
home=$(pwd)
name=$(basename $home)
name_and_date=${name}-$(date --iso)

TMPDIR=`mktemp -d /tmp/dist.XXXXXXXXXX`
cleanup() {
    cd
    rm -rf $TMPDIR
}
trap cleanup exit

make -C doc

git tag -f $name_and_date
git archive --prefix=$name_and_date/ $name_and_date | \
    ( cd $TMPDIR && tar xvf - )

echo '(progn (load "doc/atdoc.lisp") (quit))' | clbuild lisp 

rsync -a doc $TMPDIR/$name_and_date

cd $TMPDIR

tgz=$TMPDIR/${name_and_date}.tgz
tar czf $tgz $name_and_date
gpg -b -a $tgz

mkdir -p ~/clnet/project/closure/public_html/closure-html/

rsync -av \
    $name_and_date/doc/ \
    ~/clnet/project/closure/public_html/closure-html/

rsync $tgz $tgz.asc ~/clnet/project/closure/public_html/closure-html/download/

rm -f ~/clnet/project/closure/public_html/closure-html/download/closure-html.tar.gz 
rm -f ~/clnet/project/closure/public_html/closure-html/download/closure-html.tar.gz.asc

ln -sf ${name_and_date}.tgz ~/clnet/project/closure/public_html/closure-html/download/closure-html.tar.gz
ln -sf ${name_and_date}.tgz.asc ~/clnet/project/closure/public_html/closure-html/download/closure-html.tar.gz.asc

echo done
exit 0
rsync -av ~/clnet/project dlichteblau@common-lisp.net:/
