cd ~/.emacs.d && etags --lang=pascal `find /opt/rot/app/release/dev -type f -name "*sql" | fgrep -v .svn`
cd ~/.emacs.d && etags --append `find ~/src/oe_trunk -type f -name "*java" | fgrep -v .svn`