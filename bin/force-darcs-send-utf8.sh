#!/bin/bash
# darcs and msmtp not specifying a charset
cat /dev/stdin | sed "s/text\/x-darcs-patch;/text\/x-darcs-patch; charset=utf-8;/" | /usr/sbin/sendmail -i -t
