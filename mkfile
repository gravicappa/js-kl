MKSHELL = rc
js = d8

default:V: run1

prepare:V:
	for (f in kl/*.kl)
		printf 'g/fail!/s/fail!/(fail)/g\nw\nq' | ed $f

fix_hash:V:
  {
		echo 'g/#/s/#/_hash/g'
		echo 'w'
		echo 'q'
	} | ed js/sys.js

run:V: fix_hash
	$js load.js
