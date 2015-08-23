
dist/Main.js:  $(shell find src -name '*.purs' -or -name '*.js')
	pulp build -O --to dist/Main.js

deploy: dist/Main.js
	scp dist/* nfs:/home/public/dibs_on_oz
