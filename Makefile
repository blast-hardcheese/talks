SRC_FILES = $(shell ls src/*.lhs)

watch:
	echo ${SRC_FILES} | entr -c cabal run

