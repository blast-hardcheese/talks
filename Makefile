default: slides

slides:
	mkdir -p dist
	cp -rf images dist/images
	cat index.pre.html slides.md index.post.html > dist/index.html

watch:
	@while true; do \
		git ls-files \
			index.pre.html slides.md index.post.html \
			| entr -cd make slides; \
		echo -n .; \
		sleep 1; \
	done
