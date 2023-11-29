default: slides

slides:
	cat index.pre.html slides.md index.post.html > index.html

watch:
	@while true; do \
		git ls-files \
			index.pre.html slides.md index.post.html \
			| entr -cd make slides; \
		echo -n .; \
		sleep 1; \
	done
