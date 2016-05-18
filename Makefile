.PHONY: slides watch-slides

slides:
	grep -vh '^#' slides/*.replesent > REPLesent.txt

watch-slides:
	while true; do \
		ls slides/* | entr -d \
			make slides; \
		sleep 1; \
	done;
