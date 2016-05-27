.PHONY: slides watch-slides

default: slides predef

slides:
	grep -vh '^#' slides/*.replesent | grep -vh '^//-.*$$' | sed 's~^//+~~' > REPLesent.txt

target = src/main/scala/Presentation.scala

predef:
	echo '' > "$(target)"
	cat "$(target).head" > "$(target)"
	grep -vh '^#' slides/*.replesent | sed -n -e '/^```/,/^```/ p' | sed 's/^```$$//' >> "$(target)"
	cat "$(target).tail" >> "$(target)"

watch-slides:
	while true; do \
		ls slides/* | entr -d \
			make slides; \
		sleep 1; \
	done;
