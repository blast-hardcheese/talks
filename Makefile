.PHONY: watch-slides

default: predef

target = src/main/scala/Presentation.scala

predef:
	echo '' > "$(target)"
	cat "$(target).head" > "$(target)"
	grep -vh '^#' slides/*.replesent | sed -n -e '/^```/,/^```/ p' | sed 's/^```$$//' | sed 's~^//!~~' >> "$(target)"
	cat "$(target).tail" >> "$(target)"
