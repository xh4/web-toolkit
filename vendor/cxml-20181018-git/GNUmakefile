all:
	@echo no such target
	@exit 1

.PHONY: clean
clean:
	find . \( -name \*.fasl -o -name \*.x86f -o -name \*.lx64fsl \) -print0 | xargs -0 rm -f
