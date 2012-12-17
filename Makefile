TARGET = simple-scheme
RM = /bin/rm
FIND = gfind
XARGS = gxargs

clean:
	cabal clean
	$(FIND) . -name "*.hi" -or -name "*.o" -or -name "*.hi-boot" -or -name "*.o-boot" | ${XARGS} -i $(RM) -f "{}"
	$(RM) -f $(TARGET)
