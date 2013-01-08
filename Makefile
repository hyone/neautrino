TARGET = neautrino
RM = /bin/rm
FIND = gfind
XARGS = gxargs

all: ${TARGET}

${TARGET}: clean
	cabal configure --enable-tests
	cabal build
	cabal test
	cp dist/build/${TARGET}/${TARGET} .

clean:
	cabal clean
	$(FIND) . -name "*.hi" -or -name "*.o" -or -name "*.hi-boot" -or -name "*.o-boot" | ${XARGS} -i $(RM) -f "{}"
	$(RM) -f $(TARGET)

