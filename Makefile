SRC = src
TARGET = run
MAIN = $(SRC)/run.hs
GHC = /usr/local/bin/ghc
XARGS = /usr/local/bin/gxargs
FIND = /usr/local/bin/gfind
RM = /bin/rm


all: $(TARGET)

$(TARGET):
	$(GHC) -i$(SRC) --make -o $(TARGET) $(MAIN)

clean:
	$(FIND) . -name "*.hi" -or -name "*.o" | $(XARGS) -i $(RM) "{}"
	$(RM) $(TARGET)
