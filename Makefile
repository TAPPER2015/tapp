PROJECT_NAME=tapp

GENERATED_FILES=$(PROJECT_NAME).pdf $(PROJECT_NAME).html

COMMON_OPTS=-s -S $(PROJECT_BIB) --toc --number-sections --highlight-style haddock --mathjax

SOURCES= book.txt preliminaries.txt background.txt dedicated.txt multithreading.txt mutual-exclusion.txt fork-join.txt scheduling.txt benchmarking.txt intro.txt  preface.txt work-efficiency.txt granularity-control.txt parallel-arrays.txt sorting.txt synchronization.txt work-stealing.txt graphs.txt


TARGETS=$(PROJECT_NAME).html


all: $(TARGETS)

%.pdf : %.md graphics
	pandoc $< -s -S $(COMMON_OPTS) -o $@ 

%.html : %.md graphics
	pandoc $< -s -S $(COMMON_OPTS) $(PROJECT_STYLE_SHEET) -o $@

dvpt.pdf: dvpt.txt
	asciidoc --backend docbook45 dvpt.txt ; dblatex dvpt.xml 

book.html: $(SOURCES)
	asciidoc -a toc book.txt

current.html: $(SOURCES)
	asciidoc -a toc current.txt


tapp.html: $(SOURCES)
	asciidoc -a data-uri  -a toc book.txt
	cp book.html tapp.html

tapp.pdf: $(SOURCES)
	asciidoc --backend docbook45 book.txt ; dblatex book.xml 
	cp book.pdf tapp.pdf

tapp-full.html: $(SOURCES)
	asciidoc -a data-uri  -a toc book-full.txt
	cp book-full.html tapp-full.html

publish.html: $(SOURCES)
	asciidoc -a data-uri  -a toc book.txt
	cp book.html ~/Desktop/tapp-book.html

publish.pdf: $(SOURCES) book.pdf
	cp book.pdf ~/Desktop/tapp-book.pdf


www: $(SOURCES)
	asciidoc -a data-uri  -a toc book.txt
	cp book.html ~/Google\ Drive/TAPP/tapp.html


book.pdf: $(SOURCES)
	asciidoc --backend docbook45 book.txt ; dblatex book.xml 

graphics:
	./resize.sh

clean:
	rm $(TARGETS)
