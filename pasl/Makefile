PROJECT_NAME=book
PROJECT_STYLE_SHEET=-c lecture.css
WEB_DIR=~/r/15210-f15/www

GENERATED_FILES=$(PROJECT_NAME).pdf $(PROJECT_NAME).html
PROJECT_BIB=--biblio ../../bibliography/main.bib
COMMON_OPTS=-s -S $(PROJECT_BIB) --toc --number-sections --highlight-style haddock --mathjax

SOURCE_DIR = ../

SOURCES= book.txt teach.txt $(SOURCE_DIR)/preface.txt $(SOURCE_DIR)/background.txt $(SOURCE_DIR)/fork-join.txt $(SOURCE_DIR)/parallel-arrays.txt $(SOURCE_DIR)/scheduling.txt $(SOURCE_DIR)/benchmarking.txt $(SOURCE_DIR)/work-efficiency.txt $(SOURCE_DIR)/granularity-control.txt $(SOURCE_DIR)/parallel-arrays.txt $(SOURCE_DIR)/sorting.txt $(SOURCE_DIR)/synchronization.txt $(SOURCE_DIR)/graphs.txt $(SOURCE_DIR)/race-conditions.txt 


TARGETS=$(PROJECT_NAME).pdf $(PROJECT_NAME).html book.html


all: $(TARGETS)

%.pdf : %.md graphics
	pandoc $< -s -S $(COMMON_OPTS) -o $@ 

%.html : %.md graphics
	pandoc $< -s -S $(COMMON_OPTS) $(PROJECT_STYLE_SHEET) -o $@

dvpt.pdf: dvpt.txt
	asciidoc --backend docbook45 dvpt.txt ; dblatex dvpt.xml 

book.html: $(SOURCES)
	asciidoc -a toc book.txt

teach.html: $(SOURCES)
	asciidoc -a toc teach.txt


book.html.embed: $(SOURCES) 
	asciidoc -a data-uri  -a toc book.txt
	cp book.html pasl.html

book.pdf: $(SOURCES)
	asciidoc --backend docbook45 book.txt ; dblatex book.xml 

pasl.html: $(SOURCES)
	asciidoc -a data-uri  -a toc book.txt
	cp book.html ~/Desktop/pasl.html

pasl.pdf: $(SOURCES) book.pdf
	cp book.pdf ~/Desktop/pasl.pdf

www: book.html.embed
	cp book.html $(WEB_DIR)


graphics:
	./resize.sh

clean:
	rm $(TARGETS)
