## Build a single Rmd file

SHELL = /bin/bash

.DEFAULT_GOAL := render

all:       clean_all pdf 
render:    pdf upload
Ap:        Ap1
pdf:       p1 p2 p3 p4 p5 p6 p7 p7b p8 Ap
clean_all: clean_cache clean_pdfs

include .buildver.makefile

LIBRARY      = ~/LIBRARY/REPORTS/

# ### MDPI Article
# TARGET = MDPI_submition
# RMD    = $(TARGET).Rmd
# PDF    = $(TARGET).pdf
# DOC    = $(TARGET).docx
# Ap2: $(PDF)
# $(PDF): $(RMD)
# 	@echo "Building: $@"
# 	@#-Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
# 	-Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='rticles::mdpi_article', output_file='$@', clean = TRUE)"
# 	@# echo "Changed:  $?"
# 	@#setsid evince    $@ &
# 	@-rsync -a "$@" ${LIBRARY}


###   Default pdf   ###################################
TARGET := ./article/article
QMD    := $(TARGET).qmd
RMD    := $(TARGET).Rmd
PDF    := $(TARGET).pdf
Ap1: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $? -> $@"
	@#-Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@#-Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@', output_dir='article', clean = TRUE)"
	@Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='rticles::elsevier_article', output_file='$@', output_dir='article', clean = TRUE)"
	@#quarto render '$?' --to elsevier-pdf --log-level warning
	@-rsync -a "$@" ${LIBRARY}



## Diff of pdfs
diff: article/article_edits.pdf
article/article_edits.pdf: article/article.tex
	-latexdiff /home/athan/MANUSCRIPTS/02_enhancement/SUBMISSION_01/article/article.tex /home/athan/MANUSCRIPTS/02_enhancement/article/article.tex > /home/athan/MANUSCRIPTS/02_enhancement/article/article_edits.tex
	-cd /home/athan/MANUSCRIPTS/02_enhancement/article; pdflatex -interaction=nonstopmode  article_edits.tex
	@-rsync -a "$@" ${LIBRARY}


### Doc for spellining and merge  ############################
TARGET := ./article/article
RMD    := $(TARGET).Rmd
DOC1   := $(TARGET).docx
doc: $(DOC1)
$(DOC1): $(RMD)
	@echo "Building: $? -> $@"
	## build my file from source and convert to md
	Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', clean = TRUE, output_format='bookdown::word_document2', output_file='$@',  output_dir='article')"
	pandoc -s $(DOC1)                              -o ./article/article_doc.md
	## get a docx from remote and convert to md for comparison
	# pandoc -s ./Article_B11/Article_B11_AB_NA.docx -o ./Article_B11/Article_B11_AB_NA.md
	rclone --config $${HOME}/Documents/rclone.conf sync "lapauththanasis:/Enhance/SUBMISSION_01/Article_B11_AB_6-9-2024.docx" ./Article_B11/
	pandoc -s ./Article_B11/Article_B11_AB_6-9-2024.docx -o ./Article_B11/Article_remote.md
	rclone --config $${HOME}/Documents/rclone.conf sync "lapauththanasis:/Enhance/SUBMISSION_01/Article_B12_AB.docx"          ./Article_B12/
	pandoc -s ./Article_B12/Article_B12_AB.docx          -o ./Article_B12/Article_remote.md


##   Article with build number   ######################
TARGET := ./article/article
RMDv   := $(TARGET).Rmd
BUILD  := B$(shell cat $(BLD_FILE))
DIR    := ./Article_$(BUILD)
PDFa   := $(DIR)/Article_$(BUILD)_plain.pdf
DOC    := $(DIR)/Article_$(BUILD).docx
RMDn   := $(DIR)/Article_$(BUILD).Rmd

Apv: $(PDFa)
$(PDFa): $(RMDv)
	@echo "Building: $(DOC)"
	@echo "          $(PDFa)"
	@mkdir -p '$(DIR)'
	Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', clean = TRUE, output_format='bookdown::word_document2', output_file='$(DOC)',  output_dir='$(DIR)')"
	@#-Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', clean = TRUE, output_format='bookdown::pdf_document2',  output_file='$(PDFa)', output_dir='$(DIR)')"
	Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', clean = TRUE, output_format='rticles::elsevier_article',  output_file='$(PDFa)', output_dir='$(DIR)')"
	-cp '$(RMDv)' '$(RMDn)'
	-chmod 0444 '$(DIR)'/*
	-git tag $(BUILD)
	## increase build counter
	$(call buildver)





###   1. raw data  ####################################
TARGET := GHI_enh_01_raw_data
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
RUNT   := ./runtime/$(TARGET).pdf

p1: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.pdf' --include '*.png' ./GHI_*/figure-latex/ ./images
	@-rsync -a "$@" ${LIBRARY}
	@#-touch article/article.qmd
	@-touch article/article.Rmd



###   2. ID CE  ####################################
TARGET := GHI_enh_02_ID_CE
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
RUNT   := ./runtime/$(TARGET).pdf

p2: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.pdf' --include '*.png' ./GHI_*/figure-latex/ ./images
	@-rsync -a "$@" ${LIBRARY}
	@#-touch article/article.qmd
	@-touch article/article.Rmd




###   3. aggregate data   #################################
TARGET := GHI_enh_03_process
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
RUNT   := ./runtime/$(TARGET).pdf

p3: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.pdf' --include '*.png' ./GHI_*/figure-latex/ ./images
	@#setsid evince    $@ &
	@-rsync -a "$@" ${LIBRARY}
	@#-touch article/article.qmd
	@-touch article/article.Rmd


###   4. investigate  data   #################################
TARGET := GHI_enh_04_investigate
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
RUNT   := ./runtime/$(TARGET).pdf

p4: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.pdf' --include '*.png' ./GHI_*/figure-latex/ ./images
	@#setsid evince    $@ &
	@-rsync -a "$@" ${LIBRARY}
	@#-touch article/article.qmd
	@-touch article/article.Rmd




###   5. distributions  data   #################################
TARGET := GHI_enh_05_distributions
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
RUNT   := ./runtime/$(TARGET).pdf

p5: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.pdf' --include '*.png' ./GHI_*/figure-latex/ ./images
	@#setsid evince    $@ &
	@-rsync -a "$@" ${LIBRARY}
	@#-touch article/article.qmd
	@-touch article/article.Rmd



###   6. investigate  SZA   #################################
TARGET := GHI_enh_06_sza
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
RUNT   := ./runtime/$(TARGET).pdf

p6: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.pdf' --include '*.png' ./GHI_*/figure-latex/ ./images
	@#setsid evince    $@ &
	@-rsync -a "$@" ${LIBRARY}
	@#-touch article/article.qmd
	@-touch article/article.Rmd




###   7. investigate  Aerosols   #################################
TARGET := GHI_enh_07_Aerosols
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
RUNT   := ./runtime/$(TARGET).pdf

p7: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.pdf' --include '*.png' ./GHI_*/figure-latex/ ./images
	@#setsid evince    $@ &
	@-rsync -a "$@" ${LIBRARY}
	@#-touch article/article.qmd
	@-touch article/article.Rmd


###   7b. investigate  Aerosols   #################################
TARGET := GHI_enh_07_Aerosols_BR_CIM
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
RUNT   := ./runtime/$(TARGET).pdf

p7b: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.pdf' --include '*.png' ./GHI_*/figure-latex/ ./images
	@#setsid evince    $@ &
	@-rsync -a "$@" ${LIBRARY}
	@#-touch article/article.qmd
	@-touch article/article.Rmd


###   8. investigate  Aerosols   #################################
TARGET := GHI_enh_08_validation
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
RUNT   := ./runtime/$(TARGET).pdf

p8: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.pdf' --include '*.png' ./GHI_*/figure-latex/ ./images
	@#setsid evince    $@ &
	@-rsync -a "$@" ${LIBRARY}
	@#-touch article/article.qmd
	@-touch article/article.Rmd






upload:
	-./upload.sh

clean_cache:
	# trash -f  ./Article_cache
	trash -f  ./GHI_enh_02_ID_CE_files
	trash -f  ./GHI_enh_03_process_files
	trash -f  ./GHI_enh_04_investigate_files
	trash -f  ./GHI_enh_05_distributions_files
	trash -f  ./runtime/*.*

clean_pdfs:
	trash -f    ./GHI_enh_01_raw_data.pdf
	trash -f    ./GHI_enh_02_ID_CE.pdf
	trash -f    ./DHI_GHI_3_trends_consistency.pdf

