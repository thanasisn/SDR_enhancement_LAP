## Build a single Rmd file

SHELL = /bin/bash

.DEFAULT_GOAL := render

all:       clean_all pdf rtim
render:    pdf upload
Ap:        Ap1
pdf:       p1 p2 p3 p4 p5 Ap
rtim:      r1 r2 r3
clean_all: clean_cache clean_data clean_pdfs

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


## simple default pdf
TARGET = ./article/article
QMD    = $(TARGET).qmd
PDF    = $(TARGET).pdf
Ap1: $(PDF)
$(PDF): $(QMD)
	@echo "Building: $? -> $@"
	@#-Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@#-Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@', clean = TRUE)"
	quarto render '$?' --to elsevier-pdf --log-level warning
	@#setsid evince    $@ &
	@-rsync -a "$@" ${LIBRARY}




# ## Article pdf with build number
# ## using rstudio pandoc
# TARGET = MDPI_submission
# RMD    = $(TARGET).Rmd
# PDF    = $(TARGET)_B$(shell cat $(BLD_FILE)).pdf
# SLIDY  = $(TARGET).html
# Apv: $(PDF)
# $(PDF): $(RMD)
# 	@echo "Building: $@"
# 	-Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', clean = TRUE, output_format='rticles::mdpi_article', output_file='MDPI_submission_B$(shell echo $$(($$(cat $(BLD_FILE)) + 1))).pdf')"
# 	-Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', clean = TRUE, output_format='bookdown::word_document2', output_file='MDPI_submission_B$(shell echo $$(($$(cat $(BLD_FILE)) + 1))).docx')"
# 	-mkdir -p "Build_$(shell echo $$(($$(cat $(BLD_FILE)) + 1)))"
# 	-cp 'MDPI_submission.Rmd' 'MDPI_submission_B$(shell echo $$(($$(cat $(BLD_FILE)) + 1))).Rmd'
# 	-mv *_B$(shell echo $$(($$(cat $(BLD_FILE)) + 1))).* "Build_$(shell echo $$(($$(cat $(BLD_FILE)) + 1)))"
# 	-chmod 0444 Build_$(shell echo $$(($$(cat $(BLD_FILE)) + 1)))/*_B$(shell echo $$(($$(cat $(BLD_FILE)) + 1))).*
# 	## increase counter
# 	$(call buildver)



###   1. raw data  ####################################

TARGET := GHI_enh_01_raw_data
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
RUNT   := ./runtime/$(TARGET).pdf

p1: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.pdf' --include '*.png' ./GHI_*/figure-latex/ ./images
	@-rsync -a "$@" ${LIBRARY}
	@-touch article/article.qmd

r1: $(RUNT)
$(RUNT): $(RMD)
	-Rscript $?


###   2. ID CE  ####################################

TARGET := GHI_enh_02_ID_CE
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
RUNT   := ./runtime/$(TARGET).pdf

p2: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.pdf' --include '*.png' ./GHI_*/figure-latex/ ./images
	@-rsync -a "$@" ${LIBRARY}
	@-touch article/article.qmd

r2: $(RUNT)
$(RUNT): $(RMD)
	-Rscript $?



###   3. aggregate data   #################################

TARGET := GHI_enh_03_process
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
RUNT   := ./runtime/$(TARGET).pdf

p3: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.pdf' --include '*.png' ./GHI_*/figure-latex/ ./images
	@#setsid evince    $@ &
	@-rsync -a "$@" ${LIBRARY}
	@-touch article/article.qmd

r3: $(RUNT)
$(RUNT): $(RMD)
	-Rscript $?


###   4. investigate  data   #################################

TARGET := GHI_enh_04_investigate
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
RUNT   := ./runtime/$(TARGET).pdf

p4: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.pdf' --include '*.png' ./GHI_*/figure-latex/ ./images
	@#setsid evince    $@ &
	@-rsync -a "$@" ${LIBRARY}
	@-touch article/article.qmd


r4: $(RUNT)
$(RUNT): $(RMD)
	-Rscript $?



###   5. investigate  data   #################################

TARGET := GHI_enh_05_distributions
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
RUNT   := ./runtime/$(TARGET).pdf

p5: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.pdf' --include '*.png' ./GHI_*/figure-latex/ ./images
	@#setsid evince    $@ &
	@-rsync -a "$@" ${LIBRARY}
	@-touch article/article.qmd


r5: $(RUNT)
$(RUNT): $(RMD)
	-Rscript $?



upload:
	-./upload.sh


clean_cache:
	rm -f -r ./Article_cache
	rm -f -r ./GHI_enh_02_ID_CE_files
	rm -f -r ./GHI_enh_02_process_files
	rm -f -r ./runtime/GHI*.pdf

clean_pdfs:
	rm -f    ./GHI_enh_01_raw_data.pdf
	rm -f    ./GHI_enh_02_ID_CE.pdf
	rm -f    ./DHI_GHI_3_trends_consistency.pdf

clean_data:
	rm -f    ./data/*.*

