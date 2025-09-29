# Ballot Order Effects Analysis Pipeline

R_SCRIPTS := $(wildcard code/*.R) run_all.R
DATA_FILES := $(wildcard data/*)

# Default target
all: analysis docs

# Run R analysis pipeline
analysis: outputs/.analysis_done

outputs/.analysis_done: $(R_SCRIPTS) $(DATA_FILES)
	@mkdir -p paper/tables outputs
	@echo "Running R analysis pipeline..."
	Rscript run_all.R
	@touch outputs/.analysis_done

# Render documents
docs: analysis
	@if [ -f "index.qmd" ]; then \
		echo "Rendering Quarto documents..."; \
		quarto render; \
	fi
	@for tex in paper/*.tex; do \
		if [ -f "$$tex" ]; then \
			echo "Compiling $$tex with latexmk..."; \
			cd paper && latexmk -xelatex -pvc- $$(basename $$tex); \
		fi \
	done

# Clean generated files
clean:
	rm -f outputs/.analysis_done
	rm -f *.html
	cd paper && latexmk -c

# Deep clean (including PDFs)
distclean:
	rm -f outputs/.analysis_done
	rm -f *.html
	cd paper && latexmk -C

# Force rebuild
rebuild: clean all

.PHONY: all analysis docs clean distclean rebuild
