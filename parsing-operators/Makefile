SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -O globstar -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

.PHONY: default clean generalized-question-mark test

default:
	@echo 'no default target'

clean:
	# remove anything in .gitignore, including directories
	git clean -fdX

generalized_question_mark:
	mmc --make --warn-inconsistent-pred-order-clauses --warn-dead-preds --warn-dead-procs --inform-ite-instead-of-switch generalized_question_mark

test: generalized_question_mark
	./generalized_question_mark
