#!/bin/bash

lhs2TeX --verb CheatSheet.lhs > CheatSheet.tex
latex -interaction=batchmode CheatSheet.tex
pdflatex -interaction=batchmode CheatSheet.tex
