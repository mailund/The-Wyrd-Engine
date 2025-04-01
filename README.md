# The-Wyrd-Engine

Simple rules for one-shot and episodic roleplaying games

To compile the documents you must set `TEXINPUTS` so latex can find the `dndbook` document class and the `wyrdtex` macros. For example, to compile the rule book do

```
cd rules
TEXINPUTS=../dndbook:../wyrdtex: pdflatex wyrd.tex
```

