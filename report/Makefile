all:
	# -shell escape : nécessaire pour minted
	pdflatex -shell-escape rapport-xue-yu.tex
	# on compile une deuxième fois pour les références
	pdflatex -shell-escape rapport-xue-yu.tex

clean:
	rm -rf *bak *~ *out *log *aux _minted-report *.aex *.fls *.out *.fdb_latexmk *.gz
