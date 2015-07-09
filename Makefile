

allFilters.xml: myFilters
	mv allFilters.xml allFilters.xml.bak
	./myFilters

myFilters: myFilters.hs
	ghc myFilters.hs -o myFilters --make
