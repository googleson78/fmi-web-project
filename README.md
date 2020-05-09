## Реферат за курса по УЕБ към ФМИ (19/20)

## `repl` за `exmaple.hs`
```
cabal v2-repl --ghc-options="-package aeson" --ghc-options="-package warp" --ghc-options="-package servant" --ghc-options="-package servant-server" --ghc-options="-package servant-client" --ghc-options="-package http-client"
```
Като преди това са инсталирани всички тези с `cabal v2-install`.


## Компилация на презентацията:
0. `sudo apt install pandoc`
1. `sudo apt install fonts-noto`
2. `sudo apt install texlive-latex-{base,extra,recommended} texlive-xetex texlive-lang-cyrillic`
    (кой знае кои от тези всъщност са нужни)
3. `pandoc --pdf-engine=xelatex -t beamer present.md -o slides.pdf`
