RESULT     = main
SOURCES    = src/position.ml src/map.ml src/neuralnet.ml src/population.ml src/individual.ml src/view.ml src/battle.ml src/ai.ml src/main.ml
LIBS       = bigarray unix sdl sdlmixer
INCDIRS    = +sdl

all: native-code

include OCamlMakefile