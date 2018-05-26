RESULT     = main
SOURCES    = src/position.ml src/map.ml src/neuralnet.ml src/population.ml src/individual.ml src/view.ml src/battle.ml src/ai.ml src/main.ml
LIBS       = unix #bigarray sdl sdlmixer
#INCDIRS    = +sdl
# comment INCDIRS and "bigarray sdl sdlmixer" in LIBS for no sdl


all: native-code

include OCamlMakefile