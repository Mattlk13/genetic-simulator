# genetic-simulator

---

This is our **Artificial Intelligence** project in 3rd year at INSA Rennes, developed with [shymine](https://www.github.com/shymine). The goal of the project was to create an AI for a video game. We could choose any game with open source on the internet, and any technology to code the AI. In our case, we chose to create the game too, as it was not present on the web.

The game simulates two populations on a map with food. The individuals can move, copulate, pick up food, store it, eat it, and attack individuals from the other population. The winner is the population that manages to either kill all the enemies or survive until all the enemies die (e.g. starvation).

shymine chose to implement a **neural network with reinforcement learning**, and I chose **neural networks with a genetic algorithm**. This repository contains all the code for my version of the AI (the genetic one). Should you want to see his part of the project, please head to [his repository](https://www.github.com/shymine/AI_Java).

This version of the AI is implemented in **OCaml** with the **SDL** library to display the game.

---

### Dependencies

- make
- ocaml (developed with 4.05.0, theoretically ok with version >= 4.03)
- the [ocamlsdl](http://opam.ocaml.org/packages/ocamlsdl/) opam package

### How to run it

The *main<span></span>.ml* file contains only two lines. The second line can be either `Ai.learn()` if you want the AI to learn and get better, or `Ai.play()` if you want to see the AI performing against itself.

After making sure you have the right version of the main file, run  `make` to build the project (native code executable), then `./main` to execute, and `make clean` if you want to remove the executable and the compilation files.