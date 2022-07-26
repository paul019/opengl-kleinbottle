# CONWAY x KLEIN

## The result

![CONWAY x KLEIN](https://raw.githubusercontent.com/paul019/opengl-kleinbottle/master/example.gif)

## The idea

There is not much to say here. It’s Conways Game of Life on the surface of a Klein bottle.

Conway’s Game of Life is a so-called Cellular automaton where each cell has two possible states. Hence, each cell is a "binary system" and all the binary systems influence each other according to  very specific rules.

A Klein bottle, on the other hand, is a four-dimensional topological object with a non-orientable surface. Put simply, a Klein bottle doesn’t have an inside and an outside. Its inside is its outside!

Putting Conways Game of Life on the surface of a Klein bottle is a nice thought experiment I came up with after listening to Dr. Lior Ben Gai’s lecture at the [ISSI](https://davidson.weizmann.ac.il/en/programs/issi). In his work, Dr. Gai uses the surface of a torus for his Cellular automatons.

With this artwork, I realized my thought experiment. However, it would not have been possible without [Guillermo Luijk’s blog post "La botella de Klein con R"](https://www.overfitting.net/2018/04/la-botella-de-klein-con-r.html) and the code associated with it.

## The code

At the heart of the code, there is the function `drawKlein(M,n,startIndex,endIndex)`. Here, `M` is the initial state of Conway's Game Of Life, `n` is a metric for the 3D resolution, and `startIndex` and `endIndex` define which frames of the animation should be generated.

Please note that the code is quick and dirty and extremely inefficient. Whoever is reading this is invited to improve it.
