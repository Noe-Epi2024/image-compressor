# Image Compressor

From a file listing all the pixels of the image with their position and color, regroup them into a given number of clusters, according to their colors.

## Usage

```bash
./imageCompressor
USAGE: ./imageCompressor -n N -l L -f F
  N number of colors in the final image
  L convergence limit
  F path to the file containing the colors of the pixels
```

## Getting Started
![](https://img.shields.io/badge/Code-Haskell-informational?style=flat&logo=haskell&logoColor=white&color=55C2E1)
### Dependencies

* Haskell and Stack
* Makefile

### Installing

* [Download Project](https://github.com/Noe-Epi2024/image-compressor)

### Executing program

* How to run the program
```bash
make
./imageCompressor -n 2 -l 0.8 -f Test-File/example1
```

## Authors
Noe Jais and Lucas Lindemans

contact:
```
noe.jais@epitech.eu
```
```
lucas.lindemans@epitech.eu
```

## Commit Norm

- Commits have to be in english.
- Each commit should start with a square bracket enclosed key to declare the reason of the commit. Those keys can be found among those :
  ### [ADD] -- if you add files, features, and so on
  ### [FIX] -- if you were working on a bug or any form of default that you corrected 
  ### [DEL] -- if you removed files, features, assets, and so on
  ### [CHANGE] -- if you change something without adding any features or content

## Version History

* 0.1
    * Initial Release
