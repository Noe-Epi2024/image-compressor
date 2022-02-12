##
## EPITECH PROJECT, 2020
## B-FUN-400-PAR-4-1-wolfram-noe.jais
## File description:
## Makefile
##

NAME = imageCompressor

all:
	stack build
	stack install --local-bin-path .

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all