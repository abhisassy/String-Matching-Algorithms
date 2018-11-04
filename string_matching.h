#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

int naive(char * text, char *pattern, int * indices);

int horspool(char * text, char *pattern, int * indices);

int boyerMoore( char * text, char *pattern, int * indices);

int rabinKarp( char * text, char *pattern, int * indices);

int finiteAutomaton( char * text, char *pattern, int * indices);

int KMP( char * text, char *pattern, int * indices);

