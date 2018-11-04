#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#define SIZE 5000000




void test( char* text, char* pattern, int method(char* text , char * pattern,int * indices));

char * ReadEntireFile(char * filename);

char * getNextPattern(char * patterns,int offset);

int validate(char * text,char *pattern,int n, int * indices);


