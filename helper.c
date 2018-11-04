#include <stdio.h>
#include "helper.h"

#define FREQ 2.60e9

unsigned long long rdtsc()
{
  unsigned long long int x;
  unsigned a, d;

  __asm__ volatile("rdtsc" : "=a" (a), "=d" (d));

  return ((unsigned long long)a) | (((unsigned long long)d) << 32);
}

/*
	Call the specified method
	Validate results
*/

void test( char* text, char* pattern, int method(char* text , char * pattern,int * indices)){
	unsigned long long t0,t1;
	int n;
	int * indices=malloc(sizeof(int) * SIZE);


	t0=rdtsc(); 	//start timer
	n = method(text,pattern,indices);
	t1=rdtsc(); 	//stop timer

	printf("%lf\t", (t1-t0)/FREQ);	
	printf("Number of occurences : %d\tValid : %d\n",n,validate(text, pattern,n,indices));
	free(indices);
}

/*
	Get the next pattern from the string containing all patterns
*/

char * getNextPattern(char * patterns,int offset){
	patterns=patterns+offset;
	if(patterns==NULL || strlen(patterns)==0 || patterns[0]=='\0') return NULL;
	int i=offset;
	int j=0;
	while(patterns[j]!='\n' && j<strlen(patterns))j++;
	int size=j+1;
	char * pattern=malloc(sizeof(char)*size);
	strncpy(pattern,patterns,size);
	pattern[size-1]='\0';
	return pattern;
}

/*
	This is the just a sanity check.
    Checks if each index is valid 
    Output is 1 only if all are correct
*/


int validate(char * text,char *pattern,int n, int * indices){
	int count=0;;
	int i,index,j;
	char * text_string;
	for(i=0;i<n;i++){

		index=indices[i];
		text_string=malloc(sizeof(char)*strlen(pattern)+1);
		strncpy(text_string, text+index, strlen(pattern));
		text_string[strlen(pattern)]='\0';
		if(strcmp(text_string, pattern)==0)count++;
		free(text_string);
	}
	if(count==n)return 1;
	return 0;

}


// Given filename, read the entire file
char * ReadEntireFile(char * filename){
	char * buffer;
	long length;
	FILE * f = fopen (filename, "rb");
	if (f)
	{
	  fseek (f, 0, SEEK_END);
	  length = ftell (f);
	  fseek (f, 0, SEEK_SET);
	  buffer = malloc (length);
	  if (buffer)
	  {
	    fread (buffer, 1, length, f);
	  }
	  fclose (f);
	}
	buffer[length-1]='\0';
	return buffer;

}
