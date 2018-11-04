#include <stdio.h>
#include <string.h>
#include "helper.h"
#include "string_matching.h"



int main(){
	
	//Read entire text from text_file
	char * text=ReadEntireFile("text_file.txt");
	char * all_patterns=ReadEntireFile("patterns_file.txt");
	int offset=0;
	char * pattern;

	while(offset<strlen(all_patterns)){
		//Get next pattern
		pattern= getNextPattern(all_patterns,offset);
		//printf("pattern : %s\n",pattern);
		offset=offset+strlen(pattern)+1;

		test(text,pattern, naive);
		test(text,pattern, horspool);
		test(text,pattern, boyerMoore);
		test(text,pattern, rabinKarp);
		test(text,pattern, finiteAutomaton);
		test(text,pattern, KMP);

		free(pattern);
		
	}
	free(text);
	free(all_patterns);
}
