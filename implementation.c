#include <stdio.h>
#include "string_matching.h"

static void goodst(int* good_suffix_table, int* border_pos, char* pattern, int len );
static int next_state(char *pattern, int plen, int state, int x);
static void lps_array(char* pattern, int plen, int* lps);

int ASCI_SIZE = 256;
// Add any utility functions you need, eg Transition function for the Finite Automaton

int naive(char * text, char *pattern, int * indices){
	printf("Naive\t");
	int i, j;

	int k=0;
	int n=0;  //number of occurences

	int t=strlen(text);
	int p=strlen(pattern);
	for (i = 0; i <= t- p; i++) {
        for (j = 0; j < p; j++)
            if (text[i + j] != pattern[j])
                break;
 
        if (j == p) {
            //pattern found
			n++; //increase number of occurences
			indices[k]=i;         // add index 
			k++;
        }
    }
	
	return n;
}

int horspool(char * text, char *pattern, int * indices){
	printf("Implement Horspool\t");
	int n=0;  //number of occurences
	int i,j;
	int plen = strlen(pattern),tlen = strlen(text);

	//shift_table
	int* shift_table = (int*)malloc(sizeof(int)*ASCI_SIZE);
	for(i = 0 ; i < ASCI_SIZE ; i++){
		shift_table[i] = plen;
	}
	

	for(i = 0 ; i < plen-1 ; i++){
		shift_table[pattern[i]] = plen-1-i;
	}

	//stirng matching 
	i = plen-1;
	int count = 0;
	while(i < tlen){
		j = 0;
		while(j < plen && pattern[plen-1-j] == text[i-j])
			{j++;
				count++;}
		if(j == plen){
			indices[n++] = i-plen+1;
			i++;
		}
		else
			i = i + shift_table[text[i]];
	}

	free(shift_table);
	//printf("Count %d\n",count);
	return n;
}
// good suffix generator 
static void goodst(int* good_suffix_table, int* border_pos, char* pattern, int len ){
	int i = len;
	int j = len+1;
	border_pos[i] = j;
	while(i>0){
		while(j <= len && pattern[i-1] != pattern[j-1]){
			if(good_suffix_table[j] == 0)
				good_suffix_table[j] = j-i;
			j = border_pos[j];
		}
		i--;
		j--;
		border_pos[i] = j;
	}

    j = border_pos[0];
    for(i=0; i<=len; i++)
    {
        if(good_suffix_table[i] == 0)
            good_suffix_table[i] = j;
 
        if (i == j)
            j = border_pos[j];
    }
    
    return ;
}


int boyerMoore( char * text, char *pattern, int * indices){
	printf("Implement Boyer Moore\t");
	int n=0;  //number of occurences
	int i,j,shift_bad,shift_good,shift;
	int plen = strlen(pattern);
	int tlen = strlen(text);

	//bad_char_table
	int* bad_char_table = (int*)malloc(sizeof(int)*ASCI_SIZE);
	
	for(i = 0 ;i < ASCI_SIZE ;i++)
		bad_char_table[i]=plen;
	
	for(i= 0 ; i < plen ; i++){
		bad_char_table[pattern[i]] = i;
	}

	//good_suffix_table
	int* good_suffix_table = (int*)malloc(sizeof(int)*(plen+1));
	int* border_pos = (int*)malloc(sizeof(int)*(plen+1));
	memset(good_suffix_table,0,sizeof(int)*(plen+1));
	memset(border_pos,0,sizeof(int)*(plen+1));
	
	goodst(good_suffix_table,border_pos,pattern,plen);
	//printf("%d\n",tlen );
	int count=0;
	i = 0;
	while(i <= tlen - plen){
		j = plen - 1;
		//printf("--%d-- %c %c\n",j,pattern[j],text[i+j] );
		while(j >= 0 && pattern[j] == text[i+j])
			{j--;
				count++;}
		if(j < 0){
			indices[n++] = i;
			i++;
		}
		else{
			shift_bad = j - bad_char_table[text[i+j]];
			if(shift_bad <= 0){
				if(bad_char_table[text[i+j]] == plen)
					shift_bad =  j+1;
				else
					shift_bad = 1;
			}

			shift_good = good_suffix_table[j+1];
			shift = shift_good > shift_bad? shift_good : shift_bad ;
			//if(shift == shift_good)
				//printf("\ngood - %d    bad - %d\n",shift_good,shift_bad);		
			i+=shift;
			//printf(" -%d- \n",shift_bad );
			}
		}	
	free(good_suffix_table);
	free(border_pos);
	free(bad_char_table);
	//printf("%d\n",n );
	//printf("count %d\n",count );
	return n;
}

int rabinKarp( char * text, char *pattern, int * indices){
	printf("Implement Rabin Karp\t");
	int n=0;  //number of occurences
	int i,j;

	int mod = 257;//fermat prime
	int plen = strlen(pattern);
	int tlen = strlen(text);
	int text_hash = 0, pattern_hash = 0;
	
	int h = 1;         //h= Alphbet^plen-1 % mod
	for(i = 0; i < plen-1; i++){
		h = (h*ASCI_SIZE)%mod;
	}

	
	for(i = 0; i < plen; i++){
		pattern_hash = (ASCI_SIZE*pattern_hash + pattern[i])%mod;
		text_hash = (ASCI_SIZE*text_hash + text[i])%mod;
	}	

	for(i = 0; i <= tlen-plen; i++){

		if(pattern_hash == text_hash){
			
			for(j = 0; j < plen; j++){
				if(text[i+j] != pattern[j])
					break; 
			}

			if(j == plen){
				indices[n++] = i;
			}
		}

		if(i < tlen-plen){
			text_hash = (ASCI_SIZE*(text_hash - text[i]*h) + text[i+plen])%mod;

			if(text_hash < 0)
				text_hash +=  mod;
		}

	}

	
	return n;
}

 
static int next_state(char *pattern, int plen, int state, int x){
    
    if (state < plen && x == pattern[state])
        return state+1;
 
    
    int ns, i;
    for (ns = state; ns > 0; ns--)
    {
        if (pattern[ns-1] == x)
        {
            for (i = 0; i < ns-1; i++)
                if (pattern[i] != pattern[state-ns+1+i])
                    break;
            if (i == ns-1)
                return ns;
        }
    }
 
    return 0;
}
 

int finiteAutomaton( char * text, char *pattern, int * indices){
	printf("Implement Finite Automaton based search\t");
	int n = 0;
	int plen = strlen(pattern);
    int tlen = strlen(text);
 	int i;
    int** transition_table = (int**)malloc(sizeof(int*)*(plen+1));
	for(i = 0; i <= plen; i++){
		transition_table[i] = (int*)malloc(sizeof(int)*ASCI_SIZE);
	} 
 
    int state;
    for (state = 0; state <= plen; state++)
        for (i = 0; i < ASCI_SIZE; i++)
            transition_table[state][i] = next_state(pattern, plen, state, i);

 
    state=0;
    for (i = 0; i < tlen; i++)
    {
        state = transition_table[state][text[i]];
        if (state == plen)
            indices[n++]=i-plen+1;
    }

    for(i = 0; i <= plen; i++){
		int* p = transition_table[i];
		free(p);
	}
	return n;

}

static void lps_array(char* pattern, int plen, int* lps){
    
    int len = 0; 
    lps[0] = 0; 
 

    int i = 1;
    while (i < plen) {
        if (pattern[i] == pattern[len]) {
            len++;
            lps[i] = len;
            i++;
        }
        else 
        {
            if (len != 0) {
                len = lps[len - 1];
 
            }
            else 
            {
                lps[i] = 0;
                i++;
            }
        }
    }
}

int KMP( char * text, char *pattern, int * indices){
	printf("Implement KMP\t");
	int n=0;  //number of occurences

	int plen = strlen(pattern);
    int tlen = strlen(text);
 
    int* lps = (int*)malloc(sizeof(int)*plen);
 
    lps_array(pattern, plen, lps);
 
    int i = 0,j=0; 
    while (i < tlen) {
        if (pattern[j] == text[i]) {
            j++;
            i++;
        }
 
        if (j == plen) {
            indices[n++] = i-j;
            j = lps[j-1];
        }
 
       
        else if (i < tlen && pattern[j] != text[i]){
            if (j != 0)
                j = lps[j-1];
            else
                i = i + 1;
        }
    }	

    free(lps);
	return n;
}
