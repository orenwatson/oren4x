#include "stdio.h"
int main(){
	int i=0,j=0,c;
	c=getchar();
	while(c!=EOF){
		if(c>='0'&&c<='9')i=i*16+c-'0';
		else if(c>='A'&&c<='F')i=i*16+c-'A'+10;
		else if(c>='a'&&c<='f')i=i*16+c-'a'+10;
		else goto skip;
		j++;
		if(j==2)putchar(i),j=0,i=0;
		skip:;
		c=getchar();
	}
}
