#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main (int argc, char **argv) {

  int i = 0;

  srand((unsigned int)time(NULL));
  
  int r_int = rand() % 10000;
  float r_float = (float)(rand() % 10000) / 10000; 
  
  char cwd[1024];
  if (getcwd(cwd, sizeof(cwd)) != NULL)
    printf("Current working dir: %s\n", cwd);
  else
    printf("getcwd() error");
  

  printf("NumArgs: %d\n", argc);

  for (i = 0; i < argc; i ++) {
    printf("%s\n", argv[i]);
    
  }

  
  sleep(1);

  printf("\n"); 
  
  /*Tag examples*/
  printf("TAG0: %d\n", r_int);
  printf("TAG1: %f\n", r_float);
	 
  
  return 0;
}
