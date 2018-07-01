#include <unistd.h>
#include <stdio.h>

int main (int argc, char **argv) {

  int i = 0;
  
  char cwd[1024];
  if (getcwd(cwd, sizeof(cwd)) != NULL)
    printf("Current working dir: %s\n", cwd);
  else
    printf("getcwd() error");
  

  printf("NumArgs: %d\n", argc);

  for (i = 0; i < argc; i ++) {
    printf("%s\n", argv[i]);
    
  }

  /*Tag examples*/
  printf("TAG0: 15\n");
  printf("TAG1: 1.9\n");
	 
  
  return 0;
}
