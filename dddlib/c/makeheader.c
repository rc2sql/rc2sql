#include <stdio.h>
#include <string.h>
#define BEGIN_EXPORT "BEGIN_EXPORT"
#define END_EXPORT   "END_EXPORT"

int main(int argc, char* argv[]) {
  int i; FILE* f;
  if (argc>1) {
    printf("#ifndef %s\n", argv[1]);
    printf("#define %s\n", argv[1]);
    for (i=1; i<argc; i++) {
      if ((f = fopen(argv[i], "r"))!=0) {
	int mode = 0;
	printf("\n\n/* ------------ %s ------------ */\n\n",argv[i]);
	while(!feof(f)) {
	  char buf[1000];
	  fgets(buf,1000,f);
	  if (strncmp(BEGIN_EXPORT, buf, strlen(BEGIN_EXPORT))==0)
	    mode = 1;
	  else if (strncmp(END_EXPORT, buf, strlen(END_EXPORT))==0)
	    mode = 0;
	  else if (mode==1)
	    printf("%s",buf);
	}
	fclose(f);
      }
    }
    printf("#endif /* %s */\n", argv[1]);
  }
  return 0;
}
