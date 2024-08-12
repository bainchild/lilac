// #include <string.h>
#define SIGTERM 15
extern int raise (int sig);
extern int strcmp (const char *__s1, const char *__s2);
int main(int argc, char* argv[]) {
  int retur;
  if(argc>1) {
    if(strcmp(argv[2],"yeah")==0 || strcmp(argv[2],"yes")==0) {
      retur=0;
    } else if (strcmp(argv[2],"nah")==0 || strcmp(argv[2],"no")==0) {
      retur=1;
    } else {
      retur=3;
    }
  } else {
    retur=4;
  }
  return retur;
}
