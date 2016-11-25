#include <iostream>
#include <cstdlib>
#include <pthread.h>
#include <atomic>
#include "nb-stack-int.h"

using namespace std;

#define NTHREADS 8
#define NPUSHPOP 2

struct args {
  int tid;
  Stack* s;
};


void *pushPop(void *a)
{  
  args* myargs = (args*) a;
  int tid = myargs->tid;
  Stack* sharedStack = myargs->s;

  cout << "Hello world! It is me, 00" << tid << endl;
  
  for (int i = 0; i < NPUSHPOP; ++i) {
    int j = NPUSHPOP * tid + i;
    sharedStack->push (NPUSHPOP * tid + i);
    sleep (1);
    int k = sharedStack->pop ();
    sleep (1);
  }

  pthread_exit(NULL);
}

int main ()
{
  pthread_t threads[NTHREADS];
  Stack* sharedStack = new Stack (); 
  
  int rc;
  for(int i=0; i < NTHREADS; i++ ){
    args* a = new args;
    a->tid = i;
    a->s = sharedStack;

    int error = pthread_create(&threads[i], NULL, pushPop, (void *) a);
    if (error) {
      cout << "Error: unable to create thread," << error << endl;
      exit(-1);
    }
  }
  pthread_exit(NULL);
}
