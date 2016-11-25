#include <iostream>
#include <cstdlib>
#include <pthread.h>
#include <atomic>
#include "stack-int.h"

using namespace std;

#define NTHREADS 8
#define NPUSHPOP 1

std::atomic<bool> BigLock;


struct args {
  int tid;
	Stack* s;
};

/* spin lock */
void takeLock () {
  while (1) {
    bool flag = false;
    if (BigLock.compare_exchange_strong(flag,true)) {
		  return;
		}		
	}
}

/* spin lock */
void releaseLock () {
  while (1) {
    bool flag = true;
    if (BigLock.compare_exchange_strong(flag,false)) {
		  return;
		}		
	}
}

void *pushPop(void *a)
{  
  args* myargs = (args*) a;
	int tid = myargs->tid;
  Stack* sharedStack = myargs->s;

	takeLock ();
  cout << "Hello world! It is me, 00" << tid << endl;
	releaseLock ();
	
  for (int i = 0; i < NPUSHPOP; ++i) {
    int j = NPUSHPOP * tid + i;
    sharedStack->push (j)
    takeLock ();
    cout << "Thread " << tid << " pushed " << j << "+" << endl;
    releaseLock ();
		sleep (1);

    int k = sharedStack->pop ();
    takeLock ();
    cout << "Thread " << tid << " popped " << k << "-" << endl;
    releaseLock ();
		sleep (1);
  }

  pthread_exit(NULL);
}

int main ()
{
  pthread_t threads[NTHREADS];
  Stack* sharedStack = new Stack (); 
  BigLock.store(false);
	
  for(int i=0; i < NTHREADS; i++ ){
		args* a = new args;
		a->tid = i;
		a->s = sharedStack;
		
  	takeLock ();
    cout << "main: creating thread 00" << i << endl;
    releaseLock ();
    int error = pthread_create(&threads[i], NULL, pushPop, (void *) a);
    if (error) {
      cout << "Error: unable to create thread," << error << endl;
      exit(-1);
    }
  }
  pthread_exit(NULL);
}
