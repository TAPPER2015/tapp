#include <iostream>
#include <cstdlib>
#include <pthread.h>
#include <atomic>
#include "nb-stack-int.h"

using namespace std;

#define NTHREADS 8
#define NPUSHPOP 2

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
    sharedStack->push (NPUSHPOP * tid + i);
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

  std::atomic<bool> flag;

  flag.store(false);
  bool expected = false;
  bool was_successful = flag.compare_exchange_strong(expected, true);
  std::cout << "was_successful = " << was_successful
						<< "; flag = " << flag.load()
						<< "; expected = " << expected
						<< std::endl;

  bool expected2 = false;
  bool was_successful2 = flag.compare_exchange_strong(expected2, true);
  std::cout << "was_successful2 = " << was_successful2
						<< "; flag = " <<	flag.load()
						<< "; expected2 = " << expected2
						<< std::endl;

}
