#include <iostream>
#include <cstdlib>
#include "nb-stack-int-aba.h"

using namespace std;


int main () {
  Stack* S = new Stack (); 
  int i; 

  cout << "Stack driver..." << endl;

  (*S).pop ();
  (*S).push (1);
  cout << "Push 1" << endl;
  (*S).push (2);
  cout << "Push 2" << endl;
  (*S).push (3);
  cout << "Push 3" << endl;
  i = (*S).pop ();
  cout << "Pop returns " << i << endl;
  i = (*S).pop ();
  cout << "Pop returns " << i << endl;
  i = (*S).pop ();
  cout << "Pop returns " << i << endl;
  i = (*S).pop ();
  cout << "Pop returns " << i << endl;
  return 1;
}
