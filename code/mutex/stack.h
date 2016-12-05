#include <iostream>
#include <cstdlib>

using namespace std;

class Node {
public:
  int value;
  Node* next;

  Node (int v) {
    value = v;
    next = NULL;
  }
};

class Stack {
public:
  Node* top;

  Stack () {
    top = NULL;
  };

  int pop ();
  void push (int v);
};

int Stack::pop () {
  if (top == NULL) {
    cout << "Stack is Empty" << endl;
    return -12;
  }
  else {
    int oldTop = top->value;
    top = top->next;
    return oldTop;
  }
}

void Stack::push (int value) {

  Node* u = new Node (value); 
  u -> next = top;
	top = u;
  return ;  
}
