#include <iostream>
#include <cstdlib>

using namespace std;

class Node {
 private: 
  int value;
  Node* next;

 public:
  Node (int v) {
    value = v;
    next = NULL;
  }

  Node (int v, Node* u) {
    value = v;
    next = u;
  }

  int getValue () {
    return value;
  };
  
  Node* getNext () {
    return next; 
  };

  void setNext (Node u) {
    *next = u;
  };

};

class Stack {

 private: 
  Node* top;

 public: 
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
    int oldTop = (*top).getValue();   
    top = (*top).getNext ();
    return oldTop;
  }
}

void Stack::push (int value) {
  Node* u; 
  if (top == NULL) {
    u = new Node (value); 
  }
  else {
    Node* oldTop = top;
    u = new Node (value,oldTop);
  } 

  top = u;
  return ;  
}
