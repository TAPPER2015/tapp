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

    Node (int v, Node* u) {
      value = v;
      next = u;
    }

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
    int oldTop = top->value;
    top = top->next;
    return oldTop;
  }
}

void Stack::push (int value) {
  top = new Node (value,top); 
  return ;  
}
