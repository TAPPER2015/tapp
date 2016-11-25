#include <iostream>
#include <cstdlib>
#include <atomic>

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
  std::atomic<Node*> top;

  Stack () {
    top = NULL;
  };

  int pop ();
  void push (int v);
};


int Stack::pop () {
  if (top.load() == NULL) {
    cout << "Stack is Empty" << endl;
    return -12;
  }
  else {
    while (1) { 
      Node* oldTop = top.load();
      int oldTopValue = oldTop->value;
      Node* next = oldTop->next;
      
      if (top.compare_exchange_strong(oldTop,next)) {
        return oldTopValue;
      }
    }
  }
}

void Stack::push(const int value) 
{ 
  Node *u = new Node(value); 
  while (1) { 
    u->next = top.load();
    if (top.compare_exchange_strong(u->next, u)) { 
      break;
    }
  }
}
