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
    while (1) { 
			int oldTop = top->value;
			Node* next = top->next;
			if (__sync_bool_compare_and_swap(&top, top, next)) { 
				return oldTop;
			}
    }
	}
}

void Stack::push(const int value) 
{ 
    Node *u = new Node(value); 
    while (1) { 
			u->next = top;
			if (__sync_bool_compare_and_swap(&top, u->next, u)) { 
				break;
			}
    }
}
