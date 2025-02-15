namespace N {
class A {
public:
  int foo();
};

int A::foo() { return 0; }

template<typename T>
class AT;

template<typename T>
class AT {
public:
  int foo();
};

template<typename T> int AT<T>::foo() { return 1; }

template<typename T>
class AT;

void safe__id() {}

template<typename T> T ft(T x) { return x; }

int test() {
  AT<int> at;
  at.foo();
  ft(1.0);
  class Inner {
    public:
      int innerFoo() { return 0; }
  };
  return 1;
}

int main() { return test(); }
}