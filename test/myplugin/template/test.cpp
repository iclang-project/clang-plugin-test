class Test {
public:
    int x = 3;
    operator int () { return x; }
};

template<typename T> T test(T x) { return x; }

template<typename T, typename T2> void test2() {}

template<typename T> class A {
public:
  T foo(T x) { return x; }
};

int main() {
  test2<int*, A<int>>();

  int r1 = test<int>(1); // template func: test, type: int
  A<int> a;
  int r2 = a.foo(2); // template func: A.foo, type: int

  Test t;
  int r3 = test<Test>(t); // template func: test, type: Test
  A<Test> a2;
  int r4 = a.foo(t) + 1; // template func: A.foo, type: Test
  return r1 + r2 + r3 + r4;
}