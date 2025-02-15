template<typename T> class A { public:
    class B { public:
};};

template<typename T> class C {
};

int main() {
    C<A<int>::B> c;
}