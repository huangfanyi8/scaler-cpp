#include"../../scaler/ranges/range_view.hpp"
#include<iostream>
#include<variant>
#include<vector>

struct A
{
    template<class...Args>
    constexpr explicit A(Args&&...args)
        :vec{std::forward<Args>(args)...}
    {}

    template<class T>
    constexpr  A(std::initializer_list<T> l)
        :vec{l}
    {}
    std::vector<int> vec;
};

void print(const std::vector<int>&v)
{
    for (const auto&vv:v)
        std::cout<<vv<<" ";
    std::cout<<'\n';
}
void print(const A&v)
{
    print(v.vec);
}
int main()
{
    A a({1,2});
    A b(1,2,8,9);

    std::vector<int>  c={12,3};
    std::vector<int>  d(12,3);
    print(a);
    print(b);
    print(c);
    print(d);
}