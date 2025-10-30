#include "../../scaler/variant.h"

#include <cstdint>
#include <iostream>
#include <limits>
#include<vector>
#include<variant>

template<class T>
void print(std::vector<T>&v)
{
    for (const auto &x : v)
        std::cout<<x<" ";
    std::cout<<'\n';
}

struct A
{
    A() = default;
    A( const A&) = default;
    A(  A&&) = default;
    A&operator=(const A&)
    {
        std::cout<<"A operator =(const A&)";
        return *this;
    }
    A&operator=( A&&)
    {
        std::cout<<"A operator =(A&&)";
        return *this;
    }
};

struct  B
    :A
{
    using A::A;

};
int main()
{
    scaler::variant<int,double,char>  cc;;
    B b1;
    B b2;
    b1=b2;

    constexpr size_t _size= std::numeric_limits<unsigned char>::max();
}
