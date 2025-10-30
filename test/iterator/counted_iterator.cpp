#include "ranges/range_algorithm.hpp"
#include    "iterator/iterator.hpp"
#define preview scaler
#include <algorithm>
#include <iostream>
#include <iterator>
#include <string>
#include <vector>
#include <list>
#include "cassert"
#include <iomanip>
using std::operator""s;

template<class A,class B>
void print(A const remark,B const& v)
{
    const auto size = preview::ranges::ssize(v);
    std::cout << remark << '[' << size << "] { ";
    for (auto it = preview::counted_iterator{std::cbegin(v), size};
            it != preview::default_sentinel; ++it)
        std::cout << *it << (it.count() > 1 ? ", " : " ");
    std::cout << "}\n";
}

template<class A,class B>
void print1(A const& rem, B const& v)
{
    std::cout << rem << '[' << size(v) << "] {";
    char comma[]{0, ' ', 0};
    for (auto const& s : v)
        std::cout << comma << std::quoted(s), *comma = ',';
    std::cout << "}\n";
}

static_assert(preview::semiregular<preview::default_sentinel_t>);

void TestCtor()
{
    static constexpr auto pi = {3, 1, 4, 1, 5, 9, 2};
    
    // (1) default constructor:
    constexpr preview::counted_iterator<std::initializer_list<int>::iterator> i1{};
    static_assert(i1 == preview::default_sentinel);
    static_assert(i1.count() == 0);
    
    // (2) initializes the iterator and length respectively:
    constexpr preview::counted_iterator<std::initializer_list<int>::iterator> i2{
        pi.begin(), pi.size() - 2
    };
    static_assert(i2.count() == 5);
    static_assert(*i2 == 3 && i2[1] == 1);
    
    // (3) converting-constructor:
    preview::counted_iterator<std::initializer_list<const int>::iterator> i3{i2};
    
    preview::ranges::copy(i3, preview::default_sentinel,std::ostream_iterator<const int>{std::cout, " "});
}
void TestOperator()
{
    auto a = {3, 1, 4, 1, 5, 9, 2};
    preview::counted_iterator<std::initializer_list<int>::iterator> p(begin(a), size(a) - 2);
    preview::counted_iterator<std::initializer_list<int>::iterator> q;
    assert(q.count() == 0);
    assert(q.count() != p.count());
    q = p;
    assert(q.count() == p.count());
    assert(preview::ranges::equal(p, preview::default_sentinel, q, preview::default_sentinel));
}
void TestComp()
{
    static constexpr auto v = {1, 2, 3, 4, 5, 6};
    constexpr preview::counted_iterator<std::initializer_list<int>::iterator>
        it1{v.begin(), 5},
        it2{v.begin(), 5},
        it3{v.begin() + 1, 4},
        it4{v.begin(), 0};
    static_assert(it1 == it2);
    static_assert(it2 != it3);
    static_assert(it2 > it3);
    static_assert(it1 <= it2);
    static_assert(it3 != preview::default_sentinel);
    static_assert(it4 == preview::default_sentinel);
}
void TestBase()
{
    std::vector<int> v{0, 1, 2, 3, 4};
    
    std::reverse_iterator<std::vector<int>::iterator> reverse{v.rbegin()};
    
    preview::counted_iterator counted{reverse, 3};
    
    static_assert(std::is_same<
        decltype(counted.base()),
        std::reverse_iterator<std::vector<int>::iterator> const&
    >{});
    
    std::cout << "Print with reverse_iterator: ";
    for (auto r = counted.base(); r != v.rend(); ++r)
        std::cout << *r << ' ';
    std::cout << '\n';
    
    std::cout << "Print with counted_iterator: ";
    for (; counted != preview::default_sentinel; ++counted)
        std::cout << counted[0] << ' ';
    std::cout << '\n';
}
void TestComp1()
{
    static constexpr auto v = {1, 2, 3, 4};
    constexpr preview::counted_iterator<std::initializer_list<int>::iterator>
        it1{v.begin(), 3},
        it2{v.begin(), 0};
    static_assert(it1 != preview::default_sentinel);
    static_assert(it2 == preview::default_sentinel);
    static_assert(preview::default_sentinel != it1);
    static_assert(preview::default_sentinel == it2);
}
void TestCount()
{
    constexpr static auto il = {1, 2, 3, 4, 5};
    constexpr preview::counted_iterator i1{il.begin() + 1, 3};
    static_assert(i1.count() == 3);
    auto i2{i1};
    for (; preview::default_sentinel != i2; ++i2)
        std::cout << "*i2: " << *i2 << ", count(): " << i2.count() << '\n';
    assert(i2.count() == 0);
}
void TestOperatorBracket()
{
    std::array array{'A', 'B', 'C', 'D', 'E'};
    
    preview::counted_iterator it{array.begin() + 1, /*count:*/ 3};
    
    for (int i{}; i != it.count(); ++i)
        std::cout << it[i] << ' ';
    std::cout << '\n';
    
    for (int i{}; i != it.count(); ++i)
        it[i] += ('E' - 'A');
    
    for (int i{}; i != it.count(); ++i)
        std::cout << it[i] << ' ';
    std::cout << '\n';
    
    std::list list{'X', 'Y', 'Z', 'W'};
    preview::counted_iterator it2{list.begin(), 3};
//  char x = it2[0]; // Error: requirement `random_access_iterator` was not satisfied.
    std::cout << *it2 << '\n'; // OK
}
void TestIterMove()
{
    std::vector<std::string> p{"Alpha", "Bravo", "Charlie"}, q;
    print1("p", p);
    print1("q", q);
    
    using RI = preview::counted_iterator<std::vector<std::string>::iterator>;
    
    for (RI iter{p.begin(), 2}; iter != preview::default_sentinel; ++iter)
        q.emplace_back(/* ADL */ iter_move(iter));
    
    print1("p", p);
    print1("q", q);
}
void TestIterSwap()
{
    std::vector p{1, 2, 3, 4},
        q{5, 6, 7, 8};
    
    preview::counted_iterator<std::vector<int>::iterator> ip{p.begin(), 2};
    preview::counted_iterator<std::vector<int>::iterator> iq{q.begin(), 3};
    
    std::cout << *ip << ' ' << *iq << '\n';
    iter_swap(ip, iq); // ADL
    std::cout << *ip << ' ' << *iq << '\n';
    
    std::list x{0, 1, 3};
    preview::counted_iterator<std::list<int>::iterator> ix{x.begin(), 2};
}


template<class T>
struct A
{
    template<class U = T,class = std::enable_if_t<std::is_same_v<int,U>>>
    void ss()
    {}
    
    template<bool _v = std::is_same_v<int,T>,class = std::enable_if_t<_v>>
    void ss(int)
    {}
};
int main()
{
    static_assert(!preview::random_access_iterator<int>);
    static_assert(preview::contiguous_iterator<double*>);
    /*static_assert(std::is_same_v<meta::cref_t<int>,const int&>);
    const auto src = {"Arcturus"s, "Betelgeuse"s, "Canopus"s, "Deneb"s, "Elnath"s};
    preview::counted_iterator gg{src.begin(), 3};
    print("src", src);
    std::vector<decltype(src)::value_type> dst;
    
    using CT = decltype(preview::counted_iterator{src.begin(), 3});
    static_assert(preview::input_or_output_iterator<CT>);
    static_assert(preview::sentinel_for<preview::default_sentinel_t,CT>);
    preview::ranges::copy(preview::counted_iterator{src.begin(), 3},
                      preview::default_sentinel,
                      std::back_inserter(dst));
    print("dst", dst);
    TestIterMove();*/
    TestIterSwap();
    TestIterMove();
    std::vector v{1, 2, 3, 4};
    std::list l{1, 2, 3, 4};
    preview::counted_iterator iv{v.begin(), 3};
    preview::counted_iterator il{l.begin(), 3};
    using T = decltype(iv);
    static_assert(preview::random_access_iterator<T>);

    using namespace preview;
    static_assert(same_as<iter_value_t<T>,remove_cvref_t<iter_reference_t<T>>>);
    static_assert(std::is_lvalue_reference_v<preview::iter_reference_t<T>>);
    
    static_assert(std::is_same<void, typename preview::iterator_traits<decltype(il)>::pointer>());
}
