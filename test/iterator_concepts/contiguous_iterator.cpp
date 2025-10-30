#include "../../scaler/concepts/iterator_concepts.hpp"
#include<iostream>
#include <iostream>
#include <vector>
#include <deque>
#include <iterator>
#include <type_traits>

int main() {
    std::vector<int> vec;
    std::deque<int> deq;

    using VecIt = decltype(vec)::iterator;
    using DeqIt = decltype(deq)::iterator;

    // 方法一：检查迭代器类别标签
    // 注意：这是一个编译时表达式，实际代码中常用static_assert或if constexpr
    std::cout << "Vector iterator is contiguous: "
              << (std::is_same_v<typename std::iterator_traits<VecIt>::iterator_category,
                                 std::random_access_iterator_tag>)
              << std::endl;

    std::cout << "Deque iterator is contiguous: "
              << (std::is_same_v<typename std::iterator_traits<DeqIt>::iterator_category,
                                 std::random_access_iterator_tag>)
              << std::endl;

    // 方法二：使用C++20的concept特性（更现代的方式）
    std::cout << "Vector iterator is contiguous: "
              << scaler::contiguous_iterator<VecIt>
              << std::endl;

    std::cout << "Deque iterator is contiguous: "
              << scaler::contiguous_iterator<DeqIt>
              << std::endl;

    return 0;
}