#include "../../scaler/concepts//iterator_concepts.hpp"
#define preview scaler
#include "iostream"
#include <cstddef>

struct SimpleIterator
{
  using difference_type = std::ptrdiff_t;
  
  int operator*();
  
  SimpleIterator& operator++();
  void operator++(int) { ++*this; }
};

static_assert(std::is_base_of_v<preview::_details::_iterator_traits<SimpleIterator>,preview::iterator_traits<SimpleIterator>>);
static_assert(!preview::indirectly_readable<SimpleIterator>);
static_assert(!preview::_details::_is_cpp17_iterator<SimpleIterator>);
static_assert(!preview::_details::_is_cpp17_input_iterator<SimpleIterator>);
//static_assert(std::is_same_v<typename preview::iterator_traits<SimpleIterator>::difference_type,ptrdiff_t>);
static_assert(std::is_same_v<typename preview::incrementable_traits<scaler::remove_cvref_t<SimpleIterator>>::difference_type,ptrdiff_t>);
static_assert(std::is_same_v<preview::iter_difference_t<SimpleIterator>,ptrdiff_t>);
static_assert(preview::movable<SimpleIterator>);
static_assert(preview::weakly_incrementable<SimpleIterator>);
static_assert(preview::input_or_output_iterator<SimpleIterator>);

int main()
{
    std::cout<<__cplusplus;
}
