#include <cstddef>
#include "../../scaler/concepts//iterator_concepts.hpp"

struct SimpleInputIterator
{
    using difference_type = std::ptrdiff_t;
    using value_type = int;
    
    int operator*() const;
    
    SimpleInputIterator& operator++();
    void operator++(int) { ++*this; }
};

struct SimpleForwardIterator
{
    using difference_type = std::ptrdiff_t;
    using value_type = int;
    
    int operator*() const;
    
    SimpleForwardIterator& operator++();
    
    SimpleForwardIterator operator++(int)
    {
        auto tmp = *this;
        ++*this;
        return tmp;
    }
    
    bool operator==(const SimpleForwardIterator&) const;
    bool operator!=(const SimpleForwardIterator&) const;
};

static_assert(scaler::indirectly_readable<SimpleForwardIterator>);
static_assert(scaler::forward_iterator<SimpleForwardIterator>);
static_assert(scaler::input_iterator<SimpleForwardIterator>);
static_assert(scaler::sentinel_for<SimpleForwardIterator,SimpleForwardIterator>);
static_assert(scaler::incrementable<SimpleForwardIterator>);
static_assert(scaler::derived_from<scaler::_iter_concept<SimpleForwardIterator>, scaler::forward_iterator_tag>);
static_assert(scaler::_details::_is_cpp17_input_iterator<SimpleForwardIterator>);
static_assert(std::is_same_v<std::input_iterator_tag,
    typename scaler::iterator_traits<SimpleForwardIterator>::iterator_category >);

static_assert(std::is_same_v<std::random_access_iterator_tag,scaler::_iter_concept<SimpleForwardIterator>>);;