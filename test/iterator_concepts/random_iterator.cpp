
#include <cstddef>
#include "../../scaler/concepts//iterator_concepts.hpp"
#define preview scaler

namespace cxx20
{
    template<class Iter,class = std::enable_if_t<preview::input_or_output_iterator<Iter>>>
    constexpr preview::iter_difference_t<Iter> distance(Iter first, Iter last)
    {
        if constexpr(preview::random_access_iterator<Iter>)
            return last - first;
        else
        {
            preview::iter_difference_t<Iter> result{};
            for (; first != last; ++first)
                ++result;
            return result;
        }
    }
}

static_assert(preview::totally_ordered<const int*>&& preview::sized_sentinel_for<const int*,const int*>);
int main()
{
    static constexpr auto il = {3, 1, 4};
    static_assert(preview::bidirectional_iterator<const int*>);
    static_assert(preview::derived_from<preview::_iter_concept<const int*>,std::random_access_iterator_tag>);
    static_assert(preview::equality_comparable<const int*>);
    
    static_assert(preview::random_access_iterator<decltype(il.begin())> &&
                  cxx20::distance(il.begin(), il.end()) == 3 &&
                  cxx20::distance(il.end(), il.begin()) == -3);
}