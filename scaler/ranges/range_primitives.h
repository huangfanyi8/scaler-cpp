/*C++20范围原语*/

#ifndef DONGDONG_RANGE_PRIMITIVES_H
#define DONGDONG_RANGE_PRIMITIVES_H

#include"range_access.hpp"

namespace  NAMESPACE_RANGES
{
    template< class T >
    using iterator_t = decltype(ranges::begin(std::declval<T&>()));

    template< class R ,class = std::enable_if_t<_details::_range<R>::value>>
    using const_iterator_t = decltype(ranges::cbegin(std::declval<R&>()));;

    template< class R ,class =std::enable_if_t<_details::_range<R>::value>>
    using sentinel_t = decltype(ranges::end(std::declval<R&>()));

    template< class R ,class =std::enable_if_t<_details::_range<R>::value> >
    using const_sentinel_t = decltype(ranges::cend(std::declval<R&>()));

    template< class R ,class =std::enable_if_t<_details::_range<R>::value> >
    using range_size_t = decltype(ranges::size(std::declval<R&>()));;

    template< class R ,class =std::enable_if_t<_details::_range<R>::value> >
    using range_difference_t = iter_difference_t<ranges::iterator_t<R>>;

    template< class R ,class =std::enable_if_t<_details::_range<R>::value> >
    using range_value_t = iter_value_t<ranges::iterator_t<R>>;

    template< class R ,class =std::enable_if_t<_details::_range<R>::value> >
    using range_reference_t = iter_reference_t<ranges::iterator_t<R>>;

    template< class R ,class =std::enable_if_t<_details::_range<R>::value> >
    using range_const_reference_t = iter_const_reference_t<ranges::iterator_t<R>>;

    template< class R ,class =std::enable_if_t<_details::_range<R>::value> >
    using range_rvalue_reference_t = iter_rvalue_reference_t<ranges::iterator_t<R>>;

    template< class R ,class =std::enable_if_t<_details::_range<R>::value> >
    using range_common_reference_t = iter_common_reference_t<ranges::iterator_t<R>>;
}


#endif //DONGDONG_RANGE_PRIMITIVES_H
