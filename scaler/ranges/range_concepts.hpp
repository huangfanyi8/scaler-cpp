
/*cc++20 范围概念
 * borrowed_range
 * sized_range
 *input_range
 * output_range
 *forward_range
 * */

#ifndef SCALER_RANGES_CONCEPTS_HPP
#define SCALER_RANGES_CONCEPTS_HPP

#include "range_primitives.h"

namespace NAMESPACE_RANGES
{
    template<class T>class view_interface;

    template<class Range>
    CXX17_CONCEPT   range = _details::_range<Range>::value;

    template< class T >
    CXX17_CONCEPT borrowed_range =
            ranges::range<T> &&
            (std::is_lvalue_reference_v<T> ||
             ranges::enable_borrowed_range<remove_cvref_t<T>>);

    namespace _details
    {
        template<class T,class = void>
        struct _sized_range
                :std::false_type
        {};

        template<class T>
        struct _sized_range<T,std::void_t<std::enable_if_t<ranges::range<T>>,
                decltype(ranges::size(std::declval<T&>()))>>
                :std::true_type
        {};

        template< class T ,class  = void>
        struct _common_range
                :std::false_type
        {};

        template< class T >
        struct _common_range<T,std::enable_if_t<ranges::range<T>&&same_as<ranges::iterator_t<T>, ranges::sentinel_t<T>>>>
                :std::true_type
        {};

        template<class Range, class T,class = void>
        struct _output_range:std::false_type {};

        template<class Range, class T>
        struct _output_range<Range,T,
                std::void_t<iterator_t<Range>,std::enable_if_t<range<Range> && output_iterator<iterator_t<Range>, T>>>>
                :std::true_type
        {};

        template<class T,class = void>
        struct _input_range
                :std::false_type
        {};

        template<class T>
        struct _input_range<T,std::enable_if_t<ranges::range<T> && input_iterator<ranges::iterator_t<T>>>>
                :std::true_type
        {};

        template<class T,class = void>
        struct _forward_range
                :std::false_type
        {};

        template<class T>
        struct _forward_range<T,std::void_t<
                std::enable_if_t<_input_range<T>::value && forward_iterator<ranges::iterator_t<T>>>>>
                :std::true_type
        {};

        template<class T,class = void>
        struct _bidirectional_range
                :std::false_type
        {};

        template<class T>
        struct _bidirectional_range<T,std::void_t<ranges::iterator_t<T>,
                std::enable_if_t<_forward_range<T>::value && bidirectional_iterator<ranges::iterator_t<T>>>>>
                :std::true_type
        {};

        template<class T,class = void>
        struct _random_access_range
                :std::false_type
        {};

        template<class T>
        struct _random_access_range<T,std::void_t<ranges::iterator_t<T>,
                std::enable_if_t<_bidirectional_range<T>::value &&random_access_iterator<ranges::iterator_t<T>>>>>
                :std::true_type
        {};

        template<class T,class = void>
        struct _contiguous_range
                :std::false_type
        {};

        template<class T>
        struct _contiguous_range<T,std::void_t<ranges::iterator_t<T>,
                std::enable_if_t<
                        same_as<decltype(ranges::data(std::declval<T&>())),std::add_pointer_t<range_reference_t<T>>>
                        &&_random_access_range<T>::value
                        &&contiguous_iterator<ranges::iterator_t<T>>>>>
                :std::true_type
        {};

        template< class T ,class = void>
        struct _constant_range
                :std::false_type
        {};

        template< class T >
        struct _constant_range<T,std::enable_if_t<_input_range<T>::value
                                                  &&input_iterator<T>
                                                  &&same_as<iter_const_reference_t<T>, iter_reference_t<T>>>>
                :std::true_type
        {};

        template<class T>
        struct _is_initializer_list
                :std::false_type
        {};

        template<class U>
        struct _is_initializer_list<std::initializer_list<U>>
                :std::true_type
        {};

        template<class T>
        struct _is_derived_from_view_interface
                :std::true_type
        {};

        template<class U>
        struct _is_derived_from_view_interface<view_interface<U>>
        :std::false_type
    {};
    }

    template<class Range>
    CXX17_CONCEPT  input_range = _details::_input_range<Range>::value;

    template<class Range,class T>
    CXX17_CONCEPT  output_range =  _details::_output_range<Range,T>::value;

    template<class Range>
    CXX17_CONCEPT  forward_range = _details::_forward_range<Range>::value;

    template<class Range>
    CXX17_CONCEPT  bidirectional_range = _details::_bidirectional_range<Range>::value;

    template<class Range>
    CXX17_CONCEPT  random_access_range = _details::_random_access_range<Range>::value;

    template< class T >
    CXX17_CONCEPT contiguous_range = _details::_contiguous_range<T>::value;

    template<class Range>
    CXX17_CONCEPT  constant_range = _details::_constant_range<Range>::value;

    template<class T>
    CXX17_CONCEPT sized_range = _details::_sized_range<T>::value;

    template<class Range>
    CXX17_CONCEPT common_range = _details::_common_range<Range>::value;

    struct view_base{};

    template<class T>
    inline constexpr bool enable_view = derived_from<T, view_base>||_details::_is_derived_from_view_interface<T>::value ;

    template<class T>
    CXX17_CONCEPT view = ranges::range<T> &&movable<T> && ranges::enable_view<T>;

    template<class T,class  =void>
    struct is_viewable_range
            :std::false_type
    {};

    template<class T>
    struct is_viewable_range<T,std::enable_if_t< ranges::range<T> &&
                                                 ((ranges::view<remove_cvref_t<T>> &&
                                                   constructible_from<remove_cvref_t<T>, T>) ||
                                                  (!ranges::view<remove_cvref_t<T>> &&
                                                   (std::is_lvalue_reference_v<T> ||
                                                    (movable<std::remove_reference_t<T>>&&_details::_is_initializer_list<remove_cvref_t<T>>::value ))))>>
            :std::true_type
    {};

    template<class T>
    CXX17_CONCEPT  viewable_range = is_viewable_range<T>::value;

    struct dangling
    {
        constexpr dangling() noexcept = default;
        template<class...Args>
        constexpr dangling(Args&&...) noexcept { }
    };

    template< class Range,class = std::enable_if_t<ranges::range<Range>> >
    using borrowed_iterator_t = std::conditional_t<ranges::borrowed_range<Range>,ranges::iterator_t<Range>, ranges::dangling>;

}
#endif
