#ifndef SCALER_MEMORY_POINTER_TRAITS_HPP
#define SCALER_MEMORY_POINTER_TRAITS_HPP

#include "../type_traits/type_traits.hpp"
#include<memory>

namespace NAMESPACE_NAME
{
    using std::addressof;

    namespace _details
    {
        template<int _v,class Ptr, class = void>
        struct _element_type
                :_element_type<_v+1,Ptr>
        {};

        template<class Ptr>
        struct _element_type <1,Ptr, std::void_t<typename Ptr::element_type>>
        {using element_type = typename Ptr::element_type;};

        template<template<class...> class Ptr, class T, class... Args>
        struct _element_type<2,Ptr<T, Args...>>
        {using element_type = T;};

        template<class Ptr>
        struct _element_type<3,Ptr*>
        {using element_type =Ptr;};

        template<class Ptr>
        struct _element_type<4,Ptr>
        {};

        template<class Ptr,class = void>
        struct _difference_type
        {using difference_type = std::ptrdiff_t;};

        template<class Ptr>
        struct _difference_type<Ptr,std::void_t<typename Ptr::difference_type>>
        {using difference_type = typename Ptr::difference_type;};

        template <int _v,class Ptr,class Other,class = void>
        struct _pointer_rebind
                :_pointer_rebind<_v+1,Ptr,Other>
        {};

        template <class Ptr,class Other>
        struct _pointer_rebind<1,Ptr, Other, std::void_t<typename Ptr::template rebind<Other>>>
        {using type = typename Ptr::template rebind<Other>;};

        template <class Ptr,class Other>
        struct _pointer_rebind<2,Ptr*, Other>
        {using type = Other*;};

        template<template<class...>class Template,class F,class O,class...R>
        struct _pointer_rebind<3,Template<F,R...>,O>
        {using type = Template<O,R...>;};

        template <class Ptr,class Other>
        struct _pointer_rebind<4,Ptr,Other>
        {};

        template<class,class = void>
        struct _has_pointer_to
                :std::false_type
        {};

        template<class Ptr>
        struct _has_pointer_to<Ptr,
                std::void_t<std::enable_if_t<std::is_object_v<typename Ptr::element_type>>, decltype(Ptr::pointer_to(std::declval<typename Ptr::element_type&>()))>>
                :std::true_type
        {};

        template<class Ptr>
        struct _has_pointer_to<Ptr,std::enable_if_t<std::is_pointer_v<Ptr>>>
                :std::true_type
        {};
    }

    template<class Ptr>
    struct pointer_traits
            :_details::_difference_type<Ptr>,
             _details::_element_type<1,Ptr>
    {
        using pointer = Ptr;
        template<class O>
        using rebind =typename _details::_pointer_rebind<1,Ptr,O>::type;

        template<class P = Ptr,
                class = std::enable_if_t<
                        !std::is_void_v<typename pointer_traits<P>::element_type>&&
                        _details::_has_pointer_to<P>::value>>
        static pointer pointer_to(typename pointer_traits<P>::element_type&e)
        {
            if constexpr (std::is_pointer_v<P>)
                return addressof(e);
            else
                return P::pointer_to(e);
        }
    };

    template<class Ptr,class O>
    using _pointer_rebind_t = typename  pointer_traits<Ptr>::template rebind<O>;

    template<template<class >class Traits,class,class = void>
    struct _has_pointer_to
            :std::false_type
    {};

    template<template<class >class Traits,class T>
    struct _has_pointer_to<Traits,T,std::void_t<decltype(Traits<T>::to_address(std::declval<const T&>()))>>
            :std::true_type
    {};

    template<class T>
    constexpr T* to_address(T* p) noexcept
    {
        static_assert(!std::is_function_v<T>);
        return p;
    }

    template<class T>
    constexpr auto  to_address(const T& p) noexcept
    {
        if constexpr (_has_pointer_to<pointer_traits,T>::value)
            return pointer_traits<T>::to_address(p);
        else if constexpr (_has_pointer_to<std::pointer_traits,T>::value)
            return std::pointer_traits<T>::to_address(p);
        else
            return NAMESPACE_NAME::to_address(p.operator->());
    }
} // namespace preview

#endif

