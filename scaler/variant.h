#ifndef SCALER_VARIANT_H
#define SCALER_VARIANT_H

#include <cstddef>
#include <type_traits>
#include <utility>
#include <functional>
#include <variant>

#if defined(_MSVC_LANG)
#define STL_LANG _MSVC_LANG
#define  UNREACHABLE __assume(false);
#else
#define STL_LANG __cplusplus
#define  UNREACHABLE __builtin_unreachable();
#endif

//生成8个案例
#define VARIANT_STAMP8(_begin, _case) \
    _case(_begin)                 \
    _case(_begin + 1)             \
    _case(_begin + 2)            \
    _case(_begin + 3)\
    _case(_begin+4)\
    _case(_begin+5)\
    _case(_begin+6)\
    _case(_begin+7)

//生成16个案例
#define VARIANT_STAMP16(_begin,_case)\
    VARIANT_STAMP8(_begin,_case)\
    VARIANT_STAMP8(_begin+8,_case)

#define VARIANT_STAMP32(_begin,_case) \
    VARIANT_STAMP16(_begin, _case);     \
    VARIANT_STAMP16(_begin+16,_case)

#define VARIANT_STAMP64(_begin,_case) \
    VARIANT_STAMP32(_begin, _case);     \
    VARIANT_STAMP32(_begin+32,_case)

#define VARIANT_STAMP128(_begin,_case) \
    VARIANT_STAMP64(_begin, _case);     \
    VARIANT_STAMP64(_begin+64,_case)

#define VARIANT_STAMP256(_begin,_case) \
    VARIANT_STAMP128(_begin, _case);     \
    VARIANT_STAMP128(_begin+128,_case)

#define VARIANT_STAMP512(_begin,_case) \
    VARIANT_STAMP256(_begin, _case);     \
    VARIANT_STAMP256(_begin+256,_case)

#define VISIT_CASE(n)\
    case n:\
    {\
        if constexpr(_cases>n)\
        {\
            using _index_table = variant_alternative_t<n,_variants_index_table>;\
            return _variant_do_visit(_tag,_index_table{},std::forward<Visitor>(visitor),std::forward<Variants>(variants)...);\
        }\
        else\
            UNREACHABLE\
    }

namespace scaler
{
    template <bool, class... >
    struct _variant_storage_base {};

    template <class... Types>
    using _variant_storage = _variant_storage_base<(std::is_trivially_destructible_v<Types>&&...), Types...>;

    template <class First, class... Rest>
    struct _variant_storage_base<true, First, Rest...>
    {
        union
        {
            First _head;
            _variant_storage<Rest...> _tail;
        };

        constexpr _variant_storage_base() noexcept {}

        template <class... Types>
        constexpr explicit _variant_storage_base(std::in_place_index_t< 0>, Types&&... _Args)
            noexcept(std::is_nothrow_constructible_v<First, Types...>)
            : _head(static_cast<Types&&>(_Args)...)
        {}

        template <size_t Idx, class... _Types, std::enable_if_t<(Idx > 0), int> = 0>
        constexpr explicit _variant_storage_base(std::in_place_index_t<Idx>, _Types&&... _Args)
            noexcept(std::is_nothrow_constructible_v<_variant_storage<Rest...>, std::in_place_index_t<Idx - 1>, _Types...>)
            : _tail(std::in_place_index_t<Idx - 1>{}, static_cast<_Types&&>(_Args)...)
        {}

        constexpr First& _get() & noexcept {return _head;}
        constexpr const First& _get() const& noexcept {return _head;}
        constexpr First&& _get() && noexcept {return std::move(_head);}
        constexpr const First&& _get() const&& noexcept {return std::move(_head);}
    };

    template <class First, class... Rest>
    struct  _variant_storage_base<false, First, Rest...>
    {
        union
        {
            First _head;
            _variant_storage<Rest...> _tail;
        };

        constexpr _variant_storage_base() noexcept {} // no initialization (no active member)

        template <class... Types>
        constexpr explicit _variant_storage_base(std::in_place_index_t< 0>, Types&&... _Args)
            noexcept(std::is_nothrow_constructible_v<First, Types...>)
            : _head(static_cast<Types&&>(_Args)...)
        {}

        template <size_t Idx, class... Types, std::enable_if_t<(Idx > 0), int> = 0>
        constexpr explicit _variant_storage_base(std::in_place_index_t<Idx>, Types&&... _Args)
            noexcept(std::is_nothrow_constructible_v<_variant_storage<Rest...>, std::in_place_index_t<Idx - 1>, Types...>)
            : _tail(std::in_place_index_t<Idx - 1>{}, static_cast<Types&&>(_Args)...)
        {}

        constexpr First& _get() & noexcept {return _head;}
        constexpr const First& _get() const& noexcept {return _head;}
        constexpr First&& _get() && noexcept {return std::move(_head);}
        constexpr const First&& _get() const&& noexcept {return std::move(_head);}

        _variant_storage_base(_variant_storage_base&&)                 = default;
        _variant_storage_base(const _variant_storage_base&)            = default;
        _variant_storage_base& operator=(_variant_storage_base&&)      = default;
        _variant_storage_base& operator=(const _variant_storage_base&) = default;
        ~_variant_storage_base(){}
    };

    template <size_t _index, class Storage>
    [[nodiscard]]constexpr decltype(auto) _variant_raw_get(Storage&& s) noexcept
    {
        if constexpr (_index == 0)
            return std::forward<Storage>(s)._get();
        else if constexpr (_index == 1)
            return std::forward<Storage>(s)._tail._get();
        else if constexpr (_index == 2)
            return std::forward<Storage>(s)._tail._tail._get();
        else if constexpr (_index == 3)
            return std::forward<Storage>(s)._tail._tail._tail._get();
        else if constexpr (_index == 4)
            return std::forward<Storage>(s)._tail._tail._tail._tail._get();
        else if constexpr (_index == 5)
            return std::forward<Storage>(s)._tail._tail._tail._tail._tail._get();
        else if constexpr (_index == 6)
            return std::forward<Storage>(s)._tail._tail._tail._tail._tail._tail._get();
        else if constexpr (_index == 7)
            return std::forward<Storage>(s)._tail._tail._tail._tail._tail._tail._tail._get();
        else if constexpr (_index < 16)
            return _variant_raw_get<_index - 8>(std::forward<Storage>(s)._tail._tail._tail._tail._tail._tail._tail._tail);
        else if constexpr (_index < 32)
            return _variant_raw_get<_index - 16>(std::forward<Storage>(s)
                ._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail);
        else if constexpr (_index< 64)
            return _variant_raw_get<_index - 32>(std::forward<Storage>(s)
                ._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail
                ._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail);
        else
            return _variant_raw_get<_index - 64>(std::forward<Storage>(s)
                ._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail
                ._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail
                ._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail
                ._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail._tail);
    }
}

namespace scaler
{
    template<class T>
    struct type_identity
    {using type =T;};

    template<class...>class meta_list{};

    template<class ,class = meta_list<>>
    struct _variant_unique
        :std::true_type
    {};

    template<class F,class...R,class...A>
    struct _variant_unique<meta_list<F,R...>,meta_list<A...>>
        :std::conditional_t<(std::is_same_v<F,A>||...||false),
        std::false_type ,
        _variant_unique<meta_list<R...>,meta_list<A...,F>>>
    {};

    template<class>
    inline constexpr size_t variant_size_v = 0;

    template<template <class...> class Storage,class...R>
    inline constexpr size_t variant_size_v<Storage<R...>> = sizeof...(R);

    template<class,class...>struct _variant_radix_table{};

    template<size_t..._index,class B>
    struct _variant_radix_table<std::index_sequence<_index...>,B>
    {using type = std::index_sequence<_index...,1>;};

    template<size_t..._index,class  A,class ... B>
    struct _variant_radix_table<std::index_sequence<_index...>,A,B...>
        :_variant_radix_table<std::index_sequence<_index...,(variant_size_v<B>*...)>,B...>
    {};

    template<class...Variants>
    using _variant_radix_table_t  = typename  _variant_radix_table<std::index_sequence<>,Variants...>::type;

#define TEMPLATE template<class...>class T
#define P(n) class...n
#define P1(n)n...
#define P2(N)T<N...>

    template<class...>struct merge;
    template<size_t ,class,class = void>struct variant_alternative;

    template<class...Template>
    using merge_t  =  typename merge<Template...>::type;

    template<size_t _index,class Variant>
    using variant_alternative_t = typename variant_alternative<_index,Variant>::type;

    template<TEMPLATE,P(A)>
    struct merge<P2(A)>
            :type_identity<T<A...>>
    {};

    template<TEMPLATE,P(A),P(B)>
    struct merge<P2(A),P2(B)>
            :type_identity<T<P1(A),P1(B)>>
    {};

    template<TEMPLATE,P(A),P(B),P(C)>
    struct merge<P2(A),P2(B),P2(C)>
            :type_identity<T<P1(A),P1(B),P1(C)>>
    {};

    template<TEMPLATE,P(A),P(B),P(C),P(D)>
    struct merge<P2(A),P2(B),P2(C),P2(D)>
            :type_identity<T<P1(A),P1(B),P1(C),P1(D)>>
    {};

    template<TEMPLATE,P(A),P(B),P(C),P(D),P(E)>
    struct merge<P2(A),P2(B),P2(C),P2(D),P2(E)>
            :type_identity<T<P1(A),P1(B),P1(C),P1(D),P1(E)>>
    {};

    template<TEMPLATE,P(A),P(B),P(C),P(D),P(E),P(F)>
    struct merge<P2(A),P2(B),P2(C),P2(D),P2(E),P2(F)>
            :type_identity<T<P1(A),P1(B),P1(C),P1(D),P1(E),P1(F)>>
    {};

    template<TEMPLATE,P(A),P(B),P(C),P(D),P(E),P(F),P(G)>
    struct merge<P2(A),P2(B),P2(C),P2(D),P2(E),P2(F),P2(G)>
            :type_identity<T<P1(A),P1(B),P1(C),P1(D),P1(E),P1(F),P1(G)>>
    {};

    template<TEMPLATE,P(A),P(B),P(C),P(D),P(E),P(F),P(G),P(H)>
    struct merge<P2(A),P2(B),P2(C),P2(D),P2(E),P2(F),P2(G),P2(H)>
            :type_identity<T<P1(A),P1(B),P1(C),P1(D),P1(E),P1(F),P1(G),P1(H)>>
    {};

    template<TEMPLATE,P(A),P(B),P(C),P(D),P(E),P(F),P(G),P(H),P(R)>
    struct merge<P2(A),P2(B),P2(C),P2(D),P2(E),P2(F),P2(G),P2(H),R...>
            :merge<T<P1(A),P1(B),P1(C),P1(D),P1(E),P1(F),P1(G),P1(H)>,merge_t<R...>>
    {};

    template<TEMPLATE,class A,class ... B>
    struct variant_alternative<0,T<A,B...>>
        :type_identity<A>
    {};

    template<TEMPLATE,class A,class C,class ... B>
    struct variant_alternative<1,T<A,C,B...>>
        :type_identity<C>
    {};

    template<TEMPLATE,class A,class C,class D,class ... B>
    struct variant_alternative<2,T<A,C,D,B...>>
        :type_identity<D>
    {};

    template<TEMPLATE,class A,class C,class D,class E,class ... B>
    struct variant_alternative<3,T<A,C,D,E,B...>>
        :type_identity<E>
    {};

    template<TEMPLATE,class A,class C,class D,class E,class F,class ... B>
    struct variant_alternative<4,T<A,C,D,E,F,B...>>
        :type_identity<F>
    {};

    template<TEMPLATE,class A,class C,class D,class E,class F,class G,class ... B>
    struct variant_alternative<5,T<A,C,D,E,F,G,B...>>
            :type_identity<G>
    {};

    template<TEMPLATE,class A,class C,class D,class E,class F,class G,class H,class ... B>
    struct variant_alternative<6,T<A,C,D,E,F,G,H,B...>>
            :type_identity<H>
    {};

    template<TEMPLATE,class A,class C,class D,class E,class F,class G,class H,class I,class ... B>
    struct variant_alternative<7,T<A,C,D,E,F,G,H,I,B...>>
        :type_identity<I>
    {};

    template<TEMPLATE,size_t _index,class A,class C,class D,class E,class F,class G,class H,class I,class ... B>
    struct variant_alternative<_index,T<A,C,D,E,F,G,H,I,B...>,std::enable_if_t<(_index>=8)>>
        :variant_alternative<_index-8,T<B...>>
    {};

    template<size_t _index,class T>
    struct variant_alternative<_index,const T>
        :type_identity<const variant_alternative_t<_index,T>>
    {};

    template<size_t _index,class T>
    struct variant_alternative<_index,const volatile T>
    :type_identity<const volatile variant_alternative_t<_index,T>>
    {};

    template<size_t _index,class T>
    struct variant_alternative<_index,volatile T>
        :type_identity<volatile variant_alternative_t<_index,T>>
    {};
#undef TEMPLATE
#undef P
#undef P1
#undef P2

    template<class OutPut,size_t..._size>
    struct _make_index_matrix
        :type_identity<OutPut>
    {};

    template<class,size_t _size,class = void>
    struct _make_index_matrix_impl
    {
        using _new_sequence = std::make_index_sequence<_size>;

        template<class>
        struct _one{};

        template<size_t..._new>
        struct _one<std::index_sequence<_new...>>
        {using type = meta_list<std::index_sequence<_new>...>;};

        using type = typename _one<_new_sequence>::type;
    };

    template<class...IndexSequence,size_t _h>
    struct _make_index_matrix_impl<meta_list<IndexSequence...>,_h,std::enable_if_t<(sizeof...(IndexSequence)!=0)>>
    {
        using _new_sequence = std::make_index_sequence<_h>;

        template<class,class>
        struct _one{};

        template<size_t..._old,size_t..._new>
        struct _one<std::index_sequence<_old...>,std::index_sequence<_new...>>
        {using type = meta_list<std::index_sequence<_old...,_new>...>;};

        using type = merge_t<typename _one<IndexSequence,_new_sequence>::type...>;
    };

    template<class T,size_t _h,size_t..._r>
    struct _make_index_matrix<T,_h,_r...>
        :_make_index_matrix<typename _make_index_matrix_impl<T,_h>::type,_r...>
    {};

    ///Fuck Gcc
    template<size_t..._size>
    using _make_index_matrix_t = typename _make_index_matrix<meta_list<>,_size...>::type;

}

namespace scaler
{
    template<class R>
    using _func = void(*)();

    class _variant_idx_cookie{};
    class _variant_cookie{};

    template<size_t _c>
    inline constexpr auto _size_constant = std::integral_constant<size_t,_c>{};

    template<size_t..._index>
    inline constexpr auto _size_sequence = std::index_sequence<_index...>{};

    template<class Tag,class Visitor,class...Variants,std::size_t..._indices>
    constexpr decltype(auto) _variant_do_visit(std::in_place_type_t<Tag>,std::index_sequence<_indices...>,Visitor&&visitor,Variants&&...variants)
    {
        if constexpr (std::is_same_v<Tag,_variant_idx_cookie>)
            return std::invoke(visitor, _variant_raw_get<_indices>(std::forward<Variants>(variants))...,_size_constant<_indices>...);
        else
            return std::invoke(_variant_raw_get<_indices>(std::forward<Variants>(variants))...);
    }

    template<class...Size,size_t ... _indices>
    constexpr size_t _variant_count_index(std::index_sequence<_indices...>,const Size& ... _index)
    {return ((_indices*_index)+...+0);}

    template <class Callable, class... Variants>
    using _variant_visit_result_t = std::invoke_result_t<Callable,decltype(_variant_raw_get<0>(std::declval<Variants&&>()))...>;

    template<size_t  _begin,class Tag,class Visitor,class...Variants>
    constexpr decltype(auto) _variant_visit(std::in_place_type_t<Tag> _tag,
        std::in_place_index_t<_begin> _index,Visitor&&visitor,Variants&&...variants)
    {
        using _variants_index_table = _make_index_matrix_t<variant_size_v<std::decay_t<Variants>>...>;

        const size_t _final_index
            = _variant_count_index(_variant_radix_table_t<std::decay_t<Variants>...>{},variants.index()...);

        constexpr size_t _cases = (variant_size_v<Variants>*...);

        if constexpr(_cases<=512)
            switch (_final_index)
            {VARIANT_STAMP512(0,VISIT_CASE)}
        else
        {
            switch (_final_index)
            {VARIANT_STAMP128(512,VISIT_CASE)}
        }
    }
}

namespace scaler
{
    template<bool ,class...Types>
    struct _variant_default_ctor
    {};


    template<class...Types>
    class variant
    {
    public:

    };
}

#endif
