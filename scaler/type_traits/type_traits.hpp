
#ifndef SCALER_CPP_TYPE_TRAITS_HPP
#define SCALER_CPP_TYPE_TRAITS_HPP

#include <cmath>
#include <type_traits>
#include <utility>
#include <functional>

#if defined(_MSVC_LANG)
#define STL_LANG _MSVC_LANG
#define  UNREACHABLE __assume(false);
#else
#define STL_LANG __cplusplus
#define  UNREACHABLE __builtin_unreachable();
#endif

#define CXX17_CONCEPT inline constexpr bool
#define NAMESPACE_NAME scaler
#define NAMESPACE_RANGES NAMESPACE_NAME::ranges
#define NAMESPACE_DETAILS NAMESPACE_NAME::_details
#define NAMESPACE_VIEW NAMESPACE_RANGES::views
#define decay_copy(...) std::decay_t<decltype(__VA_ARGS__)>(__VA_ARGS__)
#define REQUIRE(...)  class = _require_t<__VA_ARGS__>

#define SMF_IS(_x1,a,b,c,d,e)\
        constexpr _x1()=a;\
        constexpr _x1(const _x1&)=b;\
        constexpr _x1(_x1&&)=c;\
        constexpr _x1&operator=(const _x1&)=d;\
        constexpr _x1&operator=(_x1&&)=e;

#define SMF_IS_1(_x1,b,c,d,e)\
        constexpr _x1(const _x1&)=b;\
        constexpr _x1(_x1&&)=c;\
        constexpr _x1&operator=(const _x1&)=d;\
        constexpr _x1&operator=(_x1&&)=e;

#define SMF_IS_2(_x1,a,b,c,d) \
        constexpr _x1()=a;\
        constexpr _x1(_x1&&)=b;\
        constexpr _x1&operator=(const _x1&)=c;\
        constexpr _x1&operator=(_x1&&)=d;

#define SMF_IS_3(_x1,a,b,c,d) \
        constexpr _x1()=a;    \
        constexpr _x1(const _x1&)=b; \
        constexpr _x1&operator=(const _x1&)=c;\
        constexpr _x1&operator=(_x1&&)=d;
#define SMF_IS_4(_x1,a,b,c,d) \
        constexpr _x1()=a;    \
        constexpr _x1(const _x1&)=b; \
        constexpr _x1(_x1&&)=c;\
        constexpr _x1&operator=(_x1&&)=d;

#define SMF_IS_5(_x1,a,b,c,d) \
        constexpr _x1()=a;    \
        constexpr _x1(const _x1&)=b; \
        constexpr _x1(_x1&&)=c;\
        constexpr _x1&operator=(const _x1&)=d;

namespace NAMESPACE_NAME
{
    class undefined{};

    template<class...>class meta_list{};

    template<bool...v>
    using bool_sequence = std::integer_sequence<bool,v...>;

    template<class T>
    struct type_identity
    {using type=T;};

    template<class T>
    using type_identity_t = typename type_identity<T>::type;;

    template<class T>
    using remove_cvref = std::remove_cv<std::remove_reference_t<T>>;

    template<class Type>
    using remove_cvref_t = typename remove_cvref<Type>::type;

    template<class T>
    using cref_t = const std::remove_reference_t<T>&;

    template<bool..._v>
    using _require_t = std::enable_if_t<(_v&&...&&true)>;

    template<bool..._v>
    using _require_or_t = std::enable_if_t<(_v||...||false)>;

    template<class From,class To>
    using copy_const_t = std::conditional_t<std::is_const<From>::value, std::add_const_t<To>, To>;

    template<class From,class To>
    using copy_volatile_t = std::conditional_t<std::is_volatile<From>::value, std::add_volatile_t<To>, To>;

    template<class From,class To>
    using copy_cv_t = copy_const_t<From, copy_volatile_t<From, To>>;

    template <class T>
    CXX17_CONCEPT _class_or_enum_type
            = std::is_class_v<remove_cvref_t<T>>
    || std::is_enum_v<remove_cvref_t<T>>
    || std::is_union_v<remove_cvref_t<T>>;

    template<class ,class ,template<class> class , template<class> class>
    struct basic_common_reference
    { };

    template<class...>
    struct common_reference
    {};

    template<class T>
    struct common_reference<T>
    {using type = T;};

    template<class X, class Y>
    using _cond_res = decltype(false ? std::declval<X(&)()>()() : std::declval<Y(&)()>()());

    template<class , class ,class = void>
    struct _cond_res_cvref
            :type_identity<undefined>
    {};
    template<class X, class Y>
    struct _cond_res_cvref<X,Y,std::void_t<_cond_res<copy_cv_t<X, Y>&, copy_cv_t<Y, X>&>>>
            :type_identity<_cond_res<copy_cv_t<X, Y>&, copy_cv_t<Y, X>&>>
    {};

    template<class X, class Y>
    using  _cond_res_cvref_t  = typename _cond_res_cvref<X,Y>::type ;

    /**
 * @brief 计算A与B的简单公共类型
 * @param A 引用A
 * @param B 引用B
 * @todo 采用c++23标准，可能与低版本发生冲突
 */
    template<class T,class U>
    struct _simple_common_reference
    {
    private:
        using X= std::remove_reference_t<T>;
        using Y = std::remove_reference_t<U>;
    private:
        template<class L,class R,bool =std::is_lvalue_reference_v<L>,bool =std::is_lvalue_reference_v<R>>
        struct _impl
                :type_identity<undefined>
        {};

        template<class L,class R>
        struct _impl<L,R,true,true>
        {
        private:
            using _type=_cond_res_cvref_t<X,Y>;
        public:
            using type=std::conditional_t<std::is_reference_v<_type>&&!std::is_same_v<remove_cvref_t<_type>,undefined>,_type,undefined>;
        };

        template<class L,class R>
        struct _impl<L,R,false,false>
        {
        private:
            using C=std::remove_reference_t<typename _impl<X&,Y&>::type>;
        public:
            using type=std::conditional_t<std::is_convertible_v<L,C>&&std::is_convertible_v<R,C>,C,undefined>;
        };

        template<class L,class R>
        struct  _impl<L,R, false, true>
        {
        private:
            using D = typename _impl<const X&, Y&>::type;
        public:
            using type=std::conditional_t<std::is_convertible_v<L,D>,D,undefined>;
        };

        template<class L,class R>
        struct  _impl<L,R, true,false>
                :_impl<R,L,false,true>
        {};

    public:
        using type=typename _impl<T,U>::type;
    };

    template<class T,class U>
    struct common_reference<T,U>
    {
        template<class T1,int v=std::is_lvalue_reference_v<T1>?1:(std::is_rvalue_reference_v<T1>?2:3)>
        struct _xref
        {
            template<class U1>
            using type=std::conditional_t<v == 3,copy_cv_t<T1,U1>,
                    std::conditional_t<v == 1,copy_cv_t<T1,U1>&,copy_cv_t<T1,U1>&&>>;
        };

        template<class T1, class T2>
        using _basic_common_ref
                = typename basic_common_reference<remove_cvref_t<T1>,
                remove_cvref_t<T2>,
                _xref<T1>::template type,
                _xref<T2>::template type>::type;
    private:
        template<class T1,class T2,int bullet,class = void>
        struct _impl
                :_impl<T1,T2,bullet+1>
        {};

        template<class T1,class T2>
        struct _impl<T1,T2,1,std::enable_if_t<std::is_reference_v<T2>
                &&std::is_reference_v<T1>
                &&std::is_convertible_v<std::add_pointer_t<T1>, std::add_pointer_t<typename _simple_common_reference<T1,T2>::type>>
                && std::is_convertible_v<std::add_pointer_t<T2>, std::add_pointer_t<typename _simple_common_reference<T1,T2>::type>>
                >>
        {
            using type = typename _simple_common_reference<T1,T2>::type;
        };

        template<class T1,class T2>
        struct _impl<T1,T2, 2,std::void_t<_basic_common_ref<T1, T2>>>
        { using type = _basic_common_ref<T1, T2>; };

        template<class T1,class T2>
        struct _impl<T1,T2, 3,std::void_t<_cond_res<T1, T2>>>
        { using type = _cond_res<T1, T2>; };

        template<class T1,class T2>
        struct _impl<T1,T2, 4,std::void_t<std::common_type_t<T1, T2>>>
        { using type = std::common_type_t<T1, T2>; };

        template<class T1,class T2>
        struct _impl<T1,T2, 5, void>
        { };
    public:
        using type=typename _impl<T,U,1>::type;
    };

    template<class A,class B,class C,class...Rest>
    struct common_reference<A,B,C,Rest...>
            :common_reference<typename common_reference<A,B>::type,C,Rest...>
    {};

    template<class...Types>
    using common_reference_t=typename common_reference<Types...>::type;

    template<class From, class To,class  = void>
    struct is_nothrow_convertible
            : std::conjunction<std::is_void<From>, std::is_void<To>>
    {};

    template<class From, class To>
    struct is_nothrow_convertible<From, To,std::void_t<decltype(static_cast<To(*)()>(nullptr)),
            std::enable_if_t<noexcept(std::declval<void(&)(To) noexcept>()(std::declval<From>()))>>>
            : std::true_type
    {};

    template<class T>
    struct is_bounded_array : std::false_type {};

    template<class T, std::size_t N>
    struct is_bounded_array<T[N]> : std::true_type {};

    template<class From, class To>
    inline constexpr bool is_nothrow_convertible_v = is_nothrow_convertible<From,To>::value;

    template<class T>
    inline constexpr bool is_bounded_array_v = is_bounded_array<T>::value;

    template<class T>
    using CT=std::conditional_t<std::is_lvalue_reference_v<T>,const std::remove_reference_t<T>&,const T>;

    template< bool Const, class T >
    using _maybe_const_t = std::conditional_t<Const, const T, T>;

    template<class,class = void>
    inline constexpr bool _has_arrow_operator_v = false;

    template<class T>
    inline constexpr bool _has_arrow_operator_v<T,std::void_t<decltype(std::declval<T&>().operator->())>> = true;

    template<class>
    inline constexpr bool _is_reference_wrapper_v = false;

    template<class T>
    inline constexpr bool _is_reference_wrapper_v<std::reference_wrapper<T>> = false;

    template<class T,class=void>
    struct _can_reference
            :std::false_type
    {};

    template<class T>
    struct _can_reference<T,std::void_t<T,T&>>
            :std::true_type
    {};

    template<class T,class=void>
    struct _dereference
            :std::false_type
    {};

    template<class T>
    struct _dereference<T,std::enable_if_t<_can_reference<decltype(*std::declval<T&>())>::value>>
            :std::true_type
    {};

    template<bool,class...Parameters >
    struct _enable_default_ctor
    {
        SMF_IS(_enable_default_ctor,default,default,default,default,default)
    };

    template<class...Parameters >
    struct _enable_default_ctor<false,Parameters...>
    {
        SMF_IS(_enable_default_ctor,delete,default,default,default,default)
    };

    template<class,template<class>class>
    struct _lower_bound
        :type_identity<undefined>
    {};

    template<template<class...>class Template,class Head,class ... Rest,template<class>class Unary>
    struct _lower_bound<Template<Head,Rest...>,Unary>
        :std::conditional_t<Unary<Head>::value,
        type_identity<Head>,
        _lower_bound<Template<Rest...>,Unary>>
    {};

    template<class T,template<class...>class Unary>
    using _lower_bound_t = typename _lower_bound<T,Unary>::type;

    template<class,class,class U = undefined>
    struct _switch
        :type_identity<U>
    {};

    template<bool _head,class Head,class...Rest,bool ... _rest>
    struct _switch<std::integer_sequence<bool,_head,_rest...>,meta_list<Head,Rest...>>
        :std::conditional_t<_head,type_identity<Head>,
        _switch<bool_sequence<_rest...>,meta_list<Rest...>>>
    {};

    template<class BoolSequence,class MetaList,class Undefined>
    using _switch_t =typename _switch<BoolSequence,MetaList,Undefined>::type;

    template<class ,class Sorted,class>
    struct _simple_sort
        :type_identity<Sorted>
    {};

    template<class Head,class...Rest>
    struct _simple_sort<meta_list<Head,Rest...>,meta_list<>,undefined>
        :_simple_sort<meta_list<Rest...>,meta_list<Head>,Head>
    {};

    template<class,class,class = meta_list<>>
    struct _simple_insert{};

    template<class H,class ... R,class Aim,class ... S>
    struct _simple_insert<meta_list<H,R...>,Aim,meta_list<S...>>
        :std::conditional_t<(sizeof(Aim)<=sizeof(H)),
            type_identity<meta_list<S...,Aim,H,R...>>,
    _simple_insert<meta_list<R...>,Aim,meta_list<S...,H>>>
    {};

    template<class Head,class...Rest,class...S,class Tail>
    struct _simple_sort<meta_list<Head,Rest...>,meta_list<S...>,Tail>
        :std::conditional_t<(sizeof(Tail)<=sizeof(Head)),
    _simple_sort<meta_list<Rest...>,meta_list<S...,Head>,Head>,
    _simple_sort<meta_list<Rest...>,typename _simple_insert<meta_list<S...>,Head>::type,Head>>
    {};

    template<class T>
    using _simple_sort_t = typename _simple_sort<T,meta_list<>,undefined>::type;

#if __SIZEOF_INT128__
    using  _max_size_type = __uint128_t;
    using _max_diff_type = __int128_t;
#else
    using _max_size_type = unsigned lon long;
    using _max_diff_type =  long long int;

#endif

    template <class T, class U,
    class _Tmp = _maybe_const_t<std::is_const_v<std::remove_reference_t<T>>, std::remove_reference_t<U>>>
    using _forward_like_t = std::conditional_t<std::is_rvalue_reference_v<T&&>, _Tmp&&, _Tmp&>;

    template <class T, class U>
    [[nodiscard]] constexpr _forward_like_t<T, U> forward_like(U&& x) noexcept
    {return static_cast<_forward_like_t<T, U>>(x);}

    template<template<class...>class,class>
    inline constexpr auto  _is_specialization_of =  false;

    template<template<class...>class Template,class...Types>
    inline constexpr bool _is_specialization_of<Template,Template<Types...>> =true;

    template<template<class...>class Template,class T>
    inline constexpr auto  _is_specialization_of_v =  _is_specialization_of<Template,remove_cvref<T>>;

    template<bool C, typename T>
    using _maybe_present_t = std::conditional_t<C, T, undefined>;

}//namespace scaler
#endif //SCALER_CPP_TYPE_TRAITS_HPP
