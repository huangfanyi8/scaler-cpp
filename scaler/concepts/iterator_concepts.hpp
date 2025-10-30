#ifndef SCALER_CPP_ITERATOR_CONCEPTS_HPP
#define SCALER_CPP_ITERATOR_CONCEPTS_HPP

#include "concepts.hpp"
#include "../memory/pointer_traits.hpp"
#include <iterator>

namespace NAMESPACE_NAME
{
    using std::input_iterator_tag;
    using std::forward_iterator_tag;
    using std::output_iterator_tag;
    using std::bidirectional_iterator_tag;
    using std::random_access_iterator_tag;

    struct default_sentinel_t { };

    inline constexpr default_sentinel_t default_sentinel{};

    template<class T,class = std::enable_if_t<_dereference<T>::value>>
    using iter_reference_t = decltype(*std::declval<T&>());

    template<class,class = void>struct iterator_traits;
    namespace _details
    {
        template<class,class = void>
        struct _iterator_traits
        {};
    }

    template<class Iterator>
    struct iterator_traits<Iterator,std::enable_if_t<!std::is_pointer_v<Iterator>>>
            :public _details::_iterator_traits<Iterator>
    { };

    template<class T>
    struct iterator_traits<T*,std::enable_if_t<std::is_object_v<T>>>
    {
        /**TODO 此处不是contiguous_iterator，为了兼容C++17*/
        using iterator_concept  = random_access_iterator_tag;
        using iterator_category = random_access_iterator_tag;
        using value_type	      = std::remove_cv_t<T>;
        using difference_type   = ptrdiff_t;
        using pointer	      = T*;
        using reference	      = T&;
    };

    //检查iterator_traits 是否被特化
    template<class Iter>
    inline constexpr bool  _is_specialization_of_iterator_traits_v
            =!std::is_base_of_v<_details::_iterator_traits<Iter>,iterator_traits<Iter>>;

    namespace _details
    {
        //计算类型 I 所关联的差类型
        template <int tag,class T,class U= void>
        struct _incrementable_traits
                :_incrementable_traits<tag+1,T>
        {};

        template <class T>
        struct _incrementable_traits<1,T*,std::enable_if_t<std::is_object_v<T>>>
                : type_identity<std::ptrdiff_t >
        {};

        template <int tag,class T>
        struct _incrementable_traits<tag,const T>
                :_incrementable_traits<tag,T>
        {};

        template <class T>
        struct _incrementable_traits<2,T,std::void_t<typename T::difference_type>>
                :type_identity<typename T::difference_type>
        {};

        template <typename T>
        struct _incrementable_traits<3,T,
                std::enable_if_t<integral<decltype(std::declval<const T&>()-std::declval<const T&>())>>>
                :std::make_signed<decltype(std::declval<T>() - std::declval<T>())>
        {};
    }

    template <class T,class=void>
    struct incrementable_traits
    {};

    template <class T>
    struct incrementable_traits<T,std::void_t<typename _details::_incrementable_traits<1,T>::type>>
    {using difference_type=typename _details::_incrementable_traits<1,T>::type;};

    namespace _details
    {
        template<class Iter,int _v,class = void>
        struct _iter_difference
                :_iter_difference<Iter,_v+1>
        {};

        template<class Iter>
        struct _iter_difference<Iter,0,
                std::void_t<typename iterator_traits<remove_cvref_t<Iter>>::difference_type,
                std::enable_if_t<_is_specialization_of_iterator_traits_v<remove_cvref_t<Iter>>>>>
        {
            using type = typename iterator_traits<remove_cvref_t<Iter>>::difference_type;
        };

        template<class Iter>
        struct _iter_difference<Iter,1,std::void_t<typename incrementable_traits<remove_cvref_t<Iter>>::difference_type>>
            :type_identity<typename incrementable_traits<remove_cvref_t<Iter>>::difference_type>
        {};

        template<class T>
        struct _iter_difference<T,2>
        {};
    }

    template <class Iter>
    using iter_difference_t = typename _details::_iter_difference<remove_cvref_t<Iter>,0>::type;
}

namespace NAMESPACE_NAME
{
    namespace _details
    {
        template <int match,class T, class = void>
        struct _indirectly_readable_traits
                :_indirectly_readable_traits<match+1,T>
        {};

        template <int match,class T>
        struct _indirectly_readable_traits<match,const T>
                :_indirectly_readable_traits<match,T>
        {};

        template <class T>
        struct _indirectly_readable_traits<1,T,std::enable_if_t<std::is_array_v<T>>>
        {using value_type = std::remove_cv_t<std::remove_extent_t<T>>;};

        template <class T>
        struct _indirectly_readable_traits<2,T*,std::enable_if_t<std::is_object_v<T>>>
        { using value_type = std::remove_cv_t<T>;};

        template <class T>
        struct _indirectly_readable_traits<3,T,std::void_t<typename T::value_type>>
                :type_identity<typename T::value_type>
        {using value_type = typename T::value_type;};

        template <class T>
        struct _indirectly_readable_traits<4,T,std::void_t<typename T::element_type>>
        {using value_type = typename T::element_type;};

        template <class T>
        struct _indirectly_readable_traits<5,T,std::void_t<std::remove_cv_t<typename T::value_type>,std::remove_cv_t<typename T::element_type>,
                std::enable_if_t<same_as<std::remove_cv_t<typename T::value_type>,std::remove_cv_t<typename T::element_type>>>>>
        {using value_type= typename T::value_type;};

        template <class T>
        struct _indirectly_readable_traits<6,T>
        {};
    }//_details

    template <class T>
    struct indirectly_readable_traits
            :_details::_indirectly_readable_traits<1,T>
    {};
}//main namespace

namespace NAMESPACE_NAME
{
    template<class Iter,int tag,class = void>
    struct iter_value
            :iter_value<Iter,tag+1>
    {};

    template<class Iter>
    struct iter_value<Iter,0,std::void_t<typename indirectly_readable_traits<remove_cvref_t<Iter>>::value_type >>
        :type_identity<typename indirectly_readable_traits<remove_cvref_t<Iter>>::value_type >
    {};

    template<class Iter>
    struct iter_value<Iter,-1,std::void_t<typename iterator_traits<remove_cvref_t<Iter>>::value_type ,
            std::enable_if_t<_is_specialization_of_iterator_traits_v<remove_cvref_t<Iter>>>>>
        :type_identity<typename iterator_traits<remove_cvref_t<Iter>>::value_type >
    {};

    template<class Iter>
    struct iter_value<Iter,1>
    {};

    template <class Iter>
    using iter_value_t=typename iter_value<remove_cvref_t<Iter>,-1>::type;
}

namespace NAMESPACE_NAME
{
    namespace ranges
    {
        namespace Cpo
        {
            void iter_move()=delete;

            template<class T,class=void>
            struct _adl_move
                    :std::false_type
            {};

            template<class T>
            struct _adl_move<T,std::void_t<decltype(iter_move(std::declval<T>()))>>
                    :std::bool_constant<_class_or_enum_type<T>>
            {};

            struct _iter_move
            {
            private:
                template<class T,class=void>
                struct _result
                { using type = iter_reference_t<T>; };

                template<typename T>
                struct _result<T,std::enable_if_t<_adl_move<T>::value>>
                { using type = decltype(iter_move(std::declval<T>())); };

                template<typename T>
                struct _result<T,std::enable_if_t<!_adl_move<T>::value
                                                  && std::is_lvalue_reference_v<iter_reference_t<T>>>>
                { using type = std::remove_reference_t<iter_reference_t<T>>&&; };

                template<class T>
                static constexpr bool S_noexcept()
                {
                    if constexpr (_adl_move<T>::value)
                        return noexcept(iter_move(std::declval<T>()));
                    else
                        return noexcept(*std::declval<T>());
                }

            public:

                template<class T,class=std::enable_if_t<_dereference<T>::value>>
                using _type = typename _result<T>::type;

                template<class T,class=std::enable_if_t<_dereference<T>::value>>
                [[nodiscard]]
                constexpr decltype(auto)
                operator()(T&& e) const noexcept(S_noexcept<T>())
                {
                    if constexpr (_adl_move<T>::value)
                        return iter_move(static_cast<T&&>(e));
                    else if constexpr (std::is_lvalue_reference_v<decltype(*std::declval<T&>())>)
                        return static_cast<_type<T>>(*e);
                    else
                        return *e;
                }
            };
        }
        inline constexpr auto iter_move=Cpo::_iter_move{};
    }

    template< class T ,class=std::enable_if_t<_dereference<T>::value>>
    using iter_rvalue_reference_t =decltype(ranges::iter_move(std::declval<T&>()));
}

namespace NAMESPACE_DETAILS
{
    template<class Iter,class =void>
    struct  _cpp17_iterator
            :std::false_type
    {};

    template<class Iter>
    struct _cpp17_iterator<Iter,std::enable_if_t<
                                scaler::_can_reference<decltype(*std::declval<Iter&>())>::value
                                &&same_as<decltype(++std::declval<Iter&>()),Iter&>
                                &&_can_reference<decltype(*std::declval<Iter&>()++)>::value>>
        :std::true_type
    {};

    template<class Iter>
    CXX17_CONCEPT _is_cpp17_iterator = _cpp17_iterator<Iter>::value&&copyable<Iter>;

    template<class Iter,class =void>
    struct _cpp17_input_iterator
            :std::false_type
    {};

    template<class Iter>
    struct _cpp17_input_iterator<Iter,
            std::void_t<typename incrementable_traits<Iter>::difference_type,
                    typename indirectly_readable_traits<Iter>::value_type,
                    common_reference_t<iter_reference_t<Iter>&&,typename indirectly_readable_traits<Iter>::value_type&>,
    common_reference_t<decltype(*std::declval<Iter&>()++)&&,typename indirectly_readable_traits<Iter>::value_type&>>>
        :std::bool_constant<_is_cpp17_iterator<Iter>
                        && equality_comparable<Iter>
                        &&signed_integral<typename incrementable_traits<Iter>::difference_type>>
    {};

    template<class Iter>
    CXX17_CONCEPT _is_cpp17_input_iterator = _cpp17_input_iterator<Iter>::value;

    template<class Iter,class =void>
    struct _cpp_fwd_iterator
            :std::false_type
    {};

    template<class Iter>
    struct _cpp_fwd_iterator<Iter,std::enable_if_t<
            convertible_to<decltype(std::declval<Iter&>()++),const Iter&>
            &&same_as<decltype(*std::declval<Iter&>()++), iter_reference_t<Iter>>>>
        :std::true_type
    {};

    template<class Iter>
    CXX17_CONCEPT _is_cpp17_fwd_iterator
            = _is_cpp17_input_iterator<Iter>
              && constructible_from<Iter>
              && std::is_lvalue_reference_v<iter_reference_t<Iter>>
            &&same_as<remove_cvref_t<iter_reference_t<Iter>>,typename indirectly_readable_traits<Iter>::value_type>
            &&_cpp_fwd_iterator<Iter>::value;

    template<class Iter,class =void>
    struct _cpp_bidi_iterator
            :std::false_type
    {};

    template<class Iter>
    struct _cpp_bidi_iterator<Iter,std::enable_if_t<
            convertible_to<decltype(std::declval<Iter&>()--),const Iter&>
            &&same_as<decltype(--std::declval<Iter&>()),Iter&>
                                   &&same_as<decltype(*std::declval<Iter&>()--), iter_reference_t<Iter>>>>
        :std::true_type
    {};

    template<class Iter>
    CXX17_CONCEPT  _is_cpp17_bidi_iterator = _is_cpp17_fwd_iterator<Iter>&&_cpp_bidi_iterator<Iter>::value;

    template<class Iter,class = void>
    struct _cpp17_random_access_iterator
            :std::false_type
    {};

    template<class Iter>
    struct _cpp17_random_access_iterator<Iter,
            _require_t<
            same_as<Iter&, decltype(std::declval<Iter&>()+=std::declval<typename incrementable_traits<Iter>::difference_type&>())>,
    same_as<Iter&, decltype(std::declval<Iter&>()-=std::declval<typename incrementable_traits<Iter>::difference_type&>())>,
    same_as<Iter, decltype(std::declval<Iter&>()+std::declval<typename incrementable_traits<Iter>::difference_type&>())>,
    same_as<Iter, decltype(std::declval<typename incrementable_traits<Iter>::difference_type&>()+std::declval<Iter&>())>,
    same_as<Iter, decltype(std::declval<Iter&>()-std::declval<typename incrementable_traits<Iter>::difference_type&>())>,
    same_as<typename incrementable_traits<Iter>::difference_type,decltype(std::declval<Iter&>()-std::declval<Iter&>())>,
    convertible_to<decltype(std::declval<Iter&>()[std::declval<typename incrementable_traits<Iter>::difference_type&>()]),iter_difference_t<Iter>>
    >>
        :std::true_type
    {};

    template<class Iter>
    CXX17_CONCEPT _is_cpp17_random_access_iterator
            = _is_cpp17_bidi_iterator<Iter>
              && totally_ordered<Iter>
              && _cpp17_random_access_iterator<Iter>::value;

    template<class,class=void>
    struct  _iter_with_nested_types_
            :std::false_type
    {};

    template<class Iter>
    struct  _iter_with_nested_types_<Iter,std::void_t<
            typename Iter::iterator_category,
            typename Iter::value_type,
            typename Iter::difference_type,
            typename Iter::reference>>
            :std::true_type
    {};

    //检测是否包含类型
    template<class Iter>
    CXX17_CONCEPT _iter_with_nested_types=_iter_with_nested_types_<Iter>::value;

    template<class Iterator>
    struct _iterator_traits<Iterator,std::enable_if_t<_iter_with_nested_types<Iterator>>>
    {
    private:
        template<class,class=void>
        struct _traits_ptr
        { using type = void; };

        template<class Iter>
        struct _traits_ptr<Iter,std::void_t<typename Iter::pointer>>
        { using type = typename Iter::pointer; };

    public:
        using iterator_category = typename Iterator::iterator_category;
        using value_type	      = typename Iterator::value_type;
        using difference_type   = typename Iterator::difference_type;
        using pointer	      = typename _traits_ptr<Iterator>::type;
        using reference	      = typename Iterator::reference;
    };

    template<class Iterator>
    struct _iterator_traits<Iterator,std::enable_if_t<!_iter_with_nested_types<Iterator>&&_is_cpp17_input_iterator<Iterator>>>
    {
    private:
        template<int tag,class Iter,class =void >
        struct _traits_category
                :_traits_category<tag+1,Iter>
        {};

        template<class Iter>
        struct _traits_category<1,Iter,std::void_t<typename Iter::iterator_category>>
        { using type = typename Iter::iterator_category; };

        template<class Iter>
        struct _traits_category<2,Iter,std::enable_if_t<_is_cpp17_random_access_iterator<Iter>>>
        { using type = random_access_iterator_tag; };

        template<class Iter>
        struct _traits_category<3,Iter,std::enable_if_t<_is_cpp17_bidi_iterator<Iter>>>
        { using type = bidirectional_iterator_tag; };

        template<class Iter>
        struct _traits_category<4,Iter,std::enable_if_t<_is_cpp17_fwd_iterator<Iter>>>
        { using type = forward_iterator_tag; };

        template<class Iter>
        struct _traits_category<5,Iter>
        {using type=input_iterator_tag;};

        template<int model,class Iter,class=void>
        struct _traits_ptr
                :_traits_ptr<model+1,Iter>
        {};

        template<class Iter>
        struct _traits_ptr<1,Iter,std::void_t<typename Iter::pointer>>
        { using type = typename Iter::pointer; };

        template<class Iter>
        struct _traits_ptr<2,Iter,std::void_t<decltype(std::declval<Iter&>().operator->())>>
        { using type = decltype(std::declval<Iter&>().operator->()); };

        template<class Iter>
        struct _traits_ptr<3,Iter>
        {
            using type=void;
        };

        template<class Iter,class =void >
        struct _traits_ref
        { using type = iter_reference_t<Iter>; };

        template<class Iter>
        struct _traits_ref<Iter,std::void_t<typename Iter::reference>>
        { using type = typename Iter::reference; };

    public:
        using iterator_category = typename _traits_category<1,Iterator>::type;
        using value_type
                = typename indirectly_readable_traits<Iterator>::value_type;
        using difference_type
                = typename incrementable_traits<Iterator>::difference_type;
        using pointer	      = typename _traits_ptr<1,Iterator>::type;
        using reference	      = typename _traits_ref<Iterator>::type;
    };

    template<class Iterator>
    struct _iterator_traits<Iterator,
            std::enable_if_t<!_iter_with_nested_types<Iterator>
                             &&_is_cpp17_iterator<Iterator>
                             &&!_is_cpp17_input_iterator<Iterator>>>
    {
    private:
        template<class Iter,class= void>
        struct _traits_diff
        { using type = void; };

        template<class Iter>
        struct _traits_diff<Iter,
                std::void_t<typename incrementable_traits<Iter>::difference_type>>
        {
            using type = typename incrementable_traits<Iter>::difference_type;
        };

    public:
        using iterator_category = output_iterator_tag;
        using value_type	      = void;
        using difference_type   = typename _traits_diff<Iterator>::type;
        using pointer	      = void;
        using reference	      = void;
    };
}//_details

namespace NAMESPACE_NAME
{
    namespace _details
    {
        template<class Iter>
        using _iter_traits = std::conditional_t<_is_specialization_of_iterator_traits_v<Iter>,iterator_traits<Iter>,Iter>;;

        template<int tag,class Iter,typename =void>
        struct _iter_concept_impl
                :_iter_concept_impl<tag+1,Iter>
        {};

        template<typename Iter>
        struct _iter_concept_impl<1,Iter,std::void_t< typename _iter_traits<Iter>::iterator_concept>>
        { using type = typename _iter_traits<Iter>::iterator_concept; };

        template<class Iter>
        struct _iter_concept_impl<2,Iter,std::void_t< typename _iter_traits<Iter>::iterator_category>>
        { using type = typename _iter_traits<Iter>::iterator_category; };

        template<class Iter>
        struct _iter_concept_impl<3,Iter,std::enable_if_t<!_is_specialization_of_iterator_traits_v<Iter>>>
        { using type = random_access_iterator_tag; };

        template<typename Iter>
        struct _iter_concept_impl<4,Iter>
        { };
    }//_details
    // ITER_CONCEPT
    template<class Iter>
    using _iter_concept = typename _details::_iter_concept_impl<1,Iter>::type;
}

namespace NAMESPACE_NAME
{
    namespace _details
    {
        template< class In ,class = void >
        struct _indirectly_readable
                :std::false_type
        {};

        template<class In>
        struct _indirectly_readable<In,
                std::void_t<iter_value_t<In>,
                        iter_reference_t<In>,
                iter_rvalue_reference_t<In>,
                _require_t<same_as<iter_reference_t<const In>,iter_reference_t<In>>,
        same_as<iter_rvalue_reference_t<const In>,iter_rvalue_reference_t<In>>,
        common_reference_with<iter_reference_t<In>&&, iter_value_t<In>&>,
        common_reference_with<iter_reference_t<In>&&,iter_rvalue_reference_t<In>&&>,
        common_reference_with<iter_rvalue_reference_t<In>&&,const iter_value_t<In>&>
        >//require_t
        >//void_t
        >
            :std::true_type
        {};
}
    ///std::indirectly_readable
    template< class In >
    CXX17_CONCEPT indirectly_readable =_details::_indirectly_readable<remove_cvref_t<In>>::value;
}

namespace NAMESPACE_NAME
{
    template< class T ,class=std::enable_if_t<indirectly_readable<T>>>
    using iter_const_reference_t = common_reference_t<const iter_value_t<T>&&,iter_reference_t<T>>;

    template< class T ,
            class=std::enable_if_t<indirectly_readable<T>>>
    using iter_common_reference_t =
            common_reference_t<iter_reference_t<T>,iter_value_t<T>&>;
}

namespace NAMESPACE_NAME
{
    namespace _details
    {
        template< class Out, class T ,class =void>
        struct _indirectly_writable
                :std::false_type
        {};

        template< class Out, class T >
        struct _indirectly_writable<Out,T,
                std::void_t<decltype(*std::declval<Out&>()=std::declval<T&&>()),
                        decltype(*std::declval<Out&&>() = std::declval<T&&>()),
                        decltype(const_cast<const iter_reference_t<Out>&&>(*std::declval<Out&>()) = std::declval<T&&>(),
                                const_cast<const iter_reference_t<Out>&&>(*std::declval<Out&>()) = std::declval<T&&>())
                >//void_t
        >
                :std::true_type
        {};
    }

    template< class Out, class T >
    CXX17_CONCEPT indirectly_writable = _details::_indirectly_writable<Out,T>::value;
}

namespace NAMESPACE_NAME
{
    namespace _details
    {
        template<class T,class = void>
        struct _weakly_incrementable
                :std::false_type
        {};

        template<class T>
        struct _weakly_incrementable<T,
                std::void_t<iter_difference_t<T>,
                decltype(std::declval<T&>()++),
                _require_t<movable<T>,same_as<decltype(++std::declval<T&>()),T&>>
        >//void_t
        >
        :std::true_type
    {};

    template<class T,class = void>
    struct _incrementable
            :std::false_type
    {};

    template<class T>
    struct _incrementable<T,_require_t<_weakly_incrementable<T>::value,same_as<decltype(std::declval<T&>()++),T>,regular<T>>>
        :std::true_type
    {};
    }

    template <class T>
    CXX17_CONCEPT weakly_incrementable =_details::_weakly_incrementable<T>::value;

    template <class T>
    CXX17_CONCEPT incrementable = _details::_incrementable<T>::value;
}

namespace NAMESPACE_NAME
{
    template<class T,class = void>
    struct _input_or_output
            :std::false_type
    {};

    template<class T>
    struct _input_or_output<T, std::enable_if_t<_can_reference<decltype(*std::declval<T&>())>::value&&weakly_incrementable<T>>>
            :std::true_type
    {};

    template <class Iter>
    CXX17_CONCEPT input_or_output_iterator = _input_or_output<Iter>::value;

    template <class S,class I>
    CXX17_CONCEPT sentinel_for =
            semiregular<S>
            && input_or_output_iterator<I>
            &&_details::_weakly_equality_comparable_with<S, I>;

    template <class S, class I,class=void>
    inline constexpr bool disable_sized_sentinel_for = false;

    template <class S, class I, class = void>
    struct _sized_sentinel_for
            :std::false_type
    {};

    template <class S, class I>
    struct _sized_sentinel_for<S,I,
            _require_t<same_as<decltype(std::declval<const S&>() - std::declval<const I&>()), iter_difference_t<I>>>>
    :std::true_type{};

    template <class S, class I>
    CXX17_CONCEPT sized_sentinel_for =
            sentinel_for<S, I>
            &&!disable_sized_sentinel_for<std::remove_cv_t<S>, std::remove_cv_t<I>>
            &&_sized_sentinel_for< S, I>::value;

    namespace _details
    {
        template<class Iter,class = void>
        inline constexpr bool _is_input_iterator=false;

        template<class Iter>
        inline constexpr bool _is_input_iterator<Iter,
                std::void_t<_require_t<indirectly_readable<Iter>,
                        input_or_output_iterator<Iter>>,
                _iter_concept<Iter>,_require_t<derived_from<_iter_concept<Iter>,input_iterator_tag>>>> =true;

        template<class I,class T,class= void>
        struct _output_iterator
                :std::false_type
        {};

        template<class I,class T>
        struct _output_iterator<I,T,std::void_t<
                std::enable_if_t< input_or_output_iterator<I>
                && indirectly_writable<I,T>>,
        decltype(*std::declval<I&>() = std::declval<T&&>())>>
        :std::true_type
        {};

        template<class Iter , class = void>
        struct _forward_iterator
                :std::false_type
        {};

        template<class Iter>
        struct _forward_iterator<Iter,
                std::enable_if_t<_is_input_iterator<Iter>
                && derived_from<_iter_concept<Iter>, forward_iterator_tag>
                && incrementable<Iter>
        && sentinel_for<Iter,Iter>>>
        :std::true_type
        {};

        template<class I,class = void>
        struct _bidirectional_iterator
                :std::false_type
        {};

        template<class I>
        struct _bidirectional_iterator<I,std::enable_if_t<
                _forward_iterator<I>::value
                &&derived_from<_iter_concept<I>,bidirectional_iterator_tag>
                &&same_as<decltype(--std::declval<I&>()),I&>
                &&same_as<decltype(std::declval<I&>()--),I>>>
        :std::true_type
        {};

    template<class Iter>
    struct _random_access_iterator
    {
    private:
        template<class I>
        using _iter_diff_t = const iter_difference_t<I>&;

        template<class I,class = void>
        struct _impl
                :std::false_type
        {};

        template<class I>
        struct _impl<I,std::void_t<
                _require_t<same_as<decltype(std::declval<I&>() += std::declval<_iter_diff_t<I>>()), I&>>,
                _require_t<same_as<decltype(std::declval<I&>() -= std::declval<_iter_diff_t<I>>()), I&>>,
        _require_t<same_as<I, decltype(std::declval<const I&>()+std::declval<const iter_difference_t<I>&>())>>,
        _require_t<same_as<I, decltype(std::declval<const iter_difference_t<I>&>()+std::declval<const I&>())>>,
        _require_t<same_as<I, decltype(std::declval<const I&>()-std::declval<const iter_difference_t<I>&>())>>,
        _require_t<same_as<iter_reference_t<I>, decltype(std::declval<const I&>()[std::declval<const iter_difference_t<I>&>()])>>
        >//void_t
        >
            :std::true_type
        {};

        template<class I , class = void>
        struct _impl1
                :std::false_type
        {};

        template<class I>
        struct _impl1<I , std::enable_if_t<
                _bidirectional_iterator<I>::value
                && derived_from<_iter_concept<I>,random_access_iterator_tag>
                          && totally_ordered<I>
                && sized_sentinel_for<I,I>>>
        :std::true_type
        {};
    public:
        static constexpr bool value = _impl<Iter>::value&&_impl1<Iter>::value;
    };

    template<class Iter, class  = void>
    struct _contiguous_iterator
            :std::false_type
    {};

    template<class Iter>
    struct _contiguous_iterator<Iter,
            std::enable_if_t<
            same_as<decltype(to_address(std::declval<const Iter&>())),std::add_pointer_t<iter_reference_t<Iter>>>
            &&_random_access_iterator<Iter>::value
            &&(derived_from<_iter_concept<Iter>, random_access_iterator_tag>)
            &&std::is_lvalue_reference_v<iter_reference_t<Iter>>
            &&same_as<iter_value_t<Iter>, remove_cvref_t<iter_reference_t<Iter>>>
        >>
        :std::true_type
    {};
}

template<class Iter>
CXX17_CONCEPT input_iterator= _details::_is_input_iterator<Iter>;

template<class Iter,class T>
CXX17_CONCEPT output_iterator = _details::_output_iterator<Iter,T>::value;

template<class Iter>
CXX17_CONCEPT forward_iterator = _details::_forward_iterator<Iter>::value;

template<class Iter>
CXX17_CONCEPT bidirectional_iterator = _details::_bidirectional_iterator<Iter>::value;

template<class Iter>
CXX17_CONCEPT random_access_iterator = _details::_random_access_iterator<Iter>::value;

template<class Iter>
CXX17_CONCEPT contiguous_iterator = _details::_contiguous_iterator<Iter>::value;
}

namespace NAMESPACE_DETAILS
{
    template<class Fn,class Iter,class = void>
    struct  _indirectly_unary_invocable
            :std::false_type
    {};

    template<class Fn,class Iter>
    struct  _indirectly_unary_invocable<Fn,Iter,std::enable_if_t<indirectly_readable<Iter>
                                                                 && copy_constructible<Fn>
                                                && invocable<Fn&,iter_value_t<Iter>&>
            && invocable<Fn&,iter_reference_t<Iter>>
    && invocable<Fn&,iter_common_reference_t<Iter>>
            &&common_reference_with<std::invoke_result_t<Fn&,iter_value_t<Iter>&>,
            std::invoke_result_t<Fn&,iter_reference_t<Iter>>>>>
    :std::true_type
{};

template<class Fn,class Iter,class = void>
struct  _indirectly_regular_unary_invocable
        :std::false_type
{};

template<class Fn,class Iter>
struct  _indirectly_regular_unary_invocable<Fn,Iter,std::enable_if_t<indirectly_readable<Iter>
                                                                     &&copy_constructible<Fn>
                                                    &&regular_invocable<Fn&, iter_value_t<Iter>&>
        &&regular_invocable<Fn&, iter_reference_t<Iter>>
&&regular_invocable<Fn&, iter_common_reference_t<Iter>>
&&common_reference_with<std::invoke_result_t<Fn&, iter_value_t<Iter>&>,std::invoke_result_t<Fn&, iter_reference_t<Iter>>>>>
:std::true_type
{};

template<typename Fn, typename Iter,class = void>
struct _indirect_unary_predicate:std::false_type {};

template<typename Fn, typename Iter>
struct _indirect_unary_predicate<Fn,Iter,std::enable_if_t<indirectly_readable<Iter>
                                                          && copy_constructible<Fn> && predicate<Fn&, iter_value_t<Iter>&>
        && predicate<Fn&, iter_reference_t<Iter>>
&& predicate<Fn&, iter_common_reference_t<Iter>>>>
:std::true_type
{};

template<class Fn, class I1, class I2,class = void>
struct _indirect_binary_predicate:std::false_type {};

template<class Fn, class I1, class I2>
struct _indirect_binary_predicate<Fn,I1,I2,std::enable_if_t<indirectly_readable<I1> && indirectly_readable<I2>
                                                            && copy_constructible<Fn>
                                           && predicate<Fn&, iter_value_t<I1>&, iter_value_t<I2>&>
                                           && predicate<Fn&, iter_value_t<I1>&, iter_reference_t<I2>>
        && predicate<Fn&, iter_reference_t<I1>, iter_value_t<I2>&>
&& predicate<Fn&, iter_reference_t<I1>, iter_reference_t<I2>>
&& predicate<Fn&, iter_common_reference_t<I1>,
iter_common_reference_t<I2>>>>
:std::true_type
{};

template<class F,class I1,class I2 ,class = void>
struct  _indirect_equivalence_relation:std::false_type {};

template<class F,class I1,class I2 >
struct  _indirect_equivalence_relation<F,I1,I2,std::enable_if_t<
        indirectly_readable<I1>
        && indirectly_readable<I2>
        && copy_constructible<F>
                                               && equivalence_relation<F&, iter_value_t<I1>&, iter_value_t<I2>&>
        && equivalence_relation<F&, iter_value_t<I1>&, iter_reference_t<I2>>
&& equivalence_relation<F&, iter_reference_t<I1>, iter_value_t<I2>&>
&& equivalence_relation<F&, iter_reference_t<I1>,iter_reference_t<I2>>
&& equivalence_relation<F&, iter_common_reference_t<I1>,iter_common_reference_t<I2>>>>
:std::true_type
{};

template<class F,class I1,class I2 ,class = void>
struct  _indirect_strict_weak_order:std::false_type {};

template<class F,class I1,class I2 >
struct  _indirect_strict_weak_order<F,I1,I2,std::enable_if_t<
        indirectly_readable<I1>
        && indirectly_readable<I2>
        && copy_constructible<F>
                                            && strict_weak_order<F&, iter_value_t<I1>&, iter_value_t<I2>&>
        && strict_weak_order<F&, iter_value_t<I1>&, iter_reference_t<I2>>
&& strict_weak_order<F&, iter_reference_t<I1>, iter_value_t<I2>&>
&& strict_weak_order<F&, iter_reference_t<I1>, iter_reference_t<I2>>
&& strict_weak_order<F&, iter_common_reference_t<I1>,iter_common_reference_t<I2>>>>
:std::true_type
{};

template<class In, class Out,class =void>
struct  _indirectly_movable:std::false_type {};

template<class In, class Out>
struct  _indirectly_movable<In,Out,std::enable_if_t<indirectly_readable<In>
                                                    && indirectly_writable<Out, iter_rvalue_reference_t<In>>>>
:std::true_type
{};

template<class In, class Out,class =void>
struct  _indirectly_movable_storable:std::false_type {};

template<class In, class Out>
struct  _indirectly_movable_storable<In,Out,std::enable_if_t<_indirectly_movable<In,Out>::value
                                                             &&indirectly_writable<Out,iter_value_t<In>>
                                            &&movable<iter_value_t<In>>
&&constructible_from<iter_value_t<In>, iter_rvalue_reference_t<In>>
        &&assignable_from<iter_value_t<In>&,iter_rvalue_reference_t<In>>>>
:std::true_type
{};

template<class In, class Out,class =void>
struct  _indirectly_copyable:std::false_type {};

template<class In, class Out>
struct  _indirectly_copyable<In,Out,std::enable_if_t<indirectly_readable<In>
                                                     &&indirectly_writable<Out,iter_reference_t<In>>>>
:std::true_type
{};

template<class In, class Out,class =void>
struct  _indirectly_copyable_storable:std::false_type {};

template<class In, class Out>
struct  _indirectly_copyable_storable<In,Out,std::enable_if_t<_indirectly_copyable<In, Out>::value
                                                              &&indirectly_writable<Out, iter_value_t<In>&>
                                             &&indirectly_writable<Out, const iter_value_t<In>&>
                                             &&indirectly_writable<Out, iter_value_t<In>&&>
        &&indirectly_writable<Out, const iter_value_t<In>&&>
&&copyable<iter_value_t<In>>
        &&constructible_from<iter_value_t<In>, iter_reference_t<In>>
&&assignable_from<iter_value_t<In>&, iter_reference_t<In>>>>
:std::true_type
{};

}

namespace NAMESPACE_NAME
{
    template<class Fn,class Iter>
    CXX17_CONCEPT indirectly_unary_invocable = _details::_indirectly_unary_invocable<Fn,Iter>::value;

    template<typename Fn, typename Iter>
    CXX17_CONCEPT indirectly_regular_unary_invocable = _details::_indirectly_regular_unary_invocable<Fn,Iter>::value;

    template<typename Fn, typename Iter>
    CXX17_CONCEPT indirect_unary_predicate = _details::_indirect_unary_predicate<Fn,Iter>::value;

    template<class Fn, class I1, class I2>
    CXX17_CONCEPT indirect_binary_predicate = _details::_indirect_binary_predicate<Fn,I1,I2>::value;

    template<class F,class I1,class I2 = I1>
    CXX17_CONCEPT indirect_equivalence_relation = _details::_indirect_equivalence_relation<F,I1,I2>::value;

    template<class F,class I1,class I2 = I1>
    CXX17_CONCEPT indirect_strict_weak_order = _details::_indirect_strict_weak_order<F,I1,I2>::value;

    template<class F,class...Args>
    using indirect_result_t = std::invoke_result_t<F,iter_reference_t<Args>...>;

    template<class Iter, class Proj,bool = weakly_incrementable<Iter>>
    struct _projected
    {
        using value_type = remove_cvref_t<indirect_result_t<Proj&,Iter>>;
        indirect_result_t<Proj&, Iter> operator*() const; // not defined
    };

    template<class Iter,class Proj>
    struct _projected<Iter, Proj,true>
    {
        using value_type = remove_cvref_t<indirect_result_t<Proj&, Iter>>;
        using difference_type = iter_difference_t<Iter>;
        indirect_result_t<Proj&, Iter> operator*() const; // not defined
    };

    template<class Iter,class Proj,
            class=_require_t<indirectly_readable<Iter>,indirectly_regular_unary_invocable<Proj,Iter>>>
    struct  projected
            :_projected<Iter,Proj>
    {};

    template< class I,class Proj,
            class=_require_t<indirectly_regular_unary_invocable<Proj,I>&&indirectly_readable <I>>>
    using projected_value_t = remove_cvref_t<std::invoke_result_t<Proj&,iter_value_t<I>&>>;

    namespace _details
    {
        template <class Iter,class Proj>
        struct _incrementable_traits<4,projected<Iter,Proj>,std::enable_if_t<weakly_incrementable<Iter>>>
                :type_identity<iter_difference_t<Iter>>
        {};

        template <class T>
        struct _incrementable_traits<5,T>
        {};
    }

    template<class In, class Out>
    CXX17_CONCEPT indirectly_movable = _details::_indirectly_movable<In,Out>::value;

    template<class In, class Out>
    CXX17_CONCEPT indirectly_movable_storable = _details::_indirectly_movable_storable<In,Out>::value;

    template<typename In, typename Out>
    CXX17_CONCEPT indirectly_copyable= _details::_indirectly_copyable<In,Out>::value;

    template<typename In, typename Out>
    CXX17_CONCEPT indirectly_copyable_storable=   _details::_indirectly_copyable_storable<In,Out>::value;
    }

namespace NAMESPACE_RANGES
{
    namespace Cpo
    {
        using std::swap;

        template<class A,class B>
        void iter_swap(A,B) = delete;

        template<class ,class,class=void>
        struct _adl_iter_swap
                :std::false_type
        {};

        template<class T,class U>
        struct _adl_iter_swap<T,U,
                std::void_t<std::enable_if_t<(_class_or_enum_type<T>||_class_or_enum_type<U>)>,
                        decltype(iter_swap(std::declval<T&&>(), std::declval<U&&>()))>>
                :std::true_type
        {};

        template<class X,class Y>
        [[nodiscard]]constexpr iter_value_t<X>
        _iter_exchange_move(X&&x,Y&&y)
        noexcept(noexcept(iter_value_t<X>(ranges::iter_move(x)))
                 && noexcept(*x = ranges::iter_move(y)))
        {
            iter_value_t<X> old_value(ranges::iter_move(x));
            *x = ranges::iter_move(y);
            return old_value;
        }

        struct _iter_swap
        {
        private:
            template<class T,class U>
            static constexpr bool _can_noexcept()
            {
                if constexpr (_adl_iter_swap<T,U>::value)
                    return noexcept(iter_swap(std::declval<T>(),std::declval<U>()));
                else if constexpr (indirectly_readable<T>&&indirectly_readable<U>&& swappable_with<iter_reference_t<T>,iter_reference_t<U>>)
                    return noexcept(ranges::swap(*std::declval<T>(),*std::declval<U>()));
                else
                    return noexcept(*std::declval<T>()= _iter_exchange_move(std::declval<U>(),std::declval<T>()));
            }

        public:
            template<class T,class U,
                    class=_require_t<_adl_iter_swap<T, U>::value
                          ||(indirectly_readable<std::remove_reference_t<T>>
                             && indirectly_readable<std::remove_reference_t<U>>
                             && swappable_with<iter_reference_t<T>, iter_reference_t<U>>)
                          || (indirectly_movable_storable<T, U>
                              && indirectly_movable_storable<U, T>)>>
            constexpr void
            operator()(T&&t,U&&u) const
            noexcept(_can_noexcept<T, U>())
            {
                if constexpr (_adl_iter_swap<T, U>::value)
                    iter_swap(static_cast<T&&>(t), static_cast<U&&>(u));
                else if constexpr (indirectly_readable<T>&&indirectly_readable<U>&&swappable_with<iter_reference_t<T>, iter_reference_t<U>>)
                    ranges::swap(*t,*u);
                else
                    *t = _iter_exchange_move(u,t);
            }
        };
    }

    inline constexpr Cpo::_iter_swap iter_swap{};
}

namespace NAMESPACE_RANGES
{
    struct identity
    {
        template <typename T>
        constexpr T&& operator()(T&& t) const noexcept
        {
            return std::forward<T>(t);
        }
        using is_transparent = std::true_type;
    };

    struct equal_to
    {
        template <class T,class U>
        constexpr auto operator()(T&& t, U&& u) const
                -> std::enable_if_t<equality_comparable_with<T, U>, bool>
        {return std::forward<T>(t)==std::forward<U>(u);}

        using is_transparent = std::true_type;
    };

    struct not_equal_to
    {
        template <class T,class U>
        constexpr auto operator()(T&& t, U&& u) const
                -> std::enable_if_t<equality_comparable_with<T, U>, bool>
        {
            return !equal_to{}(std::forward<T>(t), std::forward<U>(u));
        }

        using is_transparent = std::true_type;
    };

    struct less
    {
        template <class T,class U>
        constexpr auto operator()(T&& t, U&& u) const
                noexcept(noexcept(std::declval<T>() < std::declval<U>()))
                -> std::enable_if_t<totally_ordered_with<T, U>, bool>
        {
            return std::forward<T>(t)<std::forward<U>(u);
        }

        using is_transparent = std::true_type;
    };

    struct greater
    {
        template <class T,class U>
        constexpr auto operator()(T&& t, U&& u) const
                noexcept(noexcept(std::declval<U>() < std::declval<T>()))
                -> std::enable_if_t<totally_ordered_with<T, U>, bool>
        {
            return less{}(std::forward<U>(u), std::forward<T>(t));
        }

        using is_transparent = std::true_type;
    };

    struct greater_equal
    {
        template <class T,class U>
        constexpr auto operator()(T&& t, U&& u) const
                noexcept(noexcept(std::declval<T>() < std::declval<U>()))
                -> std::enable_if_t<totally_ordered_with<T, U>, bool>
        {
            return !less{}(std::forward<T>(t), std::forward<U>(u));
        }

        using is_transparent = std::true_type;
    };

    struct less_equal
    {
        template <typename T, typename U>
        constexpr auto operator()(T&& t, U&& u) const
                noexcept(noexcept(std::declval<U>() < std::declval<T>()))
                -> std::enable_if_t<totally_ordered_with<T, U>, bool>
        {
            return !less{}(std::forward<U>(u), std::forward<T>(t));
        }

        using is_transparent = std::true_type;
    };
}

namespace NAMESPACE_NAME
{
    template<class A,class B,class = void>
    struct _indirectly_swappable
            :std::false_type
    {};

    template<class A,class B>
    struct _indirectly_swappable<A,B,std::void_t<
            decltype(
            ranges::iter_swap(std::declval<const A&>(),std::declval<const B&>()),
                    ranges::iter_swap(std::declval<const B&>(),std::declval<const A&>()),
                    ranges::iter_swap(std::declval<const B&>(),std::declval<const B&>()),
                    ranges::iter_swap(std::declval<const A&>(),std::declval<const A&>()))>>
            :std::true_type
    {};

    //indirectly_swappable`
    template<class A,class B=A>
    CXX17_CONCEPT indirectly_swappable
            = indirectly_readable<A>
              &&indirectly_readable<B>
              &&_indirectly_swappable<A,B>::value;

    namespace _details
    {
        template<class I1,class I2,class Rel,class P1,class P2,class = void>
        struct _indirectly_comparable
                :std::false_type
        {};

        template<class I1,class I2,class Rel,class P1,class P2>
        struct _indirectly_comparable<I1,I2,Rel,P1,P2,std::enable_if_t<indirect_binary_predicate<Rel,projected<I1,P1>,
                projected<I2,P2>>>>
                :std::true_type
        {};

        template<class I1,class I2,class Out,class Rel,class P1,class P2,class =void>
        struct _mergeable
                :std::false_type
        {};

        template<class I1,class I2,class Out,class Rel,class P1,class P2>
        struct _mergeable<I1,I2,Out,Rel,P1,P2,std::enable_if_t<input_iterator<I1>
                                                               && input_iterator<I2>
                                                               && weakly_incrementable<Out>
                                                               && indirectly_copyable<I1,Out>
                                                               && indirectly_copyable<I2,Out>
                                                               && indirect_strict_weak_order<Rel, projected<I1,P1>,projected<I2,P2>>>>
                :std::true_type
        {};
    }

    template<class I1,class I2,class Rel,class P1=ranges::identity,class P2=ranges::identity>
    CXX17_CONCEPT indirectly_comparable = _details::_indirectly_comparable<I1,I2,Rel,P1,P2>::value;

    /**
    * @brief 专门为重排算法设计的概念，基于 forward_iterator 但增加了移动和交换要求
     */
    template<class Iter>
    CXX17_CONCEPT permutable
            = forward_iterator<Iter>
              && indirectly_movable_storable<Iter,Iter>
              && indirectly_swappable<Iter,Iter>;

    template<class I1,class I2,class Out,
            class Rel = ranges::less,class P1 = ranges::identity,
            class P2 = ranges::identity>
    CXX17_CONCEPT mergeable = _details::_mergeable<I1,I2,Out,Rel,P1,P2>::value;

    namespace _details
    {
        template<class ,class,class ,class = void>
        struct _sortable
                :std::false_type
        {};

        template<class Iter,class Rel,class Proj>
        struct _sortable<Iter,Rel,Proj,std::enable_if_t<permutable<Iter>
                                                        && indirect_strict_weak_order<Rel,projected<Iter,Proj>>>>
                :std::true_type
        {};
    }

    template<class Iter,class Rel = ranges::less,
            class Proj = ranges::identity>
    CXX17_CONCEPT sortable = _details::_sortable<Iter,Rel,Proj>::value;

    struct unreachable_sentinel_t
    {
        template<class I,class=std::enable_if_t<weakly_incrementable<I>>>
        friend constexpr bool
        operator==(unreachable_sentinel_t, const I&) noexcept
        { return false; }

        template<class I,class= std::enable_if_t<weakly_incrementable<I>>>
        friend constexpr bool
        operator!=(unreachable_sentinel_t, const I&) noexcept
        { return true; }

        template<class I,class=std::enable_if_t<weakly_incrementable<I>>>
        friend constexpr bool
        operator==(const I&,unreachable_sentinel_t) noexcept
        { return false; }

        template<class I,class=std::enable_if_t<weakly_incrementable<I>>>
        friend constexpr bool
        operator!=(const I&,unreachable_sentinel_t) noexcept
        { return true; }
    };

    inline constexpr unreachable_sentinel_t unreachable_sentinel{};
}

#endif //SCALER_CPP_ITERATOR_CONCEPTS_HPP
