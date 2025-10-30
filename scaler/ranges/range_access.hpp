
/*这个文件是实现range_access相关的代码，包含c+2x相关的cpo对象以及代码*/

#ifndef DONGDONG_RANGE_ACCESS_HPP
#define DONGDONG_RANGE_ACCESS_HPP

#include"../concepts/iterator_concepts.hpp"

#define DELETE_FUNC(NAME) \
    template<class T>void NAME(T&)=delete; \
    template<class T>void NAME(const T&)=delete;

#define MAKE_CPO(object_name)\
inline constexpr auto  object_name = Cpo::_##object_name{};

namespace NAMESPACE_RANGES
{
    template <class>
    inline constexpr auto enable_borrowed_range = false;

    template <class>
    inline constexpr auto disable_sized_range = false;

    template<class I1, class I2>
    struct in_in_result
    {
        I1 in1;
        I2 in2;

        template<class II1, class II2,
                class = std::enable_if_t<convertible_to<const I1&, II1> && convertible_to<const I2&, II2>>>
        constexpr operator in_in_result<II1, II2>() const &
        {
            return {in1, in2};
        }

        template<class II1, class II2,
                class = std::enable_if_t<convertible_to< I1, II1> && convertible_to<I2, II2>>>
        constexpr operator in_in_result<II1, II2>() &&
        {
            return {std::move(in1), std::move(in2)};
        }
    };

    template<class I, class F>
    struct in_fun_result
    {
        I in;
        F fun;

        template<class I2, class F2,
                class = std::enable_if_t<convertible_to<const I&, I2> && convertible_to<const F&, F2>>>
        constexpr operator in_fun_result<I2, F2>() const &
        {
            return {in, fun};
        }

        template<class I2, class F2,
                class = std::enable_if_t<convertible_to< I, I2> && convertible_to<F, F2>>>
        constexpr operator in_fun_result<I2, F2>() &&
        {
            return {std::move(in), std::move(fun)};
        }
    };

    template<class I, class O>
    struct in_out_result
    {
        I in;
        O out;

        template<class I2, class O2,
                class = std::enable_if_t<convertible_to<const I&, I2> && convertible_to<const O&, O2>>>
        constexpr operator in_out_result<I2, O2>() const &
        {
            return {in, out};
        }

        template<class I2, class O2,
                class = std::enable_if_t<convertible_to<I, I2> && convertible_to<O, O2>>>
        constexpr operator in_out_result<I2, O2>() &&
        {
            return {std::move(in), std::move(out)};
        }
    };

    template<class I, class O1, class O2>
    struct in_out_out_result
    {
        I  in;
        O1 out1;
        O2 out2;

        template<class II, class OO1, class OO2,
                class = std::enable_if_t<convertible_to<const I&, II>
                                         &&convertible_to<const O1&, OO1>
                                         &&convertible_to<const O2&, OO2>>>
        constexpr operator in_out_out_result<II, OO1, OO2>() const &
        {
            return {in, out1, out2};
        }

        template<class II, class OO1, class OO2,
                class = std::enable_if_t<convertible_to<I, II>
                                         &&convertible_to<O1, OO1>
                                         &&convertible_to<O2, OO2>>>
        constexpr operator in_out_out_result<II, OO1, OO2>() &&
        {
            return {std::move(in), std::move(out1), std::move(out2)};
        }
    };

    template<class T>
    struct min_max_result
    {
        T min;
        T max;

        template<class T2 ,class = std::enable_if_t<convertible_to<const T&, T2>>>
        constexpr operator min_max_result<T2>() const &
        {
            return {min, max};
        }

        template<class T2 ,class = std::enable_if_t<convertible_to<T, T2>>>
        constexpr operator min_max_result<T2>() &&
        {
            return {std::move(min), std::move(max)};
        }
    };

    template<class I>
    struct in_found_result
    {
        I in;
        bool found;

        template<class I2,class = std::enable_if_t<convertible_to<const I&, I2>>>
        constexpr operator in_found_result<I2>() const &
        {
            return {in, found};
        }

        template<class I2,class = std::enable_if_t<convertible_to<I, I2>>>
        constexpr operator in_found_result<I2>() &&
        {
            return {std::move(in), found};
        }
    };

    template<class I, class T>
    struct in_value_result
    {
        I in;
        T value;

        template<class I2, class T2
                ,class = std::enable_if_t<convertible_to<const I&, I2> && convertible_to<const T&, T2>>>
        constexpr operator in_value_result<I2, T2>() const &
        {
            return {in, value};
        }

        template<class I2, class T2
                ,class = std::enable_if_t<convertible_to<I, I2> && convertible_to<T, T2>>>
        constexpr operator in_value_result<I2, T2>() &&
        {
            return {std::move(in), std::move(value)};
        }
    };

    template<class O, class T>
    struct out_value_result
    {
        O out;
        T value;

        template<class O2, class T2,
                class = std::enable_if_t<convertible_to<const O&, O2> && convertible_to<const T&, T2>>>
        constexpr operator out_value_result<O2, T2>() const &
        {
            return {out, value};
        }

        template<class O2, class T2,
                class = std::enable_if_t<convertible_to<O, O2> && convertible_to<T, T2>>>
        constexpr operator out_value_result<O2, T2>() &&
        {
            return {std::move(out), std::move(value)};
        }
    };

    template<class I1, class I2, class O>
    struct in_in_out_result
    {
        I1 in1;
        I2 in2;
        O  out;

        template<class II1, class II2, class OO,
            class = std::enable_if_t<convertible_to<const I1&, II1> &&convertible_to<const I2&, II2>&&convertible_to<const O&, OO>>>
        constexpr operator in_in_out_result<II1, II2, OO>() const &
        {
            return {in1, in2, out};
        }

        template<class II1, class II2, class OO,
                    class = std::enable_if_t<convertible_to<const I1&, II1> &&convertible_to<const I2&, II2>&&convertible_to<const O&, OO>>>
        constexpr operator in_in_out_result<II1, II2, OO>() &&
        {
            return {std::move(in1), std::move(in2), std::move(out)};
        }
    };

}

namespace NAMESPACE_RANGES
{
    namespace Cpo
    {
        DELETE_FUNC(begin)
        DELETE_FUNC(end)
        DELETE_FUNC(rbegin)
        DELETE_FUNC(rend)
        DELETE_FUNC(size)

        #undef DELETE_FUNC

        //ranges::begin
        struct _begin
        {
            template<class T,class = void>
            struct _adl_begin
                :std::false_type
            {};

            template<class T>
            struct _adl_begin<T,std::enable_if_t<input_or_output_iterator<decltype(decay_copy(begin(std::declval<T&>())))>>>
                :std::true_type
            {};

            template<class T,class = void>
            struct _has_member
                :std::false_type
            {};

            template<class T>
            struct _has_member<T,
                std::enable_if_t<input_or_output_iterator<decltype(decay_copy(std::declval<T>().begin()))>>>
                :std::true_type
            {};

        private:
            template<class T>
            static constexpr bool _can_noexcept()
            {
                if constexpr (std::is_array_v<std::remove_reference_t<T>>)
                    return true;
                else if constexpr (_has_member<T>::value)
                    return noexcept(decay_copy(std::declval<T&>().begin()));
                else
                    return noexcept(decay_copy(begin(std::declval<T&>())));
            }

        public:
            template<class T,
                class = std::enable_if_t<std::is_lvalue_reference_v<T>||ranges::enable_borrowed_range<remove_cvref_t<T>>>,
                class= _require_or_t<
                _adl_begin<T>::value,
                std::is_array_v<std::remove_reference_t<T>>,
                _has_member<T>::value>>
            constexpr auto operator()(T&& t)const noexcept(_can_noexcept<T>())
            {
                if constexpr (std::is_array_v<std::remove_reference_t<T>>)
                    return t + 0;
                else if constexpr (_has_member<T>::value)
                    return decay_copy(t.begin());
                else
                    return decay_copy(begin(t));
            }
        };
        //ranges::end
        struct _end
        {
            template<class T>
            using _iterator_t = decltype(std::declval<Cpo::_begin&>()(std::declval<T&>()));
        private:
            template<class,class= void >
            struct _has_member
                    :std::false_type
            {};

            template<class Class>
            struct _has_member<Class,
                    std::enable_if_t<sentinel_for< decltype(decay_copy(std::declval<Class&>().end())),_iterator_t<Class>>>>
            :std::true_type
            {};

            template<class,class =void >
            struct _adl_end
                    :std::false_type
            {};

            template<class Class>
            struct _adl_end<Class,
                    std::enable_if_t<sentinel_for<
                            decltype(decay_copy(end(std::declval<Class&>()))),
                            _iterator_t<Class>>
            >//require
            > //_adl_end
            :std::bool_constant<_class_or_enum_type<std::remove_reference_t<Class>>>
            {};

        public:
            template<class T>
            static constexpr bool _can_noexcept()
            {
                if constexpr (is_bounded_array_v<std::remove_reference_t<T>>)
                    return true;
                else if constexpr (_has_member<T>::value)
                    return noexcept(decay_copy(std::declval<T&>().end()));
                else
                    return noexcept(decay_copy(end(std::declval<T&>())));
            }

        public:
            template<class T,
                    class= _require_t<std::is_lvalue_reference_v<T>||enable_borrowed_range<remove_cvref_t<T>>>,
            class= _require_or_t<is_bounded_array_v<std::remove_reference_t<T>>,_has_member<T>::value,_adl_end<T>::value>>
            constexpr auto
            operator()[[nodiscard]](T&& t)
            const noexcept(_can_noexcept<T&>())
            {
                if constexpr (is_bounded_array_v<std::remove_reference_t<T>>)
                {
                    return t + std::extent_v<std::remove_reference_t<T>>;
                }
                else if constexpr (_has_member<T>::value)
                    return decay_copy(t.end());
                else
                    return decay_copy(end(t));
            }
        };

        template<class T>
        static constexpr bool _reversable
                =bidirectional_iterator<decltype(std::declval<_begin&>()(std::declval<T&>()))>
                 &&same_as<decltype(std::declval<_end&>()(std::declval<T&>())),decltype(_begin{}(std::declval<T&>()))>;

        struct _cend final
        {
            template<typename T,
                    class=std::void_t<decltype(std::declval<_end&>()(static_cast<CT<T>&&>(std::declval<T&>())))>>
            [[nodiscard]]
            constexpr auto operator()(T&& t) const
            noexcept(noexcept(std::declval<_end&>()(static_cast<CT<T>&&>(t))))
            {
                return _end{}(static_cast<CT<T>&&>(t));
            }
        };
        struct _cbegin
        {
            template<class T,
                    class=std::void_t<decltype(std::declval<_begin&>()(static_cast<CT<T>&&>(std::declval<T&>())))>>
            [[nodiscard]]
            constexpr auto operator()(T&& t) const
            noexcept(noexcept(std::declval<_begin&>()(static_cast<CT<T>&&>(t))))
            {
                return Cpo::_begin()(static_cast<CT<T>&&>(t));
            }
        };
        struct _rbegin
        {
        private:
            template<class,class =void >
            struct _has_rbegin
                    :std::false_type
            {};

            template<class T>
            struct _has_rbegin<T,
                    std::enable_if_t<input_or_output_iterator<decltype(decay_copy(std::declval<T>().rbegin()))>>>
                    :std::true_type
            {};

            template<class T,class = void>
            struct _adl_rbegin
                    :std::false_type
            {};

            template<class T>
            struct _adl_rbegin<T,
                    std::enable_if_t<input_or_output_iterator<decltype(decay_copy(rbegin(std::declval<T&>())))>>>
                    :std::bool_constant<_class_or_enum_type<T>>
            {};

        private:
            template<class T>
            static constexpr bool _can_noexcept()
            {
                if constexpr (_has_rbegin<T>::value)
                    return noexcept(decay_copy(std::declval<T&>().rbegin()));
                else if constexpr (_adl_rbegin<T>::value)
                    return noexcept(decay_copy(rbegin(std::declval<T&>())));
                else
                {
                    if constexpr (noexcept(_end{}(std::declval<T&>())))
                    {
                        using It = decltype(std::declval<_end&>()(std::declval<T&>()));
                        // std::reverse_iterator copy-initializes its member.
                        return std::is_nothrow_copy_constructible_v<It>;
                    }
                    else
                        return false;
                }
            }

        public:
            template<class T,
                    class= _require_or_t<
                           (std::is_lvalue_reference_v<T> ||
                            ranges::enable_borrowed_range<remove_cvref_t<T>>),
            _has_rbegin<T>::value,
            _adl_rbegin<T>::value,
            _reversable<T>>>
            constexpr auto
            operator()[[nodiscard]](T&& t) const
            noexcept(_can_noexcept<T&>())
            {
                if constexpr (_has_rbegin<T>::value)
                    return t.rbegin();
                else if constexpr (_adl_rbegin<T>::value)
                    return rbegin(t);
                else
                    return std::make_reverse_iterator(_end{}(t));
            }
        };
        struct _crbegin
        {
            template<typename T,
                    class=std::void_t<decltype(std::declval<_rbegin&>()(static_cast<CT<T>&&>(std::declval<T&>())))>>
            [[nodiscard]]
            constexpr auto operator()(T&& t) const
            noexcept(noexcept(std::declval<_rbegin&>()(static_cast<CT<T>&&>(t))))
            {
                return _rbegin{}(static_cast<CT<T>&&>(t));
            }
        };
        struct _rend
        {
        private:
            //Check rend
            template<class,class =void >
            struct _has_rend
                    :std::false_type
            {};

            template<class T>
            struct _has_rend<T,
                    _require_t<sentinel_for<decltype(decay_copy(rend(std::declval<T&>()))),
                            decltype(_rbegin{}(std::forward<T>(std::declval<T&>())))
                    >>>
            :std::true_type
            {};

            template<class T,class =void >
            struct _adl_rend
                    :std::false_type
            {};

            template<class T>
            struct _adl_rend<T,
                    std::enable_if_t<sentinel_for<
                            decltype(decay_copy(std::declval<_rend&>()(std::declval<T&>()))),
                            decltype(std::declval<_rbegin&>()(std::declval<T&>()))>>>
                    :std::bool_constant<_class_or_enum_type<T>>
            {};

        private:
            template<class T>
            static constexpr bool _can_noexcept()
            {
                if constexpr (_has_rend<T>{})
                    return noexcept(decay_copy(std::declval<T&>().rend()));
                else if constexpr (_adl_rend<T>::value)
                    return noexcept(decay_copy(rend(std::declval<T&>())));
                else
                {
                    if constexpr (noexcept(_begin{}(std::declval<T&>())))
                    {
                        using It = decltype(_begin{}(std::declval<T&>()));
                        return std::is_nothrow_copy_constructible_v<It>;
                    }
                    else
                        return false;
                }
            }

        public:
            template<class T,
                    class= _require_or_t<(std::is_lvalue_reference_v<T> ||ranges::enable_borrowed_range<remove_cvref_t<T>>),
            _has_rend<T>::value,
            _adl_rend<T>::value,
            _reversable<T>>>
            constexpr auto
            operator()[[nodiscard]](T&& t) const
            noexcept(_can_noexcept<T&>())
            {
                if constexpr (_has_rend<T>::value)
                    return t.rend();
                else if constexpr (_adl_rend<T>::value)
                    return rend(t);
                else
                    return std::make_reverse_iterator(_begin{}(t));
            }
        };
        struct _crend
        {
            template<typename T,
                    class=std::void_t<decltype(std::declval<_rend&>()(static_cast<CT<T>&&>(std::declval<T&>())))>>
            [[nodiscard]]
            constexpr auto operator()(T&& t) const
            noexcept(noexcept(std::declval<_rend&>()(static_cast<CT<T>&&>(t))))
            {
                return _rend{}(static_cast<CT<T>&&>(t));
            }
        };
        struct _size
        {
        private:
            template<class T,class = void,class = void>
            struct _check_member_size
                    :std::false_type
            {};

            template<class T>
            struct _check_member_size<T,
                    std::void_t<decltype(decay_copy(std::declval<T&>().size()))>,
                    std::enable_if_t<integral<decltype(decay_copy(std::declval<T&>().size()))>>>
                    :std::true_type
            {};

            template<class,class =void >
            struct _adl_size
                    :std::false_type
            {};

            template<class T>
            struct _adl_size<T,
                    _require_t<
                    integral<decltype(decay_copy(size(std::declval<T&>())))>,
                    _class_or_enum_type<std::remove_reference_t<T>>,
            !disable_sized_range<remove_cvref_t<T>>
            >>
            :std::true_type
            {};

            template<class T,class = void>
            struct _sentinel_size
                    :std::false_type
            {};

            template<class T>
            struct _sentinel_size<T,_require_t<
                                    forward_iterator<decltype(std::declval<_begin&>()(std::declval<T&>()))>,
                    sized_sentinel_for<decltype(std::declval<_end&>()(std::declval<T&>())),decltype(_begin{}(std::declval<T&>()))>,
                    std::is_unsigned_v<decltype(std::declval<_end&>()(std::declval<T&>())-std::declval<_begin&>()(std::declval<T&>()))>>>
            :std::false_type
            {};
        private:
            template<class T>
            static constexpr bool _can_noexcept()
            {
                if constexpr (is_bounded_array_v<std::remove_reference_t<T>>)
                return true;
                else if constexpr (_check_member_size<T>::value)
                    return noexcept(decay_copy(std::declval<T&>().size()));
                else if constexpr (_adl_size<T>::value)
                    return noexcept(decay_copy(size(std::declval<T&>())));
                else if constexpr (_sentinel_size<T>::value)
                    return noexcept(_end{}(std::declval<T&>()) - _begin{}(std::declval<T&>()));
            }
        public:
            template<class T,
                    class U=std::remove_reference_t<T>,
                    class= _require_or_t<is_bounded_array_v<U>,
                    _check_member_size<T>::value,
                    _adl_size<T>::value,
                    _sentinel_size<T>::value>>
            constexpr auto operator()(T&&t)const noexcept(_can_noexcept<T&>())
            {
                if constexpr(is_bounded_array_v<U>)
                    return std::extent_v<U>;
                else if constexpr (_check_member_size<T>::value)
                    return t.size();
                else if constexpr (_adl_size<T>::value)
                    return decay_copy(size(t));
                else if constexpr(_sentinel_size<T>::value)
                    return _end{}(std::declval<T&>())- _begin{}(std::declval<T&>());
            }
        };
        struct _ssize
        {
        private:
            static constexpr Cpo::_size Size{};

            template<class,class = void>
            struct _can_size
                    :std::false_type
            {};
            template<class T>
            struct _can_size<T,std::void_t<decltype(std::declval<_size&>()(std::declval<T&>()))>>
                    :std::true_type
            {};
        public:
            template<class T,class= std::enable_if_t<_can_size<T>::value>>
            constexpr auto
            operator()[[nodiscard]](T&& t)const noexcept(noexcept(_size{}(t)))
            {return static_cast<std::ptrdiff_t>(Size(t));}
        };
    }

    MAKE_CPO(begin)
    MAKE_CPO(end)
    MAKE_CPO(cbegin)
    MAKE_CPO(cend)
    MAKE_CPO(crend)
    MAKE_CPO(crbegin)
    MAKE_CPO(ssize)
    MAKE_CPO(size)

    namespace Cpo
    {
        struct _empty
        {
        private:
            template<class T,class =void>
            struct _has_empty
                    :std::false_type
            {};

            template<class T>
            struct _has_empty<T,std::void_t<decltype(bool(std::declval<T&>().empty()))>>
                    :std::true_type
            {};

            template<class,class = void>
            struct _size_equal_0
                    :std::false_type
            {};

            template<class T>
            struct _size_equal_0<T,std::void_t<decltype(std::declval<_size&>(std::declval<T&>()==0))>>
                    :std::true_type
            {};

            template<class T,class=void>
            struct _begin_equal_end
                    :std::false_type
            {};

            template<class T>
            struct _begin_equal_end<T,
                    std::void_t<
                            std::enable_if_t<forward_iterator<decltype(std::declval<_begin&>()(std::declval<T&>()))>>,
                            decltype(bool(std::declval<_begin&>()(std::declval<T&>())==std::declval<_end&>()(std::declval<T&>())))>>
                    :std::true_type
            {};

        private:
            template<class T>
            static constexpr bool _can_noexcept()
            {
                if constexpr (_has_empty<T>::value)
                    return noexcept(bool(std::declval<T&>().empty()));
                else if constexpr (_size_equal_0<T>::value)
                    return noexcept(std::declval<_size&>()(std::declval<T&>()) == 0);
                else
                    return noexcept(bool(std::declval<_begin&>()(std::declval<T&>())==std::declval<_end&>()(std::declval<T&>())));
            }

        public:
            template<class T,
                    class= _require_or_t<_has_empty<T>::value,
                            _size_equal_0<T>::value,
                            _begin_equal_end<T>::value>>
            constexpr bool
            operator()[[nodiscard]](T&& t) const noexcept(_can_noexcept<T&>())
            {
                if constexpr (_has_empty<T>::value)
                    return bool(t.empty());
                else if constexpr (_size_equal_0<T>::value)
                    return ranges::size(t) == 0;
                else
                    return bool(ranges::begin(t) == ranges::end(t));
            }
        };
        struct _data
        {
        private:

            template<class T>
            using _pointer_to_object = std::bool_constant<std::is_object_v<std::remove_pointer_t<T>>&&std::is_pointer_v<T>>;

            template<class T ,class = void>
            struct _has_member_data
                    :std::false_type
            {};

            template<class T>
            struct _has_member_data<T,std::enable_if_t<_pointer_to_object<decltype(decay_copy(std::declval<T&>().data()))>::value>>
                    :std::true_type
            {};

            template<class T,class = void>
            struct _has_begin_to_contiguous_iterator
                    :std::false_type
            {};

            template<class T>
            struct _has_begin_to_contiguous_iterator<T,std::void_t<
                    decltype(ranges::begin(std::declval<T&>())),
                    std::enable_if_t<contiguous_iterator<decltype(ranges::begin(std::declval<T&>()))>>>>
                    :std::true_type
            {};

            template<class T>
            static constexpr bool _can_noexcept()
            {
                if constexpr (_has_member_data<T>::value)
                    return noexcept(decay_copy(std::declval<T&>().data()));
                else
                    return noexcept(std::declval<_begin&>()(std::declval<T&>()));
            }

        public:
            template<class T,
                class=_require_t<(std::is_lvalue_reference_v<T>
                                                ||ranges::enable_borrowed_range<remove_cvref_t<T>>)>,
            class=_require_or_t<_has_member_data<T>::value,
            _has_begin_to_contiguous_iterator<T>::value>>
            constexpr auto
            operator()[[nodiscard]](T&& t) const noexcept(_can_noexcept<T>())
            {
                if constexpr (_has_member_data<T>::value)
                    return t.data();
                else
                    return __builtin_addressof(_begin{}(t));
            }
        };
        struct _cdata
        {
            template<typename T,
                    class=std::void_t<decltype(std::declval<_data&>()(static_cast<CT<T>&&>(std::declval<T&>())))>>
            [[nodiscard]]
            constexpr auto operator()(T&& t) const
            noexcept(noexcept(std::declval<_data&>()(static_cast<CT<T>&&>(t))))
            {
                return _data{}(static_cast<CT<T>&&>(t));
            }
        };
    }
    MAKE_CPO(empty)
    MAKE_CPO(data)
    MAKE_CPO(cdata)

    namespace  _details
    {
        template<class,class=void>
        struct  _range
                :std::false_type
        {};

        template<class T>
        struct _range<T,std::void_t<decltype(ranges::begin(std::declval<T&>())),decltype(ranges::end(std::declval<T&>()))>>
            :std::true_type
        {};
    }
}
#endif //DONGDONG_RANGE_ACCESS_HPP
