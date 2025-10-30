
/*
 * c++20  std::ranges算法
 */

#ifndef RANGES_ALGORITHM_HPP
#define RANGES_ALGORITHM_HPP

#include  "range_concepts.hpp"
#include <random>
#include <algorithm>

namespace
NAMESPACE_RANGES
{
    namespace Cpo
    {
        struct _invoke
        {
        private:
            template <class T>
            struct _traits
            {};

            template <class Class, class R>
            struct _traits<R Class::*>
            {
                using class_type = Class;
            };

        private:
            template <class Callable, class Obj, class... Args>
            constexpr decltype(auto) operator()(std::true_type, Callable&& callable, Obj&& obj, Args&&... args) const
            {
                using Class = typename _traits<remove_cvref_t<Callable>>::class_type;
                if constexpr (std::disjunction_v<
                    std::is_same<Class, remove_cvref_t<Obj>>, std::is_base_of<Class, remove_cvref_t<Obj>>>)
                {
                    if constexpr (std::is_member_function_pointer_v<remove_cvref_t<Callable>>)
                        return (std::forward<Obj>(obj).*callable)(std::forward<Args>(args)...);
                    else if constexpr (std::is_member_object_pointer_v<Callable> && (sizeof...(Args) == 0))
                        return std::forward<Obj>(obj).*callable;
                }
                else if constexpr (_is_reference_wrapper_v<remove_cvref_t<Obj>>)
                {
                    if constexpr (std::is_member_function_pointer_v<remove_cvref_t<Callable>>)
                        return (std::forward<Obj>(obj).get().*callable)(std::forward<Args>(args)...);
                    else if constexpr (std::is_member_object_pointer_v<remove_cvref_t<Callable>>
                        && (sizeof...(Args) ==0))
                        return std::forward<Obj>(obj).get().*callable;
                }
                else
                {
                    if constexpr (std::is_member_function_pointer_v<remove_cvref_t<Callable>>)
                        return ((*static_cast<Obj&&>(obj)).*callable)(std::forward<Args>(args)...);
                    else if constexpr (std::is_member_object_pointer_v<remove_cvref_t<Callable>>
                        && (sizeof...(Args) ==0))
                        return (*std::forward<Obj>(obj)).*callable;
                }
            }

            template <class Callable, class... Args>
            constexpr decltype(auto) operator()(std::false_type, Callable&& callable, Args&&... args) const
            {
                return std::forward<Callable>(callable)(std::forward<Args>(args)...);
            }

            template <class, class, class...>
            struct _can_operator
                : std::false_type
            {};

            template <class Callable, class... Args>
            struct _can_operator<std::void_t<decltype(std::declval<Callable&&>()(std::declval<Args&&>()...))>,
                                Callable,
                                 Args...>
                : std::true_type
            {};

            template <class Callable, class... Args>
            struct _can_operator<std::enable_if_t<std::is_member_pointer_v<remove_cvref_t<Callable>>>, Callable, Args
                                 ...>
                : std::true_type
            {};

        public:
            template <class Callable, class... Args, class = std::enable_if_t<_can_operator<
                          void, Callable, Args...>::value>>
            constexpr decltype(auto) operator()(Callable&& callable, Args&&... args) const
                noexcept(noexcept((*this)(std::bool_constant<std::is_member_pointer_v<remove_cvref_t<Callable>>>{},
                                          std::forward<Callable>(callable), std::forward<Args>(args)...)))
            {
                return (*this)(std::bool_constant<std::is_member_pointer_v<remove_cvref_t<Callable>>>{},
                               std::forward<Callable>(callable), std::forward<Args>(args)...);
            }
        };
    }

    inline constexpr Cpo::_invoke invoke{};

    namespace Cpo
    {
        struct _count
        {
            template <class I, class S,
                      class Proj = identity, class T = projected_value_t<I, Proj>,
                      class = std::enable_if_t<indirect_binary_predicate<ranges::equal_to,
                                                                         projected<I, Proj>, const T*> & input_iterator<
                          I> && sentinel_for<S, I>>>
            constexpr iter_difference_t<I>
            operator()(I first, S last, const T& value, Proj proj = {}) const
            {
                iter_difference_t<I> counter = 0;
                for (; first != last; ++first)
                    if (invoke(proj, *first) == value)
                        ++counter;
                return counter;
            }

            template <class R, class Proj = ranges::identity,
                      class T = projected_value_t<ranges::iterator_t<R>, Proj>,
                      class = std::enable_if_t<indirect_binary_predicate<ranges::equal_to,
                                                                         projected<ranges::iterator_t<R>, Proj>,
                                                                         const T*> && input_range<R>>>
            constexpr ranges::range_difference_t<R>
            operator()(R&& r, const T& value, Proj proj = {}) const
            {
                return (*this)(ranges::begin(r), ranges::end(r), value, std::move(proj));
            }
        };

        struct _count_if
        {
            template <class I, class S,
                      class Proj = identity, class Pred,
                      class = std::enable_if_t<input_iterator<I>
                          && sentinel_for<S, I>
                          && indirect_unary_predicate<Pred, projected<I, Proj>>>>
            constexpr iter_difference_t<I>
            operator()(I first, S last, Pred pred, Proj proj = {}) const
            {
                iter_difference_t<I> counter = 0;
                for (; first != last; ++first)
                    if (std::invoke(pred, invoke(proj, *first)))
                        ++counter;
                return counter;
            }

            template <class R, class Proj = identity,
                      class Pred,
                      class = std::enable_if_t<ranges::input_range<R>
                          && indirect_unary_predicate<Pred, projected<ranges::iterator_t<R>, Proj>>>>
            constexpr ranges::range_difference_t<R>
            operator()(R&& r, Pred pred, Proj proj = {}) const
            {
                return (*this)(ranges::begin(r), ranges::end(r), std::move(pred), std::move(proj));
            }
        };

        struct _distance final
        {
            template <class It, class Sent,
                      class= _require_t<input_or_output_iterator<It>, sentinel_for<Sent, It>>>
            constexpr iter_difference_t<It> operator()[[nodiscard]](It first, Sent last) const
            {
                if constexpr (sized_sentinel_for<Sent, It>)
                    return last - first;
                else
                {
                    iter_difference_t<It> n = 0;
                    while (first != last)
                    {
                        ++first;
                        ++n;
                    }
                    return n;
                }
            }

            template <class Range, class= _require_t<range<Range>>>
            [[nodiscard]]
            constexpr range_difference_t<Range>
            operator()(Range&& r) const
            {
                if constexpr (sized_range<Range>)
                    return static_cast<range_difference_t<Range>>(ranges::size(r));
                else
                    return (*this)(ranges::begin(r), ranges::end(r));
            }

            void operator&() const = delete;
        };

        struct _advance
        {
            template <class I, class=std::enable_if_t<input_or_output_iterator<I>>>
            constexpr void operator()(I& i, iter_difference_t<I> n) const
            {
                if constexpr (random_access_iterator<I>)
                    i += n;
                else
                {
                    while (n > 0)
                    {
                        --n;
                        ++i;
                    }

                    if constexpr (bidirectional_iterator<I>)
                    {
                        while (n < 0)
                        {
                            ++n;
                            --i;
                        }
                    }
                }
            }

            template <class I, class S,
                      class=std::enable_if_t<input_or_output_iterator<I> && sentinel_for<S, I>>>
            constexpr void operator()(I& i, S bound) const
            {
                if constexpr (assignable_from<I&, S>)
                    i = std::move(bound);
                else if constexpr (sized_sentinel_for<S, I>)
                    (*this)(i, bound - i);
                else
                    while (i != bound)
                        ++i;
            }

            template <class I, class S,
                      class=std::enable_if_t<input_or_output_iterator<I> && sentinel_for<S, I>>>
            constexpr iter_difference_t<I>
            operator()(I& i, iter_difference_t<I> n, S bound) const
            {
                if constexpr (sized_sentinel_for<S, I>)
                {
                    // std::abs is not constexpr until C++23
                    auto abs = [](const iter_difference_t<I> x) { return x < 0 ? -x : x; };

                    if (const auto dist = abs(n) - abs(bound - i); dist < 0)
                    {
                        (*this)(i, bound);
                        return -dist;
                    }

                    (*this)(i, n);
                    return 0;
                }
                else
                {
                    while (n > 0 && i != bound)
                    {
                        --n;
                        ++i;
                    }

                    if constexpr (bidirectional_iterator<I>)
                    {
                        while (n < 0 && i != bound)
                        {
                            ++n;
                            --i;
                        }
                    }

                    return n;
                }
            }
        };

        struct _next final
        {
            template <class It, class= std::enable_if_t<input_or_output_iterator<It>>>
            [[nodiscard]]
            constexpr It operator()(It x) const
            {
                ++x;
                return x;
            }

            template <class It, class=std::enable_if_t<input_or_output_iterator<It>>>
            [[nodiscard]]
            constexpr It operator()(It x, iter_difference_t<It> n) const
            {
                _advance{}(x, n);
                return x;
            }

            template <class It, class Sent,
                      class=std::enable_if_t<input_or_output_iterator<It>>,
                      class=std::enable_if_t<sentinel_for<Sent, It>>>
            constexpr It
            operator()(It it, Sent bound) const
            {
                _advance{}(it, bound);
                return it;
            }

            template <class It, class Sent,
                      class=std::enable_if_t<input_or_output_iterator<It>>,
                      class=std::enable_if_t<sentinel_for<Sent, It>>>
            [[nodiscard]]
            constexpr It
            operator()(It it, iter_difference_t<It> n, Sent bound) const
            {
                _advance{}(it, n, bound);
                return it;
            }

            void operator&() const = delete;
        };

        struct _prev final
        {
            template <class It, class=std::enable_if_t<bidirectional_iterator<It>>>
            [[nodiscard]]
            constexpr It operator()(It it) const
            {
                --it;
                return it;
            }

            template <class It, class=_require_t<bidirectional_iterator<It>>>
            [[nodiscard]]
            constexpr It operator()(It iter, iter_difference_t<It> n) const
            {
                _advance{}(iter, -n);
                return iter;
            }

            template <class It, class=_require_t<bidirectional_iterator<It>>>
            [[nodiscard]]
            constexpr It operator()(It x, iter_difference_t<It> n, It bound) const
            {
                _advance{}(x, -n, bound);
                return x;
            }

            void operator&() const = delete;
        };

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
            template <class T, class U>
            constexpr auto operator()(T&& t, U&& u) const
                -> std::enable_if_t<equality_comparable_with<T, U>, bool>
            {
                return std::forward<T>(t) == std::forward<U>(u);
            }

            using is_transparent = std::true_type;
        };

        struct not_equal_to
        {
            template <class T, class U>
            constexpr auto operator()(T&& t, U&& u) const
                -> std::enable_if_t<equality_comparable_with<T, U>, bool>
            {
                return !equal_to{}(std::forward<T>(t), std::forward<U>(u));
            }

            using is_transparent = std::true_type;
        };

        namespace _details
        {
            template <class T, class U, class = void>
            struct _less_builtin_ptr_cmp_base
                : std::true_type
            {};

            template <class T, class U>
            struct _less_builtin_ptr_cmp_base<T, U,
                                              std::void_t<decltype(operator<(std::declval<T&&>(), std::declval<U&&>())),
                                                          decltype(std::declval<T&&>().operator<(std::declval<U&&>()))>>
                : std::false_type
            {};

            template <class T, class U, class = void, class = void>
            struct _less_builtin_ptr_cmp
                : std::false_type
            {};

            template <class T, class U>
            struct _less_builtin_ptr_cmp<T, U
                                         , _require_t<
                                             convertible_to<T, const volatile void*>,
                                             convertible_to<U, const volatile void*>,
                                             same_as<bool, decltype(std::declval<T&&>() < std::declval<U&&>())>
                                         > //require_t
                >
                : _less_builtin_ptr_cmp_base<T, U>
            {};
        }

        struct less
        {
            template <class T, class U>
            constexpr auto operator()(T&& t, U&& u) const
                noexcept(noexcept(std::declval<T>() < std::declval<U>()))
                -> std::enable_if_t<totally_ordered_with<T, U>, bool>
            {
                if constexpr (_details::_less_builtin_ptr_cmp<T, U>::value)
                {
                    auto x = reinterpret_cast<long long unsigned int>(
                        static_cast<const volatile void*>(std::forward<T>(t)));
                    auto y = reinterpret_cast<long long unsigned int>(
                        static_cast<const volatile void*>(std::forward<U>(u)));
                    return x < y;
                }
                else
                    return std::forward<T>(t) < std::forward<U>(u);
            }

            using is_transparent = std::true_type;
        };

        struct greater
        {
            template <class T, class U>
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
            template <class T, class U>
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

    inline constexpr Cpo::_count count;
    inline constexpr Cpo::_count_if count_if;
    inline constexpr auto distance = Cpo::_distance{};

    namespace Cpo
    {
        struct _find
        {
            template <class I, class S,
                      class Proj = identity,
                      class T = projected_value_t<I, Proj>,
                      class=std::enable_if_t<input_iterator<I>
                          && sentinel_for<S, I>
                          && indirect_binary_predicate<ranges::equal_to, projected<I, Proj>, const T*>>>
            constexpr I operator()(I first, S last, const T& value, Proj proj = {}) const
            {
                for (; first != last; ++first)
                    if (invoke(proj, *first) == value)
                        return first;
                return first;
            }

            template <class R, class T, class Proj = ranges::identity,
                      class=std::enable_if_t<input_range<R>
                          && indirect_binary_predicate<ranges::equal_to,
                                                       projected<ranges::iterator_t<R>, Proj>, const T*>>>
            constexpr borrowed_iterator_t<R>
            operator()(R&& r, const T& value, Proj proj = {}) const
            {
                return (*this)(ranges::begin(r), ranges::end(r), value, std::move(proj));
            }
        };

        struct _find_if
        {
            template <class I, class S, class Pred,
                      class Proj = identity,
                      class=std::enable_if_t<indirect_unary_predicate<Pred, projected<I, Proj>>>>
            constexpr I operator()(I first, S last, Pred pred, Proj proj = {}) const
            {
                for (; first != last; ++first)
                    if (invoke(pred, invoke(proj, *first)))
                        return first;
                return first;
            }

            template <class R, class Pred, class Proj = identity,
                      class=std::enable_if_t<input_range<R>
                          && indirect_unary_predicate<Pred, projected<ranges::iterator_t<R>, Proj>>>>
            constexpr ranges::borrowed_iterator_t<R>
            operator()(R&& r, Pred pred, Proj proj = {}) const
            {
                return (*this)(ranges::begin(r), ranges::end(r), std::move(pred), std::move(proj));
            }
        };

        struct _find_if_not
        {
            template <class I, class S, class Pred,
                      class Proj = identity,
                      class=std::enable_if_t<indirect_unary_predicate<Pred, projected<I, Proj>>>>
            constexpr I operator()(I first, S last, Pred pred, Proj proj = {}) const
            {
                for (; first != last; ++first)
                    if (!invoke(pred, invoke(proj, *first)))
                        return first;
                return first;
            }

            template <class R, class Pred, class Proj = identity,
                      class=std::enable_if_t<input_range<R>
                          && indirect_unary_predicate<Pred, projected<ranges::iterator_t<R>, Proj>>>>
            constexpr ranges::borrowed_iterator_t<R>
            operator()(R&& r, Pred pred, Proj proj = {}) const
            {
                return (*this)(ranges::begin(r), ranges::end(r), std::move(pred), std::move(proj));
            }
        };
    }

    inline constexpr Cpo::_find find;
    inline constexpr Cpo::_find_if find_if;
    inline constexpr Cpo::_find_if_not find_if_not;
    inline constexpr auto advance = Cpo::_advance();
    inline constexpr Cpo::_next next;
    inline constexpr Cpo::_prev prev;
}

namespace
NAMESPACE_RANGES::Cpo
{
    struct _copy
    {
    public:
        template <typename I, typename S, typename O,
                  typename = _require_t<input_iterator<I>,
                                        sentinel_for<S, I>,
                                        weakly_incrementable<O>,
                                        indirectly_copyable<I, O>
                  >>
        constexpr ranges::in_out_result<I, O> operator()(I first, S last, O result) const
        {
            for (; first != last; ++first, (void)++result)
                *result = *first;
            return {std::move(first), std::move(result)};
        }

        template <typename R, typename O,
                  typename= _require_t<
                      ranges::input_range<R>, weakly_incrementable<O>,
                      indirectly_copyable<ranges::iterator_t<R>, O>>>
        constexpr ranges::in_out_result<borrowed_iterator_t<R>, O> operator()(R&& r, O result) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(result));
        }
    };

    struct _copy_if
    {
        template <class I, class S, class O, class Proj = identity, class Pred,
                  class = _require_t<input_iterator<I>,
                                     sentinel_for<S, I>,
                                     weakly_incrementable<O>,
                                     //projectable<I, Proj>,
                                     indirect_unary_predicate<Pred, projected<I, Proj>>,
                                     indirectly_copyable<I, O>
                  >>
        constexpr ranges::in_out_result<I, O> operator()(I first, S last, O result, Pred pred, Proj proj = {}) const
        {
            while (first != last)
            {
                if (ranges::invoke(pred, ranges::invoke(proj, *first)))
                {
                    *result = *first;
                    ++result;
                }
                ++first;
            }
            return {std::move(first), std::move(result)};
        }

        template <typename R, class O, class Pred, class Proj = identity,
                  class = _require_t<input_range<R>,
                                     weakly_incrementable<O>,
                                     indirect_unary_predicate<Pred, projected<iterator_t<R>, Proj>>,
                                     indirectly_copyable<iterator_t<R>, O>
                  >>
        constexpr ranges::in_out_result<borrowed_iterator_t<R>, O> operator()(
            R&& r, O result, Pred pred, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(result), std::move(pred), std::move(proj));
        }
    };

    struct _copy_n
    {
        template <class I, class O,
                  class = std::enable_if_t<input_iterator<I> &&
                      weakly_incrementable<O>
                      && indirectly_copyable<I, O>>>
        constexpr ranges::in_out_result<I, O>
        operator()(I first, iter_difference_t<I> n, O result) const
        {
            for (iter_difference_t<I> i{}; i != n; ++i, ++first, ++result)
                *result = *first;
            return {std::move(first), std::move(result)};
        }
    };

    struct _copy_backward
    {
        template <class I1, class I2, class S1,
                  class =_require_t<
                      bidirectional_iterator<I1>,
                      sentinel_for<S1, I1>,
                      bidirectional_iterator<I2>,
                      indirectly_copyable<I1, I2>>>
        constexpr ranges::in_out_result<I1, I2>
        operator()(I1 first, S1 last, I2 d_last) const
        {
            I1 last1{ranges::next(first, std::move(last))};
            for (I1 i{last1}; i != first;)
                *--d_last = *--i;
            return {std::move(last1), std::move(d_last)};
        }

        template <class I, class R,
                  class = _require_t<
                      bidirectional_range<R>,
                      bidirectional_iterator<I>,
                      indirectly_copyable<ranges::iterator_t<R>, I>>>
        constexpr ranges::in_out_result<ranges::borrowed_iterator_t<R>, I>
        operator()(R&& r, I d_last) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(d_last));
        }
    };

    struct _move
    {
        template <class I, class S, class O,
                  class = _require_t<input_iterator<I>, sentinel_for<S, I>, weakly_incrementable<O>, indirectly_movable<
                                         I, O>>>
        constexpr ranges::in_out_result<I, O>
        operator()(I first, S last, O result) const
        {
            for (; first != last; ++first, ++result)
                *result = ranges::iter_move(first);
            return {std::move(first), std::move(result)};
        }

        template <class R, class O,
                  class = _require_t<input_range<R>, weakly_incrementable<O>, indirectly_movable<
                                         ranges::iterator_t<R>, O>>>
        constexpr ranges::in_out_result<ranges::borrowed_iterator_t<R>, O>
        operator()(R&& r, O result) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(result));
        }
    };

    struct _move_backward
    {
        template <class I1, class S1, class I2,
                  class = _require_t<bidirectional_iterator<I1>, sentinel_for<S1, I1>,
                                     bidirectional_iterator<I2>, indirectly_movable<I1, I2>>>
        constexpr ranges::in_out_result<I1, I2>
        operator()(I1 first, S1 last, I2 d_last) const
        {
            auto i{last};
            for (; i != first; *--d_last = ranges::iter_move(--i))
            {}
            return {std::move(last), std::move(d_last)};
        }

        template <class R, class I,
                  class = _require_t<ranges::bidirectional_range<R>, bidirectional_iterator<I>,
                                     indirectly_movable<ranges::iterator_t<R>, I>>>
        constexpr ranges::in_out_result<ranges::borrowed_iterator_t<R>, I>
        operator()(R&& r, I d_last) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(d_last));
        }
    };

    struct _equal
    {
        template <class I1, class I2, class S1, class S2,

                  class Pred = ranges::equal_to,
                  class Proj1 = identity, class Proj2 = identity,
                  class = _require_t<input_iterator<I1>, sentinel_for<S1, I1>,
                                     input_iterator<I2>, sentinel_for<S2, I2>,
                                     indirectly_comparable<I1, I2, Pred, Proj1, Proj2>>>
        constexpr bool
        operator()(I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            if constexpr (sized_sentinel_for<S1, I1> && sized_sentinel_for<S2, I2>)
                if (ranges::distance(first1, last1) != ranges::distance(first2, last2))
                    return false;

            for (; first1 != last1; ++first1, (void)++first2)
                if (!ranges::invoke(pred, ranges::invoke(proj1, *first1), ranges::invoke(proj2, *first2)))
                    return false;
            return true;
        }

        template <class R1, class R2,
                  class Pred = ranges::equal_to,
                  class Proj1 = identity, class Proj2 = identity,
                  class = _require_t<input_range<R1>, ranges::input_range<R2>,
                                     indirectly_comparable<ranges::iterator_t<R1>, ranges::iterator_t<R2>,
                                                           Pred, Proj1, Proj2>>>
        constexpr bool
        operator()(R1&& r1, R2&& r2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            return (*this)(ranges::begin(r1), ranges::end(r1),
                           ranges::begin(r2), ranges::end(r2),
                           std::move(pred), std::move(proj1), std::move(proj2));
        }
    };
}

namespace
NAMESPACE_RANGES
{
    inline constexpr auto copy = Cpo::_copy{};
    inline constexpr auto copy_if = Cpo::_copy_if{};
    inline constexpr auto copy_n = Cpo::_copy_n{};
    inline constexpr auto copy_backward = Cpo::_copy_backward{};
    inline constexpr auto move_backward = Cpo::_move_backward{};
    inline constexpr auto move = Cpo::_move{};
    inline constexpr Cpo::_equal equal{};
}

namespace
NAMESPACE_RANGES::Cpo
{
    struct _fill_n
    {
        template <typename O, typename T = iter_value_t<O>, std::enable_if_t<
                      output_iterator<O, const T&>, int> = 0>
        constexpr O operator()(O first, iter_difference_t<O> n, const T& value) const
        {
            while (n != 0)
            {
                *first = value;
                ++first;
                --n;
            }
            return first;
        }
    };

    struct _fill
    {
        template <class O, class S, class T = iter_value_t<O>,
                  class = _require_t<
                      sentinel_for<S, O>,
                      output_iterator<O, const T&>>>
        constexpr O operator()(O first, S last, const T& value) const
        {
            // Must not use std::memset because it may be optimized away by the compiler
            while (first != last)
                *first++ = value;
            return first;
        }

        template <typename R, typename T = range_value_t<R>, std::enable_if_t<
                      output_range<R, const T&>, int> = 0>
        constexpr borrowed_iterator_t<R> operator()(R&& r, const T& value) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), value);
        }
    };
}

namespace
NAMESPACE_RANGES
{
    inline constexpr auto fill = Cpo::_fill{};
    inline constexpr auto fill_n = Cpo::_fill_n{};
}

namespace
NAMESPACE_RANGES
{
    namespace Cpo
    {
        struct _for_each
        {
            template <class Iter, class Sent, class Fun, class Proj = identity,
                      class = std::enable_if_t<sentinel_for<Sent, Iter> && indirectly_unary_invocable<
                          Fun, projected<Iter, Proj>>>>
            constexpr in_fun_result<Iter, Fun>
            operator()(Iter first, Sent last, Fun f, Proj proj = {}) const
            {
                for (; first != last; ++first)
                    invoke(f, invoke(proj, *first));
                return {std::move(first), std::move(f)};
            }

            template <class Range, class Fun, class Proj = identity,
                      class = std::enable_if_t<input_range<Range> && indirectly_unary_invocable<
                          Fun, projected<iterator_t<Range>, Proj>>>>
            constexpr in_fun_result<borrowed_iterator_t<Range>, Fun>
            operator()(Range&& r, Fun f, Proj proj = {}) const
            {
                return (*this)(ranges::begin(r), ranges::end(r), std::move(f), std::move(proj));
            }
        };
    }

    inline constexpr Cpo::_for_each for_each;

    namespace Cpo
    {
        struct _for_each_n
        {
            template <class Iter, class Fun, class Proj = identity>
            constexpr in_fun_result<Iter, Proj> operator()(Iter first, iter_difference_t<Iter> n, Fun fun,
                                                           Proj proj = {}) const
            {
                static_assert(indirectly_unary_invocable<Fun, projected<Iter, Proj>>);
                if constexpr (random_access_iterator<Iter>)
                {
                    if (n < 0)
                        return {std::move(first), std::move(fun)};
                    auto last = first + n;
                    return ranges::for_each(std::move(first), std::move(last), std::move(fun), std::move(proj));
                }
                else
                {
                    while (n-- > 0)
                    {
                        invoke(fun, invoke(proj, *first));
                        ++first;
                    }

                    return {std::move(first), std::move(fun)};
                }
            }
        };
    }

    inline constexpr Cpo::_for_each_n for_each_n;
}

namespace
NAMESPACE_RANGES
{
    struct _transform
    {
        // First version
        template <class I, class S, class O, class F, class Proj = identity,
                  class = _require_t<input_iterator<I>,
                                     sentinel_for<S, I>,
                                     weakly_incrementable<O>,
                                     copy_constructible<F>,
                                     indirectly_writable<O, indirect_result_t<F&, projected<I, Proj>>>>>
        constexpr in_out_result<I, O> operator()(I first1, S last1, O result, F op, Proj proj = {}) const
        {
            for (; first1 != last1; ++first1, (void)++result)
                *result = invoke(op, invoke(proj, *first1));

            return {std::move(first1), std::move(result)};
        }

        // Second version
        template <class R, class O, class F, class Proj = identity,
                  class = _require_t<ranges::input_range<R>,
                                     weakly_incrementable<O>,
                                     copy_constructible<F>, indirectly_writable<
                                         O, indirect_result_t<F&, projected<ranges::iterator_t<R>, Proj>>>>>
        constexpr in_out_result<borrowed_iterator_t<R>, O>
        operator()(R&& r, O result, F op, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(result), std::move(op), std::move(proj));
        }

        // Third version
        template <class S1, class S2, class I1, class I2, class F, class O, class Proj1 = identity, class Proj2 =
                  identity,
                  class = _require_t<
                      input_iterator<I1>,
                      sentinel_for<S1, I1>,
                      input_iterator<I2>,
                      sentinel_for<S2, I2>,
                      weakly_incrementable<O>,
                      copy_constructible<F>,
                      indirectly_writable<O, indirect_result_t<F&, projected<I1, Proj1>, projected<I2, Proj2>>>>>
        constexpr in_in_out_result<I1, I2, O> operator()(I1 first1, S1 last1, I2 first2, S2 last2, O result,
                                                         F binary_op, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            for (; first1 != last1 && first2 != last2; ++first1, (void)++first2, (void)++result)
                *result = invoke(binary_op, invoke(proj1, *first1), invoke(proj2, *first2));

            return {std::move(first1), std::move(first2), std::move(result)};
        }

        // Fourth version
        template <class R1, class R2, class O, class F, class Proj1 = identity, class Proj2 = identity,
                  class = _require_t<input_range<R1>, input_range<R2>, weakly_incrementable<O>, copy_constructible<F>,
                                     indirectly_writable<
                                         O, indirect_result_t<
                                             F&, projected<iterator_t<R1>, Proj1, projected<iterator_t<R2>, Proj2>>>>>>

        constexpr in_in_out_result<borrowed_iterator_t<R1>, borrowed_iterator_t<R2>, O>
        operator()(R1&& r1, R2&& r2, O result,
                   F binary_op, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            return (*this)(ranges::begin(r1), ranges::end(r1),
                           ranges::begin(r2), ranges::end(r2),
                           std::move(result), std::move(binary_op),
                           std::move(proj1), std::move(proj2));
        }
    };

    inline constexpr _transform transform{};
}

namespace
NAMESPACE_RANGES
{
    template <int _type>
    struct _choice_of
    {
        template <class I, class Sent, class Proj = identity, class Pred,
                  class = _require_t<input_iterator<I>, sentinel_for<Sent, I>, indirect_unary_predicate<
                                         Pred, projected<I, Proj>>>>
        constexpr bool operator()(I first, Sent last, Pred pred, Proj proj = {}) const
        {
            if constexpr (_type == 1)
            {
                for (; first != last; ++first)
                    if (!(bool)invoke(pred, invoke(proj, *first)))
                        return false;
                return true;
            }
            else if constexpr (_type == 2)
            {
                for (; first != last; ++first)
                    if (invoke(pred, invoke(proj, *first)))
                        return true;
                return false;
            }
            else
            {
                for (; first != last; ++first)
                    if (invoke(pred, invoke(proj, *first)))
                        return false;
                return true;
            }
        }

        template <class Range, class Pred, class Proj = identity,
                  class = _require_t<input_range<Range>, indirect_unary_predicate<
                                         Pred, projected<iterator_t<Range>, Proj>>>>
        constexpr bool operator()(Range&& r, Pred pred, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(pred), std::move(proj));
        }
    };

    inline constexpr _choice_of<1> all_of{};
    inline constexpr _choice_of<2> any_of{};
    inline constexpr _choice_of<3> none_of{};
}

namespace
NAMESPACE_RANGES
{
    struct _adjacent_find
    {
        template <class I, class S, class Proj = identity, class Pred = equal_to,
                  class = std::enable_if_t<forward_iterator<I>
                      && sentinel_for<S, I>
                      && indirect_binary_predicate<Pred, projected<I, Proj>, projected<I, Proj>>>>
        constexpr I operator()(I first, S last, Pred pred = Pred{}, Proj proj = Proj{}) const
        {
            if (first == last)
                return first;
            auto _n = ranges::next(first);
            for (; _n != last; ++_n, ++first)
                if (std::invoke(pred, std::invoke(proj, *first), std::invoke(proj, *_n)))
                    return first;
            return _n;
        }

        template <class R, class Proj = identity, class Pred = equal_to,
                  class =std::enable_if_t<forward_range<R> && indirect_binary_predicate<
                      Pred, projected<iterator_t<R>, Proj>, projected<iterator_t<R>, Proj>>>>
        constexpr borrowed_iterator_t<R> operator()(R&& r, Pred pred = Pred{}, Proj proj = Proj{}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::ref(pred), std::ref(proj));
        }
    };

    inline constexpr _adjacent_find adjacent_find;
}

namespace
NAMESPACE_RANGES
{
    struct _generate_n
    {
        template <class O, class F,REQUIRE(input_or_output_iterator<O>, copy_constructible<F>, invocable<F&>,
                                           indirectly_writable<O, std::invoke_result_t<F&>>)>
        constexpr O operator()(O first, iter_difference_t<O> n, F gen) const
        {
            for (; n-- > 0; *first = std::invoke(gen), ++first) {}
            return first;
        }
    };

    struct _generate
    {
        template <typename O, typename S, typename F,
                  class =std::enable_if_t<input_or_output_iterator<O> &&
                      sentinel_for<S, O> &&
                      copy_constructible<F> && invocable<F&> &&
                      indirectly_writable<O, std::invoke_result_t<F&>>>>
        constexpr O operator()(O first, S last, F gen) const
        {
            for (; first != last; *first = invoke(gen), ++first) {}
            return first;
        }

        template <class R, class F,REQUIRE(invocable<F&>, output_range<R, std::invoke_result_t<F&>>)>
        constexpr borrowed_iterator_t<R>
        operator()(R&& r, F gen) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(gen));
        }
    };

    inline constexpr auto generate = _generate{};
    inline constexpr auto generate_n = _generate_n{};
}

namespace
NAMESPACE_RANGES
{
    struct _shuffle
    {
        template <class I, class S, class Gen,
                  REQUIRE(
                      permutable<I>,
                      sentinel_for<S, I>,
                      uniform_random_bit_generator<std::remove_reference_t<Gen>>,
                      convertible_to<std::invoke_result_t<Gen&>, iter_difference_t<I>>)>
        constexpr I
        operator()(I first, S last, Gen&& gen) const
        {
            using diff_t = iter_difference_t<I>;
            using distr_t = std::uniform_int_distribution<diff_t>;
            using param_t = typename distr_t::param_type;
            distr_t D;
            const auto n{last - first};
            for (diff_t i{1}; i < n; ++i)
                ranges::iter_swap(first + i, first + D(gen, param_t(0, i)));
            return ranges::next(first, last);
        }

        template <class R, class Gen,
                  class = std::enable_if_t<random_access_range<R>
                      && permutable<iterator_t<R>>
                      && uniform_random_bit_generator<std::remove_reference_t<Gen>>>>
        constexpr borrowed_iterator_t<R> operator()(R&& r, Gen&& gen) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::forward<Gen>(gen));
        }
    };

    inline constexpr _shuffle shuffle{};
}

namespace
NAMESPACE_RANGES
{
    struct _replace
    {
        template <class I, class S, class Proj = identity, class T1 = projected_value_t<I, Proj>, class T2 = T1,
                  REQUIRE(input_iterator<I>,
                          sentinel_for<S, I>,
                          indirectly_writable<I, const T2&>,
                          indirect_binary_predicate<ranges::equal_to, projected<I, Proj>, const T1*>)>
        constexpr I operator()(I first, S last, const T1& old_value, const T2& new_value, Proj proj = {}) const
        {
            for (; first != last; ++first)
                if (old_value == invoke(proj, *first))
                    *first = new_value;
            return first;
        }

        template <class R, class Proj = identity,
                  class T1 = projected_value_t<iterator_t<R>, Proj>,
                  class T2 = T1,
                  REQUIRE(
                      input_range<R>,
                      indirectly_writable<iterator_t<R>, const T2&>,
                      indirect_binary_predicate<equal_to,
                      projected<iterator_t<R>, Proj>, const T1*>)>
        constexpr borrowed_iterator_t<R>
        operator()(R&& r, const T1& old_value,
                   const T2& new_value, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), old_value, new_value, std::move(proj));
        }
    };

    struct _replace_if
    {
        template <class I, class S, class Proj = identity, class Pred, class T = projected_value_t<I, Proj>,
                  REQUIRE(input_iterator<I>,
                          sentinel_for<S, I>,
                          indirectly_writable<I, const T&>,
                          indirect_unary_predicate<Pred, projected<I, Proj>>)>
        constexpr I operator()(I first, S last, Pred pred,
                               const T& new_value, Proj proj = {}) const
        {
            for (; first != last; ++first)
                if (!!invoke(pred, invoke(proj, *first)))
                    *first = new_value;
            return std::move(first);
        }

        template <class R, class Proj = identity,
                  class T = projected_value_t<iterator_t<R>, Proj>,
                  class Pred,
                  REQUIRE(
                      input_range<R>,
                      indirectly_writable<iterator_t<R>, const T&>,
                      indirect_unary_predicate<Pred, projected<iterator_t<R>, Proj>>)>
        constexpr borrowed_iterator_t<R>
        operator()(R&& r, Pred pred, const T& new_value, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(pred),
                           new_value, std::move(proj));
        }
    };

    struct _replace_copy_if
    {
        template <class I, class S, class O, class Proj = identity, class Pred, class T = iter_value_t<O>,
                  REQUIRE(input_iterator<I>,
                          sentinel_for<S, I>,
                          indirectly_writable<I, const T&>,
                          indirect_unary_predicate<Pred, projected<I, Proj>>,
                          indirectly_copyable<I, O> && output_iterator<O, const T&>)>
        constexpr in_out_result<I, O> operator()(I first, S last, O result, Pred pred,
                                                 const T& new_value, Proj proj = {}) const
        {
            for (; first != last; ++first, ++result)
                *result = invoke(pred, invoke(proj, *first)) ? new_value : *first;
            return {std::move(first), std::move(result)};
        }

        template <
            class R, class O, class Pred, class T = iter_value_t<O>,
            class Proj = identity,
            REQUIRE(input_range<R>,
                    indirect_unary_predicate<Pred, projected<iterator_t<R>, Proj>>,
                    indirectly_copyable<ranges::iterator_t<R>, O>,
                    output_iterator<O, const T&>)>
        constexpr in_out_result<borrowed_iterator_t<R>, O>
        operator()(R&& r, O result, Pred pred,
                   const T& new_value, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(result),
                           std::move(pred), new_value, std::move(proj));
        }
    };

    struct _replace_copy
    {
        template <class I, class S, class O, class Proj = identity,
                  class T1 = projected_value_t<I, Proj>,
                  class T2 = iter_value_t<O>,
                  class = std::enable_if_t<input_iterator<I>
                      && sentinel_for<S, I>
                      && indirectly_copyable<I, O>
                      && indirect_binary_predicate<equal_to, projected<I, Proj>, const T1*>
                      && output_iterator<O, const T2&>
                  >>
        constexpr in_out_result<I, O> operator()(I first, S last, O result, const T1& old_value, const T2& new_value,
                                                 Proj proj = {}) const
        {
            for (; first != last; ++first, ++result)
                *result = (invoke(proj, *first) == old_value) ? new_value : *first;
            return {std::move(first), std::move(result)};
        }

        template <class R, class O, class Proj =identity,
                  class T1 = projected_value_t<iterator_t<R>, Proj>,
                  class T2 = iter_value_t<O>,
                  REQUIRE(input_range<R>,
                          indirectly_copyable<iterator_t<R>, O>,
                          indirect_binary_predicate<equal_to, projected<iterator_t<R>, Proj>, const T1*>)>
        constexpr in_out_result<ranges::borrowed_iterator_t<R>, O>
        operator()(R&& r, O result, const T1& old_value, const T2& new_value, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(result), old_value, new_value, std::move(proj));
        }
    };

    struct rotate_copy_fn
    {
        template <class I, class S, class O,
                  REQUIRE(forward_iterator<I>, sentinel_for<S, I>, weakly_incrementable <O>
                          , indirectly_copyable<I, O>)>
        constexpr in_out_result<I, O> operator()(I first, I middle, S last, O result) const
        {
            auto c1{ranges::copy(middle, std::move(last), std::move(result))};
            auto c2{ranges::copy(std::move(first), std::move(middle), std::move(c1.out))};
            return {std::move(c1.in), std::move(c2.out)};
        }

        template <class R, class O,
                  REQUIRE(forward_range<R>,
                          weakly_incrementable<O>,
                          indirectly_copyable<ranges::iterator_t<R>, O>)>
        constexpr in_out_result<borrowed_iterator_t<R>, O> operator()(R&& r, ranges::iterator_t<R> middle,
                                                                      O result) const
        {
            return (*this)(ranges::begin(r), std::move(middle), ranges::end(r), std::move(result));
        }
    };

    struct _mismatch
    {
        template <class Iter1, class Iter2, class Sent1, class Sent2, class Pred = equal_to,
                  typename Proj1 = identity, typename Proj2 = identity,
                  REQUIRE(input_iterator<Iter1>, sentinel_for<Sent1, Iter1>,
                          input_iterator<Iter2>, sentinel_for<Sent2, Iter2>, indirectly_comparable<Iter1, Iter2, Pred,
                          Proj1, Proj2>)>
        constexpr in_in_result<Iter1, Iter2>
        operator()(Iter1 first1, Sent1 last1,
                   Iter2 first2, Sent2 last2, Pred pred = {},
                   Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            while (first1 != last1 && first2 != last2 && (bool)invoke(pred, invoke(proj1, *first1),
                                                                      invoke(proj2, *first2)))
            {
                ++first1;
                ++first2;
            }
            return {std::move(first1), std::move(first2)};
        }

        template <class R1, class R2,
                  class Pred = equal_to,
                  class Proj1 = identity, class Proj2 = identity,
                  REQUIRE(input_range<R1>, input_range<R2>, indirectly_comparable<iterator_t<R1>, iterator_t<R2>, Pred,
                          Proj1, Proj2>)>
        constexpr in_in_result<iterator_t<R1>, iterator_t<R2>>
        operator()(R1&& __r1, R2&& __r2, Pred __pred = {}, Proj1 __proj1 = {}, Proj2 __proj2 = {}) const
        {
            return (*this)(ranges::begin(__r1), ranges::end(__r1),
                           ranges::begin(__r2), ranges::end(__r2),
                           std::move(__pred),
                           std::move(__proj1), std::move(__proj2));
        }
    };

    inline constexpr auto mismatch = _mismatch{};
    inline constexpr _replace replace{};
    inline constexpr _replace_if replace_if{};
}

namespace
NAMESPACE_RANGES
{
    struct _reverse
    {
        template <class Iter, class Sent,
                  class = std::enable_if_t<bidirectional_iterator<Iter>
                      && sentinel_for<Sent, Iter>>>
        constexpr Iter
        operator()(Iter _first, Sent _last) const
        {
            auto _i = ranges::next(_first, _last);
            auto _tail = _i;

            if constexpr (random_access_iterator<Iter>)
            {
                if (_first != _last)
                {
                    --_tail;
                    while (_first < _tail)
                    {
                        ranges::iter_swap(_first, _tail);
                        ++_first;
                        --_tail;
                    }
                }
                return _i;
            }
            else
            {
                for (;;)
                    if (_first == _tail || _first == --_tail)
                        break;
                    else
                    {
                        ranges::iter_swap(_first, _tail);
                        ++_first;
                    }
                return _i;
            }
        }

        template <class Range,REQUIRE(bidirectional_range <Range>, permutable<iterator_t<Range>>)>
        constexpr borrowed_iterator_t<Range>
        operator()(Range&& r) const
        {
            return (*this)(ranges::begin(r), ranges::end(r));
        }
    };

    struct _reverse_copy
    {
        template <class Iter, class Sent, class Out,
                  REQUIRE(bidirectional_iterator <Iter>, sentinel_for<Sent, Iter>,
                          weakly_incrementable <Out>, indirectly_copyable<Iter, Out>)>
        constexpr in_out_result<Iter, Out>
        operator()(Iter first, Sent last, Out result) const
        {
            auto i = ranges::next(first, last);
            auto tail = i;
            while (first != tail)
            {
                --tail;
                *result = *tail;
                ++result;
            }
            return {i, std::move(result)};
        }

        template <class Range, class Out,
                  REQUIRE(bidirectional_range<Range>, weakly_incrementable <Out>
                          , indirectly_copyable<iterator_t<Range>, Out>)>
        constexpr in_out_result<borrowed_iterator_t<Range>, Out>
        operator()(Range&& r, Out result) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(result));
        }
    };

    inline constexpr _reverse reverse{};
    inline constexpr _reverse_copy reverse_copy{};
}

namespace
NAMESPACE_RANGES
{
    struct _push_heap
    {
        template <class I, class S, class Comp = less, class Proj = identity,
                  REQUIRE(sortable<I, Comp, Proj>, random_access_iterator<I>, sentinel_for<S, I>)>
        constexpr I operator()(I first, S last, Comp comp = {}, Proj proj = {}) const
        {
            const auto n{ranges::distance(first, last)};
            const auto length{n};
            if (n > 1)
            {
                I _last{first + n};
                n = (n - 2) / 2;
                I i{first + n};
                if (std::invoke(comp, std::invoke(proj, *i), std::invoke(proj, *--_last)))
                {
                    iter_value_t<I> v{ranges::iter_move(_last)};
                    do
                    {
                        *_last = ranges::iter_move(i);
                        _last = i;
                        if (n == 0)
                            break;
                        n = (n - 1) / 2;
                        i = first + n;
                    }
                    while (std::invoke(comp, std::invoke(proj, *i), std::invoke(proj, v)));
                    *_last = std::move(v);
                }
            }
            return first + length;
        }

        template <class Range, class Comp = less, class Proj = identity,REQUIRE(
                      random_access_range<Range>, sortable<iterator_t<Range>, Comp, Proj>)>
        constexpr borrowed_iterator_t<Range>
        operator()(Range&& r, Comp comp = {}, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(comp), std::move(proj));
        }
    };

    inline constexpr _push_heap push_heap{};

    struct _pop_heap
    {
        template <class I, class S, class Comp = less, class Proj = identity,
                  REQUIRE(sortable<I, Comp, Proj>, random_access_iterator<I>, sentinel_for<S, I>)>
        constexpr I operator()(I first, S last, Comp comp = {}, Proj proj = {}) const
        {}

        template <class Range, class Comp = less, class Proj = identity,
                  REQUIRE(random_access_range<Range>, sortable<iterator_t<Range>, Comp, Proj>)>
        constexpr borrowed_iterator_t<Range> operator()(Range&& r, Comp comp = {}, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(comp), std::move(proj));
        }
    };

    inline constexpr _pop_heap pop_heap{};

    struct _make_heap
    {
        template <class I, class S, class Comp = less, class Proj = identity,
                  REQUIRE(sortable<I, Comp, Proj>, random_access_iterator<I>, sentinel_for<S, I>)>
        constexpr I operator()(I first, S last, Comp comp = {}, Proj proj = {}) const
        {}

        template <class Range, class Comp = less, class Proj = identity,
                  REQUIRE(random_access_range<Range>, sortable<iterator_t<Range>, Comp, Proj>)>
        constexpr borrowed_iterator_t<Range> operator()(Range&& r, Comp comp = {}, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(comp), std::move(proj));
        }
    };

    inline constexpr _make_heap make_heap{};

    struct _sort_heap
    {
        template <class I, class S, class Comp = less, class Proj = identity,
                  REQUIRE(sortable<I, Comp, Proj>, random_access_iterator<I>, sentinel_for<S, I>)>
        constexpr I operator()(I first, S last, Comp comp = {}, Proj proj = {}) const
        {}

        template <class Range, class Comp = less, class Proj = identity,
                  REQUIRE(random_access_range<Range>, sortable<iterator_t<Range>, Comp, Proj>)>
        constexpr borrowed_iterator_t<Range> operator()(Range&& r, Comp comp = {}, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(comp), std::move(proj));
        }
    };

    inline constexpr _sort_heap sort_heap{};

    struct _is_heap_until
    {
        template <class Iter, class Sent, class Comp = less, class Proj = identity,
                  class = std::enable_if_t<random_access_iterator<Iter>
                      && sentinel_for<Sent, Iter>
                      && indirect_strict_weak_order<Comp, projected<Iter, Proj>>>>
        constexpr Iter operator()(Iter _first, Sent _last, Comp comp = {}, Proj proj = {}) const
        {
            iter_difference_t<Iter> n = ranges::distance(_first, _last);
            iter_difference_t<Iter> parent = 0, child = 1;
            for (; child < n; ++child)
                if (invoke(comp, invoke(proj, *(_first + parent)), invoke(proj, *(_first + child))))
                    return _first + child;
                else if ((child & 1) == 0)
                    ++parent;

            return _first + n;
        }

        template <class Range, class Proj = identity, class Comp = less,
                  class = std::enable_if_t<random_access_range<Range>
                      && indirect_strict_weak_order<Comp, projected<iterator_t<Range>, Proj>>>>
        constexpr borrowed_iterator_t<Range>
        operator()(Range&& _r, Comp comp = {}, Proj proj = {}) const
        {
            return (*this)(ranges::begin(_r), ranges::end(_r), std::move(comp), std::move(proj));
        }
    };

    inline constexpr _is_heap_until is_heap_until{};

    struct _is_heap
    {
        template <class Iter, class Sent, class Comp = less, class Proj = identity,
                  class = std::enable_if_t<random_access_iterator<Iter>
                      && sentinel_for<Sent, Iter>
                      && indirect_strict_weak_order<Comp, projected<Iter, Proj>>>>
        constexpr bool operator()(Iter first, Sent last, Comp comp = {}, Proj proj = {}) const
        {
            return (last == ranges::is_heap_until(first, last, std::move(comp), std::move(proj)));
        }

        template <class Range, class Proj = identity, class Comp = less,
                  class = std::enable_if_t<random_access_range<Range>
                      && indirect_strict_weak_order<Comp, projected<iterator_t<Range>, Proj>>>>
        constexpr bool operator()(Range&& _r, Comp comp = {}, Proj proj = {}) const
        {
            return (*this)(ranges::begin(_r), ranges::end(_r), std::move(comp), std::move(proj));
        }
    };

    inline constexpr _is_heap is_heap{};

    struct _sort
    {
        template <class I, class S, class Comp = less, class Proj = identity,
                  REQUIRE(sortable<I, Comp, Proj>, random_access_iterator<I>, sentinel_for<S, I>)>
        constexpr I operator()(I first, S last, Comp comp = {}, Proj proj = {}) const
        {}

        template <class Range, class Comp = less, class Proj = identity,
                  REQUIRE(random_access_range<Range>, sortable<iterator_t<Range>, Comp, Proj>)>
        constexpr borrowed_iterator_t<Range> operator()(Range&& r, Comp comp = {}, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(comp), std::move(proj));
        }
    };

    inline constexpr _sort sort{};

    struct _stable_sort
    {
        template <class I, class S, class Comp = less, class Proj = identity,
                  REQUIRE(sortable<I, Comp, Proj>, random_access_iterator<I>, sentinel_for<S, I>)>
        constexpr I operator()(I first, S last, Comp comp = {}, Proj proj = {}) const
        {}

        template <class Range, class Comp = less, class Proj = identity,
                  REQUIRE(random_access_range<Range>, sortable<iterator_t<Range>, Comp, Proj>)>
        constexpr borrowed_iterator_t<Range> operator()(Range&& r, Comp comp = {}, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(comp), std::move(proj));
        }
    };

    inline constexpr _stable_sort stable_sort{};

    struct _partial_sort
    {
        template <class I, class S, class Comp = less, class Proj = identity,
                  REQUIRE(sortable<I, Comp, Proj>, random_access_iterator<I>, sentinel_for<S, I>)>
        constexpr I operator()(I _first, I _middle, S _last, Comp _comp = {}, Proj _proj = {}) const
        {
            if (_first == _middle)
                return ranges::next(_first, _last);

            ranges::make_heap(_first, _middle, _comp, _proj);
            auto _i = _middle;
            for (; _i != _last; ++_i)
                if (invoke(_comp, invoke(_proj, *_i), invoke(_proj, *_first)))
                {
                    ranges::pop_heap(_first, _middle, _comp, _proj);
                    ranges::iter_swap(_middle - 1, _i);
                    ranges::push_heap(_first, _middle, _comp, _proj);
                }
            ranges::sort_heap(_first, _middle, _comp, _proj);

            return _i;
        }

        template <class Range, class Comp = less, class Proj = identity,
                  REQUIRE(random_access_range<Range>, sortable<iterator_t<Range>, Comp, Proj>)>
        constexpr borrowed_iterator_t<Range>
        operator()(Range&& r, iterator_t<Range> middle, Comp comp = {}, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), std::move(middle), ranges::end(r), std::move(comp), std::move(proj));
        }
    };

    inline constexpr _partial_sort partial_sort{};

    template <typename Iter, typename Out>
    using partial_sort_copy_result = in_out_result<Iter, Out>;

    struct _is_sorted_until
    {
        template <class Iter, class Sent, class Comp = less, class Proj = identity,
                  class = std::enable_if_t<forward_iterator<Iter>
                      && sentinel_for<Sent, Iter>
                      && indirect_strict_weak_order<Comp, projected<Iter, Proj>>>>
        constexpr Iter operator()(Iter _first, Sent _last, Comp _comp = {}, Proj _proj = {}) const
        {
            if (_first == _last)
                return _first;

            auto _next = _first;
            for (++_next; _next != _last; _first = _next, (void)++_next)
                if (invoke(_comp, invoke(_proj, *_next), invoke(_proj, *_first)))
                    return _next;
            return _next;
        }

        template <class Range, class Comp = less, class Proj = identity,
                  REQUIRE(forward_range<Range>, sortable<iterator_t<Range>, Comp, Proj>,
                          indirect_strict_weak_order<Comp, projected<iterator_t<Range>, Proj>>)>
        constexpr borrowed_iterator_t<Range> operator()(Range&& r, Comp comp = {}, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(comp), std::move(proj));
        }
    };

    inline constexpr _is_sorted_until is_sorted_until{};

    struct _is_sorted
    {
        template <class Iter, class Sent, class Comp = less, class Proj = identity,
                  class = std::enable_if_t<forward_iterator<Iter>
                      && sentinel_for<Sent, Iter>
                      && indirect_strict_weak_order<Comp, projected<Iter, Proj>>>>
        constexpr bool operator()(Iter _first, Sent _last, Comp _comp = {}, Proj _proj = {}) const
        {
            if (_first == _last)
                return true;

            auto _next = _first;
            for (++_next; _next != _last; _first = _next, (void)++_next)
                if (invoke(_comp, invoke(_proj, *_next), invoke(_proj, *_first)))
                    return false;
            return true;
        }

        template <class Range, class Comp = less, class Proj = identity,
                  REQUIRE(forward_range<Range>, sortable<iterator_t<Range>, Comp, Proj>,
                          indirect_strict_weak_order<Comp, projected<iterator_t<Range>, Proj>>)>
        constexpr borrowed_iterator_t<Range> operator()(Range&& r, Comp comp = {}, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(comp), std::move(proj));
        }
    };

    inline constexpr _is_sorted is_sorted{};

    struct _nth_element
    {
        template <class I, class S, class Comp = less, class Proj = identity,
                  REQUIRE(sortable<I, Comp, Proj>, random_access_iterator<I>, sentinel_for<S, I>)>
        constexpr I
        operator()(I first, I nth, S last, Comp comp = {}, Proj proj = {}) const
        {}

        template <class Range, class Comp = less, class Proj = identity,
                  REQUIRE(random_access_range<Range>, sortable<iterator_t<Range>, Comp, Proj>)>
        constexpr borrowed_iterator_t<Range>
        operator()(Range&& r, iterator_t<Range> middle, Comp comp = {}, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), std::move(middle), ranges::end(r), std::move(comp), std::move(proj));
        }
    };

    inline constexpr _nth_element nth_element{};

    struct _lower_bound
    {
        template <class Iter, class S, class T, class Proj = identity, class Comp = less,
                  REQUIRE(forward_iterator <Iter>, sentinel_for<S, Iter>, indirect_strict_weak_order<const T*,
                          projected<Iter, Proj>>)>
        constexpr Iter operator()(Iter _first, S _last, const T& _value, Comp _comp = {}, Proj _proj = {}) const
        {
            auto _len = ranges::distance(_first, _last);

            while (_len > 0)
            {
                auto _half = _len / 2;
                auto _middle = _first;
                ranges::advance(_middle, _half);
                if (invoke(_comp, invoke(_proj, *_middle), _value))
                {
                    _first = _middle;
                    ++_first;
                    _len = _len - _half - 1;
                }
                else
                    _len = _half;
            }
            return _first;
        }

        template <class Range, class T, class Proj = identity, class Comp = less,REQUIRE(forward_range<Range>,
                      indirect_strict_weak_order<Comp, const T*, projected<iterator_t<Range>, Proj>>)>
        constexpr borrowed_iterator_t<Range>
        operator()(Range&& r, const T& value, Comp comp = {}, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), value, std::move(comp), std::move(proj));
        }
    };

    inline constexpr _lower_bound lower_bound{};

    struct _upper_bound
    {
        template <class Iter, class S, class T, class Proj = identity, class Comp = less,
                  REQUIRE(forward_iterator <Iter>, sentinel_for<S, Iter>, indirect_strict_weak_order<const T*,
                          projected<Iter, Proj>>)>
        constexpr Iter operator()(Iter first, S last, const T& value, Comp comp = {}, Proj proj = {}) const
        {
            auto len = ranges::distance(first, last);

            while (len > 0)
            {
                auto half = len / 2;
                auto middle = first;
                ranges::advance(middle, half);
                if (invoke(comp, value, invoke(proj, *middle)))
                    len = half;
                else
                {
                    first = middle;
                    ++first;
                    len = len - half - 1;
                }
            }
            return first;
        }

        template <class Range, class T, class Proj = identity, class Comp = less,REQUIRE(forward_range<Range>,
                      indirect_strict_weak_order<Comp, const T*, projected<iterator_t<Range>, Proj>>)>
        constexpr borrowed_iterator_t<Range>
        operator()(Range&& r, const T& value, Comp comp = {}, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), value, std::move(comp), std::move(proj));
        }
    };

    inline constexpr _upper_bound upper_bound{};

    struct _binary_search
    {
        template <class Iter, class S, class T, class Proj = identity, class Comp = less,
                  REQUIRE(forward_iterator <Iter>, sentinel_for<S, Iter>, indirect_strict_weak_order<const T*,
                          projected<Iter, Proj>>)>
        constexpr bool operator()(Iter first, S last, const T& value, Comp comp = {}, Proj proj = {}) const
        {
            auto pos = ranges::lower_bound(first, last, value, comp, proj);
            if (pos == last)
                return false;
            return !(bool)invoke(comp, value, invoke(proj, *pos));
        }

        template <class Range, class T, class Proj = identity, class Comp = less,REQUIRE(forward_range<Range>,
                      indirect_strict_weak_order<Comp, const T*, projected<iterator_t<Range>, Proj>>)>
        constexpr bool operator()(Range&& r, const T& value, Comp comp = {}, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), value, std::move(comp), std::move(proj));
        }
    };

    inline constexpr _binary_search binary_search{};

    struct _is_partitioned
    {
        template <class I, class S, class Pred, class Proj = identity,REQUIRE(input_iterator<I>, sentinel_for<S, I>,
                                                                              indirect_unary_predicate<Pred,
                                                                              projected<I, Proj>>)>
        constexpr bool operator()(I first, S last, Pred pred, Proj proj = {}) const
        {
            for (; first != last; ++first)
                if (!invoke(pred, invoke(proj, *first)))
                    break;

            for (; first != last; ++first)
                if (invoke(pred, invoke(proj, *first)))
                    return false;
            return true;
        }

        template <class R, class Proj = identity, class Pred,
                  REQUIRE(input_range<R>, indirect_unary_predicate<Pred, projected<iterator_t<R>, Proj>>)>
        constexpr bool operator()(R&& r, Pred pred, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(pred), std::move(proj));
        }
    };

    //判断满足条件一元谓词的数据是否排在未满足条件的前面
    inline constexpr _is_partitioned is_partitioned{};
}

namespace
NAMESPACE_RANGES
{
    template <class T>
    constexpr void* _voidify(T& obj) noexcept
    {
        return const_cast<void*>
            (static_cast<const volatile void*>(std::addressof(obj)));
    }

    template <class I, class = void>
    struct _nothrow_input_iterator
        : std::false_type
    {};

    template <class I>
    struct _nothrow_input_iterator<I,
                                   std::enable_if_t<input_iterator<I>
                                       && std::is_lvalue_reference_v<iter_reference_t<I>>
                                       && same_as<remove_cvref_t<iter_reference_t<I>>, iter_value_t<I>>>>
        : std::true_type
    {};

    template <class I, class = void>
    struct _nothrow_forward_iterator
        : std::false_type
    {};

    template <class I>
    struct _nothrow_forward_iterator<I,
                                     std::enable_if_t<_nothrow_input_iterator<I>::value
                                         && forward_iterator<I>
                                         && sentinel_for<I, I>>>
        : std::true_type
    {};

    template <class I, class = void>
    struct _nothrow_input_range
        : std::false_type
    {};

    template <class I>
    struct _nothrow_input_range<I,
                                std::enable_if_t<ranges::range<I>
                                    && _nothrow_input_iterator<ranges::iterator_t<I>>::value
                                    && sentinel_for<ranges::sentinel_t<I>, ranges::iterator_t<I>>>>
        : std::true_type
    {};

    template <class I, class = void>
    struct _nothrow_forward_range
        : std::false_type
    {};

    template <class I>
    struct _nothrow_forward_range<I,
                                  std::enable_if_t<_nothrow_input_range<I>::value
                                      && _nothrow_forward_iterator<I>::value>>
        : std::true_type
    {};

    template <class I>
    inline constexpr bool _nothrow_input_iterator_v = _nothrow_input_iterator<I>::value;

    template <class I>
    inline constexpr bool _nothrow_forward_iterator_v = _nothrow_forward_iterator<I>::value;

    template <class I>
    inline constexpr bool _nothrow_input_range_v = _nothrow_input_range<I>::value;

    template <class I>
    inline constexpr bool _nothrow_forward_range_v = _nothrow_forward_range<I>::value;
}

namespace
NAMESPACE_RANGES
{
    template <class I, class O>
    using uninitialized_copy_result = ranges::in_out_result<I, O>;

    template <class I, class O>
    using uninitialized_copy_n_result = ranges::in_out_result<I, O>;

    template <class I, class O>
    using uninitialized_move_result = ranges::in_out_result<I, O>;

    template <class I, class O>
    using uninitialized_move_n_result = ranges::in_out_result<I, O>;
}

namespace
NAMESPACE_RANGES
{
    namespace Cpo
    {
        struct _destroy_at
        {
            template <class T, class = std::enable_if_t<destructible<T>>>
            constexpr void operator()(T* p) const noexcept
            {
                if constexpr (std::is_array_v<T>)
                    for (auto& elem : *p)
                        operator()(std::addressof(elem));
                else
                    p->~T();
            }
        };
    }

    inline constexpr Cpo::_destroy_at destroy_at{};

    namespace Cpo
    {
        struct _destroy
        {
            template <class I, class S,
                      class = _require_t<
                          destructible<iter_value_t<I>>,
                          _nothrow_input_iterator<I>::value,
                          sentinel_for<S, I>>>
             I operator()(I first, S last) const noexcept
            {
                if constexpr (std::is_trivially_destructible_v<iter_value_t<I>>)
                    return ranges::next(std::move(first), last);
                else
                {
                    for (; first != last; ++first)
                        ranges::destroy_at(std::addressof(*first));
                    return first;
                }
            }

            template <class R,
                      class = _require_t<_nothrow_input_range_v<R>, destructible<range_value_t<R>>>>
            ranges::borrowed_iterator_t<R> operator()(R&& r) const noexcept
            {
                return operator()(ranges::begin(r), ranges::end(r));
            }
        };
    }

    inline constexpr Cpo::_destroy destroy{};

    template <class Iter, bool = std::is_trivially_destructible_v<iter_value_t<Iter>>>
    struct _destroy_guard
    {
        static_assert(destructible<iter_value_t<Iter>>);
    private:
        Iter _first;
        const Iter* _cur;

    public:
        explicit _destroy_guard(const Iter& __iter)
            : _first(__iter), _cur(std::__addressof(__iter))
        {}

        void release() noexcept { _cur = nullptr; }

        ~_destroy_guard()
        {
            if (_cur != nullptr)
                ranges::destroy(std::move(_first), *_cur);
        }
    };

    template <class Iter>
    struct _destroy_guard<Iter, true>
    {
        explicit _destroy_guard(const Iter&) {}

        void release() noexcept {}
    };

    namespace Cpo
    {
        struct _uninitialized_default_construct
        {
            template <class I, class S>
            I operator()(I first, S last) const
            {
                static_assert(_nothrow_forward_iterator<I>::value
                    && sentinel_for<S, I> && default_initializable<iter_value_t<I>>);
                using ValueType = std::remove_reference_t<iter_reference_t<I>>;
                if constexpr (std::is_trivially_default_constructible_v<ValueType>)
                    return ranges::next(first, last); // skip initialization
                I rollback{first};
                try
                {
                    for (; !(first == last); ++first)
                        ::new(const_cast<void*>(static_cast<const volatile void*>
                            (std::addressof(*first)))) ValueType;
                    return first;
                }
                catch (...) // rollback: destroy constructed elements
                {
                    for (; rollback != first; ++rollback)
                        ranges::destroy_at(std::addressof(*rollback));
                    throw;
                }
            }

            template <class R,
                      class = std::enable_if_t<_nothrow_forward_range<R>::value &&
                          default_initializable<ranges::range_value_t<R>>>>
            ranges::borrowed_iterator_t<R>
            operator()(R&& r) const
            {
                return (*this)(ranges::begin(r), ranges::end(r));
            }
        };
    }

    inline constexpr Cpo::_uninitialized_default_construct uninitialized_default_construct{};

    namespace Cpo
    {
        struct _destroy_n
        {
            template <class I, class = _require_t<destructible<iter_value_t<I>>, _nothrow_input_iterator<I>::value>>
            I operator()(I first, iter_difference_t<I> n) const noexcept
            {
                if constexpr (std::is_trivially_destructible_v<iter_value_t<I>>)
                    return ranges::next(std::move(first), n);
                else
                {
                    for (; n > 0; ++first, (void)--n)
                        ranges::destroy_at(std::__addressof(*first));
                    return first;
                }
            }
        };
    }

    inline constexpr Cpo::_destroy_n destroy_n{};

    namespace Cpo
    {
        struct _uninitialized_copy
        {
            template <class I, class S1, class S2, class O,
                      class = _require_t<
                          constructible_from<iter_value_t<O>, iter_reference_t<I>>,
                          input_iterator<I>,
                          sentinel_for<S1, I>,
                          _nothrow_forward_iterator<O>::value,
                          sentinel_for<S2, O>>>
            uninitialized_copy_result<I, O> operator()(I ifirst, S1 ilast, O ofirst, S2 olast) const
            {
                using OutType = std::remove_reference_t<iter_reference_t<O>>;
                if constexpr (sized_sentinel_for<S1, I>
                    && sized_sentinel_for<S2, O>
                    && std::is_trivial_v<OutType>
                    && std::is_nothrow_assignable_v<OutType&, iter_reference_t<I>>)
                {
                    auto d1 = ilast - ifirst;
                    auto d2 = olast - ofirst;
                    return ranges::copy_n(ranges::move(ifirst), std::min(d1, d2), ofirst);
                }
                else
                {
                    auto guard = _destroy_guard(ofirst);
                    for (; ifirst != ilast && ofirst != olast; ++ofirst, (void)++ifirst)
                        ::new(_voidify(*ofirst)) OutType(*ifirst);
                    guard.release();
                    return {std::move(ifirst), ofirst};
                }
            }

            template <class IR, class OR,
                      class = _require_t<ranges::input_range<IR>,
                                         _nothrow_forward_range<OR>::value,
                                         constructible_from<ranges::range_value_t<OR>,
                                                            ranges::range_reference_t<IR>>>>
            ranges::uninitialized_copy_result<ranges::borrowed_iterator_t<IR>,
                                              ranges::borrowed_iterator_t<OR>>
            operator()(IR&& in_range, OR&& out_range) const
            {
                return (*this)(ranges::begin(in_range), ranges::end(in_range),
                               ranges::begin(out_range), ranges::end(out_range));
            }
        };

        struct _uninitialized_copy_n
        {
            template <class I, class O, class S,
                      class = _require_t<
                          input_iterator<I>,
                          _nothrow_input_iterator<O>::value,
                          sentinel_for<S, O>,
                          constructible_from<iter_value_t<O>, iter_reference_t<I>>
                      >>
            ranges::uninitialized_copy_n_result<I, O> operator()(I ifirst, iter_difference_t<I> n, O ofirst,
                                                                 S olast) const
            {
                using OutType = std::remove_reference_t<iter_value_t<O>>;
                if constexpr (std::is_trivial_v<OutType> && sentinel_for<S, O>)
                {
                    auto d = olast - ofirst;
                    return ranges::copy_n(std::move(ifirst), std::min(n, d), ofirst);
                }
                else
                {
                    auto guard = _destroy_guard(ofirst);
                    for (; n > 0 && ofirst != olast; ++ofirst, ++ifirst, --n)
                    {
                        ::new(_voidify(*ofirst))OutType(*ifirst);
                    }
                    guard.release();
                    return {std::move(ifirst), ofirst};
                }
            }
        };
    }

    inline constexpr auto uninitialized_copy = Cpo::_uninitialized_copy{};
    inline constexpr auto uninitialized_copy_n = Cpo::_uninitialized_copy_n{};
}

namespace
NAMESPACE_RANGES
{
    struct _uninitialized_fill
    {
        template <class I, class S, class T,
                  class = std::enable_if_t<_nothrow_forward_iterator<I>::value
                      && sentinel_for<S, I>
                      && constructible_from<iter_value_t<I>, const T&>>>
        I operator()(I first, S last, const T& x) const
        {
            using _ValueType = std::remove_reference_t<iter_reference_t<I>>;
            if constexpr (std::is_trivial_v<_ValueType> && std::is_nothrow_assignable_v<_ValueType&, const T&>)
                return ranges::fill(first, last, x);
            else
            {
                auto guard = _destroy_guard(first);
                for (; first != last; ++first)
                    ::new(_voidify(*first)) _ValueType(x);
                guard.release();
                return first;
            }
        }

        template <class R, class T, class = std::enable_if_t<_nothrow_forward_range<R>::value && constructible_from<
                      ranges::range_value_t<R>, const T&>>>
        constexpr ranges::borrowed_iterator_t<R> operator()(R&& r, const T& value) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), value);
        }
    };

    struct _uninitialized_fill_n
    {
        template <class I, class T, class = std::enable_if_t<_nothrow_forward_range<I>::value && constructible_from<
                      iter_value_t<I>, const T&>>>
        I operator()(I first, iter_difference_t<I> n, const T& value) const
        {
            using _ValueType = std::remove_reference_t<iter_reference_t<I>>;
            if constexpr (std::is_trivial_v<_ValueType> && std::is_nothrow_assignable_v<_ValueType&, const T&>)
                return ranges::fill_n(first, n, value);
            else
            {
                auto guard = __destroy_guard(first);
                for (; n > 0; ++first, (void)--n)
                    ::new(_voidify(*first)) _ValueType(value);
                guard.release();
                return first;
            }
        }
    };

    struct _uninitialized_move
    {
        template <class I, class S1,
                  class O, class S2,
                  class = _require_t<input_iterator<I>,
                                     sentinel_for<S1, I>,
                                     _nothrow_forward_iterator<O>::value, sentinel_for<S2, O>,
                                     constructible_from<iter_value_t<O>,
                                                        iter_rvalue_reference_t<I>>>>
        ranges::uninitialized_move_result<I, O>
        operator()(I ifirst, S1 ilast, O ofirst, S2 olast) const
        {
            using ValueType = std::remove_reference_t<iter_reference_t<O>>;
            if constexpr (sized_sentinel_for<S1, I>
                && sized_sentinel_for<S2, O>
                && std::is_trivial_v<ValueType>
                && std::is_nothrow_assignable_v<ValueType&, iter_rvalue_reference_t<I>>)
            {
                auto d1 = ilast - ifirst;
                auto d2 = olast - ofirst;
                auto [in, out]
                    = ranges::copy_n(std::make_move_iterator(std::move(ifirst)), std::min(d1, d2), ofirst);
                return {std::move(in).base(), out};
            }
            O current{ofirst};
            try
            {
                for (; ifirst != ilast && ofirst != olast; ++ofirst, (void)++ifirst)
                    ::new(voidify(*ofirst))
                        std::remove_reference_t<iter_reference_t<O>>(ranges::iter_move(ifirst));
                return {std::move(ifirst), ofirst};
            }
            catch (...) // 回滚：析构已构造的元素
            {
                for (; ofirst != current; ++ofirst)
                    ranges::destroy_at(std::addressof(*ofirst));
                throw;
            }
        }

        template <class IR, class OR,
                  class = _require_t<constructible_from<ranges::range_value_t<OR>, ranges::range_rvalue_reference_t<IR>>
                                     ,
                                     _nothrow_forward_range_v<OR>, input_range<IR>>>
        ranges::uninitialized_move_result<ranges::borrowed_iterator_t<IR>,
                                          ranges::borrowed_iterator_t<OR>>
        operator()(IR&& in_range, OR&& out_range) const
        {
            return (*this)(ranges::begin(in_range), ranges::end(in_range), ranges::begin(out_range),
                           ranges::end(out_range));
        }
    };

    struct _uninitialized_move_n
    {
        template <class I, class O, class S, class = _require_t<input_iterator<I>,
                                                                _nothrow_forward_iterator_v<O>, sentinel_for<S, O>,
                                                                constructible_from<
                                                                    iter_value_t<O>, iter_rvalue_reference_t<I>>>>
        ranges::uninitialized_move_n_result<I, O>
        operator()(I ifirst, iter_difference_t<I> n, O ofirst, S olast) const
        {
            using OutType = std::remove_reference_t<iter_reference_t<O>>;
            if constexpr (sized_sentinel_for<S, O>
                && std::is_trivial_v<OutType>
                && std::is_nothrow_assignable_v<OutType&, iter_rvalue_reference_t<I>>)
            {
                auto d = olast - ofirst;
                auto [in, out]
                    = ranges::copy_n(std::make_move_iterator(std::move(ifirst)), std::min(count, d), ofirst);
                return {std::move(in).base(), out};
            }
            else
            {
                auto guard = _destroy_guard(ofirst);
                for (; n > 0 && ofirst != olast; ++ofirst, (void)++ifirst, (void)--n)
                    ::new(_voidify(*ofirst))OutType(ranges::iter_move(ifirst));
                guard.release();
                return {std::move(ifirst), ofirst};
            }
        }
    };

    struct _uninitialized_default_construct_n
    {
        template <class Iter,
            class = std::enable_if_t<_nothrow_forward_iterator_v<Iter>&&default_initializable<iter_value_t<Iter>>>>
        Iter operator()(Iter first, iter_difference_t<Iter> n) const
        {
            using ValueType = std::remove_reference_t<iter_reference_t<Iter>>;
            if constexpr (std::is_trivially_default_constructible_v<ValueType>)
                return ranges::next(first, n);
            else
            {
                auto guard = _destroy_guard(first);
                for (; n > 0; ++first, (void) --n)
                    ::new (_voidify(*first)) ValueType;
                guard.release();
                return first;
            }
        }
    };

    struct _uninitialized_value_construct
    {
        template <class I, class S,
            REQUIRE(default_initializable<iter_value_t<I>>,sentinel_for<S,I>,_nothrow_forward_iterator_v<I>)>
        I operator()(I first, S last) const
        {
            using ValueType = std::remove_reference_t<iter_reference_t<I>>;
            if constexpr (std::is_trivially_default_constructible_v<ValueType>)
                return ranges::fill(first, last, ValueType());
            auto guard = __destroy_guard(first);
            for (; first != last; ++first)
                ::new (_voidify(*first)) ValueType();
            guard.release();
            return first;
        }

        template <class R,std::enable_if_t<_nothrow_forward_range_v<R>
        && default_initializable<ranges::range_value_t<R>>,int> =0>
        ranges::borrowed_iterator_t<R> operator()(R&& r) const
        {
            //static_assert(_nothrow_forward_range_v<R> && default_initializable<ranges::range_value_t<R>>);
            return (*this)(ranges::begin(r), ranges::end(r));
        }
    };

    struct _uninitialized_value_construct_n
    {
        template <class I,
            std::enable_if_t<_nothrow_forward_iterator_v<I> && default_initializable<iter_value_t<I>>,bool> =false>
        I operator()(I first, iter_difference_t<I>n) const
        {
            using ValueType = std::remove_reference_t<iter_reference_t<I>>;
            if constexpr (std::is_trivial_v<ValueType>&& std::is_copy_assignable_v<ValueType>)
                return ranges::fill_n(first, n, ValueType());
            auto guard = __destroy_guard(first);
            for (; n > 0; ++first, (void) --n)
                ::new (_voidify(*first)) ValueType();
            guard.release();
            return first;
        }
    };
}

namespace
NAMESPACE_RANGES
{
    inline constexpr _uninitialized_fill uninitialized_fill{};
    inline constexpr _uninitialized_fill_n uninitialized_fill_n{};
    inline constexpr _uninitialized_move uninitialized_move{};
    inline constexpr _uninitialized_move_n uninitialized_move_n{};
    inline constexpr _uninitialized_default_construct_n uninitialized_default_construct_n;
    inline constexpr _uninitialized_value_construct uninitialized_value_construct;
    inline constexpr _uninitialized_value_construct_n uninitialized_value_construct_n;
}

namespace
NAMESPACE_RANGES
{
    template <class Comp, class Proj>
    constexpr auto _make_comp_proj(Comp& comp, Proj& proj)
    {
        return [&](auto&& lhs, auto&& rhs) -> bool
        {
            return ranges::invoke(comp, ranges::invoke(proj, std::forward<decltype(lhs)>(lhs)),
                                  ranges::invoke(proj, std::forward<decltype(rhs)>(rhs)));
        };
    }

    struct _mxx_element
    {
        template <class I, class S, class Proj = identity, class Comp = ranges::less,
                  class = _require_t<
                      forward_iterator<I>,
                      sentinel_for<S, I>,
                      indirect_strict_weak_order<Comp, projected<I, Proj>>
                  >>
        constexpr I operator()(I first, S last, Comp comp = {}, Proj proj = {}) const
        {
            if (first == last)
                return last;

            auto it = first;
            while (++first != last)
                if (ranges::invoke(comp, ranges::invoke(proj, *it), ranges::invoke(proj, *first)))
                    it = first;
            return it;
        }

        template <class R, class Proj = identity, class Comp = ranges::less,
                  class = _require_t<
                      forward_range<R>,
                      indirect_strict_weak_order<Comp, projected<ranges::iterator_t<R>, Proj>>
                  >>
        constexpr borrowed_iterator_t<R>
        operator()(R&& r, Comp comp = {}, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(comp), std::move(proj));
        }
    };

    struct _minmax_element
    {
        template <class I, class S, class Proj = identity, class Comp = ranges::less,
                  class = _require_t<
                      forward_iterator<I>,
                      sentinel_for<S, I>,
                      indirect_strict_weak_order<Comp, projected<I, Proj>>
                  >>
        constexpr ranges::min_max_result<I>
        operator()(I first, S last, Comp comp = {}, Proj proj = {}) const
        {
            auto min = first, max = first;

            if (first == last || ++first == last)
                return {min, max};

            if (ranges::invoke(comp, invoke(proj, *first), invoke(proj, *min)))
                min = first;
            else
                max = first;

            while (++first != last)
            {
                auto i = first;
                if (++first == last)
                {
                    if (invoke(comp, invoke(proj, *i), invoke(proj, *min)))
                        min = i;
                    else if (!(invoke(comp, invoke(proj, *i), invoke(proj, *max))))
                        max = i;
                    break;
                }
                else
                {
                    if (invoke(comp, invoke(proj, *first), invoke(proj, *i)))
                    {
                        if (invoke(comp, invoke(proj, *first), invoke(proj, *min)))
                            min = first;
                        if (!(invoke(comp, invoke(proj, *i), invoke(proj, *max))))
                            max = i;
                    }
                    else
                    {
                        if (invoke(comp, invoke(proj, *i), invoke(proj, *min)))
                            min = i;
                        if (!(invoke(comp, invoke(proj, *first), invoke(proj, *max))))
                            max = first;
                    }
                }
            }
            return {min, max};
        }

        template <class R, class Proj = identity, class Comp = ranges::less,
                  class = _require_t<
                      forward_range<R>,
                      indirect_strict_weak_order<Comp, projected<ranges::iterator_t<R>, Proj>>
                  >>
        constexpr ranges::min_max_result<ranges::borrowed_iterator_t<R>>
        operator()(R&& r, Comp comp = {}, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(comp), std::move(proj));
        }
    };

    template <typename Tp>
    using minmax_result = min_max_result<Tp>;

    inline constexpr auto max_element = _mxx_element{};
    inline constexpr auto min_element = _mxx_element{};
    inline constexpr auto minmax_element = _minmax_element{};

    struct _minmax
    {
        template <class T, class Comp = less, class Proj = identity,
                  class = std::enable_if_t<indirect_strict_weak_order<Comp, projected<const T*, Proj>>>>
        constexpr minmax_result<const T&>
        operator()(const T& a, const T& b, Comp comp = {}, Proj proj = {}) const
        {
            if (ranges::invoke(comp, ranges::invoke(proj, b), ranges::invoke(proj, a)))
                return {b, a};
            return {a.b};
        }

        template <class Range, class Comp = ranges::less, class Proj = identity,
                  class = _require_t<
                      indirectly_copyable_storable<iterator_t<Range>, range_value_t<Range>*>,
                      indirect_strict_weak_order<Comp, projected<iterator_t<Range>, Proj>>>>
        constexpr minmax_result<range_value_t<Range>>
        operator()(Range&& r, Comp comp = {}, Proj proj = {}) const
        {
            auto result = ranges::minmax_element(r, std::ref(comp), std::ref(proj));
            return {*result.min, *result.max};
        }

        template <class T, class Comp = ranges::less, class Proj = identity,
                  class = _require_t<
                      copyable<T>,
                      indirect_strict_weak_order<Comp, projected<const T*, Proj>>>>
        constexpr minmax_result<T>
        operator()(std::initializer_list<T> r, Comp comp = {}, Proj proj = {}) const
        {
            auto result = ranges::minmax_element(r, std::ref(comp), std::ref(proj));
            return {std::move(*result.min), std::move(*result.max)};
        }
    };

    struct _min
    {
    public:
        template <class T, class Proj = identity, class Comp = less,
                  class = std::enable_if_t<
                      indirect_strict_weak_order<Comp, projected<const T*, Proj>>>>
        constexpr const T& operator()(const T& a, const T& b, Comp comp = {}, Proj proj = {}) const
        {
            return ranges::invoke(comp, ranges::invoke(proj, a), ranges::invoke(proj, b)) ? a : b;
        }

        template <typename T, typename Proj = identity, typename Comp = less,
                  class = _require_t<copyable<T>, indirect_strict_weak_order<Comp, projected<const T*, Proj>>>>
        constexpr T operator()(std::initializer_list<T> r, Comp comp = {}, Proj proj = {}) const
        {
            return *ranges::min_element(r, std::move(comp), std::move(proj));
        }

        template <class R, class Proj = identity, class Comp = less,
                  class = std::enable_if_t<indirect_strict_weak_order<Comp,
                                                                      projected<ranges::iterator_t<R>, Proj>> &&
                      indirectly_copyable_storable<ranges::iterator_t<R>,
                                                   ranges::range_value_t<R>*>>>
        constexpr range_value_t<R> operator()(R&& r, Comp comp = {}, Proj proj = {}) const
        {
            if constexpr (ranges::forward_range<R>)
                return
                    static_cast<range_value_t<R>>(*ranges::min_element(r, std::move(comp), std::move(proj)));
            else
            {
                auto i = ranges::begin(r);
                auto s = ranges::end(r);
                ranges::range_value_t<R> m(*i);
                while (++i != s)
                    if (ranges::invoke(comp, ranges::invoke(proj, *i), ranges::invoke(proj, m)))
                        m = *i;
                return m;
            }
        }
    };

    struct _max
    {
    public:
        template <class T, class Proj = identity, class Comp = less,
                  class = std::enable_if_t<
                      indirect_strict_weak_order<Comp, projected<const T*, Proj>>>>
        constexpr const T& operator()(const T& a, const T& b, Comp comp = {}, Proj proj = {}) const
        {
            return ranges::invoke(comp, ranges::invoke(proj, a), ranges::invoke(proj, b)) ? b : a;
        }

        template <typename T, typename Proj = identity, typename Comp = less,
                  class = _require_t<copyable<T>, indirect_strict_weak_order<Comp, projected<const T*, Proj>>>>
        constexpr T operator()(std::initializer_list<T> r, Comp comp = {}, Proj proj = {}) const
        {
            return *ranges::max_element(r, std::move(comp), std::move(proj));
        }

        template <class R, class Proj = identity, class Comp = less,
                  class = std::enable_if_t<indirect_strict_weak_order<Comp,
                                                                      projected<ranges::iterator_t<R>, Proj>> &&
                      indirectly_copyable_storable<ranges::iterator_t<R>,
                                                   ranges::range_value_t<R>*>>>
        constexpr range_value_t<R> operator()(R&& r, Comp comp = {}, Proj proj = {}) const
        {
            if constexpr (ranges::forward_range<R>)
                return
                    static_cast<range_value_t<R>>(*ranges::max_element(r, std::move(comp), std::move(proj)));
            else
            {
                auto i = ranges::begin(r);
                auto s = ranges::end(r);
                ranges::range_value_t<R> m(*i);
                while (++i != s)
                    if (ranges::invoke(comp, ranges::invoke(proj, *i), ranges::invoke(proj, m)))
                        m = *i;
                return m;
            }
        }
    };

    struct _clamp
    {
        template <typename T, typename Proj = identity, typename Comp = less,
                  class = std::enable_if_t<indirect_strict_weak_order<Comp, projected<const T*, Proj>>>>
        constexpr const T& operator()(const T& v, const T& lo, const T& hi, Comp comp = {}, Proj proj = {}) const
        {
            auto&& pv = ranges::invoke(proj, v);

            if (ranges::invoke(comp, std::forward<decltype(pv)>(pv), ranges::invoke(proj, lo)))
                return lo;

            if (ranges::invoke(comp, ranges::invoke(proj, hi), std::forward<decltype(pv)>(pv)))
                return hi;

            return v;
        }
    };

    inline constexpr _minmax minmax;
    inline constexpr _min min{};
    inline constexpr _max max{};
    inline constexpr auto clamp = _clamp{};
}

#endif //DONGDONG_RANGE_ALGORITHM_HPP
