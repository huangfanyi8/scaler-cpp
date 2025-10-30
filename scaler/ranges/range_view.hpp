#ifndef DONGDONG_RANGE_VIEW_HPP
#define DONGDONG_RANGE_VIEW_HPP

#include"../iterator/iterator.hpp"
#include "range_algorithm.hpp"
#include<optional>
#include<tuple>
#include"../span.h"
#include<cassert>
#include<array>

namespace
NAMESPACE_RANGES
{
    namespace _details
    {
        template <typename From, typename To>
        CXX17_CONCEPT _uses_nonqualification_pointer_conversion
            = std::is_pointer_v<From> && std::is_pointer_v<To>
            && !convertible_to<std::remove_pointer_t<From>(*)[],
                               std::remove_pointer_t<To>(*)[]>;

        template <typename From, typename To>
        CXX17_CONCEPT _convertible_to_non_slicing = convertible_to<From, To>
            && !_uses_nonqualification_pointer_conversion<std::decay_t<From>,
                                                          std::decay_t<To>>;

        template <class T, class U, class V>
        CXX17_CONCEPT _pair_like_convertible_from =
            !ranges::range<T>
            && !std::is_reference_v<T>
            && _is_pair_like<T>
            && constructible_from<T, U, V>
            && _convertible_to_non_slicing<U, std::tuple_element_t<0, T>>
            && convertible_to<V, std::tuple_element_t<1, T>>;
    }

    template <class T>
    class view_interface
        : public view_base
    {
        static_assert(std::is_class_v<T>);
        static_assert(same_as<T, std::remove_cv_t<T>>);

        template <class, class = void>
        struct _can_empty
            : std::false_type
        {};

        template <class D>
        struct _can_empty<D, std::void_t<decltype(ranges::empty(std::declval<D&&>()))>>
            : std::true_type
        {};

        template <class D, class =void>
        struct _can_size
            : std::false_type
        {};

        template <class D>
        struct _can_size<D, _require_t<forward_range<D>,
                                       sized_sentinel_for<sentinel_t<D>, iterator_t<D>>>>
            : std::true_type
        {};

        template <class D, class =void>
        struct _can_data
            : std::false_type
        {};

        template <class D>
        struct _can_data<D, std::enable_if_t<contiguous_iterator<ranges::iterator_t<D>>>>
            : std::true_type
        {};

    private:
        constexpr T& _derived_cast() noexcept
        {
            static_assert(derived_from<T, view_interface<T>>);
            static_assert(view<T>);
            return static_cast<T&>(*this);
        }

        constexpr const T& _derived_cast() const noexcept
        {
            static_assert(derived_from<T, view_interface<T>>);
            static_assert(view<T>);
            return static_cast<const T&>(*this);
        }

    public:
        template <class D =T, class = std::enable_if_t<forward_range<D> || sized_range<D>>>
        constexpr bool empty()
        {
            if constexpr (sized_range<T>)
                return ranges::size(_derived_cast()) == 0;
            else
                return ranges::begin(_derived_cast()) == ranges::end(_derived_cast());
        }

        template <class D =T, class = std::enable_if_t<forward_range<const D> || sized_range<const D>>>
        [[nodiscard]] constexpr bool empty() const
        {
            //static_assert(forward_range<const T> || (sized_range<const T>));
            if constexpr (sized_range<T>)
                return ranges::size(_derived_cast()) == 0;
            else
                return ranges::begin(_derived_cast()) == ranges::end(_derived_cast());
        }

        template <class D = T, class = std::enable_if_t<_can_empty<D&>::value>>
        constexpr explicit
        operator bool() noexcept(noexcept(ranges::empty(_derived_cast()))) { return !ranges::empty(_derived_cast()); }

        template <class D = T, class = std::enable_if_t<_can_empty<const D&>::value>>
        constexpr explicit
        operator bool() const noexcept(noexcept(ranges::empty(_derived_cast())))
        {
            return !ranges::empty(_derived_cast());
        }

        template <class D = T, class = std::enable_if_t<_can_data<D>::value>>
        constexpr auto data() noexcept(noexcept(ranges::begin(_derived_cast())))
        {
            return to_address(ranges::begin(_derived_cast()));
        }

        template <class D = T, class = std::enable_if_t<_can_data<const D>::value>>
        constexpr auto data() const noexcept(noexcept(ranges::begin(_derived_cast())))
        {
            return to_address(ranges::begin(_derived_cast()));
        }

        template <class D = T, class = std::enable_if_t<_can_size<D>::value>>
        constexpr auto size() noexcept(noexcept(ranges::end(_derived_cast()) - ranges::begin(_derived_cast())))
        {
            return ranges::end(_derived_cast()) - ranges::begin(_derived_cast());;
        }

        template <class D = T, class = std::enable_if_t<_can_size<const D>::value>>
        constexpr auto size() const noexcept(noexcept(ranges::end(_derived_cast()) - ranges::begin(_derived_cast())))
        {
            return ranges::end(_derived_cast()) - ranges::begin(_derived_cast());;
        }

        template <class D = T, class = std::enable_if_t<forward_range<D>>>
        constexpr decltype(auto) front() { return *ranges::begin(_derived_cast()); }

        template <class D = T, class = std::enable_if_t<forward_range<const D>>>
        constexpr decltype(auto) front() const { return *ranges::begin(_derived_cast()); }

        template <class D = T, class = std::enable_if_t<bidirectional_range<D> && common_range<D>>>
        constexpr decltype(auto) back() { return *ranges::prev(ranges::end(_derived_cast())); }

        template <class D = T, class = std::enable_if_t<bidirectional_range<const D> && common_range<const D>>>
        constexpr decltype(auto) back() const { return *ranges::prev(ranges::end(_derived_cast())); }

        template <class Range = T, typename = std::enable_if_t<random_access_range<Range>>>
        constexpr decltype(auto)
        operator[](range_difference_t<Range> n) { return ranges::begin(_derived_cast())[n]; }

        template <class Range = T, typename = std::enable_if_t<random_access_range<const Range>>>
        constexpr decltype(auto)
        operator[](range_difference_t<Range> n) const { return ranges::begin(_derived_cast())[n]; }

        template <class D = T, class = std::enable_if_t<input_range<D>>>
        constexpr auto cbegin() { return ranges::cbegin(_derived_cast()); }

        template <class D = T, class = std::enable_if_t<input_range<const D>>>
        constexpr auto
        cbegin() const { return ranges::cbegin(_derived_cast()); }

        template <class D = T, class = std::enable_if_t<input_range<D>>>
        constexpr auto cend() { return ranges::cend(_derived_cast()); }

        template <class D = T, class = std::enable_if_t<input_range<const D>>>
        constexpr auto cend() const { return ranges::cend(_derived_cast()); }
    };

    enum class subrange_kind : bool
    {
        unsized,
        sized
    };

    template <class Derived, class S, bool>
    struct _subrange_size
    {
        constexpr explicit _subrange_size(S s = {})
        {}
    };

    template <class S, class Derived>
    struct _subrange_size<Derived, S, true>
    {
        constexpr explicit _subrange_size(S s)
            : _size{s}
        {}

        typename Derived::_size_type _size;
    };

    template <class I, class S = I,
              ranges::subrange_kind K = sized_sentinel_for<S, I>
                                            ? ranges::subrange_kind::sized
                                            : ranges::subrange_kind::unsized,
              class = _require_t<
                  input_or_output_iterator<I>,
                  sentinel_for<S, I>,
                  (K == ranges::subrange_kind::sized || !sized_sentinel_for<S, I>)>>
    class subrange
        : public ranges::view_interface<subrange<I, S, K>>,
          public _enable_default_ctor<default_initializable<I>>,
          public _subrange_size<subrange<I, S, K>, std::make_unsigned_t<iter_difference_t<I>>, subrange_kind::sized == K
                                && !sized_sentinel_for<S, I>>
    {
        template <class, class, bool>
        friend class _subrange_size;

    private:
        static constexpr bool _store_size = K == subrange_kind::sized && !sized_sentinel_for<S, I>;

        //friend struct views::_Drop; // Needs to inspect _S_store_size.

        I _begin = I();
        S _end = S();

    public:
        using _size_type = std::make_unsigned_t<iter_difference_t<I>>;
        subrange() = default;

        template <class It, class = std::enable_if_t<(!_store_size) && _details::_convertible_to_non_slicing<It, I>>>
        constexpr
        subrange(It i, S s)
            noexcept(std::is_nothrow_constructible_v<It, decltype(i)> && std::is_nothrow_constructible_v<S, S&>)
            : _begin(std::move(i)), _end(s)
        {}

        template <class It, class = std::enable_if_t<
                      (K == subrange_kind::sized) && _details::_convertible_to_non_slicing<It, I>>>
        constexpr
        subrange(It i, S s, _size_type n)
            : _begin(std::move(i)), _end(s) { this->_size = n; }

        template <class R, class = std::enable_if_t<different_from<subrange, R> && borrowed_range<R>
                      && _details::_convertible_to_non_slicing<iterator_t<R>, I>
                      && convertible_to<sentinel_t<R>, S> &&
                      (!_store_size || sized_range<R>)>>
        constexpr
        subrange(R&& r)
            : subrange(r, ranges::size(r))
        {}

        template <class R, class = std::enable_if_t<
                      (K == subrange_kind::sized) &&
                      borrowed_range<R> &&
                      _details::_convertible_to_non_slicing<iterator_t<R>, I>
                      && convertible_to<sentinel_t<R>, S>>>
        constexpr
        subrange(R&& r, _size_type n)
            : subrange{ranges::begin(r), ranges::end(r), n}
        {}

        template <class PairLike, class = std::enable_if_t<different_from<PairLike, subrange> &&
                      _details::_pair_like_convertible_from<PairLike, const I&, const S&>>>
        constexpr operator PairLike() const { return PairLike{this > _begin, this->_end}; }

        template <class I2 = I, std::enable_if_t<copyable<I2>, int> = 0>
        constexpr I begin() const { return this->_begin; }

        template <typename I2 = I, std::enable_if_t<copyable<I2> == false, int> = 0>
        [[nodiscard]] constexpr I begin() { return std::move(this->_begin); }

        constexpr S end() const { return _end; }

        constexpr bool empty() const { return _begin == _end; }

        template <class Bool = std::bool_constant<(K ==
                      ranges::subrange_kind::sized)>, class = std::enable_if_t<Bool::vallue>>
        constexpr _size_type size() const
        {
            if constexpr (_store_size)
                return this->_size;
            else
                return (_end - _begin);
        }

        template <class It = I, class = std::enable_if_t<forward_iterator<It>>>
        [[nodiscard]] constexpr subrange
        next(iter_difference_t<I> n = 1) const &
        {
            auto t = *this;
            t.advance(n);
            return t;
        }

        [[nodiscard]] constexpr subrange
        next(iter_difference_t<I> n = 1) &&
        {
            this->advance(n);
            return std::move(*this);
        }

        template <class It = I, class = std::enable_if_t<bidirectional_iterator<It>>>
        [[nodiscard]] constexpr subrange
        prev(iter_difference_t<It> n = 1) const
        {
            auto tmp = *this;
            tmp.advance(-n);
            return tmp;
        }

        constexpr subrange& advance(iter_difference_t<I> n)
        {
            if constexpr (bidirectional_iterator<I>)
            {
                if (n < 0)
                {
                    ranges::advance(_begin, n);
                    if constexpr (_store_size)
                        this->_size += static_cast<std::make_unsigned_t<iter_difference_t<I>>>(-n);
                }
            }
            else
            {
                auto d = n - ranges::advance(_begin, n, _end);
                if constexpr (_store_size)
                    this->_size -= static_cast<std::make_unsigned_t<remove_cvref_t<decltype(d)>>>(d);
            }
            return *this;
        }
    };

    template <class I, class S, ranges::subrange_kind K>
    inline constexpr bool enable_borrowed_range<ranges::subrange<I, S, K>> = true;

    template <class I, class S,
              class = std::enable_if_t<input_or_output_iterator<I> && sentinel_for<S, I>, int>>
    subrange(I, S) -> subrange<I, S>;

    template <class I, class S, class = std::enable_if_t<input_or_output_iterator<I> && sentinel_for<S, I>, int>>
    subrange(I, S, std::make_unsigned_t<iter_difference_t<I>>) -> subrange<I, S, subrange_kind::sized>;

    template <class R, class = std::enable_if_t<borrowed_range<R>>>
    subrange(R&&)
        -> subrange<iterator_t<R>, sentinel_t<R>,
                    (sized_range<R>
                        || sized_sentinel_for<sentinel_t<R>, iterator_t<R>>)
                        ? subrange_kind::sized
                        : subrange_kind::unsized>;

    template <class R, class = std::enable_if_t<borrowed_range<R>>>
    subrange(R&&, std::make_unsigned_t<iter_difference_t<iterator_t<R>>>)
        -> subrange<iterator_t<R>, sentinel_t<R>, subrange_kind::sized>;

    template <class T, bool = ranges::range<T>>
    struct borrowed_subrange
    {};

    template <class T>
    struct borrowed_subrange<T, true>
        : std::conditional<ranges::borrowed_range<T>,
                           ranges::subrange<ranges::iterator_t<T>>, ranges::dangling>
    {};

    template <class R>
    using borrowed_subrange_t = typename borrowed_subrange<R>::type;

    //range | closure 时，等价于 closure(range)

    template <size_t _index, class I, class S,NAMESPACE_RANGES::subrange_kind Kind,
              class = std::enable_if_t<(_index < 2)>>
    constexpr auto get(const NAMESPACE_RANGES::subrange<I, S, Kind>& value)
    {
        if constexpr (_index == 0)
            return value.begin();
        else
            return value.end();
    }

    template <size_t _index, class I, class S,NAMESPACE_RANGES::subrange_kind Kind,
              class = std::enable_if_t<(_index < 2)>>
    constexpr auto get(NAMESPACE_RANGES::subrange<I, S, Kind>&& value)
    {
        if constexpr (_index == 0)
            return value.begin();
        else
            return value.end();
    }
}

namespace std
{
    using NAMESPACE_RANGES::get;

    template <class I, class S, NAMESPACE_RANGES::subrange_kind K>
    struct tuple_size<NAMESPACE_RANGES::subrange<I, S, K>>
        : std::integral_constant<std::size_t, 2>
    {};
}

namespace
NAMESPACE_RANGES
{
    namespace Cpo
    {
        struct _search
        {
        public:
            template <
                typename I1, typename S1,
                typename I2, typename S2,
                typename Pred = equal_to, typename Proj1 = identity, typename Proj2 = identity,
                class = _require_t<
                    forward_iterator<I1>, sentinel_for<S1, I1>,
                    forward_iterator<I2>, sentinel_for<S2, I2>,
                    indirectly_comparable<I1, I2, Pred, Proj1, Proj2>
                >>
            constexpr subrange<I1>
            operator()(I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = {}, Proj1 proj1 = {},
                       Proj2 proj2 = {}) const
            {
                for (;; ++first1)
                {
                    I1 it1 = first1;
                    I2 it2 = first2;
                    for (;; ++it1, (void)++it2)
                    {
                        if (it2 == last2)
                            return {first1, it1};
                        if (it1 == last1)
                            return {it1, it1};
                        if (!invoke(pred, invoke(proj1, *it1), invoke(proj2, *it2)))
                            break;
                    }
                }

                return {last1, last1};
            }

            template <typename R1, typename R2,
                      typename Pred = equal_to, typename Proj1 = identity, typename Proj2 = identity,
                      class = _require_t<indirectly_comparable<ranges::iterator_t<R1>,
                                                               ranges::iterator_t<R2>, Pred, Proj1, Proj2>>>
            constexpr borrowed_subrange_t<R1>
            operator()(R1&& r1, R2&& r2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
            {
                return (*this)(ranges::begin(r1), ranges::end(r1),
                               ranges::begin(r2), ranges::end(r2),
                               std::move(pred), std::move(proj1), std::move(proj2));
            }
        };

        struct _search_n
        {
            template <class Iter, class Sent, class Pred = ranges::equal_to, class Proj = identity, class T =
                      projected_value_t<Iter, Proj>,
                      class = _require_t<forward_iterator<Iter>, sentinel_for<Sent, Iter>, indirectly_comparable<
                                             Iter, const T*, Pred, Proj>>>
            constexpr subrange<Iter>
            operator()(Iter first, Sent last, iter_difference_t<Iter> count, const T& value, Pred pred = {},
                       Proj proj = {}) const
            {
                if (count <= 0)
                    return {first, first};

                auto _value_comp = [&](auto&& arg) -> bool
                {
                    return invoke(pred, std::forward<decltype(arg)>(arg), value);
                };
                if (count == 1)
                {
                    first = ranges::find_if(std::move(first), last, std::move(_value_comp), std::move(proj));
                    if (first == last)
                        return {first, first};
                    else
                    {
                        auto end = first;
                        return {first, ++end};
                    }
                }

                //std::random_access_iterator可以提高搜索的平均效率。
                if constexpr (sized_sentinel_for<Sent, Iter> && random_access_iterator<Iter>)
                {
                    auto _tail_size = last - first;
                    auto _remainder = count;

                    while (_remainder <= _tail_size)
                    {
                        first += _remainder;
                        _tail_size -= _remainder;
                        auto _backtrack = first;
                        while (_value_comp(invoke(proj, *--_backtrack)))
                        {
                            if (--_remainder == 0)
                                return {first - count, first};
                        }
                        _remainder = count + 1 - (first - _backtrack);
                    }
                    auto _i = first + _tail_size;
                    return {_i, _i};
                }
                else
                {
                    first = ranges::find_if(first, last, _value_comp, proj);
                    while (first != last)
                    {
                        auto _n = count;
                        auto _i = first;
                        ++_i;
                        while (_i != last && _n != 1 && _value_comp(invoke(proj, *_i)))
                        {
                            ++_i;
                            --_n;
                        }
                        if (_n == 1)
                            return {first, _i};
                        if (_i == last)
                            return {_i, _i};
                        first = ranges::find_if(++_i, last, _value_comp, proj);
                    }
                    return {first, first};
                }
            }

            template <class R, class Pred = ranges::equal_to, class Proj = identity,
                      class T = projected_value_t<ranges::iterator_t<R>, Proj>,
                      class = std::enable_if_t<forward_range<R> && indirectly_comparable<
                          ranges::iterator_t<R>, const T*, Pred, Proj>>>
            constexpr borrowed_subrange_t<R>
            operator()(R&& r, ranges::range_difference_t<R> count, const T& value, Pred pred = {}, Proj proj = {}) const
            {
                return (*this)(ranges::begin(r), ranges::end(r), std::move(count), value, std::move(pred),
                               std::move(proj));
            }
        };
    }

    inline constexpr Cpo::_search_n search_n{};
    inline constexpr Cpo::_search search{};
}

namespace
NAMESPACE_RANGES
{
    struct _find_end
    {
        template <typename I1, typename S1, typename I2, typename S2,
                  typename Pred = equal_to, typename Proj1 = identity,
                  typename Proj2 = identity>
        constexpr std::enable_if_t<
            forward_iterator<I1> && sentinel_for<S1, I1> && forward_iterator<I2> &&
            sentinel_for<S2, I2> &&
            indirectly_comparable<I1, I2, Pred, Proj1, Proj2>, subrange<I1>>
        operator()(I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = Pred{},
                   Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
        {
            if constexpr (bidirectional_iterator<I1> && bidirectional_iterator<I2>)
            {
                auto i1 = ranges::next(first1, last1);
                auto i2 = ranges::next(first2, last2);
                auto result
                    = ranges::search(std::reverse_iterator<I1>{i1},
                                     std::reverse_iterator<I1>{first1},
                                     std::reverse_iterator<I2>{i2},
                                     std::reverse_iterator<I2>{first2},
                                     std::move(pred),
                                     std::move(proj1), std::move(proj2));
                auto _result_first = ranges::end(result).base();
                auto _result_last = ranges::begin(result).base();
                if (_result_last == first1)
                    return {i1, i1};
                else
                    return {_result_first, _result_last};
            }
            else
            {
                auto _i = ranges::next(first1, last1);
                if (first2 == last2)
                    return {_i, _i};

                auto _result_begin = _i;
                auto _result_end = _i;
                for (;;)
                {
                    auto _new_range = ranges::search(first1, last1, first2, last2, pred, proj1, proj2);
                    auto _new_result_begin = ranges::begin(_new_range);
                    auto _new_result_end = ranges::end(_new_range);
                    if (_new_result_begin == last1)
                        return {_result_begin, _result_end};
                    else
                    {
                        _result_begin = _new_result_begin;
                        _result_end = _new_result_end;
                        first1 = _result_begin;
                        ++first1;
                    }
                }
            }
        }

        template <class R1, typename R2, typename Pred = equal_to,
                  typename Proj1 = identity, typename Proj2 = identity>
        constexpr std::enable_if_t<
            forward_range<R1> && forward_range<R2> &&
            indirectly_comparable<iterator_t<R1>, iterator_t<R2>, Pred, Proj1, Proj2>,
            borrowed_subrange_t<R1>>
        operator()(R1&& r1, R2&& r2, Pred pred = {},
                   Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            return (*this)(ranges::begin(r1), ranges::end(r1),
                           ranges::begin(r2), ranges::end(r2),
                           std::move(pred),
                           std::move(proj1), std::move(proj2));
        }
    };

    struct _equal_range
    {
        template <class Iter, class S, class T, class Proj = identity, class Comp = less,
                  REQUIRE(forward_iterator <Iter>, sentinel_for<S, Iter>, indirect_strict_weak_order<const T*,
                          projected<Iter, Proj>>)>
        constexpr subrange<Iter>
        operator()(Iter first, S last, const T& value, Comp comp = {}, Proj proj = {}) const
        {
            auto len = ranges::distance(first, last);

            while (len > 0)
            {
                auto half = len / 2;
                auto middle = first;
                ranges::advance(middle, half);
                if (invoke(comp, invoke(proj, *middle), value))
                {
                    first = middle;
                    ++first;
                    len = len - half - 1;
                }
                else if (invoke(comp, value, invoke(proj, *middle)))
                    len = half;
                else
                {
                    auto left = ranges::lower_bound(first, middle, value, comp, proj);
                    ranges::advance(first, len);
                    auto right = ranges::upper_bound(++middle, first, value, comp, proj);
                    return {left, right};
                }
            }
            return {first, first};
        }

        template <class Range, class T, class Proj = identity, class Comp = less,REQUIRE(forward_range<Range>,
                      indirect_strict_weak_order<Comp, const T*, projected<iterator_t<Range>, Proj>>)>
        constexpr borrowed_subrange_t<Range>
        operator()(Range&& r, const T& value, Comp comp = {}, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), value, std::move(comp), std::move(proj));
        }
    };

    inline constexpr auto find_end = _find_end{};
    inline constexpr _equal_range equal_range{};
}

namespace
NAMESPACE_RANGES
{
    struct _contains
    {
        template <class I, class S,
                  class Proj = identity,
                  class T = projected_value_t<I, Proj>,
                  class=std::enable_if_t<input_iterator<I>
                      && sentinel_for<S, I>
                      && indirect_binary_predicate<ranges::equal_to, projected<I, Proj>, const T*>>>
        constexpr bool operator()(I first, S last, const T& value, Proj proj = {}) const
        {
            return ranges::find(std::move(first), last, value, proj) != last;
        }

        template <class R, class Proj = ranges::identity, class T = projected_value_t<iterator_t<R>, Proj>,
                  class=std::enable_if_t<input_range<R> && indirect_binary_predicate<
                      equal_to, projected<iterator_t<R>, Proj>, const T*>>>
        constexpr bool operator()(R&& r, const T& value, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(value), proj);
        }
    };

    struct _contains_subrange
    {
        template <class I1, class S1, class I2, class Pred = equal_to, class S2, class Proj1 = identity, class Proj2 =
                  identity,
                  class = _require_t<forward_iterator<I1>, sentinel_for<S1, I1>, forward_iterator<I2>, sentinel_for<
                                         S2, I2>, indirectly_comparable<I1, I2, Pred, Proj1, Proj2>>>
        constexpr bool operator()(I1 first1, S1 last1,
                                  I2 first2, S2 last2,
                                  Pred pred = {},
                                  Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            return (first2 == last2) || !ranges::search(first1, last1, first2, last2, pred, proj1, proj2).empty();
        }

        template <class R1, class R2,
                  class Pred = equal_to,
                  class Proj1 = identity, class Proj2 = identity,
                  class = _require_t<forward_range<R1>, forward_range<R2>, indirectly_comparable<
                                         ranges::iterator_t<R1>, ranges::iterator_t<R2>, Pred, Proj1, Proj2>>>
        constexpr bool operator()(R1&& r1, R2&& r2,
                                  Pred pred = {},
                                  Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            return (*this)(ranges::begin(r1), ranges::end(r1),
                           ranges::begin(r2), ranges::end(r2), std::move(pred),
                           std::move(proj1), std::move(proj2));
        }
    };

    inline constexpr _contains contains{};
    inline constexpr _contains_subrange contains_subrange{};
}

namespace
NAMESPACE_RANGES
{
    struct _partition
    {
        template <class Iter, class Sent, class P = identity, class Pred,
                  REQUIRE(permutable<Iter>, sentinel_for<Sent, Iter>,
                          indirect_unary_predicate<Pred, projected<Iter, P>>)>
        constexpr subrange<Iter> operator()(Iter _first, Sent _last, Pred _pred, P _proj = {}) const
        {
            if constexpr (bidirectional_iterator<Iter>)
            {
                auto t = ranges::next(_first, _last);
                auto _tail = t;
                for (;;)
                {
                    for (;;)
                        if (_first == _tail)
                            return {std::move(_first), std::move(t)};
                        else if (invoke(_pred, invoke(_proj, *_first)))
                            ++_first;
                        else
                            break;
                    --_tail;
                    for (;;)
                        if (_first == _tail)
                            return {std::move(_first), std::move(t)};
                        else if (!(bool)invoke(_pred, invoke(_proj, *_tail)))
                            --_tail;
                        else
                            break;
                    ranges::iter_swap(_first, _tail);
                    ++_first;
                }
            }
            else
            {
                if (_first == _last)
                    return {_first, _first};

                while (invoke(_pred, invoke(_proj, *_first)))
                    if (++_first == _last)
                        return {_first, _first};

                auto _next = _first;
                while (++_next != _last)
                    if (invoke(_pred, invoke(_proj, *_next)))
                    {
                        ranges::iter_swap(_first, _next);
                        ++_first;
                    }

                return {std::move(_first), std::move(_next)};
            }
        }

        template <class Range, class Proj = identity, class Pred,
                  class = std::enable_if_t<permutable<iterator_t<Range>> && forward_range<Range> &&
                      indirect_unary_predicate<Pred, projected<iterator_t<Range>, Proj>>>>
        constexpr borrowed_subrange_t<Range>
        operator()(Range&& r, Pred pred, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(pred), std::move(proj));
        }
    };

    ///将一个范围根据单元谓词分为两个部分
    inline constexpr _partition partition{};

    struct _stable_partition
    {
        template <class Iter, class Sent, class P = identity, class Pred,
                  REQUIRE(bidirectional_iterator<Iter>, permutable<Iter>, sentinel_for<Sent, Iter>,
                          indirect_unary_predicate<Pred, projected<Iter, P>>)>
        constexpr subrange<Iter> operator()(Iter first, Sent last, Pred _pred, P proj = {}) const
        {}

        template <class Range, class Proj = identity, class Pred,
                  class = std::enable_if_t<permutable<iterator_t<Range>>
                      && bidirectional_range<Range>
                      && indirect_unary_predicate<Pred, projected<iterator_t<Range>, Proj>>>>
        constexpr borrowed_subrange_t<Range>
        operator()(Range&& r, Pred pred, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), std::move(pred), std::move(proj));
        }
    };

    inline constexpr _stable_partition stable_partition{};
}

namespace
NAMESPACE_RANGES
{
    struct remove_if_fn
    {
        template <typename I, typename S, typename Pred, typename Proj = identity,
                  REQUIRE(forward_iterator<I>, sentinel_for<S, I>, permutable<I>, indirect_unary_predicate<Pred,
                          projected<I, Proj>>)>
        constexpr subrange<I> operator()(I first, S last, Pred pred, Proj proj = {}) const
        {
            first = ranges::find_if(std::move(first), last, pred, proj);
            if (first != last)
            {
                for (I i{std::next(first)}; i != last; ++i)
                    if (!std::invoke(pred, std::invoke(proj, *i)))
                    {
                        *first = ranges::iter_move(i);
                        ++first;
                    }
            }
            return {first, last};
        }

        template <class R, class Pred, class Proj = identity>
        constexpr std::enable_if_t<
            forward_range<R> && permutable<iterator_t<R>> &&
            indirect_unary_predicate<Pred, projected<iterator_t<R>, Proj>>,
            borrowed_iterator_t<R>>
        operator()(R&& r, Pred pred, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), pred, std::move(proj));
        }
    };

    struct remove_fn
    {
        template <class I, class S, class Proj = identity, class T = projected_value_t<I, Proj>,
                  REQUIRE(permutable<I>, sentinel_for<S, I>, indirect_binary_predicate<equal_to, projected<I, Proj>,
                          const T*>)>
        constexpr subrange<I> operator()(I first, S last, const T& value, Proj proj = {}) const
        {
            first = ranges::find(std::move(first), last, value, proj);
            if (first != last)
            {
                for (I i{std::next(first)}; i != last; ++i)
                    if (value != invoke(proj, *i))
                    {
                        *first = ranges::iter_move(i);
                        ++first;
                    }
            }
            return {first, last};
        }

        template <class R, class Proj = identity, class T = projected_value_t<iterator_t<R>, Proj>,
                  REQUIRE(forward_range <R>, permutable<ranges::iterator_t<R>>,
                          indirect_binary_predicate<equal_to, projected<iterator_t<R>, Proj>, const T*>)>
        constexpr borrowed_subrange_t<R> operator()(R&& r, const T& value, Proj proj = {}) const
        {
            return (*this)(ranges::begin(r), ranges::end(r), value, std::move(proj));
        }
    };

    struct rotate_fn
    {
        template <class I, class S, class = std::enable_if_t<permutable<I> && sentinel_for<S, I>>>
        constexpr subrange<I> operator()(I first, I middle, S last) const
        {
            if (first == middle)
            {
                auto last_it = ranges::next(first, last);
                return {last_it, last_it};
            }
            if (middle == last)
                return {std::move(first), std::move(middle)};

            if constexpr (bidirectional_iterator<I>)
            {
                ranges::reverse(first, middle);
                auto last_it = ranges::next(first, last);
                ranges::reverse(middle, last_it);

                if constexpr (random_access_iterator<I>)
                {
                    ranges::reverse(first, last_it);
                    return {first + (last_it - middle), std::move(last_it)};
                }
                else
                {
                    auto mid_last = last_it;
                    do
                    {
                        ranges::iter_swap(first, --mid_last);
                        ++first;
                    }
                    while (first != middle && mid_last != middle);
                    ranges::reverse(first, mid_last);

                    if (first == middle)
                        return {std::move(mid_last), std::move(last_it)};
                    else
                        return {std::move(first), std::move(last_it)};
                }
            }
            else //forward_iterator
            {
                auto next_it = middle;
                do
                {
                    // rotate the first cycle
                    ranges::iter_swap(first, next_it);
                    ++first;
                    ++next_it;
                    if (first == middle)
                        middle = next_it;
                }
                while (next_it != last);

                auto new_first = first;
                while (middle != last)
                {
                    // rotate subsequent cycles
                    next_it = middle;
                    do
                    {
                        ranges::iter_swap(first, next_it);
                        ++first;
                        ++next_it;
                        if (first == middle)
                            middle = next_it;
                    }
                    while (next_it != last);
                }

                return {std::move(new_first), std::move(middle)};
            }
        }

        template <class R, class = std::enable_if_t<forward_range<R> && permutable<ranges::iterator_t<R>>>>
        constexpr borrowed_subrange_t<R> operator()(R&& r, iterator_t<R> middle) const
        {
            return (*this)(ranges::begin(r), std::move(middle), ranges::end(r));
        }
    };

    inline constexpr remove_fn remove{};
    inline constexpr remove_if_fn remove_if{};
    inline constexpr auto rotate = rotate_fn{};
}

namespace
NAMESPACE_RANGES
{
    template <class T>
    struct empty_view
    {
        static_assert(std::is_object_v<T>);
        static constexpr size_t size() { return 0; }
        static constexpr T* end() { return nullptr; }
        static constexpr T* begin() { return nullptr; }
        static constexpr T* data() { return nullptr; }
        static constexpr bool empty() { return true; }
    };

    template <class T>
    inline constexpr bool enable_borrowed_range<empty_view<T>> = true;

    namespace _details
    {
        template <class T>
        inline constexpr bool _boxable = std::is_object_v<T> && move_constructible<T>;

        template <class T,
                  int _v,
                  bool _copyable,
                  class = void>
        class movable_box
            : movable_box<T, _v + 1, _copyable>
        {};

        template <class T>
        class movable_box<T, 0, false, _require_t<
                              _boxable<T>, std::is_nothrow_move_constructible_v<T> &&
                              std::is_nothrow_copy_constructible_v<T>>>
            : public _enable_default_ctor<default_initializable<T>>
        {
        public:
            constexpr movable_box() = default;

            constexpr explicit movable_box(const T& t)
                noexcept(std::is_nothrow_copy_constructible_v<T>)
                : _value(t)
            {}

            constexpr explicit movable_box(T&& t)
                noexcept(std::is_nothrow_move_constructible_v<T>)
                : _value(t)
            {}

            template <class... Args,
                      class = std::enable_if_t<constructible_from<T, Args...>>>
            constexpr explicit
            movable_box(std::in_place_t, Args&&... args)
                noexcept(std::is_nothrow_constructible_v<T, Args...>)
                : _value(std::forward<Args>(args)...)
            {}

            movable_box(const movable_box&) = default;
            movable_box(movable_box&&) = default;

        public:
            [[nodiscard]] constexpr bool has_value() const noexcept { return true; };

            constexpr T& operator*() noexcept { return _value; }
            constexpr const T& operator*() const noexcept { return _value; }
            constexpr T* operator->() noexcept { return std::addressof(_value); }
            constexpr const T* operator->() const noexcept { return std::addressof(_value); }

            constexpr movable_box& operator=(const movable_box& that)
            {
                static_assert(std::is_nothrow_copy_constructible_v<T>);
                if (this != std::addressof(that))
                {
                    this->_value.~T();
                    ::new(std::addressof(this->_value))T(*that);
                }
                return *this;
            }

            constexpr movable_box& operator=(movable_box&& that) noexcept
            {
                static_assert(std::is_nothrow_move_constructible_v<T>);
                if (this != std::addressof(that))
                {
                    this->_value.~T();
                    ::new(std::addressof(this->_value))T(std::move(*that));
                }
                return *this;
            }

        private:
            T _value;
        };

        template <class T>
        class movable_box<T, 0, true, _require_t<_boxable<T>, copyable<T> || (std::is_nothrow_move_constructible_v<T> &&
                                                     std::is_nothrow_copy_constructible_v<T>)>>
            : public _enable_default_ctor<default_initializable<T>>
        {
        public:
            constexpr movable_box() = default;

            constexpr explicit movable_box(const T& t)
                noexcept(std::is_nothrow_copy_constructible_v<T>)
                : _value(t)
            {}

            constexpr explicit movable_box(T&& t)
                noexcept(std::is_nothrow_move_constructible_v<T>)
                : _value(t)
            {}

            template <class... Args,
                      class = std::enable_if_t<constructible_from<T, Args...>>>
            constexpr explicit
            movable_box(std::in_place_t, Args&&... args)
                noexcept(std::is_nothrow_constructible_v<T, Args...>)
                : _value(std::forward<Args>(args)...)
            {}

            movable_box(const movable_box&) = default;
            movable_box(movable_box&&) = default;

        public:
            [[nodiscard]] constexpr bool has_value() const noexcept { return true; };
            constexpr T& operator*() noexcept { return _value; }
            constexpr const T& operator*() const noexcept { return _value; }
            constexpr T* operator->() noexcept { return std::addressof(_value); }
            constexpr const T* operator->() const noexcept { return std::addressof(_value); }
            constexpr movable_box& operator=(const movable_box& that) = default;
            constexpr movable_box& operator=(movable_box&& that) noexcept = default;

        private:
            T _value;
        };

        template <class T, bool _v>
        class movable_box<T, 1, _v, std::enable_if_t<_boxable<T>>>
            : public std::optional<T>
        {
        public:
            using std::optional<T>::optional;
            using std::optional<T>::operator=;

        public:
            template <class U = T, class =std::enable_if_t<default_initializable<U>>>
            constexpr movable_box() noexcept(std::is_nothrow_default_constructible_v<U>)
                : std::optional<T>{std::in_place}
            {}

            movable_box(const movable_box&) = default;
            movable_box(movable_box&&) = default;

            template <class U = T, class = std::enable_if_t<!copyable<U>>>
            constexpr movable_box& operator=(const movable_box& that)
                noexcept(std::is_nothrow_copy_constructible_v<T>)
            {
                if (this != std::addressof(that))
                {
                    if ((bool)that)
                        this->emplace(*that);
                    else
                        this->reset();
                }
                return *this;
            }

            template <class U = T, class = std::enable_if_t<!movable<U>>>
            constexpr movable_box& operator=(movable_box&& that)
                noexcept(std::is_nothrow_move_constructible_v<T>)
            {
                if (this != std::addressof(that))
                {
                    if ((bool)that)
                        this->emplace(std::move(*that));
                    else
                        this->reset();
                }
                return *this;
            }
        };

        template <class T, bool _v>
        class movable_box<T, 2, _v>
        {};
    }

    template <class T, class = std::enable_if_t<copy_constructible<T> && std::is_object_v<T>>>
    class single_view
        : public view_interface<single_view<T>>,
          public _enable_default_ctor<default_initializable<T>>
    {
    public:
        single_view() = default;

        constexpr explicit
        single_view(const T& t)
            noexcept(std::is_nothrow_copy_constructible_v<T>)
            : _value(t)
        {}

        constexpr explicit
        single_view(T&& t)
            noexcept(std::is_nothrow_move_constructible_v<T>)
            : _value(std::move(t))
        {}

        template <typename... Args, class = std::enable_if_t<constructible_from<T, Args...>>>
        constexpr explicit
        single_view(std::in_place_t, Args&&... args)
            noexcept(std::is_nothrow_constructible_v<T, Args...>)
            : _value{std::in_place, std::forward<Args>(args)...}
        {}

        constexpr T*
        begin() noexcept
        {
            return data();
        }

        constexpr const T*
        begin() const noexcept
        {
            return data();
        }

        constexpr T*
        end() noexcept
        {
            return data() + 1;
        }

        constexpr const T*
        end() const noexcept
        {
            return data() + 1;
        }

        static constexpr size_t
        size() noexcept
        {
            return 1;
        }

        constexpr T*
        data() noexcept
        {
            return _value.operator->();
        }

        constexpr const T*
        data() const noexcept
        {
            return _value.operator->();
        }

    private:
        _details::movable_box<T, 0, copyable<T>> _value;
    };

    template <typename T>
    single_view(T) -> single_view<T>;

    namespace views
    {
        template <class, class = void>
        inline constexpr bool _can_single_view = false;

        template <class T>
        inline constexpr bool _can_single_view<T, std::void_t<decltype(ranges::single_view<std::decay_t<T>>(
                                                   std::declval<T>()))>> = true;

        struct _single
        {
            template <class T, class = std::enable_if_t<_can_single_view<T>>>
            constexpr auto
            operator() [[nodiscard]](T&& e) const
                noexcept(noexcept(ranges::single_view<std::decay_t<T>>(std::forward<T>(e))))
            {
                return ranges::single_view<std::decay_t<T>>(std::forward<T>(e));
            }
        };

        inline constexpr _single single{};
    }
}

namespace
NAMESPACE_RANGES
{
    template <class Iter>
    struct _iota_diff
    {
        template <class T>
        using _traits = std::bool_constant<(sizeof(T) > sizeof(Iter))>;

        using type =
        std::conditional_t<integral<Iter> || (sizeof(iter_difference_t<Iter>) > sizeof(Iter)),
                           iter_difference_t<Iter>,
                           _lower_bound_t<meta_list<char, short, long, int, long long, _max_diff_type>, _traits
                           >>;
    };

    //TODO This is not _iota_diff_t in the standard library
    template <class T>
    using _iota_diff_t = typename _iota_diff<T>::type;;

    template <class W, class Bound = unreachable_sentinel_t>
    class iota_view
        : public view_interface<iota_view<W, Bound>>,
          public _enable_default_ctor<default_initializable<W>>
    {
        template <class, class = void>
        struct _decrementable
            : std::false_type
        {};

        template <class T>
        struct _decrementable<T, std::void_t<
                                  std::enable_if_t<same_as<decltype(std::declval<T&>()--), T>>,
                                  std::enable_if_t<same_as<T&, decltype(--std::declval<T&>())>>,
                                  std::enable_if_t<incrementable<T>>>>
            : std::true_type
        {};

        template <class, class = void>
        struct _advanceable_
            : std::false_type
        {};

        template <class T>
        struct _advanceable_<T, std::void_t<std::enable_if_t<totally_ordered<T> && _decrementable<T>::value>>>
        {
        private:
            template <class, class = void>
            struct _impl
                : std::false_type
            {};

            template <class It>
            struct _impl<It, std::void_t<const _iota_diff_t<It>&,
                                         decltype(It(
                                             std::declval<const It&>() + std::declval<const _iota_diff_t<It>&>())),
                                         decltype(It(
                                             std::declval<const It&>() - std::declval<const _iota_diff_t<It>&>())),
                                         decltype(It(
                                             std::declval<const _iota_diff_t<It>&>() + std::declval<const It&>())),
                                         _require_t<same_as<It&, decltype(It(
                                                                std::declval<const It&>() += std::declval<const
                                                                    _iota_diff_t<It>&>()))>,
                                                    same_as<It&, decltype(It(
                                                                std::declval<const It&>() -= std::declval<const
                                                                    _iota_diff_t<It>&>()))>,
                                                    convertible_to<
                                                        decltype(std::declval<const It&>() - std::declval<const It&>()),
                                                        _iota_diff_t<It>>>
                         >>
                : std::true_type
            {};

        public:
            static constexpr bool value = _impl<T>::value;
        };

    public:
        static_assert(weakly_incrementable<W>);
        static_assert(semiregular<Bound>);
        static_assert(NAMESPACE_DETAILS::_weakly_equality_comparable_with<W, Bound>);
        static_assert(copyable<W>);

    private:
        template <class, class = void>
        struct _iota_view_iterator_category
        {};

        template <class T>
        struct _iota_view_iterator_category<T, std::enable_if_t<incrementable<T>>>
        {
            using iterator_category = input_iterator_tag;
        };

        struct Iterator
            : _enable_default_ctor<default_initializable<W>>,
              _iota_view_iterator_category<W>
        {
            template <class, class>
            friend class iota_view;

            using difference_type = _iota_diff_t<W>;
            using value_type = W;
            using iterator_concept = _switch_t<
                bool_sequence<_advanceable_<W>::value, _decrementable<W>::value, incrementable<W>>,
                meta_list<random_access_iterator_tag, bidirectional_iterator_tag, forward_iterator_tag>,
                input_iterator_tag>;

            constexpr Iterator() = default;

            constexpr explicit Iterator(W value)
                : _value(value)
            {}

            constexpr W operator*() const noexcept(std::is_nothrow_copy_constructible_v<W>)
            {
                return _value;
            }

            constexpr Iterator& operator++()
            {
                ++_value;
                return *this;
            }

            constexpr auto operator++(int)
            {
                if constexpr (incrementable<W>)
                {
                    auto tmp = *this;
                    ++*this;
                    return tmp;
                }
                else
                    ++*this;
            }

            template <class WW =W, class =std::enable_if_t<_decrementable<WW>::value>>
            constexpr Iterator& operator--()
            {
                --this->_value;
                return *this;
            }

            template <class WW =W, class =std::enable_if_t<_decrementable<WW>::value>>
            constexpr auto operator--(int)
            {
                auto tmp = *this;
                --*this;
                return tmp;
            }

            template <class WW =W, class =std::enable_if_t<_advanceable_<WW>::value>>
            constexpr Iterator& operator+=(difference_type n)
            {
                if constexpr (integral<WW> && !signed_integral<WW>)
                    if (n >= difference_type(0))
                        this->_value += static_cast<W>(n);
                    else
                        this->_value -= static_cast<W>(-n);
                else
                    this->_value += n;
                return *this;
            }

            template <class WW =W, class =std::enable_if_t<_advanceable_<WW>::value>>
            constexpr Iterator& operator-=(difference_type n)
            {
                *this += -n;
                return *this;
            }

            template <class WW =W, class =std::enable_if_t<_advanceable_<WW>::value>>
            constexpr W operator[](difference_type n) const
            {
                return W(_value + n);
            }

            template <class WW =W, class =std::enable_if_t<equality_comparable<WW>>>
            friend constexpr bool operator==(const Iterator& x, const Iterator& y)
            {
                return x._value == y._value;
            }

            template <class WW =W, class =std::enable_if_t<equality_comparable<WW>>>
            friend constexpr bool operator!=(const Iterator& x, const Iterator& y)
            {
                return x._value != y._value;
            }

            template <class WW =W, class =std::enable_if_t<totally_ordered<WW>>>
            friend constexpr bool operator<(const Iterator& x, const Iterator& y)
            {
                return x._value < y._value;
            }

            template <class WW =W, class =std::enable_if_t<totally_ordered<WW>>>
            friend constexpr bool operator>(const Iterator& x, const Iterator& y)
            {
                return x._value > y._value;
            }

            template <class WW =W, class =std::enable_if_t<totally_ordered<WW>>>
            friend constexpr bool operator<=(const Iterator& x, const Iterator& y)
            {
                return x._value <= y._value;
            }

            template <class WW =W, class =std::enable_if_t<totally_ordered<WW>>>
            friend constexpr bool operator>=(const Iterator& x, const Iterator& y)
            {
                return x._value >= y._value;
            }

            template <class WW =W, class =std::enable_if_t<_advanceable_<WW>::value>>
            friend constexpr Iterator
            operator+(Iterator i, difference_type n)
            {
                i += n;
                return i;
            }

            template <class WW =W, class =std::enable_if_t<_advanceable_<WW>::value>>
            friend constexpr Iterator
            operator+(difference_type n, Iterator i)
            {
                i += n;
                return i;
            }

            template <class WW =W, class =std::enable_if_t<_advanceable_<WW>::value>>
            friend constexpr Iterator
            operator-(Iterator i, difference_type n)
            {
                i -= n;
                return i;
            }

            template <class WW =W, class =std::enable_if_t<_advanceable_<WW>::value>>
            friend constexpr difference_type operator-(const Iterator& x, const Iterator& y)
            {
                using D = difference_type;
                if constexpr (integral<D>)
                    if constexpr (signed_integral<D>)
                        return D(D(x._value) - D(y._value));
                    else
                        return (y._value > x._value) ? D(-D(y._value - x._value)) : D(x._value - y._value);
                else
                    return x._value - y._value;
            }

            friend class iota_view;
            friend class Sentinel;

            W _value = W();
        };

        class Sentinel
        {
        public:
            constexpr Sentinel() : _bound(Bound())
            {}

            constexpr explicit Sentinel(Bound bound) : _bound(bound)
            {}

            friend constexpr bool operator==(const Iterator& x, const Sentinel& y)
            {
                return *x == y._bound;
            }

            friend constexpr bool operator!=(const Iterator& x, const Sentinel& y)
            {
                return !(x == y);
            }

            friend constexpr bool operator==(const Sentinel& y, const Iterator& x)
            {
                return x == y;
            }

            friend constexpr bool operator!=(const Sentinel& y, const Iterator& x)
            {
                return !(x == y);
            }

            template <class WW = W>
            friend constexpr auto operator-(const Iterator& i, const Sentinel& s)
                -> std::enable_if_t<sized_sentinel_for<Bound, WW>, iter_difference_t<WW>>
            {
                return i._value - s._bound;
            }

            template <typename WW = W>
            friend constexpr auto operator-(const Sentinel& s, const Iterator& i)
                -> std::enable_if_t<sized_sentinel_for<Bound, WW>, iter_difference_t<WW>>
            {
                return -(i - s);
            }

        private:
            friend class iota_view;

            Bound _bound;
        };

        constexpr iota_view() = default;

        constexpr explicit iota_view(W value)
            : _value(value)
              , _bound(Bound())
        {}

        constexpr explicit iota_view(type_identity_t<W> value, type_identity_t<Bound> bound)
            : _value(value), _bound(bound)
        {}

        template <class WW = W, class B =Bound, class = std::enable_if_t<same_as<WW, B>>>
        constexpr iota_view(Iterator first, Iterator last)
            : iota_view(first._value, last._value)
        {}

        template <class B =Bound, class = std::enable_if_t<same_as<B, unreachable_sentinel_t>>>
        constexpr iota_view(Iterator first, unreachable_sentinel_t last)
            : iota_view(first._value, last)
        {}

        template <class WW = W, class B =Bound, class = std::enable_if_t<!same_as<WW, B> && !same_as<
                      B, unreachable_sentinel_t>>>
        constexpr iota_view(Iterator first, Sentinel last)
            : iota_view(first._value, last._bound)
        {}

        constexpr Iterator begin() const
        {
            return iterator(_value);
        }

        template <typename WW = W, std::enable_if_t<same_as<WW, Bound>, int> = 0>
        constexpr Iterator end() const
        {
            return Iterator{_bound};
        }

        constexpr bool empty() const
        {
            return _value == _bound;
        }

        template <typename WW = W, typename BB = Bound, std::enable_if_t<
                      (same_as<WW, BB> && _advanceable_<WW>::value) ||
                      (integral<WW> && integral<BB>) ||
                      sized_sentinel_for<BB, WW>, int> = 0>
        constexpr auto size() const
        {
            constexpr auto make_unsigned_like = [](auto i)
            {
                return std::make_unsigned_t<decltype(i)>(i);
            };

            if constexpr (integral<W> && integral<Bound>)
            {
                return (_value < 0)
                           ? ((_bound < 0)
                                  ? make_unsigned_like(-_value) - make_unsigned_like(-_bound)
                                  : make_unsigned_like(_bound) - make_unsigned_like(-_value))
                           : make_unsigned_like(_bound) - make_unsigned_like(_value);
            }
            else
                make_unsigned_like(_bound - _value);
        }

        W _value = W();
        Bound _bound = Bound();
    };

    template <typename W, typename B>
    inline constexpr bool
    enable_borrowed_range<iota_view<W, B>> = true;

    template <typename W, typename Bound, std::enable_if_t<
                  !integral<W> || !integral<Bound> ||
                  (signed_integral<W> == signed_integral<Bound>), int> = 0>
    iota_view(W, Bound) -> iota_view<W, Bound>;

    namespace views
    {
        template <class, class, class = void>
        inline constexpr bool _can_iota_view = false;

        template <class T>
        inline constexpr bool _can_iota_view<T, T, std::void_t<decltype(ranges::iota_view<T>(std::declval<T&&>()))>> =
            true;

        template <class T, class B>
        inline constexpr bool _can_iota_view<T, B, std::void_t<decltype(ranges::iota_view<T>(
                                                 std::declval<T&&>(), std::declval<B&&>()))>> = true;

        struct _iota
        {
            template <class T>
            [[nodiscard]] constexpr auto operator()(T&& _v) const
                noexcept(noexcept(ranges::iota_view(static_cast<T&&>(_v))))
            {
                return ranges::iota_view(std::forward<T&&>(_v));
            }

            template <class T, class U>
            [[nodiscard]] constexpr auto operator()(T&& _v1, U&& _v2) const
                noexcept(noexcept(ranges::iota_view(static_cast<T&&>(_v1), static_cast<U&&>(_v2))))
            {
                return ranges::iota_view(std::forward<T&&>(_v1), std::forward<U&&>(_v2));
            }
        };

        inline constexpr _iota iota;
    }
}

namespace
NAMESPACE_RANGES
{

    //用于处理闭包|闭包-->新的闭包
    template <class, class, class = void>
    class _pipe;

    //所有范围适配器的基类,如果需要自定义范围适配器都需要继承这个类
    template <class T>
    struct range_adaptor_closure
    {
        static_assert(std::is_class_v<T> && std::is_same_v<T, std::remove_cv_t<T>>);
    };

    template <class>
    inline constexpr bool _is_range_adaptor_closure_v = false;

    template <class T>
    inline constexpr bool _is_range_adaptor_closure_v<range_adaptor_closure<T>> = true;

    //范围|闭包---> 视图
    template <class Range, class Closure,
              REQUIRE(range<Range>,
                  _is_range_adaptor_closure_v<Closure>,
                  std::is_invocable_v<Closure, Range>)>
    constexpr auto operator|(Range&& range, Closure&& closure)
    {
        return std::forward<Closure>(closure)(std::forward<Range>(range));
    }

    //闭包|闭包-->新的闭包
    template <class Closure, class OtherClosure,
              REQUIRE(_is_range_adaptor_closure_v<Closure>,
                      _is_range_adaptor_closure_v<OtherClosure>,
                      std::is_constructible_v<remove_cvref_t<Closure>, Closure>,
                      std::is_constructible_v<remove_cvref_t<OtherClosure>, OtherClosure>)>
    constexpr _pipe<remove_cvref_t<Closure>, remove_cvref_t<OtherClosure>> operator|(Closure&& c1, OtherClosure&& c2)
        noexcept(std::is_nothrow_constructible_v<remove_cvref_t<Closure>, Closure>
            && std::is_nothrow_constructible_v<remove_cvref_t<OtherClosure>, OtherClosure>)
    {
        return _pipe<remove_cvref_t<Closure>, remove_cvref_t<OtherClosure>>{
            std::forward<Closure>(c1),
            std::forward<OtherClosure>(c2)
        };
    }

    template <class, class Range, class... Closures>
    struct _pipe_invocable
        : std::false_type
    {};

    template <class Range, class... Closures>
    inline constexpr bool _pipe_invocable_v = _pipe_invocable<void, Range, Closures...>::value;

    template <class Range, class Closure>
    struct _pipe_invocable<std::void_t<decltype(std::declval<Closure&&>(std::declval<Range&&>()))>, Range, Closure>
        : std::true_type
    {};

    template <class Range, class Closure, class... Other>
    struct _pipe_invocable<std::enable_if_t<_pipe_invocable_v<Range, Closure>>, Range, Closure, Other...>
        : _pipe_invocable<void, Range, Other...>
    {};

    template <class Closure, class OtherClosure>
    struct _pipe<Closure, OtherClosure>
        : range_adaptor_closure<_pipe<Closure, OtherClosure>>
    {
        Closure _closure;
        OtherClosure _other_closure;

        template <class T, class U>
        constexpr _pipe(T&& t, U&& u)
            : _closure(std::forward<T>(t)), _other_closure(std::forward<U>(u))
        {}

        template <class Range,
                  class = std::enable_if_t<_pipe_invocable_v<Range, const Closure&, const OtherClosure&>>>
        constexpr auto operator()(Range&& r) const &
        {return _other_closure(_closure(std::forward<Range>(r)));}

        template <class Range,
                  class = std::enable_if_t<_pipe_invocable_v<Range, Closure, OtherClosure>>>
        constexpr auto operator()(Range&& r) &&
        {return std::move(_other_closure)(std::move(_closure)(std::forward<Range>(r)));}

        template <class Range,
          class = std::enable_if_t<_pipe_invocable_v<Range, Closure&, OtherClosure&>>>
        constexpr auto operator()(Range&& r) &
        {return _other_closure(_closure(std::forward<Range>(r)));}

        template <class Range,
                  class = std::enable_if_t<_pipe_invocable_v<Range, const Closure,const  OtherClosure>>>
        constexpr auto operator()(Range&& r) const&&
        {return std::move(_other_closure)(std::move(_closure)(std::forward<Range>(r)));}
    };

    //处理所有范围|算子-->返回闭包
    template <class Niebloid, class... Args>
    struct _range_adaptor
        : range_adaptor_closure<_range_adaptor<Niebloid, Args...>>
    {
    public:
        template <class... Types>
        constexpr explicit _range_adaptor(Types&&... args)
            : _args_tuple(std::forward<Types>(args)...)
        {}

        template <class Range, class = std::enable_if_t<std::is_invocable_v<Niebloid, Range, const Args&...>>>
        constexpr decltype(auto) operator()(Range&& r) const &
        {
            return this->_operator(std::forward<Range>(r), this->_args_tuple, std::index_sequence_for<Args...>{});
        }

        template <class Range, class = std::enable_if_t<std::is_invocable_v<Niebloid, Range, Args&&...>>>
        constexpr decltype(auto) operator()(Range&& r) &&
        {
            return this->_operator(std::forward<Range>(r), std::move(this->_args_tuple),
                                   std::index_sequence_for<Args...>{});
        }

    private:
        template <class Tuple, class Range, std::size_t... _indices>
        constexpr decltype(auto) _operator(Range&& _range, Tuple&& tuple, std::index_sequence<_indices...>)
        {
            return Niebloid{}(std::forward<Range>(_range), std::get<_indices>(std::forward<Tuple>(tuple))...);
        }

        std::tuple<Args...> _args_tuple;
    };
}

namespace
NAMESPACE_RANGES
{
    template <class T, class U, class = void>
    struct _enable_emplace_deref
        : std::false_type
    {};

    template <class T, class U>
    struct _enable_emplace_deref<T, U, std::void_t<decltype(T{*std::declval<U>()})>>
        : std::true_type
    {};

    template <class T>
    struct _non_propagating_cache
        : private std::optional<T>
    {
        using _base = std::optional<T>;

    public:
        static_assert(std::is_object_v<T>, "Constraints not satisfied");
        using _base::operator=;
        using _base::operator*;
        using _base::operator->;
        using _base::operator bool;
        using _base::has_value;
        using _base::emplace;
        using _base::reset;

        constexpr _non_propagating_cache() = default;

        constexpr _non_propagating_cache(const _non_propagating_cache&) noexcept
            : _base()
        {}

        constexpr _non_propagating_cache(_non_propagating_cache&& other) noexcept
        {other.reset();}

        constexpr _non_propagating_cache& operator=(const _non_propagating_cache& other) noexcept
        {
            if (std::addressof(other) != this)
                reset();
            return *this;
        }

        constexpr _non_propagating_cache& operator=(_non_propagating_cache&& other) noexcept
        {
            this->reset();
            other.reset();
            return *this;
        }

        template <class I, class = std::enable_if_t<_enable_emplace_deref<T, I>::value>>
        constexpr T& emplace_deref(const I& i)
        {
            return emplace(*i);
        }
    };

    template <class T, bool = forward_range<T>>
    struct _filter_view_begin
        : _non_propagating_cache<T>
    {
        using _base = _non_propagating_cache<T>;
        using _base::_base;
        using _base::operator=;
        using _base::operator*;
        using _base::operator->;
        using _base::operator bool;
        using _base::has_value;
        using _base::emplace;
        using _base::reset;
    };

    template <class T>
    struct _filter_view_begin<T, false>
    {};

    template <class V, class Pred>
    class filter_view
        : public view_interface<filter_view<V, Pred>>,
          public _enable_default_ctor<(default_initializable<V> && default_initializable<Pred>), V, Pred>
    {
        static_assert(indirect_unary_predicate<iterator_t<V>, Pred>
            && input_range<V>
            && view<V>
            && std::is_object_v<Pred>);

    private:
        template <class, class =void>
        struct _filter_view_cat
        {};

        template <class T>
        struct _filter_view_cat<T, std::enable_if_t<forward_range<T>>>
        {
            using Cat = typename std::iterator_traits<iterator_t<T>>::iterator_category;
            using iterator_category
            = std::conditional_t<derived_from<Cat, bidirectional_iterator_tag>,
                                 std::conditional_t<derived_from<Cat, forward_iterator_tag>, forward_iterator_tag, Cat>,
                                 void>;
        };

        struct Iterator
            : _filter_view_cat<V>,
              _enable_default_ctor<default_initializable<V>>
        {
            using value_type = range_value_t<V>;
            using difference_type = range_difference_t<V>;
            using iterator_concept = std::conditional_t<bidirectional_range<V>, bidirectional_iterator_tag,
                                                        std::conditional_t<
                                                            forward_range<V>, forward_iterator_tag, input_iterator_tag>>
            ;
            friend filter_view;

            iterator_t<V> _current{};
            filter_view* _parent = nullptr;

            Iterator() = default;

            constexpr Iterator(filter_view* parent, V current)
                : _current(std::move(current)), _parent(parent)
            {}

            constexpr const V& base() const & noexcept { return _current; }
            constexpr V base() && { return std::move(_current); }

            constexpr range_reference_t<V> operator*() const { return *_current; }

            template <class VV = V,
                      class = std::enable_if_t<(std::is_pointer_v<VV> || _has_arrow_operator_v<VV>) && copyable<VV>>>
            constexpr V operator->() const
            {
                return _current;
            }

            constexpr Iterator& operator++()
            {
                _current = ranges::find_if(std::move(++_current),
                                           ranges::end(_parent->_base),
                                           std::ref(*_parent->_pred));
                return *this;
            }

            constexpr void operator++(int) { ++*this; }

            template <class VV = V, class = std::enable_if_t<forward_range<VV>>>
            constexpr Iterator operator++(int)
            {
                auto tmp = *this;
                ++*this;
                return tmp;
            }

            template <class VV = V, class = std::enable_if_t<bidirectional_range<VV>>>
            constexpr Iterator& operator--()
            {
                do
                    --_current;
                while (!ranges::invoke(*_parent->_pred, *_current));
                return *this;
            }

            template <class VV = V, class = std::enable_if_t<bidirectional_range<VV>>>
            constexpr Iterator
            operator--(int)
            {
                auto tmp = *this;
                --*this;
                return tmp;
            }

            template <class VV = V, class = std::enable_if_t<equality_comparable<VV>>>
            friend constexpr bool
            operator==(const Iterator& x, const Iterator& y)
            {
                return x._current == y._current;
            }

            template <class VV = V, class = std::enable_if_t<equality_comparable<VV>>>
            friend constexpr bool
            operator!=(const Iterator& x, const Iterator& y)
            {
                return x._current != y._current;
            }

            friend constexpr range_rvalue_reference_t<V>
            iter_move(const Iterator& i)
                noexcept(noexcept(ranges::iter_move(i._current)))
            {
                return ranges::iter_move(i._current);
            }

            template <class VV = V, class = std::enable_if_t<indirectly_swappable<VV>>>
            friend constexpr void
            iter_swap(const Iterator& x, const Iterator& y)
                noexcept(noexcept(ranges::iter_swap(x._current, y._current)))
            {
                ranges::iter_swap(x._current, y._current);
            }
        };

        struct Sentinel
        {
        private:
            sentinel_t<V> _end = sentinel_t<V>();

        public:
            Sentinel() = default;

            constexpr explicit Sentinel(filter_view* parent)
                : _end(ranges::end(parent->_base))
            {}

            constexpr sentinel_t<V> base() const{return _end;}

            friend constexpr bool operator==(const Iterator& x, const Sentinel& y){return x._current == y._end;}

            friend constexpr bool
            operator!=(const Iterator& x, const Sentinel& y)
            {
                return x._current != y._end;
            }

            friend constexpr bool
            operator==(const Sentinel& y, const Iterator& x)
            {
                return x._current == y._end;
            }

            friend constexpr bool
            operator!=(const Sentinel& y, const Iterator& x)
            {
                return x._current != y._end;
            }
        };

        V _base = V();
        _details::movable_box<Pred, 0, copyable<Pred>> _pred;
        _filter_view_begin<ranges::iterator_t<V>> _cached_begin;

    public:
        filter_view() = default;

        constexpr filter_view(V base, Pred pred)
            : _base(std::move(_base)), _pred(std::move(pred))
        {}

        template <class VV =V, class = std::enable_if_t<copy_constructible<VV>>>
        constexpr V base() const & { return _base; }

        constexpr V base() && { return std::move(_base); }
        constexpr const Pred& pred() const { return *_pred; }

        constexpr Iterator begin()
        {
            if constexpr (!ranges::forward_range<V>)
                return {*this, ranges::find_if(_base, std::ref(*_pred))};
            else
            {
                if (!_cached_begin.has_value())
                    _cached_begin = ranges::find_if(_base, std::ref(*_pred)); // caching
                return {*this, _cached_begin.value()};
            }
        }

        constexpr auto end()
        {
            if constexpr (common_range<V>)
                return Iterator{this, ranges::end(_base)};
            else
                return Sentinel{this};
        }
    };

    template <class R, class = void>
    inline constexpr bool _simple_view = false;

    template <class R>
    inline constexpr bool _simple_view<R, std::enable_if_t<
                                           ranges::view<R> && ranges::range<const R> &&
                                           same_as<ranges::iterator_t<R>, ranges::iterator_t<const R>> &&
                                           same_as<ranges::sentinel_t<R>, ranges::sentinel_t<const R>>>> = true;

    template <class V>
    class take_view
        : public view_interface<take_view<V>>,
          public _enable_default_ctor<default_initializable<V>>
    {
        static_assert(view<V>);

        template <bool Const, class T>
        using _maybe_const_t = std::conditional_t<Const, const T, T>;

        template <bool Const>
        using CI = counted_iterator<iterator_t<_maybe_const_t<Const, V>>>;

        V _base{};
        range_difference_t<V> _count = 0;

        template <bool Const>
        class Sentinel
        {
            using Base = _maybe_const_t<Const, V>;

        private:
            sentinel_t<Base> _end;

        public:
            Sentinel() = default;

            constexpr explicit Sentinel(sentinel_t<Base> end)
                : _end(std::move(end))
            {}

            template <bool NoConst,
                      class = std::enable_if_t<(NoConst != Const)
                          && convertible_to<sentinel_t<V>, sentinel_t<Base>>>>
            constexpr Sentinel(Sentinel<NoConst> s)
                : _end(std::move(s._end))
            {}

            constexpr sentinel_t<Base> base() const
            {
                return _end;
            }

            friend constexpr bool
            operator==(const CI<Const>& y, const Sentinel& x)
            {
                return y.count() == 0 || y.base() == x._end;
            }

            friend constexpr bool
            operator==(const Sentinel& x, const counted_iterator<iterator_t<Base>>& y)
            {
                return y == x;
            }

            friend constexpr bool
            operator!=(const CI<Const>& y, const Sentinel& x)
            {
                return !(y == x);
            }

            friend constexpr bool
            operator!=(const Sentinel& x, const counted_iterator<iterator_t<Base>>& y)
            {
                return !(y == x);
            }

            friend constexpr bool
            operator!=(const Sentinel& x, const CI<Const>& y)
            {
                return !(y == x);
            }

            friend constexpr bool
            operator!=(const counted_iterator<iterator_t<Base>>& y, const Sentinel& x)
            {
                return !(y == x);
            }

            template <bool OtherConst = !Const,
                      class = std::enable_if_t<sentinel_for<
                          sentinel_t<Base>, iterator_t<_maybe_const_t<OtherConst, V>>>>>
            friend constexpr bool
            operator==(const CI<OtherConst>& y, const Sentinel& x)
            {
                return y.count() == 0 || y.base() == x._end;
            }

            template <bool OtherConst = !Const,
                      class = std::enable_if_t<sentinel_for<
                          sentinel_t<Base>, iterator_t<_maybe_const_t<OtherConst, V>>>>>
            friend constexpr bool
            operator==(const Sentinel& x, const CI<OtherConst>& y)
            {
                return y.count() == 0 || y.base() == x._end;
            }
        };

    public:
        constexpr take_view() = default;

        constexpr explicit take_view(V base, range_difference_t<V> n)
            : _base(std::move(base)), _count(n)
        {}

        template <class VV = V, class = std::enable_if_t<copy_constructible<VV>>>
        constexpr V base() const & { return _base; }

        constexpr V base() && { return std::move(_base); }

        template <class VV = V, class = std::enable_if_t<!_simple_view<VV>>>
        constexpr auto begin()
        {
            if constexpr (sized_range<VV>)
            {
                if constexpr (random_access_range<VV>)
                    return ranges::begin(_base);
                else
                    return counted_iterator{ranges::begin(_base), static_cast<range_difference_t<V>>(size())};
            }
            else
                return counted_iterator{ranges::begin(_base), _count};
        }

        template <class VV = V, class = std::enable_if_t<range<const VV>>>
        constexpr auto begin() const
        {
            if constexpr (sized_range<const VV>)
            {
                if constexpr (random_access_range<VV>)
                    return ranges::begin(_base);
                else
                    return counted_iterator{ranges::begin(_base), static_cast<range_difference_t<V>>(size())};
            }
            else
                return counted_iterator{ranges::begin(_base), _count};
        }

        template <class VV = V, class = std::enable_if_t<!_simple_view<VV>>>
        constexpr auto end()
        {
            if constexpr (sized_range<VV>)
            {
                if constexpr (random_access_range<VV>)
                    return ranges::begin(_base) + size();
                else
                    return default_sentinel;
            }
            else
                return Sentinel<false>{ranges::end(_base)};
        }

        template <class VV = V, class = std::enable_if_t<range<const VV>>>
        constexpr auto end() const
        {
            if constexpr (sized_range<const VV>)
            {
                if constexpr (random_access_range<const VV>)
                    return ranges::begin(_base) + size();
                else
                    return default_sentinel;
            }
            else
                return Sentinel<true>{ranges::end(_base)};
        }

        template <class VV = V, class = std::enable_if_t<sized_range<VV>>>
        constexpr auto size()
        {
            auto n = ranges::size(_base);
            return ranges::min(n, static_cast<decltype(n)>(_count));
        }

        template <class VV = V, class = std::enable_if_t<sized_range<const VV>>>
        constexpr auto size() const
        {
            auto n = ranges::size(_base);
            return ranges::min(n, static_cast<decltype(n)>(_count));
        }
    };
}

namespace
NAMESPACE_RANGES
{
    template <class Range>
    class ref_view
        : public view_interface<ref_view<Range>>
    {
        static_assert(range<Range> && std::is_object_v<Range>);

    private:
        Range* _M_r;

        static void Fun(Range&); // not defined
        static void Fun(Range&&) = delete;

        template <class T, class = void>
        struct _enable_dctor
            : std::false_type
        {};

        template <class T>
        struct _enable_dctor<T, std::void_t<decltype(Fun(std::declval<T>()))>>
            : std::true_type
        {};

    public:
        template <class T,REQUIRE(_enable_dctor<T>::value, different_from<ref_view, T>, convertible_to<T, Range&>)>
        constexpr ref_view(T&& t) noexcept(noexcept(static_cast<Range&>(std::declval<T>())))
            : _M_r(std::addressof(static_cast<Range&>(std::forward<T>(t))))
        {}

        constexpr Range& base() const { return *_M_r; }
        constexpr iterator_t<Range> begin() const { return ranges::begin(*_M_r); }
        constexpr sentinel_t<Range> end() const { return ranges::end(*_M_r); }
        constexpr bool empty() const { return ranges::empty(*_M_r); }

        template <class R = Range,REQUIRE(sized_range<R>)>
        constexpr auto size() const { return ranges::size(*_M_r); }

        template <class R = Range,REQUIRE(contiguous_range<R>)>
        constexpr auto data() const { return ranges::data(*_M_r); }
    };

    template <typename Range>
    ref_view(Range&) -> ref_view<Range>;

    template <typename T>
    inline constexpr bool enable_borrowed_range<ref_view<T>> = true;

    template <class Range>
    class owning_view
        : public view_interface<owning_view<Range>>,
          public _enable_default_ctor<default_initializable<Range>>
    {
        static_assert(range<Range> && movable<Range>);

    private:
        Range _M_r = Range();

    public:
        owning_view() = default;

        constexpr owning_view(Range&& t)
            noexcept(std::is_nothrow_move_constructible_v<Range>)
            : _M_r(std::move(t))
        {}

        owning_view(owning_view&&) = default;
        owning_view& operator=(owning_view&&) = default;

        constexpr Range& base() & noexcept { return _M_r; }
        constexpr const Range& base() const & noexcept { return _M_r; }
        constexpr Range&& base() && noexcept { return std::move(_M_r); }
        constexpr const Range&& base() const && noexcept { return std::move(_M_r); }

        constexpr iterator_t<Range> begin() { return ranges::begin(_M_r); }
        constexpr sentinel_t<Range> end() { return ranges::end(_M_r); }

        template <class R =Range,REQUIRE(range<const R>)>
        constexpr auto begin() const { return ranges::begin(_M_r); }

        template <class R =Range,REQUIRE(range<const R>)>
        constexpr auto end() const { return ranges::end(_M_r); }

        constexpr bool empty() { return ranges::empty(_M_r); }
        constexpr bool empty() const { return ranges::empty(_M_r); }

        template <class R =Range,REQUIRE(sized_range<R>)>
        constexpr auto size() { return ranges::size(_M_r); }

        template <class R =Range,REQUIRE(sized_range<const R>)>
        constexpr auto size() const { return ranges::size(_M_r); }

        template <class R =Range,REQUIRE(contiguous_range<R>)>
        constexpr auto data() { return ranges::data(_M_r); }

        template <class R =Range,REQUIRE(contiguous_range<const R>)>
        constexpr auto data() const { return ranges::data(_M_r); }
    };

    template <typename T>
    inline constexpr bool enable_borrowed_range<owning_view<T>> = enable_borrowed_range<T>;

    namespace views
    {
        template <typename, typename = void>
        inline constexpr bool _can_ref_view = false;
        template <typename, typename = void>
        inline constexpr bool _can_owning_view = false;
        template <class, class, typename = void>
        inline constexpr bool _can_filter_view = false;

        template <typename R>
        inline constexpr bool _can_ref_view<R, std::void_t<decltype(ref_view{std::declval<R>()})>> = true;

        template <typename R>
        inline constexpr bool _can_owning_view<R, std::void_t<decltype(owning_view<R>{std::declval<R>()})>> = true;

        template <class R, class P>
        inline constexpr bool _can_filter_view<R, P, std::void_t<decltype(filter_view(
                                                   std::declval<R>(), std::declval<P>()))>> = true;

        struct _all
            : range_adaptor_closure<_all>
        {
            template <class Range>
            constexpr auto operator() [[nodiscard]](Range&& r) const
            {
                if constexpr (view<std::decay_t<Range>>)
                    return std::forward<Range>(r);
                else if constexpr (_can_ref_view<Range>)
                    return ref_view{std::forward<Range>(r)};
                else if (_can_owning_view<Range>)
                    return owning_view{std::forward<Range>(r)};
            }
        };

        struct _filter
            : _range_adaptor<_filter>
        {
            template <class Range, class Pred,REQUIRE(_can_filter_view<Range, Pred>)>
            constexpr auto operator() [[nodiscard]](Range&& r, Pred&& p) const
            {
                return filter_view(std::forward<Range>(r), std::forward<Pred>(p));
            }

            using _range_adaptor<_filter>::operator();
        };

        inline constexpr _filter filter;
        inline constexpr _all all;

        template <class Range, class = std::enable_if_t<viewable_range<Range>>>
        using all_t = decltype(all(std::declval<Range>()));
    } // namespace views

    template <class Range, class Pred>
    filter_view(Range&&, Pred) -> filter_view<views::all_t<Range>, Pred>;

    template<typename Range>
    take_view(Range&&, range_difference_t<Range>)-> take_view<views::all_t<Range>>;

    template<typename T>
      inline constexpr bool enable_borrowed_range<take_view<T>> = enable_borrowed_range<T>;
}

namespace NAMESPACE_RANGES
{
    template<class T>struct _box;

    template<typename T, typename U>
  using _converts_from_box =
    std::disjunction<
     std:: is_constructible<T, const _box<U>&>,
     std:: is_constructible<T, _box<U>&>,
     std:: is_constructible<T, const _box<U>&&>,
     std:: is_constructible<T, _box<U>&&>,
     std:: is_convertible<const _box<U>&, T>,
     std:: is_convertible<_box<U>&, T>,
     std:: is_convertible<const _box<U>&&, T>,
     std:: is_convertible<_box<U>&&, T>>;

    template<typename T, typename U>
      using _assigns_from_box =
        std::disjunction<
          std::is_assignable<T&, const _box<U>&>,
          std::is_assignable<T&, _box<U>&>,
          std::is_assignable<T&, const _box<U>&&>,
          std::is_assignable<T&, _box<U>&&>>;

    template<typename T>
    inline constexpr bool  _movable_box_only_obj =
              copyable<T>
            ||(copy_constructible<T>&&std::is_nothrow_move_constructible_v<T>&&std::is_nothrow_copy_constructible_v<T> )
            ||(! copy_constructible<T>&&movable<T> )
            ||(!copy_constructible<T>&&std::is_nothrow_copy_constructible_v<T>);

    template<class T,bool = _movable_box_only_obj<T>>
    struct _movable_box_base
    {
        template<class U = T, std::enable_if_t<default_initializable<T>,int> = 0>
        constexpr _movable_box_base()noexcept(std::is_nothrow_default_constructible_v<T>)
            : _movable_box_base(std::in_place)
        {}

     constexpr _movable_box_base( std::nullopt_t ) noexcept{}

      constexpr _movable_box_base(const _movable_box_base&) = default;
      constexpr _movable_box_base(_movable_box_base&&) = default;
      constexpr _movable_box_base& operator=(const _movable_box_base&) = default;
      constexpr _movable_box_base& operator=(_movable_box_base&&) = default;

      template<class... Args, std::enable_if_t<std::is_constructible_v<T, Args...>, int> = 0>
      constexpr explicit _movable_box_base(std::in_place_t, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<T,  Args...>)
          : _value(std::forward<Args>(args)...)
    {}

    template<class... Args, class U,std::enable_if_t<std::is_constructible_v<T, std::initializer_list<U>&,Args...>, int> = 0>
    constexpr explicit _movable_box_base(std::in_place_t, std::initializer_list<U>list,Args&&... args)
        noexcept(std::is_nothrow_constructible<T,   std::initializer_list<U>,Args...>::value)
        : _value(list,std::forward<Args>(args)...)
    {}

        template<class U=std::remove_cv_t<T>, std::enable_if_t<
          !std::is_same_v<_movable_box_base, remove_cvref_t<U>>
            &&!std::is_same_v<remove_cvref_t<U>,std::in_place_t>
            &&std::is_constructible_v<T,U>,int> = 0>
        constexpr _movable_box_base(U&& arg)noexcept(std::is_nothrow_constructible_v<T, U>)
            :_value(std::forward<U>(arg))
        {}

    template<class U, std::enable_if_t<
      !std::is_same_v<_movable_box_base, remove_cvref_t<U>>
        &&!std::is_same_v<std::decay_t<U>,T>
        &&!std::is_scalar_v<T>
        &&std::is_constructible_v<T,U>
        &&std::is_assignable_v<T&, U>,int> = 0>
    constexpr _movable_box_base& operator=(U&& arg)noexcept(std::is_nothrow_assignable_v<T, U>)
    {
    _value = std::forward<U>(arg);
    return *this;
     }

  constexpr explicit operator bool() const noexcept { return true; }
  constexpr bool has_value() const noexcept { return true; }

  constexpr const T* operator->() const noexcept { return std::addressof(_value); }
  constexpr T* operator->() noexcept { return std::addressof(_value); }
  constexpr const T& operator*() const& noexcept { return _value; }
  constexpr T& operator*() & noexcept { return _value; }
  constexpr const T&& operator*() const&& noexcept { return std::move(_value); }
  constexpr T&& operator*() && noexcept { return std::move(_value); }

  constexpr void reset() noexcept { _value.~T(); }

  template<typename... Args, std::enable_if_t<std::is_constructible<T, Args...>::value, int> = 0>
  constexpr T& emplace(Args&&... args)
      noexcept(std::conjunction< std::is_nothrow_constructible<T, Args...>,std::is_nothrow_destructible<T> >::value)
  {
    _value.~T();
    ::new(this->operator->()) T(std::forward<Args>(args)...);
    return _value;
  }

  template<class U,class... Args, std::enable_if_t<std::is_constructible_v<T, std::initializer_list<U>, Args...>, int> = 0>
  constexpr T& emplace(std::initializer_list<U> il, Args&&... args)
      noexcept(std::is_nothrow_constructible_v<T, std::initializer_list<U>, Args...>&&std::is_nothrow_destructible_v<T>)
  {
    _value.~T();
    ::new(_voidify(_value)) T(il, std::forward<Args>(args)...);
    return _value;
  }

 private:
    T _value{};
    };

    template<class T>
    struct _movable_box_base<T, false>
        : private std::optional<T>
    {
        using _base = std::optional<T>;

        template<class U =T, std::enable_if_t<default_initializable<U>, int> = 0>
        constexpr _movable_box_base()
        noexcept(std::is_nothrow_default_constructible<T>::value)
            : _base(std::in_place)
        {}

        using _base::base;
        using _base::operator=;
        using _base::operator->;
        using _base::operator*;
        using _base::operator bool;
        using _base::has_value;
        using _base::reset;
        using _base::emplace;
    };

    template<class T, bool = copyable<T>&& copy_constructible<T> /* true */>
    struct _movable_box_copy_assign
        : _movable_box_base<T>
    {
      using _base = _movable_box_base<T>;
      using _base::_base;
    };

    template<typename T>
    struct _movable_box_copy_assign<T, false>
        : _movable_box_base<T>
    {
      using _base = _movable_box_base<T>;
      using _base::base;

  constexpr _movable_box_copy_assign() = default;
  constexpr _movable_box_copy_assign(const _movable_box_copy_assign&) = default;
  constexpr _movable_box_copy_assign(_movable_box_copy_assign&&) = default;

  _movable_box_copy_assign& operator=(const _movable_box_copy_assign& other)
      noexcept(std::is_nothrow_copy_constructible<T>::value)
  {
        if (this != std::addressof(other))
        {
          if (other)
            this->emplace(*other);
          else
            this->reset();
        }
        return *this;
  }
  constexpr _movable_box_copy_assign& operator=(_movable_box_copy_assign&&) = default;
};

    template<typename T, bool = movable<T>/* true */>
    struct _movable_box_move_assign
        : _movable_box_copy_assign<T>
        {
          using base = _movable_box_copy_assign<T>;
          using base::base;
    };

    template<typename T>
    struct  _movable_box_move_assign<T,false>
        : _movable_box_copy_assign<T>
    {
  using base = _movable_box_copy_assign<T>;
  using base::base;

  constexpr _movable_box_move_assign() = default;
  constexpr _movable_box_move_assign(const _movable_box_move_assign&) = default;
  constexpr _movable_box_move_assign(_movable_box_move_assign&&) = default;
  constexpr _movable_box_move_assign& operator=(const _movable_box_move_assign&) = default;
  constexpr _movable_box_move_assign& operator=(_movable_box_move_assign&& other)
      noexcept(std::is_nothrow_move_constructible<T>::value)
  {
    if (this != std::addressof(other))
        {
      if (other)
        this->emplace(std::move(*other));
      else
        this->reset();
    }
    return *this;
  }
};

    template<class T>
    class _box
        : public _movable_box_move_assign<T>
    {
        static_assert(move_constructible<T>, "Constraints not satisfied");
        static_assert(std::is_object<T>::value, "Constraints not satisfied");
        using _base = _movable_box_move_assign<T>;
        template<bool ... C>
        using _requires =std::enable_if_t<(C&&...), bool>;
    public:
        using _base::base;
        using _base::emplace;
        using _base::operator=;
        using _base::operator->;
        using _base::operator*;
        using _base::reset;
    public:
        template<typename _Up,
          _requires<!std::is_same_v<T, _Up>,
            std::is_constructible_v<T, const _Up&>,
            std::is_convertible_v<const _Up&, T>,
            !_converts_from_box<T, _Up>::value> = true>
       constexpr _box(const _box<_Up>& t)
       noexcept(std::is_nothrow_constructible_v<T, const _Up&>)
        {
            if (t)
                this->emplace(*t);
        }

        template<typename _Up,
      _requires<!std::is_same_v<T, _Up>,
        std::is_constructible_v<T, const _Up&>,
        !std::is_convertible_v<const _Up&, T>,
        !_converts_from_box<T, _Up>::value> = true>
        constexpr explicit _box(const _box<_Up>& t)
        noexcept(std::is_nothrow_constructible_v<T, const _Up&>)
        {
            if (t)
                this->emplace(*t);
        }

        template<typename _Up,
          _requires<!std::is_same_v<T, _Up>,
            std::is_constructible_v<T,  _Up>,
            std::is_convertible_v<_Up, T>,
            !_converts_from_box<T, _Up>::value> = true>
       _box(_box<_Up>&& t)
       noexcept(std::is_nothrow_constructible_v<T, const _Up&>)
        {
            if (t)
                this->emplace(std::move(*t));
        }

        template<typename _Up,
      _requires<!std::is_same_v<T, _Up>,
        std::is_constructible_v<T, const _Up&>,
        !std::is_convertible_v<const _Up&, T>,
        !_converts_from_box<T, _Up>::value> = true>
        explicit _box(_box<_Up>&& t)
        noexcept(std::is_nothrow_constructible_v<T, const _Up&>)
        {
            if (t)
                this->emplace(std::move(*t));
        }

    };
}
namespace NAMESPACE_RANGES
{
    template<class V,class F>
    class transform_view
      : public view_interface<transform_view<V, F>>,
    public _enable_default_ctor<(default_initializable<V>&& default_initializable<F>)>
    {
        static_assert(input_range<V>&&move_constructible<F>);
        static_assert( view<V> && std::is_object_v<F>
                            && regular_invocable<F&, range_reference_t<V>>
                             &&_can_reference<std::invoke_result_t<F&,range_reference_t<V>>>::value);
    private:
      template<bool Const>using Base = _maybe_const_t<Const, V>;

      template<bool v,bool = forward_range<Base<v>>>struct _iter_cat{ };

        template<bool Const>
        struct _iter_cat<Const,true>
	{
	private:
	  static auto _iter_category()
	  {
	    using _Base = transform_view::Base<Const>;
	    using _Res = std::invoke_result_t<F&, range_reference_t<_Base>>;
	    if constexpr (std::is_lvalue_reference_v<_Res>)
	      {
		using Cat= typename iterator_traits<iterator_t<_Base>>::iterator_category;
		  return Cat{};
	      }
	    else
	      return input_iterator_tag{};
	  }
	public:
	  using iterator_category = decltype(_iter_category());
	};

      template<bool>struct Sentinel;

      template<bool Const>
	struct Iterator : _iter_cat<Const>,
        _enable_default_ctor<default_initializable<iterator_t<Base<Const>>>>
	{
	private:
        using _iterator_parent = _maybe_const_t<Const, transform_view>;
        using _iterator_base = transform_view::Base<Const>;
          using _iterator_t = iterator_t<_iterator_base>;

	  static auto _traits_iter_concept()
	  {
	    if constexpr (random_access_range<_iterator_base>)
	      return random_access_iterator_tag{};
	    else if constexpr (bidirectional_range<_iterator_base>)
	      return bidirectional_iterator_tag{};
	    else if constexpr (forward_range<_iterator_base>)
	      return forward_iterator_tag{};
	    else
	      return input_iterator_tag{};
	  }

	  _iterator_t _current {};
	  _iterator_parent* _parent = nullptr;

	public:
	  using iterator_concept = decltype(_traits_iter_concept());
	  using value_type= remove_cvref_t<std::invoke_result_t<F&, range_reference_t<_iterator_base>>>;
	  using difference_type = range_difference_t<_iterator_base>;

	  Iterator()  = default;

	  constexpr Iterator(_iterator_parent* parent, _iterator_t current)
	    : _current(std::move(current)),_parent(parent)
	  { }

          template<class  VV = V,class = std::enable_if_t<Const&&convertible_to<iterator_t<V>, _iterator_t>>>
	  constexpr Iterator(Iterator<!Const> i)
	    : _current(std::move(i._current)), _parent(i._parent)
	  { }

	  constexpr const _iterator_t&base() const & noexcept{ return _current; }
	  constexpr _iterator_t base() &&{ return std::move(_current); }

	  constexpr decltype(auto)operator*() const
	  { return ranges::invoke(*_parent->_fun, *_current); }

	  constexpr Iterator&operator++()
	  {
	    ++_current;
	    return *this;
	  }

        template<class B = _iterator_base, std::enable_if_t<!forward_range<B>, int> = 0>
	  constexpr void operator++(int){ ++_current; }

        template<class B = _iterator_base, std::enable_if_t<forward_range<B>, int> = 0>
	  constexpr Iterator operator++(int)
	  {
	    auto tmp = *this;
	    ++*this;
	    return tmp;
	  }

        template<class B = _iterator_base, std::enable_if_t<bidirectional_range<B>, int> = 0>
	  constexpr Iterator&operator--()
	  {
	    --_current;
	    return *this;
	  }

          template<class B = _iterator_base, std::enable_if_t<bidirectional_range<B>, int> = 0>
	  constexpr Iterator operator--(int)
	  {
	    auto tmp = *this;
	    --*this;
	    return tmp;
	  }

	  constexpr Iterator&operator+=(difference_type n)
	  {
	    _current += n;
	    return *this;
	  }

          template<class B = _iterator_base, std::enable_if_t< random_access_range<B>, int> = 0>
	  constexpr Iterator&operator-=(difference_type n)
	  {
	    _current -= n;
	    return *this;
	  }

          template<class B = _iterator_base, std::enable_if_t< random_access_range<B>, int> = 0>
	  constexpr decltype(auto)operator[](difference_type n) const
	  { return ranges::invoke(*_parent->_M_fun, _current[n]); }

          template<class B = _iterator_base, std::enable_if_t< equality_comparable<B>, int> = 0>
	  friend constexpr bool operator==(const Iterator& x, const Iterator& y)
	  { return x._current == y._current; }

          template<class B = _iterator_base, std::enable_if_t< equality_comparable<B>, int> = 0>
friend constexpr bool operator!=(const Iterator& x, const Iterator& y)
	  { return x._current != y._current; }

          template<class B = _iterator_base, std::enable_if_t< random_access_range<B>, int> = 0>
	  friend constexpr bool operator<(const Iterator& x, const Iterator& y)
	  { return x._current < y._current; }

          template<class B = _iterator_base, std::enable_if_t< random_access_range<B>, int> = 0>
	  friend constexpr bool operator>(const Iterator& x, const Iterator& y)
	  { return y < x; }

          template<class B = _iterator_base, std::enable_if_t< random_access_range<B>, int> = 0>
	  friend constexpr bool operator<=(const Iterator& x, const Iterator& y)
	  { return !(y < x); }

          template<class B = _iterator_base, std::enable_if_t< random_access_range<B>, int> = 0>
	  friend constexpr bool operator>=(const Iterator& x, const Iterator& y)
	  { return !(x < y); }

          template<class B = _iterator_base, std::enable_if_t< random_access_range<B>, int> = 0>
	  friend constexpr Iterator operator+(Iterator i, difference_type n)
	  { return {i._parent, i._current + n}; }

          template<class B = _iterator_base, std::enable_if_t< random_access_range<B>, int> = 0>
	  friend constexpr Iterator operator+(difference_type n, Iterator i)
	  { return {i._parent, i._current + n}; }

          template<class B = _iterator_base, std::enable_if_t< random_access_range<B>, int> = 0>
	  friend constexpr Iterator operator-(Iterator i, difference_type n)
	  { return {i._parent, i._current - n}; }

          template<typename B = _iterator_base, std::enable_if_t<sized_sentinel_for<iterator_t<B>, iterator_t<B>>, int> = 0>
	  friend constexpr difference_type operator-(const Iterator& x, const Iterator& y)
	  { return x._current - y._current; }

	  friend constexpr decltype(auto)iter_move(const Iterator& i) noexcept(noexcept(*i))
	  {
	    if constexpr (std::is_lvalue_reference_v<decltype(*i)>)
	      return std::move(*i);
	    else
	      return *i;
	  }

	  friend Iterator<!Const>;
	  template<bool> friend struct Sentinel;
	};

      template<bool Const>
	struct Sentinel
	{
	private:
	  using _sentinel_parent =_maybe_const_t<Const, transform_view>;
	  using _sentinel_base = transform_view::Base<Const>;

	  sentinel_t<_sentinel_base> _end = sentinel_t<_sentinel_base>();

	public:
	  Sentinel() = default;

	  constexpr explicit Sentinel(sentinel_t<_sentinel_base> end)
	    : _end(end)
	  { }

          template<bool AntiConst, std::enable_if_t<
   ((Const != AntiConst) && Const)&&convertible_to<sentinel_t<V>, sentinel_t<_sentinel_base>>, int> = 0>
	  constexpr Sentinel(Sentinel<!AntiConst> i)
	    : _end(std::move(i._end))
	  { }

	  constexpr sentinel_t<_sentinel_base>base() const{ return _end; }

          friend constexpr bool operator==(const Iterator<Const>& x, const Sentinel& y){ return x._current==y._end; }
	    friend constexpr bool operator!=(const Iterator<Const>& x, const Sentinel& y){ return x._current!=y._end; }

	  template<class B = _sentinel_base,
		   REQUIRE(sized_sentinel_for<sentinel_t<B>, iterator_t<B>>)>
	    friend constexpr range_difference_t<_sentinel_base>
	    operator-(const Iterator<Const>& x, const Sentinel& y)
	    { return x._current - y._end; }

      template<class B = _sentinel_base,
           REQUIRE(sized_sentinel_for<sentinel_t<B>, iterator_t<B>>)>
        friend constexpr range_difference_t<_sentinel_base>
        operator-( const Sentinel& y,const Iterator<Const>& x)
	  { return  y._end-x._current ; }

	  friend Sentinel<!Const>;
	};

      V _base = V();
      _box<F> _fun;

    public:
      transform_view()= default;

      constexpr transform_view(V base, F fun)
	    : _base(std::move(base)), _fun(std::move(fun))
      { }

      constexpr V base() const&{ return _base ; }
      constexpr V base() &&{ return std::move(_base); }

      constexpr Iterator<false>begin(){ return Iterator<false>{this, ranges::begin(_base)}; }

        template<class VV = V,class FF =F,REQUIRE(range<const VV>,regular_invocable<const FF&, range_reference_t<const VV>>)>
      constexpr Iterator<true>begin() const
      { return Iterator<true>{this, ranges::begin(_base)}; }

      constexpr Sentinel<false>end(){ return Sentinel<false>{ranges::end(_base)}; }

        template<class VV =V,class = std::enable_if_t<common_range<VV>>>
      constexpr Iterator<false>end()
      { return Iterator<false>{this, ranges::end(_base)}; }

        template<class VV =V,class FF= F,
                class = std::enable_if_t<range<VV>
                                &&regular_invocable<const FF&, range_reference_t<const VV>>>>
      constexpr Sentinel<true>end() const
      { return Sentinel<true>{ranges::end(_base)}; }

        template<class VV =V,class FF= F,
            class = std::enable_if_t<common_range<const VV>
                                                &&regular_invocable<const FF&, range_reference_t<const VV>>>>
      constexpr Iterator<true>end() const
      { return Iterator<true>{this, ranges::end(_base)}; }

        template<class VV= V,REQUIRE(sized_range<VV>)>
      constexpr auto size(){ return ranges::size(_base); }

        template<class VV= V,REQUIRE(sized_range<const VV>)>
      constexpr auto size() const{ return ranges::size(_base); }
    };

  template<typename Range, typename F>
    transform_view(Range&&, F) -> transform_view<views::all_t<Range>, F>;

    template<class T>
    CXX17_CONCEPT  _integer_like_with_usable_difference_type =
        scaler::_details::_is_signed_integer_like<T>||(scaler::_details::_is_integer_like<T>&&weakly_incrementable<T>);

        template<typename T, typename Bound = unreachable_sentinel_t>
        class repeat_view
            : public view_interface<repeat_view<T, Bound>>
    {
         public:
          static_assert(move_constructible<T>);
          static_assert(semiregular<Bound>);
          static_assert(std::is_object<T>::value);
          static_assert(same_as<T, std::remove_cv_t<T>>);
          static_assert(_integer_like_with_usable_difference_type<Bound>||same_as<Bound, unreachable_sentinel_t>);

          class Iterator
            {
            friend class repeat_view;
            using index_type = std::conditional_t<same_as<Bound, unreachable_sentinel_t>, std::ptrdiff_t, Bound>;

           public:
            using iterator_concept = random_access_iterator_tag;
            using iterator_category = random_access_iterator_tag;
            using value_type = T;
            using difference_type = std::conditional_t<
                scaler::_details::_is_signed_integer_like<index_type>, index_type, _iota_diff_t<index_type>>;
            using pointer = void;
            using reference =  const T&;


            constexpr Iterator() : _value(nullptr), _current() {}

            constexpr const T& operator*() const noexcept {return *_value;}

            constexpr const T& operator[](difference_type n) const noexcept {return *(*this + n);}

            constexpr Iterator& operator++()
              {
              ++_current;
              return *this;
            }

            constexpr Iterator operator++(int)
              {
              auto tmp = *this;
              ++*this;
              return tmp;
            }

            constexpr Iterator& operator--()
              {
              --_current;
              return *this;
            }

            constexpr Iterator operator--(int)
              {
              auto tmp = *this;
              --*this;
              return tmp;
            }

            constexpr Iterator& operator+=(difference_type n)
              {
              _current += n;
              return *this;
            }

            constexpr Iterator& operator-=(difference_type n)
              {
              _current -= n;
              return *this;
            }

            friend constexpr bool operator==(const Iterator& x, const Iterator& y)
              {return x._current == y._current;}

            friend constexpr bool operator!=(const Iterator& x, const Iterator& y) {return !(x == y);}

            friend constexpr bool operator<(const Iterator& x, const Iterator& y)
              {return x._current < y._current;}

            friend constexpr bool operator<=(const Iterator& x, const Iterator& y)
              {return (x == y) || (x < y);}

            friend constexpr bool operator>(const Iterator& x, const Iterator& y)
              {
              return y < x;
            }

            friend constexpr bool operator>=(const Iterator& x, const Iterator& y)
              {
              return !(x < y);
            }

            friend constexpr Iterator operator+(Iterator i, difference_type n)
              {
              i += n;
              return i;
            }

            friend constexpr Iterator operator+(difference_type n, Iterator i)
              {
              i += n;
              return i;
            }

            friend constexpr Iterator operator-(Iterator i, difference_type n)
              {
              i -= n;
              return i;
            }

            friend constexpr difference_type operator-(const Iterator& x, const Iterator& y)
              {
              return static_cast<difference_type>(x._current) - static_cast<difference_type>(y._current);
            }

          private:
           constexpr explicit Iterator(const T* value, index_type b = index_type())
               : _value(value), _current(b)
           {}

            const T* _value;
            index_type _current;
          };

          template<class B =T, std::enable_if_t<default_initializable<B>, int> = 0>
          constexpr repeat_view() {}

          template<class B= T, std::enable_if_t<copy_constructible<B>, int> = 0>
          constexpr explicit repeat_view(const B& value, Bound bound = Bound())
              : _value(value)
              , _bound(bound)
            {}

          constexpr explicit repeat_view(T&& value, Bound bound = Bound())
              : _value(std::move(value))
              , _bound(bound)
            {}

          template<typename... TArgs, typename... BoundArgs, std::enable_if_t<
              constructible_from<T, TArgs...>&&
              constructible_from<Bound, BoundArgs...>, int> = 0>
          constexpr repeat_view(std::piecewise_construct_t, std::tuple<TArgs...> _args,
              std::tuple<BoundArgs...> bound = std::tuple<>())
              : repeat_view(std::piecewise_construct, std::move(_args), std::move(bound),
                            std::index_sequence_for<TArgs...>{}, std::index_sequence_for<BoundArgs...>{})
            {}

          constexpr Iterator begin() const {return iterator(std::addressof(*_value));}

          template<typename B = Bound, std::enable_if_t<same_as<B, unreachable_sentinel_t> , int> = 0>
          constexpr  Iterator end() const {return iterator(std::addressof(*_value), _bound);}

          template<typename B = Bound, std::enable_if_t<same_as<B, unreachable_sentinel_t>, int> = 0>
          constexpr unreachable_sentinel_t end() const
            {return unreachable_sentinel;}

          template<typename B = Bound, std::enable_if_t<!same_as<B, unreachable_sentinel_t> ,int> = 0>
          constexpr auto size() const
            {return std::make_unsigned_t<Bound>(_bound);}

         private:

            //代替make_from_tuple
          template<class Tuple, class BoundTuple, std::size_t... _indices, std::size_t... _bound_indices>
          constexpr repeat_view(std::piecewise_construct_t,
              Tuple&& value, BoundTuple&& bound,
              std::index_sequence<_indices...>, std::index_sequence<_bound_indices...>)
                : _value(std::get<_indices>(std::forward<Tuple>(value))...),
                _bound(std::get<_bound_indices>(std::forward<BoundTuple>(bound))...)
            {}

          _box<T> _value;
          Bound _bound{};
        };

        template<typename T, typename Bound>
        repeat_view(T, Bound) -> repeat_view<T, Bound>;

    namespace views
    {
        struct _repeat
        {
            template<typename T>
            constexpr auto
            operator() [[nodiscard]] (T&& value) const
            { return repeat_view(std::forward<T>(value)); }

            template<typename T, typename Bound>
            constexpr auto
            operator() [[nodiscard]] (T&& value, Bound bound) const
            { return repeat_view(std::forward<T>(value), bound); }
        };

        inline constexpr _repeat repeat;

#define VOID(...)std::void_t<decltype(__VA_ARGS__)>

        template<class Range, class F,class = void>
      inline constexpr bool _can_transform_view = false;

        template<class Range, class F>
        inline constexpr bool _can_transform_view<Range,F,VOID(transform_view(std::declval<Range>(), std::declval<F>()))> = true;

    struct _transform
        : _range_adaptor<_transform>
    {
        template<class Range, class F,REQUIRE(viewable_range<Range>,_can_transform_view<Range,F>)>
      constexpr auto operator() [[nodiscard]] (Range&& r, F&& f) const
      {return transform_view(std::forward<Range>(r), std::forward<F>(f));}

        using _range_adaptor<_transform>::operator();
    };

    inline constexpr _transform transform;

        template<class>inline constexpr bool _is_subrange_v = false;

        template<class I,class S,subrange_kind K>
        inline constexpr bool _is_subrange_v<subrange<I,S,K>> = true;

        struct _take
            : _range_adaptor<_take>
        {
            template<class Range, class D = range_difference_t<Range>>
          constexpr auto operator() [[nodiscard]] (Range&& r, type_identity_t<D> n) const
          {
              using T = remove_cvref_t<Range>;
              if constexpr (_is_specialization_of_v<empty_view ,T>)
                  return T();
              else if constexpr (random_access_range<T>
                         && sized_range<T>
                         && (_span::_is_span_v<T>
                         || _is_specialization_of_v<std::basic_string_view,T>
                         || _is_subrange_v<T>
                         || _is_specialization_of_v<iota_view,T>))
              {
                  n = std::min<D>(ranges::distance(r), n);
                  auto begin = ranges::begin(r);
                  auto end = begin + n;
                  if constexpr (_span::_is_span_v<T>)
                      return span<typename T::element_type>(begin, end);
                  else if constexpr (_is_specialization_of_v<std::basic_string_view,T>)
                      return T(begin, end);
                  else if constexpr (_is_subrange_v<T>)
                      return subrange<iterator_t<T>>(begin, end);
                  else
                      return iota_view(*begin, *end);
              }
              else if constexpr (_is_specialization_of_v<repeat_view,T>)
                  if constexpr (sized_range<T>)
                      return views::repeat(*std::forward<Range>(r)._value,std::min(ranges::distance(r), n));
                  else
                      return views::repeat(*std::forward<Range>(r)._value, n);
              else
                  return take_view(std::forward<Range>(r), n);
          }

            using _range_adaptor<_take>::operator();
        };

        inline constexpr _take take;
    }//views
}
namespace NAMESPACE_RANGES
{
    template<class T,bool = !sized_range<T>&&!random_access_range<T>&&forward_range<T>>
    struct _drop_base
    {
        _non_propagating_cache<T> _cached;

        constexpr auto _begin()
        {
            return _cached.has_value();
        }
    };

    template<class T>
    struct _drop_base<T,false>
    {};

  template<class V>
    class drop_view
      : public view_interface<drop_view<V>>,
        public _drop_base<V>,
        public _enable_default_ctor<default_initializable<V>>
    {
      static_assert(view<V>);
#define DROP(...)template<class VV =V,class = std::enable_if_t<(__VA_ARGS__)>>
      static constexpr bool _cached_begin
    = !(random_access_range<const V> && sized_range<const V>);
    private:
      V _base = V();
      range_difference_t<V> _count = 0;

    public:
      drop_view()  = default;

      constexpr drop_view(V base, range_difference_t<V> n)
	    : _base(std::move(base)), _count( n)
      {  }

      template<class VV =V,std::enable_if_t<copy_constructible<VV>,int> =0>
      constexpr V base() const& { return _base; }

      constexpr V base() &&{ return std::move(_base); }

      // 返回指向 drop_view 首元素的迭代器，即指向底层视图的第 N 个元素的迭代器
      DROP((!(_simple_view<V>
            && random_access_range<const V>
            && sized_range<const V>)))
      constexpr auto begin()
      {
          if constexpr (random_access_range<V> && sized_range<V>)
         {
              const auto dist = std::min(ranges::distance(_base), _count);
              return ranges::begin(_base) + dist;
          }
          auto tmp = ranges::next(ranges::begin(_base), _count, ranges::end(_base));
          if constexpr (_cached_begin)
          {
              this->_cached.emplace(tmp);
              if (this->_cached.has_value())
                  return *this->_cached;
          }
          return tmp;
      }

      DROP(( random_access_range<const V>
            && sized_range<const V>))
      constexpr auto begin()const
      {
	return ranges::next(ranges::begin(_base), _count,ranges::end(_base));
      }

      DROP(_simple_view<VV>)
      constexpr auto end() { return ranges::end(_base); }

      DROP(range<const VV>)
      constexpr auto end() const { return ranges::end(_base); }

      DROP(sized_range<VV>)
      constexpr auto size()
      {
	const auto s = ranges::size(_base);
	const auto c = static_cast<decltype(s)>(_count);
	return s < c ? 0 : s - c;
      }

      DROP(sized_range<const VV>)
      constexpr auto size() const
      {
          const auto s = ranges::size(_base);
          const auto c = static_cast<decltype(s)>(_count);
          return s < c ? 0 : s - c;
      }
    };
#undef DROP
  template<typename Range>
    drop_view(Range&&, range_difference_t<Range>)
      -> drop_view<views::all_t<Range>>;

  template<class T>
    inline constexpr bool enable_borrowed_range<drop_view<T>> = enable_borrowed_range<T>;

  namespace views
  {
    namespace _details
    {
      template<typename , typename,class = void >
	    struct  _can_drop_view
          :std::false_type
        {};

        template<class R,class D>
        struct _can_drop_view<R,D,VOID(drop_view(std::declval<R>(), std::declval<D>()))>
            :std::true_type
        {};
    } // namespace __detail

    struct _drop
        : _range_adaptor<_drop>
    {
      template<class Range, class D = range_difference_t<Range>,
        REQUIRE(viewable_range<Range>,_details::_can_drop_view<Range,D>::value)>
	constexpr auto
	operator() [[nodiscard]] (Range&& r, type_identity_t<D> n) const
	{
	  using T = remove_cvref_t<Range>;
	if constexpr (_is_specialization_of_v<empty_view ,T>)
	    return T();
	else if constexpr (random_access_range<T>
               && sized_range<T>
               && (_span::_is_span_v<T>
               || _is_specialization_of_v<std::basic_string_view,T>
               || _is_subrange_v<T>
               || _is_specialization_of_v<iota_view,T>))
	    {
	        n = std::min<D>(ranges::distance(r), n);
	        auto begin = ranges::begin(r);
	        auto end = begin + n;
	      if constexpr (_span::_is_span_v<T>)
		    return span<typename T::element_type>(begin, end);
	      else if constexpr (_is_subrange_v<T>)
		    {
		  if constexpr (T::_S_store_size)
		    {
		      auto m = ranges::distance(r) - n;
		      return T(begin, end, std::make_unsigned_t<decltype(m)>(m));
		    }
		  else
		      return T(begin, end);
		}
	      else
		    return T(begin, end);
	    }
	    else if constexpr (_is_specialization_of_v<repeat_view,T>)
	        if constexpr (sized_range<T>)
	            return views::repeat(*std::forward<Range>(r)._value,std::min(ranges::distance(r), n));
	        else
	            return views::repeat(*std::forward<Range>(r)._value, n);
	  else
	    return drop_view(std::forward<Range>(r), n);
	}

      using _range_adaptor<_drop>::operator();
    };

    inline constexpr _drop drop;
  } // namespace views

  template<class _Vp, class _Pred>
    class drop_while_view
      : public view_interface<drop_while_view<_Vp, _Pred>>,
    public _enable_default_ctor<(default_initializable<_Vp>
                  && default_initializable<_Pred>)>
    {
      static_assert(view<_Vp>&&input_range<_Vp> && std::is_object_v<_Pred>
          && indirect_unary_predicate<const _Pred, iterator_t<_Vp>>);
    private:
      _Vp _M_base = _Vp();
      _box<_Pred> _M_pred;
      _non_propagating_cache<_Vp> _M_cached_begin;

    public:
      drop_while_view() = default;

      constexpr drop_while_view(_Vp __base, _Pred __pred)
	: _M_base(std::move(__base)), _M_pred(std::move(__pred))
      { }

        template<class VV=_Vp,class = std::enable_if_t<copy_constructible<_Vp>>>
      constexpr _Vp base() const& { return _M_base; }

      constexpr _Vp base() &&{ return std::move(_M_base); }

      constexpr const _Pred&pred() const
      { return *_M_pred; }

      constexpr auto begin()
      {
	if (_M_cached_begin.has_value())
	  return *_M_cached_begin;

	auto it = ranges::find_if_not(ranges::begin(_M_base),
					ranges::end(_M_base),
					std::cref(*_M_pred));
	_M_cached_begin.emplace(it);
	return it;
      }

      constexpr auto end(){ return ranges::end(_M_base); }
    };

  template<typename Range, typename Pred>
    drop_while_view(Range&&, Pred)-> drop_while_view<views::all_t<Range>, Pred>;

  template<typename T, typename Pred>
    inline constexpr bool enable_borrowed_range<drop_while_view<T, Pred>> = enable_borrowed_range<T>;

  namespace views
  {
    namespace _details
    {
        template<typename , typename,class = void >
        struct  _can_drop_while_view
            :std::false_type
        {};

        template<class R,class D>
        struct _can_drop_while_view<R,D,VOID(drop_while_view(std::declval<R>(), std::declval<D>()))>
            :std::true_type
        {};
    } // namespace __detail

    struct _drop_while
        : _range_adaptor<_drop_while>
    {
      template<class Range, class Pred,REQUIRE(_details::_can_drop_while_view<Range,Pred>::value,viewable_range<Range>)>
	constexpr auto operator() [[nodiscard]] (Range&& r, Pred&& p) const
	{return drop_while_view(std::forward<Range>(r),std::forward<Pred>(p));}

      using _range_adaptor<_drop_while>::operator();
    };

    inline constexpr _drop_while drop_while;
  } // namespace views

    template <class T>
    constexpr T& _as_lvalue(T&& x) noexcept {return static_cast<T&>(x);}

  template<class  V>
    class join_view
      : public view_interface<join_view<V>>,
        public  _enable_default_ctor<default_initializable<V>>
    {
      static_assert(input_range<V>&&view<V> && input_range<range_reference_t<V>>);

      template<bool>struct Sentinel;

      template<bool Const,class Parent,class Base>
      struct _iterator_base
      {
          using iterator_concept =_switch_t<bool_sequence<
              std::is_reference_v<range_reference_t<Base>>
                  && bidirectional_range<Base>
                  && bidirectional_range<range_reference_t<Base>>
                  && common_range<range_reference_t<Base>>,
              std::is_reference_v<range_reference_t<Base>>
                       && forward_range<Base>
                       && forward_range<range_reference_t<Base>>
              >//bool_sequence
              ,meta_list<bidirectional_iterator_tag,forward_iterator_tag>,
              input_iterator_tag
              >;
      };

      template<bool Const,class Parent,class Base,class = void>
      struct _iterator_category
      {};

      template<bool Const,class Parent,class Base>
      struct _iterator_category<Const,Parent,Base,
      std::enable_if_t<std::is_same_v<typename _iterator_base<Const,Parent,Base>::iterator_concept,std::forward_iterator_tag>>>
      {
          using  OUTERC
                = typename iterator_traits<ranges::iterator_t<Base>>::iterator_category;
          using INNERC
                = typename iterator_traits<ranges::iterator_t<ranges::range_reference_t<Base>>>::iterator_category;

          using iterator_category = std::conditional_t<
              derived_from<OUTERC,bidirectional_iterator_tag>&&derived_from<INNERC,bidirectional_iterator_tag>,
            bidirectional_iterator_tag,
          std::conditional_t<derived_from<OUTERC,forward_iterator_tag>&&derived_from<INNERC,forward_iterator_tag>,
          forward_iterator_tag,input_iterator_tag>
          >;
      };

      template<bool Const>
	struct Iterator
          :_enable_default_ctor<
              default_initializable<iterator_t<ranges::range_reference_t<_maybe_const_t<Const,V>>>>&&
              default_initializable< iterator_t<_maybe_const_t<Const,V>>>>,
      _iterator_category<Const,_maybe_const_t<Const, join_view>,_maybe_const_t<Const,V>>
	{
	private:
	  using Parent = _maybe_const_t<Const, join_view>;
	  using Base = _maybe_const_t<Const,V>;
        using OuterIter = iterator_t<Base>;
        using InnerIter =iterator_t<ranges::range_reference_t<_maybe_const_t<Const,V>>>;

          friend join_view;

         //跳过空的内部范围并初始化底层迭代器 _inner
	  constexpr void _satisfy()
	  {
	    auto _update_inner = [this] (const iterator_t<Base>& x) -> auto&& {
	      if constexpr (std::is_reference_v<range_reference_t<Base>>)
		return *x;
	      else
		return _parent->_inner.emplace_deref(x);
	    };

	    OuterIter& outer = _outer;
	    for (; outer != ranges::end(_parent->_base); ++outer)
	      {
		auto&& inner = _update_inner(outer);
		_inner = ranges::begin(inner);
		if (_inner != ranges::end(inner))
		  return;
	      }

	    if constexpr (std::is_reference_v<range_reference_t<Base>>)
	      _inner.reset();
	  }

          //一个外部迭代器
	     OuterIter _outer;
         // 一个内部迭代器
	  std::optional<InnerIter> _inner;
	  Parent* _parent = nullptr;

	public:
	  using iterator_concept =typename _iterator_base<Const,Parent,Base>::iterator_concept;
	  using value_type = range_value_t<range_reference_t<Base>>;
	  using difference_type= std::common_type_t<range_difference_t<Base>,range_difference_t<range_reference_t<Base>>>;

	  Iterator() = default;

          template<class VV=V,std::enable_if_t<Const &&
             convertible_to<ranges::iterator_t<V>, OuterIter> &&
             convertible_to<ranges::iterator_t< range_reference_t<V>>, InnerIter>,int> =0>
	  constexpr Iterator(Iterator<!Const> i)
	    : _outer(std::move(i._outer)), _inner(std::move(i._inner)),
	      _parent(i._parent)
	  { }

	  constexpr decltype(auto)operator*() const{ return **_inner; }

          template<class I = InnerIter,class = std::enable_if_t<_has_arrow_operator_v<InnerIter>
          && copyable<InnerIter>>>
	  constexpr InnerIter operator->() const
	  { return *_inner; }

	  constexpr Iterator&operator++()
	  {
	    auto&& _inner_range = [this] () -> auto&&
	    {
	      if constexpr (std::is_reference_v<ranges::range_reference_t<Base>>)
		    return *_outer;
	      else
		    return *_parent->_inner;
	    }();
	    if (++*_inner == ranges::end(_inner_range))
	      {
		    ++_outer;
		    this->_satisfy();
	      }
	    return *this;
	  }

	  constexpr void operator++(int){ ++*this; }

          template<class B= Base,
                class = std::enable_if_t<std::is_reference_v<ranges::range_reference_t<B>>
                      && forward_range<B>
                      && forward_range<range_reference_t<B>>>>
	  constexpr Iterator operator++(int)
	  {
	    auto tmp = *this;
	    ++*this;
	    return tmp;
	  }

          template<class B= Base,
        class = std::enable_if_t<std::is_reference_v<ranges::range_reference_t<B>>
                && bidirectional_range<B>
              && bidirectional_range<range_reference_t<B>>
              && common_range<range_reference_t<B>>>>
	  constexpr Iterator&operator--()
	  {
	    if (_outer == ranges::end(_parent->_base))
	      _inner = ranges::end(_as_lvalue(*--_outer));
	    while (*_inner == ranges::begin(_as_lvalue(*_outer)))
	      *_inner = ranges::end(_as_lvalue(*--_outer));
	    --*_inner;
	    return *this;
	  }

          template<class B= Base,
        class = std::enable_if_t<std::is_reference_v<ranges::range_reference_t<B>>
                && bidirectional_range<B>
              && bidirectional_range<range_reference_t<B>>
              && common_range<range_reference_t<B>>>>
	  constexpr Iterator operator--(int)
	  {
	    auto tmp = *this;
	    --*this;
	    return tmp;
	  }

        template<class B=Base,class = std::enable_if_t< std::is_reference_v<ranges::range_reference_t<B>>
        && forward_range<B>
        && equality_comparable<InnerIter>>>
        friend constexpr bool operator==(const Iterator& x, const Iterator& y)
        {return (x._outer == y._outer&& x._inner == y._inner);}

        template<class B=Base,class = std::enable_if_t< std::is_reference_v<ranges::range_reference_t<B>>
        && forward_range<B>
        && equality_comparable<InnerIter>>>
        friend constexpr bool operator!=(const Iterator& x, const Iterator& y)
        {return !(x==y);}

	  friend constexpr decltype(auto)iter_move(const Iterator& i)
	  noexcept(noexcept(ranges::iter_move(*i._inner)))
	  { return ranges::iter_move(*i._inner); }

	  friend constexpr void iter_swap(const Iterator& x, const Iterator& y)
	    noexcept(noexcept(ranges::iter_swap(*x._inner, *y._inner)))
	    requires indirectly_swappable<InnerIter>
	  { return ranges::iter_swap(*x._inner, *y._inner); }

	  friend Iterator<!Const>;
	  template<bool> friend struct Sentinel;
	};

    template<bool Const>
	struct Sentinel
	{
	private:
      using Parent = _maybe_const_t<Const, join_view>;
      using Base = _maybe_const_t<Const,V>;

	  sentinel_t<Base> _end = sentinel_t<Base>();

	public:
	  Sentinel() = default;

	  constexpr explicit Sentinel(Parent&parent)
	    : _end(ranges::end(parent._base))
	  { }

        template<class B=Base,class VV= V,
           std::enable_if_t<Const &&convertible_to<ranges::sentinel_t<V>, ranges::sentinel_t<Base>>,int> =0>
	  constexpr Sentinel(Sentinel<!Const> s)
	    : _end(std::move(s._end))
	  { }

	    friend constexpr bool operator==(const Iterator<Const>& x, const Sentinel& y)
	    { return y._end==x._outer; }

        friend constexpr bool operator!=(const Iterator<Const>& x, const Sentinel& y)
	  { return y._end!=x._outer; }

	  friend Sentinel<!Const>;
	};

      V _base = V();
	_maybe_present_t<!forward_range<V>,
	 _non_propagating_cache<iterator_t<V>>> _outer;
     _non_propagating_cache<std::remove_cv_t<range_reference_t<V>>> _inner;

    public:
      join_view()  = default;

      constexpr explicit join_view(V base)
	    : _base(std::move(base))
      { }

      template<class VV =V,class = std::enable_if_t<copy_constructible<VV>>>
      constexpr V base() const&{ return _base; }

      constexpr V base() &&{ return std::move(_base); }

      constexpr auto begin()
      {
	if constexpr (forward_range<V>)
	    return Iterator<(_simple_view<V>
         && std::is_reference_v<range_reference_t<V>>)>{this, ranges::begin(_base)};
	else
	  {
	    _outer = ranges::begin(_base);
	    return Iterator<false>{this};
	  }
      }

      template<class VV= V,REQUIRE(ranges::input_range<const VV> &&
             std::is_reference_v<ranges::range_reference_t<const VV>>)>
      constexpr auto begin() const{return Iterator<true>{this, ranges::begin(_base)};}

      //返回表示join_view末尾的哨兵或迭代器。
      constexpr auto end()
      {
          if constexpr (ranges::forward_range<V> &&
                    std::is_reference_v<ranges::range_reference_t<V>> &&
                    ranges::forward_range<ranges::range_reference_t<V>> &&
                    ranges::common_range<V> &&
                    ranges::common_range<ranges::range_reference_t<V>>)
	  return Iterator<_simple_view<V>>{this,ranges::end(_base)};
	else
	  return Sentinel<_simple_view<V>>{this};
      }

      template<class VV =V,class= std::enable_if_t<forward_range<const VV>
      && std::is_reference_v<range_reference_t<const VV>>
      && input_range<range_reference_t<const VV>>>>
      constexpr auto end() const
      {
	if constexpr (std::is_reference_v<range_reference_t<const V>>
		      && forward_range<range_reference_t<const V>>
		      && common_range<const V>
		      && common_range<range_reference_t<const V>>)
	  return Iterator<true>{this, ranges::end(_base)};
	else
	  return Sentinel<true>{this};
      }
    };

  template<typename Range>
    explicit join_view(Range&&) -> join_view<views::all_t<Range>>;

  namespace views
  {
    namespace _details
    {
      template<typename,class = void>
	inline  constexpr bool _can_join_view = false;

        template<typename R>
    inline  constexpr bool _can_join_view<R,std::void_t<decltype(join_view<all_t<R>>{std::declval<R>()})>> = true;
    }

    struct _join
        : range_adaptor_closure<_join>
    {
      template<class R,class = std::enable_if_t<_details::_can_join_view<R>&&viewable_range<R>>>
	constexpr auto operator() [[nodiscard]] (R&& r) const
	{return join_view<all_t<R>>{std::forward<R>(r)};}

    };

    inline constexpr _join join;
  } // namespace views

  namespace _details
  {
    template<class Range,class = void>
    struct _tiny_range
        :std::true_type
    {};

    template<class Range>
    struct _tiny_range<Range,std::void_t<decltype(std::remove_reference_t<Range>::size())>>
        :std::bool_constant<sized_range<Range>&&std::remove_reference_t<Range>::size()<=1>
    {};

    template<typename Base,bool = forward_range<Base>>
      struct _lazy_split_view_outer_iter_cat
      { };

    template<class Base>
      struct _lazy_split_view_outer_iter_cat<Base,true>
      { using iterator_category = input_iterator_tag; };

      template<typename Base,bool = forward_range<Base>>
      struct _lazy_split_view_inner_iter_cat
      { };

    template<class Base>
      struct _lazy_split_view_inner_iter_cat<Base,true>
      {
      private:
	static constexpr auto _iter_cat()
	{
	  using Cat = typename iterator_traits<iterator_t<Base>>::iterator_category;
	  if constexpr (derived_from<Cat, forward_iterator_tag>)
	    return forward_iterator_tag{};
	  else
	    return Cat{};
	}
      public:
	using iterator_category = decltype(_iter_cat());
      };
  }

  template<class V,class Pattern>
    class lazy_split_view
      : public view_interface<lazy_split_view<V, Pattern>>,
    public _enable_default_ctor<default_initializable<V>&&default_initializable<Pattern>>
    {
      static_assert(view<V> && view<Pattern>
      && indirectly_comparable<iterator_t<V>, iterator_t<Pattern>,ranges::equal_to>
      && (forward_range<V> ||sized_range<Pattern>&&_details::_tiny_range<V>::value));
    private:
      /*内部迭代器 (inner iterator)
    内部迭代器用于遍历某个子范围中的元素。也就是说，对于外部迭代器返回的每个子范围，你可以使用内部迭代器来遍历该子范围中的每个元素。
    用途：内部迭代器用于遍历单个子范围（片段）中的元素。
    行为：内部迭代器就是子范围本身的迭代器，因此你可以像遍历任何其他范围一样使用它。
    值类型：内部迭代器的 value_type 是原始范围的元素类型。*/
      template<bool>struct InnerIter;

      //外部迭代器用于遍历分割后的各个子范围。
      //当你对 lazy_split_view 进行迭代时，外部迭代器每次返回一个子范围（也是一个视图），代表两个分隔符之间的元素序列
      //用途：外部迭代器用于遍历分割后的每个片段（子范围）。
      //行为：每次递增外部迭代器，就会移动到下一个分隔符之后的子范围。
      //值类型：外部迭代器的 value_type 是一个 lazy_split_view 的内部定义的子范围类型，通常是一个 subrange，它本身也是一个范围。
      template<bool Const>
	struct OuterIter
          :_details::_lazy_split_view_outer_iter_cat< _maybe_const_t<Const, V>>
	{
          using Parent = _maybe_const_t<Const, lazy_split_view>;
          using Base = _maybe_const_t<Const, V>;
          using iterator_concept = std::conditional_t<forward_range<Base>,forward_iterator_tag,input_iterator_tag>;
          using difference_type = range_difference_t<Base>;
          //迭代器 ranges::lazy_split_view<V, Pattern>::outer_iterator<Const> 的值类型。
          struct value_type
              : view_interface<value_type>
          {
          private:
              OuterIter _i = OuterIter();

          public:
              value_type() = default;

              constexpr explicit value_type(OuterIter i)
                : _i(std::move(i))
              { }

              constexpr InnerIter<Const>begin() const
              { return InnerIter<Const>{_i}; }

              constexpr default_sentinel_t end() const noexcept
              { return default_sentinel; }
          };

	     OuterIter() = default;

          template<class B = Base,class = std::enable_if_t<!forward_range<B>>>
	        constexpr explicit OuterIter(Parent* parent)
	            : _parent(parent)
	    { }

          template<class B = Base,class = std::enable_if_t<forward_range<B>>>
	    constexpr OuterIter(Parent* parent, iterator_t<Base> current)
	        : _parent(parent),
	        _current(std::move(current))
	    { }

          template<class VV=V,class B = Base,REQUIRE(Const
          ,convertible_to<iterator_t<VV>, iterator_t<B>>)>
	  constexpr OuterIter(OuterIter<!Const> i)
	    : _parent(i._parent), _current(std::move(i._current)),
	      _trailing_empty(i._trailing_empty)
	  { }

	  constexpr value_type operator*() const{ return value_type{*this}; }

	  constexpr OuterIter&operator++()
	  {
	    const auto _end = ranges::end(_parent->_base);
	    if (_outer_current() == _end)
	      {
		    _trailing_empty = false;
		    return *this;
	      }
	    const auto [pbegin, pend] = subrange{_parent->_pattern};
	    if (pbegin == pend)
	      ++_outer_current();
	    else if constexpr (_details::_tiny_range<Pattern>::value)
	      {
		_outer_current() = ranges::find(std::move(_outer_current()), _end,*pbegin);
		if (_outer_current() != _end)
		  {
		    ++_outer_current();
		    if (_outer_current() == _end)
		      _trailing_empty = true;
		  }
	      }
	    else
	      do
		{
		  auto [b, p]= ranges::mismatch(_outer_current(), _end, pbegin, pend);
		  if (p == pend)
		    {
		      _outer_current() = b;
		      if (_outer_current() == _end)
			    _trailing_empty = true;
		      break;
		    }
		} while (++_outer_current() != _end);
	    return *this;
	  }

	  constexpr decltype(auto)operator++(int)
	  {
	    if constexpr (forward_range<Base>)
	      {
		auto tmp = *this;
		++*this;
		return tmp;
	      }
	    else
	      ++*this;
	  }

          template<class B=Base,REQUIRE(forward_range<B>)>
	  friend constexpr bool operator==(const OuterIter& x, const OuterIter& y)
	  {
	    return x._current == y._current&& x._trailing_empty == y._trailing_empty;
	  }

	  friend constexpr bool operator==(const OuterIter& x, default_sentinel_t)
	  { return x._outer_current() == ranges::end(x._parent->_base) && !x._trailing_empty;};

	  constexpr auto&_outer_current() noexcept
	  {
	    if constexpr (forward_range<V>)
	      return _current;
	    else
	      return *_parent->_current;
	  }

	  constexpr auto&_outer_current() const noexcept
	  {
	    if constexpr (forward_range<V>)
	      return _current;
	    else
	      return *_parent->_M_current;
	  }

	  Parent* _parent = nullptr;
	    _maybe_present_t<forward_range<V>,iterator_t<Base>> _current;
	  bool _trailing_empty = false;

	  friend OuterIter<!Const>;
	  friend InnerIter<Const>;
	};

      template<bool Const>
	struct InnerIter
	  : _details::_lazy_split_view_inner_iter_cat<_maybe_const_t<Const, V>>
	{
	private:
	  using Base = _maybe_const_t<Const, V>;

	  constexpr bool _at_end() const
	  {
	    auto [_pcur, _pend] = subrange{_i._parent->_pattern};
	    auto _end = ranges::end(_i._parent->_base);
	    if constexpr (_details::_tiny_range<Pattern>{})
	      {
		const auto& _cur = _i._current();
		if (_cur == _end)
		  return true;
		if (_pcur == _pend)
		  return _incremented;
		return *_cur == *_pcur;
	      }
	    else
	      {
		auto _cur = _i._current();
		if (_cur == _end)
		  return true;
		if (_pcur == _pend)
		  return _incremented;
		do
		  {
		    if (*_cur != *_pcur)
		      return false;
		    if (++_pcur == _pend)
		      return true;
		  } while (++_cur != _end);
		return false;
	      }
	  }

	  OuterIter<Const> _i = OuterIter<Const>();
	  bool _incremented = false;

	public:
	  using iterator_concept= typename OuterIter<Const>::iterator_concept;
	  using value_type = range_value_t<Base>;
	  using difference_type = range_difference_t<Base>;

	  InnerIter() = default;

	  constexpr explicit InnerIter(OuterIter<Const> i)
	    : _i(std::move(i))
	  { }

	  constexpr const iterator_t<Base>&base() const& noexcept{ return _i._current(); }

    template<class VV= V,REQUIRE(forward_range<VV>)>
	  constexpr iterator_t<Base>base() &&{ return std::move(_i._current()); }

	  constexpr decltype(auto)operator*() const{ return *_i._current(); }

	  constexpr InnerIter&operator++()
	  {
	    _incremented = true;
	    if constexpr (!forward_range<Base>)
	      if constexpr (Pattern::size() == 0)
		    return *this;
	    ++_i._current();
	    return *this;
	  }

	  constexpr decltype(auto)operator++(int)
	  {
	    if constexpr (forward_range<Base>)
	      {
		auto tmp = *this;
		++*this;
		return tmp;
	      }
	    else
	      ++*this;
	  }

	  friend constexpr bool operator==(const InnerIter& x, const InnerIter& y)
	  { return x._i == y._i; }

	  friend constexpr bool operator==(const InnerIter& x, default_sentinel_t)
	  { return x._at_end(); }

	  friend constexpr decltype(auto)
	  iter_move(const InnerIter& __i)
	    noexcept(noexcept(ranges::iter_move(__i._M_i_current())))
	  { return ranges::iter_move(__i._M_i_current()); }

	  friend constexpr void
	  iter_swap(const InnerIter& __x, const InnerIter& __y)
	    noexcept(noexcept(ranges::iter_swap(__x._M_i_current(),
						__y._M_i_current())))
	    requires indirectly_swappable<iterator_t<Base>>
	  { ranges::iter_swap(__x._M_i_current(), __y._M_i_current()); }
	};

      V _base = V();
      Pattern _pattern = Pattern();
	_maybe_present_t<!forward_range<V>,
	  _non_propagating_cache<iterator_t<V>>> _current;

    public:
      lazy_split_view()= default;

      constexpr lazy_split_view(V base, Pattern pattern)
	    : _base(std::move(base)), _pattern(std::move(pattern))
      { }

      template<class Range,class = std::enable_if_t<constructible_from<V, views::all_t<Range>>
      && constructible_from<Pattern, single_view<range_value_t<Range>>>>>
	constexpr lazy_split_view(Range&& r, range_value_t<Range> e)
	  : _base(views::all(std::forward<Range>(r))),_pattern(views::single(std::move(e)))
	{ }

      template<class VV =V,class = std::enable_if_t<copy_constructible<VV>>>
      constexpr V base() const& { return _base; }

      constexpr V base() &&{ return std::move(_base); }

      constexpr auto begin()
      {
	if constexpr (forward_range<V>)
	    return OuterIter<_simple_view<V> && _simple_view<Pattern>>{this, ranges::begin(_base)};
	else
	  {
	    _current = ranges::begin(_base);
	    return OuterIter<false>{this};
	  }
      }

      template<class VV =V,class = std::enable_if_t<forward_range<VV> && forward_range<const VV>>>
      constexpr auto begin() const
      {return OuterIter<true>{this, ranges::begin(_base)};}

      template<class VV =V,class = std::enable_if_t<forward_range<VV> && common_range<const VV>>>
      constexpr auto end()
      {return OuterIter<_simple_view<V> && _simple_view<Pattern>>{this, ranges::end(_base)};}

      constexpr auto end() const
      {
	if constexpr (forward_range<V>
		      && forward_range<const V>
		      && common_range<const V>)
	  return OuterIter<true>{this, ranges::end(_base)};
	else
	  return default_sentinel;
      }
    };

  template<typename Range, typename Pattern>
    lazy_split_view(Range&&, Pattern&&)
      -> lazy_split_view<views::all_t<Range>, views::all_t<Pattern>>;

  template<class Range,class = std::enable_if_t<input_range<Range>>>
    lazy_split_view(Range&&, range_value_t<Range>)
      -> lazy_split_view<views::all_t<Range>, single_view<range_value_t<Range>>>;

  namespace views
  {
    struct LazySplit
        : _range_adaptor<LazySplit>
    {
      template<class Range, class Pattern,
            class = _require_t<viewable_range<Range>>>
	constexpr auto operator() [[nodiscard]] (Range&& r, Pattern&& f) const
	{
	  return lazy_split_view(std::forward<Range>(r), std::forward<Pattern>(f));
	}
      using  _range_adaptor<LazySplit>::operator();
    };

    inline constexpr LazySplit lazy_split;
  } // namespace views

  template<class V, class Pattern>
  class split_view
      : public view_interface<split_view<V, Pattern>>,
    public _enable_default_ctor<(default_initializable<V>
               && default_initializable<Pattern>)>
  {
      static_assert(forward_range<V>&&forward_range<Pattern>);
      static_assert(view<V> && view<Pattern>
      && indirectly_comparable<iterator_t<V>, iterator_t<Pattern>,
                   ranges::equal_to>);
  private:
    V _base = V();
    Pattern _pattern = Pattern();
    _non_propagating_cache<subrange<iterator_t<V>>> _cached_begin;

    struct Iterator;
    struct Sentinel;

  public:
    split_view()= default;

    constexpr split_view(V base, Pattern pattern)
      : _base(std::move(base)), _pattern(std::move(pattern))
    { }

    template<class Range,REQUIRE(forward_range<Range>,
        constructible_from<V, views::all_t<Range>>,
        constructible_from<Pattern, single_view<range_value_t<Range>>>)>
    constexpr split_view(Range&& r, range_value_t<Range> e)
      : _base(views::all(std::forward<Range>(r))),
	_pattern(views::single(std::move(e)))
    { }

    constexpr V base() const&{ return _base; }

    constexpr V base() &&{ return std::move(_base); }

    constexpr Iterator begin()
    {
      if (!_cached_begin)
	_cached_begin = this->_find_next(ranges::begin(_base));
      return {this, ranges::begin(_base), *_cached_begin};
    }

    constexpr auto end()
    {
      if constexpr (common_range<V>)
	return Iterator{this, ranges::end(_base), {}};
      else
	return Sentinel{this};
    }
  private:
    constexpr subrange<iterator_t<V>>_find_next(iterator_t<V> it)
    {
      auto [b, e] = ranges::search(subrange(it, ranges::end(_base)), _pattern);
      if (b != ranges::end(_base) && ranges::empty(_pattern))
	{
	  ++b;
	  ++e;
	}
      return {b, e};
    }

    struct Iterator
    {
    private:
      split_view* _parent = nullptr;
      iterator_t<V> _cur = iterator_t<V>();
      subrange<iterator_t<V>> _next = subrange<iterator_t<V>>();
      bool _trailing_empty = false;

      friend struct Sentinel;

    public:
      using iterator_concept = forward_iterator_tag;
      using iterator_category = input_iterator_tag;
      using value_type = subrange<iterator_t<V>>;
      using difference_type = range_difference_t<V>;

      Iterator() = default;

      constexpr Iterator(split_view* parent,
          iterator_t<V> current,
          subrange<iterator_t<V>> __next)
	: _parent(parent),_cur(std::move(current)),_next(std::move(__next))
      { }

      constexpr iterator_t<V>base() const{ return _cur; }

      constexpr value_type operator*() const{ return {_cur, _next.begin()}; }

      constexpr Iterator&operator++()
      {
	_cur = _next.begin();
	if (_cur != ranges::end(_parent->_base))
	  {
	    _cur = _next.end();
	    if (_cur == ranges::end(_parent->_base))
	      {
		_trailing_empty = true;
		_next = {_cur, _cur};
	      }
	    else
	      _next = _parent->_find_next(_cur);
	  }
	else
	  _trailing_empty = false;
	return *this;
      }

      constexpr Iterator operator++(int)
      {
	auto tmp = *this;
	++*this;
	return tmp;
      }

      friend constexpr bool operator==(const Iterator& x, const Iterator& y)
      {return x._cur == y._cur&& x._trailing_empty == y._trailing_empty;}

        friend constexpr bool operator!=(const Iterator& x, const Iterator& y)
      {return x._cur != y._cur&& x._trailing_empty != y._trailing_empty;}
    };

    struct Sentinel
    {
    private:
      sentinel_t<V> _end = sentinel_t<V>();

      constexpr bool _equal(const Iterator& x) const
      { return x._cur == _end && !x._trailing_empty; }

    public:
      Sentinel() = default;

      constexpr explicit Sentinel(split_view* parent)
	: _end(ranges::end(parent->_base))
      { }

      friend constexpr bool operator==(const Iterator& x, const Sentinel& y)
      { return y._equal(x); }

        friend constexpr bool operator!=(const Iterator& x, const Sentinel& y)
      { return !(x==y); }

        friend constexpr bool operator==(const Sentinel& y,const Iterator& x )
      { return y._equal(x); }

        friend constexpr bool operator!=(const Sentinel& y,const Iterator& x )
      { return !(x==y); }
    };
  };

  template<typename Range, typename Pattern>
    split_view(Range&&, Pattern&&)
      -> split_view<views::all_t<Range>, views::all_t<Pattern>>;

    template<class Range,REQUIRE(forward_range<Range>)>
    split_view(Range&&, range_value_t<Range>)
      -> split_view<views::all_t<Range>, single_view<range_value_t<Range>>>;

  namespace views
  {
    namespace _details
    {
        template<typename, typename ,class = void>
	    struct  _can_split_view
            :std::false_type
        {};

        template<typename R, typename P>
        struct  _can_split_view<R,P,
        std::void_t<decltype(split_view(std::declval<R>(), std::declval<P>()))>>
            :std::true_type
        {};
    }

    struct Split
        : _range_adaptor<Split>
    {
      template<class Range, class Pattern,
         REQUIRE(viewable_range<Range>, _details::_can_split_view<Range, Pattern>::value)>
	constexpr auto
	operator() [[nodiscard]] (Range&& r, Pattern&& f) const
	{
	  return split_view(std::forward<Range>(r), std::forward<Pattern>(f));
	}

      using _range_adaptor<Split>::operator();
    };

    inline constexpr Split split;
  } // namespace views

  namespace views
  {
    struct Counted
    {
      template<class Iter,REQUIRE(input_or_output_iterator<Iter>)>
      constexpr auto
      operator() [[nodiscard]] (Iter i, iter_difference_t<Iter> n) const
      {
	if constexpr (contiguous_iterator<Iter>)
	  return span(to_address(i), n);
	else if constexpr (random_access_iterator<Iter>)
	  return subrange(i, i + n);
	else
	  return subrange(counted_iterator(std::move(i), n),default_sentinel);
      }
    };

    inline constexpr Counted counted{};
  } // namespace views

  template<class  V>
    class common_view
      : public view_interface<common_view<V>>,
      public _enable_default_ctor< default_initializable<V>>
    {
      static_assert(view<V>);
      static_assert(  (!common_range<V>) && copyable<iterator_t<V>>);
    private:
      V _base = V();

    public:
      common_view() = default;

      constexpr explicit common_view(V r)
	: _base(std::move(r))
      { }

      constexpr V base() const& { return _base; }
      constexpr V base() &&{ return std::move(_base); }

      constexpr auto begin()
      {
	if constexpr (random_access_range<V> && sized_range<V>)
	  return ranges::begin(_base);
	else
	  return common_iterator<iterator_t<V>, sentinel_t<V>>
		  (ranges::begin(_base));
      }

      template<class R = V,class = std::enable_if_t<range<const R>>>
      constexpr auto begin() const
      {
	if constexpr (random_access_range<const V> && sized_range<const V>)
	  return ranges::begin(_base);
	else
	  return common_iterator<iterator_t<const V>, sentinel_t<const V>>
		  (ranges::begin(_base));
      }

      constexpr auto end()
      {
	if constexpr (random_access_range<V> && sized_range<V>)
	  return ranges::begin(_base) + ranges::size(_base);
	else
	  return common_iterator<iterator_t<V>, sentinel_t<V>>
		  (ranges::end(_base));
      }

      template<class R = V,class = std::enable_if_t<range<const R>>>
      constexpr auto end() const
      {
	if constexpr (random_access_range<const V> && sized_range<const V>)
	  return ranges::begin(_base) + ranges::size(_base);
	else
	  return common_iterator<iterator_t<const V>, sentinel_t<const V>>
		  (ranges::end(_base));
      }

      constexpr auto size(){ return ranges::size(_base); }

      constexpr auto size() const{ return ranges::size(_base); }
    };

  template<typename Range>
    common_view(Range&&) -> common_view<views::all_t<Range>>;

  template<typename T>
    inline constexpr bool enable_borrowed_range<common_view<T>> = enable_borrowed_range<T>;

  namespace views
  {
    namespace _details
    {
      template<typename Range>
	    struct _already_common
      {
          template<class R,class = void>
          struct _impl
              :std::false_type
          {};

          template<class R>
          struct _impl<R,std::void_t<decltype(views::all(std::declval<Range>()))>>
              :std::true_type
          {};

          static constexpr bool value = common_range<Range>&&_impl<Range>::value;
      };

        template<typename,class = void>
        struct  _can_common_view
            :std::false_type
        {};

        template<typename R>
        struct  _can_common_view<R,std::void_t<decltype(common_view(std::declval<R>()))>>
            :std::true_type
        {};
    }

    struct Common
        : range_adaptor_closure<Common>
    {
      template<class Range,REQUIRE(viewable_range<Range>,
	_details::_already_common<Range>::value|| _details::_can_common_view<Range>::value)>
	constexpr auto operator() [[nodiscard]] (Range&& r) const
	{
	  if constexpr (_details::_already_common<Range>{})
	    return views::all(std::forward<Range>(r));
	  else
	    return common_view{std::forward<Range>(r)};
	}
    };

    inline constexpr Common common;
  } // namespace views

  template<class  V>
    class reverse_view
      : public view_interface<reverse_view<V>>,public _enable_default_ctor< default_initializable<V>>
    {
      static_assert(bidirectional_range<V>);
    private:
      static constexpr bool _needs_cached_begin
	= !common_range<V> && !(random_access_range<V>
				  && sized_sentinel_for<sentinel_t<V>,
							iterator_t<V>>);

      V _base = V();
	_maybe_present_t<_needs_cached_begin,_non_propagating_cache<V>>_cached_begin;

    public:
      reverse_view()  = default;

      constexpr explicit reverse_view(V r)
	: _base(std::move(r))
	{ }

      template<class R = V,class = std::enable_if_t<copy_constructible<R>>>
      constexpr V base() const&{ return _base; }
      constexpr V base() &&{ return std::move(_base); }

      constexpr std::reverse_iterator<iterator_t<V>>begin()
      {
	if constexpr (_needs_cached_begin)
	  if (_cached_begin.has_value())
	    return std::make_reverse_iterator(_cached_begin._M_get(_base));

	auto it = ranges::next(ranges::begin(_base), ranges::end(_base));
	if constexpr (_needs_cached_begin)
	  _cached_begin._M_set(_base, it);
	return std::make_reverse_iterator(std::move(it));
      }

      template<class R = V,class = std::enable_if_t<common_range<R>>>
      constexpr auto begin()
      { return std::make_reverse_iterator(ranges::end(_base)); }

      template<class R = V,class = std::enable_if_t<common_range<const R>>>
      constexpr auto begin() const
      { return std::make_reverse_iterator(ranges::end(_base)); }

      constexpr std::reverse_iterator<iterator_t<V>>end()
      { return std::make_reverse_iterator(ranges::begin(_base)); }

      template<class R = V,class = std::enable_if_t<common_range<const R>>>
      constexpr auto end() const
      { return std::make_reverse_iterator(ranges::begin(_base)); }

      template<class R = V,class = std::enable_if_t<sized_range<R>>>
      constexpr auto size()
      { return ranges::size(_base); }

      template<class R = V,class = std::enable_if_t<sized_range<R const>>>
      constexpr auto size() const
      { return ranges::size(_base); }
    };

  template<typename Range>
    reverse_view(Range&&) -> reverse_view<views::all_t<Range>>;

  template<typename T>
    inline constexpr bool enable_borrowed_range<reverse_view<T>> = enable_borrowed_range<T>;

  namespace views
  {
    namespace _details
    {
        template<typename>
      inline constexpr bool _is_reversible_subrange = false;

        template<typename Iter, subrange_kind Kind>
      inline constexpr bool _is_reversible_subrange<subrange<std::reverse_iterator<Iter>,std::reverse_iterator<Iter>,Kind>> = true;

        template<typename,class = void>
        struct  _can_reverse_view
            :std::false_type
        {};

        template<typename R>
        struct  _can_reverse_view<R,std::void_t<decltype(reverse_view(std::declval<R>()))>>
            :std::true_type
        {};
    }

    struct Reverse
        :range_adaptor_closure<Reverse>
    {
      template<class R,REQUIRE(viewable_range<R>,
      _is_specialization_of_v<reverse_view,R>
	  || _details::_is_reversible_subrange<remove_cvref_t<R>>
	  || _details::_can_reverse_view<R>::value)>
	constexpr auto operator() [[nodiscard]] (R&& r) const
	{
	  using T = remove_cvref_t<R>;
	  if constexpr ( _is_specialization_of_v<reverse_view,R>)
	    return std::forward<R>(r).base();
	  else if constexpr (_details::_is_reversible_subrange<T>)
	    {
	      using Iter = decltype(ranges::begin(r).base());
	      if constexpr (sized_range<T>)
		    return subrange<Iter, Iter, subrange_kind::sized>{r.end().base(), r.begin().base(), r.size()};
	      else
		    return subrange<Iter, Iter, subrange_kind::unsized>{r.end().base(), r.begin().base()};
	    }
	  else
	    return reverse_view{std::forward<R>(r)};
	}

      static constexpr bool _S_has_simple_call_op = true;
    };

    inline constexpr Reverse reverse;
  } // namespace views

  namespace _details
  {
      template<typename >
        inline constexpr bool _is_tuple_like_v = false;

      template<typename... Elements>
        inline constexpr bool _is_tuple_like_v<std::tuple<Elements...>> = true;

      template<typename T1, typename T2>
        inline constexpr bool _is_tuple_like_v<std::pair<T1, T2>> = true;

      template<typename T, size_t N>
        inline constexpr bool _is_tuple_like_v<std::array<T, N>> = true;

    template<typename T, size_t N>
   CXX17_CONCEPT _has_tuple_element = _is_tuple_like_v<remove_cvref_t<T>>
      && N < std::tuple_size_v<T>;

      template<size_t N,class Base,bool = forward_range<Base>>
      struct _elements_iterator_category
      {};

      template<class Base,size_t N>
      struct _elements_iterator_category<N,Base,true>
      {
          using C = typename iterator_traits<iterator_t<Base>>::iterator_category;
          using R = decltype((std::get<N>(*std::declval<iterator_t<Base>>())));
          using iterator_category = std::conditional_t<std::is_rvalue_reference_v<R>,
          input_iterator_tag,
          std::conditional_t<derived_from<C, random_access_iterator_tag>,
          random_access_iterator_tag,C>>;
      };
  }

  template<class V, size_t N>
    class elements_view
      : public view_interface<elements_view<V, N>>,
   public  _enable_default_ctor<default_initializable<V>>
    {
      static_assert(view<V>&&input_range<V>);
      static_assert(_details::_has_tuple_element<range_value_t<V>,N>);
      static_assert(_details::_has_tuple_element<range_value_t<std::remove_reference_t<range_reference_t<V>>>,N>);
      static_assert(std::is_reference_v<range_reference_t<V>>
          || move_constructible<std::tuple_element_t<N, range_reference_t<V>>>);

  public:
      elements_view()  = default;

      constexpr explicit elements_view(V base)
	    : _base(std::move(base))
      { }

      constexpr V base() const&{ return _base; }
      constexpr V base() &&{ return std::move(_base); }

      template<class VV= V,class = std::enable_if_t< (!_simple_view<VV>)>>
      constexpr auto begin()
      { return Iterator<false>(ranges::begin(_base)); }

      template<class VV= V,class = std::enable_if_t< range<const VV>>>
      constexpr auto begin() const
      { return Iterator<true>(ranges::begin(_base)); }

      constexpr auto end()
      {
          if constexpr(!_simple_view<V>)
          {
              if constexpr(common_range<V>)
                  return Iterator<false>(ranges::end(_base));
              else
                  return Sentinel<false>(ranges::end(_base));
          }
          else if constexpr(common_range<const V>)
              return Iterator<true>(ranges::end(_base));
          else if constexpr(range<const V>)
             return Sentinel<true>(ranges::end(_base));
      }

      template<class VV= V,class = std::enable_if_t< sized_range<VV>>>
      constexpr auto size(){ return ranges::size(_base); }

      template<class VV= V,class = std::enable_if_t< sized_range<const VV>>>
      constexpr auto size() const{ return ranges::size(_base); }

    private:
      template<bool>struct Sentinel;

      template<bool Const>
	struct Iterator
          :_enable_default_ctor<default_initializable<iterator_t<std::conditional_t<Const,const V,V>>>>,
      _details::_elements_iterator_category<N,std::conditional_t<Const,const V,V>>
	{
	private:
	  using Base = std::conditional_t<Const,const V,V>;

	  iterator_t<Base> _current = iterator_t<Base>();

	  friend Iterator<!Const>;
	public:
          /*
          *std::random_access_iterator_tag，如果 Base 遵循 random_access_range
            std::bidirectional_iterator_tag，如果 Base 遵循 bidirectional_range
            std::forward_iterator_tag，如果 Base 遵循 forward_range
            std::input_iterator_tag.
           */
	  using iterator_concept = _switch_t<bool_sequence<random_access_range<Base>,bidirectional_range<Base>,forward_range<Base>>,
            meta_list<random_access_iterator_tag,bidirectional_iterator_tag,forward_iterator_tag>,
          input_iterator_tag
          >;
	  using value_type= remove_cvref_t<std::tuple_element_t<N, range_value_t<Base>>>;
	  using difference_type = range_difference_t<Base>;

	  Iterator() = default;

	  constexpr explicit Iterator(iterator_t<Base> current)
	    : _current(std::move(current))
	  { }

          template<class B = Base,class VV =V,REQUIRE( Const && convertible_to<iterator_t<VV>, iterator_t<B>>)>
	  constexpr Iterator(Iterator<!Const> i)
	    : _current(std::move(i._current))
	  { }

	  constexpr const iterator_t<Base>&base() const& noexcept{ return _current; }
	  constexpr iterator_t<Base>base() &&{ return std::move(_current); }

	  constexpr decltype(auto)operator*() const
	  {
	      if constexpr (std::is_reference_v<range_reference_t<Base>>)
	          return std::get<N>(*this->_current);
	      else
	          return static_cast<std::remove_cv_t<std::tuple_element_t<N, range_reference_t<Base>>>>(std::get<N>(*this->_current));
	  }

	  constexpr Iterator&operator++()
	  {
	    ++_current;
	    return *this;
	  }

	  constexpr void operator++(int){ ++_current; }

    template<class B =Base,class = std::enable_if_t<forward_range<B>>>
	  constexpr Iterator operator++(int)
	  {
	    auto tmp = *this;
	    ++_current;
	    return tmp;
	  }

    template<class B =Base,class = std::enable_if_t<bidirectional_range<B>>>
	  constexpr Iterator&operator--()
	  {
	    --_current;
	    return *this;
	  }

    template<class B =Base,class = std::enable_if_t<bidirectional_range<B>>>
	  constexpr Iterator operator--(int)
	  {
	    auto tmp = *this;
	    --_current;
	    return tmp;
	  }

     template<class B =Base,class = std::enable_if_t<random_access_range<B>>>
	  constexpr Iterator&operator+=(difference_type n)
	  {
	    _current += n;
	    return *this;
	  }

        template<class B =Base,class = std::enable_if_t<random_access_range<B>>>
	  constexpr Iterator&operator-=(difference_type n)
	  {
	    _current -= n;
	    return *this;
	  }

    template<class B =Base,class = std::enable_if_t<random_access_range<B>>>
	  constexpr decltype(auto)operator[](difference_type n) const
	  { return _S_get_element(_current + n); }

    template<class B =Base,class = std::enable_if_t<equality_comparable<iterator_t<B>>>>
	  friend constexpr bool operator==(const Iterator& x, const Iterator& y)
	  { return x._current == y._current; }

          template<class B =Base,class = std::enable_if_t<equality_comparable<iterator_t<B>>>>
  friend constexpr bool operator!=(const Iterator& x, const Iterator& y)
	  { return x._current != y._current; }

    template<class B =Base,class = std::enable_if_t<random_access_range<B>>>
	  friend constexpr bool operator<(const Iterator& x, const Iterator& y)
	  { return x._current < y._current; }

          template<class B =Base,class = std::enable_if_t<random_access_range<B>>>
            friend constexpr bool operator<(const Iterator& x, const Iterator& y)
	  { return y._current < x._current; }

          template<class B =Base,class = std::enable_if_t<random_access_range<B>>>
            friend constexpr bool operator<=(const Iterator& x, const Iterator& y)
	  { return !(y._current > x._current); }

          template<class B =Base,class = std::enable_if_t<random_access_range<B>>>
            friend constexpr bool operator>=(const Iterator& x, const Iterator& y)
	  { return !(x._current > y._current); }

          template<class B =Base,class = std::enable_if_t<random_access_range<B>>>
	  friend constexpr Iterator operator+(const Iterator& x, difference_type y)
	  { return Iterator{x} += y; }

          template<class B =Base,class = std::enable_if_t<random_access_range<B>>>
	  friend constexpr Iterator operator+(difference_type x, const Iterator& y)
	  { return y + x; }

          template<class B =Base,class = std::enable_if_t<random_access_range<B>>>
	  friend constexpr Iterator
	  operator-(const Iterator& x, difference_type y)
	  { return Iterator{x} -= y; }

          template<class B =Base,class = std::enable_if_t<sized_sentinel_for<iterator_t<B>, iterator_t<B>>>>
	  friend constexpr difference_type operator-(const Iterator& x, const Iterator& y)
	  { return x._current - y._current; }

	  template <bool> friend struct Sentinel;
	};

      template<bool Const>
	struct Sentinel
	{
	private:
        using Base = std::conditional_t<Const,const V,V>;
	  sentinel_t<Base> _end = sentinel_t<Base>();

	public:
	  Sentinel() = default;

	  constexpr explicit Sentinel(sentinel_t<Base> end)
	    : _end(std::move(end))
	  { }

	  constexpr Sentinel(Sentinel<!Const> other)
	    : _end(std::move(other._end))
	  { }

	  constexpr sentinel_t<Base>base() const{ return _end; }

          template<bool Const2,class = std::enable_if_t< sentinel_for<sentinel_t<Base>,
                   iterator_t<_maybe_const_t<Const2, V>>>>>
	    friend constexpr bool operator==(const Iterator<Const2>& x, const Sentinel& y)
	    { return x._M_current == y._end; }

          template<bool Const2,class = std::enable_if_t< sentinel_for<sentinel_t<Base>,
         iterator_t<_maybe_const_t<Const2, V>>>>>
friend constexpr bool operator==( const Sentinel& y,const Iterator<Const2>& x)
	  { return x._M_current == y._end; }

          template<bool Const2,class = std::enable_if_t< sentinel_for<sentinel_t<Base>,
         iterator_t<_maybe_const_t<Const2, V>>>>>
friend constexpr bool operator!=(const Iterator<Const2>& x, const Sentinel& y)
	  { return x._M_current != y._end; }

          template<bool Const2,class = std::enable_if_t< sentinel_for<sentinel_t<Base>,
         iterator_t<_maybe_const_t<Const2, V>>>>>
friend constexpr bool operator!=( const Sentinel& y,const Iterator<Const2>& x)
	  { return x._M_current != y._end; }

	  template<bool Const2,class Base2 = _maybe_const_t<Const2, V>
          ,class = std::enable_if_t<sized_sentinel_for<sentinel_t<Base>, iterator_t<Base2>>>>
	    friend constexpr range_difference_t<Base2>
	    operator-(const Iterator<Const2>& x, const Sentinel& y)
	    { return x.base() - y.base(); }

          template<bool Const2,class Base2 = _maybe_const_t<Const2, V>
              ,class = std::enable_if_t<sized_sentinel_for<sentinel_t<Base>, iterator_t<Base2>>>>
            friend constexpr range_difference_t<Base2>
            operator-( const Sentinel& y,const Iterator<Const2>& x)
	  { return y.base() - x.base(); }

	  friend Sentinel<!Const>;
	};

      V _base = V();
    };

  template<typename T, size_t N>
    inline constexpr bool enable_borrowed_range<elements_view<T, N>>
      = enable_borrowed_range<T>;

  template<typename Range>
    using keys_view = elements_view<views::all_t<Range>, 0>;

  template<typename Range>
    using values_view = elements_view<views::all_t<Range>, 1>;

  namespace views
  {
    namespace _details
    {
      template<size_t N, typename Range,class  = void>
	inline constexpr bool  _can_elements_view  = false;

        template<size_t N, typename Range>
        inline constexpr bool  _can_elements_view<N,Range,std::void_t<decltype(elements_view<all_t<Range>, N>{std::declval<Range>()})>>  = true;
    }

    template<size_t N>
      struct Elements
            : range_adaptor_closure<Elements<N>>
      {
	template<class Range,
        REQUIRE(viewable_range<Range>,_details::_can_elements_view<N, Range>)>
	  constexpr auto operator() [[nodiscard]] (Range&& r) const
	  {return elements_view<all_t<Range>, N>{std::forward<Range>(r)};}
      };

    template<size_t N>
      inline constexpr Elements<N> elements;

    inline constexpr auto keys = elements<0>;
    inline constexpr auto values = elements<1>;
  } // namespace views

    template<class Callable,class Tuple, std::size_t... _indices>
    constexpr decltype(auto)_apply(Callable&& callable, Tuple&& tuple, std::index_sequence<_indices...>) // 仅用于说明
    {return ranges::invoke(std::forward<Callable>(callable), std::get<_indices>(std::forward<Tuple>(tuple))...);}

    template <class Callable,class Tuple,class = std::enable_if_t<_details::_is_tuple_like_v<remove_cvref_t<Tuple>>>>
  constexpr decltype(auto)apply(Callable&& callable, Tuple&& tuple)
  {
      using Indices= std::make_index_sequence<std::tuple_size_v<std::remove_reference_t<Tuple>>>;
      return _apply(std::forward<Callable>(callable),std::forward<Tuple>(tuple),Indices{});
  }

    template<class Callable,class...Types,size_t ... _indices>
    constexpr std::tuple<Types...> _tuple_transform(Callable&&callable,
        std::tuple<Types...>&tuple,
        std::index_sequence<_indices...>)
    {return std::tuple<std::invoke_result_t<Callable&, Types>...>(ranges::invoke(callable, std::get<_indices>(tuple))...);}

    template<class Callable,class...Types,size_t ... _indices>
    constexpr std::tuple<Types...>_tuple_transform(Callable&&callable,
        const std::tuple<Types...>&tuple,
        std::index_sequence<_indices...>)
  {return std::tuple<std::invoke_result_t<Callable&, Types>...>(ranges::invoke(callable, std::get<_indices>(tuple))...);}

    template<class Callable,class...Types,size_t ... _indices>
constexpr std::tuple<Types...> _tuple_transform(Callable&&callable,
    std::tuple<Types...>&&tuple,
    std::index_sequence<_indices...>)
  {return std::tuple<std::invoke_result_t<Callable&, Types>...>(ranges::invoke(callable, std::get<_indices>(std::move(tuple)))...);}

    template<class Callable,class...Types,size_t ... _indices>
    constexpr std::tuple<Types...> _tuple_transform(Callable&&callable,
    const std::tuple<Types...>&&tuple,
    std::index_sequence<_indices...>)
  {return std::tuple<std::invoke_result_t<Callable&, Types>...>(ranges::invoke(callable, std::get<_indices>(std::move(tuple)))...);}

    template< class Callable, class Tuple >
    constexpr auto tuple_transform( Callable&& callable, Tuple&& tuple )
    {
        return _tuple_transform(std::forward<Callable>(callable),
            std::forward<Tuple>(tuple),
            std::make_index_sequence<std::tuple_size_v<remove_cvref_t<Tuple>>>{});
    }

    template< class... Rs >
    CXX17_CONCEPT _zip_is_common =
    (sizeof...(Rs) == 1 && (ranges::common_range<Rs> && ...))
    ||
    (!(ranges::bidirectional_range<Rs> && ...) && (ranges::common_range<Rs> && ...))
    ||
    ((ranges::random_access_range<Rs> && ...) && (ranges::sized_range<Rs> && ...));

    template< bool C, class... Views >
    CXX17_CONCEPT _all_random_access = (ranges::random_access_range<std::conditional_t<C, const Views, Views>> && ...);

    template< bool C, class... Views >
    CXX17_CONCEPT _all_bidirectional = (bidirectional_range<std::conditional_t<C, const Views, Views>> && ...);

    template< bool C, class... Views >
    CXX17_CONCEPT _all_forward = (forward_range<std::conditional_t<C, const Views, Views>> && ...);

    template< class F, class Tuple >
    constexpr void _tuple_for_each( F&& f, Tuple&& tuple )
    {
        ranges::apply([&](auto&&... args)
        {
            (static_cast<void>(ranges::invoke(f, std::forward<decltype(args)>(args))), ...);
        }, std::forward<Tuple>(tuple));
    }

    template<class ... Views>
    class zip_view
    :public view_interface<zip_view<Views...>>
  {
      static_assert(sizeof...(Views)>0);
      static_assert((view<Views>&&...));
  private:
      using _zip_tuple = std::tuple<Views...>;
      template<bool>struct Iterator;
      template<bool>struct Sentinel;

      template<int,class>
      struct _enable_zip
          :std::false_type
      {};

        template<class...Ts>
        struct _enable_zip<1,std::tuple<Ts...>>
            :std::bool_constant<(_simple_view<Ts>&&...)>
        {};

        template<class...Ts>
        struct _enable_zip<2,std::tuple<Ts...>>
            :std::bool_constant<(range<const Ts> &&...)>
        {};

        template<class...Ts>
        struct _enable_zip<3,std::tuple<Ts...>>
            :std::bool_constant<(sized_range<Ts>&&...)>
        {};

        template<class...Ts>
        struct _enable_zip<4,std::tuple<Ts...>>
            :std::bool_constant<(sized_range<const Ts> &&...)>
        {};
  public:
      zip_view() = default;

      explicit zip_view(Views...vs)
          :_views{std::move(vs)...}
      {}

      template<class Tuple = _zip_tuple,class =  std::enable_if_t<_enable_zip<1,Tuple>::value>>
      constexpr auto begin()
      { return Iterator<false>(tuple_transform(ranges::begin, _views)); }

        template<class Tuple = _zip_tuple,class =  std::enable_if_t<_enable_zip<2,Tuple>::value>>
        constexpr auto begin()const
        { return Iterator<true>(tuple_transform(ranges::begin, _views)); }

      template<class Tuple = _zip_tuple,class =  std::enable_if_t<_enable_zip<1,Tuple>::value>>
      constexpr auto end()
      {
          if constexpr (!_zip_is_common<Views...>)
              return Sentinel<false>(tuple_transform(ranges::end, _views));
          else if constexpr ((random_access_range<Views> && ...))
              return begin() + iter_difference_t<Iterator<false>>(size());
          else
              return Iterator<false>(tuple_transform(ranges::end, _views));
      }

      template<class Tuple = _zip_tuple,class =  std::enable_if_t<_enable_zip<2,Tuple>::value>>
      constexpr auto end() const
      {
          if constexpr (!_zip_is_common<const Views...>)
              return Sentinel<true>(tuple_transform(ranges::end, _views));
          else if constexpr ((random_access_range<const Views> && ...))
              return begin() + iter_difference_t<Iterator<true>>(size());
          else
              return Iterator<true>(tuple_transform(ranges::end, _views));
      }

      template<class Tuple = _zip_tuple,class =  std::enable_if_t<_enable_zip<3,Tuple>::value>>
      constexpr auto size()
      {
          return apply([](auto... sizes) {
        using CT = std::make_unsigned_t<std::common_type_t<decltype(sizes)...>>;
        return ranges::min({CT(sizes)...});
          }, tuple_transform(ranges::size, _views));
      }

      template<class Tuple = _zip_tuple,class =  std::enable_if_t<_enable_zip<4,Tuple>::value>>
      constexpr auto size() const
      {
          return apply([](auto... sizes) {
        using CT = std::make_unsigned_t<std::common_type_t<decltype(sizes)...>>;
        return ranges::min({CT(sizes)...});
          }, tuple_transform(ranges::size, _views));
      }
  private:
      std::tuple<Views...> _views;
    private:
        template<bool C,bool =  _all_forward<C,Views...>>
        struct _iter_cat{};

        template<bool Const>
        struct _iter_cat<Const,true>
        {
            using iterator_category = input_iterator_tag;
        };

        template<bool Const>
        struct Iterator:_iter_cat<Const>
        {
            using iterator_concept = _switch_t<
                bool_sequence<_all_random_access<Const,Views...>,
                _all_bidirectional<Const,Views...>,
                _all_forward<Const,Views...>>,
            meta_list<random_access_iterator_tag,bidirectional_iterator_tag,forward_iterator_tag>,
            input_iterator_tag
            >;

            using value_type= std::tuple<range_value_t<_maybe_const_t<Const, Views>>...>;
            using difference_type= std::common_type_t<range_difference_t<_maybe_const_t<Const, Views>>...>;

            Iterator() = default;

            template<class D =void,
                class = std::enable_if_t< std::is_void_v<D>
                            &&Const
                            && (convertible_to<iterator_t<Views>,
                                iterator_t<_maybe_const_t<Const, Views>>> && ...)>>
            constexpr Iterator(Iterator<!Const> i)
              : _current(std::move(i._current))
            { }

            constexpr auto operator*() const
            {
                auto f = [](auto& i) -> decltype(auto) {return *i;};
                return tuple_transform(f, _current);
            }

            constexpr Iterator&operator++()
            {
                _tuple_for_each([](auto& i) { ++i; }, _current);
                return *this;
            }

            constexpr void operator++(int)
            { ++*this; }

            template<class Define = void,class = std::enable_if_t<std::is_void_v<Define>&&_all_random_access<Const, Views...>>>
            constexpr auto operator[]( difference_type n ) const
            {
                return tuple_transform([&](auto& i) -> decltype(auto)
                    {
                        using I = remove_cvref_t<decltype(i)>;
                       return i[iter_difference_t<I>(n)];
                   }, _current);}

            template<class Define = void,class = std::enable_if_t<std::is_void_v<Define>&&_all_forward<Const, Views...>>>
            constexpr Iterator operator++(int)
            {
              auto tmp = *this;
              ++*this;
              return tmp;
            }

            template<class Define = void,class = std::enable_if_t<std::is_void_v<Define>&&_all_bidirectional<Const, Views...>>>
            constexpr Iterator& operator--()
            {_tuple_for_each(_current, [](auto& i) { --i; });}

            template<class Define = void,class = std::enable_if_t<std::is_void_v<Define>&&_all_bidirectional<Const, Views...>>>
            constexpr Iterator operator--(int)
            {
                auto tmp = *this;
                --*this;
                return tmp;
            }

            template<class Define = void,class = std::enable_if_t<std::is_void_v<Define>&&_all_random_access<Const, Views...>>>
            constexpr Iterator& operator+=(difference_type n)
            {
                _tuple_for_each(_current, [&](auto& i)
                {
                    static_assert(std::is_lvalue_reference_v<decltype(i)>);
                    using I = remove_cvref_t<decltype(i)>;
                    i += iter_difference_t<I>(n);
                });
                return *this;
            }

            template<class Define = void,class = std::enable_if_t<std::is_void_v<Define>&&_all_random_access<Const, Views...>>>
            constexpr Iterator& operator-=(difference_type n)
            {
                _tuple_for_each(_current, [&](auto& i)
                {
                    static_assert(std::is_lvalue_reference_v<decltype(i)>);
                    using I = remove_cvref_t<decltype(i)>;
                    i -= iter_difference_t<I>(n);
                });
                return *this;
            }

            template<class Define = void,
                class = std::enable_if_t<std::is_void_v<Define>
                &&(equality_comparable<ranges::iterator_t<_maybe_const_t<Const, Views>>> && ...)>>
            friend constexpr bool operator==(const Iterator& x, const Iterator& y)
            {
                if constexpr(_all_bidirectional<Const,Views...>)
                    return x._current==y._current;
                return _tuples_for_each<1>(std::index_sequence_for<Views...>{},x,y);
            }

            template<class Define = void,
                class = std::enable_if_t<std::is_void_v<Define>
                &&(equality_comparable<ranges::iterator_t<_maybe_const_t<Const, Views>>> && ...)>>
            friend constexpr bool operator!=(const Iterator& x, const Iterator& y)
            {return !(x==y);}

            template<class Define = void,class = std::enable_if_t<std::is_void_v<Define>&&_all_random_access<Const, Views...>>>
            friend constexpr bool operator<(const Iterator& x, const Iterator& y)
            {return x._current < y._current;}

            template<class Define = void,class = std::enable_if_t<std::is_void_v<Define>&&_all_random_access<Const, Views...>>>
            friend constexpr bool operator<=(const Iterator& x, const Iterator& y)
            {return x._current <= y._current;}

            template<class Define = void,class = std::enable_if_t<std::is_void_v<Define>&&_all_random_access<Const, Views...>>>
           friend constexpr bool operator>(const Iterator& x, const Iterator& y)
            {return x._current > y._current;}

            template<class Define = void,class = std::enable_if_t<std::is_void_v<Define>&&_all_random_access<Const, Views...>>>
            friend constexpr bool operator>=(const Iterator& x, const Iterator& y)
            {return x._current >= y._current;}

            template<class Define = void,class = std::enable_if_t<std::is_void_v<Define>&&_all_random_access<Const, Views...>>>
            friend constexpr Iterator operator+(const Iterator& i, difference_type n)
            {
                  auto r = i;
                  r += n;
                  return r;
            }

            template<class Define = void,class = std::enable_if_t<std::is_void_v<Define>&&_all_random_access<Const, Views...>>>
            friend constexpr Iterator operator-(const Iterator& i, difference_type n)
            {
                auto r = i;
                r -= n;
                return r;
            }

            template<class Define = void,class = std::enable_if_t<std::is_void_v<Define>&&_all_random_access<Const, Views...>>>
            friend constexpr Iterator operator+=(const Iterator& i, difference_type n)
            {
                auto r = i;
                r += n;
                return r;
            }

            template<class Define = void,class = std::enable_if_t<std::is_void_v<Define>&&_all_random_access<Const, Views...>>>
            friend constexpr Iterator operator-=(const Iterator& i, difference_type n)
            {
                auto r = i;
                r -= n;
                return r;
            }

            template<class D = void,
                    class = std::enable_if_t<std::is_void_v<D>
                    &&(sized_sentinel_for<iterator_t<_maybe_const_t<Const, Views>>,
                                   iterator_t<_maybe_const_t<Const, Views>>>&&...)>>
            friend constexpr difference_type operator-(const Iterator& i, const Iterator& j)
            {return _tuples_for_each<2>(std::index_sequence_for<Views...>{},i,j);}

            friend constexpr auto iter_move(const Iterator& i)
            { return tuple_transform(ranges::iter_move, i._current); }

            template<class D =void,REQUIRE(std::is_void_v<D>,
            (indirectly_swappable<iterator_t<_maybe_const_t<Const, Views>>> && ...))>
            friend constexpr void iter_swap(const Iterator& l, const Iterator& r)
            {return _tuples_for_each<0>(std::index_sequence_for<Views...>{},l,r);}

            friend class zip_view;

            template<int _tag,size_t ... _indices,class Tuple1,class Tuple2>
            static constexpr auto _tuples_for_each(std::index_sequence<_indices...>,Tuple1&&tuple1,Tuple2&&tuple2)
            {
                if constexpr(_tag == 1)//swap
                    (ranges::iter_swap(std::get<_indices>(tuple1._current), std::get<_indices>(tuple2._current)), ...);
                else if constexpr(_tag ==0)
                    return ((std::get<_indices>(tuple1) == std::get<_indices>(tuple2))||...);
                else
                {
                    return ranges::min({difference_type(std::get<_indices>(tuple1._current)- std::get<_indices>(tuple1._current))...},
                               ranges::less{},
                               [](difference_type i) {return std::make_unsigned_t<difference_type>(i < 0 ? -i : i);});
                }
                return false;
            }

            std::tuple<_maybe_const_t<Const,Views>...> _current;
        };

      template<bool Const>
      struct Sentinel
      {
        std::tuple<sentinel_t<_maybe_const_t<Const, Views>>...> _end;

        constexpr explicit Sentinel(decltype(_end) end)
          : _end(end)
        { }

        friend class zip_view;

      public:
        Sentinel() = default;

          template<class D =void,REQUIRE(Const,
              std::is_void_v<D>,
              (convertible_to<sentinel_t<Views>,sentinel_t<_maybe_const_t<Const, Views>>> && ...))>
        constexpr Sentinel(Sentinel<!Const> i)
          : _end(std::move(i._end))
        { }

        template<bool OtherConst,
          REQUIRE((sentinel_for<sentinel_t<_maybe_const_t<Const, Views>>,
               iterator_t<_maybe_const_t<OtherConst, Views>>> && ...))>
        friend constexpr bool
        operator==(const Iterator<OtherConst>& x, const Sentinel& y)
        {return _operator_equal(std::index_sequence_for<Views...>{},x,y);}

          template<bool OtherConst,
            REQUIRE((sentinel_for<sentinel_t<_maybe_const_t<Const, Views>>,
                 iterator_t<_maybe_const_t<OtherConst, Views>>> && ...))>
          friend constexpr bool
          operator!=(const Iterator<OtherConst>& x, const Sentinel& y)
          {return !(x==y);}

          template<bool OtherConst,
            REQUIRE((sentinel_for<sentinel_t<_maybe_const_t<Const, Views>>,
                 iterator_t<_maybe_const_t<OtherConst, Views>>> && ...))>
          friend constexpr bool
          operator==(const Sentinel& y,const Iterator<OtherConst>& x)
          {return _operator_equal(std::index_sequence_for<Views...>{},x,y);}

          template<bool OtherConst,
            REQUIRE((sentinel_for<sentinel_t<_maybe_const_t<Const, Views>>,
                 iterator_t<_maybe_const_t<OtherConst, Views>>> && ...))>
          friend constexpr bool
          operator!=(const Sentinel& y,const Iterator<OtherConst>& x)
          {return !(x==y);}

          template<size_t..._indices,bool OtherConst>
          friend constexpr bool _operator_equal(std::index_sequence<_indices...>,const Iterator<OtherConst>&i,const Sentinel&s)
          {
              return ((std::get<_indices>(i._current) == std::get<_indices>(s._end)) || ...);
          }

            template<size_t..._indices,bool OtherConst>
          friend constexpr auto _operator_sub(std::index_sequence<_indices...>,const Iterator<OtherConst>&i,const Sentinel&s)
          {
              using Ret = std::common_type_t<range_difference_t<_maybe_const_t<OtherConst, Views>>...>;
              return ranges::min({Ret(std::get<_indices>(i._current) - std::get<_indices>(s._end))...},
                         ranges::less{},
                         [](Ret i) {return std::make_unsigned_t<Ret>(i < 0 ? -i : i);});
          }

        template<bool OtherConst,REQUIRE((sized_sentinel_for<sentinel_t<_maybe_const_t<Const, Views>>,
				       iterator_t<_maybe_const_t<OtherConst, Views>>> && ...))>
        friend constexpr auto
        operator-(const Iterator<OtherConst>& x, const Sentinel& y)
        {return _operator_sub(std::index_sequence_for<Views...>(),x,y);}

          template<bool OtherConst,REQUIRE((sized_sentinel_for<sentinel_t<_maybe_const_t<Const, Views>>,
                         iterator_t<_maybe_const_t<OtherConst, Views>>> && ...))>
          friend constexpr auto operator-(const Sentinel& y, const Iterator<OtherConst>& x)
        { return -(x - y); }
      };
  };

    namespace views
    {
        namespace _details
        {
            template<class ,typename...>
          inline constexpr bool _can_zip_view  = false;

            template<typename...T>
            inline constexpr bool _can_zip_view<std::void_t<decltype(zip_view<all_t<T>...>(std::declval<T>()...))>,T...>  = true;
        }

        template<typename T>
          inline constexpr empty_view<T> empty{};

        struct Zip
        {
            template<class...Views,REQUIRE(_details::_can_zip_view<void,Views...>||(sizeof...(Views)==0))>
          constexpr auto operator() [[nodiscard]] (Views&&... vs) const
          {
              if constexpr (sizeof...(Views) == 0)
                  return views::empty<std::tuple<>>;
              else
                  return zip_view<all_t<Views>...>(std::forward<Views>(vs)...);
          }
        };

        inline constexpr Zip zip;
    }
}
#endif //DONGDONG_RANGE_VIEW_HPP
