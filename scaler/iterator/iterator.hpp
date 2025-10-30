#ifndef SCALER_CPP_ITERATOR_HPP
#define SCALER_CPP_ITERATOR_HPP

#include "../concepts/iterator_concepts.hpp"
#include "../memory/pointer_traits.hpp"

namespace NAMESPACE_NAME
{
    template<class S,class=std::enable_if_t<semiregular<S>>>
    class move_sentinel
    {
    public:
        template<class T=S,
                class=std::enable_if_t<std::is_default_constructible_v<T>>>
        constexpr move_sentinel()noexcept(std::is_nothrow_default_constructible_v<T>)
        {}

        constexpr explicit move_sentinel(S x)
        noexcept(std::is_nothrow_default_constructible_v<S>)
                : _last(std::move(x))
        {}

        template<class S2, class=std::enable_if_t<convertible_to<const S2&, S>> >
        constexpr move_sentinel(const move_sentinel<S2>& other)
        noexcept(std::is_nothrow_constructible_v<S,const S2&>)
                : _last(other._last)
        {}

        template<class S2, class=std::enable_if_t<convertible_to<const S2&, S>>>
        constexpr move_sentinel& operator=(const move_sentinel<S2>& other)
        {
            _last = other._last;
            return *this;
        }

        [[nodiscard]]constexpr S base() const
        noexcept(std::is_nothrow_copy_constructible_v<S>)
        {return _last;}

        template<class Iter,
                class S2 = S,
                class=std::enable_if_t<sentinel_for<S2, Iter>>>
        friend constexpr bool operator==(const std::move_iterator<Iter>& i, const move_sentinel& s)
        {return i.base() == s.base();}

        template<class Iter,
                class S2 = S,
                class=std::enable_if_t<sentinel_for<S2, Iter>>>
        friend constexpr bool operator!=(const std::move_iterator<Iter>& i, const move_sentinel& s)
        {return i.base() != s.base();}

        template<class Iter,
                class S2 = S,
                class=std::enable_if_t<sentinel_for<S2, Iter>>>
        friend constexpr bool operator==(const move_sentinel& s, const std::move_iterator<Iter>& i)
        {return i.base() == s.base();}

        template<class Iter,
                class S2 = S,
                class=std::enable_if_t<sentinel_for<S2, Iter>>>
        friend constexpr bool operator!=(const move_sentinel& s, const std::move_iterator<Iter>& i)
        {return i.base() != s.base();}

        template<class Iter,class S2 = S,
                class=std::enable_if_t<sized_sentinel_for<S2, Iter>>>
        friend constexpr iter_difference_t<Iter> operator-(const move_sentinel& s, const std::move_iterator<Iter>& i)
        {return s.base() - i.base();}

        template<class Iter,class S2 = S,
                class=std::enable_if_t<sized_sentinel_for<S2, Iter>>>
        friend constexpr iter_difference_t<Iter> operator-(const std::move_iterator<Iter>& i, const move_sentinel& s)
        {return i.base() - s.base();}

    private:
        S _last{};
    };
}

namespace NAMESPACE_NAME
{
    template<class,class = void >
    struct _counted_iter_value_type
    { };

    template<class It>
    struct _counted_iter_value_type<It,std::enable_if_t<indirectly_readable<It>>>
    { using value_type = iter_value_t<It>; };

    template<class,class =void >
    struct _counted_iter_concept
    { };

    template<class It>
    struct _counted_iter_concept<It,std::void_t<typename It::iterator_concept>>
    { using iterator_concept = typename It::iterator_concept; };

    template<class,class = void>
    struct _counted_iter_cat
    { };

    template<class It>
    struct _counted_iter_cat<It,std::void_t<typename It::iterator_category>>
    { using iterator_category = typename It::iterator_category; };

    template<class It,class = void>
    struct counted_iterator
            : public _counted_iter_cat<It>,
              public  _counted_iter_concept<It>,
              public _counted_iter_value_type<It>,
              public _enable_default_ctor<default_initializable<It>>
    {
        static_assert(input_or_output_iterator<It>);
    public:
        using iterator_type = It;
        using difference_type = iter_difference_t<It>;

        constexpr counted_iterator() noexcept =default;

        constexpr
        counted_iterator(It x, iter_difference_t<It> n)
                : _current(std::move(x)), _length(n)
        { }

        template<class I,class=std::enable_if_t<convertible_to<const I&,It>>>
        constexpr counted_iterator(const counted_iterator<I>& x)
                : _current(x._current), _length(x._length)
        { }

        template<class I,class=std::enable_if_t<assignable_from<It&,const I&>>>
        constexpr counted_iterator&operator=(const counted_iterator<I>&x)
        {
            _current = x._current;
            _length = x._length;
            return *this;
        }

        [[nodiscard]]
        constexpr const It&base() const & noexcept
        { return _current; }

        [[nodiscard]]
        constexpr It base() &&noexcept(std::is_nothrow_move_constructible_v<It>)
        { return std::move(_current); }

        [[nodiscard]]
        constexpr iter_difference_t<It>
        count() const noexcept { return _length; }

        [[nodiscard]]
        constexpr decltype(auto)
        operator*()
        noexcept(noexcept(*_current))
        {return *_current;}

        template<bool  _v=_dereference<const It>::value,
                class = std::enable_if_t<_v>>
        constexpr decltype(auto)
        operator*() const
        //noexcept(noexcept(*_current))
        {return *_current;}

        template<bool  _v= contiguous_iterator<It>,
                class = std::enable_if_t<_v>>
        [[nodiscard]]
        constexpr auto operator->() const noexcept
        {return to_address(_current);}

        constexpr counted_iterator&operator++()
        {
            ++_current;
            --_length;
            return *this;
        }

        template<class I=It,class=std::enable_if_t<!forward_iterator<I>>>
        decltype(auto)operator++(int)
        {
            --_length;
            try
            {
                return _current++;
            }
            catch(...)
            {
                ++_length;
                throw;
            }
        }

        template<class I=It,class=std::enable_if_t<forward_iterator<I>>>
        constexpr counted_iterator operator++(int)
        {
            auto x = *this;
            ++*this;
            return x;
        }

        template<class I =It , class = std::enable_if_t<bidirectional_iterator<I>>>
        constexpr counted_iterator& operator--()
        {
            --_current;
            ++_length;
            return *this;
        }

        template<class I =It , class = std::enable_if_t<bidirectional_iterator<I>>>
        constexpr counted_iterator operator--(int)
        {
            auto x = *this;
            --*this;
            return x;
        }

        template<class I =It , class = std::enable_if_t<random_access_iterator<I>>>
        [[nodiscard]]
        constexpr counted_iterator
        operator+(iter_difference_t<It> n) const
        {return counted_iterator(_current + n, _length - n);}

        template<class I =It , class = std::enable_if_t<random_access_iterator<I>>>
        [[nodiscard]]
        friend constexpr counted_iterator
        operator+(iter_difference_t<It> n, const counted_iterator& x)
        {return x + n;}

        template<class I =It , class = std::enable_if_t<random_access_iterator<I>>>
        constexpr counted_iterator&
        operator+=(iter_difference_t<It> n)
        {
            _current += n;
            _length -= n;
            return *this;
        }

        template<class I =It , class = std::enable_if_t<random_access_iterator<I>>>
        [[nodiscard]]
        constexpr counted_iterator
        operator-(iter_difference_t<It> n) const
        {return counted_iterator(_current - n, _length + n);}

        template<class I,class=std::enable_if_t<common_with<I,It>>>
        [[nodiscard]]
        friend constexpr iter_difference_t<I>
        operator-(const counted_iterator& x,const counted_iterator<I>& y)
        { return y._length - x._length; }

        [[nodiscard]]
        friend constexpr iter_difference_t<It>
        operator-(const counted_iterator& x, default_sentinel_t)
        { return -x._length; }

        [[nodiscard]]
        friend constexpr iter_difference_t<It>
        operator-(default_sentinel_t, const counted_iterator& y)
        { return y._length; }

        template<class I =It , class = std::enable_if_t<random_access_iterator<I>>>
        constexpr counted_iterator&
        operator-=(iter_difference_t<It> n)
        {
            //static_assert(random_access_iterator<It>);
            _current -= n;
            _length += n;
            return *this;
        }

        template<class I =It , class = std::enable_if_t<random_access_iterator<I>>>
        [[nodiscard]]
        constexpr decltype(auto)
        operator[](iter_difference_t<It> n) const
        //noexcept(noexcept(_current[n]))
        {
            return _current[n];
        }

    public:
#define MAKE_OP(OP) \
        template<class I,class=std::enable_if_t<common_with<I,It>>>\
        [[nodiscard]]\
        friend constexpr bool\
        operator OP(const counted_iterator& x,const counted_iterator<I>& y)\
        { return x._length OP y._length; }                   \

        MAKE_OP(<)
        MAKE_OP(<=)
        MAKE_OP(>=)
        MAKE_OP(>)
        MAKE_OP(==)
        MAKE_OP(!=)

        [[nodiscard]]
        friend constexpr bool operator!=(const counted_iterator& x, default_sentinel_t)
        {return x.count() != 0;}
        [[nodiscard]]friend constexpr bool operator!=(default_sentinel_t, const counted_iterator& x)
        {return x.count() != 0;}

        [[nodiscard]]
        friend constexpr bool operator==(const counted_iterator& x, default_sentinel_t)
        { return x._length == 0; }

        [[nodiscard]]
        friend constexpr bool operator==(default_sentinel_t,const counted_iterator& x)
        { return x._length == 0; }

    public:
        template<class I = It,class = std::enable_if_t<input_iterator<I>>>
        [[nodiscard]]
        friend constexpr iter_rvalue_reference_t<I>
        iter_move(const counted_iterator& i)
        noexcept(noexcept(ranges::iter_move(i._current)))
        {
            return ranges::iter_move(i._current);
        }

        template<class I,class=std::enable_if_t<indirectly_swappable<I,It>>>
        friend constexpr void
        iter_swap(const counted_iterator& x,const counted_iterator<I>& y)
        noexcept(noexcept(ranges::iter_swap(x._current, y._current)))
        {ranges::iter_swap(x._current, y._current);}
    private:
        //template<class I,class=std::enable_if_t<input_or_output_iterator<I>>> friend class counted_iterator;

        It _current = It();//base() 访问的底层迭代器
        iter_difference_t<It> _length = 0;//底层迭代器与其范围末尾之间的距离

#undef MAKE_REQUIRES
    };
}

namespace NAMESPACE_NAME
{
    template<class It,class = void>
    struct _basic_const_iterator_iter_cat
    { };

    template<class  It>
    struct _basic_const_iterator_iter_cat<It,std::enable_if_t<forward_iterator<It>>>
    { using iterator_category = typename iterator_traits<It>::iterator_category; };

    template<class U,class=void>
    struct _constant_iterator
            :std::false_type
    {};

    template<class U>
    struct _constant_iterator<U,std::enable_if_t<input_iterator<U> &&same_as<iter_const_reference_t<U>, iter_reference_t<U>>>>
            :std::true_type
    {};

    template<class Iter>
    class basic_const_iterator
            : public _basic_const_iterator_iter_cat<Iter>,
              public _enable_default_ctor<default_initializable<Iter>>
    {
    private:
        static auto constexpr _basic_const_iterator_concept()noexcept
        {
            if constexpr (random_access_iterator<Iter>)
                return random_access_iterator_tag{};
            else if constexpr (bidirectional_iterator<Iter>)
                return bidirectional_iterator_tag{};
            else if constexpr (forward_iterator<Iter>)
                return forward_iterator_tag{};
            else
                return input_iterator_tag{};
        }
    private:
        Iter _current;
    public:
        static_assert(input_iterator<Iter>, "Constraints not satisfied");
        using value_type = iter_value_t<Iter>;
        using difference_type = iter_difference_t<Iter>;
        using reference = iter_const_reference_t<Iter>;
        using rvalue_reference = common_reference_t<const iter_value_t<Iter>&&, iter_rvalue_reference_t<Iter>>;
        using iterator_concept = decltype(_basic_const_iterator_concept());
    public:
        //ctor
        constexpr basic_const_iterator() = default;

        constexpr basic_const_iterator(Iter x)
                : _current(std::move(x))
        {}

        template<class  U, class = std::enable_if_t<convertible_to<U, Iter>> >
        constexpr basic_const_iterator(basic_const_iterator<U> other)
                : _current(std::move(other._current))
        {}

        template<class T, class = std::enable_if_t<different_from<T, basic_const_iterator>&&convertible_to<T, Iter>>>
        constexpr basic_const_iterator(T&& x)
                : _current(std::forward<T>(x))
        {}

        constexpr const Iter& base() const& noexcept {return _current;}
        constexpr Iter base() && {return std::move(_current);}

        constexpr reference operator*() const {return static_cast<reference>(*base());}

        template<class I = Iter,
                class = std::enable_if_t<std::is_lvalue_reference_v<iter_reference_t<I>>&&same_as<remove_cvref_t<iter_reference_t<I>>, value_type>>>
        constexpr const auto* operator->() const
        {
            if constexpr(contiguous_iterator<Iter>)
                return to_address(*base());
            else
                return std::addressof(*base());
        }

        template<class I = Iter,
                class = std::enable_if_t<random_access_iterator<I>>>
        constexpr iter_const_reference_t<I> operator[](difference_type n) const
        {
            return static_cast<iter_const_reference_t<Iter>>(base()[n]);
        }

        constexpr basic_const_iterator& operator++()
        {
            ++_current;
            return *this;
        }

        template<class I = Iter,
                class = std::enable_if_t<forward_iterator<I>>>
        constexpr void operator++(int)
        {
            ++_current;
        }

        template<class I = Iter,class =std::enable_if_t<forward_iterator<I>>>
        constexpr basic_const_iterator operator++(int)
        {
            auto tmp = *this;
            ++*this;
            return tmp;
        }

        template<typename I = Iter, class =std::enable_if_t<bidirectional_iterator<I>>>
        constexpr basic_const_iterator& operator--()
        {
            --_current;
            return *this;
        }

        template<typename I = Iter, class =std::enable_if_t<bidirectional_iterator<I>>>
        constexpr basic_const_iterator operator--(int)
        {
            auto tmp = *this;
            --*this;
            return tmp;
        }

        template<typename I = Iter, class =std::enable_if_t<random_access_iterator<I>>>
        constexpr basic_const_iterator& operator+=(difference_type n)
        {
            _current += n;
            return *this;
        }

        template<class I = Iter, class = std::enable_if_t<random_access_iterator<I>>>
        constexpr basic_const_iterator& operator-=(difference_type n)
        {
            _current -= n;
            return *this;
        }

        template<class Other,class = std::enable_if_t<_constant_iterator<Other>::value
                &&different_from<Other,basic_const_iterator>
                &&convertible_to<const Iter&, Other>>>
        constexpr operator Other() const&
        {
            return _current;
        }

        template<class Other,class = std::enable_if_t<_constant_iterator<Other>::value
                &&different_from<Other,basic_const_iterator>
                &&convertible_to<Iter, Other>>>
        constexpr operator Other() &&
        {
            return std::move(_current);
        }

        template<class S,class = std::enable_if_t<sentinel_for<S,Iter>>>
        constexpr bool
        operator==(const S& s) const
        noexcept(noexcept(_current == s))
        { return _current == s; }

#define OPERATOR_OVERLOAD(OP) \
         template<class I = Iter , class = std::enable_if_t<random_access_iterator<I>>>                                 \
        constexpr bool operator OP(const basic_const_iterator& other) const\
        noexcept(noexcept(base()OP other.base()))\
        { return base()OP other.base(); }\
        template<class I , class = std::enable_if_t<random_access_iterator<Iter>                                          \
        &&totally_ordered_with<Iter,I> \
        &&different_from<basic_const_iterator,I>>>                                \
        constexpr bool operator OP(const I&other) const\
        noexcept(noexcept(base()OP other))\
        { return base()OP other; }      \

        OPERATOR_OVERLOAD(<)
        OPERATOR_OVERLOAD(>)
        OPERATOR_OVERLOAD(<=)
        OPERATOR_OVERLOAD(>=)

#undef OPERATOR_OVERLOAD

        template<class  I = Iter, class = std::enable_if_t<random_access_iterator<I>>>
        friend constexpr basic_const_iterator operator+(const basic_const_iterator& i, difference_type n)
        {return basic_const_iterator(i.base() + n);}

        template<class I = Iter, class= std::enable_if_t<random_access_iterator<I>>>
        friend constexpr basic_const_iterator operator+(difference_type n, const basic_const_iterator& i) {
            return basic_const_iterator(i.base() + n);
        }

        template<typename I = Iter, class = std::enable_if_t<random_access_iterator<I>>>
        friend constexpr basic_const_iterator operator-(const basic_const_iterator& i, difference_type n) {
            return basic_const_iterator(i.base() - n);
        }

        template<typename S, class =std::enable_if_t<sized_sentinel_for<S, Iter>>>
        constexpr difference_type operator-(const S& s) const
        {return _current - s;}

        template<class S, class = std::enable_if_t<different_from<S,basic_const_iterator>&&sized_sentinel_for<S, Iter>> >
        friend constexpr difference_type
        operator-( const S& s, const basic_const_iterator& i )
        {return s - i.base();}

        friend constexpr rvalue_reference
        iter_move(const basic_const_iterator& i)
        noexcept(noexcept(static_cast<rvalue_reference>(ranges::iter_move(i.base()))))
        {return static_cast<rvalue_reference>(ranges::iter_move(i));}
    };

    template<class I>
    using const_iterator = std::enable_if_t<input_iterator<I>,
            std::conditional_t<_constant_iterator<I>::value,I,basic_const_iterator<I>>>;

    template<class S>
    using const_sentinel = std::enable_if_t< semiregular<S>,
            std::conditional_t<input_iterator<S>,const_iterator<S>,S>>;

    template<class I, class =std::enable_if_t<input_iterator<I>>>
    constexpr const_iterator<I>
    make_const_iterator(I it) { return it; }

    template<typename S, class=std::enable_if_t<semiregular<S> >>
    constexpr const_sentinel<S>
    make_const_sentinel(S s) { return s; }

    template<class,class,class = void>
    struct _basic_const_iterator_common_type
    {};

    template<class T,class U>
    struct _basic_const_iterator_common_type<basic_const_iterator<T>,U,
            std::enable_if_t<common_with<U,T>&&input_iterator<std::common_type_t<T,U>>>>
    {
        using type =basic_const_iterator<std::common_type_t<T,U>>;
    };

    template<class T,class U>
    struct _basic_const_iterator_common_type<U,basic_const_iterator<T>,
    std::enable_if_t<common_with<U,T>&&input_iterator<std::common_type_t<T,U>>>>
    {
        using type = basic_const_iterator<std::common_type_t<T,U>>;
    };

    template<class T,class U>
    struct _basic_const_iterator_common_type<basic_const_iterator<T>,basic_const_iterator<U>,
    std::enable_if_t<common_with<U,T>&&input_iterator<std::common_type_t<T,U>>>>
    {
        using type = basic_const_iterator<std::common_type_t<T,U>>;
    };
}

namespace std
{
    template<class T,class U>
    struct common_type<NAMESPACE_NAME::basic_const_iterator<T>,U>
        :NAMESPACE_NAME::_basic_const_iterator_common_type<NAMESPACE_NAME::basic_const_iterator<T>,U>
    {};

    template<class T,class U>
    struct common_type<U,NAMESPACE_NAME::basic_const_iterator<T>>
        :NAMESPACE_NAME::_basic_const_iterator_common_type<U,NAMESPACE_NAME::basic_const_iterator<T>>
    {};

    template<class T,class U>
    struct common_type<NAMESPACE_NAME::basic_const_iterator<T>,NAMESPACE_NAME::basic_const_iterator<U>>
        :NAMESPACE_NAME::_basic_const_iterator_common_type<NAMESPACE_NAME::basic_const_iterator<T>,NAMESPACE_NAME::basic_const_iterator<U>>
    {};
}

namespace NAMESPACE_NAME
{
    template<class I,class S,bool =  std::is_trivially_destructible_v<I>&&std::is_trivially_destructible_v<S>>
    struct _common_iterator_base
            :_enable_default_ctor<default_initializable<I>>
    {
        SMF_IS(_common_iterator_base,default,default,default,default,default)
        constexpr _common_iterator_base( I i ):_iter{std::move(i)},_index{0}{}
        constexpr _common_iterator_base( S s ):_sent{std::move(s)},_index{0}{}

    ~_common_iterator_base()noexcept
    {
        if (this->_index == 0)
            this->_iter.~I();
        else if (this->_index == 1)
            this->_sent.~S();
    }
    private:
        union
        {
            I _iter;
            S _sent;
        };
        unsigned char _index; // 0 == _iter, 1 ==_sent, 2 == valueless
    };

    template<class I,class S>
    struct _common_iterator_base<I,S,true>
            :_enable_default_ctor<default_initializable<I>>
    {
        SMF_IS(_common_iterator_base,default,default,default,default,default)
        constexpr _common_iterator_base( I i ):_iter{std::move(i)},_index{0}{}
        constexpr _common_iterator_base( S s ):_sent{std::move(s)},_index{0}{}

        ~_common_iterator_base()noexcept =default;
    private:
        union
        {
            I _iter;
            S _sent;
        };
        unsigned char _index; // 0 == _iter, 1 ==_sent, 2 == valueless
    };

    template< class I,class S>
    class common_iterator
            :public _common_iterator_base<I,S>
    {
    private:
        struct _proxy
        {
            iter_value_t<I> _keep;
            constexpr _proxy(iter_reference_t<I>&& x)
                : _keep(std::move(x))
            {}
        public:
            constexpr const iter_value_t<I>* operator->() const noexcept {return std::addressof(_keep);}
        };

        struct _postfix_proxy
        {
            iter_value_t<I> _keep;
            constexpr _postfix_proxy(iter_reference_t<I>&& x)
                : _keep(std::forward<iter_reference_t<I>>(x)) {}
        public:
            constexpr const iter_value_t<I>& operator*() const noexcept {return _keep;}
        };

        template<class T,class = void>
        struct _enable_operator_advance
                :std::false_type
        {};

        template<class T>
        struct _enable_operator_advance<T,std::enable_if_t<_can_reference<decltype(*std::declval<T&>()++)>::value>>
                :std::false_type
        {};

        template<class T,class = void>
        struct _enable_arrow
                :std::false_type
        {};

        template<class T>
        struct _enable_arrow<T,std::enable_if_t< indirectly_readable<const T>
                &&(_has_arrow_operator_v<const T>
                ||std::is_reference_v<iter_reference_t<I>>
                ||constructible_from<iter_value_t<I>,iter_reference_t<I>>)>>
                :std::true_type
        {};

        static_assert(input_or_output_iterator<I>&&sentinel_for<S,I>&&!same_as<I, S> &&copyable<I>);
    public:
        static constexpr bool _arrow(){return _enable_arrow<I>::value;}

        using _common_iterator_base<I,S>::_common_iterator_base;

        template< class I2, class S2 ,class = std::enable_if_t<convertible_to<const I2&, I> &&convertible_to<const S2&, S>>>
        constexpr common_iterator( const common_iterator<I2, S2>& x )
        {
            this->_index  = x._index;
            if (this->_index == x._index)
            {
                if (this->_index == 0)
                    this->_iter = x._iter;
                else if (this->_index == 1)
                    this->_sent = x._sent;
            }
            else
            {
                //先析构已有的对象
                if (this->_index == 0)
                    this->_iter.~I();
                else if (this->_index == 1)
                    this->_sent.~S();
                this->_index = 2;
                if (this->_index == 0)
                    ::new(addressof(this->_iter)) I(x._iter);
                else if(this->_index == 1)
                    ::new(addressof(this->_sent)) S(x._sent);
                this->_index = x._index;
            }
        }

        template< class I2, class S2 ,
                class = std::enable_if_t<convertible_to<const I2&, I>
                        &&convertible_to<const S2&, S>
                        &&assignable_from<I&, const I2&>
                        &&assignable_from<S&, const S2&>>>
        constexpr common_iterator& operator=( const common_iterator<I2, S2>& x )
        {
            if constexpr (std::is_trivially_default_constructible_v<I>)
            {
                if (this->_index == 0)
                    this->_iter = std::move(x._iter);
                else if(this->_index == 1)
                    this->_sent = std::move(x._sent);
            }
            else
            {
                if (this->_index == 0)
                    ::new(addressof(this->_iter)) I(x._iter);
                else if(this->_index == 1)
                    ::new(addressof(this->_sent)) S(x._sent);
            }
            return *this;
        }

        [[nodiscard]]constexpr decltype(auto)operator*(){return *this->_iter;}

        [[nodiscard]]constexpr decltype(auto)operator*()const{return *this->_iter;}

        [[nodiscard]]constexpr auto operator->() const
        {
            static_assert(_enable_arrow<I>::value);
            if constexpr (std::is_pointer_v<I> || _has_arrow_operator_v<I>)
                return this->_iter;
            else if constexpr (std::is_reference_v<iter_reference_t<I>>)
            {
                auto&& tmp = *this->_iter;
                return addressof(tmp);
            }
            else
                return _proxy{*this->_iter};
        }

        constexpr common_iterator&operator++()
        {
            ++this->_iter;
            return *this;
        }

        constexpr decltype(auto)operator++(int)
        {
            if constexpr (forward_iterator<I>)
            {
                common_iterator tmp = *this;
                ++*this;
                return tmp;
            }
            else if constexpr (_enable_operator_advance<I>::value
                    ||indirectly_readable<I>
                    ||constructible_from<iter_value_t<I>, iter_reference_t<I>>
                    ||move_constructible<iter_value_t<I> >)
                return this->_iter++;
            else
            {
                _postfix_proxy p(**this);
                ++*this;
                return p;
            }
        }

        template<class I2, class S2,
                class = std::enable_if_t<sentinel_for<S2, I>&&sentinel_for<S, I2>>>
        friend constexpr bool operator==(const common_iterator& x, const common_iterator<I2, S2>& y)
        {
            if constexpr(equality_comparable_with<I,I2>)
                switch(x._index << 2 | y._index)
                {
                    case 0b0101:
                        return true;
                    case 0b0000:
                        return x._iter == y._iter;
                    case 0b0001:
                        return x._iter == y._sent;
                    case 0b0100:
                        return x._sent == y._iter;
                    default:
                        UNREACHABLE
                }
            else
                switch(x._index << 2 | y._index)
                {
                    case 0b0000:
                    case 0b0101:
                        return true;
                    case 0b0001:
                        return x._iter == y._sent;
                    case 0b0100:
                        return x._sent == y._iter;
                    default:
                        UNREACHABLE
            }
        }

        template<class I2, class S2,
                class = std::enable_if_t<sentinel_for<S2, I>&&sentinel_for<S, I2>>>
        friend constexpr bool operator!=(const common_iterator& x, const common_iterator<I2, S2>& y)
        {return !(x == y);}

        template<class I2, class S2,class =  _require_t<
                sized_sentinel_for<I2, I>,
                sized_sentinel_for<S2, I>,
                sentinel_for<S, I2>>>
        friend constexpr iter_difference_t<I2> operator-(const common_iterator& x, const common_iterator<I2, S2>& y)
        {
            switch(x._index << 2 | y._index)
            {
                case 0b0101:
                    return 0;
                case 0b0000:
                    return x._iter - y._iter;
                case 0b0001:
                    return x._iter - y._sent;
                case 0b0100:
                    return x._sent - y._iter;
                default:
                    UNREACHABLE
            }
        }

        template<class II =I,class = std::enable_if_t<input_iterator<II>>>
        [[nodiscard]]friend constexpr iter_rvalue_reference_t<I>iter_move(const common_iterator& i)
        noexcept(noexcept(ranges::iter_move(std::declval<const I&>())))
        {return ranges::iter_move(i._iter);}

        template<class I2,class S2, class = std::enable_if_t<indirectly_swappable<I2, I>>>
        friend constexpr void iter_swap(const common_iterator& x, const common_iterator<I2, S2>& y)
        noexcept(noexcept(ranges::iter_swap(std::declval<const I&>(), std::declval<const I2&>())))
        {ranges::iter_swap(x._iter, y._iter);}
    };

    template<class It, class Sent>
    struct incrementable_traits<common_iterator<It, Sent>>
    {
        using difference_type = iter_difference_t<It>;
    };

    template<class It, class Sent>
    struct iterator_traits<common_iterator<It, Sent>>
    {
    private:
        template<class,class = void>
        struct _traits_ptr
        {
            using type = void;
        };

        template<class I>
        struct _traits_ptr<I,std::enable_if_t<common_iterator<It, Sent>::_arrow()>>
        {
            using type = decltype(std::declval<const common_iterator<I, Sent>&>().operator->());
        };

        template<class,class = void>
        struct _traits_category
                :std::false_type
        {};

        template<class I>
        struct _traits_category<I,std::enable_if_t<derived_from< typename iterator_traits<It>::iterator_category,forward_iterator_tag>>>
                :std::true_type
        {};
    public:
        using iterator_concept = std::conditional_t<forward_iterator<It>,forward_iterator_tag,input_iterator_tag>;
        using iterator_category = std::conditional_t<_traits_category<It>::value,forward_iterator_tag,input_iterator_tag>;
        using value_type = iter_value_t<It>;
        using difference_type = iter_difference_t<It>;
        using pointer = typename _traits_ptr<It>::type;
        using reference = iter_reference_t<It>;
    };
}
#endif //SCALER_CPP_ITERATOR_HPP
