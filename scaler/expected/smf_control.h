

#ifndef EXPECTED_SMF_CONTROL_H
#define EXPECTED_SMF_CONTROL_H

#include <utility>
#include <functional>

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


#define USING_BASE(...) \
    using  _base = __VA_ARGS__;\
    using _base::_base;\
    using _base::operator=;\

namespace DongDong
{
    template<class T>
    using remove_cvref_t = std::remove_cv_t<std::remove_reference_t<T>>;

    template<class F,class...Args>
    using _monadic_result_t = remove_cvref_t<std::invoke_result_t<F&&,Args&&...>>;

    template<class F,class...Args>
    using _monadic_result_0_t = std::remove_cv_t<std::invoke_result_t<F&&,Args&&...>>;
}

namespace DongDong
{
    template<class E>
    class unexpected
    {
        static_assert( std::is_object_v<E>
                       && (!std::is_array_v<E>)
                       //&& (!_expected::_is_unexpected<E>)
                       && (!std::is_const_v<E>) && (!std::is_volatile_v<E>) );

    public:
        constexpr unexpected(const unexpected&) = default;
        constexpr unexpected(unexpected&&) = default;

        template<class Err = E,
                class =std::enable_if_t<!std::is_same_v<remove_cvref_t<Err>, unexpected>
                                        &&!std::is_same_v<remove_cvref_t<Err>, std::in_place_t>
                                        && std::is_constructible_v<E,Err>>>
        constexpr explicit
        unexpected(Err&& e)
        noexcept(std::is_nothrow_constructible_v<E,Err>)
                : _value(std::forward<Err>(e))
        { }

        template<typename... Args,
                class = std::enable_if_t<std::is_constructible_v<E, Args...>>>
        constexpr explicit
        unexpected(std::in_place_t,Args&&... args)
        noexcept(std::is_nothrow_constructible_v<E, Args...>)
                : _value(std::forward<Args>(args)...)
        { }

        template<class U,class...Args,
                class = std::enable_if_t<std::is_constructible_v<E, std::initializer_list<U>&, Args...>>>
        constexpr explicit
        unexpected(std::in_place_t, std::initializer_list<U> il, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<E, std::initializer_list<U>&,Args...>)
                : _value(il, std::forward<Args>(args)...)
        { }

        constexpr unexpected& operator=(const unexpected&) = default;
        constexpr unexpected& operator=(unexpected&&) = default;

        [[nodiscard]]constexpr const E&error() const & noexcept { return _value; }
        [[nodiscard]]constexpr E&error() & noexcept { return _value; }
        [[nodiscard]]constexpr const E&&error() const && noexcept { return std::move(_value); }
        [[nodiscard]]constexpr E&&error() && noexcept { return std::move(_value); }

        template<class EE = E,class = std::enable_if_t<std::is_swappable_v<EE>>>
        constexpr void swap(unexpected& other) noexcept(std::is_nothrow_swappable_v<E>)
        {
            using std::swap;
            swap(_value, other._value);
        }

        template<class Err>
        [[nodiscard]]
        friend constexpr bool
        operator==(const unexpected& x, const unexpected<Err>& y)
        { return x._value == y.error(); }

        template<class Err>
        [[nodiscard]]
        friend constexpr bool
        operator!=(const unexpected& x, const unexpected<Err>& y)
        { return x._value != y.error(); }

        friend constexpr void
        swap(unexpected& x, unexpected& y) noexcept(noexcept(x.swap(y)))
        { x.swap(y); }
    private:
        E _value;
    };

    template <class E> unexpected(E) -> unexpected<E>;
}

namespace DongDong
{
    template<class T,bool = std::is_void_v<T>/*true*/>
    class bad_expected_access
            :public std::exception
    {
    protected:
        bad_expected_access() noexcept = default;
        bad_expected_access(const bad_expected_access&) = default;
        bad_expected_access(bad_expected_access&&) = default;
        bad_expected_access& operator=(const bad_expected_access&) = default;
        bad_expected_access& operator=(bad_expected_access&&) = default;
        ~bad_expected_access() override = default;
    public:
        [[nodiscard]]
        const char*what() const noexcept override
        { return "bad access to std::expected without expected value"; }
    };

    template<class T>
    class bad_expected_access<T,false>
            :public bad_expected_access<void>
    {
    public:
        template<class E=T,class=std::enable_if_t<!std::is_void<E>::value>>
        explicit bad_expected_access(E e)
                :M_value{e}
        {}
    public:
        [[nodiscard]]T&error() & noexcept{ return M_value; }
        [[nodiscard]]const T&error() const & noexcept{ return M_value; }
        [[nodiscard]]T&&error() && noexcept{ return std::move(M_value); }
        [[nodiscard]]const T&&error() const && noexcept{ return std::move(M_value); }

        [[nodiscard]]
        const char* what() const noexcept override
        { return "bad access utility::expected without expected value"; }
    private:
        T M_value;
    };

    struct unexpect_t{explicit unexpect_t() = default;};
    inline constexpr unexpect_t unexpect{};
}

namespace DongDong
{
    template<class T,class E,bool = std::is_void_v<T>>struct expected;

    template<class>
    inline constexpr auto _is_expected_v = false;

    template<class T,class E>
    inline constexpr auto _is_expected_v<expected<T,E>> = true;

    template<class>
    inline constexpr bool _is_unexpected_v = false;

    template<class T>
    inline constexpr bool _is_unexpected_v<unexpected<T>> = true;
}

namespace DongDong
{
    template<typename T, typename... Args>
    constexpr auto
    construct_at(T* location, Args&&... args)
    noexcept(noexcept(::new((void*)0) T(std::declval<Args>()...)))
    -> decltype(::new((void*)0) T(std::declval<Args>()...))
    { return ::new((void*)location) T(std::forward<Args>(args)...); }

    template<class NewType, class OldType, class... Args>
    void _expected_reinit(NewType& new_val, OldType& old_val, Args&&... args)
    {
        // Case 1: the construction of “new_val” is non-throwing:
        // “new_val” can be directly constructed after destroying “old_val”
        if constexpr (std::is_nothrow_constructible_v<NewType, Args...>)
        {
            std::destroy_at(std::addressof(old_val));
            construct_at(std::addressof(new_val), std::forward<Args>(args)...);
        }
            // Case 2: the move construction of “new_val” is non-throwing:
            // construct a temporary NewType object first
            // (“old_val” is left intact if an exception is thrown from this construction)
        else if constexpr (std::is_nothrow_move_constructible_v<NewType>)
        {
            NewType temp(std::forward<Args>(args)...); // may throw
            std::destroy_at(std::addressof(old_val));
            construct_at(std::addressof(new_val), std::move(temp));
        }
            // Case 3: the construction of “new_val” is potentially-throwing:
            // a backup of “old_val” is required in order to recover from an exception
        else
        {
            OldType temp(std::move(old_val)); // may throw
            destroy_at(std::addressof(old_val));
            try
            {
                construct_at(std::addressof(new_val),std::forward<Args>(args)...); // may throw
            }
            catch (...)
            {
                construct_at(std::addressof(old_val), std::move(temp));
                throw;
            }
        }
    }
}

namespace DongDong
{
    namespace _expected
    {
        inline constexpr auto _unexpected_ctor = std::in_place_index_t<0>{};
        inline constexpr auto _expected_ctor = std::in_place_index_t<1>{};
    }

    //Smf Control
    enum class smf_state
    {
        smf_delete,smf_trivially,smf_normal
    };

    template<smf_state>struct enable_default_ctor{SMF_IS(enable_default_ctor,default,default,default,default,default)};
    template<smf_state>struct enable_copy_ctor{SMF_IS(enable_copy_ctor,default,default,default,default,default)};
    template<smf_state>struct enable_move_ctor{SMF_IS(enable_move_ctor,default,default,default,default,default)};
    template<smf_state>struct enable_copy_assign{SMF_IS(enable_copy_assign,default,default,default,default,default)};
    template<smf_state>struct enable_move_assign{SMF_IS(enable_move_assign,default,default,default,default,default)};
    //template<>struct enable_default_ctor<smf_state::smf_delete>{SMF_IS(enable_default_ctor,delete,default,default,default,default)};
    //template<>struct enable_move_ctor<smf_state::smf_delete>{SMF_IS(enable_move_ctor,default,default,delete,default,default)};
    //template<>struct enable_copy_ctor<smf_state::smf_delete>{SMF_IS(enable_copy_ctor,default,delete,default,default,default)};
    //template<>struct enable_copy_assign<smf_state::smf_delete>{SMF_IS(enable_copy_assign,default,default,default,delete,default)};
    //template<>struct enable_move_assign<smf_state::smf_delete>{SMF_IS(enable_move_assign,default,default,default,default,delete)};

    template<class T>
    inline constexpr smf_state _expected_default_ctor = std::is_default_constructible_v<T>?smf_state::smf_normal:smf_state::smf_delete;

    template<class T,class E>
    inline constexpr bool _expected_trivially_destroy = std::is_void_v<T>?
                                                        std::is_trivially_destructible_v<E>
                                                                         :(std::is_trivially_destructible_v<T> &&std::is_trivially_destructible_v<E>);

    template<class...T>
    inline constexpr auto _expected_copy  = (std::is_trivially_copy_constructible_v<T>&&...)
                                            ?smf_state::smf_trivially
                                            : ((std::is_copy_constructible_v<T>&&...)?smf_state::smf_normal:smf_state::smf_delete);

    template<class ... T>
    inline constexpr auto _expected_move  = (std::is_trivially_move_constructible_v<T>&&...)
                                            ?smf_state::smf_trivially
                                            : ((std::is_move_constructible_v<T>&&...)?smf_state::smf_normal:smf_state::smf_delete);

    template<class T,class E,bool = std::is_void_v<T>>
    struct _expected_swap
            :std::bool_constant<std::is_swappable_v<T>
                                &&std::is_swappable_v<E>
                                &&std::is_move_constructible_v<T>
                                && std::is_move_constructible_v<E>
                                &&std::is_nothrow_move_constructible_v<T> || std::is_nothrow_move_constructible_v<E>>
    {};

    template<class T,class E>
    struct _expected_swap<T,E,true>
            :std::bool_constant<std::is_swappable_v<E> &&std::is_move_constructible_v<E>>
    {};

    template<class T,class E,bool = std::is_void_v<T>>
    struct _expected_swap_noexcept
            :std::bool_constant<std::is_nothrow_move_constructible_v<T> && std::is_nothrow_swappable_v<T> &&
                                    std::is_nothrow_move_constructible_v<E> && std::is_nothrow_swappable_v<E>>
    {};

    template<class T,class E>
    struct _expected_swap_noexcept<T,E,true>
            :std::bool_constant<std::is_nothrow_move_constructible_v<E> && std::is_nothrow_swappable_v<E>>
    {};

    template<class T,class E,class = void>
    struct _expected_copy_assign_control
            :std::integral_constant<smf_state,std::is_copy_assignable_v<T>
                                              &&std::is_copy_constructible_v<T>
                                              &&std::is_copy_assignable_v<E>
                                              &&std::is_copy_constructible_v<E>
                                              &&(std::is_nothrow_move_constructible_v<T> || std::is_nothrow_move_constructible_v<E>)?smf_state::smf_normal:smf_state::smf_delete>
    {};

    template<class T,class E>
    struct _expected_copy_assign_control<T,E,std::enable_if_t<std::is_void_v<T>>>
            :std::integral_constant<smf_state,std::is_copy_constructible_v<E>&& std::is_copy_assignable_v<E>?smf_state::smf_normal:smf_state::smf_delete>
    {};

    template<class T,class E,class = void>
    struct _expected_move_assign_control
            :std::integral_constant<smf_state,std::is_move_assignable_v<T>
                                              &&std::is_move_constructible_v<T>
                                              &&std::is_move_assignable_v<E>
                                              &&std::is_move_constructible_v<E>
                                              &&(std::is_nothrow_move_constructible_v<T> || std::is_nothrow_move_constructible_v<E>)?smf_state::smf_normal:smf_state::smf_delete>
    {};

    template<class T,class E>
    struct _expected_move_assign_control<T,E,std::enable_if_t<std::is_void_v<T>>>
            :std::integral_constant<smf_state,std::is_move_constructible_v<E>&& std::is_move_assignable_v<E>?smf_state::smf_normal:smf_state::smf_delete>
    {};

    template<class T,class E,bool = std::is_void_v<T>>
    struct _expected_copy_assign_noexcept
            :std::conjunction<std::is_nothrow_copy_constructible<T>,
                    std::is_nothrow_copy_constructible<E>,
                    std::is_nothrow_copy_assignable<T>,
                    std::is_nothrow_copy_assignable<E>>
    {};

    template<class T,class E>
    struct _expected_copy_assign_noexcept<T,E,true>
            :std::bool_constant<std::is_nothrow_copy_constructible_v<E>&&
                                std::is_nothrow_copy_assignable_v<E>>
    {};

    template<class T,class E,bool = std::is_void_v<T>>
    struct _expected_move_assign_noexcept
            :std::bool_constant<std::is_nothrow_move_constructible_v<T> && std::is_nothrow_move_assignable_v<T> &&
                                std::is_nothrow_move_constructible_v<E> && std::is_nothrow_move_assignable_v<E>>
    {};

    template<class T,class E>
    struct _expected_move_assign_noexcept<T,E,true>
            :std::bool_constant<std::is_nothrow_move_constructible_v<E> && std::is_nothrow_move_assignable_v<E>>
    {};

    template<class T,class E,bool = std::is_void_v<T>,bool =_expected_trivially_destroy<T, E>>struct _expected_storage;

    template<class T,class E>
    struct _expected_storage<T,E,true,true>
    {
        ~_expected_storage()=default;

        constexpr _expected_storage()noexcept
                :_has_value{true}
        {}

        SMF_IS_1(_expected_storage,default,default,default,default)

        template<typename... Args,
                class = std::enable_if_t<std::is_constructible_v<E, Args...>>>
        constexpr explicit
        _expected_storage(unexpect_t, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<E, Args...>)
                : _error(std::forward<Args>(args)...), _has_value(false)
        { }

        template<class  U, class... Args,
                class = std::enable_if_t<std::is_constructible_v<E, std::initializer_list<U>&, Args...>>>
        constexpr explicit
        _expected_storage(unexpect_t, std::initializer_list<U> i, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<E, std::initializer_list<U>&,Args...>)
                : _error(i, std::forward<Args>(args)...), _has_value(false)
        { }

        template<class Callable>
        explicit constexpr _expected_storage(std::in_place_index_t<1>, Callable&& callable)
                :_has_value(true)
        { std::forward<Callable>(callable)(); }

        template<class Callable>
        explicit constexpr _expected_storage(std::in_place_index_t<0>, Callable&&callable)
                : _error(std::forward<Callable>(callable)()), _has_value(false)
        { }

        constexpr void emplace() noexcept
        {
            if (!_has_value)
            {
                std::destroy_at(__builtin_addressof(_error));
                _has_value = true;
            }
        }

        template<class V>
        constexpr void _assign_error(V&& _v)
        {
            if (_has_value)
            {
                construct_at(__builtin_addressof(_error),std::forward<V>(_v));
                _has_value = false;
            }
            else
                _error = std::forward<V>(_v);
        }

        union
        {
            E _error;
        };

        bool _has_value;
    };

    template<class T,class E>
    struct  _expected_storage<T,E,false,true>
    {
    public:
        template<class CC = T,class = std::enable_if_t<std::is_default_constructible_v<CC>>>
        _expected_storage()noexcept(std::is_nothrow_default_constructible_v<T>)
                :_value{},_has_value{true}
        {}

        template<class Callable>
        explicit constexpr _expected_storage(std::in_place_index_t<1>,Callable&& callable)
            : _value(std::forward<Callable>(callable)()), _has_value(true)
        { }

        template<class Callable>
        explicit constexpr _expected_storage(std::in_place_index_t<0>,Callable&& callable)
            : _error(std::forward<Callable>(callable)()), _has_value(true)
        { }

        template<class U = T,class = std::enable_if_t<
                (!std::is_same_v<remove_cvref_t<U>, expected<T,E, false>>)
                && (!std::is_same_v<remove_cvref_t<U>, std::in_place_t>)
                && (!_is_unexpected_v<remove_cvref_t<U>>)
                && std::is_constructible_v<T, U>>>
        constexpr
        //explicit(!std::is_convertible_v<U, T>)
        _expected_storage(U&& v)noexcept(std::is_nothrow_constructible_v<T, U>)
            : _value(std::forward<U>(v)),_has_value(true)
        { }

        template<class G = E,class = std::enable_if_t<std::is_constructible_v<E, const G&>>>
        constexpr
        //explicit(!std::is_convertible_v<const Gr, E>)
        _expected_storage(const unexpected<G>& u)
        noexcept(std::is_nothrow_constructible_v<E, const G&>)
                : _error(u.error()), _has_value(false)
        { }

        template<class G = E,class = std::enable_if_t<std::is_constructible_v<E, const G&>>>
        constexpr
        //explicit(!std::is_convertible_v<Gr, E>)
        _expected_storage(unexpected<G>&& u)
        noexcept(std::is_nothrow_constructible_v<E,G>)
            : _error(u.error()), _has_value(false)
        { }

        template<typename... Args,
                class = std::enable_if_t< std::is_constructible_v<T, Args...>>>
        constexpr explicit
        _expected_storage(std::in_place_t, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<T, Args...>)
            : _value(std::forward<Args>(args)...), _has_value(true)
        { }

        template<typename U, typename... Args,
                class = std::enable_if_t<std::is_constructible_v<T, std::initializer_list<U>&, Args...>>>
        constexpr explicit
        _expected_storage(std::in_place_t, std::initializer_list<U> i, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<T, std::initializer_list<U>&,Args...>)
            : _value(i, std::forward<Args>(args)...), _has_value(true)
        { }

        template<typename... Args,
                class = std::enable_if_t<std::is_constructible_v<E, Args...>>>
        constexpr explicit _expected_storage(unexpect_t, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<E, Args...>)
            : _error(std::forward<Args>(args)...), _has_value(false)
        { }

        template<class U,class... Args,
                class = std::enable_if_t<std::is_constructible_v<E, std::initializer_list<U>&, Args...>>>
        constexpr explicit
        _expected_storage(unexpect_t, std::initializer_list<U> i, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<E, std::initializer_list<U>&,Args...>)
            : _error(i, std::forward<Args>(args)...), _has_value(false)
        { }

        SMF_IS_1(_expected_storage,default,default,default,default)
        ~_expected_storage()=default;
    public:
        template<class V>
        constexpr void _assign_value(V&& _v)
        {
            if (_has_value)
                _value = std::forward<V>(_v);
            else
            {
                _expected_reinit((_value),(_error),std::forward<V>(_v));
                _has_value = true;
            }
        }

        template<class V>
        constexpr void _assign_error(V&& _v)
        {
            if (_has_value)
            {
                _expected_reinit((_error),(_value),std::forward<V>(_v));
                _has_value = false;
            }
            else
                _error = std::forward<V>(_v);
        }

        template<class... Args,class = std::enable_if_t<std::is_nothrow_constructible_v<T, Args...>>>
        constexpr T&emplace(Args&&... args) noexcept
        {
            if (_has_value)
                std::destroy_at(__builtin_addressof(_value));
            else
            {
                std::destroy_at(__builtin_addressof(_error));
                _has_value = true;
            }
            construct_at(__builtin_addressof(_value),std::forward<Args>(args)...);
            return _value;
        }

        template<class U,class... Args,class = std::enable_if_t<std::is_nothrow_constructible_v<T,std::initializer_list<U>&,Args...>>>
        constexpr T&emplace(std::initializer_list<U>il, Args&&... args) noexcept
        {
            if (_has_value)
                std::destroy_at(__builtin_addressof(_value));
            else
            {
                std::destroy_at(__builtin_addressof(_error));
                _has_value = true;
            }
            construct_at(__builtin_addressof(_value),il,std::forward<Args>(args)...);
            return _value;
        }
    public://Storage
        union
        {
            E _error;
            T _value;
        };

        bool _has_value = true;
    };

    template<class T,class E>
    struct  _expected_storage<T,E,false,false>
    {
    public:
        template<class CC = T,class = std::enable_if_t<std::is_default_constructible_v<CC>>>
        _expected_storage()noexcept(std::is_nothrow_default_constructible_v<T>)
                :_value{},_has_value{true}
        {}

        template<class Callable>
        explicit constexpr _expected_storage(std::in_place_index_t<1>,Callable&& callable)
                : _value(std::forward<Callable>(callable)()), _has_value(true)
        { }

        template<class Callable>
        explicit constexpr _expected_storage(std::in_place_index_t<0>,Callable&& callable)
                : _error(std::forward<Callable>(callable)()), _has_value(true)
        { }

        template<class U = T,class = std::enable_if_t<
                (!std::is_same_v<remove_cvref_t<U>, expected<T,E, false>>)
                && (!std::is_same_v<remove_cvref_t<U>, std::in_place_t>)
                && (!_is_unexpected_v<remove_cvref_t<U>>)
                && std::is_constructible_v<T, U>>>
        constexpr
        //explicit(!std::is_convertible_v<U, T>)
        _expected_storage(U&& v)noexcept(std::is_nothrow_constructible_v<T, U>)
                : _value(std::forward<U>(v)),_has_value(true)
        { }

        template<class G = E,class = std::enable_if_t<std::is_constructible_v<E, const G&>>>
        constexpr
        //explicit(!std::is_convertible_v<const Gr, E>)
        _expected_storage(const unexpected<G>& u)
        noexcept(std::is_nothrow_constructible_v<E, const G&>)
                : _error(u.error()), _has_value(false)
        { }

        template<class G = E,class = std::enable_if_t<std::is_constructible_v<E, const G&>>>
        constexpr
        //explicit(!std::is_convertible_v<Gr, E>)
        _expected_storage(unexpected<G>&& u)
        noexcept(std::is_nothrow_constructible_v<E,G>)
                : _error(u.error()), _has_value(false)
        { }

        template<typename... Args,
                class = std::enable_if_t< std::is_constructible_v<T, Args...>>>
        constexpr explicit
        _expected_storage(std::in_place_t, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<T, Args...>)
                : _value(std::forward<Args>(args)...), _has_value(true)
        { }

        template<typename U, typename... Args,
                class = std::enable_if_t<std::is_constructible_v<T, std::initializer_list<U>&, Args...>>>
        constexpr explicit
        _expected_storage(std::in_place_t, std::initializer_list<U> i, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<T, std::initializer_list<U>&,Args...>)
                : _value(i, std::forward<Args>(args)...), _has_value(true)
        { }

        template<typename... Args,
                class = std::enable_if_t<std::is_constructible_v<E, Args...>>>
        constexpr explicit _expected_storage(unexpect_t, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<E, Args...>)
                : _error(std::forward<Args>(args)...), _has_value(false)
        { }

        template<class U,class... Args,
                class = std::enable_if_t<std::is_constructible_v<E, std::initializer_list<U>&, Args...>>>
        constexpr explicit
        _expected_storage(unexpect_t, std::initializer_list<U> i, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<E, std::initializer_list<U>&,Args...>)
                : _error(i, std::forward<Args>(args)...), _has_value(false)
        { }

        SMF_IS_1(_expected_storage,default,default,default,default)
        ~_expected_storage()noexcept(std::is_nothrow_destructible_v<E>)
        {
            if(this->_has_value)
                std::destroy_at(std::addressof(this->_value));
            else
                std::destroy_at(std::addressof(this->_error));
        }

    public:
        template<class V>
        constexpr void _assign_value(V&& _v)
        {
            if (_has_value)
                _value = std::forward<V>(_v);
            else
            {
                _expected_reinit((_value),(_error),std::forward<V>(_v));
                _has_value = true;
            }
        }

        template<class V>
        constexpr void _assign_error(V&& _v)
        {
            if (_has_value)
            {
                _expected_reinit((_error),(_value),std::forward<V>(_v));
                _has_value = false;
            }
            else
                _error = std::forward<V>(_v);
        }

        template<class... Args,class = std::enable_if_t<std::is_nothrow_constructible_v<T, Args...>>>
        constexpr T&emplace(Args&&... args) noexcept
        {
            if (_has_value)
                std::destroy_at(__builtin_addressof(_value));
            else
            {
                std::destroy_at(__builtin_addressof(_error));
                _has_value = true;
            }
            construct_at(__builtin_addressof(_value),std::forward<Args>(args)...);
            return _value;
        }

        template<class U,class... Args,class = std::enable_if_t<std::is_nothrow_constructible_v<T,std::initializer_list<U>&,Args...>>>
        constexpr T&emplace(std::initializer_list<U>il, Args&&... args) noexcept
        {
            if (_has_value)
                std::destroy_at(__builtin_addressof(_value));
            else
            {
                std::destroy_at(__builtin_addressof(_error));
                _has_value = true;
            }
            construct_at(__builtin_addressof(_value),il,std::forward<Args>(args)...);
            return _value;
        }
    public://Storage
        union
        {
            E _error;
            T _value;
        };

        bool _has_value = true;
    };

    template<class T,class E>
    struct  _expected_storage<T,E,true,false>
    {
        constexpr _expected_storage()noexcept
                :_has_value{true}
        {}

        SMF_IS_1(_expected_storage,default,default,default,default)

        template<typename... Args,
                class = std::enable_if_t<std::is_constructible_v<E, Args...>>>
        constexpr explicit
        _expected_storage(unexpect_t, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<E, Args...>)
                : _error(std::forward<Args>(args)...), _has_value(false)
        { }

        template<class  U, class... Args,
                class = std::enable_if_t<std::is_constructible_v<E, std::initializer_list<U>&, Args...>>>
        constexpr explicit
        _expected_storage(unexpect_t, std::initializer_list<U> i, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<E, std::initializer_list<U>&,Args...>)
                : _error(i, std::forward<Args>(args)...), _has_value(false)
        { }

        template<class Callable>
        explicit constexpr _expected_storage(std::in_place_index_t<1>, Callable&& callable)
                :_has_value(true)
        { std::forward<Callable>(callable)(); }

        template<class Callable>
        explicit constexpr _expected_storage(std::in_place_index_t<0>, Callable&&callable)
                : _error(std::forward<Callable>(callable)()), _has_value(false)
        { }

        ~_expected_storage()noexcept(std::is_nothrow_destructible_v<E>)
        {
            if(!this->_has_value)
                std::destroy_at(std::addressof(this->_error));
        }

        constexpr void emplace() noexcept
        {
            if (!_has_value)
            {
                std::destroy_at(__builtin_addressof(_error));
                _has_value = true;
            }
        }

        template<class V>
        constexpr void _assign_error(V&& _v)
        {
            if (_has_value)
            {
                construct_at(__builtin_addressof(_error),std::forward<V>(_v));
                _has_value = false;
            }
            else
                _error = std::forward<V>(_v);
        }

        union
        {
            E _error;
        };

        bool _has_value;
    };

    template<class T,class E,auto =  _expected_copy<T,E>/*smf_state::smf_normal*/>
    struct _expected_copy_ctor
            :_expected_storage<T,E>,
             enable_copy_ctor<_expected_copy<T,E>>
    {
        USING_BASE(_expected_storage<T,E>)
        SMF_IS(_expected_copy_ctor,default,default,default,default,default)
    };

    template<class T,class E>
    struct _expected_copy_ctor<T,E,smf_state::smf_normal>
            :_expected_storage<T,E>
    {
        using Nothrow = std::conditional_t<std::is_void_v<T>,std::is_nothrow_copy_constructible<E>,
                std::conjunction<std::is_nothrow_copy_constructible<T>,std::is_nothrow_copy_constructible<E>>>;

        USING_BASE(_expected_storage<T,E>)
        SMF_IS_2(_expected_copy_ctor,default,default,default,default)
        constexpr _expected_copy_ctor(const _expected_copy_ctor&other)
        noexcept(Nothrow::value)
        {
            this->_has_value=other._has_value;
            if constexpr(std::is_void_v<T>)
            {
                if (!this->_has_value)
                    construct_at(__builtin_addressof(this->_error),other._error);
            }
            else
            {
                if (this->_has_value)
                    construct_at(__builtin_addressof(this->_value),other._value);
                else
                    construct_at(__builtin_addressof(this->_error),other._error);
            }
        }
    };

    template<class T,class E,auto =  _expected_move<T,E>/*smf_state::smf_normal*/>
    struct _expected_move_ctor
            :_expected_copy_ctor<T,E>,
             enable_move_ctor<_expected_copy<T,E>>
    {
        USING_BASE(_expected_copy_ctor<T,E>)
        SMF_IS(_expected_move_ctor,default,default,default,default,default)
    };

    template<class T,class E>
    struct _expected_move_ctor<T,E,smf_state::smf_normal>
            :_expected_copy_ctor<T,E>
    {
        using Nothrow = std::conditional_t<std::is_void_v<T>,std::is_nothrow_move_constructible<E>,
                std::conjunction<std::is_nothrow_move_constructible<T>,std::is_nothrow_move_constructible<E>>>;

        USING_BASE(_expected_copy_ctor<T,E>)
        SMF_IS_3(_expected_move_ctor,default,default,default,default)
        constexpr _expected_move_ctor(_expected_move_ctor&&other)
        noexcept(Nothrow::value)
        {
            this->_has_value=other._has_value;
            if constexpr(std::is_void_v<T>)
            {
                if (!this->_has_value)
                    construct_at(__builtin_addressof(this->_error),std::move(other._error));
            }
            else
            {
                if (this->_has_value)
                    construct_at(__builtin_addressof(this->_value),std::move(other._value));
                else
                    construct_at(__builtin_addressof(this->_error),std::move(other._error));
            }
        }
    };

    template<class T,class E,smf_state =  _expected_copy_assign_control<T,E>::value>
    struct _expected_copy_assign
            :_expected_move_ctor<T,E>
    {
        USING_BASE(_expected_move_ctor<T,E>)
        SMF_IS(_expected_copy_assign,default,default,default,delete,default)
    };

    template<class T,class E>
    struct _expected_copy_assign<T,E,smf_state::smf_normal>
            :_expected_move_ctor<T,E>
    {
        USING_BASE(_expected_move_ctor<T,E>)
        SMF_IS_4(_expected_copy_assign,default,default,default,default)

        constexpr _expected_copy_assign&operator=(const _expected_copy_assign&other)
        noexcept(_expected_copy_assign_noexcept<T,E>::value)
        {
            if constexpr (std::is_void_v<T>)
            {
                if (other._has_value)
                    this->emplace();
                else
                    this->_assign_error((other._error));
            }
            else
            {
                if (other._has_value)
                    this->_assign_value(other._value);
                else
                    this->_assign_error(other._error);
            }
            return *this;
        }
    };

    template<class T,class E,smf_state = _expected_move_assign_control<T,E>::value>
    struct _expected_move_assign
            :_expected_copy_assign<T,E>
    {
        USING_BASE(_expected_copy_assign<T,E>)
        SMF_IS(_expected_move_assign,default,default,default,delete,default)
    };

    template<class T,class E>
    struct _expected_move_assign<T,E,smf_state::smf_normal>
            :_expected_copy_assign<T,E>
    {
        USING_BASE(_expected_copy_assign<T,E>)
        SMF_IS_5(_expected_move_assign,default,default,default,default)
        constexpr _expected_move_assign&operator=(_expected_move_assign&&other)
        noexcept(_expected_move_assign_noexcept<T,E>::value)
        {
            if constexpr (std::is_void_v<T>)
            {
                if (other._has_value)
                    this->emplace();
                else
                    this->_assign_error(std::move(other._error));
            }
            else
            {
                if (other._has_value)
                    this->_assign_value(std::move(other._value));
                else
                    this->_assign_error(std::move(other._error));
            }
            return *this;
        }
    };
}

namespace DongDong
{
    template<class T,class E>
    struct expected<T,E,false>
            :_expected_move_assign<T,E>
    {
        template<class A,class B,bool>friend class expected;

        using _base = _expected_move_assign<T,E>;

        template<typename U, typename Err, typename Error = unexpected<E>>
        static constexpr bool _construct_from_expected
                = std::disjunction_v<std::is_constructible<T, expected<U, Err>&>,
        std::is_constructible<T, expected<U, Err>>,
        std::is_constructible<T, const expected<U, Err>&>,
        std::is_constructible<T, const expected<U, Err>>,
        std::is_convertible<expected<U, Err>&, T>,
        std::is_convertible<expected<U, Err>, T>,
        std::is_convertible<const expected<U, Err>&, T>,
        std::is_convertible<const expected<U, Err>, T>,
        std::is_constructible<Error, expected<U, Err>&>,
        std::is_constructible<Error, expected<U, Err>>,
        std::is_constructible<Error, const expected<U, Err>&>,
        std::is_constructible<Error, const expected<U, Err>>
        >;
    public:
        using value_type = T;
        using error_type = E;
        using unexpected_type = unexpected<E>;

        template<typename U>
        using rebind = expected<U, error_type>;

        using _base::_base;
        using _base::operator=;
        using _base::emplace;

        template< class U, class G ,
                class = std::enable_if_t<!_construct_from_expected<U,G>&&std::is_constructible_v<T,const U&>&&std::is_convertible_v<E,const G&>>>
        constexpr expected( const expected<U, G>& other )
        {
            this->_has_value = other._has_value;
            if (this->_has_value)
                construct_at(__builtin_addressof(this->_value), other._value);
            else
                construct_at(__builtin_addressof(this->_error), other._error);
        }

        template< class U, class G ,
                class = std::enable_if_t<!_construct_from_expected<U,G>&&std::is_constructible_v<T,U>&&std::is_convertible_v<E,G>>>
        constexpr expected(expected<U, G>&& other )
        {
            this->_has_value = other._has_value;
            if (this->_has_value)
                construct_at(__builtin_addressof(this->_value), std::move(other)._value);
            else
                construct_at(__builtin_addressof(this->_error), std::move(other)._error);
        }

        SMF_IS(expected,default,default,default,default,default)

        template<class U = T,
                class = std::enable_if_t<
                        !std::is_same_v<expected, remove_cvref_t<U>>
                        &&!_is_unexpected_v<remove_cvref_t<U>>
                        &&std::is_constructible_v<T, U>
                        &&std::is_assignable_v<T&, U>
                        &&(std::is_nothrow_constructible_v<T, U> || std::is_nothrow_move_constructible_v<T> ||std::is_nothrow_move_constructible_v<E>)
                        >>
        constexpr expected&operator=(U&& v)
        {
            this->_assign_value(std::forward<U>(v));
            return *this;
        }

        template<class G,class = std::enable_if_t<std::is_constructible_v<E, const G&>
                &&std::is_assignable_v<E&, const G&>
                &&(std::is_nothrow_constructible_v<E, const G&> || std::is_nothrow_move_constructible_v<T> ||std::is_nothrow_move_constructible_v<E>)>>
        constexpr expected&operator=(const unexpected<G>& e)
        {
            this->_assign_error(e.error());
            return *this;
        }

        template<class G,class = std::enable_if_t<std::is_constructible_v<E, G>
                                                  &&std::is_assignable_v<E&, G>
                                                  &&(std::is_nothrow_constructible_v<E,G> || std::is_nothrow_move_constructible_v<T> ||std::is_nothrow_move_constructible_v<E>)>>
        constexpr expected&operator=(unexpected<G>&& e)
        {
            this->_assign_error(std::move(e.error()));
            return *this;
        }

        [[nodiscard]]constexpr const T*operator->() const noexcept{return __builtin_addressof(this->_value);}
        [[nodiscard]]constexpr T*operator->() noexcept{return __builtin_addressof(this->_value);}

        [[nodiscard]]constexpr const T&operator*() const & noexcept{return this->_value;}
        [[nodiscard]]constexpr T&operator*() & noexcept{ return this->_value;}
        [[nodiscard]] constexpr const T&&operator*() const && noexcept{return std::move(this->_value);}
        [[nodiscard]]constexpr T&&operator*() && noexcept{return std::move(this->_value);}

        [[nodiscard]]constexpr explicit operator bool() const noexcept{ return this->_has_value; }
        [[nodiscard]]constexpr bool has_value() const noexcept{ return this->_has_value; }

#define MAKE_VALUE_ERROR(THROW,R1,R2,POST,...)\
    constexpr  R1 value()POST\
    {\
        if(this->_has_value)\
            return __VA_ARGS__(this->_value);\
        throw bad_expected_access<E>(THROW(this->_error));\
    }\
    constexpr R2 error()POST{return __VA_ARGS__(this->_error);}

        MAKE_VALUE_ERROR(std::as_const, T&, E&, &)
        MAKE_VALUE_ERROR(std::as_const, const T&, const E&, const&)
        MAKE_VALUE_ERROR(std::move, T&&, E&&, &&)
        MAKE_VALUE_ERROR(std::move, const T&&, E&&,const&&)

#undef MAKE_VALUE_ERROR

        template<class U = std::remove_cv_t<T>,class = std::enable_if_t<std::is_copy_constructible_v<T>&&std::is_convertible_v<U,T>>>
        constexpr T value_or(U&& default_value) const &
        {return has_value() ? **this :static_cast<T>(std::forward<U>(default_value));}

        template<class U = std::remove_cv_t<T>,class = std::enable_if_t<std::is_move_constructible_v<T>&&std::is_convertible_v<U,T>>>
        constexpr T value_or(U&& default_value)&&
        {has_value() ? std::move(**this):static_cast<T>(std::forward<U>(default_value));}

        template<class U = E,class = std::enable_if_t<std::is_copy_constructible_v<E>&&std::is_convertible_v<U,T>>>
        constexpr E error_or(U&& v) const &
        {return this->_has_value ? std::forward<U>(v) :this->_error;}

        template<class U = E,class = std::enable_if_t<std::is_move_constructible_v<E>&&std::is_convertible_v<U,T>>>
        constexpr E error_or(U&& v)&&
        {return this->_has_value? std::forward<U>(v) :std::move(this->_error);}

        template<class Fn,class U = _monadic_result_t<Fn,T&>,
        class = std::enable_if_t<std::is_same_v<typename U::error_type,E>&&_is_expected_v<U>&&std::is_copy_constructible_v<E>>>
        constexpr auto and_then(Fn&& f) &
        {
            if (has_value())
                return std::invoke(std::forward<Fn>(f),value());
            else
                return U(unexpect, error());
        }

        template<class Fn,class U = _monadic_result_t<Fn,const T&>,
                class =std::enable_if_t<std::is_same_v<typename U::error_type,E>&&_is_expected_v<U>&&std::is_copy_constructible_v<E>>>
        constexpr auto and_then(Fn&& f)const&
        {
            if (has_value())
                return std::invoke(std::forward<Fn>(f),value());
            else
                return U(unexpect, error());
        }

        template<class Fn,class U = _monadic_result_t<Fn,T&&>,
        class =std::enable_if_t<std::is_same_v<typename U::error_type,E>&&_is_expected_v<U>&&std::is_move_constructible_v<E>>>
        constexpr auto and_then(Fn&& f) &&
        {
            if (has_value())
                return std::invoke(std::forward<Fn>(f), std::move(value()));
            else
                return U(unexpect, std::move(error()));
        }

        template<class Fn,class U = _monadic_result_t<Fn,const T&&>,
                class =std::enable_if_t<std::is_same_v<typename U::error_type,E>&&_is_expected_v<U>&&std::is_move_constructible_v<E>>>
        constexpr auto and_then(Fn&& f)const&&
        {
            if (has_value())
                return std::invoke(std::forward<Fn>(f), std::move(value()));
            else
                return U(unexpect, std::move(error()));
        }

        template<class Callable,class = std::enable_if_t<std::is_constructible_v<T, T&>>>
        constexpr auto
        or_else(Callable&& callable) &
        {
            using Res = _monadic_result_t<Callable, E&>;
            static_assert(_is_expected_v<Res>,"DongDong::expected<T, E>::or_else 的返回值必须是一个是DongDong::expected");
            static_assert(std::is_same_v<typename Res::value_type, T>);

            if (this->has_value())
                return Res(std::in_place, this->_value);
            else
                return std::invoke(std::forward<Callable>(callable), this->_error);
        }

        template<class Callable,class = std::enable_if_t<std::is_constructible_v<T, const T&>>>
        constexpr auto
        or_else(Callable&& callable) const&
        {
            using Res = _monadic_result_t<Callable, const E&>;
            static_assert(_is_expected_v<Res>,"DongDong::expected<T, E>::or_else 的返回值必须是一个是DongDong::expected");
            static_assert(std::is_same_v<typename Res::value_type, T>);

            if (this->has_value())
                return Res(std::in_place, this->_value);
            else
                return std::invoke(std::forward<Callable>(callable), this->_error);
        }

        template<class Callable,class = std::enable_if_t<std::is_constructible_v<T, T>>>
        constexpr auto
        or_else(Callable&& callable) &&
        {
            using Res = _monadic_result_t<Callable, E&&>;
            static_assert(_is_expected_v<Res>,"DongDong::expected<T, E>::or_else 的返回值必须是一个是DongDong::expected");
            static_assert(std::is_same_v<typename Res::value_type, T>);

            if (this->has_value())
                return Res(std::in_place, std::move(this->_value));
            else
                return std::invoke(std::forward<Callable>(callable), std::move(this->_error));
        }

        template<class Callable,class = std::enable_if_t<std::is_constructible_v<T, const T>>>
        constexpr auto
        or_else(Callable&& callable)const&&
        {
            using Res = _monadic_result_t<Callable, const E&&>;
            static_assert(_is_expected_v<Res>,"DongDong::expected<T, E>::or_else 的返回值必须是一个是DongDong::expected");
            static_assert(std::is_same_v<typename Res::value_type, T>);

            if (this->has_value())
                return Res(std::in_place, std::move(this->_value));
            else
                return std::invoke(std::forward<Callable>(callable), std::move(this->_error));
        }

        template<class Callable,class = std::enable_if_t<std::is_constructible_v<E, E&>>>
        constexpr auto
        transform(Callable&&callable) &
        {
            using U = _monadic_result_0_t<Callable, T&>;
            using Res = expected<U, E>;

            if (has_value())
                return Res(_expected::_expected_ctor, [&]() {
                    return std::invoke(std::forward<Callable>(callable),this->_value);
                });
            else
                return Res(unexpect, this->_error);
        }

        template<class Callable,class = std::enable_if_t<std::is_constructible_v<E, const E&>>>
        constexpr auto
        transform(Callable&&callable) const&
        {
            using U = _monadic_result_0_t<Callable, const T&>;
            using Res = expected<U, E>;

            if (has_value())
                return Res(_expected::_expected_ctor, [&]() {
                    return std::invoke(std::forward<Callable>(callable),this->_value);
                });
            else
                return Res(unexpect, this->_error);
        }

        template<class Callable,class = std::enable_if_t<std::is_constructible_v<E, E>>>
        constexpr auto
        transform(Callable&&callable)&&
        {
            using U = _monadic_result_0_t<Callable, T&>;
            using Res = expected<U, E>;

            if (has_value())
                return Res(_expected::_expected_ctor, [&]() {
                    return std::invoke(std::forward<Callable>(callable),std::move(this->_value));
                });
            else
                return Res(unexpect, std::move(this->_error));
        }

        template<class Callable,class = std::enable_if_t<std::is_constructible_v<E, const E>>>
        constexpr auto
        transform(Callable&&callable)const&&
        {
            using U = _monadic_result_0_t<Callable, T&>;
            using Res = expected<U, E>;

            if (has_value())
                return Res(_expected::_expected_ctor, [&]() {
                    return std::invoke(std::forward<Callable>(callable),std::move(this->_value));
                });
            else
                return Res(unexpect, std::move(this->_error));
        }

        template<class Callable,class = std::enable_if_t<std::is_constructible_v<T, T&>>>
        constexpr auto
        transform_error(Callable&&callable) &
        {
            using G = _monadic_result_0_t<Callable, E&>;
            using Res = expected<T, G>;

            if (has_value())
                return Res(std::in_place, this->_value);
            else
                return Res(_expected::_unexpected_ctor, [&]() {return std::invoke(std::forward<Callable>(callable),this->_error);});
        }

        template<class Callable,class = std::enable_if_t<std::is_constructible_v<T, const T&>>>
        constexpr auto
        transform_error(Callable&&callable)const&
        {
            using G = _monadic_result_0_t<Callable, const E&>;
            using Res = expected<T, G>;

            if (has_value())
                return Res(std::in_place, this->_value);
            else
                return Res(_expected::_unexpected_ctor, [&]() {return std::__invoke(std::forward<Callable>(callable),this->_error);});
        }

        template<class Callable,class = std::enable_if_t<std::is_constructible_v<T, const T>>>
        constexpr auto
        transform_error(Callable&&callable) const&&
        {
            using G = _monadic_result_0_t<Callable, const E&&>;
            using Res = expected<T, G>;

            if (has_value())
                return Res(std::in_place, std::move(this->_value));
            else
                return Res(_expected::_unexpected_ctor, [&]() {return std::invoke(std::forward<Callable>(callable),std::move(this->_error));});
        }

        template<class Callable,class = std::enable_if_t<std::is_constructible_v<T, T>>>
        constexpr auto
        transform_error(Callable&&callable) &&
        {
            using G = _monadic_result_0_t<Callable, E&&>;
            using Res = expected<T, G>;

            if (has_value())
                return Res(std::in_place, std::move(this->_value));
            else
                return Res(_expected::_unexpected_ctor, [&]() {return std::invoke(std::forward<Callable>(callable),std::move(this->_error));});
        }

        void   _swap(expected&other)
        {
            if constexpr (std::is_nothrow_move_constructible_v<E>)
            {
                E temp(std::move(other._error));
                std::destroy_at(std::addressof(other._error));
                try
                {
                    DongDong::construct_at(std::addressof(other._value), std::move(this->_value)); // 可能抛出异常
                    std::destroy_at(std::addressof(this->_value));
                    DongDong::construct_at(std::addressof(this->_error), std::move(temp));
                }
                catch(...)
                {
                    DongDong::construct_at(std::addressof(other._error), std::move(temp));
                    throw;
                }
            }
// 情况 2：expected 值的移动构造是非抛出的
// 如果 “this->_error” 的构造失败，“this->_value” 将被恢复
            else
            {
                T temp(std::move(this->_value));
                std::destroy_at(std::addressof(this->_value));
                try
                {
                    DongDong::construct_at(std::addressof(this->_error), std::move(other._error)); // 可能抛出异常
                    std::destroy_at(std::addressof(other._error));
                    DongDong::construct_at(std::addressof(other._value), std::move(temp));
                }
                catch(...)
                {
                    DongDong::construct_at(std::addressof(this->_value), std::move(temp));
                    throw;
                }
            }
            this->_has_value = false;
            other._has_value = true;
        }

        constexpr void  swap(expected&other)noexcept(_expected_swap_noexcept<T,E>::value)
        {
            static_assert(_expected_swap<T,E>::value);
            if(this->_has_value)
            {
                //双方都有就直接交换value
                if(other._has_value)
                {
                    using std::swap;
                    swap(this->_value,other._value);
                }
                else//自己有值对方无值
                    this->_swap(other);
            }
            else
            {
                    //z自己无值对面有值
                    if(other._has_value)
                        other.swap(*this);
                    else
                    {
                        //双方都没有值
                        using std::swap;
                        swap(this->_error,other._error);
                    }
            }
        }

        template<typename Up, typename Er2>
        friend constexpr bool
        operator==(const expected& x, const expected<Up, Er2,false>& y)
        noexcept(noexcept(bool(*x == *y)&& noexcept(bool(x.error() == y.error()))))
        {
            if (x.has_value())
                return y.has_value() && bool(*x == *y);
            else
                return !y.has_value() && bool(x.error() == y.error());
        }

        template<class U,class = std::enable_if_t<!_is_expected_v<remove_cvref_t<U>>>>
        friend constexpr bool
        operator==(const expected& x, const U& v)
        noexcept(noexcept(bool(*x == v)))
        { return x.has_value() && bool(*x == v); }

        template<class Er>
        friend constexpr bool
        operator==(const expected& x, const unexpected<Er>& e)
        noexcept(noexcept(bool(x.error() == e.error())))
        { return !x.has_value() && bool(x.error() == e.error()); }

        template<class U,class = std::enable_if_t<!_is_expected_v<remove_cvref_t<U>>>>
        friend constexpr bool
        operator==(const U& v,const expected& x)
        noexcept(noexcept(bool(*x == v)))
        { return x.has_value() && bool(*x == v); }

        template<class Er>
        friend constexpr bool
        operator==( const unexpected<Er>& e,const expected& x)
        noexcept(noexcept(bool(x.error() == e.error())))
        { return !x.has_value() && bool(x.error() == e.error()); }

        template<typename Up, typename Er2>
        friend constexpr bool
        operator!=(const expected& x, const expected<Up, Er2,false>& y)
        noexcept(noexcept(bool(*x != *y)&& noexcept(bool(x.error() != y.error()))))
        {return !(x == y);}

        template<class U,class = std::enable_if_t<!_is_expected_v<remove_cvref_t<U>>>>
        friend constexpr bool
        operator!=(const expected& x, const U& v)
        noexcept(noexcept(bool(*x != v)))
        { return !(x == v); }

        template<class Er>
        friend constexpr bool
        operator!=(const expected& x, const unexpected<Er>& e)
        noexcept(noexcept(bool(x.error() != e.error())))
        { return!(x == e); }

        template<class U,class = std::enable_if_t<!_is_expected_v<remove_cvref_t<U>>>>
        friend constexpr bool
        operator!=(const U& v,const expected& x)
        noexcept(noexcept(bool(*x != v)))
        { return !(x == v); }

        template<class Er>
        friend constexpr bool
        operator!=( const unexpected<Er>& e,const expected& x)
        noexcept(noexcept(bool(x.error() != e.error())))
        { return!(x == e); }
    };

    template<class T,class E>
    struct expected<T,E,true>
            :_expected_move_assign<T,E>
    {
        using _base = _expected_move_assign<T,E>;
    public:
        using _base::_base;
        using _base::operator=;
        using _base::emplace;

        template<class U, class G, class UE = unexpected<E>>
        static constexpr bool _construct_from_expected
                = std::disjunction_v<std::is_constructible<T, expected<U,G>&>,
                        std::is_constructible<T, expected<U,G>&&>,
                        std::is_constructible<T, const expected<U,G>&>,
                        std::is_constructible<T, const expected<U,G>&&>,
                        std::is_convertible<expected<U,G>&,T>,
                        std::is_convertible<expected<U,G>&&,T>,
                        std::is_convertible<const expected<U,G>&, T>,
                        std::is_convertible<const expected<U,G>&&, T>,
                        std::is_constructible<UE, expected<U,G>&>,
                        std::is_constructible<UE, expected<U,G>&&>,
                        std::is_constructible<UE, const expected<U,G>&>,
                        std::is_constructible<UE, const expected<U,G>&&>>;

        SMF_IS(expected,default,default,default,default,default)

        template<typename U, typename G,
                class = std::enable_if_t<
                std::is_void_v<U>
                 && std::is_constructible_v<E, const G&>
                 && (!_construct_from_expected<U, G>)>>
        constexpr
        //explicit(!std::is_convertible_v<const G&, E>)
        expected(const expected<U, G>& x)
        noexcept(std::is_nothrow_constructible_v<E, const G&>)
        {
            this->_has_value = x._has_value;
            if (!this->_has_value)
                std::construct_at(__builtin_addressof(this->_error), x._error);
        }

        template<typename U, typename G,
                class = std::enable_if_t<
                        std::is_void_v<U>
                        && std::is_constructible_v<E,G>
                        && (!_construct_from_expected<U, G>)>>
        constexpr
        //explicit(!std::is_convertible_v<const G&, E>)
        expected(expected<U, G>&& x)
        noexcept(std::is_nothrow_constructible_v<E, G>)
        {
            this->_has_value = x._has_value;
            if (!this->_has_value)
                std::construct_at(__builtin_addressof(this->_error), std::move(x)._error);
        }

        template<class G = E,
                class = std::enable_if_t<std::is_constructible_v<E, const G&>>>
        constexpr
        //explicit(!std::is_convertible_v<const G&, E>)
        expected(const unexpected<G>& u)
        noexcept(std::is_nothrow_constructible_v<E, const G&>)
        {
            this->_error = u.error();
            this->_has_value = false;
        }

        template<class G = E,
                class = std::enable_if_t<std::is_constructible_v<E, G>>>
        constexpr
        //explicit(!std::is_convertible_v<G, E>)
        expected(unexpected<G>&& u)
        noexcept(std::is_nothrow_constructible_v<E,G>)
        {
            this->_error = std::move(u).error();
            this->_has_value = false;
        }

        constexpr explicit expected(std::in_place_t) noexcept
                : expected()
        { }

        template<class G,class = std::enable_if_t<std::is_constructible_v<E, const G&>&&std::is_assignable_v<E&, const G&>>>
        constexpr expected&operator=(const unexpected<G>& e)
        {
            this->_assign_error(e.error());
            return *this;
        }

        template<class G,class = std::enable_if_t<std::is_constructible_v<E,G>&&std::is_assignable_v<E&, G>>>
        constexpr expected&operator=(unexpected<G>&& e)
        {
            this->_assign_error(std::move(e.error()));
            return *this;
        }

        [[nodiscard]]constexpr explicit operator bool() const noexcept { return this->_has_value; }
        [[nodiscard]]constexpr bool has_value() const noexcept { return this->_has_value; }
        constexpr void operator*() const noexcept { __glibcxx_assert(this->_has_value); }

        constexpr void value() const&
        {
            if(this->_has_value)
                return;
            throw bad_expected_access<E>(std::as_const(this->_error));
        }

        constexpr void value() &&
        {
            if(this->_has_value)
                return;
            throw bad_expected_access<E>(std::move(this->_error));
        }

        constexpr const E&error() const & noexcept{return this->_error;}
        constexpr const E&&error() const&& noexcept{return std::move(this->_error);}
        constexpr E&&error()&& noexcept{return std::move(this->_error);}
        constexpr E&error()& noexcept{return this->_error;}

        template<class U = E,class = std::enable_if_t<std::is_copy_constructible_v<E>&&std::is_convertible_v<U,T>>>
        constexpr E error_or(U&& v) const &
        {return this->_has_value ? std::forward<U>(v) :this->_error;}

        template<class U = E,class = std::enable_if_t<std::is_move_constructible_v<E>&&std::is_convertible_v<U,T>>>
        constexpr E error_or(U&& v)&&
        {return this->_has_value ? std::forward<U>(v) :std::move(this->_error);}

    #define MAKE_MONADIC(_a,_b,...)\
        template<class Callable,class = std::enable_if_t<std::is_constructible_v<E,_a>>>\
        constexpr auto transform(Callable&&callable) _b\
        {\
            using U = _monadic_result_0_t<Callable>;\
            using Res = expected<U, E>;\
            if (has_value())\
                return Res(_expected::_expected_ctor, std::forward<Callable>(callable));\
            else\
                return Res(unexpect, __VA_ARGS__(this->_error));\
        }\
        template<class Callable>\
        constexpr auto transform_error(Callable&&callable) _b\
        {\
            using G = _monadic_result_0_t<Callable, E _b>;\
            using Res = expected<T, G>;\
            if (has_value())\
                return Res();\
            else\
                return Res(_expected::_unexpected_ctor, [&]() {return std::invoke(std::forward<Callable>(callable),__VA_ARGS__(this->_error));});\
        }                            \
        template<class Callable>\
        constexpr auto or_else(Callable&&callable)_b\
        {\
            using G = _monadic_result_t<Callable, E _b>;\
            static_assert(_is_expected_v<G>);\
            static_assert(std::is_same_v<typename G::value_type, T>);\
            if (has_value())\
                return G();\
            else\
                return std::invoke(std::forward<Callable>(callable), __VA_ARGS__(this->_error));\
        }                            \
        template<class Callable,class = std::enable_if_t<std::is_constructible_v<E, _a>>>\
        constexpr auto and_then(Callable&& callable)_b\
        {\
            using U = _monadic_result_t<Callable>;\
            static_assert(_is_expected_v<U>);\
            static_assert(std::is_same_v<typename U::error_type, E>);\
            if (has_value())\
                return std::invoke(std::forward<Callable>(callable));\
            else\
                return U(unexpect, __VA_ARGS__(this->_error));\
        }

        MAKE_MONADIC(E &, &)
        MAKE_MONADIC(const E&, const&)
        MAKE_MONADIC(E &&, &&, std::move)
        MAKE_MONADIC(const E&&, const&&, std::move)

#undef MAKE_MONADIC
        constexpr void swap(expected&other)noexcept(_expected_swap_noexcept<T,E>::value)
        {
            static_assert(_expected_swap<T,E>::value);
            if (this->_has_value)
            {
                if (!other._has_value)
                {
                    construct_at(__builtin_addressof(this->_error),std::move(other._error));
                    std::destroy_at(__builtin_addressof(other._error));
                    this->_has_value = false;
                    this->_has_value = true;
                }
            }
            else
            {
                if (other._has_value)
                {
                    construct_at(__builtin_addressof(other._error),std::move(this->_error)); // might throw
                    std::destroy_at(__builtin_addressof(this->_error));
                   this->_has_value = true;
                    other._has_value = false;
                }
                else
                {
                    using std::swap;
                    swap(this->_error, other._error);
                }
            }
        }

        template<class U,class E2>
        friend constexpr bool
        operator==(const expected& x, const expected<U, E2,true>& y)
        noexcept(noexcept(bool(x.error() == y.error())))
        {
            if (x.has_value())
                return y.has_value();
            else
                return !y.has_value() && bool(x.error() == y.error());
        }

        template<class U,class E2>
        friend constexpr bool
        operator!=(const expected& x, const expected<U, E2,true>& y)
        noexcept(noexcept(bool(x.error() != y.error())))
        {return !(x==y);}

        template<class E2>
        friend constexpr bool
        operator==(const expected& x, const unexpected<E2>& e)
        noexcept(noexcept(bool(x.error() == e.error())))
        { return !x.has_value() && bool(x.error() == e.error()); }

        template<class E2>
        friend constexpr bool
        operator!=(const expected& x, const unexpected<E2>& e)
        noexcept(noexcept(bool(x.error() != e.error())))
        {return !(x==e);}

        template<class E2>
        friend constexpr bool
        operator==( const unexpected<E2>& e,const expected& x)
        noexcept(noexcept(bool(x.error() == e.error())))
        { return !x.has_value() && bool(x.error() == e.error()); }

        template<class E2>
        friend constexpr bool
        operator!=( const unexpected<E2>& e,const expected& x)
        noexcept(noexcept(bool(x.error() != e.error())))
        {return !(x==e);}

    };
}

#undef SMF_IS
#undef SMF_IS_1
#undef SMF_IS_2
#undef SMF_IS_3
#undef SMF_IS_4
#undef USING_BASE

#endif //EXPECTED_SMF_CONTROL_H
