#ifndef SCALER_CPP_SPAN_H
#define SCALER_CPP_SPAN_H

#include<array>
#include<cstddef>
#include "memory/pointer_traits.hpp"
#include "iterator/iterator.hpp"
#include"ranges/range_algorithm.hpp"

namespace NAMESPACE_NAME
{
    inline constexpr size_t dynamic_extent = static_cast<size_t>(-1);
    template<class,size_t >class span;
}

namespace NAMESPACE_NAME::_span
{
    template<class>inline constexpr bool _is_span_v = false;
    template<class T, size_t N>inline constexpr bool _is_span_v<span<T, N>> = true;

    template<class>inline constexpr bool _is_std_array = false;
    template<class T, size_t N>inline constexpr bool _is_std_array<std::array<T, N>> = true;

    template<class To, class From>
    inline constexpr bool _is_array_convertible_v = std::is_convertible_v<From(*)[], To(*)[]>;

    template<class T,size_t Extent,class U,size_t ArrayExtent>
    inline constexpr bool _is_compatible_array_v
        = (Extent == dynamic_extent||ArrayExtent ==Extent)
            &&_is_array_convertible_v<T,U>;

    template<class Ref,class Type>
    static constexpr bool _is_compatible_ref_v = _is_array_convertible_v<Type, std::remove_reference_t<Ref>>;

    template<class T,size_t Extent>
    struct _span_storage
    {
        template<class,size_t>friend class span;

        constexpr _span_storage(size_t =0) noexcept{ }
        static constexpr size_t _extent() noexcept{ return Extent; }

        template<class It,
            class = std::enable_if_t<contiguous_iterator<It>&&_is_compatible_ref_v<iter_reference_t<It>,T>>>
        constexpr explicit _span_storage(It it,size_t)noexcept
            :_pointer{NAMESPACE_NAME::to_address(it)}
        {}

        template<class It, class End,class = std::enable_if_t<contiguous_iterator<It>&&sized_sentinel_for<End,It>>>
        constexpr explicit  _span_storage(It first, End last)
        noexcept(noexcept(last - first))
            : _pointer(NAMESPACE_NAME::to_address(first))
        {}

        template<size_t ArrayExtent,
            class = std::enable_if_t<(ArrayExtent==Extent)>>
        constexpr _span_storage(typename type_identity<T>::type(&array)[ArrayExtent])noexcept
            :_span_storage(static_cast<T*>(array),ArrayExtent)
        {}

        template<class U,size_t ArrayExtent,
            class = std::enable_if_t<_is_compatible_array_v<T,Extent,U,ArrayExtent>>>
        constexpr _span_storage(std::array<U,ArrayExtent>&array)noexcept
            :_span_storage(static_cast<T*>(array.data()),ArrayExtent)
        {}

        template<class U,size_t ArrayExtent,
            class = std::enable_if_t<_is_compatible_array_v<T,Extent,const U,ArrayExtent>>>
        constexpr _span_storage(const std::array<U,ArrayExtent>&array)noexcept
            :_span_storage(static_cast<T*>(array.data()),ArrayExtent)
        {}

        template<class Range,
                class = std::enable_if_t<!_is_std_array<remove_cvref_t<Range>>
                        &&!_is_span_v<remove_cvref_t<Range>>
                        &&!std::is_array_v<remove_cvref_t<Range>>
                        &&ranges::contiguous_range<remove_cvref_t<Range>>
                        &&ranges::sized_range<Range>
                        && (ranges::borrowed_range<Range> || std::is_const_v<T>)
                        && _is_compatible_ref_v<ranges::range_reference_t<Range>,T>
                        >>
        constexpr explicit _span_storage(Range&&range)noexcept
                : _span_storage(ranges::data(range), ranges::size(range))
        {}

        template<typename Type, size_t OtherExtent,
                class = std::enable_if_t<(OtherExtent == dynamic_extent||Extent == OtherExtent)
        &&_is_array_convertible_v<T, Type>>>
        constexpr
        _span_storage(const span<Type, OtherExtent>& s) noexcept
                : _pointer(s.data())
        {}

        constexpr _span_storage(const _span_storage&) =default;
        ~_span_storage() noexcept = default;

        constexpr _span_storage&operator=(const _span_storage&) noexcept = default;

        T*_pointer =nullptr;
    };

    template<class T>
    struct _span_storage<T,dynamic_extent>
    {
        template<class,size_t>friend class span;
        constexpr _span_storage(size_t extent =0) noexcept
            : _extent_value(extent),_pointer{nullptr}
        { }

        template<class It,class = std::enable_if_t<contiguous_iterator<It>>>
        constexpr _span_storage(It it,size_t n)noexcept
            :_pointer{to_address(it)},_extent_value{n}
        {}

        template<class It, class End,class = std::enable_if_t<contiguous_iterator<It>&&sized_sentinel_for<End,It>>>
        constexpr _span_storage(It first, End last)
        noexcept(noexcept(last - first))
            : _pointer(to_address(first)),_extent_value(static_cast<size_t>(last -first))
        {}

        template<size_t ArrayExtent>
        constexpr _span_storage(typename type_identity<T>::type(&array)[ArrayExtent])noexcept
            :_span_storage(static_cast<T*>(array),ArrayExtent)
        {}

        template<class U,size_t ArrayExtent,
                class = std::enable_if_t<_is_compatible_array_v<T,dynamic_extent,U,ArrayExtent>>>
        constexpr _span_storage(std::array<U,ArrayExtent>&array)noexcept
                :_span_storage(static_cast<T*>(array.data()),ArrayExtent)
        {}

        template<class U,size_t ArrayExtent,
                class = std::enable_if_t<_is_compatible_array_v<T,dynamic_extent,const U,ArrayExtent>>>
        constexpr _span_storage(const std::array<U,ArrayExtent>&array)noexcept
                :_span_storage(static_cast<T*>(array.data()),ArrayExtent)
        {}

        template<class Range,
                class = std::enable_if_t<!_is_std_array<remove_cvref_t<Range>>
                                         &&!_is_span_v<remove_cvref_t<Range>>
                                         &&!std::is_array_v<remove_cvref_t<Range>>
                                         &&ranges::contiguous_range<remove_cvref_t<Range>>
                                         &&ranges::sized_range<Range>
                                         && (ranges::borrowed_range<Range> || std::is_const_v<T>)
                                         && _is_compatible_ref_v<ranges::range_reference_t<Range>,T>
                >>
        constexpr explicit _span_storage(Range&&range)noexcept
                : _span_storage(ranges::data(range), ranges::size(range))
        {}

        constexpr size_t _extent() const noexcept{ return this->_extent_value; }

        template<class Type, size_t Extent,
                class = std::enable_if_t<_is_array_convertible_v<T, Type>>>
        constexpr
        _span_storage(const span<Type, Extent>& s) noexcept
                : _extent_value(s.size()), _pointer(s.data())
        {}

        constexpr _span_storage(const _span_storage&) =default;
        ~_span_storage() noexcept = default;

        constexpr _span_storage&operator=(const _span_storage&) noexcept = default;
        size_t _extent_value;
        T*_pointer;
    };
}

namespace NAMESPACE_NAME
{
    template<class Type, size_t Extent = dynamic_extent>
    class span
        :private  _span::_span_storage<Type,Extent>
    {
    private:
        using Base = _span::_span_storage<Type,Extent>;
    public:
        using element_type           = Type;
        using value_type             = std::remove_cv_t<Type>;
        using size_type              = size_t;
        using difference_type        = ptrdiff_t;
        using pointer                = Type*;
        using const_pointer          = const Type*;
        using reference              = element_type&;
        using const_reference        = const element_type&;
        using iterator = pointer;
        using reverse_iterator       = std::reverse_iterator<iterator>;
        using const_reverse_iterator = const_iterator<reverse_iterator>;
        using const_iterator         = const_iterator<iterator>;
        static constexpr size_t extent = Extent;

        using  Base::Base;

        [[nodiscard]]constexpr size_type size() const noexcept
        { return Base::_extent(); }

        [[nodiscard]]constexpr size_type size_bytes() const noexcept
        { return this->size() * sizeof(element_type); }

        [[nodiscard]]constexpr bool empty() const noexcept
        { return size() == 0; }

        [[nodiscard]]constexpr reference front() const noexcept
        {return *this->_pointer;}

        [[nodiscard]]constexpr reference back() const noexcept
        {return *(this->_pointer + (size() - 1));}

        [[nodiscard]]constexpr reference operator[](size_type idx) const noexcept
        {return *(this->_pointer + idx);}

        constexpr reference at(size_type pos) const
        {
            if (pos >= size())
                throw std::out_of_range("preview::span: out of range");
            return *(this->_pointer + pos);
        }

        [[nodiscard]] constexpr pointer data() const noexcept
        { return this->_pointer; }

        // iterator support

        [[nodiscard]]constexpr iterator begin() const noexcept
        { return iterator(this->_pointer); }

        [[nodiscard]]constexpr iterator end() const noexcept
        { return iterator(this->_pointer+ this->size()); }

        [[nodiscard]]constexpr reverse_iterator rbegin() const noexcept
        { return reverse_iterator(this->end()); }

        [[nodiscard]]constexpr reverse_iterator rend() const noexcept
        { return reverse_iterator(this->begin()); }

        [[nodiscard]]constexpr const_iterator cbegin() const noexcept
      { return begin(); }

      [[nodiscard]]constexpr const_iterator cend() const noexcept
      { return end(); }

      [[nodiscard]]constexpr const_reverse_iterator crbegin() const noexcept
      { return rbegin(); }

      [[nodiscard]]
      constexpr const_reverse_iterator crend() const noexcept
      { return rend(); }

        template<std::size_t Count, class=std::enable_if_t<(Count <= Extent)>>
        constexpr span<element_type, Count> first() const
        {return span<element_type, Count>(data(), Count);}

        constexpr span<element_type, dynamic_extent> first(size_type count) const
        {return span<element_type, dynamic_extent>(data(), count);}

        template<std::size_t Count, std::enable_if_t<(Count <= Extent), int> = 0>
        constexpr span<element_type, Count> last() const
        {return span<element_type, Count>(data() + (size() - Count), Count);}

        constexpr span<element_type, dynamic_extent> last(size_type count) const
        {return span<element_type, dynamic_extent>(data() + (size() - count), count);}

        template<size_t Offset, size_t Count = dynamic_extent,
                class = std::enable_if_t<Offset <= Extent && (Count == dynamic_extent || Count <= Extent - Offset)>>
        [[nodiscard]]
        constexpr auto
        subspan() const noexcept
        {
            using _return_type = span<element_type, (Count != dynamic_extent ? Count :
                                                        Extent != dynamic_extent ? Extent - Offset :
                                                        dynamic_extent)>;
            return _return_type(data() + Offset, Count != dynamic_extent ? Count : size() - Offset);
        }

        [[nodiscard]]
        constexpr span<element_type, dynamic_extent>
        subspan(size_type offset, size_type count = dynamic_extent) const noexcept
        {
            return span<element_type, dynamic_extent>(data() + offset,
                (count == dynamic_extent ? size() - offset : count));
        }
    };

    template<class Iter, class End, class =std::enable_if_t<contiguous_iterator<Iter>>>
    span(Iter, End) -> span<std::remove_reference_t<iter_reference_t<Iter>>>;

    template<class T, std::size_t N>
    span(T (&)[N]) -> span<T, N>;

    template<class T, std::size_t N>
    span(std::array<T, N>&) -> span<T, N>;

    template<class T, std::size_t N>
    span(const std::array<T, N>&) -> span<const T, N>;

    template<class Range, class = std::enable_if_t<ranges::contiguous_range<Range>>>
    span(Range&&) -> span<std::remove_reference_t<ranges::range_reference_t<Range>>>;

    template<class Type, size_t Extent>
    [[nodiscard]]
    inline span<const std::byte, Extent == dynamic_extent
                        ? dynamic_extent : Extent * sizeof(Type)>
    as_bytes(span<Type, Extent> sp) noexcept
    {
        auto data = reinterpret_cast<const std::byte*>(sp.data());
        auto size = sp.size_bytes();
        constexpr auto extent = Extent == dynamic_extent? dynamic_extent : Extent * sizeof(Type);
        return span<const std::byte, extent>{data, size};
    }

    template<class Type, size_t Extent,class = std::enable_if_t<!std::is_const_v<Type>>>
    inline span<std::byte, Extent == dynamic_extent? dynamic_extent : Extent * sizeof(Type)>
    as_writable_bytes [[nodiscard]] (span<Type, Extent> sp) noexcept
    {
        auto data = reinterpret_cast<std::byte*>(sp.data());
        auto size = sp.size_bytes();
        constexpr auto extent = Extent == dynamic_extent? dynamic_extent : Extent * sizeof(Type);
        return span<std::byte, extent>{data, size};
    }

    namespace ranges
    {
        // Opt-in to borrowed_range concept
        template<typename ElementType, size_t Extent>
        inline constexpr bool
                enable_borrowed_range<span<ElementType, Extent>> = true;

        // Opt-in to view concept
        template<typename ElementType, size_t Extent>
        inline constexpr bool
                enable_view<span<ElementType, Extent>> = true;
    }
}


#endif //SCALER_CPP_SPAN_H