
#include <cassert>
#include <iterator>
#include <vector>
#include "../../scaler/iterator/iterator.hpp"
#include "../../scaler/ranges/range_access.hpp"
#define preview scaler


void foo(std::vector<int>::const_iterator) {}
int main()
{
    {
        std::vector v{1, 2, 3};
        std::vector<int>::iterator i = v.begin();
        *i = 4;   // OK, v[0] == 4 now
        i[1] = 4; // OK, the same as *(i + 1) = 4;

        auto ci = preview::make_const_iterator(i);
        assert(*ci == 4);   // OK, can read the underlying object
        assert(ci[0] == 4); // OK, ditto
        // *ci = 13;        // Error: location is read-only
        // ci[0] = 13;      // Error: ditto
        ci.base()[0] = 42;  // OK, underlying iterator is writable
        assert(*ci == 42);  // OK, underlying location v[0] was modified

        static int arr[1];
        static constexpr preview::basic_const_iterator<int*> it = std::end(arr);
        static_assert(arr < it);
    }
    {
        auto v = std::vector<int>();
        {
            // ranges::cbegin below returns vector<int>::const_iterator
            auto i1 = scaler::ranges::cbegin(v);
            foo(i1); // okay
        }

        auto t = v | std::views::take_while([](int const x) { return x < 100; });
        {
            // ranges::cbegin below returns basic_const_iterator<vector<int>::iterator>
            auto i2 = scaler::ranges::cbegin(t);
            foo(i2); // error until P2836R1
        }
    }
}