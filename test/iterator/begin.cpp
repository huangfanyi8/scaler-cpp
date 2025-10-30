#include"../../scaler/ranges/range_access.hpp"
#include"../../scaler/ranges/range_algorithm.hpp"
#include <cassert>
#include <vector>

namespace preview = scaler;

int main()
{
    {
        std::vector v{3, 1, 4};
        auto vi = preview::ranges::begin(v);
        auto vci = preview::ranges::cbegin(v);
        assert(*vi == 3 and *vi == *vci);
        ++vi;
        ++vci; // OK: vci is modifiable object
        *vi = 42; // OK: vi points to mutable element
        // *vci = 13; // Error: vci points to immutable element

        int a[]{-5, 10, 15};
        auto ai = preview::ranges::begin(a); // works with C-arrays as well
        assert(*ai == -5);
        *ai = 42; // OK
    }//scope of test begin

    {
        std::vector v{3, 1, 4};
        auto vi = preview::ranges::cbegin(v);
        assert(3 == *vi);
        ++vi; // OK, constant-iterator object is mutable
        assert(1 == *vi);
        // *vi = 13; // Error: constant-iterator points to an immutable element

        int a[]{3, 1, 4};
        auto ai = preview::ranges::cbegin(a); // cbegin works with C-arrays as well
        assert(3 == *ai and *(ai + 1) == 1);
    }//scope of test cbegin

    {
        std::vector vec{3, 1, 4};
        int arr[]{5, 10, 15};

        assert(scaler::ranges::find(vec, 5) == scaler::ranges::cend(vec));
        assert(scaler::ranges::find(arr, 5) != scaler::ranges::cend(arr));
    }
}