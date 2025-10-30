

#include <cassert>
#include <vector>
#include "../../scaler/ranges/range_algorithm.hpp"
#define preview scaler

int main()
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
    // *ai = 13; // Error: read-only variable is not assignable
}
