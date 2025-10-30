#include <algorithm>
#include <iostream>
#include <vector>
#include "../../scaler/ranges/range_algorithm.hpp"
#define preview scaler

using R =std::vector<int>&;

static_assert(preview::ranges::range<R>);
static_assert(!preview::is_bounded_array_v<std::vector<int>>);
int main() {
    std::vector<int> vec{3, 1, 4};
    if (preview::ranges::find(vec, 5) != preview::ranges::end(vec))
        std::cout << "found a 5 in vector vec!\n";
    
    int arr[]{5, 10, 15};
    if (preview::ranges::find(arr, 5) != preview::ranges::end(arr));
}
      