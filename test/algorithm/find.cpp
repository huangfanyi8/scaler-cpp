#include <algorithm>
#include <cassert>
#include <complex>
#include <iostream>
#include <iterator>
#include <string>
#include <vector>
#include "../../scaler/ranges/range_algorithm.hpp"
#define preview scaler

void projector_example()
{
    struct folk_info
    {
        unsigned uid;
        std::string name, position;
    };

    std::vector<folk_info> folks
        {
            {0, "Ana", "dev"},
            {1, "Bob", "devops"},
            {2, "Eve", "ops"}
        };

    const auto who{"Eve"};
    if (auto it = preview::ranges::find(folks, who, &folk_info::name); it != folks.end())
        std::cout << "Profile:\n"<<"UID:"<<it->uid<<'\n'
        <<"Name:"<<it->name<<'\n'<<"Position:"<<it->position<<'\n';

}

int main()
{
    namespace ranges = preview::ranges;

    projector_example();

    const int n1 = 3;
    const int n2 = 5;
    const auto v = {4, 1, 3, 2};

    if (ranges::find(v, n1) != v.end())
        std::cout << "v contains: " << n1 << '\n';
    else
        std::cout << "v does not contain: " << n1 << '\n';

    if (ranges::find(v.begin(), v.end(), n2) != v.end())
        std::cout << "v contains: " << n2 << '\n';
    else
        std::cout << "v does not contain: " << n2 << '\n';

    auto is_even = [](int x) { return x % 2 == 0; };

    if (auto result = ranges::find_if(v.begin(), v.end(), is_even); result != v.end())
        std::cout << "First even element in v: " << *result << '\n';
    else
        std::cout << "No even elements in v\n";

    if (auto result = ranges::find_if_not(v, is_even); result != v.end())
        std::cout << "First odd element in v: " << *result << '\n';
    else
        std::cout << "No odd elements in v\n";

    auto divides_13 = [](int x) { return x % 13 == 0; };

    if (auto result = ranges::find_if(v, divides_13); result != v.end())
        std::cout << "First element divisible by 13 in v: " << *result << '\n';
    else
        std::cout << "No elements in v are divisible by 13\n";

    if (auto result = ranges::find_if_not(v.begin(), v.end(), divides_13);
        result != v.end())
        std::cout << "First element indivisible by 13 in v: " << *result << '\n';
    else
        std::cout << "All elements in v are divisible by 13\n";

    std::vector<std::complex<double>> nums{{4, 2}};
    #ifdef __cpp_lib_algorithm_default_value_type
    // T gets deduced in (2) making list-initialization possible
        const auto it = ranges::find(nums, {4, 2});
    #else
    const auto it = ranges::find(nums, std::complex<double>{4, 2});
    #endif
    assert(it == nums.begin());
}
