#include "../../scaler/concepts//iterator_concepts.hpp"

struct Fn
{
    long operator()(const int&);
    int operator()(int&&);
    short operator()(int, int) const;
    auto operator()(const float) -> int&;
    void operator()(int[8]);
};

static_assert(
    std::is_same_v<scaler::indirect_result_t<Fn, const int*>, long> and
    std::is_same_v<scaler::indirect_result_t<Fn, std::move_iterator<int*>>, int> and
    std::is_same_v<scaler::indirect_result_t<const Fn, int*, int*>, short> and
    std::is_same_v<scaler::indirect_result_t<Fn, float*>, int&> and
    std::is_same_v<scaler::indirect_result_t<Fn, int**>, void>
);

int main() {}
