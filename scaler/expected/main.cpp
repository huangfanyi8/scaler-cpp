#include "smf_control.h"
#include <iostream>
#include "vector"
#include <string>
#include <iomanip>
#include <string_view>
#include <cmath>

using Ex = DongDong::expected<std::string, int>;

void show(const Ex& ex1, const Ex& ex2)
{
    for (int i{}; i < 2; ++i)
    {
        std::cout << (i ? "ex2" : "ex1");
        if (const Ex& ex = (i ? ex2 : ex1); ex.has_value())
            std::cout << ".has_value() = " << *ex << '\n';
        else
            std::cout << ".error() = " << ex.error() << '\n';
    }
}
enum class parse_error
{
    invalid_input,
    overflow
};

auto parse_number(std::string_view& str) -> DongDong::expected<double, parse_error>
{
    const char* begin = str.data();
    char* end;
    double d = std::strtod(begin, &end);

    if (begin == end)
        return DongDong::unexpected(parse_error::invalid_input);
    else if (std::isinf(d))
        return DongDong::unexpected(parse_error::overflow);

    str.remove_prefix(end - begin);
    return d;
}
// 定义一个错误类型
enum class ErrorCode {
    InvalidInput,
    DivisionByZero,
    NegativeNumber
};

// 辅助函数将错误码转换为字符串
std::string error_to_string(ErrorCode ec) {
    switch(ec) {
        case ErrorCode::InvalidInput: return "Invalid input";
        case ErrorCode::DivisionByZero: return "Division by zero";
        case ErrorCode::NegativeNumber: return "Negative number";
        default: return "Unknown error";
    }
}

// 一个可能失败的操作 - 除法
DongDong::expected<double, ErrorCode> safe_divide(double a, double b) {
    if (b == 0.0) {
        return DongDong::unexpected(ErrorCode::DivisionByZero);
    }
    return a / b;
}

// 另一个可能失败的操作 - 平方根
DongDong::expected<double, ErrorCode> safe_sqrt(double x) {
    if (x < 0) {
        return DongDong::unexpected(ErrorCode::NegativeNumber);
    }
    return std::sqrt(x);
}
int main()
{
    Ex ex1("\N{CAT FACE}");
    Ex ex2{"\N{GREEN HEART}"};
    show(ex1, ex2);
    ex1.swap(ex2);
    std::cout << "ex1.swap(ex2);\n";
    show(ex1, ex2);
    std::cout << '\n';

    ex2 = DongDong::unexpected(13);
    show(ex1, ex2);
    std::cout << "ex1.swap(ex2);\n";
    ex1.swap(ex2);
    show(ex1, ex2);
    std::cout << '\n';

    ex2 = DongDong::unexpected(19937);
    show(ex1, ex2);
    std::cout << "ex1.swap(ex2);\n";
    ex1.swap(ex2);
    show(ex1, ex2);
    auto process = [](std::string_view str)
    {
        std::cout << "str: " << std::quoted(str) << ", ";
        if (const auto num = parse_number(str); num.has_value())
            std::cout << "value: " << *num << '\n';
            // If num did not have a value, dereferencing num
            // would cause an undefined behavior, and
            // num.value() would throw std::bad_expected_access.
            // num.value_or(123) uses specified default value 123.
        else if (num.error() == parse_error::invalid_input)
            std::cout << "error: invalid input\n";
        else if (num.error() == parse_error::overflow)
            std::cout << "error: overflow\n";
        else
            std::cout << "unexpected!\n"; // or invoke std::unreachable();
    };

    for (auto src : {"42", "42abc", "meow", "inf"})
        process(src);

    using namespace DongDong;
    expected<int, int> o1 = 42;
    expected<int, int> o2{unexpect, 0};
    const expected<int, int> o3 = 42;

    (o1 == o1);
    (o1 != o2);
    (o1 == o3);
    (o3 == o3);

    expected<void, int> o6;

    (o6 == o6);

    {
        expected<int, ErrorCode> ex1 = 10;
        auto ex1_transformed = ex1.transform([](int x) { return x * 2; });
        std::cout << "Transform (success): " << *ex1_transformed << "\n";

        expected<int, ErrorCode> ex2 = unexpected(ErrorCode::InvalidInput);
        auto ex2_transformed = ex2.transform([](int x) { return x * 2; });
        std::cout << "Transform (error): " << error_to_string(ex2_transformed.error()) << "\n";

        // 测试 and_then - 链式操作
        auto result = safe_divide(10.0, 2.0)
                .and_then([](double x) { return safe_sqrt(x); })
                .transform([](double x) { return x + 1; });

        if (result) {
            std::cout << "Result: " << *result << "\n";
        } else {
            std::cout << "Error: " << error_to_string(result.error()) << "\n";
        }

        // 测试错误情况
        auto error_result = safe_divide(10.0, 0.0)
                .and_then([](double x) { return safe_sqrt(x); });

        if (error_result) {
            std::cout << "Result: " << *error_result << "\n";
        } else {
            std::cout << "Error: " << error_to_string(error_result.error()) << "\n";
        }

        // 测试 or_else - 错误处理
        auto recovered = safe_divide(10.0, 0.0)
                .or_else([](auto) { return expected<double, ErrorCode>(0.0); });

        std::cout << "Recovered value: " << *recovered << "\n";
    }
}