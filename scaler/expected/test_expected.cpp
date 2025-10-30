#include <iostream>
#include <expected>
#include <string>
#include <cmath>

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
std::expected<double, ErrorCode> safe_divide(double a, double b) {
    if (b == 0.0) {
        return std::unexpected(ErrorCode::DivisionByZero);
    }
    return a / b;
}

// 另一个可能失败的操作 - 平方根
std::expected<double, ErrorCode> safe_sqrt(double x) {
    if (x < 0) {
        return std::unexpected(ErrorCode::NegativeNumber);
    }
    return std::sqrt(x);
}

int main() {
    // 测试 transform - 当有值时应用函数
    std::expected<int, ErrorCode> ex1 = 10;
    auto ex1_transformed = ex1.transform([](int x) { return x * 2; });
    std::cout << "Transform (success): " << *ex1_transformed << "\n";

    std::expected<int, ErrorCode> ex2 = std::unexpected(ErrorCode::InvalidInput);
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
            .or_else([](auto) { return std::expected<double, ErrorCode>(0.0); });

    std::cout << "Recovered value: " << *recovered << "\n";

    return 0;
}