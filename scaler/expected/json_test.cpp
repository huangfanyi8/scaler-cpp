#include <variant>
#include <vector>
#include <map>
#include <string>
#include <sstream>
#include <stdexcept>
#include <cctype>
#include <optional>
#include <iostream>
#include <iomanip>
#include <cmath>
#include <limits>

class Json {
public:
    // 支持的 JSON 类型
    using Null = std::nullptr_t;
    using Bool = bool;
    using Number = double;
    using String = std::string;
    using Array = std::vector<Json>;
    using Object = std::map<std::string, Json>;
    using Value = std::variant<Null, Bool, Number, String, Array, Object>;

    // 构造方法 - 添加所有整数类型重载
    Json() : value(nullptr) {}
    Json(Null) : value(nullptr) {}
    Json(Bool b) : value(b) {}
    Json(Number n) : value(n) {}
    Json(int n) : value(static_cast<double>(n)) {}
    Json(unsigned int n) : value(static_cast<double>(n)) {}
    Json(long n) : value(static_cast<double>(n)) {}
    Json(unsigned long n) : value(static_cast<double>(n)) {}
    Json(long long n) : value(static_cast<double>(n)) {}             // 添加 long long 重载
    Json(unsigned long long n) : value(static_cast<double>(n)) {}    // 添加 unsigned long long 重载
    Json(const char* s) : value(std::string(s)) {}
    Json(String s) : value(std::move(s)) {}
    Json(Array a) : value(std::move(a)) {}
    Json(Object o) : value(std::move(o)) {}

    // 类型检查
    bool is_null()   const { return std::holds_alternative<Null>(value); }
    bool is_bool()   const { return std::holds_alternative<Bool>(value); }
    bool is_number() const { return std::holds_alternative<Number>(value); }
    bool is_string() const { return std::holds_alternative<String>(value); }
    bool is_array()  const { return std::holds_alternative<Array>(value); }
    bool is_object() const { return std::holds_alternative<Object>(value); }

    // 值获取（带类型检查）
    Bool to_bool() const {
        if (auto p = std::get_if<Bool>(&value)) return *p;
        throw std::runtime_error("Not a boolean");
    }

    Number to_number() const {
        if (auto p = std::get_if<Number>(&value)) return *p;
        throw std::runtime_error("Not a number");
    }

    const String& to_string() const {
        if (auto p = std::get_if<String>(&value)) return *p;
        throw std::runtime_error("Not a string");
    }

    const Array& to_array() const {
        if (auto p = std::get_if<Array>(&value)) return *p;
        throw std::runtime_error("Not an array");
    }

    const Object& to_object() const {
        if (auto p = std::get_if<Object>(&value)) return *p;
        throw std::runtime_error("Not an object");
    }

    // 序列化 JSON
    std::string dump(int indent = 0, int current_indent = 0) const {
        std::ostringstream oss;
        dump_value(oss, indent, current_indent);
        return oss.str();
    }

    // JSON 解析
    static Json parse(std::string_view json_str) {
        size_t pos = 0;
        return parse_value(json_str, pos);
    }

private:
    Value value;

    // 序列化实现
    void dump_value(std::ostringstream& oss, int indent, int current_indent) const {
        if (is_null()) {
            oss << "null";
        } else if (is_bool()) {
            oss << (to_bool() ? "true" : "false");
        } else if (is_number()) {
            double num = to_number();
            // 检查是否为整数且在可精确表示的范围内
            if (std::floor(num) == num &&
                num <= static_cast<double>(std::numeric_limits<long long>::max()) &&
                num >= static_cast<double>(std::numeric_limits<long long>::min())) {
                // 转换为整数输出
                long long int_val = static_cast<long long>(num);
                oss << int_val;
            } else {
                // 浮点数输出
                oss << num;
            }
        } else if (is_string()) {
            dump_string(oss, to_string());
        } else if (is_array()) {
            dump_array(oss, to_array(), indent, current_indent);
        } else if (is_object()) {
            dump_object(oss, to_object(), indent, current_indent);
        }
    }

    void dump_string(std::ostringstream& oss, const std::string& str) const {
        oss << '"';
        for (char c : str) {
            switch (c) {
                case '"':  oss << "\\\""; break;
                case '\\': oss << "\\\\"; break;
                case '\b': oss << "\\b";  break;
                case '\f': oss << "\\f";  break;
                case '\n': oss << "\\n";  break;
                case '\r': oss << "\\r";  break;
                case '\t': oss << "\\t";  break;
                default:
                    if (c >= 0 && c < 0x20) {
                        oss << "\\u" << std::hex << std::setw(4) << std::setfill('0') << static_cast<int>(c);
                    } else {
                        oss << c;
                    }
            }
        }
        oss << '"';
    }

    void dump_array(std::ostringstream& oss, const Array& arr, int indent, int current_indent) const {
        oss << '[';
        if (indent > 0) oss << '\n';

        for (size_t i = 0; i < arr.size(); ++i) {
            if (indent > 0) oss << std::string(current_indent + indent, ' ');

            arr[i].dump_value(oss, indent, current_indent + indent);

            if (i < arr.size() - 1) oss << ',';
            if (indent > 0) oss << '\n';
        }

        if (indent > 0) oss << std::string(current_indent, ' ');
        oss << ']';
    }

    void dump_object(std::ostringstream& oss, const Object& obj, int indent, int current_indent) const {
        oss << '{';
        if (indent > 0) oss << '\n';

        size_t i = 0;
        for (const auto& [key, val] : obj) {
            if (indent > 0) oss << std::string(current_indent + indent, ' ');

            dump_string(oss, key);
            oss << ':';
            if (indent > 0) oss << ' ';

            val.dump_value(oss, indent, current_indent + indent);

            if (++i < obj.size()) oss << ',';
            if (indent > 0) oss << '\n';
        }

        if (indent > 0) oss << std::string(current_indent, ' ');
        oss << '}';
    }

    // 解析实现
    static void skip_whitespace(std::string_view json_str, size_t& pos) {
        while (pos < json_str.size() && std::isspace(json_str[pos])) {
            ++pos;
        }
    }

    static Json parse_value(std::string_view json_str, size_t& pos) {
        skip_whitespace(json_str, pos);
        if (pos >= json_str.size()) throw std::runtime_error("Unexpected end of input");

        char c = json_str[pos];
        if (c == 'n') return parse_null(json_str, pos);
        if (c == 't' || c == 'f') return parse_bool(json_str, pos);
        if (c == '"') return parse_string(json_str, pos);
        if (c == '[') return parse_array(json_str, pos);
        if (c == '{') return parse_object(json_str, pos);
        if (c == '-' || (c >= '0' && c <= '9')) return parse_number(json_str, pos);

        throw std::runtime_error("Unexpected character: " + std::string(1, c));
    }

    static Json parse_null(std::string_view json_str, size_t& pos) {
        if (json_str.substr(pos, 4) == "null") {
            pos += 4;
            return Json(nullptr);
        }
        throw std::runtime_error("Expected 'null'");
    }

    static Json parse_bool(std::string_view json_str, size_t& pos) {
        if (json_str.substr(pos, 4) == "true") {
            pos += 4;
            return Json(true);
        }
        if (json_str.substr(pos, 5) == "false") {
            pos += 5;
            return Json(false);
        }
        throw std::runtime_error("Expected 'true' or 'false'");
    }

    static Json parse_string(std::string_view json_str, size_t& pos) {
        std::string result;
        ++pos; // 跳过开头的引号

        while (pos < json_str.size()) {
            char c = json_str[pos++];

            if (c == '"') {
                return Json(result);
            }

            if (c == '\\') {
                if (pos >= json_str.size()) throw std::runtime_error("Unexpected end of string");
                c = json_str[pos++];
                switch (c) {
                    case '"':  result += '"';  break;
                    case '\\': result += '\\'; break;
                    case '/':  result += '/';  break;
                    case 'b':  result += '\b'; break;
                    case 'f':  result += '\f'; break;
                    case 'n':  result += '\n'; break;
                    case 'r':  result += '\r'; break;
                    case 't':  result += '\t'; break;
                    case 'u':  // Unicode 转义（简化处理）
                        result += "\\u";
                        if (pos + 4 > json_str.size()) throw std::runtime_error("Invalid unicode escape");
                        result += json_str.substr(pos, 4);
                        pos += 4;
                        break;
                    default:
                        throw std::runtime_error("Invalid escape sequence");
                }
            } else {
                result += c;
            }
        }

        throw std::runtime_error("Unterminated string");
    }

    static Json parse_number(std::string_view json_str, size_t& pos) {
        size_t start = pos;

        // 处理可选负号
        if (json_str[pos] == '-') ++pos;

        // 整数部分
        if (pos < json_str.size() && json_str[pos] == '0') {
            ++pos;
        } else {
            while (pos < json_str.size() && std::isdigit(json_str[pos])) ++pos;
        }

        // 小数部分
        if (pos < json_str.size() && json_str[pos] == '.') {
            ++pos;
            while (pos < json_str.size() && std::isdigit(json_str[pos])) ++pos;
        }

        // 指数部分
        if (pos < json_str.size() && (json_str[pos] == 'e' || json_str[pos] == 'E')) {
            ++pos;
            if (pos < json_str.size() && (json_str[pos] == '+' || json_str[pos] == '-')) ++pos;
            while (pos < json_str.size() && std::isdigit(json_str[pos])) ++pos;
        }

        std::string num_str = std::string(json_str.substr(start, pos - start));
        try {
            return Json(std::stod(num_str));
        } catch (...) {
            throw std::runtime_error("Invalid number: " + num_str);
        }
    }

    static Json parse_array(std::string_view json_str, size_t& pos) {
        Array result;
        ++pos; // 跳过 '['

        skip_whitespace(json_str, pos);
        if (pos < json_str.size() && json_str[pos] == ']') {
            ++pos;
            return Json(result);
        }

        while (pos < json_str.size()) {
            result.push_back(parse_value(json_str, pos));

            skip_whitespace(json_str, pos);
            if (pos < json_str.size() && json_str[pos] == ',') {
                ++pos;
                skip_whitespace(json_str, pos);
            } else if (pos < json_str.size() && json_str[pos] == ']') {
                ++pos;
                return Json(result);
            } else {
                throw std::runtime_error("Expected ',' or ']' in array");
            }
        }

        throw std::runtime_error("Unterminated array");
    }

    static Json parse_object(std::string_view json_str, size_t& pos) {
        Object result;
        ++pos; // 跳过 '{'

        skip_whitespace(json_str, pos);
        if (pos < json_str.size() && json_str[pos] == '}') {
            ++pos;
            return Json(result);
        }

        while (pos < json_str.size()) {
            // 解析键
            skip_whitespace(json_str, pos);
            if (pos >= json_str.size() || json_str[pos] != '"') {
                throw std::runtime_error("Expected string key in object");
            }
            Json key_json = parse_string(json_str, pos);
            std::string key = key_json.to_string();

            // 解析冒号
            skip_whitespace(json_str, pos);
            if (pos >= json_str.size() || json_str[pos] != ':') {
                throw std::runtime_error("Expected ':' after key in object");
            }
            ++pos;

            // 解析值
            Json value = parse_value(json_str, pos);
            result.emplace(std::move(key), std::move(value));

            // 解析逗号或结束符
            skip_whitespace(json_str, pos);
            if (pos < json_str.size() && json_str[pos] == ',') {
                ++pos;
                skip_whitespace(json_str, pos);
            } else if (pos < json_str.size() && json_str[pos] == '}') {
                ++pos;
                return Json(result);
            } else {
                throw std::runtime_error("Expected ',' or '}' in object");
            }
        }

        throw std::runtime_error("Unterminated object");
    }

public:
    // [原有的类型定义和成员函数保持不变]
    // 为了节省空间，这里省略了之前已经展示的代码
    // 只展示新增的验证功能

    // 验证 JSON 字符串格式是否正确（轻量级语法检查）
    static bool is_valid(std::string_view json_str) {
        size_t pos = 0;
        try {
            validate_value(json_str, pos);
            skip_whitespace(json_str, pos);
            return pos == json_str.size(); // 确保整个字符串都被解析
        } catch (const std::exception&) {
            return false;
        }
    }

private:
    // [原有的私有成员函数保持不变]

    // 新增的轻量级验证函数
    static void validate_value(std::string_view json_str, size_t& pos) {
        skip_whitespace(json_str, pos);
        if (pos >= json_str.size()) throw std::runtime_error("Unexpected end of input");

        char c = json_str[pos];
        if (c == 'n') validate_null(json_str, pos);
        else if (c == 't' || c == 'f') validate_bool(json_str, pos);
        else if (c == '"') validate_string(json_str, pos);
        else if (c == '[') validate_array(json_str, pos);
        else if (c == '{') validate_object(json_str, pos);
        else if (c == '-' || (c >= '0' && c <= '9')) validate_number(json_str, pos);
        else throw std::runtime_error("Unexpected character");
    }

    static void validate_null(std::string_view json_str, size_t& pos) {
        if (json_str.substr(pos, 4) != "null") {
            throw std::runtime_error("Expected 'null'");
        }
        pos += 4;
    }

    static void validate_bool(std::string_view json_str, size_t& pos) {
        if (json_str.substr(pos, 4) == "true") {
            pos += 4;
        } else if (json_str.substr(pos, 5) == "false") {
            pos += 5;
        } else {
            throw std::runtime_error("Expected 'true' or 'false'");
        }
    }

    static void validate_string(std::string_view json_str, size_t& pos) {
        ++pos; // 跳过开头的引号
        while (pos < json_str.size()) {
            char c = json_str[pos++];
            if (c == '"') return; // 字符串结束

            if (c == '\\') {
                if (pos >= json_str.size()) throw std::runtime_error("Unexpected end of string");
                c = json_str[pos++];
                switch (c) {
                    case '"': case '\\': case '/': case 'b':
                    case 'f': case 'n': case 'r': case 't':
                        break; // 有效的转义序列
                    case 'u': // Unicode 转义
                        if (pos + 4 > json_str.size()) throw std::runtime_error("Invalid unicode escape");
                        for (int i = 0; i < 4; ++i) {
                            if (!std::isxdigit(json_str[pos + i])) {
                                throw std::runtime_error("Invalid hex digit");
                            }
                        }
                        pos += 4;
                        break;
                    default:
                        throw std::runtime_error("Invalid escape sequence");
                }
            } else if (c < 0x20) { // 控制字符
                throw std::runtime_error("Control character in string");
            }
        }
        throw std::runtime_error("Unterminated string");
    }

    static void validate_number(std::string_view json_str, size_t& pos) {
        size_t start = pos;

        // 处理可选负号
        if (json_str[pos] == '-') ++pos;

        // 整数部分
        if (pos < json_str.size() && json_str[pos] == '0') {
            ++pos;
        } else if (pos < json_str.size() && std::isdigit(json_str[pos])) {
            ++pos;
            while (pos < json_str.size() && std::isdigit(json_str[pos])) ++pos;
        } else {
            throw std::runtime_error("Invalid number");
        }

        // 小数部分
        if (pos < json_str.size() && json_str[pos] == '.') {
            ++pos;
            if (pos >= json_str.size() || !std::isdigit(json_str[pos])) {
                throw std::runtime_error("Invalid fractional part");
            }
            ++pos;
            while (pos < json_str.size() && std::isdigit(json_str[pos])) ++pos;
        }

        // 指数部分
        if (pos < json_str.size() && (json_str[pos] == 'e' || json_str[pos] == 'E')) {
            ++pos;
            if (pos < json_str.size() && (json_str[pos] == '+' || json_str[pos] == '-')) ++pos;
            if (pos >= json_str.size() || !std::isdigit(json_str[pos])) {
                throw std::runtime_error("Invalid exponent");
            }
            ++pos;
            while (pos < json_str.size() && std::isdigit(json_str[pos])) ++pos;
        }

        // 验证数字格式（不实际转换）
        std::string num_str = std::string(json_str.substr(start, pos - start));
        if (num_str.empty() ||
            num_str == "-" ||
            (num_str.find('.') == num_str.size() - 1) ||
            (num_str.find('e') != std::string::npos &&
             (num_str.find('e') == num_str.size() - 1 ||
              (num_str[num_str.find('e') + 1] == '+' || num_str[num_str.find('e') + 1] == '-') &&
              num_str.find('e') == num_str.size() - 2))) {
            throw std::runtime_error("Invalid number format");
        }
    }

    static void validate_array(std::string_view json_str, size_t& pos) {
        ++pos; // 跳过 '['
        skip_whitespace(json_str, pos);

        if (pos < json_str.size() && json_str[pos] == ']') {
            ++pos; // 空数组
            return;
        }

        while (pos < json_str.size()) {
            validate_value(json_str, pos); // 验证数组元素

            skip_whitespace(json_str, pos);
            if (pos < json_str.size() && json_str[pos] == ',') {
                ++pos;
                skip_whitespace(json_str, pos);
            } else if (pos < json_str.size() && json_str[pos] == ']') {
                ++pos;
                return;
            } else {
                throw std::runtime_error("Expected ',' or ']' in array");
            }
        }

        throw std::runtime_error("Unterminated array");
    }

    static void validate_object(std::string_view json_str, size_t& pos) {
        ++pos; // 跳过 '{'
        skip_whitespace(json_str, pos);

        if (pos < json_str.size() && json_str[pos] == '}') {
            ++pos; // 空对象
            return;
        }

        while (pos < json_str.size()) {
            // 验证键
            skip_whitespace(json_str, pos);
            if (pos >= json_str.size() || json_str[pos] != '"') {
                throw std::runtime_error("Expected string key in object");
            }
            validate_string(json_str, pos);

            // 验证冒号
            skip_whitespace(json_str, pos);
            if (pos >= json_str.size() || json_str[pos] != ':') {
                throw std::runtime_error("Expected ':' after key in object");
            }
            ++pos;

            // 验证值
            validate_value(json_str, pos);

            // 验证逗号或结束符
            skip_whitespace(json_str, pos);
            if (pos < json_str.size() && json_str[pos] == ',') {
                ++pos;
                skip_whitespace(json_str, pos);
            } else if (pos < json_str.size() && json_str[pos] == '}') {
                ++pos;
                return;
            } else {
                throw std::runtime_error("Expected ',' or '}' in object");
            }
        }

        throw std::runtime_error("Unterminated object");
    }
};

// 测试用例
int main() {
    // 创建 JSON 对象 - 包含各种整数类型
    Json obj = Json::Object{
            {"name", "Alice"},
            {"age", 30},                             // int
            {"height", 175u},                         // unsigned int
            {"weight", 65l},                          // long
            {"id", 1000000000000ull},                 // unsigned long long
            {"big_positive", 9223372036854775807ll},  // long long 最大值
            {"big_negative", -9223372036854775807ll}, // long long 最小值
            {"scores", Json::Array{95, 88, 92}},
            {"address", Json::Object{
                    {"city", "New York"},
                    {"zip", 10001}                       // int
            }},
            {"is_student", false},
            {"notes", Json::Null(nullptr)}
    };

    // 序列化为字符串
    std::string json_str = obj.dump(2);
    std::cout << "Serialized JSON:\n" << json_str << "\n\n";

    // 从字符串解析
    Json parsed = Json::parse(json_str);

    // 验证解析结果
    std::cout << "Parsed JSON:\n" << parsed.dump(2) << "\n";
    std::cout << "Name: " << parsed.to_object().at("name").to_string() << "\n";
    std::cout << "Age: " << parsed.to_object().at("age").to_number() << "\n";
    std::cout << "Height: " << parsed.to_object().at("height").to_number() << "\n";
    std::cout << "Big Positive: " << parsed.to_object().at("big_positive").to_number() << "\n";
    std::cout << "Big Negative: " << parsed.to_object().at("big_negative").to_number() << "\n";
    std::cout << "Is student: " << std::boolalpha
              << parsed.to_object().at("is_student").to_bool() << "\n";

    return 0;
}