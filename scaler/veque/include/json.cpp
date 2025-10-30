#include "json.hpp"
#include <cmath>
#include <cctype>
#include <iostream>
namespace minijson {

// Value 析构函数
    Value::~Value() {
        switch (type_) {
            case ValueType::STRING:
                PoolFree(data_.string.str);
                break;
            case ValueType::ARRAY:
                for (size_t i = 0; i < data_.array.size; ++i) {
                    data_.array.elements[i].~Value();
                }
                PoolFree(data_.array.elements);
                break;
            case ValueType::OBJECT:
                for (size_t i = 0; i < data_.object.size; ++i) {
                    PoolFree(data_.object.members[i].key);
                    data_.object.members[i].value.~Value();
                }
                PoolFree(data_.object.members);
                break;
            default:
                break;
        }
    }

// 跳过空白字符
    void Value::SkipWhitespace(const char*& json) {
        while (*json == ' ' || *json == '\t' || *json == '\n' || *json == '\r') {
            json++;
        }
    }

// 解析JSON值
    ParseError Value::Parse(const char* json) {
        ParseError err = ParseValue(json);
        if (err == ParseError::OK) {
            SkipWhitespace(json);
            if (*json != '\0') {
                return ParseError::ROOT_NOT_SINGULAR;
            }
        }
        return err;
    }

    ParseError Value::ParseValue(const char*& json) {
        SkipWhitespace(json);

        switch (*json) {
            case 'n': return ParseLiteral(json, "null", ValueType::NULL_TYPE);
            case 't': return ParseLiteral(json, "true", ValueType::BOOL);
            case 'f': return ParseLiteral(json, "false", ValueType::BOOL);
            case '"': return ParseString(json);
            case '[': return ParseArray(json);
            case '{': return ParseObject(json);
            case '\0': return ParseError::EXPECT_VALUE;
            default: return ParseNumber(json);
        }
    }

// 解析字面量
    ParseError Value::ParseLiteral(const char*& json, const char* literal, ValueType type) {
        size_t len = strlen(literal);
        for (size_t i = 0; i < len; i++) {
            if (json[i] != literal[i]) {
                return ParseError::INVALID_VALUE;
            }
        }

        json += len;
        type_ = type;
        if (type == ValueType::BOOL) {
            data_.boolean = (literal[0] == 't');
        }
        return ParseError::OK;
    }

// 解析数字（简化版）
    ParseError Value::ParseNumber(const char*& json) {
        const char* p = json;

        // 解析整数部分
        if (*p == '-') p++;
        if (*p == '0') {
            p++;
        } else {
            if (!IsDigit(*p)) return ParseError::INVALID_VALUE;
            while (IsDigit(*p)) p++;
        }

        // 解析小数部分
        if (*p == '.') {
            p++;
            if (!IsDigit(*p)) return ParseError::INVALID_VALUE;
            while (IsDigit(*p)) p++;
        }

        // 解析指数部分
        if (*p == 'e' || *p == 'E') {
            p++;
            if (*p == '+' || *p == '-') p++;
            if (!IsDigit(*p)) return ParseError::INVALID_VALUE;
            while (IsDigit(*p)) p++;
        }

        // 转换为double
        char* end;
        double num = strtod(json, &end);
        if (end != p) return ParseError::INVALID_VALUE;

        json = p;
        type_ = ValueType::NUMBER;
        data_.number = num;
        return ParseError::OK;
    }

// 解析字符串（简化版，不支持unicode转义）
    ParseError Value::ParseString(const char*& json) {
        const char* p = json + 1;  // 跳过开始的引号
        const char* start = p;

        while (*p != '"') {
            if (*p == '\0') return ParseError::MISS_QUOTATION_MARK;
            if (*p == '\\') {
                p++;  // 跳过转义字符
                if (*p == '\0') return ParseError::INVALID_STRING_ESCAPE;
            }
            p++;
        }

        size_t length = p - start;
        SetString(start, length);

        json = p + 1;  // 跳过结束的引号
        return ParseError::OK;
    }

// 设置字符串值
    void Value::SetString(const char* str, size_t length) {
        type_ = ValueType::STRING;
        data_.string.str = static_cast<char*>(PoolAllocate(length + 1));
        memcpy(data_.string.str, str, length);
        data_.string.str[length] = '\0';
        data_.string.length = length;
    }

// 解析数组
    ParseError Value::ParseArray(const char*& json) {
        json++;  // 跳过 '['
        SkipWhitespace(json);

        type_ = ValueType::ARRAY;
        data_.array.size = 0;
        data_.array.capacity = 0;
        data_.array.elements = nullptr;

        if (*json == ']') {
            json++;  // 空数组
            return ParseError::OK;
        }

        while (true) {
            SkipWhitespace(json);

            // 扩容检查
            if (data_.array.size >= data_.array.capacity) {
                size_t new_capacity = data_.array.capacity == 0 ? 4 : data_.array.capacity * 2;
                Value* new_elements = static_cast<Value*>(PoolAllocate(new_capacity * sizeof(Value)));

                if (data_.array.elements) {
                    memcpy(new_elements, data_.array.elements, data_.array.size * sizeof(Value));
                    PoolFree(data_.array.elements);
                }

                data_.array.elements = new_elements;
                data_.array.capacity = new_capacity;
            }

            // 解析数组元素
            ParseError err = data_.array.elements[data_.array.size].ParseValue(json);
            if (err != ParseError::OK) return err;
            data_.array.size++;

            SkipWhitespace(json);

            if (*json == ']') {
                json++;
                return ParseError::OK;
            }

            if (*json != ',') {
                return ParseError::MISS_COMMA_OR_SQUARE_BRACKET;
            }
            json++;  // 跳过 ','
        }
    }

// 解析对象（简化版）
    ParseError Value::ParseObject(const char*& json) {
        json++;  // 跳过 '{'
        SkipWhitespace(json);

        type_ = ValueType::OBJECT;
        data_.object.size = 0;
        data_.object.capacity = 0;
        data_.object.members = nullptr;

        if (*json == '}') {
            json++;  // 空对象
            return ParseError::OK;
        }

        while (true) {
            SkipWhitespace(json);

            // 解析key
            if (*json != '"') return ParseError::MISS_KEY;
            const char* key_start = json + 1;
            const char* p = key_start;

            while (*p != '"') {
                if (*p == '\0') return ParseError::MISS_QUOTATION_MARK;
                p++;
            }

            size_t key_len = p - key_start;
            json = p + 1;  // 跳过结束引号

            SkipWhitespace(json);
            if (*json != ':') return ParseError::MISS_COLON;
            json++;  // 跳过 ':'

            // 扩容检查
            if (data_.object.size >= data_.object.capacity) {
                size_t new_capacity = data_.object.capacity == 0 ? 4 : data_.object.capacity * 2;
                auto* new_members = static_cast<decltype(data_.object.members)>(
                        PoolAllocate(new_capacity * sizeof(decltype(data_.object.members[0]))));

                if (data_.object.members) {
                    memcpy(new_members, data_.object.members, data_.object.size * sizeof(decltype(data_.object.members[0])));
                    PoolFree(data_.object.members);
                }

                data_.object.members = new_members;
                data_.object.capacity = new_capacity;
            }

            // 保存key
            auto& member = data_.object.members[data_.object.size];
            member.key = static_cast<char*>(PoolAllocate(key_len + 1));
            memcpy(member.key, key_start, key_len);
            member.key[key_len] = '\0';
            member.key_len = key_len;

            // 解析value
            ParseError err = member.value.ParseValue(json);
            if (err != ParseError::OK) return err;
            data_.object.size++;

            SkipWhitespace(json);

            if (*json == '}') {
                json++;
                return ParseError::OK;
            }

            if (*json != ',') {
                return ParseError::MISS_COMMA_OR_CURLY_BRACKET;
            }
            json++;  // 跳过 ','
        }
    }

// 获取值的实现
    bool Value::GetBool() const {
        return data_.boolean;
    }

    double Value::GetNumber() const {
        return data_.number;
    }

    const char* Value::GetString() const {
        return data_.string.str;
    }

    size_t Value::GetStringLength() const {
        return data_.string.length;
    }

    size_t Value::GetArraySize() const {
        return data_.array.size;
    }

    const Value& Value::GetArrayElement(size_t index) const {
        return data_.array.elements[index];
    }

    size_t Value::GetObjectSize() const {
        return data_.object.size;
    }

    const char* Value::GetObjectKey(size_t index) const {
        return data_.object.members[index].key;
    }

    size_t Value::GetObjectKeyLength(size_t index) const {
        return data_.object.members[index].key_len;
    }

    const Value& Value::GetObjectValue(size_t index) const {
        return data_.object.members[index].value;
    }



    int test() {
        Parser parser;

        // 解析示例JSON
        const char *json = R"({
        "name": "John",
        "age": 30,
        "scores": [95, 88, 92],
        "is_student": false
    })";

        try {
            Value value = parser.Parse(json);

            if (value.IsObject()) {
                for (size_t i = 0; i < value.GetObjectSize(); i++) {
                    std::cout << value.GetObjectKey(i) << ": ";

                    const Value &val = value.GetObjectValue(i);
                    if (val.IsString()) {
                        std::cout << val.GetString();
                    } else if (val.IsNumber()) {
                        std::cout << val.GetNumber();
                    } else if (val.IsBool()) {
                        std::cout << (val.GetBool() ? "true" : "false");
                    } else if (val.IsArray()) {
                        std::cout << "[";
                        for (size_t j = 0; j < val.GetArraySize(); j++) {
                            if (j > 0) std::cout << ", ";
                            std::cout << val.GetArrayElement(j).GetNumber();
                        }
                        std::cout << "]";
                    }
                    std::cout << std::endl;
                }
            }

        } catch (const std::exception &e) {
            std::cerr << "Parse error: " << e.what() << std::endl;
        }

        return 0;
    }
} // namespace minijson
