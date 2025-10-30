#pragma once

#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <vector>
#include <string>
#include <memory>
#include <stdexcept>

namespace minijson {

// 错误类型
    enum class ParseError {
        OK,
        EXPECT_VALUE,
        INVALID_VALUE,
        ROOT_NOT_SINGULAR,
        NUMBER_TOO_BIG,
        MISS_QUOTATION_MARK,
        INVALID_STRING_ESCAPE,
        INVALID_STRING_CHAR,
        INVALID_UNICODE_HEX,
        INVALID_UNICODE_SURROGATE,
        MISS_COMMA_OR_SQUARE_BRACKET,
        MISS_COMMA_OR_CURLY_BRACKET,
        MISS_KEY,
        MISS_COLON,
        MISS_COMMA_OR_CURLY_BRACKET_IN_OBJECT
    };

// JSON值类型
    enum class ValueType {
        NULL_TYPE,
        BOOL,
        NUMBER,
        STRING,
        ARRAY,
        OBJECT
    };

// 内存分配器（简化版）
    class MemoryPool {
    private:
        struct Chunk {
            Chunk* next;
            size_t capacity;
            size_t size;
            char* data;

            Chunk(size_t cap) : next(nullptr), capacity(cap), size(0) {
                data = static_cast<char*>(malloc(cap));
            }

            ~Chunk() {
                free(data);
            }
        };

        Chunk* head_;
        size_t chunk_size_;

    public:
        MemoryPool(size_t chunk_size = 65536) : head_(nullptr), chunk_size_(chunk_size) {}

        ~MemoryPool() {
            while (head_) {
                Chunk* next = head_->next;
                delete head_;
                head_ = next;
            }
        }

        void* Allocate(size_t size) {
            if (!head_ || head_->size + size > head_->capacity) {
                Chunk* new_chunk = new Chunk(std::max(chunk_size_, size));
                new_chunk->next = head_;
                head_ = new_chunk;
            }

            void* ptr = head_->data + head_->size;
            head_->size += size;
            return ptr;
        }
    };

// JSON值类
    class Value {
    private:
        ValueType type_;

        union {
            bool boolean;
            double number;
            struct {
                char* str;
                size_t length;
            } string;
            struct {
                Value* elements;
                size_t size;
                size_t capacity;
            } array;
            struct {
                struct Member {
                    char* key;
                    size_t key_len;
                    Value value;
                }* members;
                size_t size;
                size_t capacity;
            } object;
        } data_;

        MemoryPool* pool_;

    public:
        Value(MemoryPool* pool = nullptr) : type_(ValueType::NULL_TYPE), pool_(pool) {}
        ~Value();

        // 类型检查
        bool IsNull() const { return type_ == ValueType::NULL_TYPE; }
        bool IsBool() const { return type_ == ValueType::BOOL; }
        bool IsNumber() const { return type_ == ValueType::NUMBER; }
        bool IsString() const { return type_ == ValueType::STRING; }
        bool IsArray() const { return type_ == ValueType::ARRAY; }
        bool IsObject() const { return type_ == ValueType::OBJECT; }

        // 获取值
        bool GetBool() const;
        double GetNumber() const;
        const char* GetString() const;
        size_t GetStringLength() const;
        size_t GetArraySize() const;
        const Value& GetArrayElement(size_t index) const;
        size_t GetObjectSize() const;
        const char* GetObjectKey(size_t index) const;
        size_t GetObjectKeyLength(size_t index) const;
        const Value& GetObjectValue(size_t index) const;

        // 解析接口
        ParseError Parse(const char* json);

    private:
        // 解析辅助方法
        ParseError ParseValue(const char*& json);
        ParseError ParseLiteral(const char*& json, const char* literal, ValueType type);
        ParseError ParseNumber(const char*& json);
        ParseError ParseString(const char*& json);
        ParseError ParseArray(const char*& json);
        ParseError ParseObject(const char*& json);

        // 工具方法
        void SkipWhitespace(const char*& json);
        bool IsDigit(char c) { return c >= '0' && c <= '9'; }
        void SetString(const char* str, size_t length);

        // 内存管理
        void* PoolAllocate(size_t size) {
            return pool_ ? pool_->Allocate(size) : malloc(size);
        }

        void PoolFree(void* ptr) {
            if (!pool_) free(ptr);
        }
    };

// 解析器类
    class Parser {
    private:
        MemoryPool pool_;

    public:
        ParseError Parse(Value& value, const char* json) {
            return value.Parse(json);
        }

        // 快速解析接口
        Value Parse(const char* json) {
            Value value(&pool_);
            ParseError err = value.Parse(json);
            if (err != ParseError::OK) {
                throw std::runtime_error("Parse error");
            }
            return value;
        }
    };

} // namespace minijson
