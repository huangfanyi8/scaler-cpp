#pragma once

#include <cstddef>
#include <cstdint>
#include <vector>
#include <mutex>
#include <algorithm>
#include <cassert>

namespace tcmalloc {

/**
 * @brief 内存分配器类，模仿TCMalloc的核心思想
 *
 * TCMalloc主要特点：
 * 1. 线程本地缓存(ThreadCache)
 * 2. 中央堆(CentralFreeList)
 * 3. 页堆(PageHeap)
 * 4. 大小分类(SizeClass)
 */
    class MemoryAllocator {
    private:
        // 大小分类相关常量
        static constexpr size_t kMaxSmallSize = 256 * 1024;  // 小对象最大256KB
        static constexpr size_t kNumSizeClasses = 86;        // 大小分类数量
        static constexpr size_t kPageSize = 4096;            // 页大小4KB
        static constexpr size_t kAlignment = 8;              // 内存对齐8字节

        /**
         * @brief 大小分类信息
         */
        struct SizeClassInfo {
            size_t size;           // 该分类的对象大小
            size_t pages;          // 每次分配的页数
            size_t num_to_move;    // 每次从中央堆转移的对象数量
        };

        /**
         * @brief 空闲链表节点
         */
        union FreeListNode {
            FreeListNode* next;    // 指向下一个空闲节点
            char data[1];          // 用于内存对齐
        };

        /**
         * @brief 线程本地缓存
         * 每个线程有自己的缓存，减少锁竞争
         */
        class ThreadCache {
        private:
            FreeListNode* free_lists_[kNumSizeClasses] = {nullptr};  // 每个大小分类的空闲链表
            size_t list_length_[kNumSizeClasses] = {0};              // 每个链表的长度

            MemoryAllocator* allocator_;  // 指向中央分配器

        public:
            explicit ThreadCache(MemoryAllocator* allocator) : allocator_(allocator) {}

            void* Allocate(size_t size);
            void Deallocate(void* ptr, size_t size);

            // 从中央堆获取更多对象
            void FetchFromCentral(size_t clazz, size_t num_objects);
            // 将过多对象归还给中央堆
            void ReleaseToCentral(size_t clazz, size_t num_objects);
        };

        /**
         * @brief 中央空闲链表
         * 管理特定大小分类的内存块
         */
        class CentralFreeList {
        private:
            FreeListNode* free_list_ = nullptr;  // 空闲链表
            size_t length_ = 0;                  // 链表长度
            size_t max_length_ = 0;              // 最大长度限制
            std::mutex mutex_;                   // 保护中央堆的锁

            SizeClassInfo size_class_info_;      // 大小分类信息

        public:
            explicit CentralFreeList(const SizeClassInfo& info) : size_class_info_(info) {
                max_length_ = info.num_to_move * 2;
            }

            void* Allocate();
            void Deallocate(void* ptr);

            // 从页堆获取更多内存
            void Populate();
        };
        struct Span {
            void* start;          // 内存块起始地址
            size_t num_pages;     // 包含的页数
            size_t refcount;      // 引用计数
            Span* next;           // 下一个Span
        };
        /**
         * @brief 页堆管理器
         * 负责从系统分配内存，按页管理
         */
        class PageHeap {
        private:


            Span* free_spans_[kNumSizeClasses] = {nullptr};  // 按大小分类的空闲Span
            std::mutex mutex_;                               // 保护页堆的锁

        public:
            Span* AllocateSpan(size_t num_pages);
            void DeallocateSpan(Span* span);

            // 从系统分配内存
            void* SystemAllocate(size_t pages);
            void SystemDeallocate(void* ptr, size_t pages);
        };

        // 静态数据成员
        static SizeClassInfo size_classes_[kNumSizeClasses];  // 大小分类配置
        static CentralFreeList* central_lists_[kNumSizeClasses];  // 中央空闲链表
        static PageHeap page_heap_;                            // 全局页堆
        static std::mutex init_mutex_;                         // 初始化锁
        static bool initialized_;                              // 初始化标志

        // 线程本地存储
        static thread_local ThreadCache* thread_cache_;        // 线程本地缓存

    public:
        MemoryAllocator();
        ~MemoryAllocator();

        static void* Allocate(size_t size);
        static void Deallocate(void* ptr, size_t size);

        // 工具函数
        static size_t ClassifySize(size_t size);
        static size_t GetSizeClassSize(size_t clazz);

    private:
        // 初始化函数
        static void InitializeSizeClasses();
        static void InitializeCentralLists();

        // 获取线程本地缓存
        static ThreadCache* GetThreadCache();
    };

} // namespace tcmalloc
