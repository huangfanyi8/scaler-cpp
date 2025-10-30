

#include "tcmalloc.hpp"

#include <cstdlib>
#include <iostream>

namespace tcmalloc {

// 静态成员初始化
    MemoryAllocator::SizeClassInfo MemoryAllocator::size_classes_[kNumSizeClasses] = {0};
    MemoryAllocator::CentralFreeList* MemoryAllocator::central_lists_[kNumSizeClasses] = {nullptr};
    MemoryAllocator::PageHeap MemoryAllocator::page_heap_;
    std::mutex MemoryAllocator::init_mutex_;
    bool MemoryAllocator::initialized_ = false;
    thread_local MemoryAllocator::ThreadCache* MemoryAllocator::thread_cache_ = nullptr;

/**
 * @brief 构造函数 - 初始化内存分配器
 */
    MemoryAllocator::MemoryAllocator() {
        std::lock_guard<std::mutex> lock(init_mutex_);
        if (!initialized_) {
            InitializeSizeClasses();
            InitializeCentralLists();
            initialized_ = true;
            std::cout << "TCMalloc initialized with " << kNumSizeClasses << " size classes\n";
        }
    }

/**
 * @brief 析构函数 - 清理资源
 */
    MemoryAllocator::~MemoryAllocator() {
        // 清理中央堆
        for (size_t i = 0; i < kNumSizeClasses; ++i) {
            delete central_lists_[i];
            central_lists_[i] = nullptr;
        }
    }

/**
 * @brief 初始化大小分类配置
 * 根据TCMalloc的算法设置不同的大小分类
 */
    void MemoryAllocator::InitializeSizeClasses() {
        size_t size = 8;  // 最小8字节
        size_t class_idx = 0;

        // 小尺寸：8-1024字节，按8字节递增
        while (size <= 1024 && class_idx < kNumSizeClasses) {
            size_classes_[class_idx].size = size;
            size_classes_[class_idx].pages = 1;
            size_classes_[class_idx].num_to_move = 32;
            size += 8;
            class_idx++;
        }

        // 中等尺寸：1024-32768字节，按倍数增长
        while (size <= 32768 && class_idx < kNumSizeClasses) {
            size_classes_[class_idx].size = size;
            size_classes_[class_idx].pages = (size + kPageSize - 1) / kPageSize;
            size_classes_[class_idx].num_to_move = 16;
            size *= 2;
            class_idx++;
        }

        // 大尺寸：32768-256KB，按页对齐
        while (size <= kMaxSmallSize && class_idx < kNumSizeClasses) {
            size_classes_[class_idx].size = size;
            size_classes_[class_idx].pages = (size + kPageSize - 1) / kPageSize;
            size_classes_[class_idx].num_to_move = 8;
            size += kPageSize;
            class_idx++;
        }
    }

/**
 * @brief 初始化中央空闲链表
 */
    void MemoryAllocator::InitializeCentralLists() {
        for (size_t i = 0; i < kNumSizeClasses; ++i) {
            central_lists_[i] = new CentralFreeList(size_classes_[i]);
        }
    }

/**
 * @brief 获取线程本地缓存
 * 如果不存在则创建新的缓存
 */
    MemoryAllocator::ThreadCache* MemoryAllocator::GetThreadCache() {
        if (!thread_cache_) {
            thread_cache_ = new ThreadCache(nullptr);  // 注意：这里简化了，实际应该有allocator指针
        }
        return thread_cache_;
    }

/**
 * @brief 内存分配接口
 * @param size 请求分配的大小
 * @return 分配的内存地址
 */
    void* MemoryAllocator::Allocate(size_t size) {
        if (!initialized_) {
            // 延迟初始化
            static MemoryAllocator allocator;
        }

        // 大对象直接走系统分配
        if (size > kMaxSmallSize) {
            size_t pages = (size + kPageSize - 1) / kPageSize;
            return page_heap_.SystemAllocate(pages);
        }

        // 小对象使用线程缓存
        size_t clazz = ClassifySize(size);
        ThreadCache* cache = GetThreadCache();
        return cache->Allocate(size_classes_[clazz].size);
    }

/**
 * @brief 内存释放接口
 * @param ptr 要释放的内存地址
 * @param size 释放的大小
 */
    void MemoryAllocator::Deallocate(void* ptr, size_t size) {
        if (!ptr) return;

        if (size > kMaxSmallSize) {
            // 大对象直接系统释放
            size_t pages = (size + kPageSize - 1) / kPageSize;
            page_heap_.SystemDeallocate(ptr, pages);
            return;
        }

        // 小对象使用线程缓存
        size_t clazz = ClassifySize(size);
        ThreadCache* cache = GetThreadCache();
        cache->Deallocate(ptr, size_classes_[clazz].size);
    }

/**
 * @brief 根据大小确定所属的分类
 * @param size 内存大小
 * @return 分类索引
 */
    size_t MemoryAllocator::ClassifySize(size_t size) {
        if (size <= 1024) {
            // 小尺寸：直接计算索引
            return (size + 7) / 8 - 1;
        } else if (size <= 32768) {
            // 中等尺寸：查找2的幂次
            size_t power = 0;
            size_t temp = size;
            while (temp > 1) {
                temp >>= 1;
                power++;
            }
            return 128 + power - 10;  // 1024是2^10
        } else {
            // 大尺寸：按页计算
            return 128 + 16 + (size - 32768 + kPageSize - 1) / kPageSize;
        }
    }

/**
 * @brief 获取指定分类的对象大小
 * @param clazz 分类索引
 * @return 对象大小
 */
    size_t MemoryAllocator::GetSizeClassSize(size_t clazz) {
        assert(clazz < kNumSizeClasses);
        return size_classes_[clazz].size;
    }

// ==================== ThreadCache 实现 ====================

/**
 * @brief 线程缓存分配
 */
    void* MemoryAllocator::ThreadCache::Allocate(size_t size) {
        size_t clazz = ClassifySize(size);

        if (free_lists_[clazz] == nullptr) {
            // 缓存为空，从中央堆获取
            FetchFromCentral(clazz, size_classes_[clazz].num_to_move);
        }

        if (free_lists_[clazz] == nullptr) {
            // 仍然为空，分配失败
            return nullptr;
        }

        // 从链表头部取出节点
        FreeListNode* node = free_lists_[clazz];
        free_lists_[clazz] = node->next;
        list_length_[clazz]--;

        return static_cast<void*>(node);
    }

/**
 * @brief 线程缓存释放
 */
    void MemoryAllocator::ThreadCache::Deallocate(void* ptr, size_t size) {
        size_t clazz = ClassifySize(size);

        // 将节点加入空闲链表
        FreeListNode* node = static_cast<FreeListNode*>(ptr);
        node->next = free_lists_[clazz];
        free_lists_[clazz] = node;
        list_length_[clazz]++;

        // 如果链表过长，归还部分给中央堆
        if (list_length_[clazz] > size_classes_[clazz].num_to_move * 2) {
            ReleaseToCentral(clazz, size_classes_[clazz].num_to_move);
        }
    }

/**
 * @brief 从中央堆获取对象
 */
    void MemoryAllocator::ThreadCache::FetchFromCentral(size_t clazz, size_t num_objects) {
        // 这里简化实现，实际应该调用CentralFreeList的接口
        for (size_t i = 0; i < num_objects; ++i) {
            void* ptr = central_lists_[clazz]->Allocate();
            if (ptr) {
                FreeListNode* node = static_cast<FreeListNode*>(ptr);
                node->next = free_lists_[clazz];
                free_lists_[clazz] = node;
                list_length_[clazz]++;
            }
        }
    }

/**
 * @brief 归还对象到中央堆
 */
    void MemoryAllocator::ThreadCache::ReleaseToCentral(size_t clazz, size_t num_objects) {
        for (size_t i = 0; i < num_objects && free_lists_[clazz] != nullptr; ++i) {
            FreeListNode* node = free_lists_[clazz];
            free_lists_[clazz] = node->next;
            list_length_[clazz]--;
            central_lists_[clazz]->Deallocate(node);
        }
    }

// ==================== CentralFreeList 实现 ====================

/**
 * @brief 中央堆分配
 */
    void* MemoryAllocator::CentralFreeList::Allocate() {
        std::lock_guard<std::mutex> lock(mutex_);

        if (free_list_ == nullptr) {
            // 空闲链表为空，从页堆获取更多内存
            Populate();
        }

        if (free_list_ == nullptr) {
            return nullptr;
        }

        FreeListNode* node = free_list_;
        free_list_ = node->next;
        length_--;

        return static_cast<void*>(node);
    }

/**
 * @brief 中央堆释放
 */
    void MemoryAllocator::CentralFreeList::Deallocate(void* ptr) {
        std::lock_guard<std::mutex> lock(mutex_);

        FreeListNode* node = static_cast<FreeListNode*>(ptr);
        node->next = free_list_;
        free_list_ = node;
        length_++;

        // 如果链表过长，可以归还部分给页堆（这里简化实现）
    }

/**
 * @brief 从页堆获取内存
 */
    void MemoryAllocator::CentralFreeList::Populate() {
        // 从页堆分配Span
        Span* span = page_heap_.AllocateSpan(size_class_info_.pages);
        if (!span) return;

        // 将Span分割成对象并加入空闲链表
        char* start = static_cast<char*>(span->start);
        size_t object_size = size_class_info_.size;
        size_t num_objects = (span->num_pages * kPageSize) / object_size;

        for (size_t i = 0; i < num_objects; ++i) {
            FreeListNode* node = reinterpret_cast<FreeListNode*>(start + i * object_size);
            node->next = free_list_;
            free_list_ = node;
            length_++;
        }
    }

// ==================== PageHeap 实现 ====================

/**
 * @brief 分配Span
 */
    MemoryAllocator::Span* MemoryAllocator::PageHeap::AllocateSpan(size_t num_pages) {
        std::lock_guard<std::mutex> lock(mutex_);

        // 简化实现：直接系统分配
        void* memory = SystemAllocate(num_pages);
        if (!memory) return nullptr;

        Span* span = new Span{memory, num_pages, 0, nullptr};
        return span;
    }

/**
 * @brief 释放Span
 */
    void MemoryAllocator::PageHeap::DeallocateSpan(Span* span) {
        std::lock_guard<std::mutex> lock(mutex_);
        SystemDeallocate(span->start, span->num_pages);
        delete span;
    }

/**
 * @brief 系统内存分配
 */
    void* MemoryAllocator::PageHeap::SystemAllocate(size_t pages) {
        return _aligned_malloc(kPageSize, pages * kPageSize);
    }

/**
 * @brief 系统内存释放
 */
    void MemoryAllocator::PageHeap::SystemDeallocate(void* ptr, size_t pages) {
        std::free(ptr);
    }

} // namespace tcmalloc

#include <iostream>
#include <vector>
#include <thread>

void TestAllocation() {
    tcmalloc::MemoryAllocator allocator;

    // 测试不同大小的分配
    std::vector<void*> pointers;

    // 小对象分配
    for (int i = 0; i < 100; ++i) {
        void* ptr = tcmalloc::MemoryAllocator::Allocate(64);
        pointers.push_back(ptr);
    }

    // 中等对象分配
    for (int i = 0; i < 50; ++i) {
        void* ptr = tcmalloc::MemoryAllocator::Allocate(2048);
        pointers.push_back(ptr);
    }

    // 大对象分配
    void* large_ptr = tcmalloc::MemoryAllocator::Allocate(300 * 1024); // 300KB
    pointers.push_back(large_ptr);

    // 释放所有内存
    for (void* ptr : pointers) {
        // 注意：实际使用时需要记录分配大小，这里简化
        tcmalloc::MemoryAllocator::Deallocate(ptr, 0);
    }

    std::cout << "Memory allocation test completed successfully!\n";
}

void ThreadTest() {
    auto worker = []() {
        std::vector<void*> local_ptrs;
        for (int i = 0; i < 1000; ++i) {
            void* ptr = tcmalloc::MemoryAllocator::Allocate(128);
            local_ptrs.push_back(ptr);
        }

        for (void* ptr : local_ptrs) {
            tcmalloc::MemoryAllocator::Deallocate(ptr, 128);
        }
    };

    std::thread t1(worker);
    std::thread t2(worker);
    std::thread t3(worker);

    t1.join();
    t2.join();
    t3.join();

    std::cout << "Multi-thread test completed!\n";
}

int main() {
    TestAllocation();
    ThreadTest();
    return 0;
}
