#pragma once
#include <stdint.h>
#include <vector>

#define ARRSIZE(arr) (sizeof(arr) / sizeof(*arr))

#define CALL_FUNCTION_MACRO(fnptr, data, count, output) \
    if(0 == count) {\
        output = reinterpret_cast<float(*)()>(fnptr)();\
    }\
    else if(1 == count) {\
        output = reinterpret_cast<float(*)(float)>(fnptr)(data[0]);\
    }\
    else if(2 == count) {\
        output = reinterpret_cast<float(*)(float, float)>(fnptr)(data[0], data[1]);\
    }\
    else if(3 == count) {\
        output = reinterpret_cast<float(*)(float, float, float)>(fnptr)(data[0], data[1], data[2]);\
    }\
    else if(4 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3]);\
    }\
    else if(5 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4]);\
    }\
    else if(6 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5]);\
    }\
    else if(7 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6]);\
    }\
    else if(8 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7]);\
    }\
    else if(9 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8]);\
    }\
    else if(10 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9]);\
    }\
    else if(11 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9], data[10]);\
    }\
    else if(12 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9], data[10], data[11]);\
    }\
    else if(13 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9], data[10], data[11], data[12]);\
    }\
    else if(14 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9], data[10], data[11], data[12], data[13]);\
    }\
    else if(15 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9], data[10], data[11], data[12], data[13], data[14]);\
    }\
    else if(16 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9], data[10], data[11], data[12], data[13], data[14], data[15]);\
    }\
    else if(17 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9], data[10], data[11], data[12], data[13], data[14], data[15], data[16]);\
    }\
    else if(18 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9], data[10], data[11], data[12], data[13], data[14], data[15], data[16], data[17]);\
    }\
    else if(19 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9], data[10], data[11], data[12], data[13], data[14], data[15], data[16], data[17], data[18]);\
    }\
    else if(20 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9], data[10], data[11], data[12], data[13], data[14], data[15], data[16], data[17], data[18], data[19]);\
    }


template<typename T>
struct FixedAllocator {
    struct Data {
        uint8_t elements[sizeof(T)*64];
        uint64_t bits;
    };

    FixedAllocator() {
    }
    FixedAllocator(FixedAllocator&) = delete;
    FixedAllocator(FixedAllocator&& other) {
        this->allocations = std::move(other.allocations);
    }
    ~FixedAllocator() {
        for(Data* d : this->allocations) {
            for(uint64_t i = 0uLL; i < 64uLL; ++i) {
                const uint64_t mask = (1uLL << i);
                if((d->bits & mask) == mask) {
                    reinterpret_cast<T*>(&d->elements[sizeof(T)*i])->~T();
                }
            }
            delete d;
        }

    }

    // doesn't call constructor
    T* AllocMemory() {
        for(Data* d : this->allocations) {
            if(d->bits != UINT64_MAX) {
                for(uint64_t i = 0uLL; i < 64uLL; ++i) {
                    const uint64_t mask = (1uLL << i);
                    if((d->bits & mask) == 0uLL) {
                        d->bits |= mask;
                        return reinterpret_cast<T*>(&d->elements[sizeof(T)*i]);
                    }
                }
            }
        }
        this->allocations.push_back(new Data{});
        this->allocations.back()->bits = 1uLL;
        return reinterpret_cast<T*>(this->allocations.back()->elements);
    }

    T* push_back(const T& data) {
        T* allocated = AllocMemory();
        new(allocated)T(data);
        return allocated;
    }
    T* emplace_back(T&& data) {
        T* allocated = AllocMemory();
        new(allocated)T(std::move(data));
        return allocated;
    }

    // calls destructor
    void Delete(T* element) {
        if(element) {
            for(size_t i = 0; i < this->allocations.size(); ++i)  {
                Data* d = this->allocations.at(i);
                if(static_cast<uintptr_t>(element) >= d->elements) {
                    const uintptr_t idx = (static_cast<uintptr_t>(element) - static_cast<uintptr_t>(d->elements)) / sizeof(T);
                    if(idx < 64) {
                        const uint64_t mask = ~(1uLL << idx);
                        d->bits &= mask;
                        element->~T();
                        memset(element, 0, sizeof(T));
                        if(d->bits == 0uLL) {
                            delete d;
                            this->allocations.erase(this->allocations.begin() + i);
                        }
                        return;
                    }
                }
            }
        }
    }
    std::vector<Data*> allocations;
};


