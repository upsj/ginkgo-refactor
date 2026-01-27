#pragma once

inline const char *gko_mock_header = R"cc(
namespace std {

template <typename T>
class shared_ptr {
public:
    T* operator->() const;
    T* get() const;
};

}

namespace gko {

class Operation {};

class Executor {
public:
    virtual void run(const Operation& op) const;
};

class ReferenceExecutor : public Executor {};

namespace matrix {

template <typename ValueType>
class Dense {
public:
    ValueType* get_values();
    const ValueType* get_const_values() const;
    int get_stride() const;
    int get_size() const;
    ValueType& at(int, int);
    const ValueType& at(int, int) const;
};

}
}
)cc";