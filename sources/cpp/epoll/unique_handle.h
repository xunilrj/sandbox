#pragma once

template <typename Type, typename Traits>
class unique_handle
{
    unique_handle(unique_handle const &) = delete;
    unique_handle &operator=(unique_handle const &) = delete;
    void close() throw()
    {
        if (*this)
        {
            Traits::close(m_value);
        }
    }
    Type m_value;

public:
    explicit unique_handle(Type value = Traits::invalid()) throw() : m_value(value)
    {
    }
    ~unique_handle() throw()
    {
        close();
    }

private:
    struct boolean_struct
    {
        int member;
    };
    typedef int boolean_struct::*boolean_type;
    bool operator==(unique_handle const &) = delete;
    bool operator!=(unique_handle const &) = delete;

public:
    operator boolean_type() const throw()
    {
        return Traits::invalid() != m_value ? &boolean_struct::member : nullptr;
    }

    bool reset(Type value = Traits::invalid()) throw()
    {
        if (m_value != value)
        {
            close();
            m_value = value;
        }
        return *this;
    }

    Type release() throw()
    {
        auto value = m_value;
        m_value = Traits::invalid();
        return value;
    }

    unique_handle(unique_handle &&other) throw() : m_value(other.release())
    {
    }

    unique_handle &operator=(unique_handle &&other) throw()
    {
        reset(other.release());
        return *this;
    }

    operator const Type &() const
    {
        return m_value;
    }
};