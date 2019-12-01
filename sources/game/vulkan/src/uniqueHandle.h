template <typename T>
struct unique_handle
{
    using THANDLE = typename T::HandleType;
    unique_handle(THANDLE h) : handle(h) {}

    //NON COPYABLE
    unique_handle(const unique_handle&) = delete;
    unique_handle& operator=(const unique_handle&) = delete;

    //MOVEABLE
    unique_handle(unique_handle&& other) 
        : handle(other.handle)
    {
        other.handle = T::INVALID_HANDLE;
    }   

    unique_handle& operator = (unique_handle&& other)
    {
        if (this != &other) {
            handle = other.handle;
            other.handle = T::INVALID_HANDLE;
        }
        return *this;
    }

    ~unique_handle()
    {
        if(handle != T::INVALID_HANDLE)
            T::CloseHandle(handle);
    }

    operator THANDLE() const & noexcept { return handle; }
private:
    THANDLE handle;
};