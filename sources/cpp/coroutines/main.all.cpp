// namespace std::experimental
// {
//     template <typename... T> struct coroutine_traits;

//     template <typename T = void> struct coroutine_handle;
//     template <>
//     struct coroutine_handle<void>
//     {
//         static coroutine_handle from_address(void* ptr)
//         {
//             return {ptr};
//         }
//         template <class _Tp, bool _CallIsValid = false>
//         static coroutine_handle from_address(_Tp*) {
//             // static_assert(_CallIsValid, "coroutine_handle<void>::from_address cannot be called with non-void pointers");
//         }

//         constexpr coroutine_handle() :ptr(nullptr) {}
//         constexpr coroutine_handle(void* p) :ptr(p) {}

//         void* address() const { return ptr; }        
//         constexpr explicit operator bool() const { return ptr; }
//         void operator()() { resume(); }

//         void resume() {
//             __builtin_coro_resume(ptr);
//         }
//         void destroy() {
//             __builtin_coro_destroy(ptr);
//         }
//         bool done() const {
//             return __builtin_coro_done(ptr);
//         }
//     private:
//         template <class _PromiseT> friend class coroutine_handle;
//         void* ptr;
//     };

//     template <typename _Promise>
//     class coroutine_handle : public coroutine_handle<> {
//         using _Base = coroutine_handle<>;
//     public:
//         static coroutine_handle from_address(void* ptr) {
//             return {ptr};
//         }

//         _Promise& promise() const 
//         {
//             return *static_cast<_Promise*>(
//                 __builtin_coro_promise(this->ptr, __alignof(_Promise), false));
//         }

//         static coroutine_handle from_promise(_Promise& __promise)
//         {
//             using _RawPromise = _Promise;
//             //auto h = __builtin_coro_promise(::std::addressof(const_cast<_RawPromise&>(__promise)), __alignof(_Promise), true);
//             auto h = __builtin_coro_promise(&(const_cast<_RawPromise&>(__promise)), __alignof(_Promise), true);
//             return {h};
//         }
//     };
// }

// struct suspend_never
// {   
//     bool await_ready() { return false; } 
//     void await_suspend( std::experimental::coroutine_handle<void> ) { }
// };

// struct suspend_always
// {
//     bool await_ready() { return true; }
//     void await_suspend( std::experimental::coroutine_handle<void> ) { }
// };

// template <typename T>
// struct task
// {
//     struct promise
//     {
//         task<T> t;

//         suspend_always initial_suspend(){}
//         suspend_never final_suspend(){}
        
//         void await_suspend( std::experimental::coroutine_handle<void> )
//         {
//         }
        
//         void return_value(T value)
//         {
//             t.value = value;
//         }
        
//         auto get_return_object()
//         {
//             return t;
//         }

//         void unhandled_exception()
//         {
//         }
//     };
//     T value;
// };

// namespace std::experimental
// {
//     template <typename R, typename... Args>
//     struct coroutine_traits<task<R>, Args...>
//     {
//         using promise_type = typename task<R>::promise;
//     };
// }

// task<int> f(int x)
// {
//     co_return x;
// }

#include <experimental/coroutine>
int main()
{
    // auto r = f(12);
    // printf("%d",r.value);
    return 0;
}