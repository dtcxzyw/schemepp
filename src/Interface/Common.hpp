// SPDX-License-Identifier: MIT

#pragma once

#if _WIN32
#define SCHEMEPP_WINDOWS
#elif __linux__ && !__ANDROID__
#define SCHEMEPP_LINUX
#elif __APPLE__
#define SCHEMEPP_MACOS
#else
#error "Unsupported platform"
#endif

#include <atomic>
#include <exception>

namespace schemepp {
    template <typename T>
    class Ref;

    // Reference Count
    class RefCountBase {
        std::atomic_uint32_t mRefCount{ 0u };

        template <typename T>
        friend class Ref;

        void addRef() noexcept {
            ++mRefCount;
        }
        void decRef() noexcept {
            if((--mRefCount) == 0) {
                delete this;
            }
        }

    public:
        RefCountBase() noexcept = default;
        virtual ~RefCountBase() noexcept = default;
        RefCountBase(RefCountBase&) = delete;
        RefCountBase(RefCountBase&&) = delete;
        RefCountBase& operator=(RefCountBase&) = delete;
        RefCountBase& operator=(RefCountBase&&) = delete;
    };

    template <typename T>
    class Ref final {
        T* mPtr;

        template <typename U>
        friend class Ref;

    public:
        Ref() noexcept : mPtr{ nullptr } {}
        explicit Ref(nullptr_t) noexcept : mPtr{ nullptr } {}
        explicit Ref(T* ptr) noexcept : mPtr{ ptr } {
            if(mPtr)
                mPtr->addRef();
        }
        ~Ref() noexcept {
            if(mPtr)
                mPtr->decRef();
        }
        Ref(const Ref& rhs) noexcept : Ref{ rhs.mPtr } {}
        template <typename U>
        // ReSharper disable once CppNonExplicitConvertingConstructor
        Ref(const Ref<U>& rhs) noexcept : Ref{ rhs.mPtr } {}

        Ref(Ref&& rhs) noexcept : mPtr{ rhs.mPtr } {
            rhs.mPtr = nullptr;
        }

        Ref& operator=(Ref&& rhs) noexcept {
            if(this != &rhs) {
                if(mPtr)
                    mPtr->decRef();
                mPtr = rhs.mPtr;
                rhs.mPtr = nullptr;
            }
            return *this;
        }

        Ref& operator=(const Ref& rhs) noexcept {  // NOLINT(bugprone-unhandled-self-assignment)
            if(mPtr != rhs.mPtr) {
                if(rhs.mPtr)
                    rhs.mPtr->addRef();
                if(mPtr)
                    mPtr->decRef();
                mPtr = rhs.mPtr;
            }
            return *this;
        }

        T& operator*() const noexcept {
            return *mPtr;
        }

        T* operator->() const noexcept {
            return mPtr;
        }

        T* get() noexcept {
            return mPtr;
        }

        const T* get() const noexcept {
            return mPtr;
        }

        explicit operator bool() const noexcept {
            return mPtr != nullptr;
        }
    };

    template <typename T, typename... Args, typename = std::enable_if_t<std::is_base_of_v<RefCountBase, T>>>
    [[nodiscard]] auto makeRefCount(Args&&... args) noexcept -> Ref<T> {
        auto ptr = new T(std::forward<Args>(args)...);
        return Ref<T>{ ptr };
    }

    class NotImplemented final : public std::exception {
    public:
        NotImplemented() : exception{ "Not implemented feature" } {}
    };
}  // namespace schemepp
