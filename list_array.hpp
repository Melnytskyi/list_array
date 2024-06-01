#ifndef SRC_LIBRARY_LIST_ARRAY_LIST_ARRAY
#define SRC_LIBRARY_LIST_ARRAY_LIST_ARRAY
#include <iterator>
#include <memory>
#include <stdexcept>
#include <stdint.h>
#include <type_traits>
#include <utility>

namespace __list_array_impl {
    template <typename... Ts>
    struct conditions_helper {};

    template <typename T, typename _ = void>
    struct is_container : std::false_type {};

    template <typename T>
    struct is_container<
        T,
        std::conditional_t<
            false,
            conditions_helper<
                typename T::value_type,
                typename T::size_type,
                typename T::iterator,
                typename T::const_iterator,
                decltype(std::declval<T>().size()),
                decltype(std::declval<T>().begin()),
                decltype(std::declval<T>().end()),
                decltype(std::declval<T>().cbegin()),
                decltype(std::declval<T>().cend())>,
            void>> : std::true_type {
        using value_type = typename T::value_type;
        using container = typename T;
    };

    template <typename T, typename _ = void>
    struct can_direct_index : std::false_type {};

    template <class Alloc, class T, bool = std::is_empty_v<Alloc> && !std::is_final_v<Alloc>>
    struct compressed_allocator final : private Alloc {
    public:
        T hold_value;

        template <class... Args>
        compressed_allocator(const Alloc& allocator, Args&&... args)
            : Alloc(allocator), hold_value(std::forward<Args>(args)...) {}

        compressed_allocator(compressed_allocator&& move)
            : Alloc(move), hold_value(std::move(move.hold_value)) {}

        using value_type = typename Alloc::value_type;
        using size_type = typename Alloc::size_type;

        value_type* allocate(size_type n) {
            return Alloc::allocate(n);
        }

        void deallocate(value_type* p, size_type n) {
            Alloc::deallocate(p, n);
        }

        value_type* allocate_default_construct(size_type n) {
            value_type* allocated = allocate(n);
            for (size_type i = 0; i < n; i++)
                std::construct_at(allocated + i);
            return allocated;
        }

        void deallocate_destruct(value_type* p, size_type n) {
            std::destroy(p, p + n);
            deallocate(p, n);
        }

        template <class... Args>
        value_type* allocate_construct(size_type n, Args... args) {
            value_type* allocated = allocate(n);
            for (size_type i = 0; i < n; i++)
                std::construct_at(allocated + i, std::forward(args)...);
            return allocated;
        }

        Alloc& get_allocator() {
            return *this;
        }

        const Alloc& get_allocator() const {
            return *this;
        }

        compressed_allocator& operator=(const compressed_allocator& allocator) {
            if constexpr (std::allocator_traits<Alloc>::propagate_on_container_copy_assignment::value)
                Alloc::operator=(allocator);
            hold_value = allocator.hold_value;
            return *this;
        }

        compressed_allocator& operator=(compressed_allocator&& allocator) {
            if constexpr (std::allocator_traits<Alloc>::propagate_on_container_move_assignment::value)
                Alloc::operator=(std::move(allocator));
            hold_value = std::move(allocator.hold_value);
            return *this;
        }
    };

    template <class Alloc, class T>
    struct compressed_allocator<Alloc, T, false> final {
        Alloc allocator;

    public:
        T hold_value;

        template <class... Args>
        compressed_allocator(const Alloc& allocator, Args&&... args)
            : allocator(allocator), hold_value(std::forward<Args>(args)...) {}

        compressed_allocator(compressed_allocator&& move)
            : allocator(move), hold_value(std::move(move.hold_value)) {}

        using value_type = typename Alloc::value_type;
        using size_type = typename Alloc::size_type;

        value_type* allocate(size_type n) {
            return allocator.allocate(n);
        }

        void deallocate(value_type* p, size_type n) {
            allocator.deallocate(p, n);
        }

        value_type* allocate_default_construct(size_type n) {
            value_type* allocated = allocate(n);
            for (size_type i = 0; i < n; i++)
                std::construct_at(allocated + i);
            return allocated;
        }

        void deallocate_destruct(value_type* p, size_type n) {
            std::destroy(p, p + n);
            deallocate(p, n);
        }

        template <class... Args>
        value_type* allocate_construct(size_type n, Args... args) {
            value_type* allocated = allocate(n);
            for (size_type i = 0; i < n; i++)
                std::construct_at(allocated + i, std::forward(args)...);
            return allocated;
        }

        Alloc& get_allocator() {
            return allocator;
        }

        const Alloc& get_allocator() const {
            return allocator;
        }

        compressed_allocator operator=(const compressed_allocator& allocator) {
            if constexpr (std::allocator_traits<Alloc>::propagate_on_container_copy_assignment::value)
                allocator = allocator;
            hold_value = allocator.hold_value;
        }

        compressed_allocator operator=(compressed_allocator&& allocator) {
            if constexpr (std::allocator_traits<Alloc>::propagate_on_container_move_assignment::value)
                allocator = std::move(allocator);
            hold_value = std::move(allocator.hold_value);
        }
    };

    template <class Alloc, class T, bool = std::is_empty_v<Alloc> && !std::is_final_v<Alloc>>
    struct compressed_allocator_refrence final : private Alloc {
    public:
        T hold_value;

        template <class... Args>
        compressed_allocator_refrence(Alloc& allocator, Args&& ...args) 
        : Alloc(allocator), hold_value(std::forward<Args>(args)...) {}

        using value_type = typename Alloc::value_type;
        using size_type = typename Alloc::size_type;

        value_type* allocate(size_type n) {
            return Alloc::allocate(n);
        }

        void deallocate(value_type* p, size_type n) {
            Alloc::deallocate(p, n);
        }

        value_type* allocate_default_construct(size_type n) {
            value_type* allocated = allocate(n);
            for (size_type i = 0; i < n; i++)
                std::construct_at(allocated + i);
            return allocated;
        }

        void deallocate_destruct(value_type* p, size_type n) {
            if constexpr (!std::is_trivially_destructible_v<T>)
                std::destroy(p, p + n);
            deallocate(p, n);
        }

        template <class... Args>
        value_type* allocate_construct(size_type n, Args... args) {
            value_type* allocated = allocate(n);
            for (size_type i = 0; i < n; i++)
                std::construct_at(allocated + i, std::forward(args)...);
            return allocated;
        }

        Alloc& get_allocator() {
            return *this;
        }

        const Alloc& get_allocator() const {
            return *this;
        }
    };

    template <class Alloc, class T>
    struct compressed_allocator_refrence<Alloc, T, false> final {
        Alloc& allocator;

    public:
        T hold_value;

        template <class... Args>
        compressed_allocator_refrence(Alloc& allocator, Args&&... args)
            : allocator(allocator), hold_value(std::forward<Args>(args)...) {}

        using value_type = typename Alloc::value_type;
        using size_type = typename Alloc::size_type;

        value_type* allocate(size_type n) {
            return allocator.allocate(n);
        }

        void deallocate(value_type* p, size_type n) {
            allocator.deallocate(p, n);
        }

        value_type* allocate_default_construct(size_type n) {
            value_type* allocated = allocate(n);
            for (size_type i = 0; i < n; i++)
                std::construct_at(allocated + i);
            return allocated;
        }

        void deallocate_destruct(value_type* p, size_type n) {
            std::destroy(p, p + n);
            deallocate(p, n);
        }

        template <class... Args>
        value_type* allocate_construct(size_type n, Args... args) {
            value_type* allocated = allocate(n);
            for (size_type i = 0; i < n; i++)
                std::construct_at(allocated + i, std::forward(args)...);
            return allocated;
        }

        Alloc& get_allocator() {
            return allocator;
        }

        const Alloc& get_allocator() const {
            return allocator;
        }
    };

    template <typename T>
    struct can_direct_index<
        T,
        std::conditional_t<
            false,
            conditions_helper<decltype(std::declval<T>().data())>,
            void>> : public std::true_type {};

    template <class T, class Allocator>
    class list_array;
    template <class T, class Allocator>
    class dynamic_arr;
    template <class T, class Allocator>
    class arr_block;

    template <class T, class Allocator>
    class reverse_iterator;
    template <class T, class Allocator>
    class const_iterator;
    template <class T, class Allocator>
    class const_reverse_iterator;

    template <class T, class Allocator>
    class iterator {
        friend class list_array<T, Allocator>;
        friend class dynamic_arr<T, Allocator>;
        friend class const_iterator<T, Allocator>;
        friend class reverse_iterator<T, Allocator>;
        friend class const_reverse_iterator<T, Allocator>;
        arr_block<T, Allocator>* block;
        size_t pos;

        constexpr bool _nextBlock() {
            block = block ? block->next_ : block;
            pos = 0;
            return block && bool(block ? block->next_ : nullptr);
        }

        template<bool copy_construct>
        constexpr void _fast_load(T* arr, size_t arr_size) {
            size_t j = pos;
            arr_block<T, Allocator>* block_tmp = block;
            size_t block_size = block_tmp->_size;
            T* block_arr = block->pair.hold_value;

            for (size_t i = 0; i < arr_size;) {
                for (; i < arr_size && j < block_size; j++){
                    if constexpr (copy_construct)
                        std::construct_at(arr + i++, block_arr[j]);
                    else
                        arr[i++] = block_arr[j];
                }
                j = 0;
                block_tmp = block_tmp->next_;
                if (!block_tmp)
                    return;
                block_size = block_tmp->_size;
                block_arr = block_tmp->pair.hold_value;
            }
        }

    public:
        using iterator_category = std::bidirectional_iterator_tag;
        using value_type = T;
        using difference_type = ptrdiff_t;
        using pointer = T*;
        using reference = T&;

        constexpr iterator() {
            block = nullptr;
            pos = 0;
        }

        constexpr iterator& operator=(const iterator& setter) {
            block = setter.block;
            pos = setter.pos;
            return *this;
        }

        constexpr iterator(const iterator& copy) {
            *this = copy;
        }

        constexpr iterator(arr_block<T, Allocator>* block_pos, size_t set_pos) {
            block = block_pos;
            pos = set_pos;
            if (block)
                if (block->_size == set_pos && block->next_) {
                    block = block->next_;
                    pos = 0;
                }
        }

        constexpr iterator& operator++() {
            if (block) {
                if (block->_size <= ++pos) {
                    block = block->next_;
                    pos = 0;
                }
            }
            return *this;
        }

        constexpr iterator operator++(int) {
            iterator tmp = *this;
            operator++();
            return tmp;
        }

        constexpr iterator& operator--() {
            if (block) {
                if (0 == --pos) {
                    block = block->_prev;
                    pos = block ? block->_size : 0;
                }
            }
            return *this;
        }

        constexpr iterator operator--(int) {
            iterator tmp = *this;
            operator--();
            return tmp;
        }

        constexpr bool operator==(const iterator& comparer) const {
            return block == comparer.block && pos == comparer.pos && block;
        }

        constexpr bool operator!=(const iterator& comparer) const {
            return (block != comparer.block || pos != comparer.pos) && block;
        }

        constexpr T& operator*() {
            return block->pair.hold_value[pos];
        }

        constexpr const T& operator*() const {
            return block->pair.hold_value[pos];
        }

        constexpr T* operator->() {
            return block->pair.hold_value + pos;
        }
    };

    template <class T, class Allocator>
    class const_iterator {
        friend class list_array<T, Allocator>;
        friend class dynamic_arr<T, Allocator>;
        friend class const_reverse_iterator<T, Allocator>;
        const arr_block<T, Allocator>* block;
        size_t pos;

        template<bool copy_construct>
        constexpr void _fast_load(T* arr, size_t arr_size) const {
            size_t j = pos;
            const arr_block<T, Allocator>* block_tmp = block;
            size_t block_size = block_tmp->_size;
            T* block_arr = block->pair.hold_value;

            for (size_t i = 0; i < arr_size;) {
                for (; i < arr_size && j < block_size; j++)
                    if constexpr (copy_construct)
                        std::construct_at(arr + i++, block_arr[j]);
                    else
                        arr[i++] = block_arr[j];
                j = 0;
                block_tmp = block_tmp->next_;
                if (!block_tmp)
                    return;
                block_size = block_tmp->_size;
                block_arr = block_tmp->pair.hold_value;
            }
        }

    public:
        using iterator_category = std::bidirectional_iterator_tag;
        using value_type = T;
        using difference_type = ptrdiff_t;
        using pointer = T*;
        using reference = T&;

        constexpr const_iterator() {
            block = nullptr;
            pos = 0;
        }

        constexpr const_iterator& operator=(const const_iterator& setter) {
            block = setter.block;
            pos = setter.pos;
            return *this;
        }

        constexpr const_iterator(const const_iterator& copy) {
            *this = copy;
        }

        constexpr const_iterator& operator=(const iterator<T, Allocator>& setter) {
            block = setter.block;
            pos = setter.pos;
            return *this;
        }

        constexpr const_iterator(const iterator<T, Allocator>& copy) {
            *this = copy;
        }

        constexpr const_iterator(arr_block<T, Allocator>* block_pos, size_t set_pos) {
            block = block_pos;
            pos = set_pos;
        }

        constexpr const_iterator& operator++() {
            if (block) {
                if (block->_size <= ++pos) {
                    block = block->next_;
                    pos = 0;
                }
            }
            return *this;
        }

        constexpr const_iterator operator++(int) {
            const_iterator tmp = *this;
            operator++();
            return tmp;
        }

        constexpr const_iterator& operator--() {
            if (block) {
                if (0 == --pos) {
                    block = block->_prev;
                    pos = block ? block->_size : 0;
                }
            }
            return *this;
        }

        constexpr const_iterator operator--(int) {
            const_iterator tmp = *this;
            operator--();
            return tmp;
        }

        constexpr bool operator==(const const_iterator& comparer) const {
            return block == comparer.block && pos == comparer.pos;
        }

        constexpr bool operator!=(const const_iterator& comparer) const {
            return (block != comparer.block || pos != comparer.pos) && block;
        }

        constexpr const T& operator*() {
            return block->pair.hold_value[pos];
        }

        constexpr T* operator->() {
            return block->pair.hold_value + pos;
        }
    };

    template <class T, class Allocator>
    class reverse_iterator {
        friend class dynamic_arr<T, Allocator>;
        friend class const_reverse_iterator<T, Allocator>;
        arr_block<T, Allocator>* block;
        size_t pos;

    public:
        using iterator_category = std::bidirectional_iterator_tag;
        using value_type = T;
        using difference_type = ptrdiff_t;
        using pointer = T*;
        using reference = T&;

        constexpr reverse_iterator() {
            block = nullptr;
            pos = 0;
        }

        constexpr reverse_iterator& operator=(const iterator<T, Allocator>& setter) {
            block = setter.block;
            pos = setter.pos;
            return *this;
        }

        constexpr reverse_iterator(const iterator<T, Allocator>& copy) {
            *this = copy;
        }

        constexpr reverse_iterator& operator=(const reverse_iterator& setter) {
            block = setter.block;
            pos = setter.pos;
            return *this;
        }

        constexpr reverse_iterator(const reverse_iterator& copy) {
            *this = copy;
        }

        constexpr reverse_iterator(arr_block<T, Allocator>* block_pos, size_t set_pos) {
            block = block_pos;
            pos = set_pos;
        }

        constexpr reverse_iterator& operator++() {
            if (block) {
                if (0 == --pos) {
                    block = block->_prev;
                    pos = block ? block->_size : 0;
                }
            }
            return *this;
        }

        constexpr reverse_iterator operator++(int) {
            reverse_iterator tmp = *this;
            operator++();
            return tmp;
        }

        constexpr reverse_iterator& operator--() {
            if (block) {
                if (block->_size == ++pos) {
                    block = block->next_;
                    pos = 0;
                }
            }
            return *this;
        }

        constexpr reverse_iterator operator--(int) {
            reverse_iterator tmp = *this;
            operator--();
            return tmp;
        }

        constexpr bool operator==(const reverse_iterator& comparer) const {
            return block == comparer.block && pos == comparer.pos;
        }

        constexpr bool operator!=(const reverse_iterator& comparer) const {
            return (block != comparer.block || pos != comparer.pos) && block;
        }

        constexpr T& operator*() {
            return block->pair.hold_value[pos - 1];
        }

        constexpr const T& operator*() const {
            return block->pair.hold_value[pos - 1];
        }

        constexpr T* operator->() {
            return block->pair.hold_value + pos - 1;
        }
    };

    template <class T, class Allocator>
    class const_reverse_iterator {
        friend class dynamic_arr<T, Allocator>;
        friend class const_iterator<T, Allocator>;
        const arr_block<T, Allocator>* block;
        size_t pos;

    public:
        using iterator_category = std::bidirectional_iterator_tag;
        using value_type = T;
        using difference_type = ptrdiff_t;
        using pointer = T*;
        using reference = T&;

        constexpr const_reverse_iterator() {
            block = nullptr;
            pos = 0;
        }

        constexpr const_reverse_iterator& operator=(const const_iterator<T, Allocator>& setter) {
            block = setter.block;
            pos = setter.pos;
            return *this;
        }

        constexpr const_reverse_iterator(const const_iterator<T, Allocator>& copy) {
            *this = copy;
        }

        constexpr const_reverse_iterator& operator=(const iterator<T, Allocator>& setter) {
            block = setter.block;
            pos = setter.pos;
            return *this;
        }

        constexpr const_reverse_iterator(const iterator<T, Allocator>& copy) {
            *this = copy;
        }

        constexpr const_reverse_iterator& operator=(const reverse_iterator<T, Allocator>& setter) {
            block = setter.block;
            pos = setter.pos;
            return *this;
        }

        constexpr const_reverse_iterator(const reverse_iterator<T, Allocator>& copy) {
            *this = copy;
        }

        constexpr const_reverse_iterator& operator=(const const_reverse_iterator& setter) {
            block = setter.block;
            pos = setter.pos;
            return *this;
        }

        constexpr const_reverse_iterator(const const_reverse_iterator& copy) {
            *this = copy;
        }

        constexpr const_reverse_iterator(arr_block<T, Allocator>* block_pos, size_t set_pos) {
            block = block_pos;
            pos = set_pos;
        }

        constexpr const_reverse_iterator& operator++() {
            if (block) {
                if (0 == --pos) {
                    block = block->_prev;
                    pos = block ? block->_size : 0;
                }
            }
            return *this;
        }

        constexpr const_reverse_iterator operator++(int) {
            const_reverse_iterator tmp = *this;
            operator++();
            return tmp;
        }

        constexpr const_reverse_iterator& operator--() {
            if (block) {
                if (block->_size == ++pos) {
                    block = block->next_;
                    pos = 0;
                }
            }
            return *this;
        }

        constexpr const_reverse_iterator operator--(int) {
            const_reverse_iterator tmp = *this;
            operator--();
            return tmp;
        }

        constexpr bool operator==(const const_reverse_iterator& comparer) const {
            return block == comparer.block && pos == comparer.pos;
        }

        constexpr bool operator!=(const const_reverse_iterator& comparer) const {
            return (block != comparer.block || pos != comparer.pos) && block;
        }

        constexpr const T& operator*() const {
            return block->pair.hold_value[pos - 1];
        }

        constexpr T* operator->() {
            return block->pair.hold_value + pos - 1;
        }
    };

    template <class T, class Allocator>
    class arr_block {
        friend class list_array<T, Allocator>;
        friend class dynamic_arr<T, Allocator>;
        friend class iterator<T, Allocator>;
        friend class const_iterator<T, Allocator>;
        friend class reverse_iterator<T, Allocator>;
        friend class const_reverse_iterator<T, Allocator>;

        arr_block* _prev = nullptr;
        arr_block* next_ = nullptr;
        compressed_allocator_refrence<Allocator, T*> pair;
        size_t _size = 0;

        void good_bye_world() {
            if (_prev)
                _prev->next_ = next_;
            if (next_)
                next_->_prev = _prev;
            _prev = next_ = nullptr;
            delete this;
        }

        constexpr static T* cxx_resize(T* val, size_t old_size, size_t new_size, compressed_allocator_refrence<Allocator, T*>& allocator) {
            if (new_size == old_size)
                return val;
            T* new_val = allocator.allocate(new_size);
            if (old_size < new_size) {
                if constexpr (std::is_move_constructible_v<T>)
                    for (size_t i = 0; i < old_size; i++)
                        std::construct_at(new_val + i, std::move(val[i]));
                else
                    for (size_t i = 0; i < old_size; i++)
                        std::construct_at(new_val + i, val[i]);
            } else {
                if constexpr (std::is_move_constructible_v<T>)
                    for (size_t i = 0; i < new_size; i++)
                        std::construct_at(new_val + i, std::move(val[i]));
                else
                    for (size_t i = 0; i < new_size; i++)
                        std::construct_at(new_val + i, val[i]);
            }
            allocator.deallocate_destruct(val, old_size);
            return new_val;
        }

    public:
        constexpr arr_block(Allocator& allocator)
            : pair(allocator) {}

        constexpr arr_block(Allocator& allocator, const arr_block& copy)
            : pair(allocator) {
            operator=(copy);
        }

        constexpr arr_block(Allocator& allocator, arr_block&& move) noexcept
            : pair(allocator) {
            operator=(std::move(move));
        }

        constexpr arr_block(Allocator& allocator, arr_block* prev, size_t len, arr_block* next)
            : pair(allocator) {
            if ((_prev = prev))
                _prev->next_ = this;
            if ((next_ = next))
                next_->_prev = this;

            pair.hold_value = pair.allocate_default_construct(len);
            _size = len;
        }

        constexpr ~arr_block() {
            if (_prev) {
                _prev->next_ = nullptr;
                delete _prev;
            } else if (next_) {
                next_->_prev = nullptr;
                delete next_;
            }
            if (pair.hold_value)
                pair.deallocate_destruct(pair.hold_value, _size);
        }

        constexpr T& operator[](size_t pos) {
            return (pos < _size) ? pair.hold_value[pos] : (*next_)[pos - _size];
        }

        constexpr const T& operator[](size_t pos) const {
            return (pos < _size) ? pair.hold_value[pos] : (*next_)[pos - _size];
        }

        constexpr arr_block& operator=(const arr_block& copy) {
            if (this == &copy)
                return *this;
            if (pair.hold_value)
                pair.deallocate_destruct(pair.hold_value, _size);
            _size = copy._size;
            pair.hold_value = pair.allocate(_size);
            for (size_t i = 0; i < _size; i++)
                std::construct_at(pair.hold_value + i, copy.pair.hold_value[i]);
            return *this;
        }

        constexpr arr_block& operator=(arr_block&& move) noexcept {
            if (this == &move)
                return *this;
            pair.hold_value = move.pair.hold_value;
            _prev = move._prev;
            next_ = move.next_;
            _size = move._size;
            move._prev = move.next_ = nullptr;
            move.pair.hold_value = nullptr;
            return *this;
        }

        constexpr T& index_back(size_t pos) {
            return (pos < _size) ? pair.hold_value[_size - pos - 1] : _prev ? (*_prev).index_back(pos - _size)
                                                                            : throw std::out_of_range("list_array index out of range");
        }

        constexpr T& index_front(size_t pos) {
            return (pos < _size) ? pair.hold_value[pos] : next_ ? (*next_).index_front(pos - _size)
                                                                : throw std::out_of_range("list_array index out of range");
        }

        constexpr const T& index_back(size_t pos) const {
            return (pos < _size) ? pair.hold_value[_size - pos - 1] : _prev ? (*_prev).index_back(pos - _size)
                                                                            : throw std::out_of_range("list_array index out of range");
        }

        constexpr const T& index_front(size_t pos) const {
            return (pos < _size) ? pair.hold_value[pos] : next_ ? (*next_).index_front(pos - _size)
                                                                : throw std::out_of_range("list_array index out of range");
        }

        constexpr iterator<T, Allocator> get_iterator(size_t pos) {
            if (pos < _size)
                return iterator<T, Allocator>(this, pos);
            else if (next_)
                return (*next_).get_iterator(pos - _size);
            else
                return iterator<T, Allocator>(nullptr, 0);
        }

        constexpr iterator<T, Allocator> get_iterator_back(size_t pos) {
            if (pos < _size)
                return iterator<T, Allocator>(this, _size - pos - 1);
            else if (_prev)
                return (*_prev).get_iterator_back(pos - _size);
            else
                return iterator<T, Allocator>(nullptr, 0);
        }

        constexpr const const_iterator<T, Allocator> get_iterator(size_t pos) const {
            if (pos < _size)
                return const_iterator<T, Allocator>(this, pos);
            else if (next_)
                return (*next_).get_iterator(pos - _size);
            else
                return const_iterator<T, Allocator>(nullptr, 0);
        }

        constexpr const const_iterator<T, Allocator> get_iterator_back(size_t pos) const {
            if (pos < _size)
                return const_iterator<T, Allocator>(this, _size - pos - 1);
            else if (_prev)
                return (*_prev).get_iterator_back(pos - _size);
            else
                return const_iterator<T, Allocator>(nullptr, 0);
        }

        constexpr iterator<T, Allocator> begin() {
            if (_prev)
                return _prev->begin();
            else
                return iterator<T, Allocator>(this, 0);
        }

        constexpr iterator<T, Allocator> end() {
            if (next_)
                return next_->end();
            else
                return iterator<T, Allocator>(this, _size);
        }

        constexpr const_iterator<T, Allocator> begin() const {
            if (_prev)
                return _prev->begin();
            else
                return const_iterator<T, Allocator>(this, 0);
        }

        constexpr const_iterator<T, Allocator> end() const {
            if (_prev)
                return _prev->end();
            else
                return const_iterator<T, Allocator>(this, _size);
        }

        constexpr inline size_t size() const {
            return _size;
        }

        constexpr void resize_front(size_t siz) {
            if (!siz) {
                good_bye_world();
                return;
            }
            T* tmp = pair.hold_value;
            pair.hold_value = cxx_resize(pair.hold_value, _size, siz, pair);
            if (pair.hold_value == nullptr) {
                pair.hold_value = tmp;
                throw std::bad_alloc();
            }
            _size = siz;
        }

        constexpr void resize_begin(size_t siz) {
            if (!siz) {
                good_bye_world();
                return;
            }
            T* new_arr = pair.allocate(siz);
            int64_t dif = _size - siz;
            if (dif > 0) {
                if constexpr (std::is_move_constructible_v<T>)
                    for (size_t i = 0; i < siz && i < _size; i++)
                        std::construct_at(new_arr + i, std::move(pair.hold_value[i + dif]));
                else
                    for (size_t i = 0; i < siz && i < _size; i++)
                        std::construct_at(new_arr + i, pair.hold_value[i + dif]);
            } else {
                dif *= -1;
                if constexpr (std::is_move_constructible_v<T>)
                    for (size_t i = 0; i < siz && i < _size; i++)
                        std::construct_at(new_arr + (dif + i), std::move(pair.hold_value[i]));
                else
                    for (size_t i = 0; i < siz && i < _size; i++)
                        std::construct_at(new_arr + (dif + i), pair.hold_value[i]);
            }
            pair.deallocate_destruct(pair.hold_value, _size);
            pair.hold_value = new_arr;
            _size = siz;
        }
    };

    template <class T, class Allocator>
    class dynamic_arr {
        friend class list_array<T, Allocator>;
        friend class iterator<T, Allocator>;
        friend class const_iterator<T, Allocator>;
        friend class reverse_iterator<T, Allocator>;
        friend class const_reverse_iterator<T, Allocator>;
        arr_block<T, Allocator>* arr = nullptr;
        arr_block<T, Allocator>* arr_end = nullptr;
        compressed_allocator_refrence<Allocator, size_t> allocator_a_size;

        constexpr void swap_block_with_blocks(arr_block<T, Allocator>& this_block, arr_block<T, Allocator>& first_block, arr_block<T, Allocator>& second_block) {
            if (this_block._prev)
                this_block._prev->next_ = &first_block;
            if (this_block.next_)
                this_block.next_->_prev = &second_block;

            if (arr == &this_block)
                arr = &first_block;
            if (arr_end == &this_block)
                arr_end = &second_block;

            this_block._prev = this_block.next_ = nullptr;
            this_block.good_bye_world();
        }

        constexpr void remove_item_slow(iterator<T, Allocator> block) {
            size_t pos = block.pos;
            auto& this_block = *block.block;
            if (pos > this_block._size / 2) {
                size_t mov_to = this_block._size - 1;
                for (size_t i = pos; i < mov_to; i++)
                    std::swap(this_block.pair.hold_value[i + 1], this_block.pair.hold_value[i]);
                this_block.resize_front(this_block._size - 1);
            } else {
                for (int64_t i = pos; i > 0; i--)
                    std::swap(this_block.pair.hold_value[i - 1], this_block.pair.hold_value[i]);
                this_block.resize_begin(this_block._size - 1);
            }
        }

        constexpr void remove_item_split(iterator<T, Allocator> block) {
            size_t pos = block.pos;
            auto& this_block = *block.block;
            size_t block_size = this_block._size;
            arr_block<T, Allocator>& first_block = *new arr_block<T, Allocator>(allocator_a_size.get_allocator(), nullptr, pos, nullptr);
            arr_block<T, Allocator>& second_block = *new arr_block<T, Allocator>(allocator_a_size.get_allocator(), &first_block, block_size - pos - 1, nullptr);

            for (size_t i = 0; i < pos; i++)
                first_block.pair.hold_value[i] = this_block.pair.hold_value[i];

            size_t block_half_size = pos + 1;
            for (size_t i = block_half_size; i < block_size; i++)
                second_block.pair.hold_value[i - block_half_size] = this_block.pair.hold_value[i];

            swap_block_with_blocks(this_block, first_block, second_block);
        }

        constexpr void insert_item_slow(iterator<T, Allocator> block, const T& item) {
            size_t pos = block.pos;
            auto& this_block = *block.block;
            if (pos > this_block._size / 2) {
                this_block.resize_front(this_block._size + 1);
                size_t mov_to = this_block._size - 1;
                for (int64_t i = mov_to - 1; i >= pos; i--)
                    std::swap(this_block.pair.hold_value[i + 1], this_block.pair.hold_value[i]);
                this_block.pair.hold_value[pos] = item;
            } else {
                this_block.resize_begin(this_block._size + 1);
                for (int64_t i = 0; i < pos; i++)
                    std::swap(this_block.pair.hold_value[i + 1], this_block.pair.hold_value[i]);
                this_block.pair.hold_value[pos] = item;
            }
        }

        constexpr void insert_item_split(iterator<T, Allocator> block, const T& item) {
            size_t pos = block.pos;
            auto& this_block = *block.block;
            size_t block_size = this_block._size;
            size_t first_size = pos + 1;
            arr_block<T, Allocator>& first_block = *new arr_block<T, Allocator>(allocator_a_size.get_allocator(), nullptr, first_size + 1, nullptr);
            arr_block<T, Allocator>& second_block = *new arr_block<T, Allocator>(allocator_a_size.get_allocator(), &first_block, block_size - first_size, nullptr);

            for (size_t i = 0; i < pos; i++)
                first_block.pair.hold_value[i] = this_block.pair.hold_value[i];
            first_block[first_size] = item;
            for (size_t i = first_size; i < block_size; i++)
                second_block.pair.hold_value[i - first_size] = this_block.pair.hold_value[i];
            swap_block_with_blocks(this_block, first_block, second_block);
        }

        constexpr void insert_item_slow(iterator<T, Allocator> block, T&& item) {
            size_t pos = block.pos;
            auto& this_block = *block.block;
            if (pos > this_block._size / 2) {
                this_block.resize_front(this_block._size + 1);
                size_t mov_to = this_block._size - 1;
                for (int64_t i = mov_to - 1; i >= pos; i--)
                    std::swap(this_block.pair.hold_value[i + 1], this_block.pair.hold_value[i]);
                this_block.pair.hold_value[pos] = std::move(item);
            } else {
                this_block.resize_begin(this_block._size + 1);
                for (int64_t i = 0; i < pos; i++)
                    std::swap(this_block.pair.hold_value[i + 1], this_block.pair.hold_value[i]);
                this_block.pair.hold_value[pos] = std::move(item);
            }
        }

        constexpr void insert_item_split(iterator<T, Allocator> block, T&& item) {
            size_t pos = block.pos;
            auto& this_block = *block.block;
            size_t block_size = this_block._size;
            size_t first_size = pos + 1;
            arr_block<T, Allocator>& first_block = *new arr_block<T, Allocator>(allocator_a_size.get_allocator(), nullptr, first_size + 1, nullptr);
            arr_block<T, Allocator>& second_block = *new arr_block<T, Allocator>(allocator_a_size.get_allocator(), &first_block, block_size - first_size, nullptr);

            for (size_t i = 0; i < pos; i++)
                first_block.pair.hold_value[i] = this_block.pair.hold_value[i];
            first_block[first_size] = std::move(item);
            for (size_t i = first_size; i < block_size; i++)
                second_block.pair.hold_value[i - first_size] = this_block.pair.hold_value[i];
            swap_block_with_blocks(this_block, first_block, second_block);
        }

        constexpr void insert_block_split(iterator<T, Allocator> block, const T* item, size_t item_size) {
            size_t pos = block.pos;
            if (block.block == nullptr)
                throw std::out_of_range("list_array index out of range");
            arr_block<T, Allocator>& this_block = *block.block;
            size_t block_size = this_block._size;
            if (pos == 0) {
                auto insert = new arr_block<T, Allocator>(allocator_a_size.get_allocator(), this_block._prev, item_size, &this_block);
                for (size_t i = 0; i < item_size; i++)
                    insert->pair.hold_value[i] = item[i];
            } else if (pos == block_size) {
                auto insert = new arr_block<T, Allocator>(allocator_a_size.get_allocator(), &this_block, item_size, this_block.next_);
                for (size_t i = 0; i < item_size; i++)
                    insert->pair.hold_value[i] = item[i];
            } else {
                arr_block<T, Allocator>& first_block = *new arr_block<T, Allocator>(allocator_a_size.get_allocator(), nullptr, pos, nullptr);
                arr_block<T, Allocator>& second_block = *new arr_block<T, Allocator>(allocator_a_size.get_allocator(), nullptr, block_size - pos, nullptr);
                for (size_t i = 0; i < pos; i++)
                    first_block.pair.hold_value[i] = this_block.pair.hold_value[i];

                for (size_t i = pos; i < block_size; i++)
                    second_block.pair.hold_value[i - pos] = this_block.pair.hold_value[i];

                arr_block<T, Allocator>& new_block_block = *new arr_block<T, Allocator>(allocator_a_size.get_allocator(), &first_block, item_size, &second_block);
                for (size_t i = 0; i < item_size; i++)
                    new_block_block.pair.hold_value[i] = item[i];
                swap_block_with_blocks(this_block, first_block, second_block);
            }
            allocator_a_size.hold_value += item_size;
        }

        constexpr size_t _remove_items(arr_block<T, Allocator>* block, size_t start, size_t end) {
            size_t size = block->_size;
            size_t new_size = block->_size - (end - start);
            if (new_size == 0) {
                if (arr == block)
                    arr = block->next_;
                if (arr_end == block)
                    arr_end = block->_prev;
                block->good_bye_world();
            } else {
                T* new_arr = allocator_a_size.allocate(new_size);
                T* arr_inter = block->pair.hold_value;
                size_t j = 0;
                for (size_t i = 0; j < new_size && i < size; i++) {
                    if (i == start)
                        i = end;
                    new_arr[j++] = arr_inter[i];
                }
                allocator_a_size.deallocate_destruct(block->pair.hold_value, block->_size);
                block->pair.hold_value = new_arr;
                block->_size = new_size;
            }
            return size - new_size;
        }

        template <class _Fn>
        constexpr size_t _remove_if(iterator<T, Allocator>& block, _Fn&& func, size_t start, size_t end) {
            // value >> 3  ==  value / 8
            size_t size = block.block->_size;
            size_t rem_filt_siz =
                size > end ? (size >> 3) + (size & 7 ? 1 : 0) : (end >> 3) + (end & 7 ? 1 : 0);
            uint8_t* remove_filter = new uint8_t[rem_filt_siz]{0};
            T* arr_inter = block.block->pair.hold_value;
            size_t new_size = size;

            for (size_t i = start; i < end; i++) {
                if (func(arr_inter[i])) {
                    remove_filter[i >> 3] |= 1 << (i & 7);
                    new_size--;
                }
            }
            if (size != new_size) {
                if (new_size == 0 && size == end) {
                    arr_block<T, Allocator>* next_block = block.block->next_;
                    if (arr == block.block)
                        arr = block.block->next_;
                    if (arr_end == block.block)
                        arr_end = block.block->_prev;
                    block.block->good_bye_world();
                    block.block = next_block;
                    block.pos = 0;
                } else {
                    T* new_arr = allocator_a_size.allocate(new_size);
                    size_t j = 0;
                    for (size_t i = 0; j < new_size && i < end; i++)
                        if (!(remove_filter[i >> 3] & (1 << (i & 7))))
                            new_arr[j++] = arr_inter[i];

                    allocator_a_size.deallocate_destruct(block.block->pair.hold_value, block.block->_size);
                    block.block->pair.hold_value = new_arr;
                    block.block->_size = new_size;
                }
            }
            delete[] remove_filter;
            return size - new_size;
        }

    public:
        constexpr void clear() {
            arr_block<T, Allocator>* blocks = arr;
            arr_block<T, Allocator>* this_block;
            while (blocks != nullptr) {
                this_block = blocks;
                blocks = blocks->next_;
                this_block->_prev = nullptr;
                this_block->next_ = nullptr;
                delete this_block;
            }
            allocator_a_size.hold_value = 0;
            arr = arr_end = nullptr;
        }

        constexpr dynamic_arr(Allocator& alloc)
            : allocator_a_size(alloc, 0) {}

        constexpr dynamic_arr(const dynamic_arr& copy, Allocator& alloc)
            : allocator_a_size(alloc, 0) {
            operator=(copy);
        }

        constexpr dynamic_arr(dynamic_arr&& move, Allocator& alloc) noexcept
            : allocator_a_size(alloc, 0) {
            arr = move.arr;
            arr_end = move.arr_end;
            allocator_a_size.hold_value = move.allocator_a_size.hold_value;

            move.arr = move.arr_end = nullptr;
            move.allocator_a_size.hold_value = 0;
        }

        constexpr dynamic_arr& operator=(dynamic_arr&& move) noexcept(false) {
            if (this == &move)
                return *this;
            clear();

            arr = move.arr;
            arr_end = move.arr_end;
            allocator_a_size.hold_value = move.allocator_a_size.hold_value;

            move.arr = move.arr_end = nullptr;
            move.allocator_a_size.hold_value = 0;
            return *this;
        }

        constexpr dynamic_arr& operator=(const dynamic_arr& copy) {
            if (this == &copy)
                return *this;
            clear();
            if (!copy.allocator_a_size.hold_value)
                return *this;
            T* tmp = (arr = arr_end = new arr_block<T, Allocator>(allocator_a_size.get_allocator(), nullptr, allocator_a_size.hold_value = copy.allocator_a_size.hold_value, nullptr))->pair.hold_value;
            size_t i = 0;
            for (auto& it : copy)
                tmp[i++] = it;
            return *this;
        }

        constexpr ~dynamic_arr() {
            clear();
        }

        constexpr T& operator[](size_t pos) {
            return (pos < (allocator_a_size.hold_value >> 1)) ? arr->operator[](pos) : arr_end->index_back(allocator_a_size.hold_value - pos - 1);
        }

        constexpr const T& operator[](size_t pos) const {
            return (pos < (allocator_a_size.hold_value >> 1)) ? arr->operator[](pos) : arr_end->index_back(allocator_a_size.hold_value - pos - 1);
        }

        constexpr T& index_back(size_t pos) {
            return arr_end->index_back(allocator_a_size.hold_value - pos - 1);
        }

        constexpr const T& index_back(size_t pos) const {
            return arr_end->index_back(allocator_a_size.hold_value - pos - 1);
        }

        constexpr iterator<T, Allocator> get_iterator(size_t pos) {
            if (!allocator_a_size.hold_value)
                return iterator<T, Allocator>(nullptr, 0);
            if (pos < (allocator_a_size.hold_value >> 1))
                return arr->get_iterator(pos);
            else {
                if (allocator_a_size.hold_value - pos - 1 == size_t(-1))
                    return iterator<T, Allocator>(arr_end, arr_end ? arr_end->_size : 0);
                else
                    return arr_end->get_iterator_back(allocator_a_size.hold_value - pos - 1);
            }
        }

        constexpr const_iterator<T, Allocator> get_iterator(size_t pos) const {
            if (!allocator_a_size.hold_value)
                return iterator<T, Allocator>(nullptr, 0);
            if (pos < (allocator_a_size.hold_value >> 1))
                return arr->get_iterator(pos);
            else {
                if (allocator_a_size.hold_value - pos - 1 == size_t(-1))
                    return iterator<T, Allocator>(arr_end, arr_end ? arr_end->_size : 0);
                else
                    return arr_end->get_iterator_back(allocator_a_size.hold_value - pos - 1);
            }
        }

        constexpr auto begin() {
            return arr->begin();
        }

        constexpr auto end() {
            return arr_end->end();
        }

        constexpr auto begin() const {
            return arr->begin();
        }

        constexpr auto end() const {
            return arr_end->end();
        }

        constexpr size_t size() const {
            return allocator_a_size.hold_value;
        }

        constexpr void resize_begin(size_t new_size) {
            size_t tsize = allocator_a_size.hold_value;
            if (tsize >= new_size) {
                if (!arr_end) {
                    if (arr) {
                        arr_end = arr;
                        while (arr_end->_prev)
                            arr_end = arr_end->_prev;
                    } else {
                        arr = arr_end = new arr_block<T, Allocator>(allocator_a_size.get_allocator(), nullptr, new_size, nullptr);
                        return;
                    }
                }
                for (size_t resize_to = tsize - new_size; resize_to > 0;) {
                    if (arr->_size > resize_to) {
                        allocator_a_size.hold_value = new_size;
                        arr->resize_begin(arr->_size - resize_to);
                        return;
                    } else {
                        resize_to -= arr->_size;
                        if (auto tmp = arr->next_) {
                            arr->next_->_prev = nullptr;
                            arr->next_ = nullptr;
                            delete arr;
                            arr = tmp;
                        } else {
                            delete arr;
                            arr_end = nullptr;
                            arr = nullptr;
                            allocator_a_size.hold_value = 0;
                            return;
                        }
                    }
                }
            } else {
                if (arr)
                    if (arr->_size + new_size <= allocator_a_size.hold_value >> 1) {
                        arr->resize_begin(new_size - tsize + arr->_size);
                        if (!arr_end)
                            arr_end = arr;
                        allocator_a_size.hold_value = new_size;
                        return;
                    }
                arr = new arr_block<T, Allocator>(allocator_a_size.get_allocator(), nullptr, new_size - tsize, arr);
                if (!arr_end)
                    arr_end = arr;
            }
            allocator_a_size.hold_value = new_size;
        }

        constexpr void resize_front(size_t new_size) {
            size_t tsize = allocator_a_size.hold_value;
            if (tsize >= new_size) {
                if (!arr_end) {
                    if (arr) {
                        arr_end = arr;
                        while (arr_end->_prev)
                            arr_end = arr_end->_prev;
                    } else {
                        arr = arr_end = new arr_block<T, Allocator>(allocator_a_size.get_allocator(), nullptr, new_size, nullptr);
                        return;
                    }
                }
                for (size_t resize_to = tsize - new_size; resize_to > 0;) {
                    if (arr_end->_size > resize_to) {
                        allocator_a_size.hold_value = new_size;
                        arr_end->resize_front(arr_end->_size - resize_to);
                        return;
                    } else {
                        resize_to -= arr_end->_size;
                        if (auto tmp = arr_end->_prev) {
                            arr_end->_prev->next_ = nullptr;
                            arr_end->_prev = nullptr;
                            delete arr_end;
                            arr_end = tmp;
                        } else {
                            delete arr_end;
                            arr_end = nullptr;
                            arr = nullptr;
                            allocator_a_size.hold_value = 0;
                            return;
                        }
                    }
                }
            } else {
                if (arr_end)
                    if (arr_end->_size + new_size <= allocator_a_size.hold_value >> 1) {
                        arr_end->resize_front(new_size - tsize + arr_end->_size);
                        if (!arr)
                            arr = arr_end;
                        allocator_a_size.hold_value = new_size;
                        return;
                    }
                arr_end = new arr_block<T, Allocator>(allocator_a_size.get_allocator(), arr_end, new_size - tsize, nullptr);
                if (!arr)
                    arr = arr_end;
            }
            allocator_a_size.hold_value = new_size;
        }

        constexpr void insert_block(size_t pos, const T* item, size_t item_size) {
            if (pos == allocator_a_size.hold_value) {
                resize_front(allocator_a_size.hold_value + item_size);
                auto iterator = get_iterator(allocator_a_size.hold_value - item_size);
                for (size_t i = 0; i < item_size; i++) {
                    *iterator = item[i];
                    ++iterator;
                }
            } else if (pos == 0) {
                resize_begin(allocator_a_size.hold_value + item_size);
                auto iterator = get_iterator(0);
                for (size_t i = 0; i < item_size; i++) {
                    *iterator = item[i];
                    ++iterator;
                }
            } else
                insert_block_split(get_iterator(pos), item, item_size);
        }

        constexpr void insert_block(size_t pos, const arr_block<T, Allocator>& item) {
            insert_block_split(get_iterator(pos), item.pair.hold_value, item._size);
        }

        constexpr void insert(size_t pos, const T& item) {
            if (!allocator_a_size.hold_value) {
                resize_front(1);
                operator[](0) = item;
                return;
            }
            iterator<T, Allocator> inter = get_iterator(pos);
            arr_block<T, Allocator>& this_block = *inter.block;
            if (inter.pos == 0) {
                this_block.resize_begin(this_block._size + 1);
                this_block[0] = item;
            } else if (inter.pos == this_block._size - 1) {
                this_block.resize_front(this_block._size + 1);
                this_block[this_block._size - 1] = item;
            } else if (this_block._size <= 50000)
                insert_item_slow(inter, item);
            else
                insert_item_split(inter, item);
            allocator_a_size.hold_value++;
        }

        constexpr void insert(size_t pos, T&& item) {
            if (!allocator_a_size.hold_value) {
                resize_front(1);
                operator[](0) = std::move(item);
                return;
            }
            iterator<T, Allocator> inter = get_iterator(pos);
            arr_block<T, Allocator>& this_block = *inter.block;
            if (inter.pos == 0) {
                this_block.resize_begin(this_block._size + 1);
                this_block[0] = std::move(item);
            } else if (inter.pos == this_block._size - 1) {
                this_block.resize_front(this_block._size + 1);
                this_block[this_block._size - 1] = std::move(item);
            } else if (this_block._size <= 50000)
                insert_item_slow(inter, std::move(item));
            else
                insert_item_split(inter, std::move(item));
            allocator_a_size.hold_value++;
        }

        constexpr void remove_item(size_t pos) {
            if (!allocator_a_size.hold_value)
                return;
            iterator<T, Allocator> inter = get_iterator(pos);
            arr_block<T, Allocator>& this_block = *inter.block;
            if (inter.pos == 0) {
                if (this_block._size == 1) {
                    if (arr == &this_block)
                        arr = this_block.next_;
                    if (arr_end == &this_block)
                        arr_end = this_block._prev;
                    this_block.good_bye_world();
                } else
                    this_block.resize_begin(this_block._size - 1);
            } else if (inter.pos == this_block._size - 1)
                this_block.resize_front(this_block._size - 1);
            else if (this_block._size <= 50000)
                remove_item_slow(inter);
            else
                remove_item_split(inter);
            allocator_a_size.hold_value--;
        }

        constexpr size_t remove_items(size_t start_pos, size_t end_pos) {
            iterator<T, Allocator> iterate = get_iterator(start_pos);
            iterator<T, Allocator> _end = get_iterator(end_pos);
            size_t removed = 0;
            size_t to_remove = end_pos - start_pos;
            if (iterate.block == _end.block) {
                removed = _remove_items(iterate.block, iterate.pos, _end.pos);
                allocator_a_size.hold_value -= removed;
                return removed;
            }
            arr_block<T, Allocator>* curr_block;
            for (; removed < to_remove;) {
                curr_block = iterate.block;
                if (iterate.pos != 0 || iterate.block->_size > to_remove - removed) {
                    removed += _remove_items(iterate.block, iterate.pos, iterate.block->_size - iterate.pos);
                } else {
                    iterate._nextBlock();
                    removed += curr_block->_size;
                    if (arr == curr_block)
                        arr = curr_block->next_;
                    if (arr_end == curr_block)
                        arr_end = curr_block->_prev;
                    curr_block->good_bye_world();
                }
            }
            allocator_a_size.hold_value -= removed;
            return removed;
        }

        template <class _Fn>
        constexpr size_t remove_if(size_t start, size_t end, _Fn&& func) {
            if (!allocator_a_size.hold_value)
                return 0;
            iterator<T, Allocator> iterate = get_iterator(start);
            iterator<T, Allocator> _end = get_iterator(end);
            size_t removed = 0;
            if (iterate.block == _end.block) {
                removed = _remove_if(iterate, func, iterate.pos, _end.pos);
                allocator_a_size.hold_value -= removed;
                return removed;
            }

            removed += _remove_if(iterate, func, iterate.pos, iterate.block->_size);
            for (;;) {
                if (!iterate._nextBlock())
                    break;
                removed += _remove_if(iterate, func, 0, iterate.block->_size);
            }

            allocator_a_size.hold_value -= removed;
            return removed;
        }

        constexpr void swap(dynamic_arr<T, Allocator>& to_swap) noexcept {
            arr_block<T, Allocator>* old_arr = arr;
            arr_block<T, Allocator>* old_arr_end = arr_end;
            size_t old__size = allocator_a_size.hold_value;
            arr = to_swap.arr;
            arr_end = to_swap.arr_end;
            allocator_a_size.hold_value = to_swap._size;
            to_swap.arr = old_arr;
            to_swap.arr_end = old_arr_end;
            to_swap._size = old__size;
        }
    };

    template <class T, class Allocator>
    class list_array {
        compressed_allocator<Allocator, dynamic_arr<T, Allocator>> pair;
        size_t reserved_begin = 0;
        size_t _size = 0;
        size_t reserved_end = 0;

        constexpr void steal_block_begin() {
            arr_block<T, Allocator>* move_block = pair.hold_value.arr;
            reserved_begin -= move_block->_size;
            reserved_end += move_block->_size;


            pair.hold_value.arr = move_block->next_;

            move_block->next_ = nullptr;
            move_block->_prev = pair.hold_value.arr_end;

            if (pair.hold_value.arr_end->_prev == move_block)
                pair.hold_value.arr->next_ = move_block;
            pair.hold_value.arr_end = move_block;
            pair.hold_value.arr->_prev = nullptr;
        }

        constexpr void steal_block_end() {
            arr_block<T, Allocator>* move_block = pair.hold_value.arr_end;
            reserved_end -= move_block->_size;
            reserved_begin += move_block->_size;

            pair.hold_value.arr_end = move_block->_prev;
            pair.hold_value.arr_end->next_ = nullptr;

            move_block->_prev = nullptr;
            move_block->next_ = pair.hold_value.arr;

            if (pair.hold_value.arr->next_ == move_block)
                pair.hold_value.arr_end->_prev = move_block;
            pair.hold_value.arr = move_block;
            pair.hold_value.arr_end->_prev = nullptr;
        }

    public:
        using iterator = __list_array_impl::iterator<T, Allocator>;
        using const_iterator = __list_array_impl::const_iterator<T, Allocator>;
        using reverse_iterator = __list_array_impl::reverse_iterator<T, Allocator>;
        using const_reverse_iterator = __list_array_impl::const_reverse_iterator<T, Allocator>;
        using value_type = T;
        using reference = T&;
        using const_reference = const T&;
        using size_type = size_t;
        using difference_type = ptrdiff_t;
        static inline constexpr const size_t npos = -1;

        class range_provider {
            size_t _start;
            size_t _end;
            list_array<T, Allocator>& ln;

        public:
            constexpr range_provider(list_array<T, Allocator>& link, size_t start, size_t end)
                : ln(link) {
                _start = start;
                _end = end;
            }

            constexpr range_provider(const range_provider& copy)
                : ln(copy.ln) {
                _start = copy._start;
                _end = copy._end;
            }

            constexpr range_provider& operator=(const range_provider& copy) {
                reinterpret_cast<list_array<T, Allocator>*&>(&ln) = reinterpret_cast<list_array<T, Allocator>*&>(&copy.ln);
                _start = copy._start;
                _end = copy._end;
                return *this;
            }

            constexpr iterator begin() {
                return ln.get_iterator(_start);
            }

            constexpr iterator end() {
                return ln.get_iterator(_end);
            }

            constexpr const_iterator begin() const {
                return ln.get_iterator(_start);
            }

            constexpr const_iterator end() const {
                return ln.get_iterator(_end);
            }

            constexpr reverse_iterator rbegin() {
                return ln.get_iterator(_end);
            }

            constexpr reverse_iterator rend() {
                return ln.get_iterator(_start);
            }

            constexpr const_reverse_iterator rbegin() const {
                return ln.get_iterator(_end);
            }

            constexpr const_reverse_iterator rend() const {
                return ln.get_iterator(_start);
            }

            constexpr size_t range_start() const {
                return _start;
            }

            constexpr size_t range_end() const {
                return _end;
            }
        };

        class const_range_provider {
            size_t _start;
            size_t _end;
            const list_array<T, Allocator>& ln;

        public:
            constexpr const_range_provider(const list_array<T, Allocator>& link, size_t start, size_t end)
                : ln(link) {
                _start = start;
                _end = end;
            }

            constexpr const_range_provider(const const_range_provider& copy)
                : ln(copy.ln) {
                _start = copy._start;
                _end = copy._end;
            }

            constexpr const_range_provider& operator=(const const_range_provider& copy) {
                reinterpret_cast<list_array<T, Allocator>*&>(&ln) = reinterpret_cast<list_array<T, Allocator>*&>(&copy.ln);
                _start = copy._start;
                _end = copy._end;
                return *this;
            }

            constexpr const_iterator begin() const {
                return ln.get_iterator(_start);
            }

            constexpr const_iterator end() const {
                return ln.get_iterator(_end);
            }

            constexpr const_reverse_iterator rbegin() const {
                return ln.get_iterator(_end);
            }

            constexpr const_reverse_iterator rend() const {
                return ln.get_iterator(_start);
            }
        };

        class reverse_provider {
            reverse_iterator _begin;
            reverse_iterator _end;

        public:
            constexpr reverse_provider(range_provider& link) {
                _begin = link.rbegin();
                _end = link.rend();
            }

            constexpr reverse_provider(range_provider&& link) {
                _begin = link.rbegin();
                _end = link.rend();
            }

            constexpr reverse_provider(list_array<T, Allocator>& link) {
                _begin = link.rbegin();
                _end = link.rend();
            }

            constexpr reverse_provider(const reverse_provider& copy) {
                _begin = copy._begin;
                _end = copy._end;
            }

            constexpr reverse_provider& operator=(const reverse_provider& copy) {
                _begin = copy._start;
                _end = copy._end;
                return *this;
            }

            constexpr reverse_iterator begin() {
                return _begin;
            }

            constexpr reverse_iterator end() {
                return _end;
            }

            constexpr const_reverse_iterator begin() const {
                return _begin;
            }

            constexpr const_reverse_iterator end() const {
                return _end;
            }
        };

        class const_reverse_provider {
            const_reverse_iterator _begin;
            const_reverse_iterator _end;

        public:
            constexpr const_reverse_provider(const range_provider& link) {
                _begin = link.rbegin();
                _end = link.rend();
            }

            constexpr const_reverse_provider(const range_provider&& link) {
                _begin = link.rbegin();
                _end = link.rend();
            }

            constexpr const_reverse_provider(const const_range_provider& link) {
                _begin = link.rbegin();
                _end = link.rend();
            }

            constexpr const_reverse_provider(const const_range_provider&& link) {
                _begin = link.rbegin();
                _end = link.rend();
            }

            constexpr const_reverse_provider(const list_array<T, Allocator>& link) {
                _begin = link.rbegin();
                _end = link.rend();
            }

            constexpr const_reverse_provider(const const_reverse_provider& copy) {
                _begin = copy._begin;
                _end = copy._end;
            }

            constexpr const_reverse_provider& operator=(const const_reverse_provider& copy) {
                _begin = copy._start;
                _end = copy._end;
                return *this;
            }

            constexpr const_reverse_iterator begin() const {
                return _begin;
            }

            constexpr const_reverse_iterator end() const {
                return _end;
            }
        };

#pragma region constructors

        constexpr list_array(const Allocator& allocator = Allocator())
            : pair(allocator, pair.get_allocator()) {}

        template <class Container>
        constexpr list_array(Container&& cont, const Allocator& allocator = Allocator())
            requires(is_container<Container>::value)
            : pair(allocator, pair.get_allocator()) {
            if constexpr (can_direct_index<Container>::value) {
                resize(cont.size());
                auto it = cont.data();
                for (size_t i = 0; i < _size; i++)
                    pair.hold_value[i] = std::move(it[i]);
            } else {
                reserve(cont.size());
                size_t i = 0;
                for (const T& it : cont)
                    push_back(std::move(it));
            }
        }

        template <class Container>
        constexpr list_array(const Container& cont, const Allocator& allocator = Allocator())
            requires(is_container<Container>::value)
            : pair(allocator, pair.get_allocator()) {
            if constexpr (can_direct_index<Container>::value) {
                resize(cont.size());
                auto it = cont.data();
                for (size_t i = 0; i < _size; i++)
                    pair.hold_value[i] = it[i];
            } else {
                reserve(cont.size());
                size_t i = 0;
                for (const T& it : cont)
                    push_back(it);
            }
        }

        constexpr list_array(std::initializer_list<T> vals, const Allocator& allocator = Allocator())
            : pair(allocator, pair.get_allocator()) {
            resize(vals.size());
            auto iter = begin();
            for (const T& it : vals) {
                *iter = it;
                ++iter;
            }
        }

        template <class AnotherT>
        constexpr list_array(std::initializer_list<AnotherT> vals, const Allocator& allocator = Allocator())
            : pair(allocator, pair.get_allocator()) {
            for (const AnotherT& it : vals)
                push_back(it);
        }

        template <size_t arr_size>
        constexpr list_array(const T (&arr)[arr_size], const Allocator& allocator = Allocator())
            : pair(allocator, pair.get_allocator()) {
            push_back(arr, arr_size);
        }

        constexpr list_array(const T* arr, size_t arr_size, const Allocator& allocator = Allocator())
            : pair(allocator, pair.get_allocator()) {
            push_back(arr, arr_size);
        }

        template <typename Iterable>
        constexpr list_array(Iterable begin, Iterable end, size_t reserve_len = 0, const Allocator& allocator = Allocator())
            : pair(allocator, pair.get_allocator()) {
            if constexpr (std::is_pointer<Iterable>::value) {
                size_t len = end - begin;
                if (len < reserve_len)
                    len = reserve_len;
                if (len == 0)
                    return;
                reserve_push_back(len);
            } else if (reserve_len)
                reserve_push_back(reserve_len);
            while (begin != end)
                push_back(*begin++);
        }

        constexpr list_array(size_t size, const Allocator& allocator = Allocator())
            : pair(allocator, pair.get_allocator()) {
            resize(size);
        }

        constexpr list_array(size_t size, const T& default_init, const Allocator& allocator = Allocator())
            : pair(allocator, pair.get_allocator()) {
            resize(size, default_init);
        }

        constexpr list_array(list_array&& move) noexcept
            : pair(move.pair.get_allocator(), pair.get_allocator()) {
            operator=(std::move(move));
        }

        constexpr list_array(const list_array& copy)
            : pair(copy.pair.get_allocator(), pair.get_allocator()) {
            operator=(copy);
        }

        constexpr list_array(const list_array& copy, const Allocator& allocator)
            : pair(allocator, pair.get_allocator()) {
            operator=(copy);
        }

        constexpr list_array(const list_array& copy, size_t start, const Allocator& allocator = Allocator())
            : list_array(copy.get_iterator(start), copy.end(), copy.size() - start, allocator) {}

        constexpr list_array(const list_array& copy, size_t start, size_t end, const Allocator& allocator = Allocator())
            : list_array(copy.get_iterator(start), copy.get_iterator(end), end - start, allocator) {}

#pragma endregion
#pragma region operators

        constexpr list_array& operator=(list_array&& move) noexcept {
            pair = std::move(move.pair);
            reserved_begin = move.reserved_begin;
            _size = move._size;
            reserved_end = move.reserved_end;
            return *this;
        }

        constexpr list_array& operator=(const list_array& copy) {
            pair = copy.pair;
            reserved_begin = copy.reserved_begin;
            _size = copy._size;
            reserved_end = copy.reserved_end;
            return *this;
        }

        template <class AnyAllocator>
        constexpr bool operator==(const list_array<T, AnyAllocator>& to_cmp) const {
            if (pair.hold_value.arr != to_cmp.pair.hold_value.arr) {
                if (_size != to_cmp._size)
                    return false;
                auto iter = to_cmp.begin();
                for (const T& it : *this) {
                    if (*iter != it)
                        return false;
                    ++iter;
                }
            }
            return true;
        }

        template <class AnyAllocator>
        constexpr bool operator!=(const list_array<T, AnyAllocator>& to_cmp) const {
            return !operator==(to_cmp);
        }

#pragma endregion
#pragma region list operations

        constexpr void push_front(const T& copy_to) {
            if (reserved_begin) {
                pair.hold_value[--reserved_begin] = copy_to;
                _size++;
                return;
            } else if (pair.hold_value.arr_end) {
                if (pair.hold_value.arr_end->_size <= reserved_end) {
                    steal_block_begin();
                    push_back(copy_to);
                    return;
                }
            }
            reserve_push_front(_size + 1);
            push_front(copy_to);
        }

        constexpr void push_front(T&& copy_to) {
            if (reserved_begin) {
                pair.hold_value[--reserved_begin] = copy_to;
                _size++;
                return;
            } else if (pair.hold_value.arr_end) {
                if (pair.hold_value.arr_end->_size <= reserved_end) {
                    steal_block_begin();
                    push_back(std::move(copy_to));
                    return;
                }
            }
            reserve_push_front(_size + 1);
            push_front(std::move(copy_to));
        }

        constexpr void push_back(const T& copy_to) {
            if (reserved_end) {
                pair.hold_value[reserved_begin + _size++] = copy_to;
                --reserved_end;
                return;
            } else if (pair.hold_value.arr) {
                if (pair.hold_value.arr->_size <= reserved_begin) {
                    steal_block_begin();
                    push_back(copy_to);
                    return;
                }
            }
            reserve_push_back(_size + 1);
            push_back(copy_to);
        }

        constexpr void push_back(T&& copy_to) {
            if (reserved_end) {
                pair.hold_value[reserved_begin + _size++] = std::move(copy_to);
                --reserved_end;
                return;
            } else if (pair.hold_value.arr) {
                if (pair.hold_value.arr->_size <= reserved_begin) {
                    steal_block_begin();
                    push_back(std::move(copy_to));
                    return;
                }
            }
            reserve_push_back(_size + 1);
            push_back(std::move(copy_to));
        }

        constexpr void pop_back() {
            if (_size) {
                ++reserved_end;
                --_size;
            } else
                throw std::out_of_range("This list_array is empty");
        }

        constexpr void pop_front() {
            if (_size) {
                ++reserved_begin;
                --_size;
            } else
                throw std::out_of_range("This list_array is empty");
        }

        constexpr T take_back() {
            if (_size) {
                T tmp(std::move(operator[](_size - 1)));
                pop_back();
                return tmp;
            } else
                throw std::out_of_range("This list_array is empty");
        }

        constexpr T take_front() {
            if (_size) {
                T tmp(std::move(operator[](0)));
                pop_front();
                return tmp;
            } else
                throw std::out_of_range("This list_array is empty");
        }

        constexpr T& back() {
            return operator[](_size - 1);
        }

        constexpr T& front() {
            return operator[](0);
        }

        constexpr const T& back() const {
            return operator[](_size - 1);
        }

        constexpr const T& front() const {
            return operator[](0);
        }

        template <size_t arr_size>
        constexpr void push_front(const T (&array)[arr_size]) {
            push_front(array, arr_size);
        }

        template <size_t arr_size>
        constexpr void push_back(const T (&array)[arr_size]) {
            push_back(array, arr_size);
        }

        constexpr void push_front(const T* array, size_t arr_size) {
            insert(0, array, arr_size);
        }

        constexpr void push_back(const T* array, size_t arr_size) {
            insert(_size, array, arr_size);
        }

        template <class AnyAllocator>
        constexpr void push_front(const list_array<T, AnyAllocator>& to_push) {
            insert(0, to_push);
        }

        template <class AnyAllocator>
        constexpr void push_back(const list_array<T, AnyAllocator>& to_push) {
            insert(_size, to_push);
        }

#pragma endregion
#pragma region insert

        template <size_t arr_size>
        constexpr void insert(size_t pos, const T (&item)[arr_size]) {
            insert(pos, item, arr_size);
        }

        constexpr void insert(size_t pos, const T* item, size_t arr_size) {
            if (!arr_size)
                return;
            pair.hold_value.insert_block(reserved_begin + pos, item, arr_size);
            _size += arr_size;
        }

        template <class AnyAllocator>
        constexpr void insert(size_t pos, const list_array<T, AnyAllocator>& item) {
            if (!item._size)
                return;
            if (item.blocks_more(1)) {
                T* as_array = item.to_array<Allocator>(pair.get_allocator());
                pair.hold_value.insert_block(reserved_begin + pos, as_array, item._size);
                pair.deallocate_destruct(as_array, item._size);
            } else {
                pair.hold_value.insert_block(reserved_begin + pos, item.data(), item._size);
            }
            _size += item._size;
        }

        template <class AnyAllocator>
        constexpr void insert(size_t pos, const list_array<T, AnyAllocator>& item, size_t start, size_t end) {
            auto item_range = item.range(start, end);
            insert(pos, list_array<T, Allocator>(item_range.begin(), item_range.end()));
        }

        constexpr void insert(size_t pos, const T& item) {
            if (pos == _size)
                return push_back(item);
            pair.hold_value.insert(reserved_begin + pos, item);
            _size++;
        }

        constexpr void insert(size_t pos, T&& item) {
            if (pos == _size)
                return push_back(std::move(item));
            pair.hold_value.insert(reserved_begin + pos, std::move(item));
            _size++;
        }

#pragma endregion
#pragma region ordered

        constexpr void ordered_insert(const T& item) {
            size_t i = 0;
            for (auto& it : *this) {
                if (it > item)
                    break;
                i++;
            }
            insert(i, item);
        }

        constexpr void ordered_insert(T&& item) {
            size_t i = 0;
            for (auto& it : *this) {
                if (it > item)
                    break;
                i++;
            }
            insert(i, std::move(item));
        }

        template <class _Fn>
        constexpr void ordered_insert(const T& item, _Fn&& order_checker) {
            size_t i = 0;
            for (auto& it : *this) {
                if (order_checker(it, item))
                    break;
                i++;
            }
            insert(i, item);
        }

        template <class _Fn>
        constexpr void ordered_insert(T&& item, _Fn&& order_checker) {
            size_t i = 0;
            for (auto& it : *this) {
                if (order_checker(it, item))
                    break;
                i++;
            }
            insert(i, std::move(item));
        }

#pragma endregion
#pragma region contains

        constexpr bool contains(const T& value, size_t start = 0) const
            requires(std::equality_comparable<T>)
        {
            return contains(value, start, _size);
        }

        constexpr bool contains(const T& value, size_t start, size_t end) const
            requires(std::equality_comparable<T>)
        {
            return find(value, start, end) != npos;
        }

        template <size_t arr_size>
        constexpr bool contains(const T (&arr)[arr_size], size_t start = 0) const
            requires(std::equality_comparable<T>)
        {
            return contains(arr, arr_size, start, _size);
        }

        template <size_t arr_size>
        constexpr bool contains(const T (&arr)[arr_size], size_t start, size_t end) const
            requires(std::equality_comparable<T>)
        {
            return contains(arr, arr_size, start, end);
        }

        constexpr bool contains(const T* arr, size_t arr_size, size_t start = 0) const
            requires(std::equality_comparable<T>)
        {
            return contains(arr, arr_size, start, _size);
        }

        constexpr bool contains(const T* arr, size_t arr_size, size_t start, size_t end) const
            requires(std::equality_comparable<T>)
        {
            return find(arr, arr_size, start, end) != npos;
        }

        template <class AnyAllocator>
        constexpr bool contains(const list_array<T, AnyAllocator>& value, size_t start = 0) const
            requires(std::equality_comparable<T>)
        {
            return contains(value, 0, value._size, start, _size);
        }

        template <class AnyAllocator>
        constexpr bool contains(const list_array<T, AnyAllocator>& value, size_t start, size_t end) const
            requires(std::equality_comparable<T>)
        {
            return contains(value, 0, value._size, start, end);
        }

        template <class AnyAllocator>
        constexpr bool contains(const list_array<T, AnyAllocator>& value, size_t value_start, size_t value_end, size_t start, size_t end) const
            requires(std::equality_comparable<T>)
        {
            return find(value, value_start, value_end, start, end) != npos;
        }

        template <class _Fn>
        constexpr bool contains_one(_Fn&& check_function) const {
            return contains_one(0, _size, check_function);
        }

        template <class _Fn>
        constexpr size_t contains_multiply(_Fn&& check_function) const {
            return contains_multiply(0, _size, check_function);
        }

        template <class _Fn>
        constexpr bool contains_one(size_t start, _Fn&& check_function) const {
            return contains_one(start, _size, check_function);
        }

        template <class _Fn>
        constexpr size_t contains_multiply(size_t start, _Fn&& check_function) const {
            return contains_multiply(start, _size, check_function);
        }

        template <class _Fn>
        constexpr bool contains_one(size_t start, size_t end, _Fn&& check_function) const {
            for (const T& it : range(start, end))
                if (check_function(it))
                    return true;
            return false;
        }

        template <class _Fn>
        constexpr size_t contains_multiply(size_t start, size_t end, _Fn&& check_function) const {
            size_t i = 0;
            for (const T& it : range(start, end))
                if (check_function(it))
                    ++i;
            return i;
        }

#pragma endregion
#pragma region remove

        constexpr void remove(size_t pos) {
            if (pos >= _size)
                throw std::out_of_range("pos value out of size limit");
            pair.hold_value.remove_item(reserved_begin + pos);
            _size--;
            if (_size == 0)
                clear();
        }

        constexpr void remove(size_t start_pos, size_t end_pos) {
            if (start_pos > end_pos)
                std::swap(start_pos, end_pos);
            if (end_pos > _size)
                throw std::out_of_range("end_pos value out of size limit");
            if (start_pos != end_pos)
                _size -= pair.hold_value.remove_items(reserved_begin + start_pos, reserved_begin + end_pos);
        }

#pragma endregion
#pragma region remove_if

        template <class _Fn>
        constexpr size_t remove_if(_Fn&& check_function) {
            return remove_if(0, _size, check_function);
        }

        template <class _Fn>
        constexpr size_t remove_if(size_t start, _Fn&& check_function) {
            return remove_if(start, _size, check_function);
        }

        template <class _Fn>
        constexpr size_t remove_if(size_t start, size_t end, _Fn&& check_function) {
            if (start > end)
                std::swap(start, end);
            if (end > _size)
                throw std::out_of_range("end value out of size limit");
            if (start > _size)
                throw std::out_of_range("start value out of size limit");
            size_t res = pair.hold_value.remove_if(reserved_begin + start, reserved_begin + end, check_function);
            _size -= res;
            return res;
        }

#pragma endregion
#pragma region remove_one

        template <class _Fn>
        constexpr bool remove_one(_Fn&& check_function) {
            return remove_one(0, _size, check_function);
        }

        template <class _Fn>
        constexpr bool remove_one(size_t start, _Fn&& check_function) {
            return remove_one(start, _size, check_function);
        }

        template <class _Fn>
        constexpr bool remove_one(size_t start, size_t end, _Fn&& check_function) {
            size_t item = find_it(start, end, check_function);
            if (item == npos)
                return false;
            remove(item);
            return true;
        }

#pragma endregion
#pragma region remove_same

        template <class _Fn>
        constexpr size_t remove_same(
            const T& val,
            size_t start,
            size_t end,
            _Fn&& comparer = [](const T& f, const T& s) { return f == s; }
        ) {
            if (end > _size)
                throw std::out_of_range("end value out of size limit");
            if (start > _size)
                throw std::out_of_range("start value out of size limit");
            if (start > end)
                std::swap(start, end);
            size_t res = pair.hold_value.remove_if(reserved_begin + start, reserved_begin + end, [&comparer, &val](const T& cval) { return comparer(val, cval); });
            _size -= res;
            return res;
        }

        template <class _Fn>
        constexpr size_t remove_same(
            const T& val,
            size_t start,
            _Fn&& comparer = [](const T& f, const T& s) { return f == s; }
        ) {
            return remove_same(val, start, _size, comparer);
        }

        template <class _Fn>
        constexpr size_t remove_same(
            const T& val,
            _Fn&& comparer = [](const T& f, const T& s) { return f == s; }
        ) {
            size_t res = pair.hold_value.remove_if(reserved_begin, reserved_begin + _size, [&comparer, &val](const T& cval) { return comparer(val, cval); });
            _size -= res;
            return res;
        }

        template <size_t arr_size>
        constexpr size_t remove_same(const T (&val)[arr_size], size_t start = 0) {
            return remove_same(val, arr_size, start, _size);
        }

        template <size_t arr_size>
        constexpr size_t remove_same(const T (&val)[arr_size], size_t start, size_t end) {
            return remove_same(val, arr_size, start, end);
        }

        constexpr size_t remove_same(const T* val, size_t arr_size, size_t start = 0) {
            return remove_same(val, arr_size, start, _size);
        }

        constexpr size_t remove_same(const T* val, size_t arr_size, size_t start, size_t end) {
            size_t old_size = _size;
            size_t pos = start;
            if (start <= end) {
                while (pos != npos) {
                    pos = find(val, arr_size, pos, end);
                    if (pos != npos) {
                        remove(pos, pos + arr_size);
                        end -= arr_size;
                    }
                }
            } else {
                while (pos != npos) {
                    pos = findr(val, arr_size, pos, end);
                    if (pos != npos) {
                        remove(pos, pos + arr_size);
                        end -= arr_size;
                    }
                }
            }
            return old_size - _size;
        }

        template <class AnyAllocator>
        constexpr size_t remove_same(const list_array<T, AnyAllocator>& val, size_t start = 0) {
            return remove_same(val, 0, val._size, start, _size);
        }

        template <class AnyAllocator>
        constexpr size_t remove_same(const list_array<T, AnyAllocator>& val, size_t start, size_t end) {
            return remove_same(val, 0, val._size, start, end);
        }

        template <class AnyAllocator>
        constexpr size_t remove_same(const list_array<T, AnyAllocator>& val, size_t val_start, size_t val_end, size_t start, size_t end) {
            size_t old_size = _size;
            size_t pos = start;
            if (start <= end) {
                while (pos != npos) {
                    pos = find(val, val_start, val_end, pos, end);
                    if (pos != npos) {
                        remove(pos, pos + val_end - val_start);
                        end -= val_end - val_start;
                    }
                }
            } else {
                while (pos != npos) {
                    pos = findr(val, val_start, val_end, pos, end);
                    if (pos != npos) {
                        remove(pos, pos + val_end - val_start);
                        end -= val_end - val_start;
                    }
                }
            }
            return old_size - _size;
        }

#pragma endregion
#pragma region find

        constexpr size_t find(const T& value, size_t continue_from = 0) const
            requires(std::equality_comparable<T>)
        {
            return find(value, continue_from, _size);
        }

        constexpr size_t find(const T& value, size_t continue_from, size_t end) const
            requires(std::equality_comparable<T>)
        {
            size_t i = 0;
            for (auto& it : range(continue_from, end)) {
                if (it == value)
                    return i;
                ++i;
            }
            return npos;
        }

        template <size_t arr_size>
        constexpr size_t find(const T (&arr)[arr_size], size_t continue_from = 0) const
            requires(std::equality_comparable<T>)
        {
            return find(arr, arr_size, continue_from, _size);
        }

        template <size_t arr_size>
        constexpr size_t find(const T (&arr)[arr_size], size_t continue_from, size_t end) const
            requires(std::equality_comparable<T>)
        {
            return find(arr, arr_size, continue_from, end);
        }

        constexpr size_t find(const T* arr, size_t arr_size, size_t continue_from = 0) const
            requires(std::equality_comparable<T>)
        {
            return find(arr, arr_size, continue_from, _size);
        }

        constexpr size_t find(const T* arr, size_t arr_size, size_t continue_from, size_t end) const
            requires(std::equality_comparable<T>)
        {
            size_t i = 0;
            size_t eq = 0;
            for (auto& it : range(continue_from, end)) {
                if (arr[eq] == it)
                    ++eq;
                else
                    eq = 0;
                if (eq == arr_size)
                    return i - arr_size;
                ++i;
            }
            return npos;
        }

        template <class AnyAllocator>
        constexpr size_t find(const list_array<T, AnyAllocator>& value, size_t continue_from = 0) const
            requires(std::equality_comparable<T>)
        {
            return find(value, 0, value._size, continue_from, _size);
        }

        template <class AnyAllocator>
        constexpr size_t find(const list_array<T, AnyAllocator>& value, size_t continue_from, size_t end_pos) const
            requires(std::equality_comparable<T>)
        {
            return find(value, 0, value._size, continue_from, end_pos);
        }

        template <class AnyAllocator>
        constexpr size_t find(const list_array<T, AnyAllocator>& value, size_t value_start, size_t value_end, size_t continue_from, size_t end) const
            requires(std::equality_comparable<T>)
        {
            if (!value._size)
                throw std::out_of_range("this value too small for searching value");
            if (value_start > value_end) {
                std::swap(value_end, value_start);
                if (value_end > value._size)
                    throw std::out_of_range("value_end is out of value size limit");
                auto v_beg = const_reverse_iterator(value.get_iterator(value_end));
                auto i_beg = v_beg;
                size_t i = value_start;
                size_t res = continue_from;
                for (const T& it : range(continue_from, end)) {
                    if (it == *i_beg) {
                        ++i;
                        if (i == value_end)
                            return res - (value_end - value_start);
                        ++i_beg;
                    } else {
                        i = value_start;
                        i_beg = v_beg;
                    }
                    ++res;
                }
            } else {
                if (value_end > value._size)
                    throw std::out_of_range("value_end is out of value size limit");
                auto v_beg = value.get_iterator(value_start);
                auto i_beg = v_beg;
                size_t i = value_start;
                size_t res = continue_from;
                for (const T& it : range(continue_from, end)) {
                    if (it == *i_beg) {
                        ++i;
                        if (i == value_end)
                            return res - (value_end - value_start);
                        ++i_beg;
                    } else {
                        i = value_start;
                        i_beg = v_beg;
                    }
                    ++res;
                }
            }
            return npos;
        }

        constexpr size_t findr(const T& value) const
            requires(std::equality_comparable<T>)
        {
            return findr(value, 0, _size);
        }

        constexpr size_t findr(const T& value, size_t continue_from, size_t begin = 0) const
            requires(std::equality_comparable<T>)
        {
            size_t i = 0;
            for (auto& it : reverse_range(begin, continue_from)) {
                if (it == value)
                    return i;
                ++i;
            }
            return npos;
        }

        template <size_t arr_size>
        constexpr size_t findr(const T (&arr)[arr_size]) const
            requires(std::equality_comparable<T>)
        {
            return findr(arr, arr_size, _size, 0);
        }

        template <size_t arr_size>
        constexpr size_t findr(const T (&arr)[arr_size], size_t continue_from, size_t begin = 0) const
            requires(std::equality_comparable<T>)
        {
            return findr(arr, arr_size, continue_from, begin);
        }

        constexpr size_t findr(const T* arr, size_t arr_size) const
            requires(std::equality_comparable<T>)
        {
            return findr(arr, arr_size, _size);
        }

        constexpr size_t findr(const T* arr, size_t arr_size, size_t continue_from, size_t begin = 0) const
            requires(std::equality_comparable<T>)
        {
            size_t i = _size;
            size_t eq = 0;

            for (auto& it : reverse_range(begin, continue_from)) {
                if (arr[eq] == it)
                    ++eq;
                else
                    eq = 0;
                if (eq == arr_size)
                    return i;
                --i;
            }
            return npos;
        }

        constexpr size_t findr(const list_array<T, Allocator>& value) const
            requires(std::equality_comparable<T>)
        {
            return findr(value, 0, value._size, _size);
        }

        constexpr size_t findr(const list_array<T, Allocator>& value, size_t continue_from, size_t begin = 0) const
            requires(std::equality_comparable<T>)
        {
            return findr(value, 0, value._size, continue_from, begin);
        }

        constexpr size_t findr(const list_array<T, Allocator>& value, size_t value_start, size_t value_end, size_t continue_from, size_t begin = 0) const
            requires(std::equality_comparable<T>)
        {
            if (!value._size)
                throw std::out_of_range("this value too small for searching value");
            if (value_start > value_end) {
                if (value_end > value._size)
                    throw std::out_of_range("value_end is out of value size limit");
                auto v_beg = const_reverse_iterator(value.get_iterator(value_end));
                auto i_beg = v_beg;
                size_t i = value_start;
                size_t res = continue_from;
                for (const T& it : reverse_range(begin, continue_from)) {
                    if (it == *i_beg) {
                        ++i;
                        if (i == value_end)
                            return res;
                        ++i_beg;
                    } else {
                        i = value_start;
                        i_beg = v_beg;
                    }
                    --res;
                }
            } else {
                if (value_end > value._size)
                    throw std::out_of_range("value_end is out of value size limit");
                auto v_beg = value.get_iterator(value_start);
                auto i_beg = v_beg;
                size_t i = value_start;
                size_t res = continue_from;
                for (const T& it : reverse_range(begin, continue_from)) {
                    if (it == *i_beg) {
                        ++i;
                        if (i == value_end)
                            return res;
                        ++i_beg;
                    } else {
                        i = value_start;
                        i_beg = v_beg;
                    }
                    --res;
                }
            }
            return npos;
        }

        template <class _Fn>
        constexpr size_t find_it(_Fn&& find_func) const {
            return find_it(0, _size, find_func);
        }

        template <class _Fn>
        constexpr size_t find_it(size_t continue_from, _Fn&& find_func) const {
            return find_it(continue_from, _size, find_func);
        }

        template <class _Fn>
        constexpr size_t find_it(size_t continue_from, size_t end, _Fn&& find_func) const {
            if (continue_from > end)
                return findr_it(end, continue_from, find_func);
            size_t i = 0;
            for (const T& it : range(continue_from, end)) {
                if (find_func(it))
                    return i;
                ++i;
            }
            return npos;
        }

        template <class _Fn>
        constexpr size_t findr_it(_Fn&& find_func) const {
            return findr_it(0, _size, find_func);
        }

        template <class _Fn>
        constexpr size_t findr_it(size_t continue_from, _Fn&& find_func) const {
            return findr_it(continue_from, _size, find_func);
        }

        template <class _Fn>
        constexpr size_t findr_it(size_t continue_from, size_t begin, _Fn&& find_func) const {
            size_t i = 0;
            for (const T& it : reverse_range(begin, continue_from)) {
                if (find_func(it))
                    return i;
                ++i;
            }
            return npos;
        }

#pragma endregion
#pragma region sort

        constexpr list_array<T, Allocator> sort_copy() const {
            return list_array<T, Allocator>(*this).sort();
        }

        constexpr list_array<T, Allocator>& sort() {
            if constexpr (std::is_unsigned<T>::value) {
                const T& min_val = mmin();
                size_t dif = mmax() - min_val + 1;
                list_array<size_t> count_arr(dif);
                list_array<T, Allocator> result(_size);
                {
                    size_t i = 0;
                    for (const T& it : *this)
                        count_arr[it - min_val] += 1;
                }
                for (size_t i = 1; i < dif; i++)
                    count_arr[i] += count_arr[i - 1];
                {
                    for (const T& it : reverse()) {
                        result[count_arr[it - min_val] - 1] = it;
                        count_arr[it - min_val] -= 1;
                    }
                }
                swap(result);
            } else if constexpr (std::is_signed_v<T> && sizeof(T) <= sizeof(size_t) && !std::is_floating_point_v<T>) {
                auto normalize = [](const T& to) {
                    constexpr const size_t to_shift = sizeof(T) * 4;
                    return size_t((SIZE_MAX >> to_shift) + to);
                };
                size_t min_val = normalize(mmin());
                size_t dif = normalize(mmax()) - min_val + 1;
                list_array<size_t> count_arr(dif, 0);
                list_array<T, Allocator> result(_size);
                {
                    size_t i = 0;
                    for (const T& it : *this)
                        count_arr[normalize(it) - min_val] += 1;
                }
                for (size_t i = 1; i < dif; i++)
                    count_arr[i] += count_arr[i - 1];
                {
                    for (const T& it : reverse()) {
                        result[count_arr[normalize(it) - min_val] - 1] = it;
                        count_arr[normalize(it) - min_val] -= 1;
                    }
                }
                swap(result);
            } else {
                size_t curr_L_size = _size / 2 + 1;
                size_t curr_M_size = _size / 2 + 1;
                T* L = pair.allocate(_size / 2 + 1);
                T* M = pair.allocate(_size / 2 + 1);
                auto fix_size = [&](size_t start, size_t middle, size_t end) {
                    size_t l = middle - start + 1;
                    size_t m = end - middle + 1;
                    if (curr_L_size < l) {
                        pair.deallocate_destruct(L, curr_L_size);
                        L = pair.allocate(l);
                        curr_L_size = l;
                    }
                    if (curr_M_size < m) {
                        pair.deallocate_destruct(M, curr_M_size);
                        M = pair.allocate(m);
                        curr_M_size = m;
                    }
                };
                auto merge = [&](size_t start, size_t middle, size_t end) {
                    size_t n1 = middle - start;
                    size_t n2 = end - middle;
                    if (curr_L_size < n1 || curr_M_size < n2)
                        fix_size(start, middle, end);
                    get_iterator(start)._fast_load(L, n1);
                    get_iterator(middle)._fast_load(M, n2);
                    size_t i = 0, j = 0, k = start;
                    for (T& it : range(start, end)) {
                        if (i < n1 && j < n2)
                            it = L[i] <= M[j] ? L[i++] : M[j++];
                        else if (i < n1)
                            it = L[i++];
                        else if (j < n2)
                            it = M[j++];
                    }
                };
                for (size_t b = 2; b < _size; b <<= 1) {
                    for (size_t i = 0; i < _size; i += b) {
                        if (i + b > _size)
                            merge(i, i + ((_size - i) >> 1), _size);
                        else
                            merge(i, i + (b >> 1), i + b);
                    }
                    if (b << 1 > _size)
                        merge(0, b, _size);
                }
                auto non_sorted_finder = [&](size_t continue_search) {
                    const auto* check = &operator[](0);
                    size_t res = 0;
                    for (const T& it : *this) {
                        if (*check > it)
                            return res;
                        check = &it;
                        ++res;
                    }
                    return size_t(0);
                };
                size_t err = 0;
                while ((err = non_sorted_finder(err)))
                    merge(0, err, _size);
                pair.deallocate_destruct(L, curr_L_size);
                pair.deallocate_destruct(M, curr_M_size);
            }
            return *this;
        }

        template <class _FN>
        constexpr list_array<T, Allocator> sort_copy(_FN compare) const {
            return list_array<T, Allocator>(*this).sort(compare);
        }

        template <class _FN>
        constexpr list_array<T, Allocator>& sort(_FN compare) {
            size_t curr_L_size = _size / 2 + 1;
            size_t curr_M_size = _size / 2 + 1;
            T* L = pair.allocate_construct(_size / 2 + 1);
            T* M = pair.allocate_construct(_size / 2 + 1);
            auto fix_size = [&L, &M, &curr_L_size, &curr_M_size](size_t start, size_t middle, size_t end) {
                size_t l = middle - start + 1;
                size_t m = end - middle + 1;
                if (curr_L_size < l) {
                    pair.deallocate_destruct(L, curr_L_size);
                    L = pair.allocate_construct(l);
                    curr_L_size = l;
                }
                if (curr_M_size < m) {
                    pair.deallocate_destruct(M, curr_M_size);
                    M = pair.allocate_construct(m);
                    curr_M_size = m;
                }
            };
            auto merge = [&](size_t start, size_t middle, size_t end) {
                size_t n1 = middle - start;
                size_t n2 = end - middle;
                if (curr_L_size < n1 || curr_M_size < n2)
                    fix_size(start, middle, end);
                get_iterator(start)._fast_load(L, n1);
                get_iterator(middle)._fast_load(M, n2);
                size_t i = 0, j = 0, k = start;
                for (T& it : range(start, end)) {
                    if (i < n1 && j < n2)
                        it = compare(L[i], M[j]) ? L[i++] : M[j++];
                    else if (i < n1)
                        it = L[i++];
                    else if (j < n2)
                        it = M[j++];
                }
            };
            for (size_t b = 2; b < _size; b <<= 1) {
                for (size_t i = 0; i < _size; i += b) {
                    if (i + b > _size)
                        merge(i, i + ((_size - i) >> 1), _size);
                    else
                        merge(i, i + (b >> 1), i + b);
                }
                if (b << 1 > _size)
                    merge(0, b, _size);
            }
            auto non_sorted_finder = [&](size_t continue_search) {
                const auto* check = &operator[](0);
                size_t res = 0;
                for (const T& it : *this) {
                    if (*check > it)
                        return res;
                    check = &it;
                    ++res;
                }
                return size_t(0);
            };
            size_t err = 0;
            while ((err = non_sorted_finder(err)))
                merge(0, err, _size);
            pair.deallocate_destruct(L, curr_L_size);
            pair.deallocate_destruct(M, curr_M_size);
            return *this;
        }

#pragma endregion
#pragma region split

        constexpr list_array<T, Allocator> split(size_t split_pos) {
            if (_size <= split_pos)
                throw std::out_of_range("Fail split due small array or split_pos is equal with array size");
            list_array<T, Allocator> res(_size - split_pos);
            size_t i = 0;
            for (auto& it : range(split_pos, _size))
                res[i++] = std::move(it);
            remove(split_pos, _size);
            return res;
        }

        constexpr std::pair<list_array<T, Allocator>, list_array<T, Allocator>> split_copy(size_t split_pos) const {
            list_array<T, Allocator> tmp(*this);
            return {tmp, tmp.split()};
        }

        template <class InnerAllocator = std::allocator<list_array<T, Allocator>>>
        constexpr list_array<list_array<T, Allocator>, InnerAllocator> split_by(const T& split_value) {
            list_array<list_array<T, Allocator>, InnerAllocator> res;
            for (size_t i = 0; i < _size; i++) {
                if (operator[](i) == split_value) {
                    if (i != 0)
                        res.push_back(take(0, i));
                    else
                        res.push_back({});
                    remove(0);
                    i = 0;
                }
            }
            if (_size)
                res.push_back(take());
            return res;
        }

        template <class InnerAllocator = std::allocator<list_array<T, Allocator>>>
        constexpr list_array<list_array<T, Allocator>, InnerAllocator> split_by_copy(const T& split_value) const {
            return list_array<T, Allocator>(*this).split_by(split_value);
        }

        template <size_t arr_size, class InnerAllocator = std::allocator<list_array<T, Allocator>>>
        constexpr list_array<list_array<T, Allocator>, InnerAllocator> split_by(const T (&split_values)[arr_size]) {
            return split_by(split_values, arr_size);
        }

        template <size_t arr_size, class InnerAllocator = std::allocator<list_array<T, Allocator>>>
        constexpr list_array<list_array<T, Allocator>, InnerAllocator> split_by_copy(const T (&split_values)[arr_size]) const {
            return split_by_copy(split_values, arr_size);
        }

        template <class InnerAllocator = std::allocator<list_array<T, Allocator>>>
        constexpr list_array<list_array<T, Allocator>, InnerAllocator> split_by(const T* split_values, size_t split_values_size) {
            list_array<list_array<T, Allocator>, InnerAllocator> res;
            for (size_t i = 0; i < _size; i++) {
                for (size_t j = 0; j < split_values_size; j++) {
                    if (operator[](i) == split_values[j]) {
                        if (i != 0)
                            res.push_back(take(0, i));
                        else
                            res.push_back({});
                        remove(0);
                        i = 0;
                        break;
                    }
                }
            }
            if (_size)
                res.push_back(take());
            return res;
        }

        template <class InnerAllocator = std::allocator<list_array<T, Allocator>>>
        constexpr list_array<list_array<T, Allocator>, InnerAllocator> split_by_copy(const T* split_values, size_t split_values_size) const {
            return copy().split_by(split_values, split_values_size);
        }

        template <class AnyAllocator, class InnerAllocator = std::allocator<list_array<T, AnyAllocator>>>
        constexpr list_array<list_array<T, AnyAllocator>, InnerAllocator> split_by(const list_array<T, AnyAllocator>& split_values) {
            list_array<list_array<T, AnyAllocator>, InnerAllocator> res;
            for (size_t i = 0; i < _size; i++) {
                if (split_values.contains(operator[](i))) {
                    if (i != 0)
                        res.push_back(take(0, i));
                    else
                        res.push_back({});
                    remove(0);
                    i = 0;
                }
            }
            if (_size)
                res.push_back(take());
            return res;
        }

        template <class AnyAllocator, class InnerAllocator = std::allocator<list_array<T, AnyAllocator>>>
        constexpr list_array<list_array<T, AnyAllocator>, InnerAllocator> split_by_copy(const list_array<T, AnyAllocator>& split_values) const {
            return copy().split_by(split_values);
        }

        template <class _Fn, class InnerAllocator = std::allocator<list_array<T, Allocator>>>
        constexpr list_array<list_array<T, Allocator>, InnerAllocator> split_if(_Fn&& split_function) {
            list_array<list_array<T, Allocator>, InnerAllocator> res;
            for (size_t i = 0; i < _size; i++) {
                if (split_function(operator[](i))) {
                    if (i != 0)
                        res.push_back(take(0, i));
                    else
                        res.push_back({});
                    remove(0);
                    i = 0;
                }
            }
            if (_size)
                res.push_back(take());
            return res;
        }

        template <class _Fn>
        constexpr std::pair<list_array<T, Allocator>, list_array<T, Allocator>> split_if_copy(_Fn&& split_function) const {
            return copy().split_if(split_function);
        }

#pragma endregion
#pragma region take

        constexpr list_array<T, Allocator> take() {
            list_array<T, Allocator> res;
            res.swap(*this);
            return res;
        }

        constexpr T take(size_t take_pos) {
            if (_size <= take_pos)
                throw std::out_of_range("Fail take item due small array");
            T res(std::move(operator[](take_pos)));
            remove(take_pos);
            return res;
        }

        constexpr T* take_raw(size_t& size) {
            if (blocks_more(1))
                commit();
            size = _size;
            T* res = pair.hold_value.arr->pair.hold_value;
            pair.hold_value.arr->pair.hold_value = nullptr;
            clear();
            return res;
        }

        constexpr list_array<T, Allocator> take(size_t start_pos, size_t end_pos) {
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (_size < end_pos)
                    throw std::out_of_range("Fail take items due small array");
                list_array<T, Allocator> res(end_pos - start_pos);
                size_t i = 0;
                for (auto& it : reverse_range(start_pos, end_pos))
                    res[i++] = std::move(it);
                remove(start_pos, end_pos);
                return res;
            } else {
                if (_size < end_pos)
                    throw std::out_of_range("Fail take items due small array");
                list_array<T, Allocator> res(end_pos - start_pos);
                size_t i = 0;
                for (auto& it : range(start_pos, end_pos))
                    res[i++] = std::move(it);
                remove(start_pos, end_pos);
                return res;
            }
        }

        template <class _Fn, std::enable_if<std::is_function<_Fn>::value>>
        constexpr list_array<T, Allocator> take(_Fn&& select_fn) {
            return take(select_fn, 0, _size);
        }

        template <class _Fn, std::enable_if<std::is_function<_Fn>::value>>
        constexpr list_array<T, Allocator> take(size_t start_pos, _Fn&& select_fn) {
            return take(select_fn, start_pos, _size);
        }

        template <class _Fn, std::enable_if<std::is_function<_Fn>::value>>
        constexpr list_array<T, Allocator> take(size_t start_pos, size_t end_pos, _Fn&& select_fn) {
            size_t i = 0;
            size_t taken_items = 0;
            list_array<uint8_t> selector(((end_pos - start_pos) >> 3) + 1, 0);
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (_size < end_pos)
                    throw std::out_of_range("Fail take items due small array");
                for (auto& it : range(start_pos, end_pos)) {
                    if (select_fn(it)) {
                        ++taken_items;
                        selector[i >> 3] |= 1 << (i & 7);
                    }
                    i++;
                }
                if (taken_items == 0)
                    return {};
                list_array<T, Allocator> res;
                res.reserve_push_back(taken_items);
                for (auto& it : reverse_range(start_pos, end_pos)) {
                    --i;
                    if (selector[i >> 3] & (1 << (i & 7)))
                        res.push_back(it);
                }
                remove_if(
                    start_pos,
                    end_pos,
                    [selector, &i]() {
                        bool res = selector[i >> 3] & (1 << (i & 7));
                        i++;
                        return res;
                    }
                );
                return res;
            } else {
                if (_size < end_pos)
                    throw std::out_of_range("Fail take items due small array");
                for (auto& it : range(start_pos, end_pos)) {
                    if (select_fn(it)) {
                        ++taken_items;
                        selector[i >> 3] |= 1 << (i & 7);
                    }
                    i++;
                }
                if (taken_items == 0)
                    return {};
                list_array<T, Allocator> res;
                res.reserve_push_back(taken_items);
                i = 0;
                for (auto& it : range(start_pos, end_pos)) {
                    if (selector[i >> 3] & (1 << (i & 7)))
                        res.push_back(it);
                }
                i = 0;
                remove_if(
                    start_pos,
                    end_pos,
                    [selector, &i]() {
                        bool res = selector[i >> 3] & (1 << (i & 7));
                        i++;
                        return res;
                    }
                );
                return res;
            }
        }

#pragma endregion
#pragma region copy/swap

        constexpr list_array<T, Allocator> copy(size_t start_pos, size_t end_pos) const {
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (_size < end_pos)
                    throw std::out_of_range("Fail take items due small array");
                list_array<T, Allocator> res(end_pos - start_pos);
                size_t i = 0;
                for (auto& it : reverse_range(start_pos, end_pos))
                    res[i++] = it;
                return res;
            } else {
                if (_size < end_pos)
                    throw std::out_of_range("Fail take items due small array");
                list_array<T, Allocator> res(end_pos - start_pos);
                size_t i = 0;
                for (auto& it : range(start_pos, end_pos))
                    res[i++] = it;
                return res;
            }
        }

        constexpr list_array<T, Allocator> copy(size_t start_pos) const {
            return copy(start_pos, _size);
        }

        constexpr list_array<T, Allocator> copy() const {
            return *this;
        }

        constexpr list_array<T, Allocator>& swap(list_array<T, Allocator>& to_swap) noexcept {
            if (pair.hold_value.arr != to_swap.pair.hold_value.arr) {
                dynamic_arr<T, Allocator> tmp(std::move(pair.hold_value), get_allocator());
                pair.hold_value = std::move(to_swap.pair.hold_value);
                to_swap.pair.hold_value = std::move(tmp);

                size_t rb = reserved_begin;
                size_t re = reserved_end;
                size_t s = _size;
                reserved_begin = to_swap.reserved_begin;
                _size = to_swap._size;
                reserved_end = to_swap.reserved_end;
                to_swap.reserved_begin = rb;
                to_swap.reserved_end = re;
                to_swap._size = s;
            }
            return *this;
        }

#pragma endregion
#pragma region remove duplicates

        constexpr size_t unique() {
            return unique(0, _size);
        }

        constexpr size_t unique(size_t start_pos, size_t end_pos) {
            if (start_pos > end_pos)
                std::swap(start_pos, end_pos);
            if (start_pos + 1 >= end_pos)
                return 0;
            if (end_pos > _size)
                throw std::out_of_range("end_pos out of size limit");
            T* it = &operator[](start_pos);
            size_t res = 0;
            remove_if(
                start_pos + 1,
                end_pos,
                [&it, &res](T& check_it) {
                    if (check_it == *it)
                        return (bool)++res;
                    it = &check_it;
                    return false;
                }
            );
            return res;
        }

        //keep only unique item neighbors
        template <class _Fn>
        constexpr size_t unique(_Fn&& compare_func) {
            return unique(compare_func, 0, _size);
        }

        template <class _Fn>
        constexpr size_t unique(size_t start_pos, size_t end_pos, _Fn&& compare_func) {
            if (start_pos > end_pos)
                std::swap(start_pos, end_pos);
            if (start_pos + 1 >= end_pos)
                return 0;
            if (end_pos > _size)
                throw std::out_of_range("end_pos out of size limit");
            const T* it = &operator[](start_pos);
            return remove_if(
                start_pos + 1,
                end_pos,
                [&it, &compare_func](const T& check_it) {
                    if (compare_func(*it, check_it))
                        return true;
                    it = &check_it;
                    return false;
                }
            );
        }

        //remove all copys
        constexpr size_t unify() {
            return unify(0, _size);
        }

        constexpr size_t unify(size_t start_pos, size_t end_pos) {
            list_array<T, Allocator> tmp_arr;
            tmp_arr.reserve_push_back((_size >> 2) + 1);
            for (T& it : range(start_pos, end_pos))
                if (!tmp_arr.contains(it))
                    tmp_arr.push_back(it);
            tmp_arr.shrink_to_fit();
            swap(tmp_arr);
            return tmp_arr._size - _size;
        }

        //keep only unique from all array
        constexpr size_t alone() {
            return alone(0, _size);
        }

        template <class LocalAllocator>
        constexpr size_t alone(size_t start_pos, size_t end_pos, LocalAllocator alloc = std::allocator<uint8_t>()) {
            if (start_pos > end_pos)
                std::swap(start_pos, end_pos);
            if (start_pos + 1 >= end_pos)
                return 0;
            if (end_pos > _size)
                throw std::out_of_range("end_pos out of size limit");
            size_t selector_size = ((end_pos - start_pos) >> 3) + 1;
            uint8_t* selector = alloc.allocate(selector_size);
            std::memset(selector, 0, selector_size);
            size_t i = 0;
            for (T& it : range(start_pos, end_pos)) {
                if (selector[i >> 3] & (1 << (i & 7))) {
                    i++;
                    continue;
                }
                size_t j = 0;
                bool is_unique = true;
                for (T& cmp_it : range(start_pos, end_pos)) {
                    if (i == j) {
                        j++;
                        continue;
                    }
                    if (it == cmp_it) {
                        is_unique = false;
                        selector[j >> 3] |= (1 << (j & 7));
                    }
                    j++;
                }
                if (!is_unique)
                    selector[i >> 3] |= (1 << (i & 7));
                i++;
            }
            i = 0;
            size_t result =
                remove_if(
                    start_pos,
                    end_pos,
                    [selector, &i](T& check_it) {
                        bool res = selector[i >> 3] & (1 << (i & 7));
                        i++;
                        return res;
                    }
                );
            alloc.deallocate(selector, selector_size);
            return result;
        }

#pragma endregion
#pragma region join

        template <class _FN = bool (*)(const T&)>
        constexpr list_array<T, Allocator>& join(const T& insert_item, _FN where_join) {
            return operator=(join_copy(insert_item, 0, _size, where_join));
        }

        template <class _FN>
        constexpr list_array<T, Allocator> join_copy(const T& insert_item, _FN where_join) const {
            return join_copy(insert_item, 0, _size, where_join);
        }

        template <class _FN>
        constexpr list_array<T, Allocator> join_copy(const T& insert_item, size_t start_pos, _FN where_join) const {
            return join_copy(insert_item, start_pos, _size, where_join);
        }

        template <class _FN>
        constexpr list_array<T, Allocator> join_copy(const T& insert_item, size_t start_pos, size_t end_pos, _FN where_join) const {
            list_array<T, Allocator> res;
            res.reserve_push_back(_size * 2);
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                for (auto& i : reverse_range(start_pos, end_pos)) {
                    res.push_back(i);
                    if (where_join(i))
                        res.push_back(insert_item);
                }
            } else {
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                for (auto& i : range(start_pos, end_pos)) {
                    res.push_back(i);
                    if (where_join(i))
                        res.push_back(insert_item);
                }
            }
            return res;
        }

        template <class AnyAllocator, class _FN>
        constexpr list_array<T, Allocator>& join(const list_array<T, AnyAllocator>& insert_items, _FN where_join) {
            return operator=(join_copy(insert_items, 0, _size, where_join));
        }

        template <class AnyAllocator, class _FN>
        constexpr list_array<T, Allocator> join_copy(const list_array<T, AnyAllocator>& insert_items, _FN where_join) const {
            return join_copy(insert_items, 0, _size, where_join);
        }

        template <class AnyAllocator, class _FN>
        constexpr list_array<T, Allocator> join_copy(const list_array<T, AnyAllocator>& insert_items, size_t start_pos, _FN where_join) const {
            return join_copy(insert_items, start_pos, _size, where_join);
        }

        template <class AnyAllocator, class _FN>
        constexpr list_array<T, Allocator> join_copy(const list_array<T, AnyAllocator>& insert_items, size_t start_pos, size_t end_pos, _FN where_join) const {
            list_array<T, Allocator> res;
            res.reserve_push_back(_size * 2);
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                for (auto& i : reverse_range(start_pos, end_pos)) {
                    res.push_back(i);
                    if (where_join(i))
                        res.push_back(insert_items);
                }
            } else {
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                for (auto& i : range(start_pos, end_pos)) {
                    res.push_back(i);
                    if (where_join(i))
                        res.push_back(insert_items);
                }
            }
            return res;
        }

        template <size_t arr_size, class _FN>
        constexpr list_array<T, Allocator>& join(const T (&insert_items)[arr_size], _FN where_join) {
            return operator=(join_copy(insert_items, arr_size, 0, _size, where_join));
        }

        template <size_t arr_size, class _FN>
        constexpr list_array<T, Allocator> join_copy(const T (&insert_items)[arr_size], _FN where_join) const {
            return join_copy(insert_items, arr_size, 0, _size, where_join);
        }

        template <size_t arr_size, class _FN>
        constexpr list_array<T, Allocator> join_copy(const T (&insert_items)[arr_size], size_t start_pos, _FN where_join) const {
            return join_copy(insert_items, arr_size, start_pos, _size, where_join);
        }

        template <size_t arr_size, class _FN>
        constexpr list_array<T, Allocator> join_copy(const T (&insert_items)[arr_size], size_t start_pos, size_t end_pos, _FN where_join) const {
            return join_copy(insert_items, arr_size, start_pos, end_pos, where_join);
        }

        template <class _FN>
        constexpr list_array<T, Allocator>& join(const T* insert_items, size_t items_count, _FN where_join) {
            return operator=(join_copy(insert_items, items_count, 0, _size, where_join));
        }

        template <class _FN>
        constexpr list_array<T, Allocator> join_copy(const T* insert_items, size_t items_count, _FN where_join) const {
            return join_copy(insert_items, items_count, 0, _size, where_join);
        }

        template <class _FN>
        constexpr list_array<T, Allocator> join_copy(const T* insert_items, size_t items_count, size_t start_pos, _FN where_join) const {
            return join_copy(insert_items, items_count, start_pos, _size, where_join);
        }

        template <class _FN>
        constexpr list_array<T, Allocator> join_copy(const T* insert_items, size_t items_count, size_t start_pos, size_t end_pos, _FN where_join) const {
            list_array<T, Allocator> res;
            res.reserve_push_back(_size * 2);
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                for (auto& i : reverse_range(start_pos, end_pos)) {
                    res.push_back(i);
                    if (where_join(i))
                        res.push_back(insert_items, items_count);
                }
            } else {
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                for (auto& i : range(start_pos, end_pos)) {
                    res.push_back(i);
                    if (where_join(i))
                        res.push_back(insert_items, items_count);
                }
            }
            return res;
        }

        constexpr list_array<T, Allocator>& join(const T& insert_item) {
            return operator=(join_copy(insert_item, 0, _size));
        }

        constexpr list_array<T, Allocator> join_copy(const T& insert_item) const {
            return join_copy(insert_item, 0, _size);
        }

        constexpr list_array<T, Allocator> join_copy(const T& insert_item, size_t start_pos) const {
            return join_copy(insert_item, start_pos, _size);
        }

        constexpr list_array<T, Allocator> join_copy(const T& insert_item, size_t start_pos, size_t end_pos) const {
            list_array<T, Allocator> res;
            res.reserve_push_back(_size * 2);
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                for (auto& i : reverse_range(start_pos, end_pos)) {
                    res.push_back(i);
                    res.push_back(insert_item);
                }
            } else {
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                for (auto& i : range(start_pos, end_pos)) {
                    res.push_back(i);
                    res.push_back(insert_item);
                }
            }
            return res;
        }

        template <class AnyAllocator>
        constexpr list_array<T, Allocator>& join(const list_array<T, AnyAllocator>& insert_items) {
            return operator=(join_copy(insert_items, 0, _size));
        }

        template <class AnyAllocator>
        constexpr list_array<T, Allocator> join_copy(const list_array<T, AnyAllocator>& insert_items) const {
            return join_copy(insert_items, 0, _size);
        }

        template <class AnyAllocator>
        constexpr list_array<T, Allocator> join_copy(const list_array<T, AnyAllocator>& insert_items, size_t start_pos) const {
            return join_copy(insert_items, start_pos, _size);
        }

        template <class AnyAllocator>
        constexpr list_array<T, AnyAllocator> join_copy(const list_array<T, AnyAllocator>& insert_items, size_t start_pos, size_t end_pos) const {
            list_array<T, AnyAllocator> res(insert_items.get_allocator());
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                res.reserve_push_back(_size * (insert_items.size() + 1));
                for (auto& i : reverse_range(start_pos, end_pos)) {
                    res.push_back(i);
                    res.push_back(insert_items);
                }
            } else {
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                res.reserve_push_back(_size * (insert_items.size() + 1));
                for (auto& i : range(start_pos, end_pos)) {
                    res.push_back(i);
                    res.push_back(insert_items);
                }
            }
            return res;
        }

        constexpr list_array<T, Allocator>& join(const T* insert_items, size_t items_count) {
            return operator=(join_copy(insert_items, items_count, 0, _size));
        }

        constexpr list_array<T, Allocator> join_copy(const T* insert_items, size_t items_count) const {
            return join_copy(insert_items, items_count, 0, _size);
        }

        constexpr list_array<T, Allocator> join_copy(const T* insert_items, size_t items_count, size_t start_pos) const {
            return join_copy(insert_items, items_count, start_pos, _size);
        }

        constexpr list_array<T, Allocator> join_copy(const T* insert_items, size_t items_count, size_t start_pos, size_t end_pos) const {
            list_array<T, Allocator> res;
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                res.reserve_push_back(_size * (items_count + 1));
                for (auto& i : reverse_range(start_pos, end_pos)) {
                    res.push_back(i);
                    res.push_back(insert_items, items_count);
                }
            } else {
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                res.reserve_push_back(_size * (items_count + 1));
                for (auto& i : range(start_pos, end_pos)) {
                    res.push_back(i);
                    res.push_back(insert_items, items_count);
                }
            }
            return res;
        }

        template <size_t arr_size>
        constexpr list_array<T, Allocator>& join(const T (&insert_items)[arr_size]) {
            return operator=(join_copy(insert_items, arr_size, 0, _size));
        }

        template <size_t arr_size>
        constexpr list_array<T, Allocator> join_copy(const T (&insert_items)[arr_size]) const {
            return join_copy(insert_items, arr_size, 0, _size);
        }

        template <size_t arr_size>
        constexpr list_array<T, Allocator> join_copy(const T (&insert_items)[arr_size], size_t start_pos) const {
            return join_copy(insert_items, arr_size, start_pos, _size);
        }

        template <size_t arr_size>
        constexpr list_array<T, Allocator> join_copy(const T (&insert_items)[arr_size], size_t start_pos, size_t end_pos) const {
            return join_copy(insert_items, arr_size, start_pos, end_pos);
        }

#pragma endregion
#pragma region concat

        template <class AnyAllocator0, class AnyAllocator1>
        static list_array<T, Allocator>& concat(const list_array<list_array<T, AnyAllocator1>, AnyAllocator0>& concat_arr) {
            for (auto& i : concat_arr)
                push_back(i);
            return *this;
        }

        template <class Y = T>
        typename is_container<Y>::container concat() {
            T res;
            for (auto& i : *this)
                res.push_back(i);
            return res;
        }

#pragma endregion
#pragma region where

        template <class _Fn>
        constexpr list_array<T, Allocator> where(_Fn&& check_fn) const {
            return where(0, _size, check_fn);
        }

        template <class _Fn>
        constexpr list_array<T, Allocator> where(size_t start_pos, _Fn&& check_fn) const {
            return where(start_pos, _size, check_fn);
        }

        template <class _Fn>
        constexpr list_array<T, Allocator> where(size_t start_pos, size_t end_pos, _Fn&& check_fn) const {
            list_array<T, Allocator> res;
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                res.reserve_push_back(end_pos - start_pos);
                for (auto& i : reverse_range(start_pos, end_pos))
                    if (check_fn(i))
                        res.push_back(i);
            } else {
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                res.reserve_push_back(end_pos - start_pos);
                for (auto& i : range(start_pos, end_pos))
                    if (check_fn(i))
                        res.push_back(i);
            }
            res.shrink_to_fit();
            return res;
        }

        template <class _Fn>
        constexpr list_array<T, Allocator> whereI(_Fn check_fn) const {
            return whereI(0, _size, check_fn);
        }

        template <class _Fn>
        constexpr list_array<T, Allocator> whereI(size_t start_pos, _Fn&& check_fn) const {
            return whereI(start_pos, _size, check_fn);
        }

        template <class _Fn>
        constexpr list_array<T, Allocator> whereI(size_t start_pos, size_t end_pos, _Fn&& check_fn) const {
            list_array<T, Allocator> res;
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                res.reserve_push_back(end_pos - start_pos);
                size_t pos = end_pos;
                for (auto& i : reverse_range(start_pos, end_pos))
                    if (check_fn(i, pos--))
                        res.push_back(i);
            } else {
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                res.reserve_push_back(end_pos - start_pos);
                size_t pos = start_pos;
                for (auto& i : range(start_pos, end_pos))
                    if (check_fn(i, pos--))
                        res.push_back(i);
            }
            res.shrink_to_fit();
            return res;
        }

#pragma endregion
#pragma region for each

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEach(_Fn&& iterate_fn) & {
            for (T& i : *this)
                iterate_fn(i);
            return *this;
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEachI(_Fn&& iterate_fn) & {
            size_t pos = 0;
            for (T& i : *this)
                iterate_fn(i, pos++);
            return *this;
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEachR(_Fn&& iterate_fn) & {
            for (T& i : reverse_range(0, _size))
                iterate_fn(i);
            return *this;
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEachIR(_Fn&& iterate_fn) & {
            size_t pos = _size - 1;
            for (T& i : reverse_range(0, _size))
                iterate_fn(i, pos--);
            return *this;
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEach(size_t start_pos, _Fn&& iterate_fn) & {
            return forEach(start_pos, _size, iterate_fn);
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEachI(size_t start_pos, _Fn&& iterate_fn) & {
            return forEach(start_pos, _size, iterate_fn);
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEach(size_t start_pos, size_t end_pos, _Fn&& iterate_fn) & {
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                for (T& i : reverse_range(start_pos, end_pos))
                    iterate_fn(i);
            } else {
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                for (T& i : range(start_pos, end_pos))
                    iterate_fn(i);
            }
            return *this;
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEachI(size_t start_pos, size_t end_pos, _Fn&& iterate_fn) & {
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                size_t pos = end_pos - 1;
                for (T& i : reverse_range(start_pos, end_pos))
                    iterate_fn(i, pos--);
            } else {
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                size_t pos = start_pos;
                for (T& i : range(start_pos, end_pos))
                    iterate_fn(i, pos++);
            }
            return *this;
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEach(_Fn&& iterate_fn) && {
            for (T& i : *this)
                iterate_fn(std::move(i));
            return *this;
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEachI(_Fn&& iterate_fn) && {
            size_t pos = 0;
            for (T& i : *this)
                iterate_fn(std::move(i), pos++);
            return *this;
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEachR(_Fn&& iterate_fn) && {
            for (T& i : reverse_range(0, _size))
                iterate_fn(std::move(i));
            return *this;
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEachIR(_Fn&& iterate_fn) && {
            size_t pos = _size - 1;
            for (T& i : reverse_range(0, _size))
                iterate_fn(std::move(i), pos--);
            return *this;
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEach(size_t start_pos, _Fn&& iterate_fn) && {
            return forEach(start_pos, _size, iterate_fn);
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEachI(size_t start_pos, _Fn&& iterate_fn) && {
            return forEach(start_pos, _size, iterate_fn);
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEach(size_t start_pos, size_t end_pos, _Fn&& iterate_fn) && {
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                for (T& i : reverse_range(start_pos, end_pos))
                    iterate_fn(std::move(i));
            } else {
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                for (T& i : range(start_pos, end_pos))
                    iterate_fn(std::move(i));
            }
            return *this;
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEachI(size_t start_pos, size_t end_pos, _Fn&& iterate_fn) && {
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                size_t pos = end_pos - 1;
                for (T& i : reverse_range(start_pos, end_pos))
                    iterate_fn(std::move(i), pos--);
            } else {
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                size_t pos = start_pos;
                for (T& i : range(start_pos, end_pos))
                    iterate_fn(std::move(i), pos++);
            }
            return *this;
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEach(_Fn&& iterate_fn) const& {
            for (const T& i : *this)
                iterate_fn(i);
            return *this;
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEachI(_Fn&& iterate_fn) const& {
            size_t pos = 0;
            for (const T& i : *this)
                iterate_fn(i, pos++);
            return *this;
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEachR(_Fn&& iterate_fn) const& {
            for (const T& i : reverse_range(0, _size))
                iterate_fn(i);
            return *this;
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEachIR(_Fn&& iterate_fn) const& {
            size_t pos = _size - 1;
            for (const T& i : reverse_range(0, _size))
                iterate_fn(i, pos++);
            return *this;
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEach(size_t start_pos, _Fn&& iterate_fn) const& {
            return forEach(start_pos, _size, iterate_fn);
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEachI(size_t start_pos, _Fn&& iterate_fn) const& {
            return forEach(start_pos, _size, iterate_fn);
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEach(size_t start_pos, size_t end_pos, _Fn&& iterate_fn) const& {
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                for (const T& i : reverse_range(start_pos, end_pos))
                    iterate_fn(i);
            } else {
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                for (const T& i : range(start_pos, end_pos))
                    iterate_fn(i);
            }
            return *this;
        }

        template <class _Fn>
        constexpr list_array<T, Allocator>& forEachI(size_t start_pos, size_t end_pos, _Fn&& iterate_fn) const& {
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                size_t pos = end_pos - 1;
                for (const T& i : reverse_range(start_pos, end_pos))
                    iterate_fn(i, pos--);
            } else {
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                size_t pos = start_pos;
                for (const T& i : range(start_pos, end_pos))
                    iterate_fn(i, pos++);
            }
            return *this;
        }

#pragma endregion
#pragma region convert

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class _Fn>
        constexpr result_array convert(_Fn&& iterate_fn) const& {
            return convert<ConvertTo>(0, _size, iterate_fn);
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class _Fn>
        constexpr result_array convert(size_t start_pos, _Fn&& iterate_fn) const& {
            return convert<ConvertTo>(start_pos, _size, iterate_fn);
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class _Fn>
        constexpr result_array convert(size_t start_pos, size_t end_pos, _Fn&& iterate_fn) const& {
            result_array res;
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                res.reserve_push_back(end_pos - start_pos);
                for (auto& i : reverse_range(start_pos, end_pos))
                    res.push_back(iterate_fn(i));
            } else {
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                res.reserve_push_back(end_pos - start_pos);
                for (auto& i : range(start_pos, end_pos))
                    res.push_back(iterate_fn(i));
            }
            return res;
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class _Fn>
        constexpr result_array convert(_Fn&& iterate_fn) && {
            return convert_take<ConvertTo, result_array>(0, _size, iterate_fn);
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class _Fn>
        constexpr result_array convert(size_t start_pos, _Fn&& iterate_fn) && {
            return convert_take<ConvertTo, result_array>(start_pos, _size, iterate_fn);
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class _Fn>
        constexpr result_array convert(size_t start_pos, size_t end_pos, _Fn&& iterate_fn) && {
            return convert_take<ConvertTo, result_array>(start_pos, end_pos, iterate_fn);
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class _Fn>
        constexpr result_array convert_take(_Fn&& iterate_fn) {
            return convert_take<ConvertTo, result_array>(0, _size, iterate_fn);
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class _Fn>
        constexpr result_array convert_take(size_t start_pos, _Fn&& iterate_fn) {
            return convert_take<ConvertTo, result_array>(start_pos, _size, iterate_fn);
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class _Fn>
        constexpr result_array convert_take(size_t start_pos, size_t end_pos, _Fn&& iterate_fn) {
            list_array<T, Allocator> tmp = take(start_pos, end_pos);
            result_array res;
            res.reserve(tmp.size());
            for (T& i : tmp)
                res.push_back(iterate_fn(std::move(i)));
            return res;
        }

#pragma endregion
#pragma region erase

        constexpr size_t erase(const T& val, size_t start = 0) {
            return remove_if(start, [&val](const T& cmp) { return cmp == val; });
        }

        constexpr size_t erase(const T& val, size_t start_pos, size_t end_pos) {
            return remove_if(start_pos, end_pos, [&val](const T& cmp) { return cmp == val; });
        }

        template <size_t arr_size>
        constexpr size_t erase(const T (&val)[arr_size], size_t start_pos = 0) {
            return remove_same(val, arr_size, start_pos, _size);
        }

        template <size_t arr_size>
        constexpr size_t erase(const T (&val)[arr_size], size_t start_pos, size_t end_pos) {
            return remove_same(val, arr_size, start_pos, end_pos);
        }

        constexpr size_t erase(const T* val, size_t val_size, size_t start_pos = 0) {
            return remove_same(val, val_size, start_pos, _size);
        }

        constexpr size_t erase(const T* val, size_t val_size, size_t start_pos, size_t end_pos) {
            return remove_same(val, val_size, start_pos, end_pos);
        }

        template <class AnyAllocator>
        constexpr size_t erase(const list_array<T, AnyAllocator>& range) {
            return remove_same(range, 0, range._size, 0, _size);
        }

        template <class AnyAllocator>
        constexpr size_t erase(const list_array<T, AnyAllocator>& range, size_t start_pos, size_t end_pos) {
            return remove_same(range, 0, range._size, start_pos, end_pos);
        }

        template <class AnyAllocator>
        constexpr size_t erase(const list_array<T, AnyAllocator>& range, size_t range_start, size_t range_end, size_t start_pos, size_t end_pos) {
            return remove_same(range, range_start, range_end, start_pos, end_pos);
        }

        constexpr size_t erase_one(const T& val, size_t start_pos = 0) {
            return remove_one(start_pos, _size, [&val](const T& cmp) { return cmp == val; });
        }

        constexpr size_t erase_one(const T& val, size_t start_pos, size_t end_pos) {
            return remove_one(start_pos, end_pos, [&val](const T& cmp) { return cmp == val; });
        }

        template <size_t arr_size>
        constexpr size_t erase_one(const T (&val)[arr_size], size_t start_pos = 0) {
            return erase_one(val, start_pos, _size);
        }

        template <size_t arr_size>
        constexpr size_t erase_one(const T (&val)[arr_size], size_t start_pos, size_t end_pos) {
            size_t pos = find(val, arr_size, start_pos, end_pos);
            if (pos != npos) {
                remove(pos, arr_size);
                return arr_size;
            }
            return 0;
        }

        constexpr size_t erase_one(const T* val, size_t val_size, size_t start_pos = 0) {
            return erase_one(val, val_size, start_pos, _size);
        }

        constexpr size_t erase_one(const T* val, size_t val_size, size_t start_pos, size_t end_pos) {
            size_t pos = find(val, val_size, start_pos, end_pos);
            if (pos != npos) {
                remove(pos, val_size);
                return val_size;
            }
            return 0;
        }

        template <class AnyAllocator>
        constexpr size_t erase_one(const list_array<T, AnyAllocator>& range) {
            return erase_one(range, 0, range._size, 0, _size);
        }

        template <class AnyAllocator>
        constexpr size_t erase_one(const list_array<T, AnyAllocator>& range, size_t start_pos, size_t end_pos) {
            return erase_one(range, 0, range._size, start_pos, end_pos);
        }

        template <class AnyAllocator>
        constexpr size_t erase_one(const list_array<T, AnyAllocator>& range, size_t range_start, size_t range_end, size_t start_pos, size_t end_pos) {
            size_t pos = find(range, range_start, range_end, start_pos, end_pos);
            if (pos != npos) {
                remove(pos, range_end - range_start);
                return range_end - range_start;
            }
            return 0;
        }

#pragma endregion
#pragma region starts/ends with

        template <size_t condition_size>
        constexpr bool starts_with(const T (&condition)[condition_size], size_t start_pos = 0) const {
            return starts_with(condition, condition_size, start_pos);
        }

        constexpr bool starts_with(const T* condition, size_t condition_size, size_t start_pos = 0) const {
            if (start_pos >= _size)
                return false;
            if (condition_size > _size - start_pos)
                return false;
            for (size_t i = 0; i < condition_size; i++)
                if (operator[](start_pos + i) != condition[i])
                    return false;
            return true;
        }

        constexpr bool starts_with(const T& condition, size_t start_pos = 0) const {
            if (start_pos >= _size)
                return false;
            return operator[](start_pos) == condition;
        }

        template <class AnyAllocator>
        constexpr bool starts_with(const list_array<T, AnyAllocator>& condition, size_t start_pos = 0) const {
            if (start_pos >= _size)
                return false;
            if (condition.size() > _size - start_pos)
                return false;
            for (size_t i = 0; i < condition.size(); i++)
                if (operator[](start_pos + i) != condition[i])
                    return false;
            return true;
        }

        template <size_t condition_size>
        constexpr bool ends_with(const T (&condition)[condition_size]) const {
            return ends_with<condition_size>(condition, condition_size, _size);
        }

        template <size_t condition_size>
        constexpr bool ends_with(const T (&condition)[condition_size], size_t end_pos) const {
            return ends_with<condition_size>(condition, condition_size, end_pos);
        }

        constexpr bool ends_with(const T* condition, size_t condition_size) const {
            return ends_with(condition, condition_size, _size);
        }

        constexpr bool ends_with(const T* condition, size_t condition_size, size_t end_pos) const {
            if (end_pos >= condition_size)
                return false;
            if (condition_size > end_pos)
                return false;
            for (size_t i = 0; i < condition_size; i++)
                if (operator[](end_pos - i - 1) != condition[condition_size - i - 1])
                    return false;
            return true;
        }

        constexpr bool ends_with(const T& condition) const {
            return ends_with(condition, _size);
        }

        constexpr bool ends_with(const T& condition, size_t end_pos) const {
            if (end_pos >= _size)
                return false;
            return operator[](end_pos - 1) == condition;
        }

        template <class AnyAllocator>
        constexpr bool ends_with(const list_array<T, AnyAllocator>& condition) const {
            return ends_with(condition, _size);
        }

        template <class AnyAllocator>
        constexpr bool ends_with(const list_array<T, AnyAllocator>& condition, size_t end_pos) const {
            if (end_pos >= _size)
                return false;
            if (condition.size() > end_pos)
                return false;
            for (size_t i = 0; i < condition.size(); i++)
                if (operator[](end_pos - i - 1) != condition[condition.size() - i - 1])
                    return false;
            return true;
        }

#pragma endregion
#pragma region range and iterators

        constexpr reverse_provider reverse() {
            return *this;
        }

        constexpr const_reverse_provider reverse() const {
            return *this;
        }

        constexpr range_provider range(size_t start, size_t end) {
            if (start > end)
                throw std::out_of_range("start > end");
            if (end > _size)
                throw std::out_of_range("end out of size limit");
            return range_provider(*this, start, end);
        }

        constexpr reverse_provider reverse_range(size_t start, size_t end) {
            if (start > end)
                throw std::out_of_range("start > end");
            if (end > _size)
                throw std::out_of_range("end out of size limit");
            return range_provider(*this, start, end);
        }

        constexpr const_range_provider range(size_t start, size_t end) const {
            if (start > end)
                throw std::out_of_range("start > end");
            if (end > _size)
                throw std::out_of_range("end out of size limit");

            return const_range_provider(*this, start, end);
        }

        constexpr const_reverse_provider reverse_range(size_t start, size_t end) const {
            if (start > end)
                throw std::out_of_range("start > end");
            if (end > _size)
                throw std::out_of_range("end out of size limit");
            return const_range_provider(*this, start, end);
        }

        constexpr iterator get_iterator(size_t pos) {
            if (pos > _size)
                throw std::out_of_range("pos out of size limit");
            return pair.hold_value.get_iterator(reserved_begin + pos);
        }

        constexpr const_iterator get_iterator(size_t pos) const {
            if (pos > _size)
                throw std::out_of_range("pos out of size limit");
            return pair.hold_value.get_iterator(reserved_begin + pos);
        }

        constexpr iterator begin() {
            return pair.hold_value.get_iterator(reserved_begin);
        }

        constexpr iterator end() {
            return pair.hold_value.get_iterator(reserved_begin + _size);
        }

        constexpr const_iterator begin() const {
            return pair.hold_value.get_iterator(reserved_begin);
        }

        constexpr const_iterator end() const {
            return pair.hold_value.get_iterator(reserved_begin + _size);
        }

        constexpr reverse_iterator rbegin() {
            return pair.hold_value.get_iterator(reserved_begin + _size);
        }

        constexpr reverse_iterator rend() {
            return pair.hold_value.get_iterator(reserved_begin);
        }

        constexpr const_reverse_iterator rbegin() const {
            return pair.hold_value.get_iterator(reserved_begin + _size);
        }

        constexpr const_reverse_iterator rend() const {
            return pair.hold_value.get_iterator(reserved_begin);
        }

        constexpr const_iterator cbegin() const {
            return pair.hold_value.get_iterator(reserved_begin);
        }

        constexpr const_iterator cend() const {
            return pair.hold_value.get_iterator(reserved_begin + _size);
        }

        constexpr const_reverse_iterator crbegin() const {
            return pair.hold_value.get_iterator(reserved_begin + _size);
        }

        constexpr const_reverse_iterator crend() const {
            return pair.hold_value.get_iterator(reserved_begin);
        }

#pragma endregion
#pragma region index

        constexpr inline T& operator[](size_t pos) {
            return pair.hold_value[reserved_begin + pos];
        }

        constexpr inline const T& operator[](size_t pos) const {
            return pair.hold_value[reserved_begin + pos];
        }

        constexpr T& at(size_t pos) {
            if (pos >= _size)
                throw std::out_of_range("pos out of size limit");
            return pair.hold_value[reserved_begin + pos];
        }

        constexpr const T& at(size_t pos) const {
            if (pos >= _size)
                throw std::out_of_range("pos out of size limit");
            return pair.hold_value[reserved_begin + pos];
        }

        constexpr T atDefault(size_t pos) const {
            if (pos >= _size)
                return value_tag_initializer{};
            return pair.hold_value[reserved_begin + pos];
        }

#pragma endregion
#pragma region view

        //editing with size change is not allowed when used raw pointer
        constexpr T* data() {
            if (blocks_more(1))
                commit();
            return pair.hold_value.arr->pair.hold_value;
        }

        constexpr const T* data() const {
            if (blocks_more(1))
                throw std::runtime_error("can't get const raw pointer when blocks more than 1");
            return pair.hold_value.arr->pair.hold_value;
        }

        constexpr const T& mmax() const {
            if (!_size)
                throw std::length_error("This list_array size is zero");
            const T* max = &operator[](0);
            for (const T& it : *this)
                if (*max < it)
                    max = &it;
            return *max;
        }

        constexpr const T& mmin() const {
            if (!_size)
                throw std::length_error("This list_array size is zero");
            const T* min = &operator[](0);
            for (const T& it : *this)
                if (*min > it)
                    min = &it;
            return *min;
        }

        constexpr T max_default() const
            requires(std::copy_constructible<T>)
        {
            if (!_size)
                return value_tag_initializer{};
            const T* max = &operator[](0);
            for (const T& it : *this)
                if (*max < it)
                    max = &it;
            return *max;
        }

        constexpr T min_default() const
            requires(std::copy_constructible<T>)
        {
            if (!_size)
                return value_tag_initializer{};
            const T* min = &operator[](0);
            for (const T& it : *this)
                if (*min > it)
                    min = &it;
            return *min;
        }

#pragma endregion
#pragma region to_...

        template <class LocalAllocator>
        constexpr T* to_array(LocalAllocator alloc = std::allocator<T>()) const {
            T* tmp = alloc.allocate(_size);
            begin()._fast_load<true>(tmp, _size);
            return tmp;
        }

#pragma endregion
#pragma region flip

        constexpr list_array<T, Allocator> flip_copy() const {
            return copy().flip();
        }

        constexpr list_array<T, Allocator>& flip() & {
            return *this = take().flip();
        }

        constexpr list_array<T, Allocator> flip() && {
            list_array<T, Allocator> cache(_size, get_allocator());
            take().forEachIR([&cache](T&& item, size_t pos) {
                cache[pos] = std::move(item);
            });
            return cache;
        }

#pragma endregion
#pragma region memory

        constexpr size_t allocated() const {
            return pair.hold_value._size * sizeof(T);
        }

        constexpr size_t reserved() const {
            return reserved_begin + reserved_end;
        }

        constexpr size_t reserved_back() const {
            return reserved_begin;
        }

        constexpr size_t reserved_front() const {
            return reserved_end;
        }

        constexpr void reserve_push_front(size_t reserve_size) {
            if (!reserve_size)
                return;
            reserved_begin += reserve_size;
            pair.hold_value.resize_begin(reserved_begin + _size + reserved_end);
        }

        constexpr void reserve_push_back(size_t reserve_size) {
            if (!reserve_size)
                return;
            reserved_end += reserve_size;
            pair.hold_value.resize_front(reserved_begin + _size + reserved_end);
        }

        constexpr void reserve(size_t reserve_size) {
            reserve_push_back(reserve_size);
        }

        constexpr size_t size() const {
            return _size;
        }

        template <bool do_shrink = false>
        constexpr void resize(size_t new_size) {
            if (new_size == 0)
                clear();
            else {
                bool resize_mode = reserved_end || !reserved_begin;
                if (resize_mode)
                    pair.hold_value.resize_front(reserved_begin + new_size);
                else
                    pair.hold_value.resize_begin(new_size + reserved_end);
                reserved_end = 0;
                if constexpr (do_shrink) {
                    if (resize_mode)
                        pair.hold_value.resize_begin(new_size);
                    else
                        pair.hold_value.resize_front(new_size);
                    reserved_begin = 0;
                }
            }
            _size = new_size;
        }

        template <bool do_shrink = false>
        constexpr void resize(size_t new_size, const T& auto_init) {
            size_t old_size = _size;
            resize(new_size);
            if (_size > old_size)
                for (auto& it : range(old_size, new_size))
                    it = auto_init;
        }

        constexpr bool empty() const {
            return !_size;
        }

        constexpr void clear() {
            pair.hold_value.clear();
            _size = reserved_end = reserved_begin = 0;
        }

        constexpr void shrink_to_fit() {
            resize<true>(_size);
        }

        //index optimization
        constexpr void commit() {
            T* tmp = pair.allocate(_size);
            begin()._fast_load<true>(tmp, _size);
            pair.hold_value.clear();
            pair.hold_value.arr = pair.hold_value.arr_end = new arr_block<T, Allocator>(get_allocator());
            pair.hold_value.arr->pair.hold_value = tmp;
            pair.hold_value.arr->_size = _size;
            pair.hold_value.allocator_a_size.hold_value = _size;
            reserved_begin = reserved_end = 0;
        }

        //insert and remove optimization
        constexpr void decommit(size_t total_blocks) {
            if (total_blocks > _size)
                throw std::out_of_range("blocks count more than elements count");
            if (total_blocks == 0)
                throw std::out_of_range("blocks count cannot be 0");
            if (total_blocks == 1)
                return commit();
            list_array<T, Allocator> tmp;
            size_t avg_block_len = _size / total_blocks;
            size_t last_block_add_len = _size % total_blocks;
            tmp.pair.hold_value.arr = tmp.pair.hold_value.arr_end = new arr_block<T, Allocator>(nullptr, avg_block_len, nullptr);
            size_t block_iterator = 0;
            size_t new_total_blocks = 1;
            auto cur_iterator = begin();
            for (size_t i = 0; i < _size; i++) {
                if (block_iterator >= avg_block_len) {
                    if (new_total_blocks >= total_blocks) {
                        tmp.pair.hold_value.arr_end->resize_front(tmp.pair.hold_value.arr_end->_size + last_block_add_len);
                        for (size_t j = 0; j < last_block_add_len; j++)
                            tmp.pair.hold_value.arr_end->pair.hold_value[avg_block_len + j] = operator[](i++);
                        break;
                    } else {
                        block_iterator = 0;
                        new_total_blocks++;
                        tmp.pair.hold_value.arr_end = new arr_block<T, Allocator>(tmp.pair.hold_value.arr_end, avg_block_len, nullptr);
                    }
                }
                tmp.pair.hold_value.arr_end->pair.hold_value[block_iterator++] = (*cur_iterator);
                ++cur_iterator;
            }
            pair.hold_value.clear();
            pair.hold_value.arr = tmp.pair.hold_value.arr;
            pair.hold_value.arr_end = tmp.pair.hold_value.arr_end;
            tmp.pair.hold_value.arr = tmp.pair.hold_value.arr_end = nullptr;
            pair.hold_value._size = _size;
            reserved_begin = reserved_end = 0;
        }

        constexpr bool need_commit() const {
            return pair.hold_value.arr != pair.hold_value.arr_end && pair.hold_value.arr->next_ != pair.hold_value.arr_end;
        }

        constexpr bool blocks_more(size_t blocks_count) const {
            const arr_block<T, Allocator>* block = pair.hold_value.arr;
            size_t res = 0;
            while (block) {
                if (++res > blocks_count)
                    return true;
                block = block->next_;
            }
            return false;
        }

        constexpr size_t blocks_count() const {
            const arr_block<T, Allocator>* block = pair.hold_value.arr;
            size_t res = 0;
            while (block) {
                ++res;
                block = block->next_;
            }
            return res;
        }

#pragma endregion

        Allocator& get_allocator() {
            return pair.get_allocator();
        }

        const Allocator& get_allocator() const {
            return pair.get_allocator();
        }
    };
}

template <class T, class Allocator = std::allocator<T>>
using list_array = __list_array_impl::list_array<T, Allocator>;

template <class Allocator = std::allocator<uint8_t>>
struct bit_list_array {
    list_array<uint8_t, Allocator> arr;
    uint8_t begin_bit : 4;
    uint8_t end_bit : 4;

    class bit_refrence {
        uint8_t& byte;
        uint8_t bit;

    public:
        constexpr bit_refrence(uint8_t& byte, uint8_t bit)
            : byte(byte), bit(bit) {}

        constexpr operator bool() const {
            return (byte >> bit) & 1;
        }

        constexpr bit_refrence& operator=(bool val) {
            if (val)
                byte |= 1 << bit;
            else
                byte &= ~(1 << bit);
            return *this;
        }

        constexpr bit_refrence& operator&=(bool val) {
            if (!val)
                byte &= ~(1 << bit);
            return *this;
        }

        constexpr bit_refrence& operator|=(bool val) {
            if (val)
                byte |= 1 << bit;
            return *this;
        }

        constexpr bit_refrence& operator^=(bool val) {
            if (val)
                byte ^= 1 << bit;
            return *this;
        }
    };

public:
    constexpr bit_list_array(const Allocator& alloc = Allocator())
        : begin_bit(0), end_bit(0), arr(alloc) {}

    constexpr bit_list_array(size_t size, const Allocator& alloc = Allocator())
        : arr(size / 8 + (size % 8 ? 1 : 0), alloc), begin_bit(0), end_bit(0) {}

    constexpr bit_list_array(const bit_list_array& copy, const Allocator& alloc = Allocator())
        : arr(copy.arr, alloc), begin_bit(copy.begin_bit), end_bit(copy.end_bit) {}

    constexpr bit_list_array(bit_list_array&& move) noexcept {
        arr.swap(move.arr);
        begin_bit = move.begin_bit;
        end_bit = move.end_bit;
    }

    constexpr size_t size() const {
        return (arr.size() - 1) * 8 + end_bit - begin_bit;
    }

    constexpr bit_list_array& operator=(const bit_list_array& copy) {
        arr = copy.arr;
        begin_bit = copy.begin_bit;
        end_bit = copy.end_bit;
        return *this;
    }

    constexpr bit_list_array& operator=(bit_list_array&& move) noexcept {
        arr.swap(move.arr);
        begin_bit = move.begin_bit;
        end_bit = move.end_bit;
        return *this;
    }

    constexpr void push_back(bool val) {
        if (end_bit == 8) {
            arr.push_back(0);
            end_bit = 0;
        }
        arr[arr.size() - 1] |= val << end_bit++;
    }

    constexpr void pop_back() {
        if (end_bit == 0) {
            arr.pop_back();
            end_bit = 8;
            return;
        }
        end_bit--;
        arr[arr.size() - 1] &= ~(1 << end_bit);
    }

    constexpr void push_front(bool val) {
        if (begin_bit == 0) {
            arr.push_front(0);
            begin_bit = 8;
        }
        arr[0] |= val << --begin_bit;
    }

    constexpr void pop_front() {
        if (begin_bit == 8) {
            arr.pop_front();
            begin_bit = 0;
            return;
        }
        arr[0] &= ~(1 << begin_bit++);
    }

    constexpr bool at(size_t pos) const {
        size_t byte = pos / 8;
        size_t bit = pos % 8;
        if (byte >= arr.size())
            throw std::out_of_range("pos out of size limit");
        if (byte == 0 && bit < begin_bit)
            throw std::out_of_range("pos out of size limit");
        if (byte == arr.size() - 1 && bit >= end_bit)
            throw std::out_of_range("pos out of size limit");
        return (arr[byte] >> bit) & 1;
    }

    constexpr bool set(size_t pos, bool val) {
        size_t byte = pos / 8;
        size_t bit = pos % 8;
        if (byte >= arr.size())
            throw std::out_of_range("pos out of size limit");
        if (byte == 0 && bit < begin_bit)
            throw std::out_of_range("pos out of size limit");
        if (byte == arr.size() - 1 && bit >= end_bit)
            throw std::out_of_range("pos out of size limit");
        bool res = (arr[byte] >> bit) & 1;
        if (val)
            arr[byte] |= 1 << bit;
        else
            arr[byte] &= ~(1 << bit);
        return res;
    }

    constexpr bit_refrence operator[](size_t pos) {
        size_t byte = pos / 8;
        size_t bit = pos % 8;
        if (byte >= arr.size())
            throw std::out_of_range("pos out of size limit");
        if (byte == 0 && bit < begin_bit)
            throw std::out_of_range("pos out of size limit");
        if (byte == arr.size() - 1 && bit >= end_bit)
            throw std::out_of_range("pos out of size limit");

        return bit_refrence(arr[byte], bit);
    }

    constexpr void clear() {
        arr.clear();
        begin_bit = 0;
        end_bit = 0;
    }

    constexpr void resize(size_t size) {
        arr.resize(size / 8 + (size % 8 ? 1 : 0));
    }

    constexpr void reserve_push_back(size_t size) {
        arr.reserve_push_back(size / 8 + (size % 8 ? 1 : 0));
    }

    constexpr void reserve_push_front(size_t size) {
        arr.reserve_push_front(size / 8 + (size % 8 ? 1 : 0));
    }

    constexpr void commit() {
        //if array contains unused bits, then shift all bits to the left
        if (begin_bit != 0) {
            bit_list_array tmp;
            size_t _size = size();
            tmp.reserve_push_back(_size);
            for (size_t i = 0; i < _size; i++)
                tmp.push_back(at(i));
            *this = std::move(tmp);
        } else {
            arr.commit();
        }
    }

    constexpr void swap(bit_list_array& to_swap) noexcept {
        if (this != &to_swap) {
            arr.swap(to_swap.arr);
            uint8_t tmp = begin_bit;
            begin_bit = to_swap.begin_bit;
            to_swap.begin_bit = tmp;
            tmp = end_bit;
            end_bit = to_swap.end_bit;
            to_swap.end_bit = tmp;
        }
    }

    constexpr bit_list_array& flip() {
        for (size_t i = 0; i < arr.size(); i++)
            arr[i] = ~arr[i];
        return *this;
    }

    constexpr bit_list_array operator~() const {
        return bit_list_array(*this).flip();
    }

    constexpr bit_list_array& operator&=(const bit_list_array& to_and) {
        size_t to_and_size = to_and.size();
        size_t this_size = size();
        size_t min_size = this_size < to_and_size ? this_size : to_and_size;
        for (size_t i = 0; i < min_size; i++)
            operator[](i) &= to_and.at(i);
        if (this_size > to_and_size) {
            size_t to_zero_bytes = (this_size - min_size) / 8;
            size_t to_zero_bits = (this_size - min_size) % 8;
            for (size_t i = 0; i < to_zero_bytes; i++)
                arr[to_and_size + i] = 0;
            for (size_t i = 0; i < to_zero_bits; i++)
                arr[to_and_size] &= ~(1 << i);
        }
        return *this;
    }

    constexpr bit_list_array operator&(const bit_list_array& to_and) const {
        return bit_list_array(*this) &= to_and;
    }

    constexpr bit_list_array& operator|=(const bit_list_array& to_or) {
        size_t to_or_size = to_or.size();
        size_t this_size = size();
        size_t min_size = this_size < to_or_size ? this_size : to_or_size;
        for (size_t i = 0; i < min_size; i++)
            operator[](i) |= to_or.at(i);
        if (this_size < to_or_size) {
            size_t to_or_bytes = (to_or_size - min_size) / 8;
            size_t to_or_bits = (to_or_size - min_size) % 8;
            for (size_t i = 0; i < to_or_bytes; i++)
                push_back(to_or.arr[min_size + i]);
            for (size_t i = 0; i < to_or_bits; i++)
                push_back(to_or.arr[min_size] & (1 << i));
        }
        return *this;
    }

    constexpr bit_list_array operator|(const bit_list_array& to_or) const {
        return bit_list_array(*this) |= to_or;
    }

    constexpr bit_list_array& operator^=(const bit_list_array& to_xor) {
        size_t to_xor_size = to_xor.size();
        size_t this_size = size();
        size_t min_size = this_size < to_xor_size ? this_size : to_xor_size;
        for (size_t i = 0; i < min_size; i++)
            operator[](i) ^= to_xor.at(i);
        if (this_size < to_xor_size) {
            size_t to_xor_bytes = (to_xor_size - min_size) / 8;
            size_t to_xor_bits = (to_xor_size - min_size) % 8;
            for (size_t i = 0; i < to_xor_bytes; i++)
                push_back(to_xor.arr[min_size + i]);
            for (size_t i = 0; i < to_xor_bits; i++)
                push_back(to_xor.arr[min_size] & (1 << i));
        }
        return *this;
    }

    constexpr bit_list_array operator^(const bit_list_array& to_xor) const {
        return bit_list_array(*this) ^= to_xor;
    }

    constexpr bool operator==(const bit_list_array& to_cmp) const {
        size_t this_size = size();
        size_t to_cmp_size = to_cmp.size();
        size_t min_size = this_size < to_cmp_size ? this_size : to_cmp_size;
        for (size_t i = 0; i < min_size; i++)
            if (at(i) != to_cmp.at(i))
                return false;
        if (this_size != to_cmp_size)
            return false;
        return true;
    }

    constexpr bool operator!=(const bit_list_array& to_cmp) const {
        return !operator==(to_cmp);
    }

    constexpr const list_array<uint8_t>& data() const {
        return arr;
    }

    constexpr list_array<uint8_t>& data() {
        return arr;
    }
};

namespace std {
    template <class B, class Allocator>
    struct hash<list_array<B, Allocator>> {
        constexpr size_t operator()(const list_array<B, Allocator>& list) {
            std::hash<B> hasher;
            size_t res = 0;
            for (auto& it : list)
                res ^= hasher(it) + 0x9e3779b9 + (res << 6) + (res >> 2);
            return res;
        }
    };
}

#endif
