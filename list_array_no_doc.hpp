#ifndef LIST_ARRAY
#define LIST_ARRAY
#include <algorithm>
#include <concepts>
#include <cstring>
#include <functional>
#include <iterator>
#include <memory>
#include <stdexcept>
#include <stdint.h>
#include <type_traits>
#include <utility>

//same as list_array.hpp but applied regex `\/\*\*([\W\w]*?)\*\/` and `[ ]*\/\/.*`

namespace _list_array_impl {
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
        using container_type = typename T::value_type;
        using container = T;
    };

    template <typename T, typename _ = void>
    struct can_direct_index : std::false_type {};

    template <typename T>
    struct can_direct_index<
        T,
        std::conditional_t<
            false,
            conditions_helper<decltype(std::declval<T>().data())>,
            void>> : public std::true_type {};

    template <typename FN, typename>
    struct is_apply_invocable : std::false_type {};

    template <typename FN, typename... T>
    struct is_apply_invocable<FN, std::tuple<T...>> : std::bool_constant<std::is_invocable_v<FN, std::add_lvalue_reference_t<T>...> || std::is_invocable_v<FN, std::add_rvalue_reference_t<T>...>> {
    };

    template <typename FN, typename... T>
    struct is_apply_invocable<FN, std::pair<T...>> : std::bool_constant<std::is_invocable_v<FN, std::add_lvalue_reference_t<T>...> || std::is_invocable_v<FN, std::add_rvalue_reference_t<T>...>> {
    };

    template <typename Ret, class FN, typename>
    struct is_apply_invocable_r : std::false_type {};

    template <typename Ret, typename FN, typename... T>
    struct is_apply_invocable_r<Ret, FN, std::tuple<T...>> : std::bool_constant<std::is_invocable_r_v<Ret, FN, std::add_lvalue_reference_t<T>...> || std::is_invocable_r_v<Ret, FN, std::add_rvalue_reference_t<T>...>> {
    };

    template <typename Ret, typename FN, typename... T>
    struct is_apply_invocable_r<Ret, FN, std::pair<T...>> : std::bool_constant<std::is_invocable_r_v<Ret, FN, std::add_lvalue_reference_t<T>...> || std::is_invocable_r_v<Ret, FN, std::add_rvalue_reference_t<T>...>> {
    };

    template <typename T>
    static inline constexpr bool can_direct_index_v = can_direct_index<T>::value;

    template <typename T>
    static inline constexpr bool is_container_v = is_container<T>::value;


    template <typename FN, typename Tupple>
    static inline constexpr bool is_apply_invocable_v = is_apply_invocable<FN, Tupple>::value;

    template <typename Ret, typename FN, typename Tupple>
    static inline constexpr bool is_apply_invocable_r_v = is_apply_invocable_r<Ret, FN, Tupple>::value;

    template <class T>
    concept is_equality_comparable = requires(const T& it) { it == it; } || requires(const T it) { it == it; };


    template <class T0, class T1>
    concept is_equality_comparable_with = requires(const T0& it0, const T0& it1) { it0 == it1; };

    template <class Alloc, class T, bool = std::is_empty_v<Alloc> && !std::is_final_v<Alloc>>
    struct compressed_allocator final : private Alloc {
    public:
        T hold_value{};

        template <class... Args>
        constexpr compressed_allocator(const Alloc& allocator, Args&&... args)
            : Alloc(allocator), hold_value(std::forward<Args>(args)...) {}

        constexpr compressed_allocator(compressed_allocator<Alloc, T>&& move)
            : Alloc(std::move(move)), hold_value(std::move(move.hold_value)) {}

        constexpr compressed_allocator(const compressed_allocator<Alloc, T>& move)
            : Alloc(move), hold_value(move.hold_value) {}

        using value_type = typename Alloc::value_type;
        using size_type = typename Alloc::size_type;

        constexpr value_type* allocate(size_type n) {
            return Alloc::allocate(n);
        }

        constexpr void deallocate(value_type* p, size_type n) {
            Alloc::deallocate(p, n);
        }

        constexpr Alloc& get_allocator() {
            return *this;
        }

        constexpr const Alloc& get_allocator() const {
            return *this;
        }

        constexpr compressed_allocator<Alloc, T>& operator=(const compressed_allocator<Alloc, T>& allocator) {
            if constexpr (std::allocator_traits<Alloc>::propagate_on_container_copy_assignment::value) {
                Alloc::operator=(allocator);
            }
            hold_value = allocator.hold_value;
            return *this;
        }

        constexpr compressed_allocator<Alloc, T>& operator=(compressed_allocator<Alloc, T>&& allocator) {
            if constexpr (std::allocator_traits<Alloc>::propagate_on_container_move_assignment::value) {
                Alloc::operator=(std::move(allocator));
            }
            hold_value = std::move(allocator.hold_value);
            return *this;
        }
    };

    template <class Alloc, class T>
    struct compressed_allocator<Alloc, T, false> final {
        Alloc allocator;

    public:
        T hold_value{};

        template <class... Args>
        constexpr compressed_allocator(const Alloc& allocator, Args&&... args)
            : allocator(allocator), hold_value(std::forward<Args>(args)...) {}

        constexpr compressed_allocator(compressed_allocator<Alloc, T>&& move)
            : allocator(move), hold_value(std::move(move.hold_value)) {}

        using value_type = typename Alloc::value_type;
        using size_type = typename Alloc::size_type;

        constexpr value_type* allocate(size_type n) {
            return allocator.allocate(n);
        }

        constexpr void deallocate(value_type* p, size_type n) {
            allocator.deallocate(p, n);
        }

        constexpr Alloc& get_allocator() {
            return allocator;
        }

        constexpr const Alloc& get_allocator() const {
            return allocator;
        }

        constexpr compressed_allocator<Alloc, T>& operator=(const compressed_allocator<Alloc, T>& allocator) {
            if constexpr (std::allocator_traits<Alloc>::propagate_on_container_copy_assignment::value) {
                this->allocator = allocator;
            }
            hold_value = allocator.hold_value;
            return *this;
        }

        constexpr compressed_allocator<Alloc, T>& operator=(compressed_allocator<Alloc, T>&& allocator) {
            if constexpr (std::allocator_traits<Alloc>::propagate_on_container_move_assignment::value) {
                this->allocator = std::move(allocator);
            }
            hold_value = std::move(allocator.hold_value);
            return *this;
        }
    };

    template <class T, class Allocator>
    struct auto_deallocate {
        T* data;
        Allocator& allocator;
        size_t size;

        constexpr auto_deallocate(T* data, Allocator& allocator, size_t size)
            : data(data), allocator(allocator), size(size) {}

        ~auto_deallocate() {
            if (data)
                allocator.deallocate(data, size);
        }

        constexpr void release() noexcept {
            data = nullptr;
        }
    };

    class bit_array_helper {
        uint8_t* data;
        size_t _set_values = 0;

    public:
        constexpr bit_array_helper(size_t size) : data(new uint8_t[(size + 7) / 8]) {
            size_t compressed_size = (size + 7) / 8;
            for (size_t i = 0; i < compressed_size; i++)
                data[i] = 0;
        }

        ~bit_array_helper() {
            delete[] data;
        }

        constexpr void set(size_t index, bool value) noexcept {
            if (get(index) != value) {
                if (value)
                    _set_values++;
                else
                    _set_values--;
            }
            if (value)
                data[index / 8] |= 1 << (index % 8);
            else
                data[index / 8] &= ~(1 << (index % 8));
        }

        constexpr bool get(size_t index) const noexcept {
            return data[index / 8] & (1 << (index % 8));
        }

        constexpr size_t set_values() const noexcept {
            return _set_values;
        }
    };

    template <class T, class Allocator>
    class custom_unique_ptr {
        T* data;
        Allocator& allocator;
        void (*deleter)(T*, Allocator&);

    public:
        constexpr custom_unique_ptr(T* data, Allocator& allocator, void (*deleter)(T*, Allocator&))
            : data(data), allocator(allocator), deleter(deleter) {}

        ~custom_unique_ptr() {
            if (data)
                deleter(data, allocator);
            data = 0;
        }

        constexpr T* get() const noexcept {
            return data;
        }

        constexpr T* release() noexcept {
            T* temp = data;
            data = nullptr;
            return temp;
        }

        constexpr T* operator->() const noexcept {
            return data;
        }

        constexpr T& operator*() const noexcept {
            return *data;
        }
    };

    template <class T>
    struct arr_block {
        arr_block<T>* next;
        arr_block<T>* prev;
        T* data;
        size_t data_size;

        template <class Allocator>
        static constexpr arr_block<T>* create_block(Allocator& allocator, size_t size) {
            custom_unique_ptr block(new arr_block<T>, allocator, __destroy_block);
            auto_deallocate<T, Allocator> hold(allocator.allocate(size), allocator, size);
            block->data = hold.data;
            block->data_size = size;
            block->next = nullptr;
            block->prev = nullptr;
            hold.release();
            return block.release();
        }

        template <class Allocator>
        static constexpr void destroy_block(arr_block<T>* block, Allocator& allocator) {
            if (block->data)
                allocator.deallocate(block->data, block->data_size);
            delete block;
        }

        template <class Allocator>
        static constexpr auto create_safe_block(Allocator& allocator, size_t size) {
            return custom_unique_ptr(create_block(allocator, size), allocator, __destroy_block);
        }

    private:
        template <class Allocator>
        static constexpr void __destroy_block(arr_block<T>* block, Allocator& allocator) {
            if (block->data)
                allocator.deallocate(block->data, block->data_size);
            delete block;
        }
    };

    template <class T, class Allocator>
    class arr_block_manager {
        arr_block<T>* first = nullptr;
        arr_block<T>* last = nullptr;
        Allocator& allocator;

    public:
        constexpr arr_block_manager(Allocator& allocator) noexcept
            : allocator(allocator) {}

        ~arr_block_manager() {
            clear();
        }

        constexpr void add_block(arr_block<T>* block) noexcept {
            if (!first) {
                first = last = block;
            } else {
                last->next = block;
                block->prev = last;
                last = block;
            }
        }

        constexpr arr_block<T>* allocate_and_take_from(size_t block_size, auto& iter) {
            auto block = arr_block<T>::create_safe_block(allocator, block_size);
            size_t constructed_at = 0;
            try {
                for (size_t i = 0; i < block_size; ++i, ++iter, ++constructed_at)
                    std::construct_at(block->data + i, std::move(*iter));
            } catch (...) {
                for (size_t j = 0; j < constructed_at; ++j)
                    std::destroy_at(block->data + j);
                throw;
            }
            return block.release();
        }

        constexpr void clear() {
            while (first) {
                auto temp = first;
                first = first->next;
                for (size_t k = 0; k < temp->data_size; ++k) {
                    std::destroy_at(temp->data + k);
                }
                arr_block<T>::destroy_block(temp, allocator);
            }
        }

        constexpr arr_block<T>* get_first() const noexcept {
            return first;
        }

        constexpr arr_block<T>* get_last() const noexcept {
            return last;
        }

        constexpr void release() noexcept {
            first = last = nullptr;
        }
    };

    template <class T, class Allocator>
    class list_array {
    public:
        class const_iterator {
            friend class list_array<T, Allocator>;
            arr_block<T>* block;
            size_t relative_index;
            size_t absolute_index;

        public:
            using iterator_category = std::bidirectional_iterator_tag;
            using value_type = T;
            using difference_type = ptrdiff_t;
            using pointer = const T*;
            using reference = const T&;

            constexpr const_iterator(arr_block<T>* block, size_t relative_index, size_t absolute_index) noexcept
                : block(block), relative_index(relative_index), absolute_index(absolute_index) {}

            constexpr const_iterator(const const_iterator& iterator) noexcept
                : block(iterator.block), relative_index(iterator.relative_index), absolute_index(iterator.absolute_index) {}

            constexpr const_iterator& operator=(const_iterator iterator) noexcept {
                block = iterator.block;
                relative_index = iterator.relative_index;
                absolute_index = iterator.absolute_index;
                return *this;
            }

            constexpr const_iterator& operator++() noexcept {
                if (block) {
                    if (++relative_index >= block->data_size) {
                        block = block->next;
                        relative_index = 0;
                    }
                }
                absolute_index++;
                return *this;
            }

            constexpr const_iterator& operator--() noexcept {
                if (block) {
                    if (relative_index-- == 0) {
                        block = block->prev;
                        if (block)
                            relative_index = block->data_size - 1;
                        else
                            relative_index = 0;
                    }
                }
                absolute_index--;
                return *this;
            }

            constexpr const_iterator& operator+=(size_t add) {
                size_t remaining = add;
                absolute_index += add;
                while (remaining > 0) {
                    size_t space_left = block->data_size - relative_index;
                    if (space_left > remaining) {
                        relative_index += remaining;
                        return *this;
                    }
                    remaining -= space_left;
                    block = block->next;
                    relative_index = 0;
                }
                return *this;
            }

            constexpr const_iterator& operator-=(size_t sub) {
                if (sub > absolute_index)
                    throw std::out_of_range("list_array::const_iterator::operator-=: sub out of range");
                size_t remaining = sub;
                absolute_index -= sub;
                while (remaining > 0) {
                    if (relative_index >= remaining) {
                        relative_index -= remaining;
                        return *this;
                    }
                    remaining -= relative_index + 1;
                    block = block->prev;
                    relative_index = block->data_size - 1;
                }
                return *this;
            }

            constexpr const_iterator operator+(size_t add) const {
                const_iterator copy = *this;
                copy += add;
                return copy;
            }

            constexpr const_iterator operator-(size_t sub) const {
                const_iterator copy = *this;
                copy -= sub;
                return copy;
            }

            constexpr const_iterator operator++(int) noexcept {
                const_iterator copy = *this;
                ++(*this);
                return copy;
            }

            constexpr const_iterator operator--(int) noexcept {
                const_iterator copy = *this;
                --(*this);
                return copy;
            }

            constexpr const T& operator*() const noexcept {
                return block->data[relative_index];
            }

            constexpr const T* operator->() const noexcept {
                return &block->data[relative_index];
            }

            constexpr bool operator==(const const_iterator& other) const noexcept {
                return absolute_index == other.absolute_index;
            }

            constexpr bool operator!=(const const_iterator& other) const noexcept {
                return absolute_index != other.absolute_index;
            }

            ptrdiff_t operator-(const const_iterator& other) {
                return ptrdiff_t(absolute_index) - other.absolute_index;
            }
        };

        class iterator {
            friend class list_array<T, Allocator>;
            arr_block<T>* block;
            size_t relative_index;
            size_t absolute_index;

            template <bool copy_construct, bool make_move = false>
            constexpr void _fast_load(T* arr, size_t arr_size) {
                size_t j = relative_index;
                arr_block<T>* block_tmp = block;
                size_t block_size = block_tmp->data_size;
                T* block_arr = block_tmp->data;

                for (size_t i = 0; i < arr_size;) {
                    for (; i < arr_size && j < block_size; j++) {
                        if constexpr (make_move) {
                            if constexpr (copy_construct)
                                std::construct_at(arr + i++, std::move(block_arr[j]));
                            else
                                arr[i++] = std::move(block_arr[j]);
                        } else {
                            if constexpr (copy_construct)
                                std::construct_at(arr + i++, block_arr[j]);
                            else
                                arr[i++] = block_arr[j];
                        }
                    }
                    j = 0;
                    block_tmp = block_tmp->next;
                    if (!block_tmp)
                        return;
                    block_size = block_tmp->data_size;
                    block_arr = block_tmp->data;
                }
            }

        public:
            using iterator_category = std::bidirectional_iterator_tag;
            using value_type = T;
            using difference_type = ptrdiff_t;
            using pointer = T*;
            using reference = T&;

            constexpr iterator(arr_block<T>* block, size_t relative_index, size_t absolute_index) noexcept
                : block(block), relative_index(relative_index), absolute_index(absolute_index) {}

            constexpr iterator(const iterator& iterator) noexcept
                : block(iterator.block), relative_index(iterator.relative_index), absolute_index(iterator.absolute_index) {}

            constexpr iterator& operator=(iterator iterator) noexcept {
                block = iterator.block;
                relative_index = iterator.relative_index;
                absolute_index = iterator.absolute_index;
                return *this;
            }

            constexpr iterator& operator++() noexcept {
                if (block) {
                    if (++relative_index >= block->data_size) {
                        block = block->next;
                        relative_index = 0;
                    }
                }
                absolute_index++;
                return *this;
            }

            constexpr iterator& operator--() noexcept {
                if (block) {
                    if (relative_index-- == 0) {
                        block = block->prev;
                        if (block)
                            relative_index = block->data_size - 1;
                        else
                            relative_index = 0;
                    }
                }
                absolute_index--;
                return *this;
            }

            constexpr iterator operator++(int) noexcept {
                iterator copy = *this;
                ++(*this);
                return copy;
            }

            constexpr iterator operator--(int) noexcept {
                iterator copy = *this;
                --(*this);
                return copy;
            }

            constexpr iterator& operator+=(size_t add) {
                size_t remaining = add;
                absolute_index += add;
                while (remaining > 0) {
                    size_t space_left = block->data_size - relative_index;
                    if (space_left > remaining) {
                        relative_index += remaining;
                        return *this;
                    }
                    remaining -= space_left;
                    block = block->next;
                    relative_index = 0;
                }
                return *this;
            }

            constexpr iterator& operator-=(size_t sub) {
                if (sub > absolute_index)
                    throw std::out_of_range("list_array::iterator::operator-=: sub out of range");
                size_t remaining = sub;
                absolute_index -= sub;
                while (remaining > 0) {
                    if (relative_index >= remaining) {
                        relative_index -= remaining;
                        return *this;
                    }
                    remaining -= relative_index + 1;
                    block = block->prev;
                    relative_index = block->data_size - 1;
                }
                return *this;
            }

            constexpr iterator operator+(size_t add) const {
                iterator copy = *this;
                copy += add;
                return copy;
            }

            constexpr iterator operator-(size_t sub) const {
                iterator copy = *this;
                copy -= sub;
                return copy;
            }

            constexpr T& operator*() {
                return block->data[relative_index];
            }

            constexpr T* operator->() {
                return &block->data[relative_index];
            }

            constexpr bool operator==(const iterator& other) const noexcept {
                return absolute_index == other.absolute_index;
            }

            constexpr bool operator!=(const iterator& other) const noexcept {
                return absolute_index != other.absolute_index;
            }

            constexpr operator const_iterator() const noexcept {
                return const_iterator(block, relative_index, absolute_index);
            }

            ptrdiff_t operator-(const iterator& other) {
                return ptrdiff_t(absolute_index) - other.absolute_index;
            }
        };

        class reverse_iterator {
            friend class list_array<T, Allocator>;
            arr_block<T>* block;
            size_t relative_index;
            size_t absolute_index;

        public:
            using iterator_category = std::bidirectional_iterator_tag;
            using value_type = T;
            using difference_type = ptrdiff_t;
            using pointer = T*;
            using reference = T&;

            constexpr reverse_iterator() {
                block = nullptr;
                relative_index = 0;
                absolute_index = 0;
            }

            constexpr reverse_iterator& operator=(const iterator& setter) {
                block = setter.block;
                relative_index = setter.relative_index;
                absolute_index = setter.absolute_index;
                return *this;
            }

            constexpr reverse_iterator(const iterator& copy) {
                *this = copy;
            }

            constexpr reverse_iterator& operator=(const reverse_iterator& setter) {
                block = setter.block;
                relative_index = setter.relative_index;
                absolute_index = setter.absolute_index;
                return *this;
            }

            constexpr reverse_iterator(const reverse_iterator& copy) {
                *this = copy;
            }

            constexpr reverse_iterator(arr_block<T>* block_pos, size_t relative_pos, size_t absolute_pos) {
                block = block_pos;
                relative_index = relative_pos;
                absolute_index = absolute_pos;
            }

            constexpr reverse_iterator& operator++() {
                if (block) {
                    if (0 == --relative_index) {
                        block = block->prev;
                        relative_index = block ? block->data_size : 0;
                    }
                    --absolute_index;
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
                    if (block->data_size == ++relative_index) {
                        block = block->next;
                        relative_index = 0;
                    }
                    ++absolute_index;
                }
                return *this;
            }

            constexpr reverse_iterator operator--(int) {
                reverse_iterator tmp = *this;
                operator--();
                return tmp;
            }

            constexpr reverse_iterator& operator+=(size_t sub) {
                if (sub > absolute_index)
                    throw std::out_of_range("list_array::reverse_iterator::operator+=: sub out of range");
                size_t remaining = sub;
                absolute_index -= sub;
                while (remaining > 0) {
                    if (relative_index >= remaining) {
                        relative_index -= remaining;
                        return *this;
                    }
                    remaining -= relative_index + 1;
                    block = block->prev;
                    relative_index = block->data_size - 1;
                }
                return *this;
            }

            constexpr reverse_iterator& operator-=(size_t add) {
                size_t remaining = add;
                absolute_index += add;
                while (remaining > 0) {
                    size_t space_left = block->data_size - relative_index;
                    if (space_left > remaining) {
                        relative_index += remaining;
                        return *this;
                    }
                    remaining -= space_left;
                    block = block->next;
                    relative_index = 0;
                }
                return *this;
            }

            constexpr reverse_iterator operator+(size_t add) const {
                reverse_iterator copy = *this;
                copy += add;
                return copy;
            }

            constexpr reverse_iterator operator-(size_t sub) const {
                reverse_iterator copy = *this;
                copy -= sub;
                return copy;
            }

            constexpr bool operator==(const reverse_iterator& comparer) const {
                return absolute_index == comparer.absolute_index;
            }

            constexpr bool operator!=(const reverse_iterator& comparer) const {
                return absolute_index != comparer.absolute_index;
            }

            constexpr T& operator*() {
                return block->data[relative_index - 1];
            }

            constexpr const T& operator*() const {
                return block->data[relative_index - 1];
            }

            constexpr T* operator->() {
                return block->data + relative_index - 1;
            }

            constexpr ptrdiff_t operator-(const reverse_iterator& other) {
                return ptrdiff_t(other.absolute_index) - absolute_index;
            }
        };

        class const_reverse_iterator {
            friend class list_array<T, Allocator>;
            arr_block<T>* block;
            size_t relative_index;
            size_t absolute_index;

        public:
            using iterator_category = std::bidirectional_iterator_tag;
            using value_type = T;
            using difference_type = ptrdiff_t;
            using pointer = const T*;
            using reference = const T&;

            constexpr const_reverse_iterator() {
                block = nullptr;
                relative_index = 0;
                absolute_index = 0;
            }

            constexpr const_reverse_iterator& operator=(const iterator& setter) {
                block = setter.block;
                relative_index = setter.relative_index;
                absolute_index = setter.absolute_index;
                return *this;
            }

            constexpr const_reverse_iterator(const iterator& copy) {
                *this = copy;
            }

            constexpr const_reverse_iterator& operator=(const const_iterator& setter) {
                block = setter.block;
                relative_index = setter.relative_index;
                absolute_index = setter.absolute_index;
                return *this;
            }

            constexpr const_reverse_iterator(const const_iterator& copy) {
                *this = copy;
            }

            constexpr const_reverse_iterator& operator=(const reverse_iterator& setter) {
                block = setter.block;
                relative_index = setter.relative_index;
                absolute_index = setter.absolute_index;
                return *this;
            }

            constexpr const_reverse_iterator(const reverse_iterator& copy) {
                *this = copy;
            }

            constexpr const_reverse_iterator& operator=(const const_reverse_iterator& setter) {
                block = setter.block;
                relative_index = setter.relative_index;
                absolute_index = setter.absolute_index;
                return *this;
            }

            constexpr const_reverse_iterator(const const_reverse_iterator& copy) {
                *this = copy;
            }

            constexpr const_reverse_iterator(arr_block<T>* block_pos, size_t relative_pos, size_t absolute_pos) {
                block = block_pos;
                relative_index = relative_pos;
                absolute_index = absolute_pos;
            }

            constexpr const_reverse_iterator& operator++() {
                if (block) {
                    if (0 == --relative_index) {
                        block = block->prev;
                        relative_index = block ? block->data_size : 0;
                    }
                    --absolute_index;
                }
                return *this;
            }

            constexpr const_reverse_iterator operator++(int) {
                const_reverse_iterator tmp = *this;
                operator++();
                return tmp;
            }

            constexpr reverse_iterator& operator--() {
                if (block) {
                    if (block->data_size == ++relative_index) {
                        block = block->next;
                        relative_index = 0;
                    }
                    ++absolute_index;
                }
                return *this;
            }

            constexpr const_reverse_iterator operator--(int) {
                const_reverse_iterator tmp = *this;
                operator--();
                return tmp;
            }

            constexpr const_reverse_iterator& operator+=(size_t sub) {
                if (sub > absolute_index)
                    throw std::out_of_range("list_array::const_reverse_iterator::operator+=: sub out of range");
                size_t remaining = sub;
                absolute_index -= sub;
                while (remaining > 0) {
                    if (relative_index >= remaining) {
                        relative_index -= remaining;
                        return *this;
                    }
                    remaining -= relative_index + 1;
                    block = block->prev;
                    relative_index = block->data_size - 1;
                }
                return *this;
            }

            constexpr const_reverse_iterator& operator-=(size_t add) {
                size_t remaining = add;
                absolute_index += add;
                while (remaining > 0) {
                    size_t space_left = block->data_size - relative_index;
                    if (space_left > remaining) {
                        relative_index += remaining;
                        return *this;
                    }
                    remaining -= space_left;
                    block = block->next;
                    relative_index = 0;
                }
                return *this;
            }

            constexpr const_reverse_iterator operator+(size_t add) const {
                const_reverse_iterator copy = *this;
                copy += add;
                return copy;
            }

            constexpr const_reverse_iterator operator-(size_t sub) const {
                const_reverse_iterator copy = *this;
                copy -= sub;
                return copy;
            }

            constexpr bool operator==(const const_reverse_iterator& comparer) const {
                return absolute_index == comparer.absolute_index;
            }

            constexpr bool operator!=(const const_reverse_iterator& comparer) const {
                return absolute_index != comparer.absolute_index;
            }

            constexpr const T& operator*() const {
                return block->data[relative_index - 1];
            }

            constexpr const T* operator->() const {
                return block->data + relative_index - 1;
            }

            constexpr ptrdiff_t operator-(const const_reverse_iterator& other) const {
                return ptrdiff_t(other.absolute_index) - absolute_index;
            }
        };

        using value_type = T;
        using reference = T&;
        using const_reference = const T&;
        using size_type = size_t;
        using difference_type = ptrdiff_t;
        static constexpr inline size_t npos = (size_t)-1;

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
                _begin = copy._begin;
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
                _begin = copy._begin;
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

    private:
        compressed_allocator<Allocator, size_t> allocator_and_size;
        arr_block<T>* first_block = nullptr;
        arr_block<T>* last_block = nullptr;
        size_t _reserved_front = 0;
        size_t _reserved_back = 0;

        constexpr inline size_t& _size() noexcept {
            return allocator_and_size.hold_value;
        }

        constexpr inline size_t _size() const noexcept {
            return allocator_and_size.hold_value;
        }

        constexpr T* get_element_at_index_front(size_t index) const noexcept {
            size_t i = index;
            arr_block<T>* block = first_block;
            while (i >= block->data_size) {
                i -= block->data_size;
                block = block->next;
            }
            return block->data + i;
        }

        constexpr T* get_element_at_index_back(size_t index) const noexcept {
            size_t i = index;
            arr_block<T>* block = last_block;
            while (i >= block->data_size) {
                i -= block->data_size;
                block = block->prev;
            }
            return block->data + block->data_size - i - 1;
        }

        constexpr T* get_element_at_index(size_t index) const noexcept {
            index += _reserved_front;
            return index < capacity() / 2
                       ? get_element_at_index_front(index)
                       : get_element_at_index_back(capacity() - index - 1);
        }

        constexpr T* get_direct_element_at_index(size_t index) const noexcept {
            return index < capacity() / 2
                       ? get_element_at_index_front(index)
                       : get_element_at_index_back(capacity() - index - 1);
        }

        constexpr iterator get_iterator_at_index_front(size_t index) noexcept {
            size_t i = index;
            arr_block<T>* block = first_block;
            if (!block)
                return iterator(nullptr, 0, index);
            while (i >= block->data_size) {
                i -= block->data_size;
                block = block->next;
                if (!block)
                    return iterator(nullptr, 0, index);
            }
            return iterator(block, i, index);
        }

        constexpr iterator get_iterator_at_index_back(size_t index, size_t absolute_index) noexcept {
            size_t i = index;
            arr_block<T>* block = last_block;
            if (!block)
                return iterator(nullptr, 0, absolute_index);
            if (absolute_index == _size())
                return iterator(last_block, block->data_size - (i + 1), absolute_index);

            while (i >= block->data_size) {
                i -= block->data_size;
                block = block->prev;
                if (!block)
                    return iterator(nullptr, 0, absolute_index);
            }
            return iterator(block, block->data_size - i - 1, absolute_index);
        }

        constexpr iterator get_iterator_at_index(size_t index) noexcept {
            return index < capacity() / 2
                       ? get_iterator_at_index_front(index + _reserved_front)
                       : get_iterator_at_index_back(capacity() - (index + _reserved_front) - 1, index + _reserved_front);
        }

        constexpr void steal_reserve_block_back_to_front() noexcept {
            if (first_block == last_block)
                return;
            auto l_block = last_block;
            last_block = last_block->prev;
            last_block->next = nullptr;
            l_block->prev = nullptr;

            l_block->next = first_block;
            first_block->prev = l_block;
            first_block = l_block;

            _reserved_front += l_block->data_size;
            _reserved_back -= l_block->data_size;
        }

        constexpr void steal_reserve_block_front_to_back() noexcept {
            if (first_block == last_block)
                return;
            auto f_block = first_block;
            first_block = first_block->next;
            first_block->prev = nullptr;
            f_block->next = nullptr;

            f_block->prev = last_block;
            last_block->next = f_block;
            last_block = f_block;

            _reserved_front -= f_block->data_size;
            _reserved_back += f_block->data_size;
        }

        constexpr static inline bool split_policy(size_t block_size) noexcept {
            if (block_size < 2048)
                return false;
            else
                return true;
        }

        constexpr size_t increase_policy() noexcept {
            size_t c = capacity();
            if (c == 0)
                return 64;
            if (c < 1024)
                return c;
            else if (c < 2048) {
                return c / 2;
            } else
                return c / 4;
        }

        constexpr std::pair<size_t, size_t> get_handle_range(const_iterator iter) noexcept {
            size_t block_absolute_start = iter.absolute_index - iter.relative_index;
            size_t block_absolute_end = block_absolute_start + iter.block->data_size;

            size_t handle_begin = std::max(block_absolute_start, _reserved_front);
            size_t handle_end = std::min(block_absolute_end, _size() + _reserved_front);

            if (handle_begin >= block_absolute_end || handle_end <= block_absolute_start)
                return {0, 0};

            size_t local_begin = handle_begin - block_absolute_start;
            size_t local_end = handle_end - block_absolute_start;
            return {local_begin, local_end};
        }

        constexpr std::pair<size_t, size_t> checked_get_handle_range(const_iterator iter) {
            auto [startConstructed, endConstructed] = get_handle_range(iter);
            if (iter.relative_index < startConstructed || iter.relative_index > endConstructed)
                throw std::out_of_range("Iterator is outside the constructed range.");
            return {startConstructed, endConstructed};
        }

        constexpr void check_handle_range(const_iterator iter) {
            checked_get_handle_range(iter);
        }

        constexpr void release_arr_block(arr_block<T>* block, size_t block_index) {
            auto [startConstructed, endConstructed] = get_handle_range(const_iterator(block, 0, block_index));
            if (block->prev)
                block->prev->next = block->next;
            else
                first_block = block->next;

            if (block->next)
                block->next->prev = block->prev;
            else
                last_block = block->prev;

            block->next = block->prev = nullptr;

            for (size_t i = startConstructed; i < endConstructed; ++i)
                std::destroy_at(block->data + i);
            arr_block<T>::destroy_block(block, allocator_and_size.get_allocator());
        }

        constexpr void swap_block_with_blocks(arr_block<T>* block, size_t handle_begin, size_t handle_end, arr_block<T>* swap_block_0, arr_block<T>* swap_block_1) {
            if (block->prev) {
                block->prev->next = swap_block_0;
                swap_block_0->prev = block->prev;
            } else {
                first_block = swap_block_0;
                swap_block_0->prev = nullptr;
            }

            if (block->next) {
                block->next->prev = swap_block_1;
                swap_block_1->next = block->next;
            } else {
                last_block = swap_block_1;
                swap_block_1->next = nullptr;
            }
            block->next = block->prev = nullptr;
            for (size_t i = handle_begin; i < handle_end; ++i)
                std::destroy_at(block->data + i);
            arr_block<T>::destroy_block(block, allocator_and_size.get_allocator());
        }

        constexpr void insert_between(arr_block<T>* to_insert, arr_block<T>* block_0, arr_block<T>* block_1) noexcept {
            block_0->next = to_insert;
            to_insert->prev = block_0;
            to_insert->next = block_1;
            block_1->prev = to_insert;
        }

        constexpr void swap_block(arr_block<T>* block_0, arr_block<T>* block_1) noexcept {
            if (block_0->prev)
                block_0->prev->next = block_1;
            else
                first_block = block_1;

            if (block_0->next)
                block_0->next->prev = block_1;
            else
                last_block = block_1;

            if (block_1->prev)
                block_1->prev->next = block_0;

            if (block_1->next)
                block_1->next->prev = block_0;

            std::swap(block_0->next, block_1->next);
            std::swap(block_0->prev, block_1->prev);
        }

#pragma region insertion

        template <class Ty>
        constexpr void insert_item_split(const_iterator iter, Ty&& item) {
            auto [handle_begin, handle_end] = get_handle_range(iter);
            if (iter.relative_index < handle_begin || iter.relative_index > handle_end)
                throw std::out_of_range("Insertion point is outside the constructed range.");

            size_t half_0_size = iter.relative_index;
            size_t half_1_size = iter.block->data_size - half_0_size;
            bool add_in_ = half_0_size > half_1_size;
            auto half_0 = arr_block<T>::create_safe_block(allocator_and_size.get_allocator(), half_0_size + !add_in_);
            auto half_1 = arr_block<T>::create_safe_block(allocator_and_size.get_allocator(), half_0_size + add_in_);
            arr_block<T>* to_swap = iter.block;
            size_t half_0_constructed = 0;
            size_t half_1_constructed = 0;

            try {
                for (size_t i = handle_begin; i < half_0_size && i < handle_end; ++half_0_constructed, i++) {
                    std::construct_at(half_0->data + i, std::move(to_swap->data[i]));
                    std::destroy_at(to_swap->data + i);
                }

                if (!add_in_) {
                    ++half_0_constructed;
                    std::construct_at(half_0->data + half_0_size, std::forward<Ty>(item));
                } else {
                    ++half_1_constructed;
                    std::construct_at(half_1->data, std::forward<Ty>(item));
                }

                for (size_t i = half_0_size, j = 0; j < half_1_size && i < handle_end; ++half_1_constructed, i++, j++) {
                    std::construct_at(half_1->data + j + add_in_, std::move(to_swap->data[i]));
                    std::destroy_at(to_swap->data + i);
                }
            } catch (...) {
                for (size_t i = 0; i < half_0_constructed; i++)
                    std::destroy_at(half_0->data + i);
                for (size_t i = 0; i < half_1_constructed; i++)
                    std::destroy_at(half_1->data + i);
                throw;
            }

            swap_block_with_blocks(to_swap, handle_begin, handle_end, half_0.release(), half_1.release());
            ++_size();
        }

        constexpr void insert_item_split(const_iterator iter, const T* items, size_t items_size) {
            auto [handle_begin, handle_end] = get_handle_range(iter);

            if (iter.relative_index < handle_begin || iter.relative_index > handle_end)
                throw std::out_of_range("Insertion point is outside the constructed range.");

            size_t half_0_size = iter.relative_index;
            size_t half_1_size = iter.block->data_size - half_0_size;
            auto half_0 = arr_block<T>::create_safe_block(allocator_and_size.get_allocator(), half_0_size);
            auto insert = arr_block<T>::create_safe_block(allocator_and_size.get_allocator(), items_size);
            auto half_1 = arr_block<T>::create_safe_block(allocator_and_size.get_allocator(), half_1_size);
            arr_block<T>* to_swap = iter.block;
            size_t half_0_constructed = 0;
            size_t insert_constructed = 0;
            size_t half_1_constructed = 0;

            try {
                for (size_t i = handle_begin; i < half_0_size && i < handle_end; ++half_0_constructed, i++)
                    std::construct_at(half_0->data + i, std::move(to_swap->data[i]));

                for (size_t i = 0; i < items_size; ++insert_constructed, i++)
                    std::construct_at(insert->data + i, items[i]);

                for (size_t i = half_0_size, j = 0; j < half_1_size && i < handle_end; ++half_1_constructed, i++, j++)
                    std::construct_at(half_1->data + j, std::move(to_swap->data[i]));
            } catch (...) {
                for (size_t i = 0; i < half_0_constructed; i++)
                    std::destroy_at(half_0->data + i);
                for (size_t i = 0; i < insert_constructed; i++)
                    std::destroy_at(insert->data + i);
                for (size_t i = 0; i < half_1_constructed; i++)
                    std::destroy_at(half_1->data + i);
                throw;
            }
            insert_between(insert.release(), half_0.get(), half_1.get());
            swap_block_with_blocks(to_swap, handle_begin, handle_end, half_0.release(), half_1.release());
            _size() += items_size;
        }

        template <bool make_move>
        constexpr void insert_item_split(const_iterator iter, const_iterator another_iter, size_t items_size) {
            auto [handle_begin, handle_end] = get_handle_range(iter);

            if (iter.relative_index < handle_begin || iter.relative_index > handle_end)
                throw std::out_of_range("Insertion point is outside the constructed range.");

            size_t half_0_size = iter.relative_index;
            size_t half_1_size = iter.block->data_size - half_0_size;
            auto half_0 = arr_block<T>::create_safe_block(allocator_and_size.get_allocator(), half_0_size);
            auto insert = arr_block<T>::create_safe_block(allocator_and_size.get_allocator(), items_size);
            auto half_1 = arr_block<T>::create_safe_block(allocator_and_size.get_allocator(), half_1_size);
            arr_block<T>* to_swap = iter.block;
            size_t half_0_constructed = 0;
            size_t insert_constructed = 0;
            size_t half_1_constructed = 0;
            try {
                for (size_t i = handle_begin; i < half_0_size && i < handle_end; ++half_0_constructed, i++)
                    std::construct_at(half_0->data + i, std::move(to_swap->data[i]));

                for (size_t i = 0; i < items_size; ++insert_constructed, i++) {
                    if constexpr (make_move)
                        std::construct_at(insert->data + i, std::move(*another_iter));
                    else
                        std::construct_at(insert->data + i, *another_iter);
                    ++another_iter;
                }

                for (size_t i = half_0_size, j = 0; j < half_1_size && i < handle_end; ++half_1_constructed, i++, j++)
                    std::construct_at(half_1->data + j, std::move(to_swap->data[i]));

            } catch (...) {
                for (size_t i = 0; i < half_0_constructed; i++)
                    std::destroy_at(half_0->data + i);
                for (size_t i = 0; i < insert_constructed; i++)
                    std::destroy_at(insert->data + i);
                for (size_t i = 0; i < half_1_constructed; i++)
                    std::destroy_at(half_1->data + i);
                throw;
            }

            insert_between(insert.release(), half_0.get(), half_1.get());
            swap_block_with_blocks(to_swap, handle_begin, handle_end, half_0.release(), half_1.release());
            _size() += items_size;
        }

        template <class Ty>
        constexpr void insert_item_slow(const_iterator iter, Ty&& item) {
            auto [startConstructed, endConstructed] = get_handle_range(iter);

            if (iter.relative_index < startConstructed || iter.relative_index > endConstructed)
                throw std::out_of_range("Insertion point is outside the constructed range.");


            auto new_block = arr_block<T>::create_safe_block(allocator_and_size.get_allocator(), iter.block->data_size + 1);
            size_t new_block_constructed = 0;
            try {
                for (size_t i = startConstructed; i < iter.relative_index; ++new_block_constructed, ++i) {
                    std::construct_at(new_block->data + i, std::move(iter.block->data[i]));
                    std::destroy_at(iter.block->data + i);
                }
                ++new_block_constructed;
                std::construct_at(new_block->data + iter.relative_index, std::forward<Ty>(item));
                for (size_t i = iter.relative_index, j = iter.relative_index + 1; i < endConstructed; ++new_block_constructed, ++i, ++j) {
                    std::construct_at(new_block->data + j, std::move(iter.block->data[i]));
                    std::destroy_at(iter.block->data + i);
                }
            } catch (...) {
                for (size_t i = 0; i < new_block_constructed; ++i)
                    std::destroy_at(new_block->data + i);
                throw;
            }
            auto old = iter.block;
            swap_block(iter.block, new_block.release());
            arr_block<T>::destroy_block(old, allocator_and_size.get_allocator());
            ++_size();
        }

        constexpr void insert_item_slow(const_iterator iter, const T* items, size_t items_size) {
            auto [startConstructed, endConstructed] = get_handle_range(iter);

            if (iter.relative_index < startConstructed || iter.relative_index > endConstructed)
                throw std::out_of_range("Insertion point is outside the constructed range.");

            auto new_block = arr_block<T>::create_safe_block(allocator_and_size.get_allocator(), iter.block->data_size + items_size);
            size_t new_block_constructed = 0;

            try {
                for (size_t i = startConstructed; i < iter.relative_index; ++new_block_constructed, ++i) {
                    std::construct_at(new_block->data + i, std::move(iter.block->data[i]));
                    std::destroy_at(iter.block->data + i);
                }
                for (size_t i = 0, j = iter.relative_index; i < items_size; ++new_block_constructed, ++i, ++j)
                    std::construct_at(new_block->data + j, items[i]);

                for (size_t i = iter.relative_index, j = iter.relative_index + items_size; i < endConstructed; ++new_block_constructed, ++i, ++j) {
                    std::construct_at(new_block->data + j, std::move(iter.block->data[i]));
                    std::destroy_at(iter.block->data + i);
                }
            } catch (...) {
                for (size_t i = 0; i < new_block_constructed; ++i)
                    std::destroy_at(new_block->data + i);
                throw;
            }

            auto old = iter.block;
            swap_block(iter.block, new_block.release());
            arr_block<T>::destroy_block(old, allocator_and_size.get_allocator());
            _size() += items_size;
        }

        template <bool make_move>
        constexpr void insert_item_slow(const_iterator iter, const_iterator another_iter, size_t items_size) {
            auto [startConstructed, endConstructed] = get_handle_range(iter);

            if (iter.relative_index < startConstructed || iter.relative_index > endConstructed)
                throw std::out_of_range("Insertion point is outside the constructed range.");

            auto new_block = arr_block<T>::create_safe_block(allocator_and_size.get_allocator(), iter.block->data_size + items_size);
            size_t new_block_constructed = 0;
            try {
                for (size_t i = startConstructed; i < iter.relative_index; ++new_block_constructed, ++i) {
                    std::construct_at(new_block->data + i, std::move(iter.block->data[i]));
                    std::destroy_at(iter.block->data + i);
                }
                for (size_t i = 0, j = iter.relative_index; i < items_size; ++new_block_constructed, ++i, ++j) {
                    if constexpr (make_move)
                        std::construct_at(new_block->data + j, std::move(*another_iter));
                    else
                        std::construct_at(new_block->data + j, *another_iter);
                    ++another_iter;
                }

                for (size_t i = iter.relative_index, j = iter.relative_index + items_size; i < endConstructed; ++new_block_constructed, ++i, ++j) {
                    std::construct_at(new_block->data + j, std::move(iter.block->data[i]));
                    std::destroy_at(iter.block->data + i);
                }
            } catch (...) {
                for (size_t i = 0; i < new_block_constructed; ++i)
                    std::destroy_at(new_block->data + i);
                throw;
            }

            auto old = iter.block;
            swap_block(iter.block, new_block.release());
            arr_block<T>::destroy_block(old, allocator_and_size.get_allocator());
            _size() += items_size;
        }

#pragma endregion

        constexpr void erase_range(const_iterator begin, const_iterator end) {
            if (!begin.block)
                return;
            if (begin.block == end.block) {
                auto [beginStartConstructed, beginEndConstructed] = get_handle_range(begin);
                if (begin.relative_index < beginStartConstructed || end.relative_index > beginEndConstructed)
                    throw std::out_of_range("Erase range is outside the constructed range.");

                auto [endStartConstructed, endEndConstructed] = get_handle_range(end);
                if (begin.relative_index < endStartConstructed || end.relative_index > endEndConstructed)
                    throw std::out_of_range("Erase range is outside the constructed range.");
                size_t erase_size = end.relative_index - begin.relative_index;
                if (beginStartConstructed == begin.relative_index && endEndConstructed == end.relative_index) {
                    if (beginStartConstructed != 0)
                        _reserved_front -= beginStartConstructed;
                    if (endEndConstructed != begin.block->data_size)
                        _reserved_back -= begin.block->data_size - endEndConstructed;

                    _size() -= erase_size;
                    release_arr_block(begin.block, begin.absolute_index);
                    return;
                } else if (beginStartConstructed == begin.relative_index && begin.block == first_block) {
                    for (size_t i = begin.relative_index; i < end.relative_index; ++i)
                        std::destroy_at(begin.block->data + i);
                    _reserved_front += erase_size;
                } else if (endEndConstructed == end.relative_index && begin.block == last_block) {
                    for (size_t i = begin.relative_index; i < end.relative_index; ++i)
                        std::destroy_at(begin.block->data + i);
                    _reserved_back += erase_size;
                } else {
                    size_t new_arr_size = begin.block->data_size - erase_size;
                    auto_deallocate hold(allocator_and_size.allocate(new_arr_size), allocator_and_size.get_allocator(), new_arr_size);
                    T* new_arr = hold.data;
                    size_t new_arr_constructed = 0;

                    try {
                        for (size_t i = beginStartConstructed; i < begin.relative_index; ++new_arr_constructed, ++i)
                            std::construct_at(new_arr + i, std::move(begin.block->data[i]));

                        for (size_t i = end.relative_index; i < endEndConstructed; ++new_arr_constructed, ++i)
                            std::construct_at(new_arr + i - erase_size, std::move(begin.block->data[i]));

                        for (size_t i = beginStartConstructed; i < beginEndConstructed; ++i)
                            std::destroy_at(begin.block->data + i);
                    } catch (...) {
                        for (size_t j = 0; j < new_arr_constructed; ++j)
                            std::destroy_at(new_arr + j);
                        throw;
                    }
                    allocator_and_size.deallocate(begin.block->data, begin.block->data_size);
                    begin.block->data = new_arr;
                    begin.block->data_size = new_arr_size;
                    hold.release();
                }
                _size() -= erase_size;
            } else {
                const_iterator block_iter = begin;
                block_iter.absolute_index += begin.block->data_size - begin.relative_index;
                block_iter.relative_index = begin.block->data_size;

                const_iterator current = const_iterator(block_iter.block->next, 0, begin.absolute_index - begin.relative_index + begin.block->data_size);

                erase_range(begin, block_iter);
                block_iter = current;
                while (block_iter.block != end.block) {
                    current = const_iterator(block_iter.block->next, 0, block_iter.absolute_index + block_iter.block->data_size);
                    erase_range(const_iterator(block_iter.block, 0, block_iter.absolute_index - block_iter.block->data_size), block_iter);
                    block_iter = current;
                }
                erase_range(block_iter, end);
            }
        }

        constexpr void apply_for_block_remove_unsafe(bit_array_helper& selected, const_iterator begin, const_iterator end, size_t startConstructed, size_t endConstructed) {
            size_t new_arr_size = begin.block->data_size - selected.set_values();
            auto_deallocate hold(allocator_and_size.allocate(new_arr_size), allocator_and_size.get_allocator(), new_arr_size);
            T* new_arr = hold.data;
            size_t new_arr_index = 0;

            try {
                for (size_t i = startConstructed; i < begin.relative_index; ++i)
                    std::construct_at(new_arr + new_arr_index++, std::move(begin.block->data[i]));

                for (size_t i = begin.relative_index; i < end.relative_index; ++i)
                    if (!selected.get(i - startConstructed))
                        std::construct_at(new_arr + new_arr_index++, std::move(begin.block->data[i]));

                for (size_t i = end.relative_index; i < endConstructed; ++i)
                    std::construct_at(new_arr + new_arr_index++, std::move(begin.block->data[i]));
            } catch (...) {
                for (size_t j = 0; j < new_arr_index; ++j)
                    std::destroy_at(new_arr + j);
                allocator_and_size.deallocate(new_arr, new_arr_size);
                throw;
            }
            for (size_t i = startConstructed; i < endConstructed; ++i)
                std::destroy_at(begin.block->data + i);
            allocator_and_size.deallocate(begin.block->data, begin.block->data_size);
            begin.block->data = new_arr;
            begin.block->data_size = new_arr_size;
            hold.release();
        }

        template <class FN>
        constexpr void select_for_block_unsafe(FN&& fn, bit_array_helper& selector, const_iterator begin, const_iterator end) noexcept(std::is_nothrow_invocable_r_v<bool, FN, size_t, const T&> || std::is_nothrow_invocable_r_v<bool, FN, const T&>) {
            size_t count = end.relative_index - begin.relative_index;
            if constexpr (std::is_invocable_r_v<bool, FN, size_t, const T&>) {
                size_t index_off = begin.absolute_index - _reserved_front;
                for (size_t i = 0; i < count; ++i)
                    selector.set(i, fn(i + index_off, *(begin + i)));
            } else
                for (size_t i = 0; i < count; ++i)
                    selector.set(i, fn(*(begin + i)));
        }

        template <class FN>
        constexpr void remove_in_block(FN&& fn, const_iterator begin, const_iterator end) {
            check_handle_range(end);
            auto [beginStartConstructed, beginEndConstructed] = checked_get_handle_range(begin);

            bit_array_helper selector(end.absolute_index - begin.absolute_index);
            select_for_block_unsafe(std::forward<FN>(fn), selector, begin, end);
            if (selector.set_values() == begin.block->data_size)
                release_arr_block(begin.block, begin.absolute_index);
            else
                apply_for_block_remove_unsafe(selector, begin, end, beginStartConstructed, beginEndConstructed);
            _size() -= selector.set_values();
        }

        template <class FN>
        constexpr void remove_in(FN&& fn, const_iterator begin, const_iterator end) {
            if (begin.absolute_index > end.absolute_index)
                throw std::invalid_argument("Begin iterator is after the end iterator.");
            if (begin.block == end.block) {
                remove_in_block(std::forward<FN>(fn), begin, end);
            } else {
                const_iterator block_iter = end;
                remove_in_block(fn, const_iterator(end.block, 0, end.absolute_index), block_iter);
                block_iter = const_iterator(end.block->prev, 0, end.absolute_index - end.block->data_size);
                while (block_iter.block != begin.block) {
                    remove_in_block(fn, const_iterator(block_iter.block, 0, block_iter.absolute_index - block_iter.block->data_size), block_iter);
                    block_iter = const_iterator(block_iter.block->prev, 0, block_iter.absolute_index - block_iter.block->data_size);
                }
                remove_in_block(std::forward<FN>(fn), begin, const_iterator(begin.block, begin.block->data_size, begin.absolute_index + begin.block->data_size - begin.relative_index));
            }
        }


    public:
        constexpr ~list_array() {
            clear();
        }

#pragma region constructors

        constexpr list_array(const Allocator& allocator = Allocator())
            : allocator_and_size(allocator) {}

        template <class AnotherT>
        constexpr list_array(std::initializer_list<AnotherT> vals, const Allocator& allocator = Allocator())
            requires std::convertible_to<AnotherT, T> && std::is_copy_constructible_v<T>
            : allocator_and_size(allocator) {
            reserve(vals.size());
            for (const AnotherT& it : vals)
                push_back((T)it);
        }

        template <size_t arr_size>
        constexpr list_array(const T (&arr)[arr_size], const Allocator& allocator = Allocator())
            : allocator_and_size(allocator) {
            push_back(arr, arr_size);
        }

        constexpr list_array(const T* arr, size_t arr_size, const Allocator& allocator = Allocator())
            : allocator_and_size(allocator) {
            push_back(arr, arr_size);
        }

        template <typename Iterable>
        constexpr list_array(Iterable begin, Iterable end, size_t reserve_len = 0, const Allocator& allocator = Allocator())
            : allocator_and_size(allocator) {
            if constexpr (std::is_pointer_v<Iterable>) {
                size_t len = end - begin;
                if (len < reserve_len)
                    len = reserve_len;
                if (len == 0)
                    return;
                reserve_back(len);
            } else if (reserve_len)
                reserve_back(reserve_len);
            while (begin != end)
                push_back(*begin++);
        }

        constexpr list_array(size_t size, const Allocator& allocator = Allocator())
            requires std::default_initializable<T>
            : allocator_and_size(allocator) {
            resize(size);
        }

        constexpr list_array(size_t size, const T& default_init, const Allocator& allocator = Allocator())
            : allocator_and_size(allocator) {
            resize(size, default_init);
        }

        constexpr list_array(list_array&& move) noexcept
            : allocator_and_size(move.allocator_and_size.get_allocator()) {
            operator=(std::move(move));
        }

        constexpr list_array(const list_array& copy)
            : allocator_and_size(copy.allocator_and_size.get_allocator()) {
            operator=(copy);
        }

        constexpr list_array(const list_array& copy, const Allocator& allocator)
            : allocator_and_size(allocator) {
            operator=(copy);
        }

        constexpr list_array(const list_array& copy, size_t start, const Allocator& allocator = Allocator())
            : list_array(copy.get_iterator(start), copy.end(), copy.size() - start, allocator) {}

        constexpr list_array(const list_array& copy, size_t start, size_t end, const Allocator& allocator = Allocator())
            : list_array(copy.get_iterator(start), copy.get_iterator(end), end - start, allocator) {}

        template <class Container>
        constexpr list_array(Container&& cont, const Allocator& allocator = Allocator())
            requires is_container_v<Container>
            : allocator_and_size(allocator) {
            reserve(cont.size());
            for (auto&& it : cont)
                push_back(T(std::move(it)));
        }

        template <class Container>
        constexpr list_array(const Container& cont, const Allocator& allocator = Allocator())
            requires is_container_v<Container> && std::copy_constructible<T>
            : allocator_and_size(allocator) {
            reserve(cont.size());
            for (const auto& it : cont)
                push_back(it);
        }

#pragma endregion
#pragma region operators

        constexpr list_array<T, Allocator>& operator=(list_array<T, Allocator>&& move) noexcept {
            if (first_block == move.first_block)
                return *this;
            clear();
            allocator_and_size = move.allocator_and_size;
            first_block = move.first_block;
            last_block = move.last_block;
            _size() = move._size();
            _reserved_front = move._reserved_front;
            _reserved_back = move._reserved_back;
            move.first_block = nullptr;
            move.last_block = nullptr;
            move._size() = 0;
            move._reserved_front = 0;
            move._reserved_back = 0;
            return *this;
        }

        constexpr list_array<T, Allocator>& operator=(const list_array<T, Allocator>& copy) {
            if (first_block == copy.first_block)
                return *this;
            clear();
            reserve(copy.size());
            for (auto& it : copy)
                push_back(it);
            return *this;
        }

#pragma endregion
#pragma region push

        constexpr list_array<T, Allocator>& push_back(const T& value) &
            requires std::is_copy_constructible_v<T>
        {
            if (_reserved_back) {
                std::construct_at(get_direct_element_at_index(_reserved_front + _size()), value);
                ++_size();
                --_reserved_back;
                return *this;
            } else if (_reserved_front) {
                if (first_block->data_size <= _reserved_front) {
                    steal_reserve_block_front_to_back();
                    return push_back(value);
                }
            }
            reserve_back(increase_policy());
            return push_back(value);
        }

        constexpr list_array<T, Allocator>& push_back(T&& value) & {
            if (_reserved_back) {
                std::construct_at(get_direct_element_at_index(_reserved_front + _size()), std::move(value));
                ++_size();
                --_reserved_back;
                return *this;
            } else if (_reserved_front) {
                if (first_block->data_size <= _reserved_front) {
                    steal_reserve_block_front_to_back();
                    return push_back(std::move(value));
                }
            }
            reserve_back(increase_policy());
            return push_back(std::move(value));
        }

        template <class AnyAllocator>
        constexpr list_array<T, Allocator>& push_back(const list_array<T, AnyAllocator>& alloc) & {
            if (_reserved_back < alloc.size())
                reserve_back(alloc.size() - _reserved_back);
            for (const auto& value : alloc)
                push_back(value);
            return *this;
        }

        template <class AnyAllocator>
        constexpr list_array<T, Allocator>& push_back(list_array<T, AnyAllocator>&& alloc) & {
            if (_reserved_back < alloc.size())
                reserve_back(alloc.size() - _reserved_back);
            for (auto& value : alloc)
                push_back(std::move(value));
            return *this;
        }

        constexpr list_array<T, Allocator>& push_back(const T* begin, const T* end) & {
            if (_reserved_back < size_t(end - begin))
                reserve_back(end - begin - _reserved_back);
            for (const T* it = begin; it != end; it++)
                push_back(*it);
            return *this;
        }

        template <size_t N>
        constexpr list_array<T, Allocator>& push_back(const T (&arr)[N]) & {
            return push_back(arr, arr + N);
        }

        constexpr list_array<T, Allocator>& push_back(const T* arr, size_t size) & {
            return push_back(arr, arr + size);
        }

        template <class AnotherT>
        constexpr list_array<T, Allocator>& push_back_for(const AnotherT& value) & {
            for (auto& it : *this)
                it.push_back(value);
            return *this;
        }

        template <class... Args>
        constexpr auto& emplace_back(Args&&... args) & {
            if (_reserved_back) {
                auto it = std::construct_at(get_direct_element_at_index(_reserved_front + _size()), std::forward<Args>(args)...);
                ++_size();
                --_reserved_back;
                return *it;
            } else if (_reserved_front) {
                if (first_block->data_size <= _reserved_front) {
                    steal_reserve_block_front_to_back();
                    return emplace_back(std::forward<Args>(args)...);
                }
            }
            reserve_back(increase_policy());
            return emplace_back(std::forward<Args>(args)...);
        }

        constexpr list_array<T, Allocator>& push_front(const T& value) &
            requires std::is_copy_constructible_v<T>
        {
            if (_reserved_front) {
                std::construct_at(get_direct_element_at_index(_reserved_front - 1), value);
                ++_size();
                --_reserved_front;
                return *this;
            } else if (_reserved_back) {
                if (last_block->data_size <= _reserved_back) {
                    steal_reserve_block_back_to_front();
                    return push_front(value);
                }
            }
            reserve_front(increase_policy());
            return push_front(value);
        }

        constexpr list_array<T, Allocator>& push_front(T&& value) & {
            if (_reserved_front) {
                std::construct_at(get_direct_element_at_index(_reserved_front - 1), std::move(value));
                ++_size();
                --_reserved_front;
                return *this;
            } else if (_reserved_back) {
                if (last_block->data_size <= _reserved_back) {
                    steal_reserve_block_back_to_front();
                    return push_front(std::move(value));
                }
            }
            reserve_front(increase_policy());
            return push_front(std::move(value));
        }

        template <class AnyAllocator>
        constexpr list_array<T, Allocator>& push_front(const list_array<T, AnyAllocator>& alloc) & {
            if (_reserved_front < alloc.size())
                reserve_front(alloc.size() - _reserved_front);
            for (const auto& value : alloc.reverse())
                push_front(value);
            return *this;
        }

        template <class AnyAllocator>
        constexpr list_array<T, Allocator>& push_front(list_array<T, AnyAllocator>&& alloc) & {
            if (_reserved_front < alloc.size())
                reserve_front(alloc.size() - _reserved_front);
            for (auto& value : alloc.reverse())
                push_front(std::move(value));
            alloc.clear();
            return *this;
        }

        constexpr list_array<T, Allocator>& push_front(const T* begin, const T* end) & {
            if (_reserved_front < size_t(end - begin))
                reserve_front(end - begin - _reserved_front);
            for (const T* it = begin; it != end; it++)
                push_front(*it);
            return *this;
        }

        template <size_t N>
        constexpr list_array<T, Allocator>& push_front(const T (&arr)[N]) & {
            return push_front(arr, arr + N);
        }

        constexpr list_array<T, Allocator>& push_front(const T* arr, size_t size) & {
            return push_front(arr, arr + size);
        }

        template <class AnotherT>
        constexpr list_array<T, Allocator>& push_front_for(const AnotherT& value) & {
            for (auto& it : *this)
                it.push_front(value);
            return *this;
        }

        template <class... Args>
        constexpr auto& emplace_front(Args&&... args) & {
            if (_reserved_front) {
                auto it = std::construct_at(get_direct_element_at_index(_reserved_front - 1), std::forward<Args>(args)...);
                ++_size();
                --_reserved_front;
                return *it;
            } else if (_reserved_back) {
                if (last_block->data_size <= _reserved_back) {
                    steal_reserve_block_back_to_front();
                    return emplace_front(std::forward<Args>(args)...);
                }
            }
            reserve_front(increase_policy());
            return emplace_front(std::forward<Args>(args)...);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> push_back(const T& value) &&
            requires std::is_copy_constructible_v<T>
        {
            push_back(value);
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> push_back(T&& value) && {
            push_back(std::move(value));
            return std::move(*this);
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr list_array<T, Allocator> push_back(const list_array<T, AnyAllocator>& alloc) && {
            push_back(alloc);
            return std::move(*this);
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr list_array<T, Allocator> push_back(list_array<T, AnyAllocator>&& alloc) && {
            push_back(std::move(alloc));
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> push_back(const T* begin, const T* end) && {
            push_back(begin, end);
            return std::move(*this);
        }

        template <size_t N>
        [[nodiscard]] constexpr list_array<T, Allocator> push_back(const T (&arr)[N]) && {
            push_back(arr, arr + N);
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> push_back(const T* arr, size_t size) && {
            push_back(arr, arr + size);
            return std::move(*this);
        }

        template <class AnotherT>
        [[nodiscard]] constexpr list_array<T, Allocator> push_back_for(const AnotherT& value) && {
            for (auto& it : *this)
                it.push_back(value);
            return std::move(*this);
        }

        template <class... Args>
        [[nodiscard]] constexpr list_array<T, Allocator> emplace_back(Args&&... args) && {
            emplace_back(std::forward<Args>(args)...);
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> push_front(const T& value) &&
            requires std::is_copy_constructible_v<T>
        {
            push_front(value);
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> push_front(T&& value) && {
            push_front(std::move(value));
            return std::move(*this);
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr list_array<T, Allocator> push_front(const list_array<T, AnyAllocator>& alloc) && {
            push_front(alloc);
            return std::move(*this);
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr list_array<T, Allocator> push_front(list_array<T, AnyAllocator>&& alloc) && {
            push_front(std::move(alloc));
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> push_front(const T* begin, const T* end) && {
            push_front(begin, end);
            return std::move(*this);
        }

        template <size_t N>
        [[nodiscard]] constexpr list_array<T, Allocator> push_front(const T (&arr)[N]) && {
            push_front(arr, arr + N);
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> push_front(const T* arr, size_t size) && {
            push_front(arr, arr + size);
            return std::move(*this);
        }

        template <class AnotherT>
        [[nodiscard]] constexpr list_array<T, Allocator> push_front_for(const AnotherT& value) && {
            for (auto& it : *this)
                it.push_front(value);
            return std::move(*this);
        }

        template <class... Args>
        [[nodiscard]] constexpr list_array<T, Allocator> emplace_front(Args&&... args) && {
            emplace_front(std::forward<Args>(args)...);
            return std::move(*this);
        }

#pragma endregion
#pragma region list_ops

        [[nodiscard]] constexpr list_array<T, Allocator> pop_back() && noexcept(std::is_nothrow_constructible_v<T>) {
            pop_back();
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> pop_front() && noexcept(std::is_nothrow_constructible_v<T>) {
            pop_front();
            return std::move(*this);
        }

        constexpr list_array<T, Allocator>& pop_back() & noexcept(std::is_nothrow_constructible_v<T>) {
            if (_size() == 0)
                return *this;
            std::destroy_at(&operator[](_size() - 1));
            --_size();
            ++_reserved_back;
            return *this;
        }

        constexpr list_array<T, Allocator>& pop_front() & noexcept(std::is_nothrow_constructible_v<T>) {
            if (_size() == 0)
                return *this;
            std::destroy_at(&operator[](0));
            --_size();
            ++_reserved_front;
            return *this;
        }

        [[nodiscard]] constexpr T& front() & {
            if (!_size())
                throw std::out_of_range("list_array::front: array is empty");
            return operator[](0);
        }

        [[nodiscard]] constexpr T front() && {
            return take_front();
        }

        [[nodiscard]] constexpr const T& front() const& {
            if (!_size())
                throw std::out_of_range("list_array::front: array is empty");
            return operator[](0);
        }

        [[nodiscard]] constexpr T& back() & {
            if (!_size())
                throw std::out_of_range("list_array::back: array is empty");
            return operator[](_size() - 1);
        }

        [[nodiscard]] constexpr T& back() && {
            return take_back();
        }

        [[nodiscard]] constexpr const T& back() const& {
            if (!_size())
                throw std::out_of_range("list_array::back: array is empty");
            return operator[](_size() - 1);
        }

        [[nodiscard]] constexpr T take_front() {
            if (!_size())
                throw std::out_of_range("list_array::take_front: array is empty");
            T value(std::move(front()));
            pop_front();
            return value;
        }

        [[nodiscard]] constexpr T take_back() {
            if (!_size())
                throw std::out_of_range("list_array::take_front: array is empty");
            T value(std::move(back()));
            pop_back();
            return value;
        }

#pragma endregion
#pragma region insert

        constexpr list_array<T, Allocator>& insert(size_t index, const T& value) &
            requires std::is_copy_constructible_v<T>
        {
            if (index == 0)
                push_front(value);
            else if (index == _size())
                push_back(value);
            else {
                auto iter = get_iterator(index);
                if (split_policy(iter.block->data_size))
                    insert_item_split(iter, value);
                else
                    insert_item_slow(iter, value);
            }
            return *this;
        }

        constexpr list_array<T, Allocator>& insert(size_t index, T&& value) & {
            if (index == 0)
                push_front(value);
            else if (index == _size())
                push_back(value);
            else {
                auto iter = get_iterator(index);
                if (split_policy(iter.block->data_size))
                    insert_item_split(iter, std::move(value));
                else
                    insert_item_slow(iter, std::move(value));
            }
            return *this;
        }

        constexpr list_array<T, Allocator>& insert(size_t index, const T* values, size_t size) &
            requires std::is_copy_constructible_v<T>
        {
            if (index == 0)
                push_front(values, size);
            else if (index == _size())
                push_back(values, size);
            else {
                auto iter = get_iterator(index);
                if (split_policy(iter.block->data_size))
                    insert_item_split(iter, values, size);
                else
                    insert_item_slow(iter, values, size);
            }
            return *this;
        }

        constexpr list_array<T, Allocator>& insert(size_t index, const T* begin, const T* end) &
            requires std::is_copy_constructible_v<T>
        {
            return insert(index, begin, end - begin);
        }

        template <size_t N>
        constexpr list_array<T, Allocator>& insert(size_t index, const T (&arr)[N]) &
            requires std::is_copy_constructible_v<T>
        {
            return insert(index, arr, N);
        }

        template <class AnyAllocator>
        constexpr list_array<T, Allocator>& insert(size_t index, const list_array<T, AnyAllocator>& values) &
            requires std::is_copy_constructible_v<T>
        {
            if (index == 0)
                push_front(values);
            else if (index == _size())
                push_back(values);
            else {
                auto iter = get_iterator(index);
                if (split_policy(iter.block->data_size))
                    insert_item_split<false>(iter, values.begin(), values.size());
                else
                    insert_item_slow<false>(iter, values.begin(), values.size());
            }
            return *this;
        }

        template <class AnyAllocator>
        constexpr list_array<T, Allocator>& insert(size_t index, list_array<T, AnyAllocator>&& values) & {
            if (index == 0)
                push_front(values);
            else if (index == _size())
                push_back(values);
            else {
                auto iter = get_iterator(index);
                if (split_policy(iter.block->data_size))
                    insert_item_split<true>(iter, values.begin(), values.size());
                else
                    insert_item_slow<true>(iter, values.begin(), values.size());
            }
            return *this;
        }

        [[nodiscard]] constexpr list_array<T, Allocator> insert(size_t index, const T& value) &&
            requires std::is_copy_constructible_v<T>
        {
            insert(index, value);
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> insert(size_t index, T&& value) && {
            insert(index, std::move(value));
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> insert(size_t index, const T* values, size_t size) && {
            insert(index, values, size);
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> insert(size_t index, const T* begin, const T* end) && {
            insert(index, begin, end - begin);
            return std::move(*this);
        }

        template <size_t N>
        [[nodiscard]] constexpr list_array<T, Allocator> insert(size_t index, const T (&arr)[N]) && {
            insert(index, arr, N);
            return std::move(*this);
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr list_array<T, Allocator> insert(size_t index, const list_array<T, AnyAllocator>& values) && {
            insert(index, values);
            return std::move(*this);
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr list_array<T, Allocator> insert(size_t index, list_array<T, AnyAllocator>&& values) && {
            insert(index, std::move(values));
            return std::move(*this);
        }

#pragma endregion
#pragma region remove

        constexpr size_t remove(const T& val) &
            requires is_equality_comparable<T>
        {
            return remove_if([&](const T& value) constexpr { return value == val; });
        }

        template <class FN>
        constexpr size_t remove_if(FN&& fn) &
            requires std::is_invocable_r_v<bool, FN, size_t, const T&> || std::is_invocable_r_v<bool, FN, const T&>
        {
            size_t old_size = _size();
            if (old_size == 0)
                return 0;
            remove_in(std::forward<FN>(fn), begin(), end());
            return old_size - _size();
        }

        template <class FN>
        constexpr size_t remove_if(size_t begin, FN&& fn) &
            requires std::is_invocable_r_v<bool, FN, size_t, const T&> || std::is_invocable_r_v<bool, FN, const T&>
        {
            size_t old_size = _size();
            if (old_size == 0)
                return 0;
            remove_in(std::forward<FN>(fn), get_iterator(begin), end());
            return old_size - _size();
        }

        template <class FN>
        constexpr size_t remove_if(size_t begin, size_t end, FN&& fn) &
            requires std::is_invocable_r_v<bool, FN, size_t, const T&> || std::is_invocable_r_v<bool, FN, const T&>
        {
            size_t old_size = _size();
            if (old_size == 0)
                return 0;
            remove_in(std::forward<FN>(fn), get_iterator(begin), get_iterator(end));
            return old_size - _size();
        }

        [[nodiscard]] constexpr list_array<T, Allocator> remove(const T& val) &&
            requires is_equality_comparable<T>
        {
            remove(val);
            return std::move(*this);
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> remove_if(FN&& fn) &&
            requires std::is_invocable_r_v<bool, FN, size_t, const T&> || std::is_invocable_r_v<bool, FN, const T&>
        {
            remove_if(0, _size(), std::forward<FN>(fn));
            return std::move(*this);
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> remove_if(size_t begin, FN&& fn) &&
            requires std::is_invocable_r_v<bool, FN, size_t, const T&> || std::is_invocable_r_v<bool, FN, const T&>
        {
            remove_if(begin, _size(), std::forward<FN>(fn));
            return std::move(*this);
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> remove_if(size_t begin, size_t end, FN&& fn) &&
            requires std::is_invocable_r_v<bool, FN, size_t, const T&> || std::is_invocable_r_v<bool, FN, const T&>
        {
            remove_if(begin, end, std::forward<FN>(fn));
            return std::move(*this);
        }

#pragma endregion
#pragma region remove_one

        template <class FN>
        constexpr bool remove_one(FN&& check_function) & {
            return remove_one(0, _size(), std::forward<FN>(check_function));
        }

        template <class FN>
        constexpr bool remove_one(size_t start, FN&& check_function) & {
            return remove_one(start, _size(), std::forward<FN>(check_function));
        }

        template <class FN>
        constexpr bool remove_one(size_t start, size_t end, FN&& check_function) &
            requires std::is_invocable_r_v<bool, FN, size_t, const T&> || std::is_invocable_r_v<bool, FN, const T&>
        {
            size_t item = find_if(start, end, std::forward<FN>(check_function));
            if (item == npos)
                return false;
            erase(item);
            return true;
        }

#pragma endregion
#pragma region remove_same

        template <class FN>
        constexpr size_t remove_same(
            const T& val,
            size_t start,
            size_t end,
            FN&& comparer = [](const T& f, const T& s) constexpr { return f == s; }
        ) &
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            if (end > _size())
                throw std::out_of_range("end value out of size limit");
            if (start > _size())
                throw std::out_of_range("start value out of size limit");
            if (start > end)
                std::swap(start, end);
            return remove_if(start, end, [&comparer, &val](const T& cval) { return comparer(val, cval); });
        }

        template <class FN>
        constexpr size_t remove_same(
            const T& val,
            size_t start,
            FN&& comparer = [](const T& f, const T& s) constexpr { return f == s; }
        ) &
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            return remove_same(val, start, _size(), std::forward<FN>(comparer));
        }

        template <class FN>
        constexpr size_t remove_same(
            const T& val,
            FN&& comparer = [](const T& f, const T& s) constexpr { return f == s; }
        ) &
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            return remove_if(_reserved_front, _reserved_front + _size(), [&comparer, &val](const T& cval) { return comparer(val, cval); });
        }

        template <size_t arr_size>
        constexpr size_t remove_same(const T (&val)[arr_size], size_t start = 0) &
            requires is_equality_comparable<T>
        {
            return remove_same(val, arr_size, start, _size());
        }

        template <size_t arr_size>
        constexpr size_t remove_same(const T (&val)[arr_size], size_t start, size_t end) &
            requires is_equality_comparable<T>
        {
            return remove_same(val, arr_size, start, end);
        }

        constexpr size_t remove_same(const T* val, size_t arr_size, size_t start = 0) &
            requires is_equality_comparable<T>
        {
            return remove_same(val, arr_size, start, _size());
        }

        constexpr size_t remove_same(const T* val, size_t arr_size, size_t start, size_t end) &
            requires is_equality_comparable<T>
        {
            size_t old_size = _size();
            size_t pos = start;
            if (start < end)
                throw std::out_of_range("start > end");

            while (pos != npos) {
                pos = find(val, arr_size, pos, end);
                if (pos != npos) {
                    erase(pos, pos + arr_size);
                    end -= arr_size;
                }
            }
            return old_size - _size();
        }

        template <class AnyAllocator>
        constexpr size_t remove_same(const list_array<T, AnyAllocator>& val, size_t start = 0) &
            requires is_equality_comparable<T>
        {
            return remove_same(val, 0, val.size(), start, _size());
        }

        template <class AnyAllocator>
        constexpr size_t remove_same(const list_array<T, AnyAllocator>& val, size_t start, size_t end) &
            requires is_equality_comparable<T>
        {
            return remove_same(val, 0, val.size(), start, end);
        }

        template <class AnyAllocator>
        constexpr size_t remove_same(const list_array<T, AnyAllocator>& val, size_t val_start, size_t val_end, size_t start, size_t end) &
            requires is_equality_comparable<T>
        {
            size_t old_size = _size();
            size_t pos = start;
            if (start < end)
                throw std::out_of_range("start > end");
            while (pos != npos) {
                pos = find(val, val_start, val_end, pos, end);
                if (pos != npos) {
                    erase(pos, pos + val_end - val_start);
                    end -= val_end - val_start;
                }
            }
            return old_size - _size();
        }

#pragma endregion
#pragma region find

        [[nodiscard]] constexpr size_t find(const T& it) const&
            requires is_equality_comparable<T>
        {
            return find(0, size(), it);
        }

        [[nodiscard]] constexpr size_t find(size_t begin, const T& it) const&
            requires is_equality_comparable<T>
        {
            return find(begin, size(), it);
        }

        [[nodiscard]] constexpr size_t find(size_t begin, size_t end, const T& it) const&
            requires is_equality_comparable<T>
        {
            auto _end = get_iterator(end);
            for (const_iterator iter = get_iterator(begin); iter != _end; ++iter)
                if (*iter == it)
                    return iter.absolute_index;
            return npos;
        }

        [[nodiscard]] constexpr size_t find(const T* arr, const T* arr_end) const&
            requires is_equality_comparable<T>
        {
            return find(0, arr, arr_end);
        }

        [[nodiscard]] constexpr size_t find(size_t begin, const T* arr, const T* arr_end) const&
            requires is_equality_comparable<T>
        {
            return find(begin, size(), arr, arr_end);
        }

        [[nodiscard]] constexpr size_t find(size_t begin, size_t end, const T* arr, const T* arr_end) const&
            requires is_equality_comparable<T>
        {
            size_t arr_size = arr_end - arr;
            if (end - begin < arr_size)
                return npos;
            size_t i = 0;

            auto _end = get_iterator(end);
            for (const_iterator it = get_iterator(begin); it != _end; ++it)
                if (*it == arr[i]) {
                    if (++i == arr_size)
                        return it;
                } else
                    i = 0;
            return npos;
        }

        template <size_t N>
        [[nodiscard]] constexpr size_t find(const T (&arr)[N]) const&
            requires is_equality_comparable<T>
        {
            return find(0, size(), arr, arr + N);
        }

        template <size_t N>
        [[nodiscard]] constexpr size_t find(size_t begin, const T (&arr)[N]) const&
            requires is_equality_comparable<T>
        {
            return find(begin, size(), arr, arr + N);
        }

        template <size_t N>
        [[nodiscard]] constexpr size_t find(size_t begin, size_t _end, const T (&arr)[N]) const&
            requires is_equality_comparable<T>
        {
            return find(begin, _end, arr, arr + N);
        }

        template <class any_iter>
        [[nodiscard]] constexpr size_t find(any_iter extern_begin, any_iter extern_end) const&
            requires is_equality_comparable_with<T, typename std::iterator_traits<any_iter>::value_type>
        {
            return find(0, size(), extern_begin, extern_end);
        }

        template <class any_iter>
        [[nodiscard]] constexpr size_t find(size_t begin, any_iter extern_begin, any_iter extern_end) const&
            requires is_equality_comparable_with<T, typename std::iterator_traits<any_iter>::value_type>
        {
            return find(begin, size(), extern_begin, extern_end);
        }

        template <class any_iter>
        [[nodiscard]] constexpr size_t find(size_t begin, size_t end, any_iter extern_begin, any_iter extern_end) const&
            requires is_equality_comparable_with<T, typename std::iterator_traits<any_iter>::value_type>
        {

            auto _end = get_iterator(end);
            if constexpr (requires { extern_end - extern_begin; }) {
                size_t arr_size = extern_end - extern_begin;
                size_t i = 0;
                if (end - begin < arr_size)
                    return npos;
                for (const_iterator it = get_iterator(begin); it != _end; ++it)
                    if (*it == *extern_begin) {
                        if (++i == arr_size)
                            return it.absolute_index;
                        ++extern_begin;
                    } else
                        i = 0;
            } else {
                any_iter a_it = extern_begin;
                for (const_iterator it = get_iterator(begin); it != _end; ++it)
                    if (*it == *a_it) {
                        if (a_it == extern_end)
                            return it.absolute_index;
                        ++a_it;
                    }
            }
            return npos;
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr size_t find(const list_array<T, AnyAllocator>& arr) const&
            requires is_equality_comparable<T>
        {
            return find(0, size(), arr.begin(), arr.end());
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr size_t find(size_t begin, const list_array<T, AnyAllocator>& arr) const&
            requires is_equality_comparable<T>
        {
            return find(begin, size(), arr.begin(), arr.end());
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr size_t find(size_t begin, size_t _end, const list_array<T, AnyAllocator>& arr) const&
            requires is_equality_comparable<T>
        {
            return find(begin, _end, arr.begin(), arr.end());
        }

        template <class FN>
        [[nodiscard]] constexpr size_t find_if(FN&& fn) const&
            requires std::is_invocable_r_v<bool, FN, size_t, T&> || std::is_invocable_r_v<bool, FN, T&>
        {
            return find_if(0, size(), std::forward<FN>(fn));
        }

        template <class FN>
        [[nodiscard]] constexpr size_t find_if(size_t begin, FN&& fn) const&
            requires std::is_invocable_r_v<bool, FN, size_t, T&> || std::is_invocable_r_v<bool, FN, T&>
        {
            return find_if(begin, size(), std::forward<FN>(fn));
        }

        template <class FN>
        [[nodiscard]] constexpr size_t find_if(size_t begin, size_t end, FN&& fn) const&
            requires std::is_invocable_r_v<bool, FN, size_t, T&> || std::is_invocable_r_v<bool, FN, T&>
        {
            auto _end = get_iterator(end);
            if constexpr (std::is_invocable_r_v<bool, FN, size_t, T>) {
                for (const_iterator it = get_iterator(begin); it != _end; ++it)
                    if (fn(it.absolute_index, *it))
                        return it.absolute_index;
            } else {
                for (const_iterator it = get_iterator(begin); it != _end; ++it)
                    if (fn(*it))
                        return it.absolute_index;
            }
            return npos;
        }

#pragma endregion
#pragma region split

        [[nodiscard]] constexpr list_array<T, Allocator> split(size_t split_pos) & {
            if (_size() <= split_pos)
                throw std::out_of_range("Fail split due small array or split_pos is equal with array size");
            list_array<T, Allocator> res(_size() - split_pos);
            size_t i = 0;
            for (auto& it : range(split_pos, _size()))
                res[i++] = std::move(it);
            erase(split_pos, _size());
            return res;
        }

        [[nodiscard]] constexpr std::pair<list_array<T, Allocator>, list_array<T, Allocator>> split(size_t split_pos) && {
            list_array<T, Allocator> tmp = take();
            return {tmp, tmp.split(split_pos)};
        }

        template <class InnerAllocator = std::allocator<list_array<T, Allocator>>>
        [[nodiscard]] constexpr list_array<list_array<T, Allocator>, InnerAllocator> split_by(const T& split_value)
            requires is_equality_comparable<T>
        {
            list_array<list_array<T, Allocator>, InnerAllocator> res;
            for (size_t i = 0; i < _size(); i++) {
                if (operator[](i) == split_value) {
                    if (i != 0)
                        res.push_back(take(0, i));
                    else
                        res.push_back({});
                    erase(0);
                    i = 0;
                }
            }
            if (_size())
                res.push_back(take());
            return res;
        }

        template <size_t arr_size, class InnerAllocator = std::allocator<list_array<T, Allocator>>>
        [[nodiscard]] constexpr list_array<list_array<T, Allocator>, InnerAllocator> split_by(const T (&split_values)[arr_size])
            requires is_equality_comparable<T>
        {
            return split_by(split_values, arr_size);
        }

        template <class InnerAllocator = std::allocator<list_array<T, Allocator>>>
        [[nodiscard]] constexpr list_array<list_array<T, Allocator>, InnerAllocator> split_by(const T* split_values, size_t split_values_size)
            requires is_equality_comparable<T>
        {
            list_array<list_array<T, Allocator>, InnerAllocator> res;
            for (size_t i = 0; i < _size(); i++) {
                for (size_t j = 0; j < split_values_size; j++) {
                    if (operator[](i) == split_values[j]) {
                        if (i != 0)
                            res.push_back(take(0, i));
                        else
                            res.push_back({});
                        erase(0);
                        i = 0;
                        break;
                    }
                }
            }
            if (_size())
                res.push_back(take());
            return res;
        }

        template <class AnyAllocator, class InnerAllocator = std::allocator<list_array<T, AnyAllocator>>>
        [[nodiscard]] constexpr list_array<list_array<T, AnyAllocator>, InnerAllocator> split_by(const list_array<T, AnyAllocator>& split_values)
            requires is_equality_comparable<T>
        {
            list_array<list_array<T, AnyAllocator>, InnerAllocator> res;
            for (size_t i = 0; i < _size(); i++) {
                if (split_values.contains(operator[](i))) {
                    if (i != 0)
                        res.push_back(take(0, i));
                    else
                        res.push_back({});
                    erase(0);
                    i = 0;
                }
            }
            if (_size())
                res.push_back(take());
            return res;
        }

        template <class FN, class InnerAllocator = std::allocator<list_array<T, Allocator>>>
        [[nodiscard]] constexpr list_array<list_array<T, Allocator>, InnerAllocator> split_if(FN&& split_function)
            requires std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>
        {
            list_array<list_array<T, Allocator>, InnerAllocator> res;
            for (size_t i = 0; i < _size(); i++) {
                if constexpr (std::is_invocable_r_v<bool, FN, size_t, const T&>) {
                    if (split_function(i, operator[](i))) {
                        if (i != 0)
                            res.push_back(take(0, i));
                        else
                            res.push_back({});
                        erase(0);
                        i = 0;
                    }
                } else {
                    if (split_function(operator[](i))) {
                        if (i != 0)
                            res.push_back(take(0, i));
                        else
                            res.push_back({});
                        erase(0);
                        i = 0;
                    }
                }
            }
            if (_size())
                res.push_back(take());
            return res;
        }

#pragma endregion
#pragma region take

        [[nodiscard]] constexpr list_array<T, Allocator> take() & {
            return std::move(*this);
        }

        [[nodiscard]] constexpr T take(size_t take_pos) & {
            if (_size() <= take_pos)
                throw std::out_of_range("Fail take item due small array");
            T res(std::move(operator[](take_pos)));
            erase(take_pos);
            return res;
        }

        [[nodiscard]] constexpr T take(size_t take_pos) && {
            if (_size() <= take_pos)
                throw std::out_of_range("Fail take item due small array");
            return std::move(operator[](take_pos));
        }

        [[nodiscard]] constexpr T* take_raw(size_t& size) {
            if (blocks_more(1))
                commit();
            if (_reserved_front || _reserved_back)
                shrink_to_fit();
            size = _size();
            T* res = first_block->data;
            first_block->data = nullptr;
            clear();
            return res;
        }

        [[nodiscard]] constexpr list_array<T, Allocator> take(size_t start_pos, size_t end_pos) {
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (_size() < end_pos)
                throw std::out_of_range("Fail take items due small array");
            if (start_pos == 0 && end_pos == _size())
                return take();
            list_array<T, Allocator> res;
            res.reserve(end_pos - start_pos);
            for (auto& it : range(start_pos, end_pos))
                res.push_back(std::move(it));
            erase(start_pos, end_pos);
            return res;
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> take(FN&& select_fn)
            requires(std::is_invocable_r_v<bool, FN, size_t, const T&> || std::is_invocable_r_v<bool, FN, const T&>)
        {
            return take(0, _size(), std::forward<FN>(select_fn));
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> take(size_t start_pos, FN&& select_fn)
            requires(std::is_invocable_r_v<bool, FN, size_t, const T&> || std::is_invocable_r_v<bool, FN, const T&>)
        {
            return take(start_pos, _size(), std::forward<FN>(select_fn));
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> take(size_t start_pos, size_t end_pos, FN&& select_fn)
            requires(std::is_invocable_r_v<bool, FN, size_t, const T&> || std::is_invocable_r_v<bool, FN, const T&>)
        {
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (_size() < end_pos)
                throw std::out_of_range("Fail take items due small array");
            bit_array_helper selector(end_pos - start_pos);
            list_array<T, Allocator> res;
            size_t i = 0;
            size_t first_selection = 0;
            size_t last_selection = 0;
            bool first = true;
            for (auto& it : range(start_pos, end_pos)) {
                if constexpr (std::is_invocable_r_v<bool, FN, size_t, T>) {
                    if (select_fn(i, it)) {
                        if (first) {
                            first_selection = i;
                            first = false;
                        }
                        selector.set(i, true);
                        res.push_back(std::move(it));
                        last_selection = i;
                    }
                } else {
                    if (select_fn(it)) {
                        if (first) {
                            first_selection = i;
                            first = false;
                        }
                        selector.set(i, true);
                        res.push_back(std::move(it));
                        last_selection = i;
                    }
                }
                i++;
            }
            if (selector.set_values() == 0)
                return {};
            i = 0;
            remove_if(
                first_selection,
                last_selection,
                [selector, &i](const T&) {
                    return selector.get(i++);
                }
            );
            return res;
        }

#pragma endregion
#pragma region copy/swap

        [[nodiscard]] constexpr list_array<T, Allocator> copy(size_t start_pos, size_t end_pos) const
            requires std::is_copy_constructible_v<T>
        {
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (_size() < end_pos)
                throw std::out_of_range("Fail take items due small array");
            list_array<T, Allocator> res;
            res.reserve(end_pos - start_pos);
            for (auto& it : range(start_pos, end_pos))
                res.push_back(it);
            return res;
        }

        [[nodiscard]] constexpr list_array<T, Allocator> copy(size_t start_pos) const
            requires std::is_copy_constructible_v<T>
        {
            return copy(start_pos, _size());
        }

        [[nodiscard]] constexpr list_array<T, Allocator> copy() const
            requires std::is_copy_constructible_v<T>
        {
            return *this;
        }

        constexpr list_array<T, Allocator>& swap(list_array<T, Allocator>& to_swap) noexcept {
            if (first_block != to_swap.first_block) {
                arr_block<T>* fb = first_block;
                arr_block<T>* lb = last_block;
                size_t rb = _reserved_front;
                size_t re = _reserved_back;
                size_t s = _size();

                first_block = to_swap.first_block;
                last_block = to_swap.last_block;
                _reserved_front = to_swap._reserved_front;
                _size() = to_swap._size();
                _reserved_back = to_swap._reserved_back;

                to_swap.first_block = fb;
                to_swap.last_block = lb;
                to_swap._reserved_front = rb;
                to_swap._reserved_back = re;
                to_swap._size() = s;
            }
            return *this;
        }

#pragma endregion
#pragma region remove duplicates

        constexpr size_t unique() &
            requires is_equality_comparable<T>
        {
            return unique(0, _size());
        }

        constexpr size_t unique(size_t start_pos) &
            requires is_equality_comparable<T>
        {
            return unique(start_pos, _size());
        }

        constexpr size_t unique(size_t start_pos, size_t end_pos) &
            requires is_equality_comparable<T>
        {
            if (start_pos > end_pos)
                std::swap(start_pos, end_pos);
            if (start_pos + 1 >= end_pos)
                return 0;
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            const T* it = &operator[](start_pos);
            size_t res = 0;
            remove_if(
                start_pos + 1,
                end_pos,
                [&it, &res](const T& check_it) {
                    if (check_it == *it)
                        return (bool)++res;
                    it = &check_it;
                    return false;
                }
            );
            return res;
        }

        template <class FN>
        constexpr size_t unique(FN&& compare_func) &
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            return unique(0, _size(), std::forward<FN>(compare_func));
        }

        template <class FN>
        constexpr size_t unique(size_t start_pos, FN&& compare_func) &
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            return unique(start_pos, _size(), std::forward<FN>(compare_func));
        }

        template <class FN>
        constexpr size_t unique(size_t start_pos, size_t end_pos, FN&& compare_func) &
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            if (start_pos > end_pos)
                std::swap(start_pos, end_pos);
            if (start_pos + 1 >= end_pos)
                return 0;
            if (end_pos > _size())
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

        constexpr size_t unify() &
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return unify(0, _size());
        }

        constexpr size_t unify(size_t start_pos) &
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return unify(start_pos, _size());
        }

        constexpr size_t unify(size_t start_pos, size_t end_pos) &
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            list_array<T, Allocator> tmp_arr;
            tmp_arr.reserve_back((_size() >> 2) + 1);
            for (T& it : range(start_pos, end_pos))
                if (!tmp_arr.contains(it))
                    tmp_arr.push_back(it);
            tmp_arr.shrink_to_fit();
            swap(tmp_arr);
            return tmp_arr._size() - _size();
        }

        template <class FN>
        constexpr size_t unify(FN&& compare_func) &
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            return unify(0, _size(), std::forward<FN>(compare_func));
        }

        template <class FN>
        constexpr size_t unify(size_t start_pos, FN&& compare_func) &
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            return unify(start_pos, _size(), std::forward<FN>(compare_func));
        }

        template <class FN>
        constexpr size_t unify(size_t start_pos, size_t end_pos, FN&& compare_func) &
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            list_array<T, Allocator> tmp_arr;
            tmp_arr.reserve_back((_size() >> 2) + 1);
            for (T& it : range(start_pos, end_pos))
                if (!tmp_arr.contains_one([&it, &compare_func](const T& check_it) { return compare_func(it, check_it); }))
                    tmp_arr.push_back(it);
            tmp_arr.shrink_to_fit();
            swap(tmp_arr);
            return tmp_arr._size() - _size();
        }

        constexpr size_t alone() &
            requires is_equality_comparable<T>
        {
            return alone(0, _size());
        }

        constexpr size_t alone(size_t start_pos) &
            requires is_equality_comparable<T>
        {
            return alone(start_pos, _size());
        }

        constexpr size_t alone(size_t start_pos, size_t end_pos) &
            requires is_equality_comparable<T>
        {
            if (start_pos > end_pos)
                std::swap(start_pos, end_pos);
            if (start_pos + 1 >= end_pos)
                return 0;
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            bit_array_helper selector(end_pos - start_pos);
            size_t i = 0;
            for (T& it : range(start_pos, end_pos)) {
                if (selector.get(i)) {
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
                        selector.set(j, true);
                    }
                    j++;
                }
                if (!is_unique)
                    selector.set(i, true);
                i++;
            }
            i = 0;
            size_t result = remove_if(
                start_pos,
                end_pos,
                [&selector, &i](const T& _) {
                    return selector.get(i++);
                }
            );
            return result;
        }

        template <class FN>
        constexpr size_t alone(FN&& compare_func) &
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            return alone(0, _size(), std::forward<FN>(compare_func));
        }

        template <class FN>
        constexpr size_t alone(size_t start_pos, FN&& compare_func) &
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            return alone(start_pos, _size(), std::forward<FN>(compare_func));
        }

        template <class FN>
        constexpr size_t alone(size_t start_pos, size_t end_pos, FN&& compare_func) &
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            if (start_pos > end_pos)
                std::swap(start_pos, end_pos);
            if (start_pos + 1 >= end_pos)
                return 0;
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            bit_array_helper selector(end_pos - start_pos);
            size_t i = 0;
            for (T& it : range(start_pos, end_pos)) {
                if (selector.get(i)) {
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
                    if (compare_func(it, cmp_it)) {
                        is_unique = false;
                        selector.set(j, true);
                    }
                    j++;
                }
                if (!is_unique)
                    selector.set(i, true);
                i++;
            }
            i = 0;
            size_t result = remove_if(
                start_pos,
                end_pos,
                [&selector, &i](const T& _) {
                    return selector.get(i++);
                }
            );
            return result;
        }

        [[nodiscard]] constexpr list_array<T, Allocator> unique() &&
            requires is_equality_comparable<T>
        {
            unique(0, _size());
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> unique(size_t start_pos) &&
            requires is_equality_comparable<T>
        {
            unique(start_pos, _size());
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> unique(size_t start_pos, size_t end_pos) &&
            requires is_equality_comparable<T>
        {
            unique(start_pos, end_pos);
            return std::move(*this);
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> unique(FN&& compare_func) &&
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            unique(0, _size(), std::forward<FN>(compare_func));
            return std::move(*this);
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> unique(size_t start_pos, FN&& compare_func) &&
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            unique(start_pos, _size(), std::forward<FN>(compare_func));
            return std::move(*this);
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> unique(size_t start_pos, size_t end_pos, FN&& compare_func) &&
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            unique(start_pos, end_pos, std::forward<FN>(compare_func));
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> unify() &&
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            unify(0, _size());
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> unify(size_t start_pos) &&
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            unify(start_pos, _size());
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> unify(size_t start_pos, size_t end_pos) &&
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            unify(start_pos, end_pos);
            return std::move(*this);
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> unify(FN&& compare_func) &&
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            unify(0, _size(), std::forward<FN>(compare_func));
            return std::move(*this);
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> unify(size_t start_pos, FN&& compare_func) &&
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            unify(start_pos, _size(), std::forward<FN>(compare_func));
            return std::move(*this);
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> unify(size_t start_pos, size_t end_pos, FN&& compare_func) &&
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            unify(start_pos, end_pos, std::forward<FN>(compare_func));
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> alone() &&
            requires is_equality_comparable<T>
        {
            alone(0, _size());
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> alone(size_t start_pos) &&
            requires is_equality_comparable<T>
        {
            alone(start_pos, _size());
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> alone(size_t start_pos, size_t end_pos) &&
            requires is_equality_comparable<T>
        {
            alone(start_pos, end_pos);
            return std::move(*this);
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> alone(FN&& compare_func) &&
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            alone(0, _size(), std::forward<FN>(compare_func));
            return std::move(*this);
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> alone(size_t start_pos, FN&& compare_func) &&
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            alone(start_pos, _size(), std::forward<FN>(compare_func));
            return std::move(*this);
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> alone(size_t start_pos, size_t end_pos, FN&& compare_func) &&
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            alone(start_pos, end_pos, std::forward<FN>(compare_func));
            return std::move(*this);
        }

#pragma endregion
#pragma region join

        template <class FN>
        constexpr list_array<T, Allocator>& join(const T& insert_item, FN&& where_join) &
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_item, 0, _size(), std::forward<FN>(where_join));
        }

        template <class FN = bool (*)(const T&)>
        constexpr list_array<T, Allocator>& join(const T& insert_item, size_t start_pos, FN&& where_join) &
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_item, start_pos, _size(), std::forward<FN>(where_join));
        }

        template <class FN>
        constexpr list_array<T, Allocator>& join(const T& insert_item, size_t start_pos, size_t end_pos, FN&& where_join) &
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_item, start_pos, end_pos, std::forward<FN>(where_join));
        }

        template <class AnyAllocator, class FN>
        constexpr list_array<T, Allocator>& join(const list_array<T, AnyAllocator>& insert_items, FN&& where_join) &
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_items, 0, _size(), std::forward<FN>(where_join));
        }

        template <class AnyAllocator, class FN>
        constexpr list_array<T, Allocator>& join(const list_array<T, AnyAllocator>& insert_items, size_t start_pos, FN&& where_join) &
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_items, start_pos, _size(), std::forward<FN>(where_join));
        }

        template <class AnyAllocator, class FN>
        constexpr list_array<T, Allocator>& join(const list_array<T, AnyAllocator>& insert_items, size_t start_pos, size_t end_pos, FN&& where_join) &
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_items, start_pos, end_pos, std::forward<FN>(where_join));
        }

        template <size_t arr_size, class FN>
        constexpr list_array<T, Allocator>& join(const T (&insert_items)[arr_size], FN&& where_join) &
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_items, arr_size, 0, _size(), std::forward<FN>(where_join));
        }

        template <size_t arr_size, class FN>
        constexpr list_array<T, Allocator>& join(const T (&insert_items)[arr_size], size_t start_pos, FN&& where_join) &
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_items, arr_size, start_pos, _size(), std::forward<FN>(where_join));
        }

        template <size_t arr_size, class FN>
        constexpr list_array<T, Allocator>& join(const T (&insert_items)[arr_size], size_t start_pos, size_t end_pos, FN&& where_join) &
            requires std::is_copy_constructible_v<T>
        {
            return *this = std::move(*this).join(insert_items, arr_size, start_pos, end_pos, std::forward<FN>(where_join));
        }

        template <class FN>
        constexpr list_array<T, Allocator>& join(const T* insert_items, size_t items_count, FN&& where_join) &
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_items, items_count, 0, _size(), std::forward<FN>(where_join));
        }

        template <class FN>
        constexpr list_array<T, Allocator>& join(const T* insert_items, size_t items_count, size_t start_pos, FN&& where_join) &
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_items, items_count, start_pos, _size(), std::forward<FN>(where_join));
        }

        template <class FN>
        constexpr list_array<T, Allocator>& join(const T* insert_items, size_t items_count, size_t start_pos, size_t end_pos, FN&& where_join) &
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_items, items_count, start_pos, end_pos, std::forward<FN>(where_join));
        }

        constexpr list_array<T, Allocator>& join(const T& insert_item) &
            requires std::is_copy_constructible_v<T>
        {
            return *this = std::move(*this).join(insert_item, 0, _size());
        }

        constexpr list_array<T, Allocator>& join(const T& insert_item, size_t start_pos) &
            requires std::is_copy_constructible_v<T>
        {
            return *this = std::move(*this).join(insert_item, start_pos, _size());
        }

        constexpr list_array<T, Allocator>& join(const T& insert_item, size_t start_pos, size_t end_pos) &
            requires std::is_copy_constructible_v<T>
        {
            return *this = std::move(*this).join(insert_item, start_pos, end_pos);
        }

        template <class AnyAllocator>
        constexpr list_array<T, Allocator>& join(const list_array<T, AnyAllocator>& insert_items) &
            requires std::is_copy_constructible_v<T>
        {
            return *this = std::move(*this).join(insert_items, 0, _size());
        }

        template <class AnyAllocator>
        constexpr list_array<T, Allocator>& join(const list_array<T, AnyAllocator>& insert_items, size_t start_pos) &
            requires std::is_copy_constructible_v<T>
        {
            return *this = std::move(*this).join(insert_items, start_pos, _size());
        }

        template <class AnyAllocator>
        constexpr list_array<T, AnyAllocator>& join(const list_array<T, AnyAllocator>& insert_items, size_t start_pos, size_t end_pos) &
            requires std::is_copy_constructible_v<T>
        {
            return *this = std::move(*this).join(insert_items, start_pos, end_pos);
        }

        constexpr list_array<T, Allocator>& join(const T* insert_items, size_t items_count) &
            requires std::is_copy_constructible_v<T>
        {
            return *this = std::move(*this).join(insert_items, items_count, 0, _size());
        }

        constexpr list_array<T, Allocator>& join(const T* insert_items, size_t items_count, size_t start_pos) &
            requires std::is_copy_constructible_v<T>
        {
            return *this = std::move(*this).join(insert_items, items_count, start_pos, _size());
        }

        constexpr list_array<T, Allocator>& join(const T* insert_items, size_t items_count, size_t start_pos, size_t end_pos) &
            requires std::is_copy_constructible_v<T>
        {
            return *this = std::move(*this).join(insert_items, items_count, start_pos, end_pos);
        }

        template <size_t arr_size>
        constexpr list_array<T, Allocator>& join(const T (&insert_items)[arr_size]) &
            requires std::is_copy_constructible_v<T>
        {
            return *this = std::move(*this).join(insert_items, arr_size, 0, _size());
        }

        template <size_t arr_size>
        constexpr list_array<T, Allocator>& join(const T (&insert_items)[arr_size], size_t start_pos) &
            requires std::is_copy_constructible_v<T>
        {
            return *this = std::move(*this).join(insert_items, arr_size, start_pos, _size());
        }

        template <size_t arr_size>
        constexpr list_array<T, Allocator>& join(const T (&insert_items)[arr_size], size_t start_pos, size_t end_pos) &
            requires std::is_copy_constructible_v<T>
        {
            return *this = std::move(*this).join(insert_items, arr_size, start_pos, end_pos);
        }

        template <class FN = bool (*)(const T&)>
        [[nodiscard]] constexpr list_array<T, Allocator> join(const T& insert_item, FN&& where_join) &&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return std::move(*this).join(insert_item, 0, _size(), std::forward<FN>(where_join));
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> join(const T& insert_item, size_t start_pos, FN&& where_join) &&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return std::move(*this).join(insert_item, start_pos, _size(), std::forward<FN>(where_join));
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> join(const T& insert_item, size_t start_pos, size_t end_pos, FN&& where_join) &&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            list_array<T, Allocator> res;
            res.reserve(_size() * 2);

            if constexpr (std::is_invocable_r_v<bool, FN, const T&>) {
                for (auto& i : range(start_pos, end_pos)) {
                    bool make_join = where_join(i);
                    res.push_back(std::move(i));
                    if (make_join)
                        res.push_back(insert_item);
                }
            } else {
                auto end = get_iterator(end_pos);
                for (const_iterator begin = get_iterator(start_pos); begin != end; ++begin) {
                    bool make_join = where_join(begin.absolute_index, *begin);
                    res.push_back(std::move(*begin));
                    if (make_join)
                        res.push_back(insert_item);
                }
            }

            return res;
        }

        template <class AnyAllocator, class FN = bool (*)(const T&)>
        [[nodiscard]] constexpr list_array<T, Allocator> join(const list_array<T, AnyAllocator>& insert_items, FN&& where_join) &&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return std::move(*this).join(insert_items, 0, _size(), std::forward<FN>(where_join));
        }

        template <class AnyAllocator, class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> join(const list_array<T, AnyAllocator>& insert_items, size_t start_pos, FN&& where_join) &&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return std::move(*this).join(insert_items, start_pos, _size(), std::forward<FN>(where_join));
        }

        template <class AnyAllocator, class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> join(const list_array<T, AnyAllocator>& insert_items, size_t start_pos, size_t end_pos, FN&& where_join) &&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            list_array<T, Allocator> res;
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            if constexpr (std::is_invocable_r_v<bool, FN, const T&>) {
                for (auto& i : range(start_pos, end_pos)) {
                    bool make_join = where_join(i);
                    res.push_back(std::move(i));
                    if (make_join)
                        res.push_back(insert_items);
                }
            } else {
                auto end = get_iterator(end_pos);
                for (const_iterator begin = get_iterator(start_pos); begin != end; ++begin) {
                    bool make_join = where_join(begin.absolute_index, *begin);
                    res.push_back(std::move(*begin));
                    if (make_join)
                        res.push_back(insert_items);
                }
            }

            return res;
        }

        template <size_t arr_size, class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> join(const T (&insert_items)[arr_size], FN&& where_join) &&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return std::move(*this).join(insert_items, arr_size, 0, _size(), std::forward<FN>(where_join));
        }

        template <size_t arr_size, class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> join(const T (&insert_items)[arr_size], size_t start_pos, FN&& where_join) &&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return std::move(*this).join(insert_items, arr_size, start_pos, _size(), std::forward<FN>(where_join));
        }

        template <size_t arr_size, class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> join(const T (&insert_items)[arr_size], size_t start_pos, size_t end_pos, FN&& where_join) &&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return std::move(*this).join(insert_items, arr_size, start_pos, end_pos, std::forward<FN>(where_join));
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> join(const T* insert_items, size_t items_count, FN&& where_join) &&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return std::move(*this).join(insert_items, items_count, 0, _size(), std::forward<FN>(where_join));
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> join(const T* insert_items, size_t items_count, size_t start_pos, FN&& where_join) &&
            requires std::is_copy_constructible_v<T>
        {
            return std::move(*this).join(insert_items, items_count, start_pos, _size(), std::forward<FN>(where_join));
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> join(const T* insert_items, size_t items_count, size_t start_pos, size_t end_pos, FN&& where_join) &&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            list_array<T, Allocator> res;
            res.reserve_back(_size() * 2);
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            if constexpr (std::is_invocable_r_v<bool, FN, const T&>) {
                for (auto& i : range(start_pos, end_pos)) {
                    bool make_join = where_join(i);
                    res.push_back(std::move(i));
                    if (make_join)
                        res.push_back(insert_items, items_count);
                }
            } else {
                auto end = get_iterator(end_pos);
                for (const_iterator begin = get_iterator(start_pos); begin != end; ++begin) {
                    bool make_join = where_join(begin.absolute_index, *begin);
                    res.push_back(std::move(*begin));
                    if (make_join)
                        res.push_back(insert_items, items_count);
                }
            }

            return res;
        }

        [[nodiscard]] constexpr list_array<T, Allocator> join(const T& insert_item) &&
            requires std::is_copy_constructible_v<T>
        {
            return std::move(*this).join(insert_item, 0, _size());
        }

        [[nodiscard]] constexpr list_array<T, Allocator> join(const T& insert_item, size_t start_pos) &&
            requires std::is_copy_constructible_v<T>
        {
            return std::move(*this).join(insert_item, start_pos, _size());
        }

        [[nodiscard]] constexpr list_array<T, Allocator> join(const T& insert_item, size_t start_pos, size_t end_pos) &&
            requires std::is_copy_constructible_v<T>
        {
            list_array<T, Allocator> res;
            res.reserve_back(_size() * 2);
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            for (auto& i : range(start_pos, end_pos)) {
                res.push_back(std::move(i));
                res.push_back(insert_item);
            }

            return res;
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr list_array<T, Allocator> join(const list_array<T, AnyAllocator>& insert_items) &&
            requires std::is_copy_constructible_v<T>
        {
            return std::move(*this).join(insert_items, 0, _size());
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr list_array<T, Allocator> join(const list_array<T, AnyAllocator>& insert_items, size_t start_pos) &&
            requires std::is_copy_constructible_v<T>
        {
            return std::move(*this).join(insert_items, start_pos, _size());
        }

        template <class AnyAllocator>
        constexpr list_array<T, AnyAllocator> join(const list_array<T, AnyAllocator>& insert_items, size_t start_pos, size_t end_pos) &&
            requires std::is_copy_constructible_v<T>
        {
            list_array<T, AnyAllocator> res(insert_items.get_allocator());
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            res.reserve_back(_size() * (insert_items.size() + 1));
            for (auto& i : range(start_pos, end_pos)) {
                res.push_back(std::move(i));
                res.push_back(insert_items);
            }

            return res;
        }

        [[nodiscard]] constexpr list_array<T, Allocator> join(const T* insert_items, size_t items_count) &&
            requires std::is_copy_constructible_v<T>
        {
            return std::move(*this).join(insert_items, items_count, 0, _size());
        }

        [[nodiscard]] constexpr list_array<T, Allocator> join(const T* insert_items, size_t items_count, size_t start_pos) &&
            requires std::is_copy_constructible_v<T>
        {
            return std::move(*this).join(insert_items, items_count, start_pos, _size());
        }

        [[nodiscard]] constexpr list_array<T, Allocator> join(const T* insert_items, size_t items_count, size_t start_pos, size_t end_pos) &&
            requires std::is_copy_constructible_v<T>
        {
            list_array<T, Allocator> res;
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            res.reserve_back(_size() * (items_count + 1));
            for (auto& i : range(start_pos, end_pos)) {
                res.push_back(std::move(i));
                res.push_back(insert_items, items_count);
            }

            return res;
        }

        template <size_t arr_size>
        [[nodiscard]] constexpr list_array<T, Allocator> join(const T (&insert_items)[arr_size]) &&
            requires std::is_copy_constructible_v<T>
        {
            return std::move(*this).join(insert_items, arr_size, 0, _size());
        }

        template <size_t arr_size>
        [[nodiscard]] constexpr list_array<T, Allocator> join(const T (&insert_items)[arr_size], size_t start_pos) &&
            requires std::is_copy_constructible_v<T>
        {
            return std::move(*this).join(insert_items, arr_size, start_pos, _size());
        }

        template <size_t arr_size>
        [[nodiscard]] constexpr list_array<T, Allocator> join(const T (&insert_items)[arr_size], size_t start_pos, size_t end_pos) &&
            requires std::is_copy_constructible_v<T>
        {
            return std::move(*this).join(insert_items, arr_size, start_pos, end_pos);
        }

#pragma endregion
#pragma region join_with

        template <class AnyT, class AnyAllocator, class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> join_with(const list_array<AnyT, AnyAllocator>& array, FN&& iterate_fn)
            requires std::convertible_to<AnyT, T> && (std::is_invocable_r_v<T, FN, const T&, const AnyT&> || std::is_invocable_r_v<T, FN, size_t, const T&, const AnyT&>)
        {
            return join_with(array, 0, _size(), std::forward<FN>(iterate_fn));
        }

        template <class AnyT, class AnyAllocator, class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> join_with(const list_array<AnyT, AnyAllocator>& array, size_t start_pos, FN&& iterate_fn)
            requires std::convertible_to<AnyT, T> && (std::is_invocable_r_v<T, FN, const T&, const AnyT&> || std::is_invocable_r_v<T, FN, size_t, const T&, const AnyT&>)
        {
            return join_with(array, start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        template <class AnyT, class AnyAllocator, class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> join_with(const list_array<AnyT, AnyAllocator>& array, size_t start_pos, size_t end_pos, FN&& iterate_fn)
            requires std::convertible_to<AnyT, T> && (std::is_invocable_r_v<T, FN, const T&, const AnyT&> || std::is_invocable_r_v<T, FN, size_t, const T&, const AnyT&>)
        {
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            if (array.size() == 0)
                return {};
            list_array<T, Allocator> result;
            result.reserve(_size() * array.size());
            if constexpr (std::is_invocable_v<FN, size_t, const T&, const AnyT&>) {
                size_t pos = start_pos;
                for (const T& i : range(start_pos, end_pos)) {
                    for (auto& it : array)
                        result.push_back(iterate_fn(pos, i, it));
                    ++pos;
                }
            } else
                for (const T& i : range(start_pos, end_pos))
                    for (auto& it : array)
                        result.push_back(iterate_fn(i, it));
            return result;
        }

        template <class AnyT, class AnyAllocator, class ResAllocator, class FN>
        [[nodiscard]] constexpr list_array<T, ResAllocator> join_with(const list_array<AnyT, AnyAllocator>& array, FN&& iterate_fn)
            requires std::convertible_to<AnyT, T> && (std::is_invocable_r_v<list_array<T, ResAllocator>, FN, const T&, const AnyT&> || std::is_invocable_r_v<list_array<T, ResAllocator>, FN, size_t, const T&, const AnyT&>)
        {
            return join_with(array, 0, _size(), std::forward<FN>(iterate_fn));
        }

        template <class AnyT, class AnyAllocator, class ResAllocator, class FN>
        [[nodiscard]] constexpr list_array<T, ResAllocator> join_with(const list_array<AnyT, AnyAllocator>& array, size_t start_pos, FN&& iterate_fn)
            requires std::convertible_to<AnyT, T> && (std::is_invocable_r_v<list_array<T, ResAllocator>, FN, const T&, const AnyT&> || std::is_invocable_r_v<list_array<T, ResAllocator>, FN, size_t, const T&, const AnyT&>)
        {
            return join_with(array, start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        template <class AnyT, class AnyAllocator, class ResAllocator, class FN>
        [[nodiscard]] constexpr list_array<T, ResAllocator> join_with(const list_array<AnyT, AnyAllocator>& array, size_t start_pos, size_t end_pos, FN&& iterate_fn)
            requires std::convertible_to<AnyT, T> && (std::is_invocable_r_v<list_array<T, ResAllocator>, FN, const T&, const AnyT&> || std::is_invocable_r_v<list_array<T, ResAllocator>, FN, size_t, const T&, const AnyT&>)
        {
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            if (array.size() == 0)
                return {};
            list_array<T, ResAllocator> result;
            result.reserve(_size() * array.size());
            if constexpr (std::is_invocable_v<FN, size_t, const T&, const AnyT&>) {
                size_t pos = start_pos;
                for (const T& i : range(start_pos, end_pos)) {
                    for (auto& it : array)
                        result.push_back(iterate_fn(pos, i, it));
                    ++pos;
                }
            } else
                for (const T& i : range(start_pos, end_pos))
                    for (auto& it : array)
                        result.push_back(iterate_fn(i, it));
            return result;
        }

#pragma endregion
#pragma region contains

        [[nodiscard]] constexpr bool contains(const T& value) const&
            requires is_equality_comparable<T>
        {
            return contains(0, _size(), value);
        }

        [[nodiscard]] constexpr bool contains(size_t start, const T& value) const&
            requires is_equality_comparable<T>
        {
            return contains(start, _size(), value);
        }

        [[nodiscard]] constexpr bool contains(size_t start, size_t end, const T& value) const&
            requires is_equality_comparable<T>
        {
            return find(start, end, value) != npos;
        }

        template <size_t arr_size>
        [[nodiscard]] constexpr bool contains(const T (&arr)[arr_size]) const&
            requires is_equality_comparable<T>
        {
            return contains(0, _size(), arr, arr_size);
        }

        template <size_t arr_size>
        [[nodiscard]] constexpr bool contains(size_t start, const T (&arr)[arr_size]) const&
            requires is_equality_comparable<T>
        {
            return contains(start, _size(), arr, arr_size);
        }

        template <size_t arr_size>
        [[nodiscard]] constexpr bool contains(size_t start, size_t end, const T (&arr)[arr_size]) const&
            requires is_equality_comparable<T>
        {
            return contains(start, end, arr, arr_size);
        }

        [[nodiscard]] constexpr bool contains(const T* arr, size_t arr_size) const&
            requires is_equality_comparable<T>
        {
            return contains(0, _size(), arr, arr_size);
        }

        [[nodiscard]] constexpr bool contains(size_t start, const T* arr, size_t arr_size) const&
            requires is_equality_comparable<T>
        {
            return contains(start, _size(), arr, arr_size);
        }

        [[nodiscard]] constexpr bool contains(size_t start, size_t end, const T* arr, size_t arr_size) const&
            requires is_equality_comparable<T>
        {
            return find(start, end, arr, arr + arr_size) != npos;
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr bool contains(const list_array<T, AnyAllocator>& value) const&
            requires is_equality_comparable<T>
        {
            return contains(0, _size(), value);
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr bool contains(size_t start, const list_array<T, AnyAllocator>& value) const&
            requires is_equality_comparable<T>
        {
            return contains(start, _size(), value);
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr bool contains(size_t start, size_t end, const list_array<T, AnyAllocator>& value) const&
            requires is_equality_comparable<T>
        {
            return contains(start, end, value);
        }

        template <class any_iter>
        [[nodiscard]] constexpr bool contains(any_iter value_begin, any_iter value_end) const&
            requires is_equality_comparable_with<T, typename std::iterator_traits<any_iter>::value_type>
        {
            return contains(0, _size(), value_begin, value_end);
        }

        template <class any_iter>
        [[nodiscard]] constexpr bool contains(size_t start, any_iter value_begin, any_iter value_end) const&
            requires is_equality_comparable_with<T, typename std::iterator_traits<any_iter>::value_type>
        {
            return contains(start, _size(), value_begin, value_end);
        }

        template <class any_iter>
        [[nodiscard]] constexpr bool contains(size_t start, size_t end, any_iter value_begin, any_iter value_end) const&
            requires is_equality_comparable_with<T, typename std::iterator_traits<any_iter>::value_type>
        {
            return find(start, end, value_begin, value_end) != npos;
        }

        template <class FN>
        [[nodiscard]] constexpr bool contains_one(FN&& check_function) const&
            requires std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>
        {
            return contains_one(0, _size(), std::forward<FN>(check_function));
        }

        template <class FN>
        [[nodiscard]] constexpr size_t contains_multiply(FN&& check_function) const&
            requires std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>
        {
            return contains_multiply(0, _size(), std::forward<FN>(check_function));
        }

        template <class FN>
        [[nodiscard]] constexpr bool contains_one(size_t start, FN&& check_function) const&
            requires std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>
        {
            return contains_one(start, _size(), std::forward<FN>(check_function));
        }

        template <class FN>
        [[nodiscard]] constexpr size_t contains_multiply(size_t start, FN&& check_function) const&
            requires std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>
        {
            return contains_multiply(start, _size(), std::forward<FN>(check_function));
        }

        template <class FN>
        [[nodiscard]] constexpr bool contains_one(size_t start, size_t end, FN&& check_function) const&
            requires std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>
        {
            return find_if(start, end, std::forward<FN>(check_function)) != npos;
        }

        template <class FN>
        [[nodiscard]] constexpr size_t contains_multiply(size_t start, size_t end, FN&& check_function) const&
            requires std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>
        {
            size_t i = 0;
            const_iterator _end = get_iterator(end);
            if constexpr (std::is_invocable_r_v<bool, FN, size_t, T>) {
                for (const_iterator it = get_iterator(start); it != _end; ++it)
                    if (check_function(it.absolute_index, *it))
                        ++i;
            } else
                for (const_iterator it = get_iterator(start); it != _end; ++it)
                    if (check_function(*it))
                        ++i;
            return i;
        }

#pragma endregion
#pragma region sort

        constexpr list_array<T, Allocator>& sort() & {
            if (empty())
                return *this;
            if constexpr (std::is_unsigned_v<T>) {
                const T& min_val = min();
                size_t dif = max() - min_val + 1;
                list_array<size_t, std::allocator<size_t>> count_arr(dif);
                list_array<T, Allocator> result(_size());
                {
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
                size_t min_val = normalize(min());
                size_t dif = normalize(max()) - min_val + 1;
                list_array<size_t, std::allocator<size_t>> count_arr(dif, 0);
                list_array<T, Allocator> result(_size());
                {
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
            } else
                sort([](auto& first, auto& second) { return first < second; });
            return *this;
        }

        template <class FN>
        constexpr list_array<T, Allocator>& sort(FN&& compare) & {
            if (empty())
                return *this;
            size_t curr_L_size = _size() / 2 + 1;
            size_t curr_M_size = _size() / 2 + 1;
            T* L = allocator_and_size.allocate(_size() / 2 + 1);
            T* M = allocator_and_size.allocate(_size() / 2 + 1);
            auto fix_size = [&L, &M, &curr_L_size, &curr_M_size, this](size_t start, size_t middle, size_t end) {
                size_t l = middle - start + 1;
                size_t m = end - middle + 1;
                if (curr_L_size < l) {
                    allocator_and_size.deallocate(L, curr_L_size);
                    L = allocator_and_size.allocate(l);
                    curr_L_size = l;
                }
                if (curr_M_size < m) {
                    allocator_and_size.deallocate(M, curr_M_size);
                    M = allocator_and_size.allocate(m);
                    curr_M_size = m;
                }
            };
            auto merge = [&](size_t start, size_t middle, size_t end) {
                size_t n1 = middle - start;
                size_t n2 = end - middle;
                if (curr_L_size < n1 || curr_M_size < n2)
                    fix_size(start, middle, end);
                get_iterator(start).template _fast_load<true, true>(L, n1);
                get_iterator(middle).template _fast_load<true, true>(M, n2);
                size_t i = 0, j = 0;
                for (T& it : range(start, end)) {
                    if (i < n1 && j < n2)
                        it = std::move(compare(L[i], M[j]) ? L[i++] : M[j++]);
                    else if (i < n1)
                        it = std::move(L[i++]);
                    else if (j < n2)
                        it = std::move(M[j++]);
                }
                for (size_t l = 0; l < n1; l++)
                    L[l].~T();
                for (size_t l = 0; l < n2; l++)
                    M[l].~T();
            };
            for (size_t b = 2; b < _size(); b <<= 1) {
                for (size_t i = 0; i < _size(); i += b) {
                    if (i + b > _size())
                        merge(i, i + ((_size() - i) >> 1), _size());
                    else
                        merge(i, i + (b >> 1), i + b);
                }
                if (b << 1 > _size())
                    merge(0, b, _size());
            }
            auto non_sorted_finder = [&](size_t continue_search) {
                const auto* check = &operator[](continue_search);
                size_t res = continue_search;
                for (const T& it : *this) {
                    if (!compare(*check, it))
                        return res;
                    check = &it;
                    ++res;
                }
                return size_t(0);
            };
            size_t err = 0;
            while ((err = non_sorted_finder(err)))
                merge(0, err ? err - 1 : 0, _size());
            allocator_and_size.deallocate(L, curr_L_size);
            allocator_and_size.deallocate(M, curr_M_size);
            return *this;
        }

        [[nodiscard]] constexpr list_array<T, Allocator> sort() && {
            sort();
            return std::move(*this);
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> sort(FN&& compare) && {
            sort(std::forward<FN>(compare));
            return std::move(*this);
        }

#pragma endregion
#pragma region concat

        template <class... Arguments>
        [[nodiscard]] static constexpr list_array<T, Allocator> concat(Arguments&&... args) {
            list_array<T, Allocator> result;
            result.reserve(sizeof...(Arguments));
            (result.push_back(std::forward<Arguments>(args)), ...);
            return result;
        }

        template <class AnyAllocator0, class AnyAllocator1>
        static constexpr list_array<T, Allocator> concat(const list_array<list_array<T, AnyAllocator1>, AnyAllocator0>& concat_arr) {
            list_array<T, AnyAllocator1> res;
            for (auto& i : concat_arr)
                res.push_back(i);
            return res;
        }

        template <class AnyAllocator0, class AnyAllocator1>
        static constexpr list_array<T, Allocator> concat(list_array<list_array<T, AnyAllocator1>, AnyAllocator0>&& concat_arr) {
            list_array<T, AnyAllocator1> res;
            for (auto& i : concat_arr)
                res.push_back(std::move(i));
            return res;
        }

        template <class Y = T>
        [[nodiscard]] constexpr typename is_container<Y>::container concat() & {
            T res;
            for (auto& i : *this)
                res.push_back(i);
            return res;
        }

        template <class Y = T>
        [[nodiscard]] constexpr typename is_container<Y>::container concat() && {
            T res;
            for (auto& i : *this)
                res.push_back(std::move(i));
            return res;
        }

        template <class AnyAllocator>
        [[nodiscard]] list_array<T, Allocator> operator+(const list_array<T, AnyAllocator>& op) && {
            return take().push_back(op);
        }

        template <class AnyAllocator>
        [[nodiscard]] list_array<T, Allocator> operator+(const list_array<T, AnyAllocator>& op) const& {
            return list_array<T, Allocator>(*this).push_back(op);
        }

        template <class AnyAllocator>
        list_array<T, Allocator>& operator+=(const list_array<T, AnyAllocator>& op) {
            return push_back(op);
        }

#pragma endregion
#pragma region where

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> where(FN&& check_fn) const&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&> || is_apply_invocable_r_v<bool, FN, T>)
        {
            return where(0, _size(), std::forward<FN>(check_fn));
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> where(size_t start_pos, FN&& check_fn) const&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&> || is_apply_invocable_r_v<bool, FN, T>)
        {
            return where(start_pos, _size(), std::forward<FN>(check_fn));
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> where(size_t start_pos, size_t end_pos, FN&& check_fn) const&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&> || is_apply_invocable_r_v<bool, FN, T>)
        {
            list_array<T, Allocator> res;
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            res.reserve_back(end_pos - start_pos);
            if constexpr (std::is_invocable_r_v<bool, FN, size_t, const T&>) {
                auto end = get_iterator(end_pos);
                for (const_iterator begin = get_iterator(start_pos); begin != end; ++begin)
                    if (check_fn(begin.absolute_index, *begin))
                        res.push_back(*begin);
            } else if constexpr (is_apply_invocable_r_v<bool, FN, T>) {
                for (T& i : reverse_range(start_pos, end_pos))
                    if (std::apply(check_fn, i))
                        res.push_back(i);
            } else {
                for (auto& i : range(start_pos, end_pos))
                    if (check_fn(i))
                        res.push_back(i);
            }

            res.shrink_to_fit();
            return res;
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> where(FN&& check_fn) &&
            requires std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&> || is_apply_invocable_r_v<bool, FN, T>
        {
            return take().where(0, _size(), std::forward<FN>(check_fn));
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> where(size_t start_pos, FN&& check_fn) &&
            requires std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&> || is_apply_invocable_r_v<bool, FN, T>
        {
            return take().where(start_pos, _size(), std::forward<FN>(check_fn));
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> where(size_t start_pos, size_t end_pos, FN&& check_fn) &&
            requires std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&> || is_apply_invocable_r_v<bool, FN, T>
        {
            list_array<T, Allocator> res;
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            res.reserve_back(end_pos - start_pos);
            if constexpr (std::is_invocable_r_v<bool, FN, size_t, const T&>) {
                auto end = get_iterator(end_pos);
                for (const_iterator begin = get_iterator(start_pos); begin != end; ++begin)
                    if (check_fn(begin.absolute_index, *begin))
                        res.push_back(std::move(*begin));
            } else if constexpr (is_apply_invocable_r_v<bool, FN, T>) {
                for (T& i : reverse_range(start_pos, end_pos))
                    if (std::apply(check_fn, i))
                        res.push_back(std::move(i));
            } else {
                for (auto& i : range(start_pos, end_pos))
                    if (check_fn(i))
                        res.push_back(std::move(i));
            }

            res.shrink_to_fit();
            return res;
        }

#pragma endregion
#pragma region for each

        template <class FN>
        constexpr list_array<T, Allocator>& for_each_reverse(FN&& iterate_fn) &
            requires std::is_invocable_v<FN, T&> || std::is_invocable_v<FN, size_t, T&> || is_apply_invocable_v<FN, T>
        {
            return for_each_reverse(0, _size(), std::forward<FN>(iterate_fn));
        }

        template <class FN>
        constexpr list_array<T, Allocator>& for_each_reverse(size_t start_pos, FN&& iterate_fn) &
            requires std::is_invocable_v<FN, T&> || std::is_invocable_v<FN, size_t, T&> || is_apply_invocable_v<FN, T>
        {
            return for_each_reverse(start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        template <class FN>
        constexpr list_array<T, Allocator>& for_each_reverse(size_t start_pos, size_t end_pos, FN&& iterate_fn) &
            requires std::is_invocable_v<FN, T&> || std::is_invocable_v<FN, size_t, T&> || is_apply_invocable_v<FN, T>
        {
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            if constexpr (std::is_invocable_v<FN, size_t, T&>) {
                size_t pos = start_pos;
                for (T& i : reverse_range(start_pos, end_pos))
                    iterate_fn(--pos, i);
            } else if constexpr (is_apply_invocable_v<FN, T>)
                for (T& i : reverse_range(start_pos, end_pos))
                    std::apply(iterate_fn, i);
            else
                for (T& i : reverse_range(start_pos, end_pos))
                    iterate_fn(i);

            return *this;
        }

        template <class FN>
        constexpr void for_each_reverse(FN&& iterate_fn) &&
            requires std::is_invocable_v<FN, T&&> || std::is_invocable_v<FN, size_t, T&&> || is_apply_invocable_v<FN, T>
        {
            std::move(*this).for_each_reverse(0, _size(), std::forward<FN>(iterate_fn));
        }

        template <class FN>
        constexpr void for_each_reverse(size_t start_pos, FN&& iterate_fn) &&
            requires std::is_invocable_v<FN, T&&> || std::is_invocable_v<FN, size_t, T&&> || is_apply_invocable_v<FN, T>
        {
            std::move(*this).for_each_reverse(start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        template <class FN>
        constexpr void for_each_reverse(size_t start_pos, size_t end_pos, FN&& iterate_fn) &&
            requires std::is_invocable_v<FN, T&&> || std::is_invocable_v<FN, size_t, T&&> || is_apply_invocable_v<FN, T>
        {
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            if constexpr (std::is_invocable_v<FN, size_t, T&&>) {
                size_t pos = start_pos;
                for (T& i : reverse_range(start_pos, end_pos))
                    iterate_fn(--pos, std::move(i));
            } else if constexpr (is_apply_invocable_v<FN, T>)
                for (T& i : reverse_range(start_pos, end_pos))
                    std::apply(iterate_fn, std::move(i));
            else
                for (T& i : reverse_range(start_pos, end_pos))
                    iterate_fn(std::move(i));
        }

        template <class FN>
        constexpr const list_array<T, Allocator>& for_each_reverse(FN&& iterate_fn) const&
            requires std::is_invocable_v<FN, const T&> || std::is_invocable_v<FN, size_t, const T&> || is_apply_invocable_v<FN, T>
        {
            return for_each_reverse(0, _size(), std::forward<FN>(iterate_fn));
        }

        template <class FN>
        constexpr const list_array<T, Allocator>& for_each_reverse(size_t start_pos, FN&& iterate_fn) const&
            requires std::is_invocable_v<FN, const T&> || std::is_invocable_v<FN, size_t, const T&> || is_apply_invocable_v<FN, T>
        {
            return for_each_reverse(start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        template <class FN>
        constexpr const list_array<T, Allocator>& for_each_reverse(size_t start_pos, size_t end_pos, FN&& iterate_fn) const&
            requires std::is_invocable_v<FN, const T&> || std::is_invocable_v<FN, size_t, const T&> || is_apply_invocable_v<FN, T>
        {
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            if constexpr (std::is_invocable_v<FN, size_t, const T&>) {
                size_t pos = end_pos;
                for (const T& i : reverse_range(start_pos, end_pos))
                    iterate_fn(--pos, i);
            } else if constexpr (is_apply_invocable_v<FN, T>)
                for (const T& i : reverse_range(start_pos, end_pos))
                    std::apply(iterate_fn, i);
            else
                for (const T& i : reverse_range(start_pos, end_pos))
                    iterate_fn(i);
            return *this;
        }

        template <class FN>
        constexpr list_array<T, Allocator>& for_each(FN&& iterate_fn) &
            requires std::is_invocable_v<FN, T&> || std::is_invocable_v<FN, size_t, T&> || is_apply_invocable_v<FN, T>
        {
            return for_each(0, _size(), std::forward<FN>(iterate_fn));
        }

        template <class FN>
        constexpr list_array<T, Allocator>& for_each(size_t start_pos, FN&& iterate_fn) &
            requires std::is_invocable_v<FN, T&> || std::is_invocable_v<FN, size_t, T&> || is_apply_invocable_v<FN, T>
        {
            return for_each(start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        template <class FN>
        constexpr list_array<T, Allocator>& for_each(size_t start_pos, size_t end_pos, FN&& iterate_fn) &
            requires std::is_invocable_v<FN, T&> || std::is_invocable_v<FN, size_t, T&> || is_apply_invocable_v<FN, T>
        {
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            if constexpr (std::is_invocable_v<FN, size_t, T&>) {
                size_t pos = start_pos;
                for (T& i : range(start_pos, end_pos))
                    iterate_fn(pos++, i);
            } else if constexpr (is_apply_invocable_v<FN, T>)
                for (T& i : range(start_pos, end_pos))
                    std::apply(iterate_fn, i);
            else
                for (T& i : range(start_pos, end_pos))
                    iterate_fn(i);


            return *this;
        }

        template <class FN>
        constexpr void for_each(FN&& iterate_fn) &&
            requires std::is_invocable_v<FN, T&&> || std::is_invocable_v<FN, size_t, T&&> || is_apply_invocable_v<FN, T>
        {
            std::move(*this).for_each(0, _size(), std::forward<FN>(iterate_fn));
        }

        template <class FN>
        constexpr void for_each(size_t start_pos, FN&& iterate_fn) &&
            requires std::is_invocable_v<FN, T&&> || std::is_invocable_v<FN, size_t, T&&> || is_apply_invocable_v<FN, T>
        {
            std::move(*this).for_each(start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        template <class FN>
        constexpr void for_each(size_t start_pos, size_t end_pos, FN&& iterate_fn) &&
            requires std::is_invocable_v<FN, T&&> || std::is_invocable_v<FN, size_t, T&&> || is_apply_invocable_v<FN, T>
        {
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");

            if constexpr (std::is_invocable_v<FN, size_t, T&&>) {
                size_t pos = start_pos;
                for (T& i : range(start_pos, end_pos))
                    iterate_fn(pos++, std::move(i));
            } else if constexpr (is_apply_invocable_v<FN, T>)
                for (T& i : range(start_pos, end_pos))
                    std::apply(iterate_fn, std::move(i));
            else
                for (T& i : range(start_pos, end_pos))
                    iterate_fn(std::move(i));
        }

        template <class FN>
        constexpr const list_array<T, Allocator>& for_each(FN&& iterate_fn) const&
            requires std::is_invocable_v<FN, const T&> || std::is_invocable_v<FN, size_t, const T&> || is_apply_invocable_v<FN, T>
        {
            return for_each(0, _size(), std::forward<FN>(iterate_fn));
        }

        template <class FN>
        constexpr const list_array<T, Allocator>& for_each(size_t start_pos, FN&& iterate_fn) const&
            requires std::is_invocable_v<FN, const T&> || std::is_invocable_v<FN, size_t, const T&> || is_apply_invocable_v<FN, T>
        {
            return for_each(start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        template <class FN>
        constexpr const list_array<T, Allocator>& for_each(size_t start_pos, size_t end_pos, FN&& iterate_fn) const&
            requires std::is_invocable_v<FN, const T&> || std::is_invocable_v<FN, size_t, const T&> || is_apply_invocable_v<FN, T>
        {
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");

            if constexpr (std::is_invocable_v<FN, size_t, const T&>) {
                size_t pos = start_pos;
                for (const T& i : range(start_pos, end_pos))
                    iterate_fn(pos++, i);
            } else if constexpr (is_apply_invocable_v<FN, T>)
                for (const T& i : range(start_pos, end_pos))
                    std::apply(iterate_fn, i);
            else
                for (const T& i : range(start_pos, end_pos))
                    iterate_fn(i);
            return *this;
        }

#pragma endregion
#pragma region transform

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> transform(FN&& iterate_fn) &&
            requires std::is_invocable_r_v<T, FN, T&&> || std::is_invocable_r_v<T, FN, size_t, T&&> || is_apply_invocable_r_v<T, FN, T>
        {
            return std::move(*this).transform(0, _size(), std::forward<FN>(iterate_fn));
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> transform(size_t start_pos, FN&& iterate_fn) &&
            requires std::is_invocable_r_v<T, FN, T&&> || std::is_invocable_r_v<T, FN, size_t, T&&> || is_apply_invocable_r_v<T, FN, T>
        {
            return std::move(*this).transform(start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> transform(size_t start_pos, size_t end_pos, FN&& iterate_fn) &&
            requires std::is_invocable_r_v<T, FN, T&&> || std::is_invocable_r_v<T, FN, size_t, T&&> || is_apply_invocable_r_v<T, FN, T>
        {
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");

            if constexpr (std::is_invocable_v<FN, size_t, T&&>) {
                size_t pos = start_pos;
                for (T& i : range(start_pos, end_pos))
                    i = iterate_fn(pos++, std::move(i));
            } else if constexpr (std::is_invocable_v<FN, T&&>) {
                for (T& i : range(start_pos, end_pos))
                    i = iterate_fn(std::move(i));
            } else if constexpr (is_apply_invocable_v<FN, T>)
                for (T& i : range(start_pos, end_pos))
                    i = std::apply(iterate_fn, std::move(i));
            else
                for (T& i : range(start_pos, end_pos))
                    i = iterate_fn(std::move(i));
            return take(start_pos, end_pos);
        }

        template <class FN>
        constexpr list_array<T, Allocator>& transform(FN&& iterate_fn) &
            requires std::is_invocable_r_v<T, FN, T&&> || std::is_invocable_r_v<T, FN, size_t, T&&> || is_apply_invocable_r_v<T, FN, T>
        {
            return *this = std::move(*this).transform(iterate_fn);
        }

        template <class FN>
        constexpr list_array<T, Allocator>& transform(size_t start_pos, FN&& iterate_fn) &
            requires std::is_invocable_r_v<T, FN, T&&> || std::is_invocable_r_v<T, FN, size_t, T&&> || is_apply_invocable_r_v<T, FN, T>
        {
            return *this = std::move(*this).transform(start_pos, iterate_fn);
        }

        template <class FN>
        constexpr list_array<T, Allocator>& transform(size_t start_pos, size_t end_pos, FN&& iterate_fn) &
            requires std::is_invocable_r_v<T, FN, T&&> || std::is_invocable_r_v<T, FN, size_t, T&&> || is_apply_invocable_r_v<T, FN, T>
        {
            return *this = std::move(*this).transform(start_pos, end_pos, iterate_fn);
        }

#pragma endregion
#pragma region transform_with

        template <class AnyT, class AnyAllocator, class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> transform_with(const list_array<AnyT, AnyAllocator>& array, FN&& iterate_fn) &&
            requires std::is_invocable_v<FN, T&, const AnyT&> || std::is_invocable_v<FN, size_t, T&, const AnyT&>
        {
            return std::move(*this).transform_with(array, 0, _size(), std::forward<FN>(iterate_fn));
        }

        template <class AnyT, class AnyAllocator, class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> transform_with(const list_array<AnyT, AnyAllocator>& array, size_t start_pos, FN&& iterate_fn) &&
            requires std::is_invocable_v<FN, T&, const AnyT&> || std::is_invocable_v<FN, size_t, T&, const AnyT&>
        {
            return std::move(*this).transform_with(array, start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        template <class AnyT, class AnyAllocator, class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> transform_with(const list_array<AnyT, AnyAllocator>& array, size_t start_pos, size_t end_pos, FN&& iterate_fn) &&
            requires std::is_invocable_v<FN, T&, const AnyT&> || std::is_invocable_v<FN, size_t, T&, const AnyT&>
        {
            if (start_pos > end_pos)
                throw std::invalid_argument("end_pos must be bigger than start_pos");
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            if (array.size() == 0)
                return std::move(*this);
            auto arr_beg = array.begin();
            auto arr_end = array.end();
            if constexpr (std::is_invocable_v<FN, size_t, T&, const AnyT&>) {
                size_t pos = start_pos;
                for (T& i : range(start_pos, end_pos)) {
                    if (arr_beg == arr_end)
                        arr_beg = array.begin();
                    iterate_fn(pos, i, *arr_beg);
                    ++arr_beg;
                    ++pos;
                }
            } else
                for (T& i : range(start_pos, end_pos)) {
                    if (arr_beg == arr_end)
                        arr_beg = array.begin();
                    iterate_fn(i, *arr_beg);
                    ++arr_beg;
                }
            return std::move(*this);
        }

        template <class AnyT, class AnyAllocator = std::allocator<AnyT>, class FN>
        constexpr list_array<T, Allocator>& transform_with(const list_array<AnyT, AnyAllocator>& array, FN&& iterate_fn) &
            requires std::is_invocable_v<FN, T&, const AnyT&> || std::is_invocable_v<FN, size_t, T&, const AnyT&>
        {
            return *this = std::move(*this).transform_with(array, 0, _size(), iterate_fn);
        }

        template <class AnyT, class AnyAllocator = std::allocator<AnyT>, class FN>
        constexpr list_array<T, Allocator>& transform_with(const list_array<AnyT, AnyAllocator>& array, size_t start_pos, FN&& iterate_fn) &
            requires std::is_invocable_v<FN, T&, const AnyT&> || std::is_invocable_v<FN, size_t, T&, const AnyT&>
        {
            return *this = std::move(*this).transform_with(array, start_pos, _size(), iterate_fn);
        }

        template <class AnyT, class AnyAllocator = std::allocator<AnyT>, class FN>
        constexpr list_array<T, Allocator>& transform_with(const list_array<AnyT, AnyAllocator>& array, size_t start_pos, size_t end_pos, FN&& iterate_fn) &
            requires std::is_invocable_v<FN, T&, const AnyT&> || std::is_invocable_v<FN, size_t, T&, const AnyT&>
        {
            return *this = std::move(*this).transform_with(array, start_pos, end_pos, iterate_fn);
        }

#pragma endregion
#pragma region convert

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>>
        [[nodiscard]] constexpr result_array convert() const&
            requires std::convertible_to<T, ConvertTo>
        {
            return convert<ConvertTo>(0, _size());
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>>
        [[nodiscard]] constexpr result_array convert(size_t start_pos) const&
            requires std::convertible_to<T, ConvertTo>
        {
            return convert<ConvertTo>(start_pos, _size());
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>>
        [[nodiscard]] constexpr result_array convert(size_t start_pos, size_t end_pos) const&
            requires std::convertible_to<T, ConvertTo>
        {
            result_array res;
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            res.reserve_back(end_pos - start_pos);
            for (auto& i : range(start_pos, end_pos))
                res.push_back((ConvertTo)(i));
            return res;
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>>
        [[nodiscard]] constexpr result_array convert() &&
            requires std::convertible_to<T, ConvertTo>
        {
            return convert_take<ConvertTo, result_array>(0, _size());
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>>
        [[nodiscard]] constexpr result_array convert(size_t start_pos) &&
            requires std::convertible_to<T, ConvertTo>
        {
            return convert_take<ConvertTo, result_array>(start_pos, _size());
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>>
        [[nodiscard]] constexpr result_array convert(size_t start_pos, size_t end_pos) &&
            requires std::convertible_to<T, ConvertTo>
        {
            return convert_take<ConvertTo, result_array>(start_pos, end_pos);
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>>
        [[nodiscard]] constexpr result_array convert_take()
            requires std::convertible_to<T, ConvertTo>
        {
            return convert_take<ConvertTo, result_array>(0, _size());
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>>
        [[nodiscard]] constexpr result_array convert_take(size_t start_pos)
            requires std::convertible_to<T, ConvertTo>
        {
            return convert_take<ConvertTo, result_array>(start_pos, _size());
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>>
        [[nodiscard]] constexpr result_array convert_take(size_t start_pos, size_t end_pos)
            requires std::convertible_to<T, ConvertTo>
        {
            list_array<T, Allocator> tmp = take(start_pos, end_pos);
            result_array res;
            res.reserve(tmp.size());
            for (auto& i : tmp.range(start_pos, end_pos))
                res.push_back((ConvertTo)(std::move(i)));
            return res;
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class FN>
        [[nodiscard]] constexpr result_array convert(FN&& iterate_fn) const&
            requires(
                (std::is_invocable_v<FN, const T&> && std::convertible_to<std::invoke_result_t<FN, const T&>, ConvertTo>) || (std::is_invocable_v<FN, const T&> && std::convertible_to<std::invoke_result_t<FN, const T&>, result_array>) || (std::is_invocable_v<FN, size_t, const T&> && std::convertible_to<std::invoke_result_t<FN, size_t, const T&>, ConvertTo>) || (std::is_invocable_v<FN, size_t, const T&> && std::convertible_to<std::invoke_result_t<FN, size_t, const T&>, result_array>)
            )
        {
            return convert<ConvertTo>(0, _size(), std::forward<FN>(iterate_fn));
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class FN>
        [[nodiscard]] constexpr result_array convert(size_t start_pos, FN&& iterate_fn) const&
            requires(
                (std::is_invocable_v<FN, const T&> && std::convertible_to<std::invoke_result_t<FN, const T&>, ConvertTo>) || (std::is_invocable_v<FN, const T&> && std::convertible_to<std::invoke_result_t<FN, const T&>, result_array>) || (std::is_invocable_v<FN, size_t, const T&> && std::convertible_to<std::invoke_result_t<FN, size_t, const T&>, ConvertTo>) || (std::is_invocable_v<FN, size_t, const T&> && std::convertible_to<std::invoke_result_t<FN, size_t, const T&>, result_array>)
            )
        {
            return convert<ConvertTo>(start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class FN>
        [[nodiscard]] constexpr result_array convert(size_t start_pos, size_t end_pos, FN&& iterate_fn) const&
            requires(
                (std::is_invocable_v<FN, const T&> && std::convertible_to<std::invoke_result_t<FN, const T&>, ConvertTo>) || (std::is_invocable_v<FN, const T&> && std::convertible_to<std::invoke_result_t<FN, const T&>, result_array>) || (std::is_invocable_v<FN, size_t, const T&> && std::convertible_to<std::invoke_result_t<FN, size_t, const T&>, ConvertTo>) || (std::is_invocable_v<FN, size_t, const T&> && std::convertible_to<std::invoke_result_t<FN, size_t, const T&>, result_array>)
            )
        {
            result_array res;
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            res.reserve_back(end_pos - start_pos);
            if constexpr (std::is_invocable_r_v<ConvertTo, FN, size_t, const T&>) {
                size_t pos = start_pos;
                for (auto& i : range(start_pos, end_pos))
                    res.push_back(iterate_fn(pos++, i));
            } else
                for (auto& i : range(start_pos, end_pos))
                    res.push_back(iterate_fn(i));
            return res;
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class FN>
            [[nodiscard]] constexpr result_array convert(FN&& iterate_fn) && requires((std::is_invocable_v<FN, T&&> && std::convertible_to<std::invoke_result_t<FN, T&&>, ConvertTo>) || (std::is_invocable_v<FN, T&&> && std::convertible_to<std::invoke_result_t<FN, T&&>, result_array>) || (std::is_invocable_v<FN, size_t, T&&> && std::convertible_to<std::invoke_result_t<FN, size_t, T&&>, ConvertTo>) || (std::is_invocable_v<FN, size_t, T&&> && std::convertible_to<std::invoke_result_t<FN, size_t, T&&>, result_array>)) {
                return convert_take<ConvertTo, result_array>(0, _size(), std::forward<FN>(iterate_fn));
            }


            template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class FN>
            [[nodiscard]] constexpr result_array convert(size_t start_pos, FN&& iterate_fn) && requires((std::is_invocable_v<FN, T&&> && std::convertible_to<std::invoke_result_t<FN, T&&>, ConvertTo>) || (std::is_invocable_v<FN, T&&> && std::convertible_to<std::invoke_result_t<FN, T&&>, result_array>) || (std::is_invocable_v<FN, size_t, T&&> && std::convertible_to<std::invoke_result_t<FN, size_t, T&&>, ConvertTo>) || (std::is_invocable_v<FN, size_t, T&&> && std::convertible_to<std::invoke_result_t<FN, size_t, T&&>, result_array>)) {
                return convert_take<ConvertTo, result_array>(start_pos, _size(), std::forward<FN>(iterate_fn));
            }


            template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class FN>
            [[nodiscard]] constexpr result_array convert(size_t start_pos, size_t end_pos, FN&& iterate_fn) && requires((std::is_invocable_v<FN, T&&> && std::convertible_to<std::invoke_result_t<FN, T&&>, ConvertTo>) || (std::is_invocable_v<FN, T&&> && std::convertible_to<std::invoke_result_t<FN, T&&>, result_array>) || (std::is_invocable_v<FN, size_t, T&&> && std::convertible_to<std::invoke_result_t<FN, size_t, T&&>, ConvertTo>) || (std::is_invocable_v<FN, size_t, T&&> && std::convertible_to<std::invoke_result_t<FN, size_t, T&&>, result_array>)) {
                return convert_take<ConvertTo, result_array>(start_pos, end_pos, std::forward<FN>(iterate_fn));
            }


            template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class FN>
            [[nodiscard]] constexpr result_array convert_take(FN&& iterate_fn)
                requires((std::is_invocable_v<FN, T &&> && std::convertible_to<std::invoke_result_t<FN, T &&>, ConvertTo>) || (std::is_invocable_v<FN, T &&> && std::convertible_to<std::invoke_result_t<FN, T &&>, result_array>) || (std::is_invocable_v<FN, size_t, T &&> && std::convertible_to<std::invoke_result_t<FN, size_t, T &&>, ConvertTo>) || (std::is_invocable_v<FN, size_t, T &&> && std::convertible_to<std::invoke_result_t<FN, size_t, T &&>, result_array>))
        {
            return convert_take<ConvertTo, result_array>(0, _size(), std::forward<FN>(iterate_fn));
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class FN>
        [[nodiscard]] constexpr result_array convert_take(size_t start_pos, FN&& iterate_fn)
            requires(
                (std::is_invocable_v<FN, T &&> && std::convertible_to<std::invoke_result_t<FN, T &&>, ConvertTo>) || (std::is_invocable_v<FN, T &&> && std::convertible_to<std::invoke_result_t<FN, T &&>, result_array>) || (std::is_invocable_v<FN, size_t, T &&> && std::convertible_to<std::invoke_result_t<FN, size_t, T &&>, ConvertTo>) || (std::is_invocable_v<FN, size_t, T &&> && std::convertible_to<std::invoke_result_t<FN, size_t, T &&>, result_array>)
            )
        {
            return convert_take<ConvertTo, result_array>(start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class FN>
        [[nodiscard]] constexpr result_array convert_take(size_t start_pos, size_t end_pos, FN&& iterate_fn)
            requires(
                (std::is_invocable_v<FN, T &&> && std::convertible_to<std::invoke_result_t<FN, T &&>, ConvertTo>) || (std::is_invocable_v<FN, T &&> && std::convertible_to<std::invoke_result_t<FN, T &&>, result_array>) || (std::is_invocable_v<FN, size_t, T &&> && std::convertible_to<std::invoke_result_t<FN, size_t, T &&>, ConvertTo>) || (std::is_invocable_v<FN, size_t, T &&> && std::convertible_to<std::invoke_result_t<FN, size_t, T &&>, result_array>)
            )
        {
            list_array<T, Allocator> tmp = take(start_pos, end_pos);
            result_array res;
            res.reserve(tmp.size());
            if constexpr (std::is_invocable_r_v<ConvertTo, FN, size_t, T&&>) {
                size_t pos = start_pos;
                for (auto& i : tmp.range(start_pos, end_pos))
                    res.push_back(iterate_fn(pos++, std::move(i)));
            } else
                for (auto& i : tmp.range(start_pos, end_pos))
                    res.push_back(iterate_fn(std::move(i)));
            return res;
        }

#pragma endregion
#pragma region convert_fn

        template <class FN>
        [[nodiscard]] constexpr list_array<std::invoke_result_t<FN, const T&>, std::allocator<std::invoke_result_t<FN, const T&>>> convert_fn(FN&& iterate_fn) const&
            requires std::is_invocable_v<FN, const T&>
        {
            return convert_fn(0, _size(), std::forward<FN>(iterate_fn));
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<std::invoke_result_t<FN, const T&>, std::allocator<std::invoke_result_t<FN, const T&>>> convert_fn(size_t start_pos, FN&& iterate_fn) const&
            requires std::is_invocable_v<FN, const T&>
        {
            return convert_fn(start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<std::invoke_result_t<FN, const T&>, std::allocator<std::invoke_result_t<FN, const T&>>> convert_fn(size_t start_pos, size_t end_pos, FN&& iterate_fn) const&
            requires std::is_invocable_v<FN, const T&>
        {
            list_array<std::invoke_result_t<FN, const T&>, std::allocator<std::invoke_result_t<FN, const T&>>> res;
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            res.reserve_back(end_pos - start_pos);
            for (auto& i : range(start_pos, end_pos))
                res.push_back(iterate_fn(i));
            return res;
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<std::invoke_result_t<FN, T&&>, std::allocator<std::invoke_result_t<FN, T&&>>> convert_fn(FN&& iterate_fn) &&
            requires std::is_invocable_v<FN, T&&>
        {
            return std::move(*this).convert_fn(0, _size(), std::forward<FN>(iterate_fn));
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<std::invoke_result_t<FN, T&&>, std::allocator<std::invoke_result_t<FN, T&&>>> convert_fn(size_t start_pos, FN&& iterate_fn) &&
            requires std::is_invocable_v<FN, T&&>
        {
            return std::move(*this).convert_fn(start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<std::invoke_result_t<FN, T&&>, std::allocator<std::invoke_result_t<FN, T&&>>> convert_fn(size_t start_pos, size_t end_pos, FN&& iterate_fn) &&
            requires std::is_invocable_v<FN, T&&>
        {
            list_array<std::invoke_result_t<FN, T&&>, std::allocator<std::invoke_result_t<FN, T&&>>> res;
            if (end_pos > _size())
                throw std::out_of_range("end_pos out of size limit");
            res.reserve_back(end_pos - start_pos);
            for (auto& i : range(start_pos, end_pos))
                res.push_back(iterate_fn(std::move(i)));
            return res;
        }

#pragma endregion
#pragma region erase

        constexpr list_array<T, Allocator>& erase(size_t where) & {
            if (where == 0)
                pop_front();
            else if (where == _size() - 1)
                pop_back();
            else
                erase_range(get_iterator(where), get_iterator(where + 1));
            return *this;
        }

        constexpr list_array<T, Allocator>& erase(size_t begin, size_t end) & {
            erase_range(get_iterator(begin), get_iterator(end));
            return *this;
        }

        [[nodiscard]] constexpr list_array<T, Allocator> erase(size_t where) && {
            erase(where);
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> erase(size_t begin, size_t end) && {
            erase(begin, end);
            return std::move(*this);
        }

#pragma endregion
#pragma region starts/ends with

        template <size_t condition_size>
        [[nodiscard]] constexpr bool starts_with(const T (&condition)[condition_size], size_t start_pos = 0) const&
            requires is_equality_comparable<T>
        {
            return starts_with(condition, condition_size, start_pos);
        }

        [[nodiscard]] constexpr bool starts_with(const T* condition, size_t condition_size, size_t start_pos = 0) const&
            requires is_equality_comparable<T>
        {
            if (start_pos >= _size())
                return false;
            if (condition_size > _size() - start_pos)
                return false;
            for (size_t i = 0; i < condition_size; i++)
                if (!(operator[](start_pos + i) == condition[i]))
                    return false;
            return true;
        }

        [[nodiscard]] constexpr bool starts_with(const T& condition, size_t start_pos = 0) const&
            requires is_equality_comparable<T>
        {
            if (start_pos >= _size())
                return false;
            return operator[](start_pos) == condition;
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr bool starts_with(const list_array<T, AnyAllocator>& condition, size_t start_pos = 0) const&
            requires is_equality_comparable<T>
        {
            if (start_pos >= _size())
                return false;
            if (condition.size() > _size() - start_pos)
                return false;
            for (size_t i = 0; i < condition.size(); i++)
                if (!(operator[](start_pos + i) == condition[i]))
                    return false;
            return true;
        }

        template <size_t condition_size>
        [[nodiscard]] constexpr bool ends_with(const T (&condition)[condition_size]) const&
            requires is_equality_comparable<T>
        {
            return ends_with<condition_size>(condition, condition_size, _size());
        }

        template <size_t condition_size>
        [[nodiscard]] constexpr bool ends_with(const T (&condition)[condition_size], size_t end_pos) const&
            requires is_equality_comparable<T>
        {
            return ends_with<condition_size>(condition, condition_size, end_pos);
        }

        [[nodiscard]] constexpr bool ends_with(const T* condition, size_t condition_size) const&
            requires is_equality_comparable<T>
        {
            return ends_with(condition, condition_size, _size());
        }

        [[nodiscard]] constexpr bool ends_with(const T* condition, size_t condition_size, size_t end_pos) const&
            requires is_equality_comparable<T>
        {
            if (end_pos >= condition_size)
                return false;
            if (condition_size > end_pos)
                return false;
            for (size_t i = 0; i < condition_size; i++)
                if (!(operator[](end_pos - i - 1) == condition[condition_size - i - 1]))
                    return false;
            return true;
        }

        [[nodiscard]] constexpr bool ends_with(const T& condition) const&
            requires is_equality_comparable<T>
        {
            return ends_with(condition, _size());
        }

        [[nodiscard]] constexpr bool ends_with(const T& condition, size_t end_pos) const&
            requires is_equality_comparable<T>
        {
            if (end_pos >= _size())
                return false;
            return operator[](end_pos - 1) == condition;
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr bool ends_with(const list_array<T, AnyAllocator>& condition) const&
            requires is_equality_comparable<T>
        {
            return ends_with(condition, _size());
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr bool ends_with(const list_array<T, AnyAllocator>& condition, size_t end_pos) const&
            requires is_equality_comparable<T>
        {
            if (end_pos >= _size())
                return false;
            if (condition.size() > end_pos)
                return false;
            for (size_t i = 0; i < condition.size(); i++)
                if (!(operator[](end_pos - i - 1) == condition[condition.size() - i - 1]))
                    return false;
            return true;
        }

#pragma endregion
#pragma region replace

        constexpr list_array<T, Allocator>& replace(const T& target, const T& with) &
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return for_each([target, with](T& it) {
                if (it == target)
                    it = with;
            });
        }

        [[nodiscard]] constexpr list_array<T, Allocator> replace(const T& target, const T& with) &&
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, with).take();
        }

        constexpr list_array<T, Allocator>& replace(const T& target, const T* with, size_t with_size) &
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            auto cache_iter = end();
            for (ptrdiff_t i = size(); i > 0;) {
                --cache_iter;
                --i;
                if (*cache_iter == target) {
                    erase(i);
                    insert(i, with, with_size);
                    cache_iter = get_iterator(i);
                }
            }
            return *this;
        }

        [[nodiscard]] constexpr list_array<T, Allocator> replace(const T& target, const T* with, size_t with_size) &&
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, with, with_size).take();
        }

        constexpr list_array<T, Allocator>& replace(const T* target, size_t target_size, const T& with) &
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            auto cache_iter = end();
            size_t t = 0;
            for (ptrdiff_t i = size(); i > 0;) {
                --cache_iter;
                --i;
                if (!(*cache_iter == target[t]))
                    t = 0;
                else if (t == target_size) {
                    erase(i, target_size);
                    insert(i, with);
                    cache_iter = get_iterator(i);
                }
            }
            return *this;
        }

        [[nodiscard]] constexpr list_array<T, Allocator> replace(const T* target, size_t target_size, const T& with) &&
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, target_size, with).take();
        }

        constexpr list_array<T, Allocator>& replace(const T* target, size_t target_size, const T* with, size_t with_size) &
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            auto cache_iter = end();
            size_t t = 0;
            for (ptrdiff_t i = size(); i > 0;) {
                --cache_iter;
                --i;
                if (!(*cache_iter = target[t]))
                    t = 0;
                else if (t == target_size) {
                    erase(i, target_size);
                    insert(i, with, with_size);
                    cache_iter = get_iterator(i);
                }
            }
            return *this;
        }

        [[nodiscard]] constexpr list_array<T, Allocator> replace(const T* target, size_t target_size, const T* with, size_t with_size) &&
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, target_size, with, with_size).take();
        }

        template <class AnyAllocator>
        constexpr list_array<T, Allocator>& replace(const T& target, const list_array<T, AnyAllocator>& with) &
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            auto cache_iter = end();
            for (ptrdiff_t i = size(); i > 0;) {
                --cache_iter;
                --i;
                if (*cache_iter == target) {
                    erase(i);
                    insert(i, with);
                    cache_iter = get_iterator(i);
                }
            }
            return *this;
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr list_array<T, Allocator> replace(const T& target, const list_array<T, AnyAllocator>& with) &&
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, with).take();
        }

        template <class AnyAllocator>
        constexpr list_array<T, Allocator>& replace(const list_array<T, AnyAllocator>& target, const T& with) &
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            auto cache_iter = end();
            size_t t = 0, target_size = target.size();

            for (ptrdiff_t i = size(); i > 0;) {
                --cache_iter;
                --i;
                if (!(*cache_iter == target[t]))
                    t = 0;
                else if (t == target_size) {
                    erase(i, target_size);
                    insert(i, with);
                    cache_iter = get_iterator(i);
                }
            }
            return *this;
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr list_array<T, Allocator> replace(const list_array<T, AnyAllocator>& target, const T& with) &&
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, with).take();
        }

        template <class AnyAllocator0, class AnyAllocator1>
        constexpr list_array<T, Allocator>& replace(const list_array<T, AnyAllocator0>& target, const list_array<T, AnyAllocator1>& with) &
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            auto cache_iter = end();
            size_t t = 0, target_size = target.size();
            for (ptrdiff_t i = size(); i > 0;) {
                --cache_iter;
                --i;
                if (!(*cache_iter == target[t]))
                    t = 0;
                else if (t == target_size) {
                    erase(i, target_size);
                    insert(i, with);
                    cache_iter = get_iterator(i);
                }
            }
            return *this;
        }

        template <class AnyAllocator0, class AnyAllocator1>
        [[nodiscard]] constexpr list_array<T, Allocator> replace(const list_array<T, AnyAllocator0>& target, const list_array<T, AnyAllocator1>& with) &&
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, with).take();
        }

        template <size_t with_size>
        constexpr list_array<T, Allocator>& replace(const T& target, const T (&with)[with_size]) &
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, with, with_size);
        }

        template <size_t with_size>
        [[nodiscard]] constexpr list_array<T, Allocator> replace(const T& target, const T (&with)[with_size]) &&
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, with, with_size).take();
        }

        template <size_t target_size>
        constexpr list_array<T, Allocator>& replace(const T (&target)[target_size], const T& with) &
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, target_size, with);
        }

        template <size_t target_size>
        [[nodiscard]] constexpr list_array<T, Allocator> replace(const T (&target)[target_size], const T& with) &&
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, target_size, with).take();
        }

        template <size_t target_size, size_t with_size>
        constexpr list_array<T, Allocator>& replace(const T (&target)[target_size], const T (&with)[with_size]) &
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, target_size, with, with_size);
        }

        template <size_t target_size, size_t with_size>
        [[nodiscard]] constexpr list_array<T, Allocator> replace(const T (&target)[target_size], const T (&with)[with_size]) &&
            requires is_equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, target_size, with, with_size).take();
        }

        template <class FN>
        constexpr list_array<T, Allocator>& replace_if(const T& with, FN&& selector) &
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, T&> || std::is_invocable_r_v<bool, FN, size_t, T&>)
        {
            if constexpr (std::is_invocable_r_v<bool, FN, size_t, T&>)
                return for_each([&selector, &with](size_t i, T& it) {
                    if (selector(i, it))
                        it = with;
                });
            else
                return for_each([&selector, &with](T& it) {
                    if (selector(it))
                        it = with;
                });
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> replace_if(const T& with, FN&& selector) &&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, T&> || std::is_invocable_r_v<bool, FN, size_t, T&>)
        {
            return replace_if(with, std::forward<FN>(selector)).take();
        }

        template <class FN>
        constexpr list_array<T, Allocator>& replace_if(const T* with, size_t with_size, FN&& selector) &
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, T&> || std::is_invocable_r_v<bool, FN, size_t, T&>)
        {
            auto cache_iter = end();
            for (ptrdiff_t i = size(); i > 0;) {
                --cache_iter;
                --i;
                if constexpr (std::is_invocable_r_v<bool, FN, size_t, T&>) {
                    if (selector(cache_iter.absolute_index, *cache_iter)) {
                        erase(i);
                        insert(i, with, with_size);
                        cache_iter = get_iterator(i);
                    }
                } else {
                    if (selector(*cache_iter)) {
                        erase(i);
                        insert(i, with, with_size);
                        cache_iter = get_iterator(i);
                    }
                }
            }
            return *this;
        }

        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> replace_if(const T* with, size_t with_size, FN&& selector) &&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, T&> || std::is_invocable_r_v<bool, FN, size_t, T&>)
        {
            return replace_if(with, with_size, std::forward<FN>(selector)).take();
        }

        template <class AnyAllocator, class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> replace_if(const list_array<T, AnyAllocator>& with, FN&& selector) &
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, T&> || std::is_invocable_r_v<bool, FN, size_t, T&>)
        {
            auto cache_iter = end();
            for (ptrdiff_t i = size(); i > 0;) {
                --cache_iter;
                --i;
                if constexpr (std::is_invocable_r_v<bool, FN, size_t, T&>) {
                    if (selector(cache_iter.absolute_index, *cache_iter)) {
                        erase(i);
                        insert(i, with);
                        cache_iter = get_iterator(i);
                    }
                } else {
                    if (selector(*cache_iter)) {
                        erase(i);
                        insert(i, with);
                        cache_iter = get_iterator(i);
                    }
                }
            }
            return *this;
        }

        template <class AnyAllocator, class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> replace_if(const list_array<T, AnyAllocator>& with, FN&& selector) &&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, T&> || std::is_invocable_r_v<bool, FN, size_t, T&>)
        {
            return replace_if(with, std::forward<FN>(selector)).take();
        }

        template <size_t with_size, class FN>
        constexpr list_array<T, Allocator>& replace_if(const T (&with)[with_size], FN&& selector) &
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, T&> || std::is_invocable_r_v<bool, FN, size_t, T&>)
        {
            return replace_if(with, with_size, std::forward<FN>(selector));
        }

        template <size_t with_size, class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> replace_if(const T (&with)[with_size], FN&& selector) &&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, T&> || std::is_invocable_r_v<bool, FN, size_t, T&>)
        {
            return replace_if(with, with_size, std::forward<FN>(selector)).take();
        }

#pragma endregion
#pragma region index and iterators

        [[nodiscard]] constexpr iterator begin() & noexcept {
            return get_iterator(0);
        }

        [[nodiscard]] constexpr iterator end() & noexcept {
            return get_iterator(_size());
        }

        [[nodiscard]] constexpr const_iterator begin() const& noexcept {
            return get_iterator(0);
        }

        [[nodiscard]] constexpr const_iterator end() const& noexcept {
            return get_iterator(_size());
        }

        [[nodiscard]] constexpr reverse_iterator rbegin() & {
            return get_iterator(_size());
        }

        [[nodiscard]] constexpr reverse_iterator rend() & {
            return get_iterator(0);
        }

        [[nodiscard]] constexpr const_reverse_iterator rbegin() const& {
            return get_iterator(_size());
        }

        [[nodiscard]] constexpr const_reverse_iterator rend() const& {
            return get_iterator(0);
        }

        [[nodiscard]] constexpr const_iterator cbegin() const& {
            return get_iterator(0);
        }

        [[nodiscard]] constexpr const_iterator cend() const& {
            return get_iterator(_size());
        }

        [[nodiscard]] constexpr const_reverse_iterator crbegin() const& {
            return get_iterator(_size());
        }

        [[nodiscard]] constexpr const_reverse_iterator crend() const& {
            return get_iterator(0);
        }

        [[nodiscard]] constexpr T& operator[](size_t index) & noexcept {
            return *get_element_at_index(index);
        }

        [[nodiscard]] constexpr const T& operator[](size_t index) const& noexcept {
            return *get_element_at_index(index);
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr bool operator==(const list_array<T, AnyAllocator>& other) const
            requires is_equality_comparable<T>
        {
            if (size() != other.size())
                return false;

            auto _begin = begin();
            auto _end = end();
            auto _other_begin = other.begin();
            auto _other_end = other.end();

            while (_begin != _end && _other_begin != _other_end) {
                if (!(*_begin == *_other_begin))
                    return false;

                ++_begin;
                ++_other_begin;
            }
            return true;
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr bool operator!=(const list_array<T, AnyAllocator>& other) const
            requires is_equality_comparable<T>
        {
            return !operator==(other);
        }

        template <class AnyAllocator>
        [[nodiscard]] constexpr std::strong_ordering operator<=>(const list_array<T, AnyAllocator>& other) const
            requires is_equality_comparable<T>
        {
            return operator==(other) ? std::strong_ordering::equal : std::strong_ordering::less;
        }

        template <class AnyAllocator, class FN>
        [[nodiscard]] constexpr bool equal(const list_array<T, AnyAllocator>& other, FN&& fn) const {
            if (size() != other.size())
                return false;

            auto _begin = begin();
            auto _end = end();
            auto _other_begin = other.begin();
            auto _other_end = other.end();

            while (_begin != _end && _other_begin != _other_end) {
                if (!fn(*_begin, *_other_begin))
                    return false;

                ++_begin;
                ++_other_begin;
            }
            return true;
        }

        template <class AnyAllocator, class FN>
        [[nodiscard]] constexpr bool not_equal(const list_array<T, AnyAllocator>& other, FN&& fn) const {
            return !equal(other, std::move(fn));
        }

        [[nodiscard]] constexpr T& at(size_t index) & {
            if (index >= _size())
                throw std::out_of_range("Index out of range");
            return *get_element_at_index(index);
        }

        [[nodiscard]] constexpr const T& at(size_t index) const& {
            if (index >= _size())
                throw std::out_of_range("Index out of range");
            return *get_element_at_index(index);
        }

        [[nodiscard]] constexpr T at_default(size_t index) const& noexcept(std::is_nothrow_default_constructible_v<T> && std::is_nothrow_copy_constructible_v<T>)
            requires std::is_copy_constructible_v<T> && std::default_initializable<T>
        {
            if (index >= _size())
                return T{};
            return *get_element_at_index(index);
        }

        [[nodiscard]] constexpr iterator get_iterator(size_t index) & noexcept {
            return get_iterator_at_index(index);
        }

        [[nodiscard]] constexpr const_iterator get_iterator(size_t index) const& noexcept {
            return (const_iterator) const_cast<list_array<T, Allocator>*>(this)->get_iterator(index);
        }

        [[nodiscard]] constexpr range_provider range(size_t start, size_t end) & {
            if (start > end)
                throw std::out_of_range("start > end");
            if (end > _size())
                throw std::out_of_range("end out of size limit");
            return range_provider(*this, start, end);
        }

        [[nodiscard]] constexpr reverse_provider reverse_range(size_t start, size_t end) & {
            if (start > end)
                throw std::out_of_range("start > end");
            if (end > _size())
                throw std::out_of_range("end out of size limit");
            return range_provider(*this, start, end);
        }

        [[nodiscard]] constexpr const_range_provider range(size_t start, size_t end) const& {
            if (start > end)
                throw std::out_of_range("start > end");
            if (end > _size())
                throw std::out_of_range("end out of size limit");

            return const_range_provider(*this, start, end);
        }

        [[nodiscard]] constexpr const_reverse_provider reverse_range(size_t start, size_t end) const& {
            if (start > end)
                throw std::out_of_range("start > end");
            if (end > _size())
                throw std::out_of_range("end out of size limit");
            return const_range_provider(*this, start, end);
        }

        [[nodiscard]] constexpr reverse_provider reverse() & {
            return *this;
        }

        [[nodiscard]] constexpr list_array<T, Allocator> reverse() && {
            list_array<T, Allocator> result(get_allocator());
            result.reserve(_size());
            for (auto& it : reverse())
                result.push_back(std::move(it));
            return result;
        }

        [[nodiscard]] constexpr const_reverse_provider reverse() const& {
            return *this;
        }

#pragma endregion
#pragma region size, capacity, reservation

        constexpr void clear() & {
            if (!first_block)
                return;
            for (auto& value : *this)
                std::destroy_at(&value);
            _size() = 0;
            _reserved_front = 0;
            _reserved_back = 0;
            while (first_block != last_block) {
                auto next = first_block->next;
                arr_block<T>::destroy_block(first_block, allocator_and_size.get_allocator());
                first_block = next;
            }
            arr_block<T>::destroy_block(first_block, allocator_and_size.get_allocator());
            first_block = nullptr;
            last_block = nullptr;
        }

        [[nodiscard]] constexpr size_t size() const noexcept {
            return _size();
        }

        [[nodiscard]] constexpr bool empty() const noexcept {
            return _size() == 0;
        }

        [[nodiscard]] constexpr inline size_t capacity() const noexcept {
            return allocator_and_size.hold_value + _reserved_front + _reserved_back;
        }

        [[nodiscard]] constexpr inline size_t reserved() const noexcept {
            return _reserved_front + _reserved_back;
        }

        [[nodiscard]] constexpr inline size_t reserved_front() const noexcept {
            return _reserved_front;
        }

        [[nodiscard]] constexpr inline size_t reserved_back() const noexcept {
            return _reserved_back;
        }

        constexpr list_array<T, Allocator>& reserve_front(size_t size) & {
            if (!size)
                return *this;
            _reserved_front += size;
            if (first_block) {
                auto new_block = arr_block<T>::create_block(allocator_and_size.get_allocator(), size);
                new_block->next = first_block;
                first_block->prev = new_block;
                first_block = new_block;
            } else
                last_block = first_block = arr_block<T>::create_block(allocator_and_size.get_allocator(), size);
            return *this;
        }

        constexpr list_array<T, Allocator>& reserve_back(size_t size) & {
            if (!size)
                return *this;
            _reserved_back += size;
            if (last_block) {
                auto new_block = arr_block<T>::create_block(allocator_and_size.get_allocator(), size);
                new_block->prev = last_block;
                last_block->next = new_block;
                last_block = new_block;
            } else
                first_block = last_block = arr_block<T>::create_block(allocator_and_size.get_allocator(), size);
            return *this;
        }

        constexpr list_array<T, Allocator>& prepare_front(size_t size, bool steal = true) & {
            if (_reserved_front < size) {
                if (steal)
                    if (_reserved_back)
                        if (last_block->data_size <= _reserved_back) {
                            steal_reserve_block_back_to_front();
                            prepare_back(size - last_block->data_size);
                            return;
                        }
                reserve_front(size - _reserved_front);
            }
            return *this;
        }

        constexpr list_array<T, Allocator>& prepare_back(size_t size, bool steal = true) & {
            if (_reserved_back < size) {
                if (steal)
                    if (_reserved_front)
                        if (first_block->data_size <= _reserved_front) {
                            steal_reserve_block_front_to_back();
                            prepare_back(size - first_block->data_size);
                            return *this;
                        }
                reserve_back(size - _reserved_back);
            }
            return *this;
        }

        constexpr list_array<T, Allocator>& reserve(size_t size) & {
            reserve_back(size);
            return *this;
        }

#pragma endregion
#pragma region resize

        constexpr list_array<T, Allocator>& resize(size_t new_size) &
            requires std::default_initializable<T>
        {
            if (new_size < _size()) {
                auto iter = get_iterator_at_index(new_size);
                erase_range(iter, end());
            } else if (new_size > _size()) {
                size_t elements_to_add = new_size - _size();
                if (_reserved_back < elements_to_add)
                    prepare_back(std::max(increase_policy(), elements_to_add));
                std::uninitialized_default_construct_n(get_element_at_index(_size()), elements_to_add);
                _size() += elements_to_add;
                _reserved_back -= elements_to_add;
            }
            return *this;
        }

        constexpr list_array<T, Allocator>& resize(size_t new_size, const T& set) &
            requires std::is_copy_constructible_v<T>
        {
            if (new_size < _size()) {
                auto iter = get_iterator_at_index(new_size);
                erase_range(iter, end());
            } else if (new_size > _size()) {
                size_t elements_to_add = new_size - _size();
                if (_reserved_back < elements_to_add)
                    prepare_back(std::max(increase_policy(), elements_to_add));
                std::uninitialized_fill_n(get_element_at_index(_size()), elements_to_add, set);
                _size() += elements_to_add;
                _reserved_back -= elements_to_add;
            }
            return *this;
        }

#pragma endregion
#pragma region memory

        constexpr list_array<T, Allocator>& shrink_front() & {
            auto current = first_block;
            auto& allocator = allocator_and_size.get_allocator();
            while (current->next && _reserved_front) {
                auto next = current->next;
                if (current->data_size > _reserved_front) {
                    arr_block_manager<T, Allocator> manager(allocator);
                    auto iter = begin();
                    manager.add_block(manager.allocate_and_take_from(current->data_size - _reserved_front, iter));
                    current->next = nullptr;
                    arr_block<T>::destroy_block(current, allocator);
                    first_block = manager.get_first();
                    first_block->next = next;
                    next->prev = first_block;
                    _reserved_front = 0;
                    manager.release();
                    break;
                }
                _reserved_front -= current->data_size;
                current->next = nullptr;
                next->prev = nullptr;
                first_block = next;
                arr_block<T>::destroy_block(current, allocator);
                current = next;
            }
            return *this;
        }

        constexpr list_array<T, Allocator>& shrink_back() & {
            auto current = last_block;
            auto& allocator = allocator_and_size.get_allocator();
            while (current->prev && _reserved_back) {
                auto prev = current->prev;
                if (current->data_size > _reserved_back) {
                    arr_block_manager<T, Allocator> manager(allocator);
                    auto iter = end() - (current->data_size) + 1;
                    manager.add_block(manager.allocate_and_take_from(current->data_size - _reserved_back, iter));
                    current->prev = nullptr;
                    arr_block<T>::destroy_block(current, allocator);
                    last_block = manager.get_last();
                    last_block->prev = prev;
                    prev->next = last_block;
                    _reserved_back = 0;
                    manager.release();
                    break;
                }
                _reserved_back -= current->data_size;
                current->prev = nullptr;
                prev->next = nullptr;
                last_block = prev;
                arr_block<T>::destroy_block(current, allocator);
                current = prev;
            }
            return *this;
        }

        constexpr list_array<T, Allocator>& shrink_to_fit() & {
            if (_reserved_front)
                shrink_front();
            if (_reserved_back)
                shrink_back();
            return *this;
        }

        constexpr list_array<T, Allocator>& commit() & {
            if (!blocks_more(1))
                return *this;
            arr_block_manager<T, Allocator> manager(allocator_and_size.get_allocator());
            auto iter = begin();
            manager.add_block(
                manager.allocate_and_take_from(_size(), iter)
            );
            clear();
            first_block = last_block = manager.get_first();
            _reserved_back = _reserved_front = 0;
            _size() = last_block->data_size;
            manager.release();
            return *this;
        }

        constexpr list_array<T, Allocator>& decommit(size_t total_blocks) & {
            if (total_blocks == 0 || _size() == 0)
                return *this;
            else if (total_blocks > _size())
                total_blocks = _size();

            size_t blocks_size = _size() / total_blocks;
            size_t add_last = _size() % total_blocks;

            arr_block_manager<T, Allocator> manager(allocator_and_size.get_allocator());
            auto iter = begin();

            for (size_t i = 0; i < total_blocks; i++) {
                size_t current_block_size = blocks_size + (i == total_blocks - 1 ? add_last : 0);
                manager.add_block(
                    manager.allocate_and_take_from(current_block_size, iter)
                );
            }

            clear();
            first_block = manager.get_first();
            last_block = manager.get_last();
            _size() = blocks_size * total_blocks + add_last;
            manager.release();
            return *this;
        }

        [[nodiscard]] constexpr bool need_commit() const noexcept {
            return blocks_more(2);
        }

        [[nodiscard]] constexpr bool blocks_more(size_t blocks_count) const noexcept {
            if (!first_block)
                return false;
            auto current = first_block;
            size_t blocks = 1;
            while (current->next) {
                if (blocks++ == blocks_count)
                    return true;
                current = current->next;
            }
            return false;
        }

        [[nodiscard]] constexpr size_t blocks_count() const noexcept {
            if (!first_block)
                return 0;
            auto current = first_block;
            size_t blocks = 1;
            while (current->next) {
                blocks++;
                current = current->next;
            }
            return blocks;
        }

#pragma endregion
#pragma region view

        [[nodiscard]] constexpr T* data() & {
            if (blocks_more(1))
                commit();
            if (first_block)
                return first_block->data + _reserved_front;
            else
                return nullptr;
        }

        [[nodiscard]] constexpr const T* data() const& {
            if (blocks_more(1))
                throw std::runtime_error("can't get const raw pointer when blocks more than 1");
            if (first_block)
                return first_block->data + _reserved_front;
            else
                return nullptr;
        }

        [[nodiscard]] constexpr T sum() const
            requires std::is_arithmetic_v<T>
        {
            T i = 0;
            for (T val : *this)
                i += val;
            return i;
        }

        template <class FN>
        [[nodiscard]] constexpr size_t sum(FN&& converter) const
            requires std::is_arithmetic_v<std::invoke_result_t<FN, const T&>>
        {
            size_t i = 0;
            for (const T& val : *this)
                i += converter(val);
            return i;
        }

        [[nodiscard]] constexpr T avg() const
            requires std::is_arithmetic_v<T>
        {
            T i = 0;
            for (T val : *this)
                i += val;
            if (i == 0)
                return 0;
            return i / size();
        }

        template <class FN>
        [[nodiscard]] constexpr size_t avg(FN&& converter) const
            requires std::is_arithmetic_v<std::invoke_result_t<FN, const T&>>
        {
            size_t i = 0;
            for (const T& val : *this)
                i += converter(val);
            if (i == 0)
                return 0;
            return i / size();
        }

        [[nodiscard]] constexpr T max() &&
            requires std::totally_ordered<T>
        {
            if (!_size())
                throw std::length_error("This list_array size is zero");
            const T* max = &operator[](0);
            for (const T& it : *this)
                if (*max < it)
                    max = &it;
            return std::move(*max);
        }

        [[nodiscard]] constexpr T min() &&
            requires std::totally_ordered<T>
        {
            if (!_size())
                throw std::length_error("This list_array size is zero");
            const T* min = &operator[](0);
            for (const T& it : *this)
                if (*min > it)
                    min = &it;
            return std::move(*min);
        }

        [[nodiscard]] constexpr const T& max() const&
            requires std::totally_ordered<T>
        {
            if (!_size())
                throw std::length_error("This list_array size is zero");
            const T* max = &operator[](0);
            for (const T& it : *this)
                if (*max < it)
                    max = &it;
            return *max;
        }

        [[nodiscard]] constexpr const T& min() const&
            requires std::totally_ordered<T>
        {
            if (!_size())
                throw std::length_error("This list_array size is zero");
            const T* min = &operator[](0);
            for (const T& it : *this)
                if (*min > it)
                    min = &it;
            return *min;
        }

        template <class FN>
        [[nodiscard]] constexpr size_t max_index() const&
            requires std::totally_ordered<T>
        {
            if (!_size())
                return npos;
            const T* max = &operator[](0);
            size_t index = 0;
            for_each([&index, &max](size_t i, const T& it) {
                if (*max < it) {
                    max = &it;
                    index = i;
                }
            });
            return index;
        }

        template <class FN>
        [[nodiscard]] constexpr size_t max_index(FN&& fn) const&
            requires std::is_arithmetic_v<std::invoke_result_t<FN, const T&>>
        {
            std::invoke_result_t<FN, const T&> v_check{};
            size_t index = npos;
            for_each([&index, &v_check, fn](size_t i, const T& it) {
                if (index != npos) {
                    auto v = fn(it);
                    if (v_check < v) {
                        v_check = v;
                        index = i;
                    }
                } else {
                    v_check = fn(it);
                    index = i;
                }
            });
            return index;
        }

        template <class FN>
        [[nodiscard]] constexpr size_t min_index() const&
            requires std::totally_ordered<T>
        {
            if (!_size())
                return npos;
            const T* min = &operator[](0);
            size_t index = 0;
            for_each([&index, &min](size_t i, const T& it) {
                if (*min > it) {
                    min = &it;
                    index = i;
                }
            });
            return index;
        }

        template <class FN>
        [[nodiscard]] constexpr size_t min_index(FN&& fn) const&
            requires std::is_arithmetic_v<std::invoke_result_t<FN, const T&>>
        {
            std::invoke_result_t<FN, const T&> v_check{};
            size_t index = npos;
            for_each([&index, &v_check, fn](size_t i, const T& it) {
                if (index != npos) {
                    auto v = fn(it);
                    if (v_check > v) {
                        v_check = v;
                        index = i;
                    }
                } else {
                    v_check = fn(it);
                    index = i;
                }
            });
            return index;
        }

        [[nodiscard]] constexpr T max_default() const
            requires std::is_copy_constructible_v<T> && std::totally_ordered<T>
        {
            if (!_size())
                return T{};
            const T* max = &operator[](0);
            for (const T& it : *this)
                if (*max < it)
                    max = &it;
            return *max;
        }

        [[nodiscard]] constexpr T min_default() const
            requires std::is_copy_constructible_v<T> && std::totally_ordered<T>
        {
            if (!_size())
                return T{};
            const T* min = &operator[](0);
            for (const T& it : *this)
                if (*min > it)
                    min = &it;
            return *min;
        }

#pragma endregion
#pragma region to_...

        template <class LocalAllocator>
        [[nodiscard]] constexpr T* to_array(LocalAllocator alloc = std::allocator<T>()) const&
            requires std::is_copy_constructible_v<T>
        {
            T* tmp = alloc.allocate(_size());
            for_each([tmp](size_t index, const T& it) { std::construct_at(tmp + index, it); });
            return tmp;
        }

        template <class LocalAllocator>
        [[nodiscard]] constexpr T* to_array(LocalAllocator alloc = std::allocator<T>()) &&
            requires std::is_copy_constructible_v<T>
        {
            T* tmp = alloc.allocate(_size());
            for_each([tmp](size_t index, T&& it) { std::construct_at(tmp + index, std::move(it)); });
            return tmp;
        }

        template <class Container>
        [[nodiscard]] constexpr Container to_container() const&
            requires std::is_copy_constructible_v<T> && is_container_v<Container>
        {
            Container copy_container;
            copy_container.resize(size());
            for_each([&copy_container](size_t i, const T& it) {
                copy_container[i] = it;
            });
            return copy_container;
        }

        template <class Container>
        [[nodiscard]] constexpr Container to_container() &&
            requires is_container_v<Container>
        {
            Container move_container;
            move_container.resize(size());
            std::move(*this).for_each([&move_container](size_t i, T&& it) {
                move_container[i] = std::move(it);
            });
            return move_container;
        }

        template <template <class...> class Container>
        [[nodiscard]] constexpr Container<T, Allocator> to_container() const&
            requires std::is_copy_constructible_v<T> && is_container_v<Container<T, Allocator>>
        {
            Container<T, Allocator> copy_container;
            copy_container.resize(size());
            for_each([&copy_container](size_t i, const T& it) {
                copy_container[i] = it;
            });
            return copy_container;
        }

        template <template <class...> class Container>
        [[nodiscard]] constexpr Container<T, Allocator> to_container() &&
            requires is_container_v<Container<T, Allocator>>
        {
            Container<T, Allocator> move_container;
            move_container.resize(size());
            std::move(*this).for_each([&move_container](size_t i, T&& it) {
                move_container[i] = std::move(it);
            });
            return move_container;
        }

        template <class Container>
        [[nodiscard]] constexpr Container to_set() const&
            requires std::is_copy_constructible_v<T> && is_container_v<Container>
        {
            Container copy_container;
            copy_container.reserve(size());
            for_each([&copy_container](const T& it) {
                copy_container.emplace(it);
            });
            return copy_container;
        }

        template <class Container>
        [[nodiscard]] constexpr Container to_set() &&
            requires is_container_v<Container>
        {
            Container move_container;
            move_container.reserve(size());
            std::move(*this).for_each([&move_container](T&& it) {
                move_container.emplace(std::move(it));
            });
            return move_container;
        }

        template <template <class...> class Container>
        [[nodiscard]] constexpr Container<T, Allocator> to_set() const&
            requires std::is_copy_constructible_v<T> && is_container_v<Container<T, Allocator>>
        {
            Container<T, Allocator> copy_container;
            copy_container.reserve(size());
            for_each([&copy_container](const T& it) {
                copy_container.emplace(it);
            });
            return copy_container;
        }

        template <template <class...> class Container>
        [[nodiscard]] constexpr Container<T, Allocator> to_set() &&
            requires is_container_v<Container<T, Allocator>>
        {
            Container<T, Allocator> move_container;
            move_container.reserve(size());
            std::move(*this).for_each([&move_container](T&& it) {
                move_container.emplace(std::move(it));
            });
            return move_container;
        }

#pragma endregion
#pragma region flip

        constexpr list_array<T, Allocator>& flip() & {
            return *this = take().flip();
        }

        [[nodiscard]] constexpr list_array<T, Allocator> flip() && {
            list_array<T, Allocator> cache(get_allocator());
            cache.reserve(size());
            take().for_each_reverse([&cache](T&& item) {
                cache.push_back(std::move(item));
            });
            return cache;
        }

#pragma endregion
#pragma region ordered_insert

        constexpr list_array<T, Allocator>& ordered_insert(const T& value) &
            requires std::is_copy_constructible_v<T> && std::totally_ordered<T>
        {
            if (_size() == 0) {
                push_back(value);
                return *this;
            }
            insert(std::lower_bound(begin(), end(), value).absolute_index, value);
            return *this;
        }

        constexpr list_array<T, Allocator>& ordered_insert(T&& value) &
            requires std::totally_ordered<T>
        {
            if (_size() == 0) {
                push_back(std::move(value));
                return *this;
            }
            insert(std::lower_bound(begin(), end(), value).absolute_index, std::move(value));
            return *this;
        }

        [[nodiscard]] constexpr list_array<T, Allocator> ordered_insert(const T& value) &&
            requires std::is_copy_constructible_v<T> && std::totally_ordered<T>
        {
            if (_size() == 0) {
                push_back(value);
                return std::move(*this);
            }
            insert(std::lower_bound(begin(), end(), value).absolute_index, value);
            return std::move(*this);
        }

        [[nodiscard]] constexpr list_array<T, Allocator> ordered_insert(T&& value) &&
            requires std::totally_ordered<T>
        {
            if (_size() == 0) {
                push_back(std::move(value));
                return std::move(*this);
            }
            insert(std::lower_bound(begin(), end(), value).absolute_index, std::move(value));
            return std::move(*this);
        }

#pragma endregion
#pragma region count

        template <class FN>
        [[nodiscard]] constexpr size_t count(FN counter) const
            requires std::is_invocable_v<FN, const T&> && std::convertible_to<std::invoke_result_t<FN, const T&>, size_t>
        {
            size_t c = 0;
            for (auto& it : *this)
                c += (size_t)counter(it);
            return c;
        }

#pragma endregion
#pragma endregion

        [[nodiscard]] constexpr Allocator& get_allocator() & noexcept {
            return allocator_and_size.get_allocator();
        }

        [[nodiscard]] constexpr const Allocator& get_allocator() const& noexcept {
            return allocator_and_size.get_allocator();
        }
    };
}

template <class T, class Allocator = std::allocator<T>>
using list_array = _list_array_impl::list_array<T, Allocator>;

template <class Array, class Allocator = std::allocator<typename _list_array_impl::is_container<Array>::container_type>>
[[nodiscard]] constexpr list_array<typename _list_array_impl::is_container<Array>::container_type, Allocator> to_list_array(Array&& arr) {
    return list_array<typename _list_array_impl::is_container<Array>::container_type, Allocator>(std::move(arr));
}

template <class Array, class Allocator = std::allocator<typename _list_array_impl::is_container<Array>::container_type>>
[[nodiscard]] constexpr list_array<typename _list_array_impl::is_container<Array>::container_type, Allocator> to_list_array(const Array& arr) {
    return list_array<typename _list_array_impl::is_container<Array>::container_type, Allocator>(arr);
}

template <class T = uint8_t, class Allocator = std::allocator<T>>
struct bit_list_array {
    list_array<T, Allocator> arr;
    T begin_bit;
    T end_bit;

    static inline constexpr size_t max_bits = sizeof(T) * 8;

    class bit_refrence {
        T& byte;
        T bit;

    public:
        constexpr bit_refrence(T& byte, T bit)
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
        : arr(alloc), begin_bit(0), end_bit(0) {}

    constexpr bit_list_array(size_t size, const Allocator& alloc = Allocator())
        : arr(size / max_bits + (size % max_bits ? 1 : 0), alloc), begin_bit(0), end_bit(max_bits - size % max_bits) {
        std::memset(arr.data(), 0, arr.size());
    }

    constexpr bit_list_array(const bit_list_array& copy, const Allocator& alloc = Allocator())
        : arr(copy.arr, alloc), begin_bit(copy.begin_bit), end_bit(copy.end_bit) {}

    constexpr bit_list_array(bit_list_array&& move) noexcept {
        arr.swap(move.arr);
        begin_bit = move.begin_bit;
        end_bit = move.end_bit;
    }

    constexpr size_t size() const {
        return (arr.size() - 1) * max_bits + end_bit - begin_bit;
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

    constexpr bit_list_array& push_back(bool val) {
        if (end_bit == max_bits || arr.empty()) {
            arr.push_back(0);
            end_bit = 0;
        }
        arr[arr.size() - 1] |= T(val) << end_bit++;
        return *this;
    }

    constexpr bit_list_array& pop_back() {
        if (end_bit == 0 || arr.empty()) {
            arr.pop_back();
            end_bit = max_bits;
            return *this;
        }
        end_bit--;
        arr[arr.size() - 1] &= ~(T(1) << end_bit);
        return *this;
    }

    constexpr bit_list_array& push_front(bool val) {
        if (begin_bit == 0 || arr.empty()) {
            arr.push_front(0);
            begin_bit = max_bits;
        }
        arr[0] |= T(val) << --begin_bit;
        return *this;
    }

    constexpr bit_list_array& pop_front() {
        if (begin_bit == max_bits || arr.empty()) {
            arr.pop_front();
            begin_bit = 0;
            return *this;
        }
        arr[0] &= ~(T(1) << begin_bit++);
        return *this;
    }

    constexpr bool at(size_t pos) const {
        size_t byte = pos / max_bits;
        size_t bit = pos % max_bits;
        if (byte >= arr.size())
            throw std::out_of_range("pos out of size limit");
        if (byte == 0 && bit < begin_bit)
            throw std::out_of_range("pos out of size limit");
        if (byte == arr.size() - 1 && bit >= end_bit)
            throw std::out_of_range("pos out of size limit");
        return (arr[byte] >> bit) & 1;
    }

    constexpr bool set(size_t pos, bool val) {
        size_t byte = pos / max_bits;
        size_t bit = pos % max_bits;
        if (byte >= arr.size())
            throw std::out_of_range("pos out of size limit");
        if (byte == 0 && bit < begin_bit)
            throw std::out_of_range("pos out of size limit");
        if (byte == arr.size() - 1 && bit >= end_bit)
            throw std::out_of_range("pos out of size limit");
        bool res = (arr[byte] >> bit) & 1;
        if (val)
            arr[byte] |= T(1) << bit;
        else
            arr[byte] &= ~(T(1) << bit);
        return res;
    }

    constexpr bit_refrence operator[](size_t pos) {
        size_t byte = pos / max_bits;
        T bit = pos % max_bits;
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

    constexpr bit_list_array& resize(size_t size) {
        if (begin_bit)
            commit();
        arr.resize(size / max_bits + (size % max_bits ? 1 : 0));
        end_bit = max_bits - size % max_bits;
        return *this;
    }

    constexpr bit_list_array& reserve_back(size_t size) {
        arr.reserve_back(size / max_bits + (size % max_bits ? 1 : 0));
        return *this;
    }

    constexpr bit_list_array& reserve_front(size_t size) {
        arr.reserve_front(size / max_bits + (size % max_bits ? 1 : 0));
        return *this;
    }

    constexpr bool need_commit() const {
        return begin_bit ? true : arr.need_commit();
    }

    constexpr bit_list_array& commit() {

        if (begin_bit != 0) {
            bit_list_array tmp;
            size_t _size = size();
            tmp.reserve_back(_size);
            for (size_t i = 0; i < _size; i++)
                tmp.push_back(at(i));
            *this = std::move(tmp);
        } else {
            arr.commit();
        }
        return *this;
    }

    constexpr bit_list_array& swap(bit_list_array& to_swap) noexcept {
        if (this != &to_swap) {
            arr.swap(to_swap.arr);
            T tmp = begin_bit;
            begin_bit = to_swap.begin_bit;
            to_swap.begin_bit = tmp;
            tmp = end_bit;
            end_bit = to_swap.end_bit;
            to_swap.end_bit = tmp;
        }
        return *this;
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
            size_t to_zero_bytes = (this_size - min_size) / max_bits;
            size_t to_zero_bits = (this_size - min_size) % max_bits;
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
            size_t to_or_bytes = (to_or_size - min_size) / max_bits;
            size_t to_or_bits = (to_or_size - min_size) % max_bits;
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
            size_t to_xor_bytes = (to_xor_size - min_size) / max_bits;
            size_t to_xor_bits = (to_xor_size - min_size) % max_bits;
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

    constexpr const list_array<T, Allocator>& data() const {
        return arr;
    }

    constexpr list_array<T, Allocator>& data() {
        return arr;
    }

    constexpr list_array<T, Allocator> take() {
        return commit().arr.take();
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

    template <class T, class Allocator>
    struct hash<bit_list_array<T, Allocator>> {
        constexpr size_t operator()(const bit_list_array<T, Allocator>& list) {
            hash<list_array<T, Allocator>> hasher;
            return hasher(list.data());
        }
    };
}

#endif /* LIBRARY_LIST_ARRAY_LIST_ARRAY */
