#ifndef LIST_ARRAY
#define LIST_ARRAY
#include <algorithm>
#include <concepts>
#include <functional>
#include <iterator>
#include <memory>
#include <stdexcept>
#include <stdint.h>
#include <type_traits>
#include <utility>

/**
 * @namespace __new_list_array_impl
 * @brief Implementation details for a hybrid `list_array` container.
 *
 * This namespace contains the implementation of the `list_array` class, a container designed to
 * offer the random access capabilities of an array and the insertion/deletion flexibility of a list. 
 * It is particularly well-suited for move-only types where copying is expensive.
 */
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
        using value_type = typename T::value_type;
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

    /**
     * @struct compressed_allocator
     * @brief A space-optimized allocator that stores a value alongside its allocation state.
     *
     * This allocator is designed to be used when the allocator itself is small or empty, allowing
     * it to store an additional value without incurring significant overhead.
     *
     * @tparam Alloc The underlying allocator type.
     * @tparam T The type of the value to store alongside the allocator.
     */
    template <class Alloc, class T, bool = std::is_empty_v<Alloc> && !std::is_final_v<Alloc>>
    struct compressed_allocator final : private Alloc {
    public:
        T hold_value{};

        template <class... Args>
        compressed_allocator(const Alloc& allocator, Args&&... args)
            : Alloc(allocator), hold_value(std::forward<Args>(args)...) {}

        compressed_allocator(compressed_allocator<Alloc, T>&& move)
            : Alloc(move), hold_value(std::move(move.hold_value)) {}

        using value_type = typename Alloc::value_type;
        using size_type = typename Alloc::size_type;

        value_type* allocate(size_type n) {
            return Alloc::allocate(n);
        }

        void deallocate(value_type* p, size_type n) {
            Alloc::deallocate(p, n);
        }

        Alloc& get_allocator() {
            return *this;
        }

        const Alloc& get_allocator() const {
            return *this;
        }

        compressed_allocator<Alloc, T>& operator=(const compressed_allocator<Alloc, T>& allocator) {
            if constexpr (std::allocator_traits<Alloc>::propagate_on_container_copy_assignment::value) {
                Alloc::operator=(allocator);
            }
            hold_value = allocator.hold_value;
            return *this;
        }

        compressed_allocator<Alloc, T>& operator=(compressed_allocator<Alloc, T>&& allocator) {
            if constexpr (std::allocator_traits<Alloc>::propagate_on_container_move_assignment::value) {
                Alloc::operator=(std::move(allocator));
            }
            hold_value = std::move(allocator.hold_value);
            return *this;
        }
    };

    /**
     * @struct compressed_allocator
     * @brief A space-optimized allocator that stores a value alongside its allocation state.
     *
     * This allocator is designed to be used when the allocator itself is small or empty, allowing
     * it to store an additional value without incurring significant overhead.
     *
     * @tparam Alloc The underlying allocator type.
     * @tparam T The type of the value to store alongside the allocator.
     */
    template <class Alloc, class T>
    struct compressed_allocator<Alloc, T, false> final {
        Alloc allocator;

    public:
        T hold_value{};

        template <class... Args>
        compressed_allocator(const Alloc& allocator, Args&&... args)
            : allocator(allocator), hold_value(std::forward<Args>(args)...) {}

        compressed_allocator(compressed_allocator<Alloc, T>&& move)
            : allocator(move), hold_value(std::move(move.hold_value)) {}

        using value_type = typename Alloc::value_type;
        using size_type = typename Alloc::size_type;

        value_type* allocate(size_type n) {
            return allocator.allocate(n);
        }

        void deallocate(value_type* p, size_type n) {
            allocator.deallocate(p, n);
        }

        Alloc& get_allocator() {
            return allocator;
        }

        const Alloc& get_allocator() const {
            return allocator;
        }

        compressed_allocator<Alloc, T> operator=(const compressed_allocator<Alloc, T>& allocator) {
            if constexpr (std::allocator_traits<Alloc>::propagate_on_container_copy_assignment::value) {
                this->allocator = allocator;
            }
            hold_value = allocator.hold_value;
        }

        compressed_allocator<Alloc, T> operator=(compressed_allocator<Alloc, T>&& allocator) {
            if constexpr (std::allocator_traits<Alloc>::propagate_on_container_move_assignment::value) {
                this->allocator = std::move(allocator);
            }
            hold_value = std::move(allocator.hold_value);
        }
    };

    /** 
     * @struct auto_deallocate
     * @brief A RAII (Resource Acquisition Is Initialization) helper class that automatically deallocates
     *        memory allocated by an allocator when it goes out of scope.
     */
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

    /**
     * @class bit_array_helper
     * @brief A helper class to efficiently manage a bit array, used for tracking selected elements during removals.
     */
    class bit_array_helper {
        uint8_t* data;
        size_t size;
        size_t _set_values = 0;

    public:
        constexpr bit_array_helper(size_t size)
            : size(size), data(new uint8_t[(size + 7) / 8]) {
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
        custom_unique_ptr(T* data, Allocator& allocator, void (*deleter)(T*, Allocator&))
            : data(data), allocator(allocator), deleter(deleter) {}

        ~custom_unique_ptr() {
            if (data)
                deleter(data, allocator);
            data = 0;
        }

        T* get() const noexcept {
            return data;
        }

        T* release() noexcept {
            T* temp = data;
            data = nullptr;
            return temp;
        }

        T* operator->() const noexcept {
            return data;
        }

        T& operator*() const noexcept {
            return *data;
        }
    };

    /**
     * @struct arr_block
     * @brief A block of memory used to store elements within the `list_array`.
     *
     * Each `arr_block` holds a contiguous array of elements and pointers to the next
     * and previous blocks in the list.
     *
     * @tparam T The type of the elements stored in the block.
     */
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

    /**
     * @class arr_block_manager
     * @brief Manages the allocation and deallocation of `arr_block` instances.
     *
     * @tparam T The type of the elements stored in the blocks.
     * @tparam Allocator The allocator type used for memory management.
     */
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

    /**
     * @class list_array
     * @brief A hybrid list-array container optimized for move-only types.
     *
     * This class provides the random access capabilities of an array combined with 
     * the insertion/deletion flexibility of a linked list. It's designed to be 
     * efficient for storing move-only types, as it minimizes unnecessary copying 
     * of elements.
     *
     * @tparam T The type of elements stored in the container.
     * @tparam Allocator The allocator type used for memory management.
     *
     * @note This class is not thread-safe.
     * @note Can operate on move-only elements.
     * @note The iterator will become invalid after removing, taking, committing, decommitting, inserting, sorting, or any other operation that modifies the block size or removes the block from the array.
     */
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
                    throw std::out_of_range("list_array::reverse_iterator::operator-=: sub out of range");
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

            ptrdiff_t operator-(const reverse_iterator& other) {
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
                    throw std::out_of_range("list_array::const_reverse_iterator::operator-=: sub out of range");
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

            ptrdiff_t operator-(const const_reverse_iterator& other) const {
                return ptrdiff_t(other.absolute_index) - absolute_index;
            }
        };

        using value_type = T;
        using reference = T&;
        using const_reference = const T&;
        using size_type = size_t;
        using difference_type = ptrdiff_t;
        static constexpr inline size_t npos = -1;

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
        /// @name internal
        /// @{
        compressed_allocator<Allocator, size_t> allocator_and_size;
        arr_block<T>* first_block = nullptr; // front
        arr_block<T>* last_block = nullptr;  // back
        size_t _reserved_front = 0;          // .*==] first
        size_t _reserved_back = 0;           // .[==* last

        constexpr inline size_t& _size() noexcept {
            return allocator_and_size.hold_value;
        }

        constexpr inline size_t _size() const noexcept {
            return allocator_and_size.hold_value;
        }

        /**
         * @brief Retrieves the element at the specified index from the front block.
         * @param index The index of the element to retrieve.
         * @return The element at the specified index.
         */
        constexpr T* get_element_at_index_front(size_t index) const noexcept {
            size_t i = index;
            arr_block<T>* block = first_block;
            while (i >= block->data_size) {
                i -= block->data_size;
                block = block->next;
            }
            return block->data + i;
        }

        /**
         * @brief Retrieves the element at the specified index from the back block.
         * @param index The index of the element to retrieve.
         * @return The element at the specified index.
         */
        constexpr T* get_element_at_index_back(size_t index) const noexcept {
            size_t i = index;
            arr_block<T>* block = last_block;
            while (i >= block->data_size) {
                i -= block->data_size;
                block = block->prev;
            }
            return block->data + block->data_size - i - 1;
        }

        /**
         * @brief Retrieves the element at the specified index.
         * @param index The index of the element to retrieve.
         * @return The element at the specified index.
         */
        constexpr T* get_element_at_index(size_t index) const noexcept {
            index += _reserved_front;
            return index < capacity() / 2
                       ? get_element_at_index_front(index)
                       : get_element_at_index_back(capacity() - index - 1);
        }

        /**
         * @brief Retrieves the element at the specified index.
         * @param index The index of the element to retrieve.
         * @return The element at the specified index.
         */
        constexpr T* get_direct_element_at_index(size_t index) const noexcept {
            return index < capacity() / 2
                       ? get_element_at_index_front(index)
                       : get_element_at_index_back(capacity() - index - 1);
        }

        /**
         * @brief Retrieves an iterator pointing to the element at the specified index from the front block.
         * @param index The index of the element to retrieve the iterator for.
         * @return An iterator pointing to the element at the specified index.
         */
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

        /**
         * @brief Retrieves an iterator pointing to the element at the specified index from the back block.
         * @param index The index of the element to retrieve the iterator for.
         * @return An iterator pointing to the element at the specified index.
         */
        constexpr iterator get_iterator_at_index_back(size_t index, size_t absolute_index) noexcept {
            size_t i = index;
            arr_block<T>* block = last_block;
            if (!block)
                return iterator(nullptr, 0, absolute_index);
            if (absolute_index == _size())
                return iterator(last_block, block->data_size, absolute_index);

            while (i >= block->data_size) {
                i -= block->data_size;
                block = block->prev;
                if (!block)
                    return iterator(nullptr, 0, absolute_index);
            }
            return iterator(block, block->data_size - i - 1, absolute_index);
        }

        /**
         * @brief Retrieves an iterator pointing to the element at the specified index.
         * @param index The index of the element to retrieve the iterator for.
         * @return An iterator pointing to the element at the specified index.
         */
        constexpr iterator get_iterator_at_index(size_t index) noexcept {
            return index < capacity() / 2
                       ? get_iterator_at_index_front(index + _reserved_front)
                       : get_iterator_at_index_back(capacity() - (index + _reserved_front) - 1, index + _reserved_front);
        }

        /**
         * @brief Steals a reserve block from the back and moves it to the front.
         */
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

        /**
         * @brief Steals a reserve block from the front and moves it to the back.
         */
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

        /**
         * @brief Policy to decide if a block should be split during insertion.
         *        Larger blocks are more likely to be split to maintain balance.
         * @param block_size The size of the block being considered.
         * @return true if the block should be split, false otherwise.
         */
        constexpr static inline bool split_policy(size_t block_size) noexcept {
            if (block_size < 2048)
                return false;
            else
                return true;
        }

        /**
         * @brief Policy to determine how much to increase capacity during push operations.
         *        Scales down the increase factor as the array grows.
         * @return The amount to increase capacity by.
         */
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

        /** 
         * @brief Get the range of constructed elements within a block, relative to the overall array.
         * @param iter An iterator pointing to an element in the block.
         * @return A pair containing the start and end indices (relative to the block) of the constructed range.
         */
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

        /**
         * @brief Similar to get_handle_range, but throws an exception if the iterator is not within the
         *        constructed part of its block.
         * @param iter An iterator pointing to an element in the block.
         * @return A pair containing the start and end indices (relative to the block) of the constructed range.
         * @throws std::out_of_range If the iterator is outside the constructed range of its block.
         */
        constexpr std::pair<size_t, size_t> checked_get_handle_range(const_iterator iter) {
            auto [startConstructed, endConstructed] = get_handle_range(iter);
            if (iter.relative_index < startConstructed || iter.relative_index > endConstructed)
                throw std::out_of_range("Iterator is outside the constructed range.");
            return {startConstructed, endConstructed};
        }

        /**
         * @brief Throws an exception if the given iterator is not within the constructed range of its block.
         * @param iter The iterator to check.
         * @throws std::out_of_range If the iterator is outside the constructed range of its block.
         */
        constexpr void check_handle_range(const_iterator iter) {
            checked_get_handle_range(iter);
        }

        /**
         * @brief Releases (destroys and deallocates) a block of memory from the array.
         * @param block The block to release.
         * @param block_index The absolute index of the block's first element in the array.
         */
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

        /**
         * @brief Replaces a block with two new blocks, splitting the constructed elements.
         * @param block The block to replace.
         * @param handle_begin The start index (relative to the block) of the constructed range.
         * @param handle_end The end index (relative to the block) of the constructed range.
         * @param swap_block_0 The first new block to replace with.
         * @param swap_block_1 The second new block to replace with.
         */
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

        /** 
         * @brief Inserts a block between two existing blocks.
         * @param to_insert The block to insert.
         * @param block_0 The block before the insertion point.
         * @param block_1 The block after the insertion point.
         */
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
        constexpr void insert_item_split(const_iterator iter, T&& item) {
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
            auto [startConstructed, endConstructed] = get_handle_range(iter.block);

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

        /**
         * @brief Removes all elements from the array within the specified range.
         *
         * This function erases elements from the array starting at `begin` and up to (but not including) `end`. The iterators `begin` and `end` must be valid and dereferenceable, and `end` must not be before `begin`.
         *
         * @param begin An iterator to the first element to erase.
         * @param end An iterator to the element after the last element to erase.
         * @throws std::invalid_argument If `begin` is after `end`.
         * @throws std::out_of_range If either `begin` or `end` is outside the constructed range of the array.
         */
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

        /**
         * @brief This function assumes the validity of input parameters and is meant for internal use.
         * @param[in] selected The bit_array_helper indicating which elements to remove.
         * @param begin An iterator to the beginning of the range to remove from.
         * @param end An iterator to the end of the range to remove from.
         * @param startConstructed The index of the first constructed element in the block.
         * @param endConstructed The index of the last constructed element in the block.
         */
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

        /** 
         * @brief Selects elements within a block for removal based on a predicate.
         *        This function assumes the validity of input parameters and is meant for internal use.
         * @tparam FN The type of the predicate function.
         * @param fn The predicate function to apply.
         * @param[out] selector The bit_array_helper to store the selection results.
         * @param begin An iterator to the beginning of the range to select from.
         * @param end An iterator to the end of the range to select from.
         */
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
                remove_in_block(std::forward<FN>(fn), const_iterator(end.block, 0, end.absolute_index), block_iter);
                block_iter = const_iterator(end.block->prev, 0, end.absolute_index - end.block->data_size);
                while (block_iter.block != begin.block) {
                    remove_in_block(std::forward<FN>(fn), const_iterator(block_iter.block, 0, block_iter.absolute_index - block_iter.block->data_size), block_iter);
                    block_iter = const_iterator(block_iter.block->prev, 0, block_iter.absolute_index - block_iter.block->data_size);
                }
                remove_in_block(std::forward<FN>(fn), begin, const_iterator(begin.block, begin.block->data_size, begin.absolute_index + begin.block->data_size - begin.relative_index));
            }
        }

        /// @}
    public:
        /**
         * @brief Removes all elements from the array.
         *
         * This function destroys all elements in the `list_array` and resets its size, capacity, and reserved space to zero. The underlying memory is deallocated, effectively making the container empty.
         */
        constexpr ~list_array() {
            clear();
        }

#pragma region constructors

        /// @name constructors
        /// @{
        /**
         * @brief Default constructor for the `list_array` class.
         *
         * This constructor creates an empty `list_array` object with the specified allocator.
         *
         * @param allocator The allocator object to use for memory management (defaults to the default allocator for type `T`).
         */
        constexpr list_array(const Allocator& allocator = Allocator())
            : allocator_and_size(allocator) {}

        /**
         * @brief Constructor for the `list_array` class that takes an initializer list as input.
         *
         * This constructor creates a `list_array` object by copying elements from the given initializer list `vals`.
         *
         * @param vals The initializer list containing the elements to copy.
         * @param allocator The allocator object to use for memory management (defaults to the default allocator for type `T`).
         */
        constexpr list_array(std::initializer_list<T> vals, const Allocator& allocator = Allocator())
            : allocator_and_size(allocator) {
            reserve(vals.size());
            for (const T& it : vals)
                push_back(it);
        }

        /**
         * @brief Constructor for the `list_array` class that takes an initializer list of a different type as input.
         *
         * This constructor creates a `list_array` object by converting and copying elements from the given initializer list `vals`.
         *
         * @tparam AnotherT The type of the elements in the initializer list.
         * @param vals The initializer list containing the elements to convert and copy.
         * @param allocator The allocator object to use for memory management (defaults to the default allocator for type `T`).
         */
        template <class AnotherT>
        constexpr list_array(std::initializer_list<AnotherT> vals, const Allocator& allocator = Allocator())
            : allocator_and_size(allocator) {
            for (const AnotherT& it : vals)
                push_back(it);
        }

        /**
         * @brief Constructor for the `list_array` class that takes a C-style array as input.
         *
         * This constructor creates a `list_array` object by copying elements from the given C-style array `arr`.
         *
         * @tparam arr_size The size of the C-style array `arr`.
         * @param arr The C-style array containing the elements to copy.
         * @param allocator The allocator object to use for memory management (defaults to the default allocator for type `T`).
         */
        template <size_t arr_size>
        constexpr list_array(const T (&arr)[arr_size], const Allocator& allocator = Allocator())
            : allocator_and_size(allocator) {
            push_back(arr, arr_size);
        }

        /**
         * @brief Constructor for the `list_array` class that takes a pointer and size as input.
         *
         * This constructor creates a `list_array` object by copying elements from the array pointed to by `arr` with the specified `arr_size`.
         *
         * @param arr A pointer to the beginning of the array to copy elements from.
         * @param arr_size The number of elements to copy from the array.
         * @param allocator The allocator object to use for memory management (defaults to the default allocator for type `T`).
         */
        constexpr list_array(const T* arr, size_t arr_size, const Allocator& allocator = Allocator())
            : allocator_and_size(allocator) {
            push_back(arr, arr_size);
        }

        /**
         * @brief Constructor for the `list_array` class that takes an iterable range as input.
         *
         * This constructor creates a `list_array` object by copying elements from the iterable range defined by `begin` and `end`. It optionally allows reserving space for a specific number of elements (`reserve_len`).
         *
         * @tparam Iterable The type of the iterators defining the range.
         * @param begin An iterator to the beginning of the range.
         * @param end An iterator to the end of the range.
         * @param reserve_len The number of elements to reserve space for (defaults to 0).
         * @param allocator The allocator object to use for memory management (defaults to the default allocator for type `T`).
         */
        template <typename Iterable>
        constexpr list_array(Iterable begin, Iterable end, size_t reserve_len = 0, const Allocator& allocator = Allocator())
            : allocator_and_size(allocator) {
            if constexpr (std::is_pointer<Iterable>::value) {
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

        /**
         * @brief Constructor for the `list_array` class that creates an array of a specific size.
         *
         * This constructor creates a `list_array` object with the specified `size`. The elements are default-initialized.
         *
         * @param size The number of elements to create in the array.
         * @param allocator The allocator object to use for memory management (defaults to the default allocator for type `T`).
         */
        constexpr list_array(size_t size, const Allocator& allocator = Allocator())
            requires std::is_default_constructible_v<T>
            : allocator_and_size(allocator) {
            resize(size);
        }

        /**
         * @brief Constructor for the `list_array` class that creates an array of a specific size with a default value.
         *
         * This constructor creates a `list_array` object with the specified `size`. All elements are initialized with the given `default_init` value.
         *
         * @param size The number of elements to create in the array.
         * @param default_init The value to initialize each element with.
         * @param allocator The allocator object to use for memory management (defaults to the default allocator for type `T`).
         */
        constexpr list_array(size_t size, const T& default_init, const Allocator& allocator = Allocator())
            requires std::is_copy_constructible_v<T>
            : allocator_and_size(allocator) {
            resize(size, default_init);
        }

        /**
         * @brief Move constructor for the `list_array` class.
         *
         * This constructor creates a new `list_array` object by moving the contents of another `list_array` object (`move`).
         *
         * @param move The `list_array` object to move from.
         * @note After the move, the `move` object is left in a valid but unspecified state.
         */
        constexpr list_array(list_array&& move) noexcept
            : allocator_and_size(move.allocator_and_size.get_allocator()) {
            operator=(std::move(move));
        }

        /**
         * @brief Copy constructor for the `list_array` class.
         *
         * This constructor creates a new `list_array` object by copying the contents of another `list_array` object (`copy`).
         *
         * @param copy The `list_array` object to copy from.
         */
        constexpr list_array(const list_array& copy)
            : allocator_and_size(copy.allocator_and_size.get_allocator()) {
            operator=(copy);
        }

        /**
         * @brief Copy constructor for the `list_array` class with a specified allocator.
         *
         * This constructor creates a new `list_array` object by copying the contents of another `list_array` object (`copy`) and using the specified allocator.
         *
         * @param copy The `list_array` object to copy from.
         * @param allocator The allocator object to use for memory management.
         */
        constexpr list_array(const list_array& copy, const Allocator& allocator)
            : allocator_and_size(allocator) {
            operator=(copy);
        }

        /**
         * @brief Copy constructor for the `list_array` class with a starting position.
         *
         * This constructor creates a new `list_array` object by copying elements from another `list_array` object (`copy`), starting from the specified `start` index.
         *
         * @param copy The `list_array` object to copy from.
         * @param start The starting index from which to copy elements.
         * @param allocator The allocator object to use for memory management (defaults to the default allocator for type `T`).
         */
        constexpr list_array(const list_array& copy, size_t start, const Allocator& allocator = Allocator())
            : list_array(copy.get_iterator(start), copy.end(), copy.size() - start, allocator) {}

        /**
         * @brief Copy constructor for the `list_array` class with a starting and ending position.
         *
         * This constructor creates a new `list_array` object by copying elements from another `list_array` object (`copy`), within the range from `start` to `end`.
         *
         * @param copy The `list_array` object to copy from.
         * @param start The starting index from which to copy elements (inclusive).
         * @param end The ending index up to which to copy elements (exclusive).
         * @param allocator The allocator object to use for memory management (defaults to the default allocator for type `T`).
         */
        constexpr list_array(const list_array& copy, size_t start, size_t end, const Allocator& allocator = Allocator())
            : list_array(copy.get_iterator(start), copy.get_iterator(end), end - start, allocator) {}

        /**
         * @brief Constructor for the `list_array` class that takes a container as input.
         *
         * This constructor creates a `list_array` object by copying or moving elements from the given container `cont`. The elements are moved if `cont` is an rvalue reference, otherwise they are copied.
         *
         * @tparam Container The type of the container to copy or move elements from.
         * @param cont The container object to copy or move elements from.
         * @param allocator The allocator object to use for memory management (defaults to the default allocator for type `T`).
         */
        template <class Container>
        constexpr list_array(Container&& cont, const Allocator& allocator = Allocator())
            requires is_container<Container>::value
            : allocator_and_size(allocator) {
            reserve(cont.size());
            size_t i = 0;
            for (auto&& it : cont)
                push_back(T(std::move(it)));
        }

        /**
         * @brief Constructor for the `list_array` class that takes a container as input.
         *
         * This constructor creates a `list_array` object by copying elements from the given container `cont`.
         *
         * @tparam Container The type of the container to copy elements from.
         * @param cont The container object to copy elements from.
         * @param allocator The allocator object to use for memory management (defaults to the default allocator for type `T`).
         */
        template <class Container>
        constexpr list_array(const Container& cont, const Allocator& allocator = Allocator())
            requires is_container<Container>::value
            : allocator_and_size(allocator) {
            reserve(cont.size());
            size_t i = 0;
            for (const auto& it : cont)
                push_back(it);
        }

/// @}
#pragma endregion
#pragma region operators

        /// @name operators
        /// @{
        /**
         * @brief Assigns new contents to the container, replacing its current contents.
         *
         * This function replaces the contents of the `list_array` with the contents of another `list_array` (`move`) using move semantics. After the operation, the current `list_array` will contain the elements from `move`, and `move` will be left in an unspecified but valid state.
         *
         * @param move The `list_array` whose contents will be moved into this `list_array`.
         * @return A reference to this `list_array` after the assignment.
         */
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

        /**
         * @brief Assigns new contents to the container, replacing its current contents.
         *
         * This function replaces the contents of the `list_array` with the contents of another `list_array` (`copy`). The elements in `copy` are copied into this `list_array`.
         *
         * @param copy The `list_array` whose contents will be copied into this `list_array`.
         * @return A reference to this `list_array` after the assignment.
         */
        constexpr list_array<T, Allocator>& operator=(const list_array<T, Allocator>& copy) {
            if (first_block == copy.first_block)
                return *this;
            clear();
            reserve(copy.size());
            for (auto& it : copy)
                push_back(it);
            return *this;
        }

/// @}
#pragma endregion
#pragma region push

        /// @name push
        /// @{
        /**
        * @brief Adds a copy of the value to the end of the array.
        *
        * @param value The value to be added.
        */
        constexpr list_array<T, Allocator>& push_back(const T& value) & {
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

        /**
         * @brief Adds a value to the end of the array using move semantics.
         * 
         * @param value The value to be moved into the array.
         */
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

        /**
         * @brief Appends a copy of another `list_array` to the end of this array.
         * 
         * @tparam AnyAllocator The allocator type of the other `list_array`.
         * @param alloc The other `list_array` whose elements will be copied.
         */
        template <class AnyAllocator>
        constexpr list_array<T, Allocator>& push_back(const list_array<T, AnyAllocator>& alloc) & {
            if (_reserved_back < alloc.size())
                reserve_back(alloc.size() - _reserved_back);
            for (const auto& value : alloc)
                push_back(value);
            return *this;
        }

        /**
         * @brief Appends another `list_array` to the end of this array using move semantics.
         * 
         * @tparam AnyAllocator The allocator type of the other `list_array`.
         * @param alloc The other `list_array` whose elements will be moved.
         */
        template <class AnyAllocator>
        constexpr list_array<T, Allocator>& push_back(list_array<T, AnyAllocator>&& alloc) & {
            if (_reserved_back < alloc.size())
                reserve_back(alloc.size() - _reserved_back);
            for (auto& value : alloc)
                push_back(std::move(value));
            return *this;
        }

        /**
         * @brief Appends a range of elements to the end of the array.
         * 
         * @param begin An iterator to the beginning of the range.
         * @param end An iterator to the end of the range.
         */
        constexpr list_array<T, Allocator>& push_back(const T* begin, const T* end) & {
            if (_reserved_back < end - begin)
                reserve_back(end - begin - _reserved_back);
            for (const T* it = begin; it != end; it++)
                push_back(*it);
            return *this;
        }

        /**
         * @brief Appends a C-style array to the end of the array.
         * 
         * @tparam N The size of the C-style array.
         * @param arr The C-style array to append.
         */
        template <size_t N>
        constexpr list_array<T, Allocator>& push_back(const T (&arr)[N]) & {
            return push_back(arr, arr + N);
        }

        /**
         * @brief Appends elements from an array to the end, given a pointer and size.
         * 
         * @param arr A pointer to the beginning of the array.
         * @param size The number of elements to append.
         */
        constexpr list_array<T, Allocator>& push_back(const T* arr, size_t size) & {
            return push_back(arr, arr + size);
        }

        /**
         * @brief Constructs an element in-place at the end of the array.
         * 
         * @tparam Args The types of the arguments to be forwarded to the constructor of T.
         * @param args The arguments to be forwarded to the constructor of T.
         */
        template <class... Args>
        constexpr list_array<T, Allocator>& emplace_back(Args&&... args) & {
            if (_reserved_back) {
                std::construct_at(get_direct_element_at_index(_reserved_front + _size()), std::forward<Args>(args)...);
                ++_size();
                --_reserved_back;
                return *this;
            } else if (_reserved_front) {
                if (first_block->data_size <= _reserved_front) {
                    steal_reserve_block_front_to_back();
                    return emplace_back(std::forward<Args>(args)...);
                }
            }
            reserve_back(increase_policy());
            return emplace_back(std::forward<Args>(args)...);
        }

        /**
         * @brief Adds a copy of the value to the beginning of the array.
         * 
         * @param value The value to be added.
         */
        constexpr list_array<T, Allocator>& push_front(const T& value) & {
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

        /**
         * @brief Adds a value to the beginning of the array using move semantics.
         * 
         * @param value The value to be moved into the array.
         */
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

        /**
         * @brief Appends a copy of another `list_array` to the beginning of this array.
         * 
         * @tparam AnyAllocator The allocator type of the other `list_array`.
         * @param alloc The other `list_array` whose elements will be copied.
         */
        template <class AnyAllocator>
        constexpr list_array<T, Allocator>& push_front(const list_array<T, AnyAllocator>& alloc) & {
            if (_reserved_front < alloc.size())
                reserve_front(alloc.size() - _reserved_front);
            for (const auto& value : alloc)
                push_front(value);
            return *this;
        }

        /**
         * @brief Appends another `list_array` to the beginning of this array using move semantics.
         * 
         * @tparam AnyAllocator The allocator type of the other `list_array`.
         * @param alloc The other `list_array` whose elements will be moved.
         */
        template <class AnyAllocator>
        constexpr list_array<T, Allocator>& push_front(list_array<T, AnyAllocator>&& alloc) & {
            if (_reserved_front < alloc.size())
                reserve_front(alloc.size() - _reserved_front);
            for (auto& value : alloc)
                push_front(std::move(value));
            return *this;
        }

        /**
         * @brief Appends a range of elements to the beginning of the array.
         * 
         * @param begin An iterator to the beginning of the range.
         * @param end An iterator to the end of the range.
         */
        constexpr list_array<T, Allocator>& push_front(const T* begin, const T* end) & {
            if (_reserved_front < end - begin)
                reserve_front(end - begin - _reserved_front);
            for (const T* it = begin; it != end; it++)
                push_front(*it);
            return *this;
        }

        /**
         * @brief Appends a C-style array to the beginning of the array.
         * 
         * @tparam N The size of the C-style array.
         * @param arr The C-style array to append.
         */
        template <size_t N>
        constexpr list_array<T, Allocator>& push_front(const T (&arr)[N]) & {
            return push_front(arr, arr + N);
        }

        /**
         * @brief Appends elements from an array to the beginning, given a pointer and size.
         * 
         * @param arr A pointer to the beginning of the array.
         * @param size The number of elements to append.
         */
        constexpr list_array<T, Allocator>& push_front(const T* arr, size_t size) & {
            return push_front(arr, arr + size);
        }

        /**
         * @brief Constructs an element in-place at the beginning of the array.
         * 
         * @tparam Args The types of the arguments to be forwarded to the constructor of T.
         * @param args The arguments to be forwarded to the constructor of T.
         */
        template <class... Args>
        constexpr list_array<T, Allocator>& emplace_front(Args&&... args) & {
            if (_reserved_front) {
                std::construct_at(get_direct_element_at_index(_reserved_front - 1), std::forward<Args>(args)...);
                ++_size();
                --_reserved_front;
                return *this;
            } else if (_reserved_back) {
                if (last_block->data_size <= _reserved_back) {
                    steal_reserve_block_back_to_front();
                    return emplace_front(std::forward<Args>(args)...);
                }
            }
            reserve_front(increase_policy());
            return emplace_front(std::forward<Args>(args)...);
        }

        /**
        * @brief Adds a copy of the value to the end of the array.
        *
        * @param value The value to be added.
        * @return list_array<T, Allocator> A new list_array with the last element removed.
        */
        [[nodiscard]] constexpr list_array<T, Allocator> push_back(const T& value) && {
            push_back(value);
            return std::move(*this);
        }

        /**
         * @brief Adds a value to the end of the array using move semantics.
         * 
         * @param value The value to be moved into the array.
         * @return list_array<T, Allocator> A new list_array with the last element removed.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> push_back(T&& value) && {
            push_back(std::move(value));
            return std::move(*this);
        }

        /**
         * @brief Appends a copy of another `list_array` to the end of this array.
         * 
         * @tparam AnyAllocator The allocator type of the other `list_array`.
         * @param alloc The other `list_array` whose elements will be copied.
         * @return list_array<T, Allocator> A new list_array with the last element removed.
         */
        template <class AnyAllocator>
        [[nodiscard]] constexpr list_array<T, Allocator> push_back(const list_array<T, AnyAllocator>& alloc) && {
            push_back(alloc);
            return std::move(*this);
        }

        /**
         * @brief Appends another `list_array` to the end of this array using move semantics.
         * 
         * @tparam AnyAllocator The allocator type of the other `list_array`.
         * @param alloc The other `list_array` whose elements will be moved.
         * @return list_array<T, Allocator> A new list_array with the last element removed.
         */
        template <class AnyAllocator>
        [[nodiscard]] constexpr list_array<T, Allocator> push_back(list_array<T, AnyAllocator>&& alloc) && {
            push_back(std::move(alloc));
            return std::move(*this);
        }

        /**
         * @brief Appends a range of elements to the end of the array.
         * 
         * @param begin An iterator to the beginning of the range.
         * @param end An iterator to the end of the range.
         * @return list_array<T, Allocator> A new list_array with the last element removed.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> push_back(const T* begin, const T* end) && {
            push_front(begin, end);
            return std::move(*this);
        }

        /**
         * @brief Appends a C-style array to the end of the array.
         * 
         * @tparam N The size of the C-style array.
         * @param arr The C-style array to append.
         * @return list_array<T, Allocator> A new list_array with the last element removed.
         */
        template <size_t N>
        [[nodiscard]] constexpr list_array<T, Allocator> push_back(const T (&arr)[N]) && {
            push_back(arr, arr + N);
            return std::move(*this);
        }

        /**
         * @brief Appends elements from an array to the end, given a pointer and size.
         * 
         * @param arr A pointer to the beginning of the array.
         * @param size The number of elements to append.
         * @return list_array<T, Allocator> A new list_array with the last element removed.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> push_back(const T* arr, size_t size) && {
            push_back(arr, arr + size);
            return std::move(*this);
        }

        /**
         * @brief Constructs an element in-place at the end of the array.
         * 
         * @tparam Args The types of the arguments to be forwarded to the constructor of T.
         * @param args The arguments to be forwarded to the constructor of T.
         * @return list_array<T, Allocator> A new list_array with the last element removed.
         */
        template <class... Args>
        [[nodiscard]] constexpr list_array<T, Allocator> emplace_back(Args&&... args) && {
            emplace_back(std::forward<Args>(args)...);
            return std::move(*this);
        }

        /**
         * @brief Adds a copy of the value to the beginning of the array.
         * 
         * @param value The value to be added.
         * @return list_array<T, Allocator> A new list_array with the last element removed.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> push_front(const T& value) && {
            push_front(value);
            return std::move(*this);
        }

        /**
         * @brief Adds a value to the beginning of the array using move semantics.
         * 
         * @param value The value to be moved into the array.
         * @return list_array<T, Allocator> A new list_array with the last element removed.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> push_front(T&& value) && {
            push_front(std::move(value));
            return std::move(*this);
        }

        /**
         * @brief Appends a copy of another `list_array` to the beginning of this array.
         * 
         * @tparam AnyAllocator The allocator type of the other `list_array`.
         * @param alloc The other `list_array` whose elements will be copied.
         * @return list_array<T, Allocator> A new list_array with the last element removed.
         */
        template <class AnyAllocator>
        [[nodiscard]] constexpr list_array<T, Allocator> push_front(const list_array<T, AnyAllocator>& alloc) && {
            push_front(alloc);
            return std::move(*this);
        }

        /**
         * @brief Appends another `list_array` to the beginning of this array using move semantics.
         * 
         * @tparam AnyAllocator The allocator type of the other `list_array`.
         * @param alloc The other `list_array` whose elements will be moved.
         * @return list_array<T, Allocator> A new list_array with the last element removed.
         */
        template <class AnyAllocator>
        [[nodiscard]] constexpr list_array<T, Allocator> push_front(list_array<T, AnyAllocator>&& alloc) && {
            push_front(std::move(alloc));
            return std::move(*this);
        }

        /**
         * @brief Appends a range of elements to the beginning of the array.
         * 
         * @param begin An iterator to the beginning of the range.
         * @param end An iterator to the end of the range.
         * @return list_array<T, Allocator> A new list_array with the last element removed.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> push_front(const T* begin, const T* end) && {
            push_front(begin, end);
            return std::move(*this);
        }

        /**
         * @brief Appends a C-style array to the beginning of the array.
         * 
         * @tparam N The size of the C-style array.
         * @param arr The C-style array to append.
         * @return list_array<T, Allocator> A new list_array with the last element removed.
         */
        template <size_t N>
        [[nodiscard]] constexpr list_array<T, Allocator> push_front(const T (&arr)[N]) && {
            push_front(arr, arr + N);
            return std::move(*this);
        }

        /**
         * @brief Appends elements from an array to the beginning, given a pointer and size.
         * 
         * @param arr A pointer to the beginning of the array.
         * @param size The number of elements to append.
         * @return list_array<T, Allocator> A new list_array with the last element removed.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> push_front(const T* arr, size_t size) && {
            push_front(arr, arr + size);
            return std::move(*this);
        }

        /**
         * @brief Constructs an element in-place at the beginning of the array.
         * 
         * @tparam Args The types of the arguments to be forwarded to the constructor of T.
         * @param args The arguments to be forwarded to the constructor of T.
         * @return list_array<T, Allocator> A new list_array with the last element removed.
         */
        template <class... Args>
        [[nodiscard]] constexpr list_array<T, Allocator> emplace_front(Args&&... args) && {
            emplace_front(std::forward<Args>(args)...);
            return std::move(*this);
        }

/// @}
#pragma endregion
#pragma region list_ops

        /// @name list operations
        /// @{
        /**
         * @brief Pops and returns the last element from the `list_array` (move version).
         *
         *  This operation is marked noexcept if the element type `T` has a noexcept destructor.
         *
         * @return list_array<T, Allocator> A new list_array with the last element removed.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> pop_back() && noexcept(std::is_nothrow_constructible_v<T>) {
            pop_back();
            return std::move(*this);
        }

        /**
         * @brief Pops and returns the first element from the `list_array` (move version).
         * 
         * This operation is marked noexcept if the element type `T` has a noexcept destructor.
         *
         * @return list_array<T, Allocator> A new list_array with the first element removed.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> pop_front() && noexcept(std::is_nothrow_constructible_v<T>) {
            pop_front();
            return std::move(*this);
        }

        /**
         * @brief Removes the last element from the array.
         *
         * This function is noexcept if and only if the destructor of `T` is noexcept.
         * 
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        constexpr list_array<T, Allocator>& pop_back() & noexcept(std::is_nothrow_constructible_v<T>) {
            if (_size() == 0)
                return *this;
            std::destroy_at(&operator[](_size() - 1));
            --_size();
            ++_reserved_back;
            return *this;
        }

        /**
         * @brief Removes the first element from the array.
         *
         * This function is noexcept if and only if the destructor of `T` is noexcept.
         * 
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        constexpr list_array<T, Allocator>& pop_front() & noexcept(std::is_nothrow_constructible_v<T>) {
            if (_size() == 0)
                return *this;
            std::destroy_at(&operator[](0));
            --_size();
            ++_reserved_front;
            return *this;
        }

        /**
         * @brief Returns a reference to the first element in the array.
         * 
         * @return A reference to the first element.
         * @throws std::out_of_range If the array is empty.
         */
        [[nodiscard]] constexpr T& front() & {
            if (!_size())
                throw std::out_of_range("list_array::front: array is empty");
            return operator[](0);
        }

        /**
         * @brief Takes the first element from the array.
         *
         * @return The element by value.
         * @throws std::out_of_range If the array is empty.
         */
        [[nodiscard]] constexpr T front() && {
            return take_front();
        }

        /**
         * @brief Returns a const reference to the first element in the array.
         * 
         * @return A const reference to the first element.
         * @throws std::out_of_range If the array is empty.
         */
        [[nodiscard]] constexpr const T& front() const& {
            if (!_size())
                throw std::out_of_range("list_array::front: array is empty");
            return operator[](0);
        }

        /**
         * @brief Returns a reference to the last element in the array.
         *
         * @return A reference to the last element.
         * @throws std::out_of_range If the array is empty.
         */
        [[nodiscard]] constexpr T& back() & {
            if (!_size())
                throw std::out_of_range("list_array::back: array is empty");
            return operator[](_size() - 1);
        }

        /**
         * @brief Takes the last element from the array.
         *
         * @return The element by value.
         * @throws std::out_of_range If the array is empty.
         */
        [[nodiscard]] constexpr T& back() && {
            return take_back();
        }

        /**
         * @brief Returns a const reference to the last element in the array.
         *
         * @return A const reference to the last element.
         * @throws std::out_of_range If the array is empty.
         */
        [[nodiscard]] constexpr const T& back() const& {
            if (!_size())
                throw std::out_of_range("list_array::back: array is empty");
            return operator[](_size() - 1);
        }

        /**
         * @brief Removes and returns the first element from the array.
         *
         * @return The removed element by value.
         * @throws std::out_of_range If the array is empty.
         */
        [[nodiscard]] constexpr T take_front() {
            if (!_size())
                throw std::out_of_range("list_array::take_front: array is empty");
            T value(std::move(front()));
            pop_front();
            return value;
        }

        /**
         * @brief Removes and returns the last element from the array.
         *
         * @return The removed element by value.
         * @throws std::out_of_range If the array is empty.
         */
        [[nodiscard]] constexpr T take_back() {
            if (!_size())
                throw std::out_of_range("list_array::take_front: array is empty");
            T value(std::move(back()));
            pop_back();
            return value;
        }

/// @}
#pragma endregion
#pragma region insert

        /// @name insert
        /// @{
        /**
         * @brief Inserts an element at the specified index.
         *
         * This function inserts a copy of the given `value` at the specified `index` in the array. The elements at and after the insertion point are shifted to make space for the new element.
         *
         * @param index The index at which to insert the element.
         * @param value The value to insert.
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        constexpr list_array<T, Allocator>& insert(size_t index, const T& value) & {
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

        /**
         * @brief Inserts an element at the specified index using move semantics.
         *
         * This function inserts the given `value` at the specified `index` in the array using move semantics. The elements at and after the insertion point are shifted to make space for the new element.
         *
         * @param index The index at which to insert the element.
         * @param value The value to move into the array.
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
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

        /**
         * @brief Inserts a range of elements at the specified index.
         *
         * This function inserts copies of the elements from the range [`values`, `values + size`) at the specified `index` in the array. The elements at and after the insertion point are shifted to make space for the new elements.
         *
         * @param index The index at which to insert the elements.
         * @param values A pointer to the beginning of the range of elements to insert.
         * @param size The number of elements to insert.
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        constexpr list_array<T, Allocator>& insert(size_t index, const T* values, size_t size) & {
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

        /**
         * @brief Inserts a range of elements at the specified index.
         *
         * This function inserts copies of the elements from the range [`begin`, `end`) at the specified `index` in the array. The elements at and after the insertion point are shifted to make space for the new elements.
         *
         * @param index The index at which to insert the elements.
         * @param begin An iterator to the beginning of the range of elements to insert.
         * @param end An iterator to the end of the range of elements to insert.
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        constexpr list_array<T, Allocator>& insert(size_t index, const T* begin, const T* end) & {
            return insert(index, begin, end - begin);
        }

        /**
         * @brief Inserts a C-style array at the specified index.
         *
         * This function inserts copies of the elements from the C-style array `arr` at the specified `index` in the array. The elements at and after the insertion point are shifted to make space for the new elements.
         *
         * @tparam N The size of the C-style array `arr`.
         * @param index The index at which to insert the elements.
         * @param arr The C-style array to insert.
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        template <size_t N>
        constexpr list_array<T, Allocator>& insert(size_t index, const T (&arr)[N]) & {
            return insert(index, arr, N);
        }

        /**
         * @brief Inserts elements from another `list_array` at the specified index.
         *
         * This function inserts copies of the elements from the `list_array` `values` at the specified `index` in the array. The elements at and after the insertion point are shifted to make space for the new elements.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `values`.
         * @param index The index at which to insert the elements.
         * @param values The `list_array` containing the elements to insert.
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        template <class AnyAllocator>
        constexpr list_array<T, Allocator>& insert(size_t index, const list_array<T, AnyAllocator>& values) & {
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

        /**
         * @brief Inserts elements from another `list_array` at the specified index using move semantics.
         *
         * This function inserts the elements from the `list_array` `values` at the specified `index` in the array using move semantics. The elements at and after the insertion point are shifted to make space for the new elements.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `values`.
         * @param index The index at which to insert the elements.
         * @param values The `list_array` containing the elements to move into the array.
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
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

        /**
         * @brief Inserts an element at the specified index.
         *
         * This function inserts a copy of the given `value` at the specified `index` in the array. The elements at and after the insertion point are shifted to make space for the new element. If the index is 0, the element is inserted at the beginning (equivalent to `push_front`). If the index is equal to the size of the array, the element is inserted at the end (equivalent to `push_back`).
         *
         * @param index The index at which to insert the element.
         * @param value The value to insert.
         * @return A new `list_array` object with the inserted element.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> insert(size_t index, const T& value) && {
            insert(index, value);
            return std::move(*this);
        }

        /**
         * @brief Inserts an element at the specified index using move semantics.
         *
         * This function inserts the given `value` at the specified `index` in the array using move semantics. The elements at and after the insertion point are shifted to make space for the new element. If the index is 0, the element is inserted at the beginning (equivalent to `push_front`). If the index is equal to the size of the array, the element is inserted at the end (equivalent to `push_back`).
         *
         * @param index The index at which to insert the element.
         * @param value The value to move into the array.
         * @return A new `list_array` object with the inserted element.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> insert(size_t index, T&& value) && {
            insert(index, std::move(value));
            return std::move(*this);
        }

        /**
         * @brief Inserts a range of elements at the specified index.
         *
         * This function inserts copies of the elements from the range [`values`, `values + size`) at the specified `index` in the array. The elements at and after the insertion point are shifted to make space for the new elements. If the index is 0, the elements are inserted at the beginning (equivalent to `push_front`). If the index is equal to the size of the array, the elements are inserted at the end (equivalent to `push_back`).
         *
         * @param index The index at which to insert the elements.
         * @param values A pointer to the beginning of the range of elements to insert.
         * @param size The number of elements to insert.
         * @return A new `list_array` object with the inserted elements.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> insert(size_t index, const T* values, size_t size) && {
            insert(index, values, size);
            return std::move(*this);
        }

        /**
         * @brief Inserts a range of elements at the specified index.
         *
         * This function inserts copies of the elements from the range [`begin`, `end`) at the specified `index` in the array. The elements at and after the insertion point are shifted to make space for the new elements. If the index is 0, the elements are inserted at the beginning (equivalent to `push_front`). If the index is equal to the size of the array, the elements are inserted at the end (equivalent to `push_back`).
         *
         * @param index The index at which to insert the elements.
         * @param begin An iterator to the beginning of the range of elements to insert.
         * @param end An iterator to the end of the range of elements to insert.
         * @return A new `list_array` object with the inserted elements.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> insert(size_t index, const T* begin, const T* end) && {
            insert(index, begin, end - begin);
            return std::move(*this);
        }

        /**
         * @brief Inserts a C-style array at the specified index.
         *
         * This function inserts copies of the elements from the C-style array `arr` at the specified `index` in the array. The elements at and after the insertion point are shifted to make space for the new elements. If the index is 0, the elements are inserted at the beginning (equivalent to `push_front`). If the index is equal to the size of the array, the elements are inserted at the end (equivalent to `push_back`).
         *
         * @tparam N The size of the C-style array `arr`.
         * @param index The index at which to insert the elements.
         * @param arr The C-style array to insert.
         * @return A new `list_array` object with the inserted elements.
         */
        template <size_t N>
        [[nodiscard]] constexpr list_array<T, Allocator> insert(size_t index, const T (&arr)[N]) && {
            insert(index, arr, N);
            return std::move(*this);
        }

        /**
         * @brief Inserts elements from another `list_array` at the specified index.
         *
         * This function inserts copies of the elements from the `list_array` `values` at the specified `index` in the array. The elements at and after the insertion point are shifted to make space for the new elements. If the index is 0, the elements are inserted at the beginning (equivalent to `push_front`). If the index is equal to the size of the array, the elements are inserted at the end (equivalent to `push_back`).
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `values`.
         * @param index The index at which to insert the elements.
         * @param values The `list_array` containing the elements to insert.
         * @return A new `list_array` object with the inserted elements.
         */
        template <class AnyAllocator>
        [[nodiscard]] constexpr list_array<T, Allocator> insert(size_t index, const list_array<T, AnyAllocator>& values) && {
            insert(index, values);
            return std::move(*this);
        }

        /**
         * @brief Inserts elements from another `list_array` at the specified index using move semantics.
         *
         * This function inserts the elements from the `list_array` `values` at the specified `index` in the array using move semantics. The elements at and after the insertion point are shifted to make space for the new elements. If the index is 0, the elements are inserted at the beginning (equivalent to `push_front`). If the index is equal to the size of the array, the elements are inserted at the end (equivalent to `push_back`).
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `values`.
         * @param index The index at which to insert the elements.
         * @param values The `list_array` containing the elements to move into the array.
         * @return A new `list_array` object with the inserted elements.
         */
        template <class AnyAllocator>
        [[nodiscard]] constexpr list_array<T, Allocator> insert(size_t index, list_array<T, AnyAllocator>&& values) && {
            insert(index, std::move(values));
            return std::move(*this);
        }

/// @}
#pragma endregion
#pragma region remove

        /// @name remove
        /// @{
        /**
         * @brief Removes all elements equal to a specific value from the array.
         *
         * @param val The value to remove.
         * @return The number of elements removed.
         *
         */
        constexpr size_t remove(const T& val) &
            requires std::equality_comparable<T>
        {
            return remove_if([&](const T& value) constexpr { return value == val; });
        }

        /**
         * @brief Removes all elements satisfying a given predicate.
         *
         * @tparam FN The type of the predicate function. It must be invocable with 
         *            either `const T&` or `size_t, const T&` (index and element).
         * @param fn The predicate function returning true for elements to be removed.
         * @return The number of elements removed.
         */
        template <class FN>
        constexpr size_t remove_if(FN&& fn) &
            requires std::is_invocable_r_v<bool, FN, size_t, const T&> || std::is_invocable_r_v<bool, FN, const T&>
        {
            size_t old_size = _size();
            remove_in(std::forward<FN>(fn), begin(), end());
            return old_size - _size();
        }

        /**
         * @brief Removes elements from the array based on a predicate, starting from a given position.
         *
         * This function removes elements from the array, starting from the specified `begin` index, that satisfy the given predicate `fn`. The predicate can be a lambda function, a function object, or a function pointer. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         *
         * @tparam FN The type of the predicate function.
         * @param begin The starting index for removal.
         * @param fn The predicate function to apply to each element.
         * @return The number of elements removed.
         */
        template <class FN>
        constexpr size_t remove_if(size_t begin, FN&& fn) &
            requires std::is_invocable_r_v<bool, FN, size_t, const T&> || std::is_invocable_r_v<bool, FN, const T&>
        {
            size_t old_size = _size();
            remove_in(std::forward<FN>(fn), get_iterator(begin), end());
            return old_size - _size();
        }

        /**
         * @brief Removes elements from the array based on a predicate within a specified range.
         *
         * This function removes elements from the array, within the range from `begin` to `end`, that satisfy the given predicate `fn`. The predicate can be a lambda function, a function object, or a function pointer. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         *
         * @tparam FN The type of the predicate function.
         * @param begin The starting index of the range for removal (inclusive).
         * @param end The ending index of the range for removal (exclusive).
         * @param fn The predicate function to apply to each element.
         * @return The number of elements removed.
         */
        template <class FN>
        constexpr size_t remove_if(size_t begin, size_t end, FN&& fn) &
            requires std::is_invocable_r_v<bool, FN, size_t, const T&> || std::is_invocable_r_v<bool, FN, const T&>
        {
            size_t old_size = _size();
            remove_in(std::forward<FN>(fn), get_iterator(begin), get_iterator(end));
            return old_size - _size();
        }

        /**
         * @brief Removes all occurrences of a specific value from the array (move version).
         *
         * This function is similar to the lvalue reference version of `remove`, but it operates on an rvalue reference (temporary) of the `list_array`. It removes all occurrences of the given `val` and then returns the modified array by value.
         *
         * @param val The value to remove.
         * @return A new `list_array` object with all occurrences of `val` removed.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> remove(const T& val) &&
            requires std::equality_comparable<T>
        {
            remove(val);
            return std::move(*this);
        }

        /**
         * @brief Removes all elements satisfying a given predicate (move version).
         *
         * This function is similar to the lvalue reference version of `remove_if`, but it operates on an rvalue reference (temporary) of the `list_array`. It removes all elements that satisfy the predicate `fn` and then returns the modified array by value.
         *
         * @tparam FN The type of the predicate function. It must be invocable with either `const T&` or `size_t, const T&` (index and element).
         * @param fn The predicate function returning true for elements to be removed.
         * @return A new `list_array` object with all elements satisfying the predicate removed.
         */
        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> remove_if(FN&& fn) &&
            requires std::is_invocable_r_v<bool, FN, size_t, const T&> || std::is_invocable_r_v<bool, FN, const T&>
        {
            remove_if(0, _size(), std::forward<FN>(fn));
            return std::move(*this);
        }

        /**
         * @brief Removes all elements satisfying a given predicate, starting from a given position (move version).
         *
         * This function is similar to the lvalue reference version of `remove_if(size_t, FN&&)`, but it operates on an rvalue reference (temporary) of the `list_array`. It removes all elements starting from the specified `begin` index that satisfy the predicate `fn` and then returns the modified array by value.
         *
         * @tparam FN The type of the predicate function.
         * @param begin The starting index for removal.
         * @param fn The predicate function returning true for elements to be removed.
         * @return A new `list_array` object with all elements satisfying the predicate removed, starting from `begin`.
         */
        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> remove_if(size_t begin, FN&& fn) &&
            requires std::is_invocable_r_v<bool, FN, size_t, const T&> || std::is_invocable_r_v<bool, FN, const T&>
        {
            remove_if(begin, _size(), std::forward<FN>(fn));
            return std::move(*this);
        }

        /**
         * @brief Removes all elements satisfying a given predicate within a specified range (move version).
         *
         * This function is similar to the lvalue reference version of `remove_if(size_t, size_t, FN&&)`, but it operates on an rvalue reference (temporary) of the `list_array`. It removes all elements within the range from `begin` to `end` that satisfy the predicate `fn` and then returns the modified array by value.
         *
         * @tparam FN The type of the predicate function.
         * @param begin The starting index of the range for removal (inclusive).
         * @param end The ending index of the range for removal (exclusive).
         * @param fn The predicate function returning true for elements to be removed.
         * @return A new `list_array` object with all elements satisfying the predicate removed within the specified range.
         */
        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> remove_if(size_t begin, size_t end, FN&& fn) &&
            requires std::is_invocable_r_v<bool, FN, size_t, const T&> || std::is_invocable_r_v<bool, FN, const T&>
        {
            remove_if(begin, end, std::forward<FN>(fn));
            return std::move(*this);
        }

/// @}
#pragma endregion
#pragma region remove_one

        /// @name remove_one
        /// @{
        /**
         * @brief Removes the first occurrence from the array.
         *
         * This function searches for the first element in the array that satisfies predicate and removes it. If the element is not found, the array remains unchanged.
         *
         * @tparam FN The type of the predicate function.
         * @param check_function The predicate function returning true for element to be removed.
         * @return `true` if the value was found and removed, `false` otherwise.
         */
        template <class FN>
        constexpr bool remove_one(FN&& check_function) & {
            return remove_one(0, _size(), std::forward<FN>(check_function));
        }

        /**
         * @brief Removes the first occurrence from the array, starting from a given position.
         *
         * This function searches for the first element in the array, starting from the specified `start` index, that satisfies predicate and removes it. If the value is not found, the array remains unchanged.
         *
         * @tparam FN The type of the predicate function.
         * @param start The starting index for the search.
         * @param check_function The predicate function returning true for element to be removed.
         * @return `true` if the value was found and removed, `false` otherwise.
         */
        template <class FN>
        constexpr bool remove_one(size_t start, FN&& check_function) & {
            return remove_one(start, _size(), std::forward<FN>(check_function));
        }

        /**
         * @brief Removes the first occurrence from the array within a specified range.
         *
         * This function searches for the first element in the array, within the range from `start` to `end`, that satisfies predicate and removes it. If the value is not found, the array remains unchanged.
         *
         * @tparam FN The type of the predicate function.
         * @param start The starting index of the range to search (inclusive).
         * @param end The ending index of the range to search (exclusive).
         * @param check_function The predicate function returning true for element to be removed.
         * @return `true` if the value was found and removed, `false` otherwise.
         */
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

/// @}
#pragma endregion
#pragma region remove_same

        /// @name remove_same
        /// @{
        /**
         * @brief Removes all occurrences of a specific value from the array within a specified range.
         *
         * This function searches for all elements in the array, within the range from `start` to `end`, that are equal to the given `val` and removes them. It uses a custom comparison function (`comparer`) to determine equality.
         *
         * @tparam FN The type of the comparison function. It should take two const references to `T` and return `true` if they are considered equal, `false` otherwise.
         * @param val The value to remove.
         * @param start The starting index of the range to search (inclusive).
         * @param end The ending index of the range to search (exclusive).
         * @param comparer The custom comparison function to use (defaults to `==`).
         * @return The number of elements removed.
         * @throws std::out_of_range If `end` exceeds the size of the array.
         */
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

        /**
         * @brief Removes all occurrences of a specific value from the array, starting from a given position.
         *
         * This function is similar to `remove_same(const T&, size_t, size_t, FN&&)`, but it searches from the specified `start` index to the end of the array.
         *
         * @tparam FN The type of the comparison function.
         * @param val The value to remove.
         * @param start The starting index for the search.
         * @param comparer The custom comparison function to use (defaults to `==`).
         * @return The number of elements removed.
         */
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

        /**
         * @brief Removes all occurrences of a specific value from the entire array.
         *
         * This function is similar to `remove_same(const T&, size_t, size_t, FN&&)`, but it searches the entire array.
         *
         * @tparam FN The type of the comparison function.
         * @param val The value to remove.
         * @param comparer The custom comparison function to use (defaults to `==`).
         * @return The number of elements removed.
         */
        template <class FN>
        constexpr size_t remove_same(
            const T& val,
            FN&& comparer = [](const T& f, const T& s) constexpr { return f == s; }
        ) &
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            return remove_if(_reserved_front, _reserved_front + _size(), [&comparer, &val](const T& cval) { return comparer(val, cval); });
        }

        /**
         * @brief Removes all occurrences of a C-style array from the array.
         *
         * This function searches for all occurrences of the C-style array `val` in the array and removes them.
         *
         * @tparam arr_size The size of the C-style array `val`.
         * @param val The C-style array to remove.
         * @param start The starting index of the range to search (inclusive).
         * @return The number of elements removed.
         */
        template <size_t arr_size>
        constexpr size_t remove_same(const T (&val)[arr_size], size_t start = 0) &
            requires std::equality_comparable<T>
        {
            return remove_same(val, arr_size, start, _size());
        }

        /**
         * @brief Removes all occurrences of a C-style array from the array within a specified range.
         *
         * This function searches for all occurrences of the C-style array `val` within the range from `start` to `end` and removes them.
         *
         * @tparam arr_size The size of the C-style array `val`.
         * @param val The C-style array to remove.
         * @param start The starting index of the range to search (inclusive).
         * @param end The ending index of the range to search (exclusive).
         * @return The number of elements removed.
         */
        template <size_t arr_size>
        constexpr size_t remove_same(const T (&val)[arr_size], size_t start, size_t end) &
            requires std::equality_comparable<T>
        {
            return remove_same(val, arr_size, start, end);
        }

        /**
         * @brief Removes all occurrences of an array from the array.
         *
         * This function searches for all occurrences of the array pointed to by `val` (with size `arr_size`) in the array and removes them.
         *
         * @param val A pointer to the array to remove.
         * @param arr_size The size of the array to remove.
         * @param start The starting index of the range to search (inclusive).
         * @return The number of elements removed.
         */
        constexpr size_t remove_same(const T* val, size_t arr_size, size_t start = 0) &
            requires std::equality_comparable<T>
        {
            return remove_same(val, arr_size, start, _size());
        }

        /**
         * @brief Removes all occurrences of an array from the array within a specified range.
         *
         * This function searches for all occurrences of the array pointed to by `val` (with size `arr_size`) within the range from `start` to `end` and removes them.
         *
         * @param val A pointer to the array to remove.
         * @param arr_size The size of the array to remove.
         * @param start The starting index of the range to search (inclusive).
         * @param end The ending index of the range to search (exclusive).
         * @return The number of elements removed.
         */
        constexpr size_t remove_same(const T* val, size_t arr_size, size_t start, size_t end) &
            requires std::equality_comparable<T>
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

        /**
         * @brief Removes all occurrences of another `list_array` from the array.
         *
         * This function searches for all occurrences of the `list_array` `val` in the array and removes them.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `val`.
         * @param val The `list_array` to remove.
         * @param start The starting index of the range to search (inclusive).
         * @return The number of elements removed.
         */
        template <class AnyAllocator>
        constexpr size_t remove_same(const list_array<T, AnyAllocator>& val, size_t start = 0) &
            requires std::equality_comparable<T>
        {
            return remove_same(val, 0, val.size(), start, _size());
        }

        /**
         * @brief Removes all occurrences of another `list_array` from the array within a specified range.
         *
         * This function searches for all occurrences of the `list_array` `val` within the range from `start` to `end` and removes them.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `val`.
         * @param val The `list_array` to remove.
         * @param start The starting index of the range to search in this array (inclusive).
         * @param end The ending index of the range to search in this array (exclusive).
         * @return The number of elements removed.
         */
        template <class AnyAllocator>
        constexpr size_t remove_same(const list_array<T, AnyAllocator>& val, size_t start, size_t end) &
            requires std::equality_comparable<T>
        {
            return remove_same(val, 0, val.size(), start, end);
        }

        /**
         * @brief Removes all occurrences of another `list_array` from the array within a specified range.
         *
         * This function searches for all occurrences of the `list_array` `val` within the range from `start` to `end` and removes them.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `val`.
         * @param val The `list_array` to remove.
         * @param val_start The starting index within `val` to consider (inclusive).
         * @param val_end The ending index within `val` to consider (exclusive).
         * @param start The starting index of the range to search in this array (inclusive).
         * @param end The ending index of the range to search in this array (exclusive).
         * @return The number of elements removed.
         */
        template <class AnyAllocator>
        constexpr size_t remove_same(const list_array<T, AnyAllocator>& val, size_t val_start, size_t val_end, size_t start, size_t end) &
            requires std::equality_comparable<T>
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

/// @}
#pragma endregion
#pragma region find

        /// @name find
        /// @{
        /**
         * @brief Finds the index of the first occurrence of a specific value in the array.
         *
         * This function searches for the first element in the array that is equal to the given `it`.
         *
         * @param it The value to search for.
         * @return The index of the first occurrence of the value, or `npos` if not found.
         */
        [[nodiscard]] constexpr size_t find(const T& it) const&
            requires std::equality_comparable<T>
        {
            return find(0, size(), it);
        }

        /**
         * @brief Finds the index of the first occurrence of a specific value in the array, starting from a given position.
         *
         * This function searches for the first element in the array, starting from the specified `begin` index, that is equal to the given `it`.
         *
         * @param begin The starting index for the search.
         * @param it The value to search for.
         * @return The index of the first occurrence of the value, or `npos` if not found.
         */
        [[nodiscard]] constexpr size_t find(size_t begin, const T& it) const&
            requires std::equality_comparable<T>
        {
            return find(begin, size(), it);
        }

        /**
         * @brief Finds the index of the first occurrence of a specific value in the array within a specified range.
         *
         * This function searches for the first element in the array, within the range from `begin` to `end`, that is equal to the given `it`.
         *
         * @param begin The starting index of the range to search (inclusive).
         * @param end The ending index of the range to search (exclusive).
         * @param it The value to search for.
         * @return The index of the first occurrence of the value, or `npos` if not found.
         */
        [[nodiscard]] constexpr size_t find(size_t begin, size_t end, const T& it) const&
            requires std::equality_comparable<T>
        {
            auto _end = get_iterator(end);
            for (const_iterator iter = get_iterator(begin); iter != _end; ++iter)
                if (*iter == it)
                    return iter.absolute_index;
            return npos;
        }

        /**
         * @brief Finds the index of the first occurrence of a C-style array in the array.
         *
         * This function searches for the first occurrence of the C-style array `arr` in the array.
         *
         * @param arr The C-style array to search for.
         * @param arr_end A pointer to the end of the C-style array `arr`.
         * @return The index of the first occurrence of the C-style array, or `npos` if not found.
         */
        [[nodiscard]] constexpr size_t find(const T* arr, const T* arr_end) const&
            requires std::equality_comparable<T>
        {
            return find(0, arr, arr_end);
        }

        /**
         * @brief Finds the index of the first occurrence of a C-style array in the array, starting from a given position.
         *
         * This function searches for the first occurrence of the C-style array `arr` in the array, starting from the specified `begin` index.
         *
         * @param begin The starting index for the search.
         * @param arr The C-style array to search for.
         * @param arr_end A pointer to the end of the C-style array `arr`.
         * @return The index of the first occurrence of the C-style array, or `npos` if not found.
         */
        [[nodiscard]] constexpr size_t find(size_t begin, const T* arr, const T* arr_end) const&
            requires std::equality_comparable<T>
        {
            return find(begin, size(), arr, arr_end);
        }

        /**
         * @brief Finds the index of the first occurrence of a C-style array in the array within a specified range.
         *
         * This function searches for the first occurrence of the C-style array `arr` in the array, within the range from `begin` to `end`.
         *
         * @param begin The starting index of the range to search (inclusive).
         * @param end The ending index of the range to search (exclusive).
         * @param arr The C-style array to search for.
         * @param arr_end A pointer to the end of the C-style array `arr`.
         * @return The index of the first occurrence of the C-style array, or `npos` if not found.
         */
        [[nodiscard]] constexpr size_t find(size_t begin, size_t end, const T* arr, const T* arr_end) const&
            requires std::equality_comparable<T>
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

        /**
         * @brief Finds the index of the first occurrence of a C-style array in the array.
         *
         * This function searches for the first occurrence of the C-style array `arr` in the array.
         *
         * @tparam N The size of the C-style array `arr`.
         * @param arr The C-style array to search for.
         * @return The index of the first occurrence of the C-style array, or `npos` if not found.
         */
        template <size_t N>
        [[nodiscard]] constexpr size_t find(const T (&arr)[N]) const&
            requires std::equality_comparable<T>
        {
            return find(0, size(), arr, arr + N);
        }

        /**
         * @brief Finds the index of the first occurrence of a C-style array in the array, starting from a given position.
         *
         * This function searches for the first occurrence of the C-style array `arr` in the array, starting from the specified `begin` index.
         *
         * @tparam N The size of the C-style array `arr`.
         * @param begin The starting index for the search.
         * @param arr The C-style array to search for.
         * @return The index of the first occurrence of the C-style array, or `npos` if not found.
         */
        template <size_t N>
        [[nodiscard]] constexpr size_t find(size_t begin, const T (&arr)[N]) const&
            requires std::equality_comparable<T>
        {
            return find(begin, size(), arr, arr + N);
        }

        /**
         * @brief Finds the index of the first occurrence of a C-style array in the array within a specified range.
         *
         * This function searches for the first occurrence of the C-style array `arr` in the array, within the range from `begin` to `_end`.
         *
         * @tparam N The size of the C-style array `arr`.
         * @param begin The starting index of the range to search (inclusive).
         * @param _end The ending index of the range to search (exclusive).
         * @param arr The C-style array to search for.
         * @return The index of the first occurrence of the C-style array, or `npos` if not found.
         */
        template <size_t N>
        [[nodiscard]] constexpr size_t find(size_t begin, size_t _end, const T (&arr)[N]) const&
            requires std::equality_comparable<T>
        {
            return find(begin, _end, arr, arr + N);
        }

        /**
         * @brief Finds the index of the first occurrence of a specific value in the array.
         *
         * This function searches for the first element in the array, within the specified range (`begin` to `size()`), that is equal to the given `it`.
         *
         * @param begin An iterator to the beginning of the range to search.
         * @param it The value to search for.
         * @return The index of the first occurrence of the value, or `npos` if not found.
         */
        template <class any_iter>
        [[nodiscard]] constexpr size_t find(any_iter extern_begin, any_iter extern_end) const&
            requires std::equality_comparable<T>
        {
            return find(0, size(), extern_begin, extern_end);
        }

        /**
         * @brief Finds the index of the first matching subsequence from a given starting index.
         * @param begin The starting index to search from (inclusive).
         * @param extern_begin An iterator to the beginning of the subsequence to find.
         * @param extern_end An iterator to the end of the subsequence to find.
         * @return size_t The index of the first matching subsequence, or `npos` if not found.
         */
        template <class any_iter>
        [[nodiscard]] constexpr size_t find(size_t begin, any_iter extern_begin, any_iter extern_end) const&
            requires std::equality_comparable<T>
        {
            return find(begin, size(), extern_begin, extern_end);
        }

        /**
         * @brief Finds the index of the first matching subsequence within a given range.
         * @param begin The starting index to search from (inclusive).
         * @param end The ending index to search to (exclusive).
         * @param extern_begin An iterator to the beginning of the subsequence to find.
         * @param extern_end An iterator to the end of the subsequence to find.
         * @return size_t The index of the first matching subsequence, or `npos` if not found.
         */
        template <class any_iter>
        [[nodiscard]] constexpr size_t find(size_t begin, size_t end, any_iter extern_begin, any_iter extern_end) const&
            requires std::equality_comparable<T>
        {
            //check if distance of any_iter can be calculated
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

        /**
         * @brief Finds the index of the first occurrence of another `list_array` in the array.
         *
         * This function searches for the first occurrence of the `list_array` `arr` in the array.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `arr`.
         * @param arr The `list_array` to search for.
         * @return The index of the first occurrence of the `list_array`, or `npos` if not found.
         */
        template <class AnyAllocator>
        [[nodiscard]] constexpr size_t find(const list_array<T, AnyAllocator>& arr) const&
            requires std::equality_comparable<T>
        {
            return find(0, size(), arr.begin(), arr.end());
        }

        /**
         * @brief Finds the index of the first occurrence of another `list_array` in the array, starting from a given position and within a specified range.
         *
         * This function searches for the first occurrence of the `list_array` `arr` in the array, within the range from `begin` to `end`, starting the search from the specified `begin` index.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `arr`.
         * @param begin The starting index for the search.
         * @param arr The `list_array` to search for.
         * @return The index of the first occurrence of the `list_array`, or `npos` if not found.
         */
        template <class AnyAllocator>
        [[nodiscard]] constexpr size_t find(size_t begin, const list_array<T, AnyAllocator>& arr) const&
            requires std::equality_comparable<T>
        {
            return find(begin, size(), arr.begin(), arr.end());
        }

        /**
         * @brief Finds the index of the first occurrence of another `list_array` in the array, starting from a given position and within a specified range.
         *
         * This function searches for the first occurrence of the `list_array` `arr` in the array, within the range from `begin` to `end`, starting the search from the specified `begin` index.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `arr`.
         * @param begin The starting index for the search.
         * @param end The ending index of the range to search (exclusive).
         * @param arr The `list_array` to search for.
         * @return The index of the first occurrence of the `list_array`, or `npos` if not found.
         */
        template <class AnyAllocator>
        [[nodiscard]] constexpr size_t find(size_t begin, size_t _end, const list_array<T, AnyAllocator>& arr) const&
            requires std::equality_comparable<T>
        {
            return find(begin, _end, arr.begin(), arr.end());
        }

        /**
         * @brief Finds the index of the first element that satisfies a given predicate.
         *
         * This function searches for the first element in the array that satisfies the predicate `fn`.
         *
         * @tparam FN The type of the predicate function. It must be invocable with either `const T&` or `size_t, const T&` (index and element).
         * @param fn The predicate function to apply to each element.
         * @return The index of the first element that satisfies the predicate, or `npos` if not found.
         */
        template <class FN>
        [[nodiscard]] constexpr size_t find_if(FN&& fn) const&
            requires std::is_invocable_r_v<bool, FN, size_t, T&> || std::is_invocable_r_v<bool, FN, T&>
        {
            return find_if(0, size(), std::forward<FN>(fn));
        }

        /**
         * @brief Finds the index of the first element that satisfies a given predicate, starting from a given position.
         *
         * This function searches for the first element in the array, starting from the specified `begin` index, that satisfies the predicate `fn`.
         *
         * @tparam FN The type of the predicate function.
         * @param begin The starting index for the search.
         * @param fn The predicate function to apply to each element.
         * @return The index of the first element that satisfies the predicate, or `npos` if not found.
         */
        template <class FN>
        [[nodiscard]] constexpr size_t find_if(size_t begin, FN&& fn) const&
            requires std::is_invocable_r_v<bool, FN, size_t, T&> || std::is_invocable_r_v<bool, FN, T&>
        {
            return find_if(begin, size(), std::forward<FN>(fn));
        }

        /**
         * @brief Finds the index of the first element that satisfies a given predicate within a specified range.
         *
         * This function searches for the first element in the array, within the range from `begin` to `end`, that satisfies the predicate `fn`.
         *
         * @tparam FN The type of the predicate function.
         * @param begin The starting index of the range to search (inclusive).
         * @param end The ending index of the range to search (exclusive).
         * @param fn The predicate function to apply to each element.
         * @return The index of the first element that satisfies the predicate, or `npos` if not found.
         */
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

/// @}
#pragma endregion
#pragma region split

        /// @name split
        /// @{

        /**
         * @brief Splits the array at the specified position.
         *
         * This function splits the array into two at the given `split_pos`. The elements from `split_pos` to the end are moved into a new `list_array`, and the original array is resized to contain only the elements up to `split_pos`.
         *
         * @param split_pos The position at which to split the array.
         * @return A new `list_array` containing the elements from `split_pos` to the end of the original array.
         * @throws std::out_of_range If `split_pos` is greater than or equal to the size of the array.
         */
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

        /**
         * @brief Splits the array at the specified position (move version).
         *
         * This function is similar to the lvalue reference version of `split`, but it operates on an rvalue reference (temporary) of the `list_array`. It first moves the temporary array into a new `list_array` object, then calls the lvalue reference version of `split` on the new object, and finally returns a pair containing the split arrays.
         *
         * @param split_pos The position at which to split the array.
         * @return A pair of `list_array` objects. The first contains the elements up to `split_pos`, and the second contains the elements from `split_pos` to the end.
         */
        [[nodiscard]] constexpr std::pair<list_array<T, Allocator>, list_array<T, Allocator>> split(size_t split_pos) && {
            list_array<T, Allocator> tmp = take();
            return {tmp, tmp.split()};
        }

        /**
         * @brief Splits the array into multiple subarrays based on a delimiter value.
         *
         * This function splits the array into multiple subarrays whenever it encounters the specified `split_value`. The resulting subarrays are stored in a new `list_array` of `list_array` objects.
         *
         * @tparam InnerAllocator The allocator type for the inner `list_array` objects (defaults to `std::allocator<list_array<T, Allocator>>`).
         * @param split_value The delimiter value at which to split the array.
         * @return A new `list_array` containing the split subarrays.
         */
        template <class InnerAllocator = std::allocator<list_array<T, Allocator>>>
        [[nodiscard]] constexpr list_array<list_array<T, Allocator>, InnerAllocator> split_by(const T& split_value) {
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

        /**
         * @brief Splits the array into multiple subarrays based on delimiter values in a C-style array.
         *
         * This function is similar to `split_by(const T&)`, but it takes a C-style array of delimiter values.
         *
         * @tparam arr_size The size of the C-style array `split_values`.
         * @tparam InnerAllocator The allocator type for the inner `list_array` objects.
         * @param split_values The C-style array containing delimiter values.
         * @return A new `list_array` containing the split subarrays.
         */
        template <size_t arr_size, class InnerAllocator = std::allocator<list_array<T, Allocator>>>
        [[nodiscard]] constexpr list_array<list_array<T, Allocator>, InnerAllocator> split_by(const T (&split_values)[arr_size]) {
            return split_by(split_values, arr_size);
        }

        /**
         * @brief Splits the array into multiple subarrays based on delimiter values in a raw array.
         *
         * This function is similar to `split_by(const T&)`, but it takes a raw pointer to an array of delimiter values and its size.
         *
         * @tparam InnerAllocator The allocator type for the inner `list_array` objects.
         * @param split_values A pointer to the array containing delimiter values.
         * @param split_values_size The size of the array of delimiter values.
         * @return A new `list_array` containing the split subarrays.
         */
        template <class InnerAllocator = std::allocator<list_array<T, Allocator>>>
        [[nodiscard]] constexpr list_array<list_array<T, Allocator>, InnerAllocator> split_by(const T* split_values, size_t split_values_size) {
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

        /**
         * @brief Splits the array into multiple subarrays based on delimiter values in another `list_array`.
         *
         * This function is similar to `split_by(const T&)`, but it takes another `list_array` containing delimiter values.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` containing delimiter values.
         * @tparam InnerAllocator The allocator type for the inner `list_array` objects.
         * @param split_values The `list_array` containing delimiter values.
         * @return A new `list_array` containing the split subarrays.
         */
        template <class AnyAllocator, class InnerAllocator = std::allocator<list_array<T, AnyAllocator>>>
        [[nodiscard]] constexpr list_array<list_array<T, AnyAllocator>, InnerAllocator> split_by(const list_array<T, AnyAllocator>& split_values) {
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

        /**
         * @brief Splits the array into multiple subarrays based on a predicate.
         *
         * This function splits the array into multiple subarrays whenever the predicate `split_function` returns `true` for an element. The resulting subarrays are stored in a new `list_array` of `list_array` objects.
         *
         * @tparam FN The type of the predicate function. It must be invocable with either `const T&` or `size_t, const T&` (index and element).
         * @tparam InnerAllocator The allocator type for the inner `list_array` objects (defaults to `std::allocator<list_array<T, Allocator>>`).
         * @param split_function The predicate function to determine split points.
         * @return A new `list_array` containing the split subarrays.
         */
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

/// @}
#pragma endregion
#pragma region take

        /// @name take
        /// @{
        /**
         * @brief Takes ownership of the entire list_array.
         *
         * This function transfers ownership of the list_array to the caller, leaving the original list_array empty.
         *
         * @return A new list_array object that takes ownership of the original list_array's elements.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> take() {
            return std::move(*this);
        }

        /**
         * @brief Takes an element from the array at the specified position.
         *
         * This function removes the element at the given `take_pos` from the array and returns it by value.
         *
         * @param take_pos The index of the element to take.
         * @return The element at the specified position.
         * @throws std::out_of_range If `take_pos` is greater than or equal to the size of the array.
         */
        [[nodiscard]] constexpr T take(size_t take_pos) {
            if (_size() <= take_pos)
                throw std::out_of_range("Fail take item due small array");
            T res(std::move(operator[](take_pos)));
            erase(take_pos);
            return res;
        }

        /**
         * @brief Extracts the raw data array from the list_array.
         *
         * This function returns a pointer to the underlying data array and sets the `size` parameter to the number of elements in the array. After calling this function, the list_array will be empty.
         *
         * @param[out] size A reference to a size_t variable that will be set to the number of elements in the array.
         * @return T* A pointer to the raw data array.
         */
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

        /**
         * @brief Takes elements from the array within a specified range.
         *
         * This function creates a new list_array object containing only the elements from the original array within the range [`start_pos`, `end_pos`).
         *
         * @param start_pos The starting index of the range (inclusive).
         * @param end_pos The ending index of the range (exclusive).
         * @return A new list_array object containing the elements within the specified range.
         * @throws std::invalid_argument If `end_pos` is less than `start_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
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

        /**
         * @brief Creates a new list_array object by taking elements from this list_array based on a predicate.
         *
         * This function is similar to the three-argument version of `take`, but it takes elements from the beginning to the end of the array.
         *
         * @tparam FN The type of the predicate function. It must be invocable with either `const T&` or `size_t, const T&` (index, element).
         * @param select_fn The predicate function to apply to each element.
         * @return A new list_array object containing the taken elements.
         */
        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> take(FN&& select_fn)
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            return take(std::forward<FN>(select_fn), 0, _size());
        }

        /**
         * @brief Creates a new list_array object by taking elements from this list_array based on a predicate, starting from a given position.
         *
         * This function is similar to the three-argument version of `take`, but it takes elements from `start_pos` to the end of the array.
         *
         * @tparam FN The type of the predicate function. It must be invocable with either `const T&` or `size_t, const T&` (index, element).
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param select_fn The predicate function to apply to each element.
         * @return A new list_array object containing the taken elements.
         */
        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> take(size_t start_pos, FN&& select_fn)
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            return take(std::forward<FN>(select_fn), start_pos, _size());
        }

        /**
         * @brief Creates a new list_array object by taking elements from this list_array based on a predicate within a specified range.
         *
         * This function iterates over the elements in the specified range (`start_pos` to `end_pos`) and applies the given predicate `select_fn` to each element. The elements for which the predicate returns true are moved into a new list_array object, and then removed from the original list_array.
         *
         * @tparam FN The type of the predicate function. It must be invocable with either `const T&` or `size_t, const T&` (index, element).
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @param select_fn The predicate function to apply to each element.
         * @return A new list_array object containing the taken elements.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> take(size_t start_pos, size_t end_pos, FN&& select_fn)
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
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
                        if (first)
                            first_selection = i;
                        selector.set(i, true);
                        res.push_back(std::move(it));
                        last_selection = i;
                    }
                } else {
                    if (select_fn(it)) {
                        if (first)
                            first_selection = i;
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

/// @}
#pragma endregion
#pragma region copy/swap

        /// @name copy/swap
        /// @{
        /**
         * @brief Creates a copy of a subrange of the `list_array`.
         *
         * This function creates a new list_array object by copying elements from the original list_array, within the range from `start_pos` to `end_pos`.
         *
         * @param start_pos The starting index (inclusive) of the elements to copy.
         * @param end_pos The ending index (exclusive) of the elements to copy.
         * @return list_array<T, Allocator> A new `list_array` object containing the copied elements.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> copy(size_t start_pos, size_t end_pos) const
            requires std::copy_constructible<T>
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

        /**
         * @brief Creates a copy of a subrange of the `list_array`, starting from `start_pos` to the end.
         *
         * @param start_pos The starting index (inclusive) of the elements to copy.
         * @return list_array<T, Allocator> A new `list_array` object containing the copied elements.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> copy(size_t start_pos) const
            requires std::copy_constructible<T>
        {
            return copy(start_pos, _size());
        }

        /**
         * @brief Creates a copy of the entire `list_array`.
         *
         * @return list_array<T, Allocator> A new `list_array` object that is a copy of the original.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> copy() const
            requires std::copy_constructible<T>
        {
            return *this;
        }

        /**
         * @brief Swaps the contents of this `list_array` with another `list_array`.
         * 
         * This operation is guaranteed to be noexcept if the element type `T` is noexcept destructible.
         *
         * @param to_swap The `list_array` to swap contents with.
         * @return list_array<T, Allocator>& A reference to this `list_array` after the swap.
         */
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

/// @}
#pragma endregion
#pragma region remove duplicates

        /// @name remove duplicates
        /// @{
        /**
         * @brief Removes consecutive duplicate elements from the array.
         *
         * This function modifies the array in-place, removing consecutive duplicate elements within the entire array. It uses the default equality comparison operator (`==`) to determine duplicates.
         *
         * @return The number of elements removed.
         */
        constexpr size_t unique() &
            requires std::equality_comparable<T>
        {
            return unique(0, _size());
        }

        /**
         * @brief Removes consecutive duplicate elements from the array, starting from a specified position.
         *
         * This function modifies the array in-place, removing consecutive duplicate elements starting from the specified `start_pos`. It uses the default equality comparison operator (`==`) to determine duplicates.
         *
         * @param start_pos The starting index from which to remove duplicates.
         * @return The number of elements removed.
         */
        constexpr size_t unique(size_t start_pos) &
            requires std::equality_comparable<T>
        {
            return unique(start_pos, _size());
        }

        /**
         * @brief Removes consecutive duplicate elements from the array.
         *
         * This function modifies the array in-place, removing consecutive duplicate elements within the specified range (`start_pos` to `end_pos`). It uses the default equality comparison operator (`==`) to determine duplicates.
         *
         * @param start_pos The starting index from which to remove duplicates.
         * @param end_pos The ending index of the range to consider (exclusive).
         * @return The number of elements removed.
         */
        constexpr size_t unique(size_t start_pos, size_t end_pos) &
            requires std::equality_comparable<T>
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

        /**
         * @brief Removes consecutive duplicate elements from the array using a custom comparison function.
         *
         * This function modifies the array in-place, removing consecutive duplicate elements within the entire array. It allows you to provide a custom comparison function (`compare_func`) to determine duplicates.
         *
         * @tparam FN The type of the comparison function. It should take two const references to `T` and return `true` if they are considered duplicates, `false` otherwise.
         * @param compare_func The custom comparison function to use.
         * @return The number of elements removed.
         */
        template <class FN>
        constexpr size_t unique(FN&& compare_func) &
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            return unique(0, _size(), std::forward<FN>(compare_func));
        }

        /**
         * @brief Removes consecutive duplicate elements from the array, starting from a specified position, using a custom comparison function.
         *
         * This function modifies the array in-place, removing consecutive duplicate elements starting from the specified `start_pos`. It uses the provided `compare_func` to determine duplicates.
         *
         * @tparam FN The type of the comparison function. It should take two const references to `T` and return `true` if they are considered duplicates, `false` otherwise.
         * @param start_pos The starting index from which to remove duplicates.
         * @param compare_func The custom comparison function to use.
         * @return The number of elements removed.
         */
        template <class FN>
        constexpr size_t unique(size_t start_pos, FN&& compare_func) &
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            return unique(start_pos, _size(), std::forward<FN>(compare_func));
        }

        /**
         * @brief Removes consecutive duplicate elements from the array using a custom comparison function.
         *
         * This function is similar to `unique(size_t, size_t)`, but it allows you to provide a custom comparison function (`compare_func`) to determine duplicates.
         *
         * @tparam FN The type of the comparison function. It should take two const references to `T` and return `true` if they are considered duplicates, `false` otherwise.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @param compare_func The custom comparison function to use.
         * @return The number of elements removed.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
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

        /**
         * @brief Removes duplicate elements from the array, leaving only unique elements.
         *
         * This function modifies the array in-place, removing duplicate elements within the entire array. It uses the default equality comparison operator (`==`) to determine duplicates. The order of the remaining unique elements is not guaranteed to be preserved.
         *
         * @return The number of elements removed.
         */
        constexpr size_t unify() &
            requires std::equality_comparable<T> && std::copy_constructible<T>
        {
            return unify(0, _size());
        }

        /**
             * @brief Removes duplicate elements from the array, leaving only unique elements, starting from a specified position.
             *
             * This function modifies the array in-place, removing duplicate elements starting from the specified `start_pos`. It uses the default equality comparison operator (`==`) to determine duplicates. The order of the remaining unique elements is not guaranteed to be preserved.
             *
             * @param start_pos The starting index from which to remove duplicates.
             * @return The number of elements removed.
                 */
        constexpr size_t unify(size_t start_pos) &
            requires std::equality_comparable<T> && std::copy_constructible<T>
        {
            return unify(start_pos, _size());
        }

        /**
             * @brief Removes duplicate elements from the array, leaving only unique elements.
             *
             * This function modifies the array in-place, removing duplicate elements within the specified range (`start_pos` to `end_pos`). It uses the default equality comparison operator (`==`) to determine duplicates. The order of the remaining unique elements is not guaranteed to be preserved.
             *
             * @param start_pos The starting index of the range to consider (inclusive).
             * @param end_pos The ending index of the range to consider (exclusive).
             * @return The number of elements removed.
             * @throws std::out_of_range If `end_pos` exceeds the size of the array.
                 */
        constexpr size_t unify(size_t start_pos, size_t end_pos) &
            requires std::equality_comparable<T> && std::copy_constructible<T>
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

        /**
             * @brief Returns the number of elements with unique values.
             *
             * This function iterates over the elements within the entire array and counts the number of elements that have unique values (i.e., they appear only once in the array).
             *
             * @return The number of elements with unique values within the entire array.
                 */
        constexpr size_t alone() &
            requires std::equality_comparable<T>
        {
            return alone(0, _size());
        }

        /**
         * @brief Returns the number of elements with unique values, starting from a specified position.
         *
         * This function iterates over the elements starting from the specified `start_pos` and counts the number of elements that have unique values (i.e., they appear only once in the array from that position onwards).
         *
         * @param start_pos The starting index from which to consider unique values.
         * @return The number of elements with unique values within the array, starting from `start_pos`.
         */
        constexpr size_t alone(size_t start_pos) &
            requires std::equality_comparable<T>
        {
            return alone(start_pos, _size());
        }

        /**
         * @brief Returns the number of deleted elements.
         *
         * This function iterates over the elements within the specified range (`start_pos` to `end_pos`) and counts the number of elements that have unique values (i.e., they appear only once in the range).
         *
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @return The number of elements with unique values within the specified range.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
        constexpr size_t alone(size_t start_pos, size_t end_pos) &
            requires std::equality_comparable<T>
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
                [&selector, &i](const T& check_it) {
                    return selector.get(i++);
                }
            );
            return result;
        }

        /**
         * @brief Removes consecutive duplicate elements from the array (move version).
         *
         * This function is similar to the lvalue reference version of `unique`, but it operates on an rvalue reference (temporary) of the `list_array`. It removes all occurrences of the given `val` and then returns the modified array by value.
         *
         * @param val The value to remove.
         * @return A new `list_array` object with all occurrences of `val` removed.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> unique() &&
            requires std::equality_comparable<T>
        {
            unique(0, _size());
            return std::move(*this);
        }

        /**
         * @brief Removes consecutive duplicate elements from the array (move version), starting from a specified position.
         *
         * This function is similar to the lvalue reference version of `unique(size_t)`, but it operates on an rvalue reference (temporary) of the `list_array`. It removes all occurrences of the given `val`, starting from `start_pos`, and then returns the modified array by value.
         *
         * @param start_pos The starting index from which to remove duplicates.
         * @return A new `list_array` object with all occurrences of `val` removed, starting from `start_pos`.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> unique(size_t start_pos) &&
            requires std::equality_comparable<T>
        {
            unique(start_pos, _size());
            return std::move(*this);
        }

        /**
         * @brief Removes consecutive duplicate elements from the array (move version), starting from a specified position.
         *
         * This function is similar to the lvalue reference version of `unique(size_t)`, but it operates on an rvalue reference (temporary) of the `list_array`. It removes all occurrences of the given `val`, starting from `start_pos`, and then returns the modified array by value.
         *
         * @param start_pos The starting index from which to remove duplicates.
         * @return A new `list_array` object with all occurrences of `val` removed, starting from `start_pos`.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> unique(size_t start_pos, size_t end_pos) &&
            requires std::equality_comparable<T>
        {
            unique(start_pos, end_pos);
            return std::move(*this);
        }

        /**
         * @brief Removes consecutive duplicate elements from the array (move version) using a custom comparison function.
         *
         * This function is similar to the lvalue reference version of `unique(size_t, size_t, FN&&)`, but it operates on an rvalue reference (temporary) of the `list_array`. It removes consecutive duplicate elements within the specified range (`start_pos` to `end_pos`) using the provided comparison function and then returns the modified array by value.
         *
         * @tparam FN The type of the comparison function. It should take two const references to `T` and return `true` if they are considered duplicates, `false` otherwise.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @param compare_func The custom comparison function to use.
         * @return A new `list_array` object with consecutive duplicate elements removed within the specified range.
         */
        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> unique(FN&& compare_func) &&
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            unique(0, _size(), std::forward<FN>(compare_func));
            return std::move(*this);
        }

        /**
         * @brief Removes consecutive duplicate elements from the array (move version) using a custom comparison function.
         *
         * This function is similar to the lvalue reference version of `unique(size_t, size_t, FN&&)`, but it operates on an rvalue reference (temporary) of the `list_array`. It removes consecutive duplicate elements within the specified range (`start_pos` to `end_pos`) using the provided comparison function and then returns the modified array by value.
         *
         * @tparam FN The type of the comparison function. It should take two const references to `T` and return `true` if they are considered duplicates, `false` otherwise.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param compare_func The custom comparison function to use.
         * @return A new `list_array` object with consecutive duplicate elements removed within the specified range.
         */
        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> unique(size_t start_pos, FN&& compare_func) &&
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            unique(start_pos, _size(), std::forward<FN>(compare_func));
            return std::move(*this);
        }

        /**
         * @brief  Removes duplicate elements from a specified range within the list_array, based on a comparison function (move version).
         *
         * @tparam FN The type of the comparison function. It should take two const references to `T` and return `true` if the elements are considered equal, and `false` otherwise.
         * @param start_pos The starting index of the range to process (inclusive).
         * @param end_pos The ending index of the range to process (exclusive).
         * @param compare_func The comparison function to determine element equality.
         * @return A new list_array object with duplicates removed.
         */
        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> unique(size_t start_pos, size_t end_pos, FN&& compare_func) &&
            requires std::is_invocable_r_v<bool, FN, const T&, const T&>
        {
            unique(end_pos, _size(), std::forward<FN>(compare_func));
            return std::move(*this);
        }

        /**
         * @brief Removes duplicate elements from the array (move version), leaving only unique elements.
         *
         * This function is similar to the lvalue reference version of `unify`, but it operates on an rvalue reference (temporary) of the `list_array`. It removes duplicate elements within the entire array and then returns the modified array by value. The order of the remaining unique elements is not guaranteed to be preserved.
         *
         * @return A new `list_array` object with all duplicate elements removed.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> unify() &&
            requires std::equality_comparable<T> && std::copy_constructible<T>
        {
            unify(0, _size());
            return std::move(*this);
        }

        /**
             * @brief Removes duplicate elements from the array (move version), leaving only unique elements, starting from a specified position.
             *
             * This function is similar to the lvalue reference version of `unify(size_t)`, but it operates on an rvalue reference (temporary) of the `list_array`. It removes duplicate elements starting from the specified `start_pos` and then returns the modified array by value. The order of the remaining unique elements is not guaranteed to be preserved.
             *
             * @param start_pos The starting index from which to remove duplicates.
             * @return A new `list_array` object with all duplicate elements removed, starting from `start_pos`.
                 */
        [[nodiscard]] constexpr list_array<T, Allocator> unify(size_t start_pos) &&
            requires std::equality_comparable<T> && std::copy_constructible<T>
        {
            unify(start_pos, _size());
            return std::move(*this);
        }

        /**
             * @brief Removes duplicate elements from the array (move version), leaving only unique elements within a specified range.
             *
             * This function is similar to the lvalue reference version of `unify(size_t, size_t)`, but it operates on an rvalue reference (temporary) of the `list_array`. It removes duplicate elements within the specified range (`start_pos` to `end_pos`) and then returns the modified array by value. The order of the remaining unique elements is not guaranteed to be preserved.
             *
             * @param start_pos The starting index of the range to consider (inclusive).
             * @param end_pos The ending index of the range to consider (exclusive).
             * @return A new `list_array` object with all duplicate elements removed within the specified range.
                 */
        [[nodiscard]] constexpr list_array<T, Allocator> unify(size_t start_pos, size_t end_pos) &&
            requires std::equality_comparable<T> && std::copy_constructible<T>
        {
            unify(start_pos, end_pos);
            return std::move(*this);
        }

        /**
             * @brief Removes elements with unique values from the array (move version).
             *
             * This function is similar to the lvalue reference version of `alone`, but it operates on an rvalue reference (temporary) of the `list_array`. It removes elements with unique values within the entire array and then returns the modified array by value.
             *
             * @return A new `list_array` object with all elements with unique values removed.
                 */
        [[nodiscard]] constexpr list_array<T, Allocator> alone() &&
            requires std::equality_comparable<T>
        {
            alone(0, _size());
            return std::move(*this);
        }

        /**
         * @brief Removes elements with unique values from the array (move version), starting from a specified position.
         *
         * This function is similar to the lvalue reference version of `alone(size_t)`, but it operates on an rvalue reference (temporary) of the `list_array`. It removes elements with unique values starting from the specified `start_pos` and then returns the modified array by value.
         *
         * @param start_pos The starting index from which to consider unique values.
         * @return A new `list_array` object with all elements with unique values removed, starting from `start_pos`.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> alone(size_t start_pos) &&
            requires std::equality_comparable<T>
        {
            alone(start_pos, _size());
            return std::move(*this);
        }

        /**
         * @brief Removes elements with unique values from the array (move version) within a specified range.
         *
         * This function is similar to the lvalue reference version of `alone(size_t, size_t)`, but it operates on an rvalue reference (temporary) of the `list_array`. It removes elements with unique values within the specified range (`start_pos` to `end_pos`) and then returns the modified array by value.
         *
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @return A new `list_array` object with all elements with unique values removed within the specified range.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> alone(size_t start_pos, size_t end_pos) &&
            requires std::equality_comparable<T>
        {
            alone(start_pos, end_pos);
            return std::move(*this);
        }

/// @}
#pragma endregion
#pragma region join

        /// @name join
        /// @{

        /**
         * @brief Joins elements of the array with an item or other array, based on a condition.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element where the `where_join` predicate returns true, it inserts the `insert_item` after the element. The modified elements, along with the inserted items, are stored in this `list_array` object.
         *
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_item The item to insert after elements where the predicate is true.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @param where_join The predicate function to determine where to insert the `insert_item`.
         * @return A reference to this `list_array` after the join operation.
         */
        template <class FN>
        constexpr list_array<T, Allocator>& join(const T& insert_item, FN&& where_join) &
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_item, 0, _size(), std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with an item or other array, based on a condition.
         *
         * This function iterates over the elements in the array, starting from the specified `start_pos`. For each element where the `where_join` predicate returns true, it inserts the `insert_item` after the element. The modified elements, along with the inserted items, are stored in this `list_array` object.
         *
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_item The item to insert after elements where the predicate is true.
         * @param start_pos The starting index from which to insert the `insert_item`.
         * @param where_join The predicate function to determine where to insert the `insert_item`.
         * @return A reference to this `list_array` after the join operation.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
        template <class FN = bool (*)(const T&)>
        constexpr list_array<T, Allocator>& join(const T& insert_item, size_t start_pos, FN&& where_join) &
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_item, start_pos, _size(), std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with an item or other array, based on a condition.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element where the `where_join` predicate returns true, it inserts the `insert_item` after the element. The modified elements, along with the inserted items, are stored in this `list_array` object.
         *
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_item The item to insert after elements where the predicate is true.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @param where_join The predicate function to determine where to insert the `insert_item`.
         * @return A reference to this `list_array` after the join operation.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
        template <class FN>
        constexpr list_array<T, Allocator>& join(const T& insert_item, size_t start_pos, size_t end_pos, FN&& where_join) &
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_item, start_pos, end_pos, std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with items from another array, based on a condition.
         *
         * This function iterates over all elements in the array. For each element where the `where_join` predicate returns true, it inserts the elements from `insert_items` after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `insert_items`.
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_items The `list_array` containing the items to insert after elements where the predicate is true.
         * @param where_join The predicate function to determine where to insert the items from `insert_items`.
         * @return A new `list_array` object containing the joined elements.
         */
        template <class AnyAllocator, class FN>
        constexpr list_array<T, Allocator>& join(const list_array<T, AnyAllocator>& insert_items, FN&& where_join) &
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_items, 0, _size(), std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with items from another array, based on a condition.
         *
         * This function iterates over the elements in the array, starting from the specified `start_pos`. For each element where the `where_join` predicate returns true, it inserts the elements from `insert_items` after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `insert_items`.
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_items The `list_array` containing the items to insert after elements where the predicate is true.
         * @param start_pos The starting index from which to insert the items from `insert_items`.
         * @param where_join The predicate function to determine where to insert the items from `insert_items`.
         * @return A new `list_array` object containing the joined elements.
         * @throws std::invalid_argument If `start_pos` is greater than size of the array.
         */
        template <class AnyAllocator, class FN>
        constexpr list_array<T, Allocator>& join(const list_array<T, AnyAllocator>& insert_items, size_t start_pos, FN&& where_join) &
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_items, start_pos, _size(), std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with items from another array, based on a condition.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element where the `where_join` predicate returns true, it inserts the elements from `insert_items` after the element. The modified elements, along with the inserted items, are stored in this `list_array` object.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `insert_items`.
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_items The `list_array` containing the items to insert after elements where the predicate is true.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @param where_join The predicate function to determine where to insert the items from `insert_items`.
         * @return A reference to this `list_array` after the join operation.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
        template <class AnyAllocator, class FN>
        constexpr list_array<T, Allocator>& join(const list_array<T, AnyAllocator>& insert_items, size_t start_pos, size_t end_pos, FN&& where_join) &
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_items, start_pos, end_pos, std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with items from a C-style array, based on a condition.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element where the `where_join` predicate returns true, it inserts the elements from the C-style array `insert_items` (with size `arr_size`) after the element. The modified elements, along with the inserted items, are stored in this `list_array` object.
         *
         * @tparam arr_size The size of the C-style array `insert_items`.
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_items The C-style array containing the items to insert after elements where the predicate is true.
         * @param where_join The predicate function to determine where to insert the items from `insert_items`.
         * @return A reference to this `list_array` after the join operation.
         */
        template <size_t arr_size, class FN>
        constexpr list_array<T, Allocator>& join(const T (&insert_items)[arr_size], FN&& where_join) &
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_items, arr_size, 0, _size(), std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with items from a C-style array, based on a condition.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element where the `where_join` predicate returns true, it inserts the elements from the C-style array `insert_items` (with size `arr_size`) after the element. The modified elements, along with the inserted items, are stored in this `list_array` object.
         *
         * @tparam arr_size The size of the C-style array `insert_items`.
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_items The C-style array containing the items to insert after elements where the predicate is true.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param where_join The predicate function to determine where to insert the items from `insert_items`.
         * @return A reference to this `list_array` after the join operation.
         * @throws std::invalid_argument If `start_pos` is greater than  size of the array.
         */
        template <size_t arr_size, class FN>
        constexpr list_array<T, Allocator>& join(const T (&insert_items)[arr_size], size_t start_pos, FN&& where_join) &
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_items, arr_size, start_pos, _size(), std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with items from a C-style array, based on a condition.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element where the `where_join` predicate returns true, it inserts the elements from the C-style array `insert_items` (with size `arr_size`) after the element. The modified elements, along with the inserted items, are stored in this `list_array` object.
         *
         * @tparam arr_size The size of the C-style array `insert_items`.
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_items The C-style array containing the items to insert after elements where the predicate is true.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @param where_join The predicate function to determine where to insert the items from `insert_items`.
         * @return A reference to this `list_array` after the join operation.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
        template <size_t arr_size, class FN>
        constexpr list_array<T, Allocator>& join(const T (&insert_items)[arr_size], size_t start_pos, size_t end_pos, FN&& where_join) &
            requires std::copy_constructible<T>
        {
            return *this = std::move(*this).join(insert_items, arr_size, start_pos, end_pos, std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with items from a raw array, based on a condition.
         *
         * This function iterates over the elements in the array. For each element where the `where_join` predicate returns true, it inserts the elements from the raw array `insert_items` (with `items_count` elements) after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_items A pointer to the beginning of the raw array containing the items to insert.
         * @param items_count The number of items in the raw array `insert_items`.
         * @param where_join The predicate function to determine where to insert the items from `insert_items`.
         * @return A new `list_array` object containing the joined elements.
         */
        template <class FN>
        constexpr list_array<T, Allocator>& join(const T* insert_items, size_t items_count, FN&& where_join) &
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_items, items_count, 0, _size(), std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with items from a raw array, based on a condition.
         *
         * This function iterates over the elements in the array, starting from the specified `start_pos`. For each element where the `where_join` predicate returns true, it inserts the elements from the raw array `insert_items` (with `items_count` elements) after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_items A pointer to the beginning of the raw array containing the items to insert.
         * @param items_count The number of items in the raw array `insert_items`.
         * @param start_pos The index where the insertion starts (inclusive).
         * @return A new `list_array` object containing the joined elements.
         * @throws std::invalid_argument If `start_pos` is greater than size of the array.
         */
        template <class FN>
        constexpr list_array<T, Allocator>& join(const T* insert_items, size_t items_count, size_t start_pos, FN&& where_join) &
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_items, items_count, start_pos, _size(), std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with items from a raw array, based on a condition.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element where the `where_join` predicate returns true, it inserts the elements from the raw array `insert_items` (with `items_count` elements) after the element. The modified elements, along with the inserted items, are stored in this `list_array` object.
         *
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_items A pointer to the beginning of the raw array containing the items to insert.
         * @param items_count The number of items in the raw array `insert_items`.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @param where_join The predicate function to determine where to insert the items from `insert_items`.
         * @return A reference to this `list_array` after the join operation.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
        template <class FN>
        constexpr list_array<T, Allocator>& join(const T* insert_items, size_t items_count, size_t start_pos, size_t end_pos, FN&& where_join) &
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return *this = std::move(*this).join(insert_items, start_pos, end_pos, std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with an item.
         *
         * This function iterates over all elements in the array and inserts the `insert_item` after each element. The modified elements, along with the inserted items, are stored in this `list_array` object.
         *
         * @param insert_item The item to insert after each element.
         * @return A reference to this `list_array` after the join operation.
         */
        constexpr list_array<T, Allocator>& join(const T& insert_item) &
            requires std::copy_constructible<T>
        {
            return *this = std::move(*this).join(insert_item, 0, _size());
        }

        /**
         * @brief Joins elements of the array with an item, starting from a given position.
         *
         * This function iterates over the elements in the array, starting from the specified `start_pos`, and inserts the `insert_item` after each element. The modified elements, along with the inserted items, are stored in this `list_array` object.
         *
         * @param insert_item The item to insert after each element.
         * @param start_pos The starting index from which to insert the `insert_item`.
         * @return A reference to this `list_array` after the join operation.
         * @throws std::invalid_argument If `start_pos` is greater than size of the array.
         */
        constexpr list_array<T, Allocator>& join(const T& insert_item, size_t start_pos) &
            requires std::copy_constructible<T>
        {
            return *this = std::move(*this).join(insert_item, start_pos, _size());
        }

        /**
         * @brief Joins elements of the array with an item, starting from a given position and ending before a specified position.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element, it inserts the `insert_item` after the element. The modified elements, along with the inserted items, are stored in this `list_array` object.
         *
         * @param insert_item The item to insert after each element.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @return A reference to this `list_array` after the join operation.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
        constexpr list_array<T, Allocator>& join(const T& insert_item, size_t start_pos, size_t end_pos) &
            requires std::copy_constructible<T>
        {
            return *this = std::move(*this).join(insert_item, start_pos, end_pos);
        }

        /**
         * @brief Joins elements of the array with items from another array.
         *
         * This function iterates over all elements in the array and inserts the elements from `insert_items` after each element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `insert_items`.
         * @param insert_items The `list_array` containing the items to insert after elements.
         * @return A new `list_array` object containing the joined elements.
         */
        template <class AnyAllocator>
        constexpr list_array<T, Allocator>& join(const list_array<T, AnyAllocator>& insert_items) &
            requires std::copy_constructible<T>
        {
            return *this = std::move(*this).join(insert_items, 0, _size());
        }

        /**
         * @brief Joins elements of the array with items from another array, starting from a given position.
         *
         * This function iterates over the elements in the array, starting from the specified `start_pos`. For each element, it inserts the elements from `insert_items` after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `insert_items`.
         * @param insert_items The `list_array` containing the items to insert after elements.
         * @param start_pos The starting index from which to insert the items from `insert_items`.
         * @return A new `list_array` object containing the joined elements.
         * @throws std::invalid_argument If `start_pos` is greater than size of the array.
         */
        template <class AnyAllocator>
        constexpr list_array<T, Allocator>& join(const list_array<T, AnyAllocator>& insert_items, size_t start_pos) &
            requires std::copy_constructible<T>
        {
            return *this = std::move(*this).join(insert_items, start_pos, _size());
        }

        /**
         * @brief Joins elements of the array with items from another array, starting from a given position and ending before a specified position.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element, it inserts the elements from `insert_items` after the element. The modified elements, along with the inserted items, are stored in this `list_array` object.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `insert_items`.
         * @param insert_items The `list_array` containing the items to insert after elements.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @return A reference to this `list_array` after the join operation.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
        template <class AnyAllocator>
        constexpr list_array<T, AnyAllocator>& join(const list_array<T, AnyAllocator>& insert_items, size_t start_pos, size_t end_pos) &
            requires std::copy_constructible<T>
        {
            return *this = std::move(*this).join(insert_items, start_pos, end_pos);
        }

        /**
         * @brief Joins elements of the array with items from a raw array.
         *
         * This function iterates over all elements in the array and inserts the elements from the raw array `insert_items` (with `items_count` elements) after each element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @param insert_items A pointer to the beginning of the raw array containing the items to insert.
         * @param items_count The number of items in the raw array `insert_items`.
         * @return A new `list_array` object containing the joined elements.
         */
        constexpr list_array<T, Allocator>& join(const T* insert_items, size_t items_count) &
            requires std::copy_constructible<T>
        {
            return *this = std::move(*this).join(insert_items, items_count, 0, _size());
        }

        /**
         * @brief Joins elements of the array with items from a raw array, starting from a given position.
         *
         * This function iterates over the elements in the array, starting from the specified `start_pos`, and inserts the elements from the raw array `insert_items` (with `items_count` elements) after each element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @param insert_items A pointer to the beginning of the raw array containing the items to insert.
         * @param items_count The number of items in the raw array `insert_items`.
         * @param start_pos The index where the insertion starts (inclusive).
         * @return A new `list_array` object containing the joined elements.
         * @throws std::invalid_argument If `start_pos` is greater than size of the array.
         */
        constexpr list_array<T, Allocator>& join(const T* insert_items, size_t items_count, size_t start_pos) &
            requires std::copy_constructible<T>
        {
            return *this = std::move(*this).join(insert_items, items_count, start_pos, _size());
        }

        /**
         * @brief Joins elements of the array with items from a raw array within a specified range.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element, it inserts the elements from the raw array `insert_items` (with `items_count` elements) after the element. The modified elements, along with the inserted items, are stored in this `list_array` object.
         *
         * @param insert_items A pointer to the beginning of the raw array containing the items to insert.
         * @param items_count The number of items in the raw array `insert_items`.
         * @param start_pos The index where the insertion starts (inclusive).
         * @param end_pos The index where the insertion ends (exclusive).
         * @return A reference to this `list_array` after the join operation.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
        constexpr list_array<T, Allocator>& join(const T* insert_items, size_t items_count, size_t start_pos, size_t end_pos) &
            requires std::copy_constructible<T>
        {
            return *this = std::move(*this).join(insert_items, items_count, start_pos, end_pos);
        }

        /**
         * @brief Joins elements of the array with items from a C-style array.
         *
         * This function iterates over all elements in the array and inserts the elements from the C-style array `insert_items` (with size `arr_size`) after each element. The modified elements, along with the inserted items, are stored in this `list_array` object.
         *
         * @tparam arr_size The size of the C-style array `insert_items`.
         * @param insert_items The C-style array containing the items to insert after each element.
         * @return A reference to this `list_array` after the join operation.
         */
        template <size_t arr_size>
        constexpr list_array<T, Allocator>& join(const T (&insert_items)[arr_size]) &
            requires std::copy_constructible<T>
        {
            return *this = std::move(*this).join(insert_items, arr_size, 0, _size());
        }

        /**
         * @brief Joins elements of the array with items from a C-style array, based on a condition.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element where the `where_join` predicate returns true, it inserts the elements from the C-style array `insert_items` (with size `arr_size`) after the element. The modified elements, along with the inserted items, are stored in this `list_array` object.
         *
         * @tparam arr_size The size of the C-style array `insert_items`.
         * @param insert_items The C-style array containing the items to insert after elements where the predicate is true.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @return A reference to this `list_array` after the join operation.
         * @throws std::invalid_argument If `start_pos` is greater than size of the array.
         */
        template <size_t arr_size>
        constexpr list_array<T, Allocator>& join(const T (&insert_items)[arr_size], size_t start_pos) &
            requires std::copy_constructible<T>
        {
            return *this = std::move(*this).join(insert_items, arr_size, start_pos, _size());
        }

        /**
         * @brief Joins elements of the array with items from a C-style array, based on a condition.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element where the `where_join` predicate returns true, it inserts the elements from the C-style array `insert_items` (with size `arr_size`) after the element. The modified elements, along with the inserted items, are stored in this `list_array` object.
         *
         * @tparam arr_size The size of the C-style array `insert_items`.
         * @param insert_items The C-style array containing the items to insert after elements where the predicate is true.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @return A reference to this `list_array` after the join operation.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
        template <size_t arr_size>
        constexpr list_array<T, Allocator>& join(const T (&insert_items)[arr_size], size_t start_pos, size_t end_pos) &
            requires std::copy_constructible<T>
        {
            return *this = std::move(*this).join(insert_items, arr_size, start_pos, _size());
        }

        /**
         * @brief Joins elements of the array with an item or other array, based on a condition.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element where the `where_join` predicate returns true, it inserts the `insert_item` after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_item The item to insert after elements where the predicate is true.
         * @param where_join The predicate function to determine where to insert the `insert_item`.
         * @return A new `list_array` object containing the joined elements.
         */
        template <class FN = bool (*)(const T&)>
        constexpr list_array<T, Allocator> join(const T& insert_item, FN&& where_join) &&
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return std::move(*this).join(insert_item, 0, _size(), std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with an item or other array, based on a condition.
         *
         * This function iterates over the elements in the array, starting from the specified `start_pos`. For each element where the `where_join` predicate returns true, it inserts the `insert_item` after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_item The item to insert after elements where the predicate is true.
         * @param start_pos The starting index from which to insert the `insert_item`.
         * @param where_join The predicate function to determine where to insert the `insert_item`.
         * @return A new `list_array` object containing the joined elements.
         * @throws std::invalid_argument If `start_pos` is greater than size of the array.
         */
        template <class FN>
        constexpr list_array<T, Allocator> join(const T& insert_item, size_t start_pos, FN&& where_join) &&
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return std::move(*this).join(insert_item, start_pos, _size(), std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with an item or other array, based on a condition.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element where the `where_join` predicate returns true, it inserts the `insert_item` after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_item The item to insert after elements where the predicate is true.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @param where_join The predicate function to determine where to insert the `insert_item`.
         * @return A new `list_array` object containing the joined elements.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
        template <class FN>
        constexpr list_array<T, Allocator> join(const T& insert_item, size_t start_pos, size_t end_pos, FN&& where_join) &&
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
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

        /**
         * @brief Joins elements of the array with items from another array, based on a condition.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element where the `where_join` predicate returns true, it inserts the elements from `insert_items` after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `insert_items`.
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_items The `list_array` containing the items to insert after elements where the predicate is true.
         * @param where_join The predicate function to determine where to insert the items from `insert_items`.
         * @return A new `list_array` object containing the joined elements.
         */
        template <class AnyAllocator, class FN = bool (*)(const T&)>
        constexpr list_array<T, Allocator> join(const list_array<T, AnyAllocator>& insert_items, FN&& where_join) &&
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return std::move(*this).join(insert_items, 0, _size(), std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with items from another array, based on a condition.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element where the `where_join` predicate returns true, it inserts the elements from `insert_items` after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `insert_items`.
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_items The `list_array` containing the items to insert after elements where the predicate is true.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @param where_join The predicate function to determine where to insert the items from `insert_items`.
         * @return A new `list_array` object containing the joined elements.
         * @throws std::invalid_argument If `start_pos` is greater than size of the array.
         */
        template <class AnyAllocator, class FN>
        constexpr list_array<T, Allocator> join(const list_array<T, AnyAllocator>& insert_items, size_t start_pos, FN&& where_join) &&
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return std::move(*this).join(insert_items, start_pos, _size(), std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with items from another array, based on a condition.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element where the `where_join` predicate returns true, it inserts the elements from `insert_items` after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `insert_items`.
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_items The `list_array` containing the items to insert after elements where the predicate is true.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @param where_join The predicate function to determine where to insert the items from `insert_items`.
         * @return A new `list_array` object containing the joined elements.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
        template <class AnyAllocator, class FN>
        constexpr list_array<T, Allocator> join(const list_array<T, AnyAllocator>& insert_items, size_t start_pos, size_t end_pos, FN&& where_join) &&
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
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

        /**
         * @brief Joins elements of the array with items from a C-style array, based on a condition (move version).
         *
         * This function iterates over all elements in the array. For each element where the `where_join` predicate returns true, it inserts the elements from the C-style array `insert_items` (with size `arr_size`) after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam arr_size The size of the C-style array `insert_items`.
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_items The C-style array containing the items to insert after elements where the predicate is true.
         * @param where_join The predicate function to determine where to insert the items from `insert_items`.
         * @return A new `list_array` object containing the joined elements.
         */
        template <size_t arr_size, class FN>
        constexpr list_array<T, Allocator> join(const T (&insert_items)[arr_size], FN&& where_join) &&
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return std::move(*this).join(insert_items, arr_size, 0, _size(), std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with items from a C-style array, based on a condition (move version).
         *
         * This function iterates over the elements in the array, starting from the specified `start_pos`. For each element where the `where_join` predicate returns true, it inserts the elements from the C-style array `insert_items` (with size `arr_size`) after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam arr_size The size of the C-style array `insert_items`.
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_items The C-style array containing the items to insert after elements where the predicate is true.
         * @param start_pos The starting index from which to insert the items from `insert_items`.
         * @param where_join The predicate function to determine where to insert the items from `insert_items`.
         * @return A new `list_array` object containing the joined elements.
         * @throws std::invalid_argument If `start_pos` is greater than size of the array.
         */
        template <size_t arr_size, class FN>
        constexpr list_array<T, Allocator> join(const T (&insert_items)[arr_size], size_t start_pos, FN&& where_join) &&
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return std::move(*this).join(insert_items, arr_size, start_pos, _size(), std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with items from a C-style array, based on a condition.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element where the `where_join` predicate returns true, it inserts the elements from the C-style array `insert_items` (with size `arr_size`) after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam arr_size The size of the C-style array `insert_items`.
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_items The C-style array containing the items to insert after elements where the predicate is true.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @param where_join The predicate function to determine where to insert the items from `insert_items`.
         * @return A new `list_array` object containing the joined elements.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
        template <size_t arr_size, class FN>
        constexpr list_array<T, Allocator> join(const T (&insert_items)[arr_size], size_t start_pos, size_t end_pos, FN&& where_join) &&
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return std::move(*this).join(insert_items, arr_size, start_pos, end_pos, std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with items from a raw array, based on a condition.
         *
         * This function iterates over the elements in the array. For each element where the `where_join` predicate returns true, it inserts the elements from the raw array `insert_items` (with `items_count` elements) after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_items A pointer to the beginning of the raw array containing the items to insert.
         * @param items_count The number of items in the raw array `insert_items`.
         * @param where_join The predicate function to determine where to insert the items from `insert_items`.
         * @return A new `list_array` object containing the joined elements.
         */
        template <class FN>
        constexpr list_array<T, Allocator> join(const T* insert_items, size_t items_count, FN&& where_join) &&
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
        {
            return std::move(*this).join(insert_items, items_count, 0, _size(), std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with items from a raw array, based on a condition.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element where the `where_join` predicate returns true, it inserts the elements from the raw array `insert_items` (with `items_count` elements) after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_items A pointer to the beginning of the raw array containing the items to insert.
         * @param items_count The number of items in the raw array `insert_items`.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param where_join The predicate function to determine where to insert the items from `insert_items`.
         * @return A new `list_array` object containing the joined elements.
         * @throws std::invalid_argument If `start_pos` is greater than size of the array.
         */
        template <class FN>
        constexpr list_array<T, Allocator> join(const T* insert_items, size_t items_count, size_t start_pos, FN&& where_join) &&
            requires std::copy_constructible<T>
        {
            return std::move(*this).join(insert_items, items_count, start_pos, _size(), std::forward<FN>(where_join));
        }

        /**
         * @brief Joins elements of the array with items from a raw array, based on a condition.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element where the `where_join` predicate returns true, it inserts the elements from the raw array `insert_items` (with `items_count` elements) after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_items A pointer to the beginning of the raw array containing the items to insert.
         * @param items_count The number of items in the raw array `insert_items`.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @param where_join The predicate function to determine where to insert the items from `insert_items`.
         * @return A new `list_array` object containing the joined elements.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
        template <class FN>
        constexpr list_array<T, Allocator> join(const T* insert_items, size_t items_count, size_t start_pos, size_t end_pos, FN&& where_join) &&
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>)
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

        /**
         * @brief Joins elements of the array with an item, based on a condition.
         *
         * This function iterates over all elements in the array. For each element where the `where_join` predicate returns true, it inserts the `insert_item` after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param insert_item The item to insert after elements where the predicate is true.
         * @param where_join The predicate function to determine where to insert the `insert_item`.
         * @return A new `list_array` object containing the joined elements.
         */
        constexpr list_array<T, Allocator> join(const T& insert_item) &&
            requires std::copy_constructible<T>
        {
            return std::move(*this).join(insert_item, 0, _size());
        }

        /**
         * @brief Joins elements of the array with an item, starting from a given position.
         *
         * This function iterates over the elements in the array, starting from the specified `start_pos`, and inserts the `insert_item` after each element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @param insert_item The item to insert after each element.
         * @param start_pos The starting index from which to insert the `insert_item`.
         * @return A new `list_array` object containing the joined elements.
         * @throws std::invalid_argument If `start_pos` is greater than size of the array.
         */
        constexpr list_array<T, Allocator> join(const T& insert_item, size_t start_pos) &&
            requires std::copy_constructible<T>
        {
            return std::move(*this).join(insert_item, start_pos, _size());
        }

        /**
         * @brief Joins elements of the array with an item, starting from a given position and ending before a specified position.
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element, it inserts the `insert_item` after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @param insert_item The item to insert after each element.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @return A new `list_array` object containing the joined elements.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
        constexpr list_array<T, Allocator> join(const T& insert_item, size_t start_pos, size_t end_pos) &&
            requires std::copy_constructible<T>
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

        /**
         * @brief Joins elements of the array with items from another array.
         *
         * This function iterates over all elements in the array and inserts the elements from `insert_items` after each element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `insert_items`.
         * @param insert_items The `list_array` containing the items to insert after elements.
         * @return A new `list_array` object containing the joined elements.
         */
        template <class AnyAllocator>
        constexpr list_array<T, Allocator> join(const list_array<T, AnyAllocator>& insert_items) &&
            requires std::copy_constructible<T>
        {
            return std::move(*this).join(insert_items, 0, _size());
        }

        /**
         * @brief Joins elements of the array with items from another array, starting from a given position.
         *
         * This function iterates over the elements in the array, starting from the specified `start_pos`. For each element, it inserts the elements from `insert_items` after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `insert_items`.
         * @param insert_items The `list_array` containing the items to insert after elements.
         * @param start_pos The starting index from which to insert the items from `insert_items`.
         * @return A new `list_array` object containing the joined elements.
         * @throws std::invalid_argument If `start_pos` is greater than size of the array.
         */
        template <class AnyAllocator>
        constexpr list_array<T, Allocator> join(const list_array<T, AnyAllocator>& insert_items, size_t start_pos) &&
            requires std::copy_constructible<T>
        {
            return std::move(*this).join(insert_items, start_pos, _size());
        }

        /**
         * @brief Joins elements of the array with items from another array, starting from a given position and ending before a specified position.
         *
         * This function iterates over the elements in the array, starting from the specified `start_pos` and ending before `end_pos`. For each element, it inserts the elements from `insert_items` after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `insert_items`.
         * @param insert_items The `list_array` containing the items to insert after elements.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @return A new `list_array` object containing the joined elements.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
        template <class AnyAllocator>
        constexpr list_array<T, AnyAllocator> join(const list_array<T, AnyAllocator>& insert_items, size_t start_pos, size_t end_pos) &&
            requires std::copy_constructible<T>
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

        /**
         * @brief Joins elements of the array with items from a raw array.
         *
         * This function iterates over all elements in the array and inserts the elements from the raw array `insert_items` (with `items_count` elements) after each element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @param insert_items A pointer to the beginning of the raw array containing the items to insert.
         * @param items_count The number of items in the raw array `insert_items`.
         * @return A new `list_array` object containing the joined elements.
         */
        constexpr list_array<T, Allocator> join(const T* insert_items, size_t items_count) &&
            requires std::copy_constructible<T>
        {
            return std::move(*this).join(insert_items, items_count, 0, _size());
        }

        /**
         * @brief Joins elements of the array with items from a raw array, starting from a given position.
         *
         * This function iterates over the elements in the array, starting from the specified `start_pos`, and inserts the elements from the raw array `insert_items` (with `items_count` elements) after each element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @param insert_items A pointer to the beginning of the raw array containing the items to insert.
         * @param items_count The number of items in the raw array `insert_items`.
         * @param start_pos The index where the insertion starts (inclusive).
         * @return A new `list_array` object containing the joined elements.
         * @throws std::invalid_argument If `start_pos` is greater than size of the array.
         */
        constexpr list_array<T, Allocator> join(const T* insert_items, size_t items_count, size_t start_pos) &&
            requires std::copy_constructible<T>
        {
            return std::move(*this).join(insert_items, items_count, start_pos, _size());
        }

        /**
         * @brief Joins elements of the array with items from a raw array within a specified range (move version).
         *
         * This function inserts the elements from the raw array `insert_items` (with `items_count` elements) into the array, starting at position `start_pos` and ending before `end_pos`. The modified array, including the inserted elements, is moved into a new `list_array` object.
         *
         * @param insert_items A pointer to the beginning of the raw array containing the items to insert.
         * @param items_count The number of items in the raw array `insert_items`.
         * @param start_pos The index where the insertion starts (inclusive).
         * @param end_pos The index where the insertion ends (exclusive).
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
        constexpr list_array<T, Allocator> join(const T* insert_items, size_t items_count, size_t start_pos, size_t end_pos) &&
            requires std::copy_constructible<T>
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

        /**
         * @brief Joins elements of the array with items from a C-style array, based on a condition (move version).
         *
         * This function iterates over all elements in the array. For each element where the `where_join` predicate returns true, it inserts the elements from the C-style array `insert_items` (with size `arr_size`) after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam arr_size The size of the C-style array `insert_items`.
         * @param insert_items The C-style array containing the items to insert after elements where the predicate is true.
         * @return A new `list_array` object containing the joined elements.
         */
        template <size_t arr_size>
        constexpr list_array<T, Allocator> join(const T (&insert_items)[arr_size]) &&
            requires std::copy_constructible<T>
        {
            return std::move(*this).join(insert_items, arr_size, 0, _size());
        }

        /**
         * @brief Joins elements of the array with items from a C-style array, based on a condition (move version).
         *
         * This function iterates over the elements in the array, starting from the specified `start_pos`. For each element where the `where_join` predicate returns true, it inserts the elements from the C-style array `insert_items` (with size `arr_size`) after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam arr_size The size of the C-style array `insert_items`.
         * @param insert_items The C-style array containing the items to insert after elements where the predicate is true.
         * @param start_pos The starting index from which to insert the items from `insert_items`.
         * @return A new `list_array` object containing the joined elements.
         * @throws std::invalid_argument If `start_pos` is greater than size of the array.
         */
        template <size_t arr_size>
        constexpr list_array<T, Allocator> join(const T (&insert_items)[arr_size], size_t start_pos) &&
            requires std::copy_constructible<T>
        {
            return std::move(*this).join(insert_items, arr_size, start_pos, _size());
        }

        /**
         * @brief Joins elements of the array with items from a C-style array, based on a condition (move version).
         *
         * This function iterates over the elements in the array within the specified range (`start_pos` to `end_pos`). For each element where the `where_join` predicate returns true, it inserts the elements from the C-style array `insert_items` (with size `arr_size`) after the element. The modified elements, along with the inserted items, are moved into a new `list_array` object.
         *
         * @tparam arr_size The size of the C-style array `insert_items`.
         * @param insert_items The C-style array containing the items to insert after elements where the predicate is true.
         * @param start_pos The starting index of the range to consider (inclusive).
         * @param end_pos The ending index of the range to consider (exclusive).
         * @return A new `list_array` object containing the joined elements.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
        template <size_t arr_size>
        constexpr list_array<T, Allocator> join(const T (&insert_items)[arr_size], size_t start_pos, size_t end_pos) &&
            requires std::copy_constructible<T>
        {
            return std::move(*this).join(insert_items, arr_size, start_pos, _size());
        }

/// @}
#pragma endregion
#pragma region contains

        /// @name contains
        /// @{
        /**
         * @brief Checks if the array contains a specific value.
         *
         * This function checks if the array contains the given `value` within the entire array. It uses the default equality comparison operator (`==`) to determine equality.
         *
         * @param value The value to search for.
         * @return `true` if the array contains the `value`, `false` otherwise.
         */
        [[nodiscard]] constexpr bool contains(const T& value) const&
            requires std::equality_comparable<T>
        {
            return contains(0, _size(), value);
        }

        /**
         * @brief Checks if the array contains a specific value.
         *
         * This function checks if the array contains the given `value` within the entire array. It uses the default equality comparison operator (`==`) to determine equality.
         *
         * @param value The value to search for.
         * @param start The starting index from which to search for the `value`.
         * @return `true` if the array contains the `value`, `false` otherwise.
         */
        [[nodiscard]] constexpr bool contains(size_t start, const T& value) const&
            requires std::equality_comparable<T>
        {
            return contains(start, _size(), value);
        }

        /**
         * @brief Checks if the array contains a specific value within a specified range.
         *
         * This function checks if the array contains the given `value` within the range of this array from `start` to `end`.
         *
         * @param value The value to search for.
         * @param start The starting index of the range to search (inclusive).
         * @param end The ending index of the range to search (exclusive).
         * @return `true` if the array contains the `value`, `false` otherwise.
         */
        [[nodiscard]] constexpr bool contains(size_t start, size_t end, const T& value) const&
            requires std::equality_comparable<T>
        {
            return find(start, end, value) != npos;
        }

        /**
         * @brief Checks if the array contains a specific C-style array.
         *
         * This function checks if the array contains the elements in the given C-style array `arr` within the entire array.
         *
         * @tparam arr_size The size of the C-style array `arr`.
         * @param arr The C-style array to search for.
         * @return `true` if the array contains the `arr` array, `false` otherwise.
         */
        template <size_t arr_size>
        [[nodiscard]] constexpr bool contains(const T (&arr)[arr_size]) const&
            requires std::equality_comparable<T>
        {
            return contains(0, _size(), arr, arr_size);
        }

        /**
         * @brief Checks if the array contains a specific C-style array.
         *
         * This function checks if the array contains the elements in the given C-style array `arr` within the entire array.
         *
         * @tparam arr_size The size of the C-style array `arr`.
         * @param arr The C-style array to search for.
         * @param start The starting index from which to search for the `arr` array.
         * @return `true` if the array contains the `arr` array, `false` otherwise.
         */
        template <size_t arr_size>
        [[nodiscard]] constexpr bool contains(size_t start, const T (&arr)[arr_size]) const&
            requires std::equality_comparable<T>
        {
            return contains(start, _size(), arr, arr_size);
        }

        /**
         * @brief Checks if the array contains a specific raw array within a specified range.
         *
         * This function checks if the array contains the elements in the given raw array `arr` (with size `arr_size`) within the range of this array from `start` to `end`.
         *
         * @param arr A pointer to the beginning of the raw array to search for.
         * @param arr_size The size of the raw array `arr`.
         * @param start The starting index of the range to search in this array (inclusive).
         * @param end The ending index of the range to search in this array (exclusive).
         * @return `true` if the array contains the `arr` array, `false` otherwise.
         */
        template <size_t arr_size>
        [[nodiscard]] constexpr bool contains(size_t start, size_t end, const T (&arr)[arr_size]) const&
            requires std::equality_comparable<T>
        {
            return contains(start, end, arr, arr_size);
        }

        /**
         * @brief Checks if the array contains a specific raw array.
         *
         * This function checks if the array contains the elements in the given raw array `arr` (with size `arr_size`) within the entire array.
         *
         * @param arr A pointer to the beginning of the raw array to search for.
         * @param arr_size The size of the raw array `arr`.
         * @param start The starting index from which to search for the `arr` array.
         * @return `true` if the array contains the `arr` array, `false` otherwise.
         */
        [[nodiscard]] constexpr bool contains(const T* arr, size_t arr_size) const&
            requires std::equality_comparable<T>
        {
            return contains(0, _size(), arr, arr_size);
        }

        /**
         * @brief Checks if the array contains a specific raw array.
         *
         * This function checks if the array contains the elements in the given raw array `arr` (with size `arr_size`) within the entire array.
         *
         * @param arr A pointer to the beginning of the raw array to search for.
         * @param arr_size The size of the raw array `arr`.
         * @param start The starting index from which to search for the `arr` array.
         * @return `true` if the array contains the `arr` array, `false` otherwise.
         */
        [[nodiscard]] constexpr bool contains(size_t start, const T* arr, size_t arr_size) const&
            requires std::equality_comparable<T>
        {
            return contains(start, _size(), arr, arr_size);
        }

        /**
         * @brief Checks if the array contains a specific raw array within a specified range.
         *
         * This function checks if the array contains the elements in the given raw array `arr` (with size `arr_size`) within the range of this array from `start` to `end`.
         *
         * @param arr A pointer to the beginning of the raw array to search for.
         * @param arr_size The size of the raw array `arr`.
         * @param start The starting index of the range to search in this array (inclusive).
         * @param end The ending index of the range to search in this array (exclusive).
         * @return `true` if the array contains the `arr` array, `false` otherwise.
         */
        [[nodiscard]] constexpr bool contains(size_t start, size_t end, const T* arr, size_t arr_size) const&
            requires std::equality_comparable<T>
        {
            return find(start, end, arr, arr + arr_size) != npos;
        }

        /**
         * @brief Checks if the array contains a specific `list_array`.
         *
         * This function checks if the array contains the elements in the given `list_array` `value` within the entire array.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `value`.
         * @param value The `list_array` to search for.
         * @return `true` if the array contains the `value` list_array, `false` otherwise.
         */
        template <class AnyAllocator>
        [[nodiscard]] constexpr bool contains(const list_array<T, AnyAllocator>& value) const&
            requires std::equality_comparable<T>
        {
            return contains(0, _size(), value);
        }

        /**
         * @brief Checks if the array contains a specific `list_array`.
         *
         * This function checks if the array contains the elements in the given `list_array` `value` within the entire array.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `value`.
         * @param value The `list_array` to search for.
         * @param start The starting index from which to search for the `value` list_array.
         * @return `true` if the array contains the `value` list_array, `false` otherwise.
         */
        template <class AnyAllocator>
        [[nodiscard]] constexpr bool contains(size_t start, const list_array<T, AnyAllocator>& value) const&
            requires std::equality_comparable<T>
        {
            return contains(start, _size(), value);
        }

        /**
         * @brief Checks if the array contains a specific list_array within a specified range.
         *
         * This function checks if the array contains the elements in the given list_array `value` within the range of this array from `start` to `end`.
         *
         * @tparam AnyAllocator The allocator type of the list_array `value`.
         * @param value The list_array to search for.
         * @param start The starting index of the range to search in this array (inclusive).
         * @param end The ending index of the range to search in this array (exclusive).
         * @return `true` if the array contains the `value` list_array, `false` otherwise.
         */
        template <class AnyAllocator>
        [[nodiscard]] constexpr bool contains(size_t start, size_t end, const list_array<T, AnyAllocator>& value) const&
            requires std::equality_comparable<T>
        {
            return contains(start, end, value);
        }

        /**
         * @brief Checks if the array contains a specific list_array within a specified range.
         *
         * This function checks if the array contains the elements in the given list_array `value`, starting from `value_start` and up to `value_end`, within the range of this array from `start` to `end`.
         *
         * @tparam AnyAllocator The allocator type of the list_array `value`.
         * @param value_begin The iterator to the beginning of the container to search for.
         * @param value_end The iterator to the end of the container to search for.
         * @return `true` if the array contains the specified sub-array of `value`, `false` otherwise.
         */
        template <class any_iter>
        [[nodiscard]] constexpr bool contains(any_iter value_begin, any_iter value_end) const&
            requires std::equality_comparable_with<T, typename std::iterator_traits<any_iter>::value_type>
        {
            return contains(0, _size(), value_begin, value_end);
        }

        /**
         * @brief Checks if the array contains a specific list_array within a specified range.
         *
         * This function checks if the array contains the elements in the given list_array `value`, starting from `value_start` and up to `value_end`, within the range of this array from `start` to `end`.
         *
         * @tparam AnyAllocator The allocator type of the list_array `value`.
         * @param value_begin The iterator to the beginning of the container to search for.
         * @param value_end The iterator to the end of the container to search for.
         * @param start The starting index of the range to search in this array (inclusive).
         * @return `true` if the array contains the specified sub-array of `value`, `false` otherwise.
         */
        template <class any_iter>
        [[nodiscard]] constexpr bool contains(size_t start, any_iter value_begin, any_iter value_end) const&
            requires std::equality_comparable_with<T, typename std::iterator_traits<any_iter>::value_type>
        {
            return contains(start, _size(), value_begin, value_end);
        }

        /**
         * @brief Checks if the array contains a specific list_array within a specified range.
         *
         * This function checks if the array contains the elements in the given list_array `value`, starting from `value_start` and up to `value_end`, within the range of this array from `start` to `end`.
         *
         * @tparam AnyAllocator The allocator type of the list_array `value`.
         * @param value_begin The iterator to the beginning of the container to search for.
         * @param value_end The iterator to the end of the container to search for.
         * @param start The starting index of the range to search in this array (inclusive).
         * @param end The ending index of the range to search in this array (exclusive).
         * @return `true` if the array contains the specified sub-array of `value`, `false` otherwise.
         */
        template <class any_iter>
        [[nodiscard]] constexpr bool contains(size_t start, size_t end, any_iter value_begin, any_iter value_end) const&
            requires std::equality_comparable_with<T, typename std::iterator_traits<any_iter>::value_type>
        {
            return contains(start, end, value_begin, value_end);
        }

        /**
         * @brief Checks if at least one element in the entire array satisfies a predicate.
         *
         * This function is similar to `contains_one(size_t, size_t, FN&&)`, but it considers the entire array.
         *
         * @tparam FN The type of the predicate function.
         * @param check_function The predicate function.
         * @return `true` if at least one element satisfies the predicate, `false` otherwise.
         */
        template <class FN>
        [[nodiscard]] constexpr bool contains_one(FN&& check_function) const&
            requires std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>
        {
            return contains_one(0, _size(), std::forward<FN>(check_function));
        }

        /**
         * @brief Counts how many elements in the entire array satisfy a predicate.
         *
         * This function is similar to `contains_multiply(size_t, size_t, FN&&)`, but it considers the entire array.
         *
         * @tparam FN The type of the predicate function.
         * @param check_function The predicate function.
         * @return The number of elements that satisfy the predicate.
         */
        template <class FN>
        [[nodiscard]] constexpr size_t contains_multiply(FN&& check_function) const&
            requires std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>
        {
            return contains_multiply(0, _size(), std::forward<FN>(check_function));
        }

        /**
         * @brief Checks if at least one element from a starting position satisfies a predicate.
         *
         * This function is similar to `contains_one(size_t, size_t, FN&&)`, but it starts checking from the specified `start` index and continues until the end of the array.
         *
         * @tparam FN The type of the predicate function.
         * @param start The starting index.
         * @param check_function The predicate function.
         * @return `true` if at least one element satisfies the predicate, `false` otherwise.
         */
        template <class FN>
        [[nodiscard]] constexpr bool contains_one(size_t start, FN&& check_function) const&
            requires std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>
        {
            return contains_one(start, _size(), std::forward<FN>(check_function));
        }

        /**
         * @brief Counts how many elements from a starting position satisfy a predicate.
         *
         * This function is similar to `contains_multiply(size_t, size_t, FN&&)`, but it starts counting from the specified `start` index and continues until the end of the array.
         *
         * @tparam FN The type of the predicate function.
         * @param start The starting index.
         * @param check_function The predicate function.
         * @return The number of elements that satisfy the predicate.
         */
        template <class FN>
        [[nodiscard]] constexpr size_t contains_multiply(size_t start, FN&& check_function) const&
            requires std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>
        {
            return contains_multiply(start, _size(), std::forward<FN>(check_function));
        }

        /**
         * @brief Checks if at least one element in a range satisfies a predicate.
         *
         * This function iterates over the elements in the specified range (`start` to `end`) and checks if at least one of them satisfies the given predicate `check_function`.
         *
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param start The starting index of the range (inclusive).
         * @param end The ending index of the range (exclusive).
         * @param check_function The predicate function to apply to each element.
         * @return `true` if at least one element satisfies the predicate, `false` otherwise.
         */
        template <class FN>
        [[nodiscard]] constexpr bool contains_one(size_t start, size_t end, FN&& check_function) const&
            requires std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&>
        {
            return find_if(start, end, std::forward<FN>(check_function)) != npos;
        }

        /**
         * @brief Counts how many elements in a range satisfy a predicate.
         *
         * This function iterates over the elements in the specified range (`start` to `end`) and counts how many of them satisfy the given predicate `check_function`.
         *
         * @tparam FN The type of the predicate function. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         * @param start The starting index of the range (inclusive).
         * @param end The ending index of the range (exclusive).
         * @param check_function The predicate function to apply to each element.
         * @return The number of elements that satisfy the predicate.
         */
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

/// @}
#pragma endregion
#pragma region sort

        /// @name sort
        /// @{

        /**
         * @brief Sorts the list_array in ascending order.
         *
         * This function sorts the elements in the list_array in-place in ascending order using the default less-than operator (`<`). It modifies the original list_array and does not return a new object.
         *
         * @return A reference to the modified list_array after sorting.
         */
        constexpr list_array<T, Allocator>& sort() & {
            if (empty())
                return *this;
            if constexpr (std::is_unsigned<T>::value) {
                const T& min_val = min();
                size_t dif = max() - min_val + 1;
                list_array<size_t, std::allocator<size_t>> count_arr(dif);
                list_array<T, Allocator> result(_size());
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
                size_t min_val = normalize(min());
                size_t dif = normalize(max()) - min_val + 1;
                list_array<size_t, std::allocator<size_t>> count_arr(dif, 0);
                list_array<T, Allocator> result(_size());
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
                size_t curr_L_size = _size() / 2 + 1;
                size_t curr_M_size = _size() / 2 + 1;
                T* L = allocator_and_size.allocate(_size() / 2 + 1);
                T* M = allocator_and_size.allocate(_size() / 2 + 1);
                auto fix_size = [&](size_t start, size_t middle, size_t end) {
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
                    size_t i = 0, j = 0, k = start;
                    for (T& it : range(start, end)) {
                        if (i < n1 && j < n2)
                            it = std::move(L[i] <= M[j] ? L[i++] : M[j++]);
                        else if (i < n1)
                            it = std::move(L[i++]);
                        else if (j < n2)
                            it = std::move(M[j++]);
                    }
                    for (size_t i = 0; i < n1; i++)
                        L[i].~T();
                    for (size_t i = 0; i < n2; i++)
                        M[i].~T();
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
                    merge(0, err, _size());
                allocator_and_size.deallocate(L, curr_L_size);
                allocator_and_size.deallocate(M, curr_M_size);
            }
            return *this;
        }

        /**
         * @brief Sorts the list_array using a custom comparison function.
         *
         * This function sorts the elements in the list_array in-place using the provided comparison function `compare`. It modifies the original list_array and does not return a new object.
         *
         * @tparam FN The type of the comparison function. It should take two const references to `T` and return `true` if the first element is less than the second, `false` otherwise.
         * @param compare The comparison function to use for sorting.
         * @return A reference to the modified list_array after sorting.
         */
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
                get_iterator(start)._fast_load<true, true>(L, n1);
                get_iterator(middle)._fast_load<true, true>(M, n2);
                size_t i = 0, j = 0, k = start;
                for (T& it : range(start, end)) {
                    if (i < n1 && j < n2)
                        it = std::move(compare(L[i], M[j]) ? L[i++] : M[j++]);
                    else if (i < n1)
                        it = std::move(L[i++]);
                    else if (j < n2)
                        it = std::move(M[j++]);
                }
                for (size_t i = 0; i < n1; i++)
                    L[i].~T();
                for (size_t i = 0; i < n2; i++)
                    M[i].~T();
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
                merge(0, err ? err - 1 : 0, _size());
            allocator_and_size.deallocate(L, curr_L_size);
            allocator_and_size.deallocate(M, curr_M_size);
            return *this;
        }

        /**
         * @brief Sorts the list_array in ascending order (move version).
         *
         * This function sorts the elements in the list_array in ascending order using the default less-than operator (`<`). It returns a new list_array object containing the sorted elements, leaving the original list_array unmodified.
         *
         * @return A new list_array object containing the sorted elements.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> sort() && {
            sort();
            return std::move(*this);
        }

        /**
         * @brief Sorts the list_array using a custom comparison function (move version).
         *
         * This function sorts the elements in the list_array using the provided comparison function `compare`. It returns a new list_array object containing the sorted elements, leaving the original list_array unmodified.
         *
         * @tparam FN The type of the comparison function. It should take two const references to `T` and return `true` if the first element is less than the second, `false` otherwise.
         * @param compare The comparison function to use for sorting.
         * @return A new list_array object containing the sorted elements.
         */
        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> sort(FN&& compare) && {
            sort(std::forward<FN>(compare));
            return std::move(*this);
        }

/// @}
#pragma endregion
#pragma region concat

        /// @name concat
        /// @{
        /**
         * @brief Creates a new list_array object by concatenating multiple containers or values.
         *
         * This function creates a new list_array object by concatenating the elements from the provided arguments. The arguments can be other list_array objects, standard containers, or individual values.
         *
         * @tparam Arguments The types of the arguments to concatenate.
         * @param args The containers or values to concatenate.
         * @return A new list_array object containing the concatenated elements.
         */
        template <class... Arguments>
        [[nodiscard]] static constexpr list_array<T, Allocator> concat(Arguments&&... args) {
            list_array<T, Allocator> result;
            result.reserve(sizeof...(Arguments));
            (result.push_back(std::forward<Arguments>(args)), ...);
            return result;
        }

        /**
         * @brief Concatenates multiple list_array objects into this list_array.
         *
         * This function appends the elements from each list_array in `concat_arr` to the end of this list_array. The elements are copied.
         *
         * @tparam AnyAllocator0 The allocator type of the outer list_array `concat_arr`.
         * @tparam AnyAllocator1 The allocator type of the inner list_array objects within `concat_arr`.
         * @param concat_arr The list_array containing the list_array objects to concatenate.
         * @return A reference to this list_array after concatenation.
         */
        template <class AnyAllocator0, class AnyAllocator1>
        static constexpr list_array<T, Allocator> concat(const list_array<list_array<T, AnyAllocator1>, AnyAllocator0>& concat_arr) {
            list_array<T, AnyAllocator1> res;
            for (auto& i : concat_arr)
                res.push_back(i);
            return res;
        }

        /**
         * @brief Concatenates multiple list_array objects into this list_array.
         *
         * This function appends the elements from each list_array in `concat_arr` to the end of this list_array. The elements are moved.
         *
         * @tparam AnyAllocator0 The allocator type of the outer list_array `concat_arr`.
         * @tparam AnyAllocator1 The allocator type of the inner list_array objects within `concat_arr`.
         * @param concat_arr The list_array containing the list_array objects to concatenate.
         * @return A reference to this list_array after concatenation.
         */
        template <class AnyAllocator0, class AnyAllocator1>
        static constexpr list_array<T, Allocator> concat(list_array<list_array<T, AnyAllocator1>, AnyAllocator0>&& concat_arr) {
            list_array<T, AnyAllocator1> res;
            for (auto& i : concat_arr)
                res.push_back(std::move(i));
            return res;
        }

        /**
         * @brief Concatenates the elements of this list_array into a new container.
         *
         * This function creates a new container of type `Container` (which must satisfy the `is_container` concept) and copies or moves the elements from this list_array into it. The elements are moved if this list_array is an rvalue reference, otherwise they are copied.
         *
         * @tparam Y The type of the container to create (defaults to `T`, the element type of the list_array).
         * @return A new container object containing the concatenated elements.
         */
        template <class Y = T>
        [[nodiscard]] constexpr typename is_container<Y>::container concat() & {
            T res;
            for (auto& i : *this)
                res.push_back(i);
            return res;
        }

        /**
         * @brief Concatenates the elements of this list_array into a new container (move version).
         *
         * This function is similar to the lvalue reference version of `concat`, but it operates on an rvalue reference (temporary) of the `list_array`. The elements are moved into the new container.
         *
         * @tparam Y The type of the container to create (defaults to `T`, the element type of the list_array).
         * @return A new container object containing the concatenated elements.
         */
        template <class Y = T>
        [[nodiscard]] constexpr typename is_container<Y>::container concat() && {
            T res;
            for (auto& i : *this)
                res.push_back(std::move(i));
            return res;
        }

/// @}
#pragma endregion
#pragma region where

        /// @name where
        /// @{
        /**
         * @brief Filters elements from the array based on a predicate.
         *
         * This function creates a new list_array object containing only the elements from the original array that satisfy the given predicate `check_fn`. The predicate can be a lambda function, a function object, or a function pointer. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         *
         * @tparam FN The type of the predicate function.
         * @param check_fn The predicate function to apply to each element.
         * @return A new list_array object containing the filtered elements.
         */
        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> where(FN&& check_fn) const&
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&> || is_apply_invocable_r_v<bool, FN, T>)
        {
            return where(0, _size(), std::forward<FN>(check_fn));
        }

        /**
         * @brief Filters elements from the array based on a predicate, starting from a given position.
         *
         * This function creates a new list_array object containing only the elements from the original array, starting from the specified `start_pos`, that satisfy the given predicate `check_fn`. The predicate can be a lambda function, a function object, or a function pointer. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         *
         * @tparam FN The type of the predicate function.
         * @param start_pos The starting index from which to filter elements.
         * @param check_fn The predicate function to apply to each element.
         * @return A new list_array object containing the filtered elements.
         */
        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> where(size_t start_pos, FN&& check_fn) const&
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&> || is_apply_invocable_r_v<bool, FN, T>)
        {
            return where(start_pos, _size(), std::forward<FN>(check_fn));
        }

        /**
         * @brief Filters elements from the array based on a predicate within a specified range.
         *
         * This function creates a new list_array object containing only the elements from the original array, within the range from `start_pos` to `end_pos`, that satisfy the given predicate `check_fn`. The predicate can be a lambda function, a function object, or a function pointer. It can take either a `const T&` (element) or `size_t, const T&` (index, element) as arguments.
         *
         * @tparam FN The type of the predicate function.
         * @param start_pos The starting index of the range to filter from (inclusive).
         * @param end_pos The ending index of the range to filter from (exclusive).
         * @param check_fn The predicate function to apply to each element.
         * @return A new list_array object containing the filtered elements.
         */
        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> where(size_t start_pos, size_t end_pos, FN&& check_fn) const&
            requires std::copy_constructible<T> && (std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&> || is_apply_invocable_r_v<bool, FN, T>)
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

        /**
         * @brief Filters elements from the array based on a predicate (move version).
         *
         * This function is similar to the lvalue reference version of `where`, but it operates on an rvalue reference (temporary) of the `list_array`. It moves the elements that satisfy the predicate `check_fn` into a new list_array object.
         *
         * @tparam FN The type of the predicate function.
         * @param check_fn The predicate function to apply to each element.
         * @return A new list_array object containing the filtered elements.
         */
        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> where(FN&& check_fn) &&
            requires std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&> || is_apply_invocable_r_v<bool, FN, T>
        {
            return take().where(0, _size(), std::forward<FN>(check_fn));
        }

        /**
         * @brief Filters elements from the array based on a predicate, starting from a given position (move version).
         *
         * This function is similar to the lvalue reference version of `where(size_t, FN&&)`, but it operates on an rvalue reference (temporary) of the `list_array`. It moves the elements starting from the specified `start_pos` that satisfy the predicate `check_fn` into a new list_array object.
         *
         * @tparam FN The type of the predicate function.
         * @param start_pos The starting index from which to filter elements.
         * @param check_fn The predicate function to apply to each element.
         * @return A new list_array object containing the filtered elements.
         */
        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> where(size_t start_pos, FN&& check_fn) &&
            requires std::is_invocable_r_v<bool, FN, const T&> || std::is_invocable_r_v<bool, FN, size_t, const T&> || is_apply_invocable_r_v<bool, FN, T>
        {
            return take().where(start_pos, _size(), std::forward<FN>(check_fn));
        }

        /**
         * @brief Filters elements from the array based on a predicate within a specified range (move version).
         *
         * This function is similar to the lvalue reference version of `where(size_t, size_t, FN&&)`, but it operates on an rvalue reference (temporary) of the `list_array`. It moves the elements within the range from `start_pos` to `end_pos` that satisfy the predicate `check_fn` into a new list_array object.
         *
         * @tparam FN The type of the predicate function.
         * @param start_pos The starting index of the range to filter from (inclusive).
         * @param end_pos The ending index of the range to filter from (exclusive).
         * @param check_fn The predicate function to apply to each element.
         * @return A new list_array object containing the filtered elements.
         */
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

/// @}
#pragma endregion
#pragma region for each

        /// @name for each
        /// @{
        /**
         * @brief Applies a function to each element in the array in reverse order.
         *
         * This function iterates over the elements in the array in reverse order (from the last element to the first) and applies the given function `iterate_fn` to each element. The function can modify the elements in the array.
         *
         * @tparam FN The type of the function to apply. It must be invocable with `T&` or `size_t, T&` (index, element) or accept reference to tuple/pair elements.
         * @param iterate_fn The function to apply to each element.
         * @return A reference to this `list_array` after the function has been applied to all elements.
         */
        template <class FN>
        constexpr list_array<T, Allocator>& for_each_reverse(FN&& iterate_fn) &
            requires std::is_invocable_v<FN, T&> || std::is_invocable_v<FN, size_t, T&> || is_apply_invocable_v<FN, T>
        {
            return for_each_reverse(0, _size(), std::forward<FN>(iterate_fn));
        }

        /**
         * @brief Applies a function to each element in the array in reverse order, starting from a given position.
         *
         * This function iterates over the elements in the array in reverse order, starting from the specified `start_pos`, and applies the given function `iterate_fn` to each element. The function can modify the elements in the array.
         *
         * @tparam FN The type of the function to apply.
         * @param start_pos The starting index from which to apply the function in reverse order.
         * @param iterate_fn The function to apply to each element.
         * @return A reference to this `list_array` after the function has been applied to all elements.
         * @throws std::invalid_argument If `start_pos` is greater than size of array.
         */
        template <class FN>
        constexpr list_array<T, Allocator>& for_each_reverse(size_t start_pos, FN&& iterate_fn) &
            requires std::is_invocable_v<FN, T&> || std::is_invocable_v<FN, size_t, T&> || is_apply_invocable_v<FN, T>
        {
            return for_each_reverse(start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        /**
         * @brief Applies a function to each element in the array in reverse order within a specified range.
         *
         * This function iterates over the elements in the array in reverse order, within the range from `start_pos` to `end_pos`, and applies the given function `iterate_fn` to each element. The function can modify the elements in the array.
         *
         * @tparam FN The type of the function to apply.
         * @param start_pos The starting index of the range to apply the function to (inclusive).
         * @param end_pos The ending index of the range to apply the function to (exclusive).
         * @param iterate_fn The function to apply to each element.
         * @return A reference to this `list_array` after the function has been applied to all elements.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
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

        /**
         * @brief Applies a function to each element in the array in reverse order (move version).
         *
         * This function is similar to the lvalue reference version of `for_each_reverse`, but it operates on an rvalue reference (temporary) of the `list_array`. It applies the given function `iterate_fn` to each element in reverse order and then moves the modified elements into a new `list_array` object.
         *
         * @tparam FN The type of the function to apply.
         * @param iterate_fn The function to apply to each element.
         */
        template <class FN>
        constexpr void for_each_reverse(FN&& iterate_fn) &&
            requires std::is_invocable_v<FN, T&&> || std::is_invocable_v<FN, size_t, T&&> || is_apply_invocable_v<FN, T>
        {
            std::move(*this).for_each_reverse(0, _size(), std::forward<FN>(iterate_fn));
        }

        /**
         * @brief Applies a function to each element in the array in reverse order, starting from a given position (move version).
         *
         * This function is similar to the lvalue reference version of `for_each_reverse(size_t, FN&&)`, but it operates on an rvalue reference (temporary) of the `list_array`. It applies the given function `iterate_fn` to each element in reverse order, starting from the specified `start_pos`, and then moves the modified elements into a new `list_array` object.
         *
         * @tparam FN The type of the function to apply.
         * @param start_pos The starting index from which to apply the function in reverse order.
         * @param iterate_fn The function to apply to each element.
         * @throws std::invalid_argument If `start_pos` is greater than size of array.
         */
        template <class FN>
        constexpr void for_each_reverse(size_t start_pos, FN&& iterate_fn) &&
            requires std::is_invocable_v<FN, T&&> || std::is_invocable_v<FN, size_t, T&&> || is_apply_invocable_v<FN, T>
        {
            std::move(*this).for_each_reverse(start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        /**
         * @brief Applies a function to each element in the array in reverse order within a specified range (move version).
         *
         * This function is similar to the lvalue reference version of `for_each_reverse(size_t, size_t, FN&&)`, but it operates on an rvalue reference (temporary) of the `list_array`. It applies the given function `iterate_fn` to each element in reverse order, within the range from `start_pos` to `end_pos`, and then moves the modified elements into a new `list_array` object.
         *
         * @tparam FN The type of the function to apply.
         * @param start_pos The starting index of the range to apply the function to (inclusive).
         * @param end_pos The ending index of the range to apply the function to (exclusive).
         * @param iterate_fn The function to apply to each element.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
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

        /**
         * @brief Applies a function to each element in the array in reverse order.
         *
         * This function iterates over the elements in the array in reverse order (from the last element to the first) and applies the given function `iterate_fn` to each element. The function cannot modify the elements in the array.
         *
         * @tparam FN The type of the function to apply. It must be invocable with `const T&` or `size_t, const T&` (index, element) or accept const reference to tuple/pair elements.
         * @param iterate_fn The function to apply to each element.
         * @return A reference to this `list_array` after the function has been applied to all elements.
         */
        template <class FN>
        constexpr const list_array<T, Allocator>& for_each_reverse(FN&& iterate_fn) const&
            requires std::is_invocable_v<FN, const T&> || std::is_invocable_v<FN, size_t, const T&> || is_apply_invocable_v<FN, T>
        {
            return for_each_reverse(0, _size(), std::forward<FN>(iterate_fn));
        }

        /**
         * @brief Applies a function to each element in the array in reverse order, starting from a given position.
         *
         * This function iterates over the elements in the array in reverse order, starting from the specified `start_pos`, and applies the given function `iterate_fn` to each element. The function cannot modify the elements in the array.
         *
         * @tparam FN The type of the function to apply.
         * @param start_pos The starting index from which to apply the function in reverse order.
         * @param iterate_fn The function to apply to each element.
         * @return A reference to this `list_array` after the function has been applied to all elements.
         * @throws std::invalid_argument If `start_pos` is greater than size of array.
         */
        template <class FN>
        constexpr const list_array<T, Allocator>& for_each_reverse(size_t start_pos, FN&& iterate_fn) const&
            requires std::is_invocable_v<FN, const T&> || std::is_invocable_v<FN, size_t, const T&> || is_apply_invocable_v<FN, T>
        {
            return for_each_reverse(start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        /**
         * @brief Applies a function to each element in the array in reverse order within a specified range.
         *
         * This function iterates over the elements in the array in reverse order, within the range from `start_pos` to `end_pos`, and applies the given function `iterate_fn` to each element. The function cannot modify the elements in the array.
         *
         * @tparam FN The type of the function to apply.
         * @param start_pos The starting index of the range to apply the function to (inclusive).
         * @param end_pos The ending index of the range to apply the function to (exclusive).
         * @param iterate_fn The function to apply to each element.
         * @return A reference to this `list_array` after the function has been applied to all elements.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
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

        /**
         * @brief Applies a function to each element in the array.
         *
         * This function iterates over the elements in the array and applies the given function `iterate_fn` to each element. The function can modify the elements in the array.
         *
         * @tparam FN The type of the function to apply. It must be invocable with `T&` or `size_t, T&` (index, element) or accept reference to tuple/pair elements.
         * @param iterate_fn The function to apply to each element.
         * @return A reference to this `list_array` after the function has been applied to all elements.
         */
        template <class FN>
        constexpr list_array<T, Allocator>& for_each(FN&& iterate_fn) &
            requires std::is_invocable_v<FN, T&> || std::is_invocable_v<FN, size_t, T&> || is_apply_invocable_v<FN, T>
        {
            return for_each(0, _size(), std::forward<FN>(iterate_fn));
        }

        /**
         * @brief Applies a function to each element in the array.
         *
         * This function iterates over the elements in the array and applies the given function `iterate_fn` to each element. The function can modify the elements in the array.
         *
         * @tparam FN The type of the function to apply. It must be invocable with `T&` or `size_t, T&` (index, element) or accept reference to tuple/pair elements.
         * @param iterate_fn The function to apply to each element.
         * @return A reference to this `list_array` after the function has been applied to all elements.
         * @throws std::invalid_argument If `start_pos` is greater than size of array.
         */
        template <class FN>
        constexpr list_array<T, Allocator>& for_each(size_t start_pos, FN&& iterate_fn) &
            requires std::is_invocable_v<FN, T&> || std::is_invocable_v<FN, size_t, T&> || is_apply_invocable_v<FN, T>
        {
            return for_each(start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        /**
         * @brief Applies a function to each element in the array within a specified range.
         *
         * This function iterates over the elements in the array, within the range from `start_pos` to `end_pos`, and applies the given function `iterate_fn` to each element. The function can modify the elements in the array.
         *
         * @tparam FN The type of the function to apply.
         * @param start_pos The starting index of the range to apply the function to (inclusive).
         * @param end_pos The ending index of the range to apply the function to (exclusive).
         * @param iterate_fn The function to apply to each element.
         * @return A reference to this `list_array` after the function has been applied to all elements.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
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

        /**
         * @brief Applies a function to each element in the array (move version).
         *
         * This function is similar to the lvalue reference version of `for_each`, but it operates on an rvalue reference (temporary) of the `list_array`.
         *
         * @tparam FN The type of the function to apply.
         * @param iterate_fn The function to apply to each element.
         */
        template <class FN>
        constexpr void for_each(FN&& iterate_fn) &&
            requires std::is_invocable_v<FN, T&&> || std::is_invocable_v<FN, size_t, T&&> || is_apply_invocable_v<FN, T>
        {
            std::move(*this).for_each(0, _size(), std::forward<FN>(iterate_fn));
        }

        /**
         * @brief Applies a function to each element in the array, starting from a given position (move version).
         *
         * This function is similar to the lvalue reference version of `for_each(size_t, FN&&)`, but it operates on an rvalue reference (temporary) of the `list_array`.
         *
         * @tparam FN The type of the function to apply.
         * @param start_pos The starting index from which to apply the function.
         * @param iterate_fn The function to apply to each element.
         * @throws std::invalid_argument If `start_pos` is greater than size of array.
         */
        template <class FN>
        constexpr void for_each(size_t start_pos, FN&& iterate_fn) &&
            requires std::is_invocable_v<FN, T&&> || std::is_invocable_v<FN, size_t, T&&> || is_apply_invocable_v<FN, T>
        {
            std::move(*this).for_each(start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        /**
         * @brief Applies a function to each element in the array within a specified range (move version).
         *
         * This function is similar to the lvalue reference version of `for_each(size_t, size_t, FN&&)`, but it operates on an rvalue reference (temporary) of the `list_array`.
         *
         * @tparam FN The type of the function to apply.
         * @param start_pos The starting index of the range to apply the function to (inclusive).
         * @param end_pos The ending index of the range to apply the function to (exclusive).
         * @param iterate_fn The function to apply to each element.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
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

        /**
         * @brief Applies a function to each element in the array.
         *
         * This function iterates over the elements in the array and applies the given function `iterate_fn` to each element. The function cannot modify the elements in the array.
         *
         * @tparam FN The type of the function to apply. It must be invocable with `const T&` or `size_t, const T&` (index, element).
         * @param iterate_fn The function to apply to each element.
         * @return A reference to this `list_array` after the function has been applied to all elements.
         */
        template <class FN>
        constexpr const list_array<T, Allocator>& for_each(FN&& iterate_fn) const&
            requires std::is_invocable_v<FN, const T&> || std::is_invocable_v<FN, size_t, const T&> || is_apply_invocable_v<FN, T>
        {
            return for_each(0, _size(), std::forward<FN>(iterate_fn));
        }

        /**
         * @brief Applies a function to each element in the array, starting from a given position.
         *
         * This function iterates over the elements in the array, starting from the specified `start_pos`, and applies the given function `iterate_fn` to each element. The function cannot modify the elements in the array.
         *
         * @tparam FN The type of the function to apply.
         * @param start_pos The starting index from which to apply the function.
         * @param iterate_fn The function to apply to each element.
         * @return A reference to this `list_array` after the function has been applied to all elements.
         * @throws std::invalid_argument If `start_pos` is greater than size of array.
         */
        template <class FN>
        constexpr const list_array<T, Allocator>& for_each(size_t start_pos, FN&& iterate_fn) const&
            requires std::is_invocable_v<FN, const T&> || std::is_invocable_v<FN, size_t, const T&> || is_apply_invocable_v<FN, T>
        {
            return for_each(start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        /**
         * @brief Applies a function to each element in the array within a specified range.
         *
         * This function iterates over the elements in the array, within the range from `start_pos` to `end_pos`, and applies the given function `iterate_fn` to each element. The function cannot modify the elements in the array.
         *
         * @tparam FN The type of the function to apply.
         * @param start_pos The starting index of the range to apply the function to (inclusive).
         * @param end_pos The ending index of the range to apply the function to (exclusive).
         * @param iterate_fn The function to apply to each element.
         * @return A reference to this `list_array` after the function has been applied to all elements.
         * @throws std::invalid_argument If `start_pos` is greater than `end_pos`.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
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

/// @}
#pragma endregion
#pragma region transform

        /// @name transform
        /// @{

        /**
         * @brief Transforms the elements of the `list_array` by applying the `iterate_fn` to each element, returning a new `list_array` with the transformed values.
         * @tparam FN The type of the function to apply to each element.
         * @param iterate_fn A callable that takes either `T&&` (element) or `size_t, T&&` (index, element) or unpacked elements form tuple/pair and returns transformed element.
         * @return A new `list_array` containing the transformed elements.
         */
        template <class FN>
        constexpr list_array<T, Allocator> transform(FN&& iterate_fn) &&
            requires std::is_invocable_r_v<T, FN, T&&> || std::is_invocable_r_v<T, FN, size_t, T&&> || is_apply_invocable_r_v<T, FN, T>
        {
            return std::move(*this).transform(0, _size(), std::forward<FN>(iterate_fn));
        }

        /**
         * @brief Transforms elements starting from `start_pos` by applying the `iterate_fn` to each element, returning a new `list_array` with the transformed values.
         * @tparam FN The type of the function to apply to each element.
         * @param start_pos The index of the first element to transform.
         * @param iterate_fn A callable that takes either `T&&` (element) or `size_t, T&&` (index, element) or unpacked elements form tuple/pair and returns transformed element.
         * @return A new `list_array` containing the transformed elements.
         * @throws std::invalid_argument If `start_pos` is greater than the size of the `list_array`.
         */
        template <class FN>
        constexpr list_array<T, Allocator> transform(size_t start_pos, FN&& iterate_fn) &&
            requires std::is_invocable_r_v<T, FN, T&&> || std::is_invocable_r_v<T, FN, size_t, T&&> || is_apply_invocable_r_v<T, FN, T>
        {
            return std::move(*this).transform(start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        /**
         * @brief (rvalue) Transforms elements in the range [`start_pos`, `end_pos`) by applying the `iterate_fn` to each element, returning a new `list_array` with the transformed values.
         * @tparam FN The type of the function to apply to each element.
         * @param start_pos The index of the first element to transform.
         * @param end_pos The index one past the last element to transform.
         * @param iterate_fn A callable that takes either `T&&` (element) or `size_t, T&&` (index, element) or unpacked elements form tuple/pair and returns transformed element.
         * @return A new `list_array` containing the transformed elements.
         * @throws std::invalid_argument If `end_pos` is less than or equal to `start_pos`.
         * @throws std::out_of_range If `end_pos` is greater than the size of the `list_array`.
         */
        template <class FN>
        constexpr list_array<T, Allocator> transform(size_t start_pos, size_t end_pos, FN&& iterate_fn) &&
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
            } else if constexpr (is_apply_invocable_v<FN, T>)
                for (T& i : range(start_pos, end_pos))
                    i = std::apply(iterate_fn, std::move(i));
            else
                for (T& i : range(start_pos, end_pos))
                    i = iterate_fn(std::move(i));
            return take(start_pos, end_pos);
        }

        /**
         * @brief Transforms the elements of the `list_array` in place by applying the `iterate_fn` to each element.
         * @tparam FN The type of the function to apply to each element.
         * @param iterate_fn A callable that takes either `T&&` (element) or `size_t, T&&` (index, element) or unpacked elements form tuple/pair and returns transformed element.
         * @return A reference to the modified `list_array`.
         * @note This function modifies the `list_array` in place and returns a reference to itself.
         */
        template <class FN>
        constexpr list_array<T, Allocator>& transform(FN&& iterate_fn) &
            requires std::is_invocable_r_v<T, FN, T&&> || std::is_invocable_r_v<T, FN, size_t, T&&> || is_apply_invocable_r_v<T, FN, T>
        {
            return *this = std::move(*this).transform(iterate_fn);
        }

        /**
         * @brief Transforms elements starting from `start_pos` in place by applying the `iterate_fn` to each element.
         * @tparam FN The type of the function to apply to each element.
         * @param start_pos The index of the first element to transform.
         * @param iterate_fn A callable that takes either `T&&` (element) or `size_t, T&&` (index, element) or unpacked elements form tuple/pair and returns transformed element.
         * @return A reference to the modified `list_array`.
         * @note This function modifies the `list_array` in place and returns a reference to itself.
         * @throws std::invalid_argument If `start_pos` is greater than the size of the `list_array`.
         */
        template <class FN>
        constexpr list_array<T, Allocator>& transform(size_t start_pos, FN&& iterate_fn) &
            requires std::is_invocable_r_v<T, FN, T&&> || std::is_invocable_r_v<T, FN, size_t, T&&> || is_apply_invocable_r_v<T, FN, T>
        {
            return *this = std::move(*this).transform(start_pos, iterate_fn);
        }

        /**
         * @brief Transforms elements in the range [`start_pos`, `end_pos`) in place by applying the `iterate_fn` to each element.
         * @tparam FN The type of the function to apply to each element.
         * @param start_pos The index of the first element to transform.
         * @param end_pos The index one past the last element to transform.
         * @param iterate_fn A callable that takes either `T&&` (element) or `size_t, T&&` (index, element) or unpacked elements form tuple/pair and returns transformed element.
         * @return A reference to the modified `list_array`.
         * @note This function modifies the `list_array` in place and returns a reference to itself.
         * @throws std::invalid_argument If `end_pos` is less than or equal to `start_pos`.
         * @throws std::out_of_range If `end_pos` is greater than the size of the `list_array`.
         */
        template <class FN>
        constexpr list_array<T, Allocator>& transform(size_t start_pos, size_t end_pos, FN&& iterate_fn) &
            requires std::is_invocable_r_v<T, FN, T&&> || std::is_invocable_r_v<T, FN, size_t, T&&> || is_apply_invocable_r_v<T, FN, T>
        {
            return *this = std::move(*this).transform(start_pos, end_pos, iterate_fn);
        }

/// @}
#pragma endregion
#pragma region convert

        /// @name convert
        /// @{


        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>>
        [[nodiscard]] constexpr result_array convert() const&
            requires std::is_convertible_v<ConvertTo, T>
        {
            return convert<ConvertTo>(0, _size());
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>>
        [[nodiscard]] constexpr result_array convert(size_t start_pos) const&
            requires std::is_convertible_v<ConvertTo, T>
        {
            return convert<ConvertTo>(start_pos, _size());
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>>
        [[nodiscard]] constexpr result_array convert(size_t start_pos, size_t end_pos) const&
            requires std::is_convertible_v<ConvertTo, T>
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
            requires std::is_convertible_v<ConvertTo, T>
        {
            return convert_take<ConvertTo, result_array>(0, _size());
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>>
        [[nodiscard]] constexpr result_array convert(size_t start_pos) &&
            requires std::is_convertible_v<ConvertTo, T>
        {
            return convert_take<ConvertTo, result_array>(start_pos, _size());
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>>
        [[nodiscard]] constexpr result_array convert(size_t start_pos, size_t end_pos) &&
            requires std::is_convertible_v<ConvertTo, T>
        {
            return convert_take<ConvertTo, result_array>(start_pos, end_pos);
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>>
        [[nodiscard]] constexpr result_array convert_take()
            requires std::is_convertible_v<ConvertTo, T>
        {
            return convert_take<ConvertTo, result_array>(0, _size());
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>>
        [[nodiscard]] constexpr result_array convert_take(size_t start_pos)
            requires std::is_convertible_v<ConvertTo, T>
        {
            return convert_take<ConvertTo, result_array>(start_pos, _size());
        }

        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>>
        [[nodiscard]] constexpr result_array convert_take(size_t start_pos, size_t end_pos)
            requires std::is_convertible_v<ConvertTo, T>
        {
            list_array<T, Allocator> tmp = take(start_pos, end_pos);
            result_array res;
            res.reserve(tmp.size());
            for (auto& i : tmp.range(start_pos, end_pos))
                res.push_back((ConvertTo)(std::move(i)));
            return res;
        }

        /**
         * @brief Converts elements in the array to a different type.
         *
         * This function applies the given function `iterate_fn` to each element in the entire array, converting them to a new type `ConvertTo`. The converted elements are stored in a new `result_array`.
         *
         * @tparam ConvertTo The type to convert the elements to.
         * @tparam result_array The type of the array to store the converted elements (defaults to `list_array<ConvertTo, std::allocator<ConvertTo>>`).
         * @tparam FN The type of the conversion function. It must be invocable with either `const T&` or `size_t, const T&` (index, element).
         * @param iterate_fn The conversion function to apply to each element.
         * @return A new `result_array` containing the converted elements.
         */
        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class FN>
        [[nodiscard]] constexpr result_array convert(FN&& iterate_fn) const&
            requires(
                (std::is_invocable_v<FN, const T&> && std::is_convertible_v<ConvertTo, std::invoke_result_t<FN, const T&>>)
                || (std::is_invocable_v<FN, const T&> && std::is_convertible_v<result_array, std::invoke_result_t<FN, const T&>>)
                || (std::is_invocable_v<FN, size_t, const T&> && std::is_convertible_v<ConvertTo, std::invoke_result_t<FN, size_t, const T&>>)
                || (std::is_invocable_v<FN, size_t, const T&> && std::is_convertible_v<result_array, std::invoke_result_t<FN, size_t, const T&>>)
            )
        {
            return convert<ConvertTo>(0, _size(), std::forward<FN>(iterate_fn));
        }

        /**
         * @brief Converts elements in the array to a different type, starting from a given position.
         *
         * This function applies the given function `iterate_fn` to each element in the array, starting from the specified `start_pos`, converting them to a new type `ConvertTo`. The converted elements are stored in a new `result_array`.
         *
         * @tparam ConvertTo The type to convert the elements to.
         * @tparam result_array The type of the array to store the converted elements (defaults to `list_array<ConvertTo, std::allocator<ConvertTo>>`).
         * @tparam FN The type of the conversion function. It must be invocable with either `const T&` or `size_t, const T&` (index, element).
         * @param start_pos The starting index of the range to convert (inclusive).
         * @param iterate_fn The conversion function to apply to each element.
         * @return A new `result_array` containing the converted elements.
         */
        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class FN>
        [[nodiscard]] constexpr result_array convert(size_t start_pos, FN&& iterate_fn) const&
            requires(
                (std::is_invocable_v<FN, const T&> && std::is_convertible_v<ConvertTo, std::invoke_result_t<FN, const T&>>)
                || (std::is_invocable_v<FN, const T&> && std::is_convertible_v<result_array, std::invoke_result_t<FN, const T&>>)
                || (std::is_invocable_v<FN, size_t, const T&> && std::is_convertible_v<ConvertTo, std::invoke_result_t<FN, size_t, const T&>>)
                || (std::is_invocable_v<FN, size_t, const T&> && std::is_convertible_v<result_array, std::invoke_result_t<FN, size_t, const T&>>)
            )
        {
            return convert<ConvertTo>(start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        /**
         * @brief Converts elements in the array to a different type.
         *
         * This function applies the given function `iterate_fn` to each element in the array within the specified range (`start_pos` to `end_pos`), converting them to a new type `ConvertTo`. The converted elements are stored in a new `result_array`.
         *
         * @tparam ConvertTo The type to convert the elements to.
         * @tparam result_array The type of the array to store the converted elements (defaults to `list_array<ConvertTo, std::allocator<ConvertTo>>`).
         * @tparam FN The type of the conversion function. It must be invocable with either `const T&` or `size_t, const T&` (index, element).
         * @param start_pos The starting index of the range to convert (inclusive).
         * @param end_pos The ending index of the range to convert (exclusive).
         * @param iterate_fn The conversion function to apply to each element.
         * @return A new `result_array` containing the converted elements.
         * @throws std::out_of_range If `end_pos` exceeds the size of the array.
         */
        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class FN>
        [[nodiscard]] constexpr result_array convert(size_t start_pos, size_t end_pos, FN&& iterate_fn) const&
            requires(
                (std::is_invocable_v<FN, const T&> && std::is_convertible_v<ConvertTo, std::invoke_result_t<FN, const T&>>)
                || (std::is_invocable_v<FN, const T&> && std::is_convertible_v<result_array, std::invoke_result_t<FN, const T&>>)
                || (std::is_invocable_v<FN, size_t, const T&> && std::is_convertible_v<ConvertTo, std::invoke_result_t<FN, size_t, const T&>>)
                || (std::is_invocable_v<FN, size_t, const T&> && std::is_convertible_v<result_array, std::invoke_result_t<FN, size_t, const T&>>)
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

        /**
         * @brief Converts elements in the array to a different type, starting from a given position.
         *
         * This function applies the given function `iterate_fn` to each element in the array, starting from the specified `start_pos`, converting them to a new type `ConvertTo`. The converted elements are stored in a new `result_array`.
         *
         * @tparam ConvertTo The type to convert the elements to.
         * @tparam result_array The type of the array to store the converted elements (defaults to `list_array<ConvertTo, std::allocator<ConvertTo>>`).
         * @tparam FN The type of the conversion function. It must be invocable with either `const T&` or `size_t, const T&` (index, element).
         * @param start_pos The starting index of the range to convert (inclusive).
         * @param iterate_fn The conversion function to apply to each element.
         * @return A new `result_array` containing the converted elements.
         */
        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class FN>
            [[nodiscard]] constexpr result_array convert(FN&& iterate_fn) && requires((std::is_invocable_v<FN, T&&> && std::is_convertible_v<ConvertTo, std::invoke_result_t<FN, T&&>>) || (std::is_invocable_v<FN, T&&> && std::is_convertible_v<result_array, std::invoke_result_t<FN, T&&>>) || (std::is_invocable_v<FN, size_t, T&&> && std::is_convertible_v<ConvertTo, std::invoke_result_t<FN, size_t, T&&>>) || (std::is_invocable_v<FN, size_t, T&&> && std::is_convertible_v<result_array, std::invoke_result_t<FN, size_t, T&&>>)) {
                return convert_take<ConvertTo, result_array>(0, _size(), std::forward<FN>(iterate_fn));
            }

            /**
         * @brief Converts elements in the array to a different type.
         *
         * This function applies the given function `iterate_fn` to each element in the entire array, converting them to a new type `ConvertTo`. The converted elements are stored in a new `result_array`.
         *
         * @tparam ConvertTo The type to convert the elements to.
         * @tparam result_array The type of the array to store the converted elements (defaults to `list_array<ConvertTo, std::allocator<ConvertTo>>`).
         * @tparam FN The type of the conversion function. It must be invocable with either `const T&` or `size_t, const T&` (index, element).
         * @param iterate_fn The conversion function to apply to each element.
         * @return A new `result_array` containing the converted elements.
         */
            template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class FN>
            [[nodiscard]] constexpr result_array convert(size_t start_pos, FN&& iterate_fn) && requires((std::is_invocable_v<FN, T&&> && std::is_convertible_v<ConvertTo, std::invoke_result_t<FN, T&&>>) || (std::is_invocable_v<FN, T&&> && std::is_convertible_v<result_array, std::invoke_result_t<FN, T&&>>) || (std::is_invocable_v<FN, size_t, T&&> && std::is_convertible_v<ConvertTo, std::invoke_result_t<FN, size_t, T&&>>) || (std::is_invocable_v<FN, size_t, T&&> && std::is_convertible_v<result_array, std::invoke_result_t<FN, size_t, T&&>>)) {
                return convert_take<ConvertTo, result_array>(start_pos, _size(), std::forward<FN>(iterate_fn));
            }

            /**
         * @brief Converts elements in the array to a different type (move version).
         *
         * This function is similar to the lvalue reference version of `convert`, but it operates on an rvalue reference (temporary) of the `list_array`. It moves the elements within the specified range (`start_pos` to `end_pos`), applies the conversion function `iterate_fn` to them, and stores the converted elements in a new `result_array`.
         *
         * @tparam ConvertTo The type to convert the elements to.
         * @tparam result_array The type of the array to store the converted elements (defaults to `list_array<ConvertTo, std::allocator<ConvertTo>>`).
         * @tparam FN The type of the conversion function. It must be invocable with either `const T&` or `size_t, const T&` (index, element).
         * @param start_pos The starting index of the range to convert (inclusive).
         * @param end_pos The ending index of the range to convert (exclusive).
         * @param iterate_fn The conversion function to apply to each element.
         * @return A new `result_array` containing the converted elements.
         */
            template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class FN>
            [[nodiscard]] constexpr result_array convert(size_t start_pos, size_t end_pos, FN&& iterate_fn) && requires((std::is_invocable_v<FN, T&&> && std::is_convertible_v<ConvertTo, std::invoke_result_t<FN, T&&>>) || (std::is_invocable_v<FN, T&&> && std::is_convertible_v<result_array, std::invoke_result_t<FN, T&&>>) || (std::is_invocable_v<FN, size_t, T&&> && std::is_convertible_v<ConvertTo, std::invoke_result_t<FN, size_t, T&&>>) || (std::is_invocable_v<FN, size_t, T&&> && std::is_convertible_v<result_array, std::invoke_result_t<FN, size_t, T&&>>)) {
                return convert_take<ConvertTo, result_array>(start_pos, end_pos, std::forward<FN>(iterate_fn));
            }

            /**
         * @brief Converts elements in the array to a different type and takes ownership (move version).
         *
         * This function is similar to the rvalue reference version of `convert`, but it also takes ownership of the elements in the entire array by moving them out. The converted elements are stored in a new `result_array`.
         *
         * @tparam ConvertTo The type to convert the elements to.
         * @tparam result_array The type of the array to store the converted elements (defaults to `list_array<ConvertTo, std::allocator<ConvertTo>>`).
         * @tparam FN The type of the conversion function. It must be invocable with either `const T&` or `size_t, const T&` (index, element).
         * @param iterate_fn The conversion function to apply to each element.
         * @return A new `result_array` containing the converted elements.
         */
            template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class FN>
            [[nodiscard]] constexpr result_array convert_take(FN&& iterate_fn)
                requires((std::is_invocable_v<FN, T &&> && std::is_convertible_v<ConvertTo, std::invoke_result_t<FN, T &&>>) || (std::is_invocable_v<FN, T &&> && std::is_convertible_v<result_array, std::invoke_result_t<FN, T &&>>) || (std::is_invocable_v<FN, size_t, T &&> && std::is_convertible_v<ConvertTo, std::invoke_result_t<FN, size_t, T &&>>) || (std::is_invocable_v<FN, size_t, T &&> && std::is_convertible_v<result_array, std::invoke_result_t<FN, size_t, T &&>>))
        {
            return convert_take<ConvertTo, result_array>(0, _size(), std::forward<FN>(iterate_fn));
        }

        /**
         * @brief Converts elements in the array to a different type and takes ownership (move version), starting from a given position.
         *
         * This function is similar to the rvalue reference version of `convert_take`, but it starts taking ownership and converting elements from the specified `start_pos`.
         *
         * @tparam ConvertTo The type to convert the elements to.
         * @tparam result_array The type of the array to store the converted elements (defaults to `list_array<ConvertTo, std::allocator<ConvertTo>>`).
         * @tparam FN The type of the conversion function. It must be invocable with either `const T&` or `size_t, const T&` (index, element).
         * @param start_pos The starting index of the range to convert (inclusive).
         * @param iterate_fn The conversion function to apply to each element.
         * @return A new `result_array` containing the converted elements.
         */
        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class FN>
        [[nodiscard]] constexpr result_array convert_take(size_t start_pos, FN&& iterate_fn)
            requires(
                (std::is_invocable_v<FN, T &&> && std::is_convertible_v<ConvertTo, std::invoke_result_t<FN, T &&>>)
                || (std::is_invocable_v<FN, T &&> && std::is_convertible_v<result_array, std::invoke_result_t<FN, T &&>>)
                || (std::is_invocable_v<FN, size_t, T &&> && std::is_convertible_v<ConvertTo, std::invoke_result_t<FN, size_t, T &&>>)
                || (std::is_invocable_v<FN, size_t, T &&> && std::is_convertible_v<result_array, std::invoke_result_t<FN, size_t, T &&>>)
            )
        {
            return convert_take<ConvertTo, result_array>(start_pos, _size(), std::forward<FN>(iterate_fn));
        }

        /**
         * @brief Converts elements in the array to a different type and takes ownership (move version).
         *
         * This function is similar to the rvalue reference version of `convert`, but it also takes ownership of the elements in the specified range (`start_pos` to `end_pos`) by moving them out of the original array. The converted elements are stored in a new `result_array`.
         *
         * @tparam ConvertTo The type to convert the elements to.
         * @tparam result_array The type of the array to store the converted elements (defaults to `list_array<ConvertTo, std::allocator<ConvertTo>>`).
         * @tparam FN The type of the conversion function. It must be invocable with either `const T&` or `size_t, const T&` (index, element).
         * @param start_pos The starting index of the range to convert (inclusive).
         * @param end_pos The ending index of the range to convert (exclusive).
         * @param iterate_fn The conversion function to apply to each element.
         * @return A new `result_array` containing the converted elements.
         */
        template <class ConvertTo, class result_array = list_array<ConvertTo, std::allocator<ConvertTo>>, class FN>
        [[nodiscard]] constexpr result_array convert_take(size_t start_pos, size_t end_pos, FN&& iterate_fn)
            requires(
                (std::is_invocable_v<FN, T &&> && std::is_convertible_v<ConvertTo, std::invoke_result_t<FN, T &&>>)
                || (std::is_invocable_v<FN, T &&> && std::is_convertible_v<result_array, std::invoke_result_t<FN, T &&>>)
                || (std::is_invocable_v<FN, size_t, T &&> && std::is_convertible_v<ConvertTo, std::invoke_result_t<FN, size_t, T &&>>)
                || (std::is_invocable_v<FN, size_t, T &&> && std::is_convertible_v<result_array, std::invoke_result_t<FN, size_t, T &&>>)
            )
        {
            list_array<T, Allocator> tmp = take(start_pos, end_pos);
            result_array res;
            res.reserve(tmp.size());
            if constexpr (std::is_invocable_r_v<ConvertTo, FN, size_t, T&&>) {
                size_t pos = start_pos;
                for (auto& i : tmp.range(start_pos, end_pos))
                    res.push_back(pos++, std::forward<FN>(iterate_fn)(std::move(i)));
            } else
                for (auto& i : tmp.range(start_pos, end_pos))
                    res.push_back(iterate_fn(std::move(i)));
            return res;
        }

/// @}
#pragma endregion
#pragma region erase

        /// @name erase
        /// @{
        /**
         * @brief Removes an element at the specified index.
         *
         * This function erases the element at the given `where` index from the array. If the index is 0, the element is removed from the beginning (equivalent to `pop_front`). If the index is the last element, it is removed from the end (equivalent to `pop_back`).
         *
         * @param where The index of the element to erase.
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        constexpr list_array<T, Allocator>& erase(size_t where) & {
            if (where == 0)
                pop_front();
            else if (where == _size() - 1)
                pop_back();
            else
                erase_range(get_iterator(where), get_iterator(where + 1));
            return *this;
        }

        /**
         * @brief Removes a range of elements from the array.
         *
         * This function erases elements from the array starting at `begin` and up to (but not including) `end`. The indices `begin` and `end` must be valid.
         *
         * @param begin The starting index of the range to erase (inclusive).
         * @param end The ending index of the range to erase (exclusive).
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        constexpr list_array<T, Allocator>& erase(size_t begin, size_t end) & {
            erase_range(get_iterator(begin), get_iterator(end));
            return *this;
        }

        /**
         * @brief Removes an element at the specified index (move version).
         *
         * This function is similar to the lvalue reference version of `erase(size_t)`, but it operates on an rvalue reference (temporary) of the `list_array`. It erases the element at the given index and then returns the modified array by value.
         *
         * @param where The index of the element to erase.
         * @return A new `list_array` object with the element at the specified index removed.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> erase(size_t where) && {
            erase(where);
            return std::move(*this);
        }

        /**
         * @brief Removes a range of elements from the array (move version).
         *
         * This function is similar to the lvalue reference version of `erase(size_t, size_t)`, but it operates on an rvalue reference (temporary) of the `list_array`. It erases the elements within the specified range and then returns the modified array by value.
         *
         * @param begin The starting index of the range to erase (inclusive).
         * @param end The ending index of the range to erase (exclusive).
         * @return A new `list_array` object with the elements in the specified range removed.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> erase(size_t begin, size_t end) && {
            erase(begin, end);
            return std::move(*this);
        }

/// @}
#pragma endregion
#pragma region starts/ends with

        /// @name starts/ends with
        /// @{
        /**
         * @brief Checks if the array starts with a specific value.
         *
         * This function checks if the array starts with the given `condition` value at the specified `start_pos`.
         *
         * @param condition The value to check for at the beginning.
         * @param start_pos The starting index to check from (defaults to 0).
         * @return `true` if the array starts with the `condition` value at `start_pos`, `false` otherwise.
         */
        template <size_t condition_size>
        [[nodiscard]] constexpr bool starts_with(const T (&condition)[condition_size], size_t start_pos = 0) const&
            requires std::equality_comparable<T>
        {
            return starts_with(condition, condition_size, start_pos);
        }

        /**
         * @brief Checks if the array starts with a specific C-style array.
         *
         * This function checks if the array starts with the elements in the given C-style array `condition` at the specified `start_pos`.
         *
         * @tparam condition_size The size of the C-style array `condition`.
         * @param condition The C-style array to check for at the beginning.
         * @param start_pos The starting index to check from (defaults to 0).
         * @return `true` if the array starts with the `condition` array at `start_pos`, `false` otherwise.
         */
        [[nodiscard]] constexpr bool starts_with(const T* condition, size_t condition_size, size_t start_pos = 0) const&
            requires std::equality_comparable<T>
        {
            if (start_pos >= _size())
                return false;
            if (condition_size > _size() - start_pos)
                return false;
            for (size_t i = 0; i < condition_size; i++)
                if (operator[](start_pos + i) != condition[i])
                    return false;
            return true;
        }

        /**
         * @brief Checks if the array starts with a specific raw array.
         *
         * This function checks if the array starts with the elements in the given raw array `condition` (with size `condition_size`) at the specified `start_pos`.
         *
         * @param condition A pointer to the beginning of the raw array to check for.
         * @param condition_size The size of the raw array `condition`.
         * @param start_pos The starting index to check from (defaults to 0).
         * @return `true` if the array starts with the `condition` array at `start_pos`, `false` otherwise.
         */
        [[nodiscard]] constexpr bool starts_with(const T& condition, size_t start_pos = 0) const&
            requires std::equality_comparable<T>
        {
            if (start_pos >= _size())
                return false;
            return operator[](start_pos) == condition;
        }

        /**
         * @brief Checks if the array starts with another `list_array`.
         *
         * This function checks if the array starts with the elements in the given `list_array` `condition` at the specified `start_pos`.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `condition`.
         * @param condition The `list_array` to check for at the beginning.
         * @param start_pos The starting index to check from (defaults to 0).
         * @return `true` if the array starts with the `condition` list_array at `start_pos`, `false` otherwise.
         */
        template <class AnyAllocator>
        [[nodiscard]] constexpr bool starts_with(const list_array<T, AnyAllocator>& condition, size_t start_pos = 0) const&
            requires std::equality_comparable<T>
        {
            if (start_pos >= _size())
                return false;
            if (condition.size() > _size() - start_pos)
                return false;
            for (size_t i = 0; i < condition.size(); i++)
                if (operator[](start_pos + i) != condition[i])
                    return false;
            return true;
        }

        /**
         * @brief Checks if the array ends with a specific C-style array.
         *
         * This function checks if the array ends with the elements in the given C-style array `condition`.
         *
         * @tparam condition_size The size of the C-style array `condition`.
         * @param condition The C-style array to check for at the end.
         * @return `true` if the array ends with the `condition` array, `false` otherwise.
         */
        template <size_t condition_size>
        [[nodiscard]] constexpr bool ends_with(const T (&condition)[condition_size]) const&
            requires std::equality_comparable<T>
        {
            return ends_with<condition_size>(condition, condition_size, _size());
        }

        /**
         * @brief Checks if the array ends with a specific C-style array within a specified range.
         *
         * This function checks if the array ends with the elements in the given C-style array `condition`, considering elements up to the specified `end_pos`.
         *
         * @tparam condition_size The size of the C-style array `condition`.
         * @param condition The C-style array to check for at the end.
         * @param end_pos The ending index to check up to (defaults to the size of the array).
         * @return `true` if the array ends with the `condition` array within the specified range, `false` otherwise.
         */
        template <size_t condition_size>
        [[nodiscard]] constexpr bool ends_with(const T (&condition)[condition_size], size_t end_pos) const&
            requires std::equality_comparable<T>
        {
            return ends_with<condition_size>(condition, condition_size, end_pos);
        }

        /**
         * @brief Checks if the array ends with a specific raw array.
         *
         * This function checks if the array ends with the elements in the given raw array `condition` (with size `condition_size`).
         *
         * @param condition A pointer to the beginning of the raw array to check for.
         * @param condition_size The size of the raw array `condition`.
         * @return `true` if the array ends with the `condition` array, `false` otherwise.
         */
        [[nodiscard]] constexpr bool ends_with(const T* condition, size_t condition_size) const&
            requires std::equality_comparable<T>
        {
            return ends_with(condition, condition_size, _size());
        }

        /**
         * @brief Checks if the array ends with a specific raw array within a specified range.
         *
         * This function checks if the array ends with the elements in the given raw array `condition` (with size `condition_size`), considering elements up to the specified `end_pos`.
         *
         * @param condition A pointer to the beginning of the raw array to check for.
         * @param condition_size The size of the raw array `condition`.
         * @param end_pos The ending index to check up to (defaults to the size of the array).
         * @return `true` if the array ends with the `condition` array within the specified range, `false` otherwise.
         */
        [[nodiscard]] constexpr bool ends_with(const T* condition, size_t condition_size, size_t end_pos) const&
            requires std::equality_comparable<T>
        {
            if (end_pos >= condition_size)
                return false;
            if (condition_size > end_pos)
                return false;
            for (size_t i = 0; i < condition_size; i++)
                if (operator[](end_pos - i - 1) != condition[condition_size - i - 1])
                    return false;
            return true;
        }

        /**
         * @brief Checks if the array ends with a specific value.
         *
         * This function checks if the array ends with the given `condition` value.
         *
         * @param condition The value to check for at the end.
         * @return `true` if the array ends with the `condition` value, `false` otherwise.
         */
        [[nodiscard]] constexpr bool ends_with(const T& condition) const&
            requires std::equality_comparable<T>
        {
            return ends_with(condition, _size());
        }

        /**
         * @brief Checks if the array ends with a specific value within a specified range.
         *
         * This function checks if the array ends with the given `condition` value, considering elements up to the specified `end_pos`.
         *
         * @param condition The value to check for at the end.
         * @param end_pos The ending index to check up to (defaults to the size of the array).
         * @return `true` if the array ends with the `condition` value within the specified range, `false` otherwise.
         */
        [[nodiscard]] constexpr bool ends_with(const T& condition, size_t end_pos) const&
            requires std::equality_comparable<T>
        {
            if (end_pos >= _size())
                return false;
            return operator[](end_pos - 1) == condition;
        }

        /**
         * @brief Checks if the array ends with another `list_array`.
         *
         * This function checks if the array ends with the elements in the given `list_array` `condition`.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `condition`.
         * @param condition The `list_array` to check for at the end.
         * @return `true` if the array ends with the `condition` list_array, `false` otherwise.
         */
        template <class AnyAllocator>
        [[nodiscard]] constexpr bool ends_with(const list_array<T, AnyAllocator>& condition) const&
            requires std::equality_comparable<T>
        {
            return ends_with(condition, _size());
        }

        /**
         * @brief Checks if the array ends with another `list_array` within a specified range.
         *
         * This function checks if the array ends with the elements in the given `list_array` `condition`, considering elements up to the specified `end_pos`.
         *
         * @tparam AnyAllocator The allocator type of the `list_array` `condition`.
         * @param condition The `list_array` to check for at the end.
         * @param end_pos The ending index to check up to (defaults to the size of the array).
         * @return `true` if the array ends with the `condition` list_array within the specified range, `false` otherwise.
         */
        template <class AnyAllocator>
        [[nodiscard]] constexpr bool ends_with(const list_array<T, AnyAllocator>& condition, size_t end_pos) const&
            requires std::equality_comparable<T>
        {
            if (end_pos >= _size())
                return false;
            if (condition.size() > end_pos)
                return false;
            for (size_t i = 0; i < condition.size(); i++)
                if (operator[](end_pos - i - 1) != condition[condition.size() - i - 1])
                    return false;
            return true;
        }

/// @}
#pragma endregion
#pragma region replace

        /// @name replace
        /// @{

        /**
         * @brief Replaces all occurrences of `target` with `with`.
         * @param target The value to be replaced.
         * @param with The new value to insert.
         * @return A reference to the modified `list_array`.
         */
        constexpr list_array<T, Allocator>& replace(const T& target, const T& with) &
            requires std::equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return for_each([target, with](T& it) {
                if (it == target)
                    it = with;
            });
        }

        /**
         * @brief Replaces all occurrences of `target` with `with`, returning a new `list_array`.
         * @param target The value to be replaced.
         * @param with The new value to insert.
         * @return A new `list_array` with the replacements made.
         */
        constexpr list_array<T, Allocator> replace(const T& target, const T& with) &&
            requires std::equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, with).take();
        }

        /**
         * @brief Replaces all occurrences of `target` with elements from the C array `with`.
         * @param target The value to be replaced.
         * @param with A pointer to the first element of the replacement C array.
         * @param with_size The number of elements in the replacement C array.
         * @return A reference to the modified `list_array`.
         */
        constexpr list_array<T, Allocator>& replace(const T& target, const T* with, size_t with_size) &
            requires std::equality_comparable<T> && std::is_copy_constructible_v<T>
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

        /**
         * @brief Replaces all occurrences of `target` with elements from the C array `with`, returning a new `list_array`.
         * @param target The value to be replaced.
         * @param with A pointer to the first element of the replacement C array.
         * @param with_size The number of elements in the replacement C array.
         * @return A new `list_array` with the replacements made.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> replace(const T& target, const T* with, size_t with_size) &&
            requires std::equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, with, with_size).take();
        }

        /**
         * @brief Replaces all occurrences of the sequence defined by `target` (C array) with `with`.
         * @param target A pointer to the first element of the sequence to be replaced.
         * @param target_size The number of elements in the sequence to be replaced.
         * @param with The new value to insert.
         * @return A reference to the modified `list_array`.
         */
        constexpr list_array<T, Allocator>& replace(const T* target, size_t target_size, const T& with) &
            requires std::equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            auto cache_iter = end();
            size_t t = 0;
            for (ptrdiff_t i = size(); i > 0;) {
                --cache_iter;
                --i;
                if (*cache_iter != target[t])
                    t = 0;
                else if (t == target_size) {
                    erase(i, target_size);
                    insert(i, with);
                    cache_iter = get_iterator(i);
                }
            }
            return *this;
        }

        /**
         * @brief Replaces all occurrences of the sequence defined by `target` (C array) with `with`, returning a new `list_array`.
         * @param target A pointer to the first element of the sequence to be replaced.
         * @param target_size The number of elements in the sequence to be replaced.
         * @param with The new value to insert.
         * @return A new `list_array` with the replacements made.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> replace(const T* target, size_t target_size, const T& with) &&
            requires std::equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, target_size, with).take();
        }

        /**
         * @brief Replaces all occurrences of the sequence defined by `target` (C array) with elements from the C array `with`.
         * @param target A pointer to the first element of the sequence to be replaced.
         * @param target_size The number of elements in the sequence to be replaced.
         * @param with A pointer to the first element of the replacement C array.
         * @param with_size The number of elements in the replacement C array.
         * @return A reference to the modified `list_array`.
         */
        constexpr list_array<T, Allocator>& replace(const T* target, size_t target_size, const T* with, size_t with_size) &
            requires std::equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            auto cache_iter = end();
            size_t t = 0;
            for (ptrdiff_t i = size(); i > 0;) {
                --cache_iter;
                --i;
                if (*cache_iter != target[t])
                    t = 0;
                else if (t == target_size) {
                    erase(i, target_size);
                    insert(i, with, with_size);
                    cache_iter = get_iterator(i);
                }
            }
            return *this;
        }

        /**
         * @brief Replaces all occurrences of the sequence defined by `target` (C array) with elements from the C array `with`, returning a new `list_array`.
         * @param target A pointer to the first element of the sequence to be replaced.
         * @param target_size The number of elements in the sequence to be replaced.
         * @param with A pointer to the first element of the replacement C array.
         * @param with_size The number of elements in the replacement C array.
         * @return A new `list_array` with the replacements made.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> replace(const T* target, size_t target_size, const T* with, size_t with_size) &&
            requires std::equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, target_size, with, with_size).take();
        }

        /**
         * @brief Replaces all occurrences of `target` with elements from the `list_array` `with`.
         * @tparam AnyAllocator The allocator type of the `list_array` containing the replacement elements.
         * @param target The value to be replaced.
         * @param with The `list_array` containing the replacement elements.
         * @return A reference to the modified `list_array`.
         */
        template <class AnyAllocator>
        constexpr list_array<T, Allocator>& replace(const T& target, const list_array<T, AnyAllocator>& with) &
            requires std::equality_comparable<T> && std::is_copy_constructible_v<T>
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

        /**
         * @brief Replaces all occurrences of `target` with elements from the `list_array` `with`, returning a new `list_array`.
         * @tparam AnyAllocator The allocator type of the `list_array` containing the replacement elements.
         * @param target The value to be replaced.
         * @param with The `list_array` containing the replacement elements.
         * @return A new `list_array` with the replacements made.
         */
        template <class AnyAllocator>
        [[nodiscard]] constexpr list_array<T, Allocator> replace(const T& target, const list_array<T, AnyAllocator>& with) &&
            requires std::equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, with).take();
        }

        /**
         * @brief (lvalue) Replaces all occurrences of the sequence defined by `target` (`list_array`) with `with`.
         * @tparam AnyAllocator The allocator type of the `list_array` containing the sequence to be replaced.
         * @param target The `list_array` containing the sequence to be replaced.
         * @param with The new value to insert.
         * @return A reference to the modified `list_array`.
         */
        template <class AnyAllocator>
        constexpr list_array<T, Allocator>& replace(const list_array<T, AnyAllocator>& target, const T& with) &
            requires std::equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            auto cache_iter = end();
            size_t t = 0, target_size = target.size();

            for (ptrdiff_t i = size(); i > 0;) {
                --cache_iter;
                --i;
                if (*cache_iter != target[t])
                    t = 0;
                else if (t == target_size) {
                    erase(i, target_size);
                    insert(i, with);
                    cache_iter = get_iterator(i);
                }
            }
            return *this;
        }

        /**
         * @brief Replaces all occurrences of the sequence defined by `target` (`list_array`) with `with`, returning a new `list_array`.
         * @tparam AnyAllocator The allocator type of the `list_array` containing the sequence to be replaced.
         * @param target The `list_array` containing the sequence to be replaced.
         * @param with The new value to insert.
         * @return A new `list_array` with the replacements made.
         */
        template <class AnyAllocator>
        [[nodiscard]] constexpr list_array<T, Allocator> replace(const list_array<T, AnyAllocator>& target, const T& with) &&
            requires std::equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, with).take();
        }

        /**
         * @brief Replaces all occurrences of the sequence defined by `target` (`list_array`) with elements from the `list_array` `with`.
         * @tparam AnyAllocator0 The allocator type of the `list_array` containing the sequence to be replaced.
         * @tparam AnyAllocator1 The allocator type of the `list_array` containing the replacement elements.
         * @param target The `list_array` containing the sequence to be replaced.
         * @param with The `list_array` containing the replacement elements.
         * @return A reference to the modified `list_array`.
         */
        template <class AnyAllocator0, class AnyAllocator1>
        constexpr list_array<T, Allocator>& replace(const list_array<T, AnyAllocator0>& target, const list_array<T, AnyAllocator1>& with) &
            requires std::equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            auto cache_iter = end();
            size_t t = 0, target_size = target.size();
            for (ptrdiff_t i = size(); i > 0;) {
                --cache_iter;
                --i;
                if (*cache_iter != target[t])
                    t = 0;
                else if (t == target_size) {
                    erase(i, target_size);
                    insert(i, with);
                    cache_iter = get_iterator(i);
                }
            }
            return *this;
        }

        /**
         * @brief Replaces all occurrences of the sequence defined by `target` (`list_array`) with elements from the `list_array` `with`, returning a new `list_array`.
         * @tparam AnyAllocator0 The allocator type of the `list_array` containing the sequence to be replaced.
         * @tparam AnyAllocator1 The allocator type of the `list_array` containing the replacement elements.
         * @param target The `list_array` containing the sequence to be replaced.
         * @param with The `list_array` containing the replacement elements.
         * @return A new `list_array` with the replacements made.
         */
        template <class AnyAllocator0, class AnyAllocator1>
        [[nodiscard]] constexpr list_array<T, Allocator> replace(const list_array<T, AnyAllocator0>& target, const list_array<T, AnyAllocator1>& with) &&
            requires std::equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, with).take();
        }

        /**
         * @brief Replaces all occurrences of `target` with elements from the C array `with`.
         *        The size of the C array is automatically deduced.
         * @tparam with_size The size of the C array `with` (deduced automatically).
         * @param target The value to be replaced.
         * @param with A C array containing the replacement elements.
         * @return A reference to the modified `list_array`.
         */
        template <size_t with_size>
        constexpr list_array<T, Allocator>& replace(const T& target, const T (&with)[with_size]) &
            requires std::equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, with, with_size);
        }

        /**
         * @brief Replaces all occurrences of `target` with elements from the C array `with`, returning a new `list_array`.
         *        The size of the C array is automatically deduced.
         * @tparam with_size The size of the C array `with` (deduced automatically).
         * @param target The value to be replaced.
         * @param with A C array containing the replacement elements.
         * @return A new `list_array` with the replacements made.
         */
        template <size_t with_size>
        [[nodiscard]] constexpr list_array<T, Allocator> replace(const T& target, const T (&with)[with_size]) &&
            requires std::equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, with, with_size).take();
        }

        /**
         * @brief Replaces all occurrences of the sequence defined by `target` (C array) with `with`.
         *        The size of the `target` C array is automatically deduced.
         * @tparam target_size The size of the C array `target` (deduced automatically).
         * @param target A C array containing the sequence to be replaced.
         * @param with The new value to insert.
         * @return A reference to the modified `list_array`.
         */
        template <size_t target_size>
        constexpr list_array<T, Allocator>& replace(const T (&target)[target_size], const T& with) &
            requires std::equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, target_size, with);
        }

        /**
         * @brief Replaces all occurrences of the sequence defined by `target` (C array) with `with`, returning a new `list_array`.
         *        The size of the `target` C array is automatically deduced.
         * @tparam T The element type stored in the `list_array`.
         * @tparam target_size The size of the C array `target` (deduced automatically).
         * @param target A C array containing the sequence to be replaced.
         * @param with The new value to insert.
         * @return A new `list_array` with the replacements made.
         */
        template <size_t target_size>
        [[nodiscard]] constexpr list_array<T, Allocator> replace(const T (&target)[target_size], const T& with) &&
            requires std::equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, target_size, with).take();
        }

        /**
         * @brief Replaces all occurrences of the sequence defined by `target` (C array) with elements from the C array `with`.
         *        The sizes of both C arrays are automatically deduced.
         * @tparam target_size The size of the C array `target` (deduced automatically).
         * @tparam with_size The size of the C array `with` (deduced automatically).
         * @param target A C array containing the sequence to be replaced.
         * @param with A C array containing the replacement elements.
         * @return A reference to the modified `list_array`.
         */
        template <size_t target_size, size_t with_size>
        constexpr list_array<T, Allocator>& replace(const T (&target)[target_size], const T (&with)[with_size]) &
            requires std::equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, target_size, with, with_size);
        }

        /**
         * @brief Replaces all occurrences of the sequence defined by `target` (C array) with elements from the C array `with`, returning a new `list_array`.
         *        The sizes of both C arrays are automatically deduced.
         * @tparam target_size The size of the C array `target` (deduced automatically).
         * @tparam with_size The size of the C array `with` (deduced automatically).
         * @param target A C array containing the sequence to be replaced.
         * @param with A C array containing the replacement elements.
         * @return A new `list_array` with the replacements made.
         */
        template <size_t target_size, size_t with_size>
        [[nodiscard]] constexpr list_array<T, Allocator> replace(const T (&target)[target_size], const T (&with)[with_size]) &&
            requires std::equality_comparable<T> && std::is_copy_constructible_v<T>
        {
            return replace(target, target_size, with, with_size).take();
        }

        /**
        * @brief Replaces elements with `with` if the `selector` function returns `true`.
        * @tparam FN The type of the selector function.
        * @param with The new value to insert.
        * @param selector A callable that takes either `T&` (element reference) or `size_t, T&` (index, element reference) and returns `bool`.
        * @return A reference to the modified `list_array`.
                *
        * The `selector` can optionally take the element's index as its first argument.
        * If the `selector` accepts the index, it will be provided during the replacement process.
        */
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

        /**
        * @brief Replaces elements with `with` if the `selector` function returns `true`,
        *        returning a new `list_array`.
        * @tparam FN The type of the selector function.
        * @param with The new value to insert.
        * @param selector A callable that takes either `T&` (element reference) or `size_t, T&` (index, element reference) and returns `bool`.
        * @return A new `list_array` with the replacements made.
                *
        * The `selector` can optionally take the element's index as its first argument.
        * If the `selector` accepts the index, it will be provided during the replacement process.
        */
        template <class FN>
        constexpr list_array<T, Allocator> replace_if(const T& with, FN&& selector) &&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, T&> || std::is_invocable_r_v<bool, FN, size_t, T&>)
        {
            return replace_if(with, std::forward<FN>(selector)).take();
        }

        /**
        * @brief Replaces elements with elements from the C array `with` if the `selector` function returns `true`.
        * @tparam FN The type of the selector function.
        * @param with A pointer to the first element of the replacement C array.
        * @param with_size The number of elements in the replacement C array.
        * @param selector A callable that takes either `T&` (element reference) or `size_t, T&` (index, element reference) and returns `bool`.
        * @return A reference to the modified `list_array`.
                *
        * The `selector` can optionally take the element's index as its first argument.
        * If the `selector` accepts the index, it will be provided during the replacement process.
        */
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

        /**
        * @brief Replaces elements with elements from the C array `with` if the `selector` function returns `true`,
        *        returning a new `list_array`.
        * @tparam FN The type of the selector function.
        * @param with A pointer to the first element of the replacement C array.
        * @param with_size The number of elements in the replacement C array.
        * @param selector A callable that takes either `T&` (element reference) or `size_t, T&` (index, element reference) and returns `bool`.
        * @return A new `list_array` with the replacements made.
                *
        * The `selector` can optionally take the element's index as its first argument.
        * If the `selector` accepts the index, it will be provided during the replacement process.
        */
        template <class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> replace_if(const T* with, size_t with_size, FN&& selector) &&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, T&> || std::is_invocable_r_v<bool, FN, size_t, T&>)
        {
            return replace_if(with, with_size, std::forward<FN>(selector)).take();
        }

        /**
        * @brief Replaces elements with elements from the `list_array` `with` if the `selector` function returns `true`.
        * @tparam AnyAllocator The allocator type of the `list_array` containing the replacement elements.
        * @tparam FN The type of the selector function.
        * @param with The `list_array` containing the replacement elements.
        * @param selector A callable that takes either `T&` (element reference) or `size_t, T&` (index, element reference) and returns `bool`.
        * @return A reference to the modified `list_array`.
                *
        * The `selector` can optionally take the element's index as its first argument.
        * If the `selector` accepts the index, it will be provided during the replacement process.
        */
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

        /**
        * @brief Replaces elements with elements from the `list_array` `with` if the `selector` function returns `true`,
        *        returning a new `list_array`.
        * @tparam AnyAllocator The allocator type of the `list_array` containing the replacement elements.
        * @tparam FN The type of the selector function.
        * @param with The `list_array` containing the replacement elements.
        * @param selector A callable that takes either `T&` (element reference) or `size_t, T&` (index, element reference) and returns `bool`.
        * @return A new `list_array` with the replacements made.
                *
        * The `selector` can optionally take the element's index as its first argument.
        * If the `selector` accepts the index, it will be provided during the replacement process.
        */
        template <class AnyAllocator, class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> replace_if(const list_array<T, AnyAllocator>& with, FN&& selector) &&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, T&> || std::is_invocable_r_v<bool, FN, size_t, T&>)
        {
            return replace_if(with, std::forward<FN>(selector)).take();
        }

        /**
         * @brief Replaces elements with elements from the C array `with` if the `selector` function returns `true`.
         *        The size of the C array is automatically deduced.
         * @tparam with_size The size of the C array `with` (deduced automatically).
         * @tparam FN The type of the selector function.
         * @param with A C array containing the replacement elements.
         * @param selector A callable that takes either `T&` (element reference) or `size_t, T&` (index, element reference) and returns `bool`.
         * @return A reference to the modified `list_array`.
         *
         * The `selector` can optionally take the element's index as its first argument.
         * If the `selector` accepts the index, it will be provided during the replacement process.
         */
        template <size_t with_size, class FN>
        constexpr list_array<T, Allocator>& replace_if(const T (&with)[with_size], FN&& selector) &
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, T&> || std::is_invocable_r_v<bool, FN, size_t, T&>)
        {
            return replace_if(with, with_size, std::forward<FN>(selector));
        }

        /**
         * @brief Replaces elements with elements from the C array `with` if the `selector` function returns `true`,
         *        returning a new `list_array`. The size of the C array is automatically deduced.
         * @tparam with_size The size of the C array `with` (deduced automatically).
         * @tparam FN The type of the selector function.
         * @param with A C array containing the replacement elements.
         * @param selector A callable that takes either `T&` (element reference) or `size_t, T&` (index, element reference) and returns `bool`.
         * @return A new `list_array` with the replacements made.
         *
         * The `selector` can optionally take the element's index as its first argument.
         * If the `selector` accepts the index, it will be provided during the replacement process.
         */
        template <size_t with_size, class FN>
        [[nodiscard]] constexpr list_array<T, Allocator> replace_if(const T (&with)[with_size], FN&& selector) &&
            requires std::is_copy_constructible_v<T> && (std::is_invocable_r_v<bool, FN, T&> || std::is_invocable_r_v<bool, FN, size_t, T&>)
        {
            return replace_if(with, with_size, std::forward<FN>(selector)).take();
        }

/// @}
#pragma endregion
#pragma region index and iterators

        /// @name index and iterators
        /// @{
        /**
         * @brief Returns an iterator to the beginning of the array.
         *
         * @return An iterator pointing to the first element.
         */
        [[nodiscard]] constexpr iterator begin() & noexcept {
            return get_iterator(0);
        }

        /**
         * @brief Returns an iterator to the end of the array.
         *
         * @return An iterator pointing past the last element.
         */
        [[nodiscard]] constexpr iterator end() & noexcept {
            return get_iterator(_size());
        }

        /**
         * @brief Returns a const_iterator to the beginning of the array.
         *
         * @return A const_iterator pointing to the first element.
         */
        [[nodiscard]] constexpr const_iterator begin() const& noexcept {
            return get_iterator(0);
        }

        /**
         * @brief Returns a const_iterator to the end of the array.
         *
         * @return A const_iterator pointing past the last element.
         */
        [[nodiscard]] constexpr const_iterator end() const& noexcept {
            return get_iterator(_size());
        }

        /**
         * @brief Returns a reverse iterator to the beginning of the reversed array.
         *
         * @return A reverse iterator pointing to the last element in the reversed order.
         */
        [[nodiscard]] constexpr reverse_iterator rbegin() & {
            return get_iterator(_size());
        }

        /**
         * @brief Returns a reverse iterator to the end of the reversed array.
         *
         * @return A reverse iterator pointing before the first element in the reversed order.
         */
        [[nodiscard]] constexpr reverse_iterator rend() & {
            return get_iterator(0);
        }

        /**
         * @brief Returns a const reverse iterator to the beginning of the reversed array.
         *
         * @return A const reverse iterator pointing to the last element in the reversed order.
         */
        [[nodiscard]] constexpr const_reverse_iterator rbegin() const& {
            return get_iterator(_size());
        }

        /**
         * @brief Returns a const reverse iterator to the end of the reversed array.
         *
         * @return A const reverse iterator pointing before the first element in the reversed order.
         */
        [[nodiscard]] constexpr const_reverse_iterator rend() const& {
            return get_iterator(0);
        }

        /**
         * @brief Returns a const_iterator to the beginning of the array.
         *
         * This is equivalent to `begin()` for const objects.
         *
         * @return A const_iterator pointing to the first element.
         */
        [[nodiscard]] constexpr const_iterator cbegin() const& {
            return get_iterator(0);
        }

        /**
         * @brief Returns a const_iterator to the end of the array.
         *
         * This is equivalent to `end()` for const objects.
         *
         * @return A const_iterator pointing past the last element.
         */
        [[nodiscard]] constexpr const_iterator cend() const& {
            return get_iterator(_size());
        }

        /**
         * @brief Returns a const reverse iterator to the beginning of the reversed array.
         *
         * @return A const reverse iterator pointing to the last element in the reversed order.
         */
        [[nodiscard]] constexpr const_reverse_iterator crbegin() const& {
            return get_iterator(_size());
        }

        /**
         * @brief Returns a const reverse iterator to the end of the reversed array.
         *
         * @return A const reverse iterator pointing before the first element in the reversed order.
         */
        [[nodiscard]] constexpr const_reverse_iterator crend() const& {
            return get_iterator(0);
        }

        /**
         * @brief Provides direct access to an element at a given index.
         *
         * This operator does not perform bounds checking. Use `at()` for safe access.
         *
         * @param index The index of the element to access.
         * @return A reference to the element at the given index.
         */
        [[nodiscard]] constexpr T& operator[](size_t index) & noexcept {
            return *get_element_at_index(index);
        }

        /**
         * @brief Provides const direct access to an element at a given index.
         *
         * This operator does not perform bounds checking. Use `at()` for safe access.
         *
         * @param index The index of the element to access.
         * @return A const reference to the element at the given index.
         */
        [[nodiscard]] constexpr const T& operator[](size_t index) const& noexcept {
            return *get_element_at_index(index);
        }

        /**
         * @brief Compares elements in array and returns true if every element in array is equal
         *
         * @tparam AnyAllocator The allocator type of the other `list_array`.
         * @param other The second array to compare
         * @return Comparaison result, true if equal
         */
        template <class AnyAllocator>
        [[nodiscard]] constexpr bool operator==(const list_array<T, AnyAllocator>& other) const
            requires std::equality_comparable<T>
        {
            if (size() != other.size())
                return false;

            auto _begin = begin();
            auto _end = end();
            auto _other_begin = other.begin();
            auto _other_end = other.end();

            while (_begin != _end && _other_begin != _other_end) {
                if (*_begin != *_other_begin)
                    return false;

                ++_begin;
                ++_other_begin;
            }
            return true;
        }

        /**
         * @brief Compares elements in array and returns true if arrays is differed
         *
         * @tparam AnyAllocator The allocator type of the other `list_array`.
         * @param other The second array to compare
         * @return Comparaison result, true if not equal
         */
        template <class AnyAllocator>
        [[nodiscard]] constexpr bool operator!=(const list_array<T, AnyAllocator>& other) const
            requires std::equality_comparable<T>
        {
            return !operator==(other);
        }

        /**
         * @brief Compares elements in array using `FN` function and returns true if every element in array is equal
         *
         * @tparam AnyAllocator The allocator type of the other `list_array`.
         * @param other The second array to compare
         * @return Comparaison result, true if equal
         */
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

        /**
         * @brief Compares elements in array using `FN` function and returns true if arrays is differed
         *
         * @tparam AnyAllocator The allocator type of the other `list_array`.
         * @param other The second array to compare
         * @return Comparaison result, true if not equal
         */
        template <class AnyAllocator, class FN>
        [[nodiscard]] constexpr bool not_equal(const list_array<T, AnyAllocator>& other, FN&& fn) const {
            return !equal(other, std::move(fn));
        }

        /**
         * @brief Returns a reference to the element at the specified index.
         *
         * @param index The index of the element to access.
         * @return A reference to the element at the given index.
         * @throws std::out_of_range If the index is out of bounds.
         */
        [[nodiscard]] constexpr T& at(size_t index) & {
            if (index >= _size())
                throw std::out_of_range("Index out of range");
            return *get_element_at_index(index);
        }

        /**
         * @brief Returns a const reference to the element at the specified index.
         *
         * @param index The index of the element to access.
         * @return A const reference to the element at the given index.
         * @throws std::out_of_range If the index is out of bounds.
         */
        [[nodiscard]] constexpr const T& at(size_t index) const& {
            if (index >= _size())
                throw std::out_of_range("Index out of range");
            return *get_element_at_index(index);
        }

        /**
         * @brief Returns a copy of the element at the specified index, or a default-constructed T if the index is out of bounds.
         *
         * This function provides a safe way to access elements without throwing exceptions. If the index is valid, it returns a copy of the element at that index. If the index is out of bounds, it returns a default-constructed T object.
         *
         * @param index The index of the element to access.
         * @return A copy of the element at the given index, or a default-constructed T if the index is out of bounds.
         * @note This function is noexcept if and only if T is both nothrow default constructible and nothrow copy constructible.
         */
        [[nodiscard]] constexpr T at_default(size_t index) const& noexcept(std::is_nothrow_default_constructible_v<T> && std::is_nothrow_copy_constructible_v<T>)
            requires std::is_default_constructible_v<T> && std::is_constructible_v<T>
        {
            if (index >= _size())
                return T{};
            return *get_element_at_index(index);
        }

        /**
         * @brief Returns an iterator to the element at the specified index.
         *
         * @param index The index of the element to get an iterator for.
         * @return An iterator pointing to the element at the given index.
         */
        [[nodiscard]] constexpr iterator get_iterator(size_t index) & noexcept {
            return get_iterator_at_index(index);
        }

        /**
         * @brief Returns a const_iterator to the element at the specified index.
         *
         * @param index The index of the element to get a const_iterator for.
         * @return A const_iterator pointing to the element at the given index.
         */
        [[nodiscard]] constexpr const_iterator get_iterator(size_t index) const& noexcept {
            return (const_iterator) const_cast<list_array<T, Allocator>*>(this)->get_iterator(index);
        }

        /**
         * @brief Returns a range_provider object representing a range of elements in the array.
         *
         * @param start The starting index of the range (inclusive).
         * @param end The ending index of the range (exclusive).
         * @return A range_provider object that can be used to iterate over the specified range.
         * @throws std::out_of_range If either `start` or `end` is out of bounds, or if `start` is greater than `end`.
         */
        [[nodiscard]] constexpr range_provider range(size_t start, size_t end) & {
            if (start > end)
                throw std::out_of_range("start > end");
            if (end > _size())
                throw std::out_of_range("end out of size limit");
            return range_provider(*this, start, end);
        }

        /**
         * @brief Returns a reverse_provider object representing a reversed range of elements in the array.
         *
         * @param start The starting index of the range in the original array (inclusive).
         * @param end The ending index of the range in the original array (exclusive).
         * @return A reverse_provider object that can be used to iterate over the specified range in reverse order.
         * @throws std::out_of_range If either `start` or `end` is out of bounds, or if `start` is greater than `end`.
         */
        [[nodiscard]] constexpr reverse_provider reverse_range(size_t start, size_t end) & {
            if (start > end)
                throw std::out_of_range("start > end");
            if (end > _size())
                throw std::out_of_range("end out of size limit");
            return range_provider(*this, start, end);
        }

        /**
         * @brief Returns a const_range_provider object representing a range of elements in the array.
         *
         * @param start The starting index of the range (inclusive).
         * @param end The ending index of the range (exclusive).
         * @return A const_range_provider object that can be used to iterate over the specified range.
         * @throws std::out_of_range If either `start` or `end` is out of bounds, or if `start` is greater than `end`.
         */
        [[nodiscard]] constexpr const_range_provider range(size_t start, size_t end) const& {
            if (start > end)
                throw std::out_of_range("start > end");
            if (end > _size())
                throw std::out_of_range("end out of size limit");

            return const_range_provider(*this, start, end);
        }

        /**
         * @brief Returns a const_reverse_provider object representing a reversed range of elements in the array.
         *
         * @param start The starting index of the range in the original array (inclusive).
         * @param end The ending index of the range in the original array (exclusive).
         * @return A const_reverse_provider object that can be used to iterate over the specified range in reverse order.
         * @throws std::out_of_range If either `start` or `end` is out of bounds, or if `start` is greater than `end`.
         */
        [[nodiscard]] constexpr const_reverse_provider reverse_range(size_t start, size_t end) const& {
            if (start > end)
                throw std::out_of_range("start > end");
            if (end > _size())
                throw std::out_of_range("end out of size limit");
            return const_range_provider(*this, start, end);
        }

        /**
         * @brief Returns a reverse_provider object representing the entire array in reverse order.
         *
         * @return A reverse_provider object that can be used to iterate over the entire array in reverse order.
         */
        [[nodiscard]] constexpr reverse_provider reverse() & {
            return *this;
        }

        /**
         * @brief Creates a new `list_array` object with the elements in reverse order.
         *
         * @return A new `list_array` object containing the elements of the original array in reverse order.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> reverse() && {
            list_array<T, Allocator> result(get_allocator());
            result.reserve(_size());
            for (auto& it : reverse())
                result.push_back(std::move(it));
            return result;
        }

        /**
         * @brief Returns a const_reverse_provider object representing the entire array in reverse order.
         *
         * @return A const_reverse_provider object that can be used to iterate over the entire array in reverse order.
         */
        [[nodiscard]] constexpr const_reverse_provider reverse() const& {
            return *this;
        }

/// @}
#pragma endregion
#pragma region size, capacity, reservation

        /// @name size, capacity, reservation
        /// @{
        /**
         * @brief Removes all elements from the container, deallocating memory.
         * 
         * This function destroys all elements in the `list_array` and resets its size,
         * capacity, and reserved space to zero.
         */
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

        /**
         * @brief Returns the number of elements in the container.
         * 
         * @return The number of elements currently stored in the `list_array`.
         */
        [[nodiscard]] constexpr size_t size() const noexcept {
            return _size();
        }

        /**
         * @brief Checks whether the container is empty.
         * 
         * @return `true` if the container contains no elements, `false` otherwise.
         */
        [[nodiscard]] constexpr bool empty() const noexcept {
            return _size() == 0;
        }

        /**
         * @brief Returns the number of elements the container can hold without reallocation.
         *
         * This includes both the currently used space and any reserved space.
         * 
         * @return The total capacity of the `list_array`.
         */
        [[nodiscard]] constexpr inline size_t capacity() const noexcept {
            return allocator_and_size.hold_value + _reserved_front + _reserved_back;
        }

        /**
         * @brief Returns the total reserved capacity of the list_array.
         *
         * @return size_t The total number of elements that can be held without reallocation.
         */
        [[nodiscard]] constexpr inline size_t reserved() const noexcept {
            return _reserved_front + _reserved_back;
        }

        /**
         * @brief Returns the reserved capacity of the front block of the list_array.
         *
         * @return size_t The number of elements that can be held in the front block without reallocation.
         */
        [[nodiscard]] constexpr inline size_t reserved_front() const noexcept {
            return _reserved_front;
        }

        /**
         * @brief Returns the reserved capacity of the back block of the list_array.
         *
         * @return size_t The number of elements that can be held in the back block without reallocation.
         */
        [[nodiscard]] constexpr inline size_t reserved_back() const noexcept {
            return _reserved_back;
        }

        /**
         * @brief Reserves additional space at the front of the container for efficient `push_front` operations.
         * 
         * @param size The amount of additional space to reserve at the front.
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        constexpr list_array<T, Allocator>& reserve_front(size_t size) & {
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

        /**
         * @brief Reserves additional space at the back of the container for efficient `push_back` operations.
         * 
         * @param size The amount of additional space to reserve at the back.
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        constexpr list_array<T, Allocator>& reserve_back(size_t size) & {
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

        /**
         * @brief Ensures there is enough reserved space at the front for a certain number of elements.
         * 
         * If there isn't enough space, it will either steal a block from the back (if possible) or 
         * reserve additional space at the front.
         *
         * @param size The required amount of reserved space at the front.
         * @param steal If true (default), allows stealing a block from the back if needed.
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
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

        /**
         * @brief Ensures there is enough reserved space at the back for a certain number of elements.
         * 
         * If there isn't enough space, it will either steal a block from the front (if possible) or 
         * reserve additional space at the back.
         *
         * @param size The required amount of reserved space at the back.
         * @param steal If true (default), allows stealing a block from the front if needed.
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
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

        /**
         * @brief Reserves space at the back of the container for efficient `push_back` operations.
         *
         * This is equivalent to calling `reserve_back(size)`.
         * 
         * @param size The total number of elements the container should be able to hold without reallocation.
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        constexpr list_array<T, Allocator>& reserve(size_t size) & {
            reserve_back(size);
            return *this;
        }

/// @}
#pragma endregion
#pragma region resize

        /// @name resize
        /// @{

        /**
         * @brief Resizes the container to the specified size, using default construction for new elements.
         * 
         * If `new_size` is smaller than the current size, elements are removed from the end. 
         * If `new_size` is larger, new elements are value-initialized (default constructed).
         *
         * @param new_size The new size of the container.
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        constexpr list_array<T, Allocator>& resize(size_t new_size) &
            requires std::is_default_constructible_v<T>
        {
            return resize(new_size, T());
        }

        /**
         * @brief Resizes the container to the specified size, copying a given value for new elements.
         * 
         * If `new_size` is smaller than the current size, elements are removed from the end.
         * If `new_size` is larger, new elements are copy-constructed from the provided `set` value.
         *
         * @param new_size The new size of the container.
         * @param set The value to copy-construct new elements from.
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        constexpr list_array<T, Allocator>& resize(size_t new_size, const T& set) & {
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

/// @}
#pragma endregion
#pragma region memory

        /// @name memory
        /// @{

        /**
         * @brief Releases any reserved space at the front of the container.
         *
         * This function iterates through the blocks at the front of the array that are part of the reserved space. If a block is entirely within the reserved space, it is destroyed and deallocated. If a block is partially within the reserved space, it is resized to exclude the reserved portion, and the remaining elements are copied to a new block. The reserved space at the front is then reset to zero.
         *
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        constexpr list_array<T, Allocator>& shrink_front() & {
            auto current = first_block;
            auto& allocator = allocator_and_size.get_allocator();
            while (current->next && _reserved_front) {
                auto next = current->next;
                if (current->data_size > _reserved_front) {
                    arr_block_manager<T, Allocator> manager(allocator);
                    auto iter = begin(); // Assuming 'begin' is accessible or passed as an argument
                    manager.add_block(manager.allocate_and_take_from(current->data_size - _reserved_front, iter));
                    next = current->next;
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

        /**
         * @brief Releases any reserved space at the back of the container.
         *
         * This function is similar to `shrink_front()`, but it operates on the blocks at the back of the array. It iterates through the blocks in reverse order, destroying and deallocating blocks that are entirely within the reserved space, and resizing blocks that are partially within the reserved space. The reserved space at the back is then reset to zero.
         *
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        constexpr list_array<T, Allocator>& shrink_back() & {
            auto current = last_block;
            auto& allocator = allocator_and_size.get_allocator();
            while (current->prev && _reserved_back) {
                auto prev = current->prev;
                if (current->data_size > _reserved_back) {
                    arr_block_manager<T, Allocator> manager(allocator);
                    auto iter = end() - (current->data_size); // Assuming 'end' is accessible or passed as an argument
                    manager.add_block(manager.allocate_and_take_from(current->data_size - _reserved_back, iter));
                    prev = current->prev;
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

        /**
         * @brief Releases all reserved space in the container.
         * 
         * This is equivalent to calling both `shrink_front()` and `shrink_back()`.
         * 
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        constexpr list_array<T, Allocator>& shrink_to_fit() & {
            if (_reserved_front)
                shrink_front();
            if (_reserved_back)
                shrink_back();
            return *this;
        }

        /**
         * @brief Combines all blocks into a single contiguous block for improved memory access speed.
         *
         * This operation may be expensive, so it's typically used after a series of insertions 
         * and deletions to optimize memory layout.
         * 
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
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

        /**
         * @brief Distributes elements across multiple blocks to improve insertion/deletion performance.
         *
         * This is the opposite of `commit()`. It can be used after a period of mostly random access 
         * to prepare the container for more insertions and deletions.
         *
         * @param total_blocks The desired number of blocks to distribute elements into.
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        constexpr list_array<T, Allocator>& decommit(size_t total_blocks) & {
            if (total_blocks == 0 || _size() == 0)
                return;
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

        /**
         * @brief Checks if committing the blocks would likely improve performance.
         *
         * This function is a heuristic to determine if calling `commit()` would be beneficial.
         *  It returns `true` if the container has more than two blocks,
         *  suggesting that consolidating the blocks into a single contiguous block
         *  might improve memory access speed.
         *
         * @return `true` if there are more than two blocks, `false` otherwise.
         */
        [[nodiscard]] constexpr bool need_commit() const noexcept {
            return blocks_more(2);
        }

        /**
         * @brief Checks if the container has more blocks than a given count.
         *
         * This is used to avoid unnecessary iteration over all blocks when checking 
         * conditions that depend on the number of blocks.
         * 
         * @param blocks_count The number of blocks to compare against.
         * @return `true` if there are more blocks than `blocks_count`, `false` otherwise.
         */
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

        /**
         * @brief Returns the total number of blocks in the container.
         * 
         * @return The count of blocks currently used by the `list_array`.
         */
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

/// @}
#pragma endregion
#pragma region view

        /// @name view
        /// @{
        /**
         * @brief Returns a raw pointer to the underlying data array, if possible.
         *
         * This function returns a pointer to the first element of the underlying contiguous data array, if the array is stored in a single block. If the array is fragmented across multiple blocks, commits it.
         *
         * @return A pointer to the first element of the underlying data array.
         */
        [[nodiscard]] constexpr T* data() & {
            if (blocks_more(1))
                commit();
            if (first_block)
                return first_block->data + _reserved_front;
            else
                return nullptr;
        }

        /**
         * @brief Returns a const raw pointer to the underlying data array, if possible.
         *
         * This function returns a const pointer to the first element of the underlying contiguous data array, if the array is stored in a single block. If the array is fragmented across multiple blocks, it throws a `std::runtime_error`.
         *
         * @return A const pointer to the first element of the underlying data array.
         * @throws std::runtime_error If the array is fragmented across multiple blocks.
         */
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

        /**
         * @brief Returns the maximum value in the array by moving it out.
         *
         * This function finds the maximum value in the array, moves it out of the array, and returns it by value. The array is modified as the maximum element is removed.
         *
         * @return The maximum value in the array.
         * @throws std::length_error If the array is empty.
         */
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

        /**
         * @brief Returns the minimum value in the array by moving it out.
         *
         * This function finds the minimum value in the array, moves it out of the array, and returns it by value. The array is modified as the minimum element is removed.
         *
         * @return The minimum value in the array.
         * @throws std::length_error If the array is empty.
         */
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

        /**
         * @brief Returns a const reference to the maximum value in the array.
         *
         * This function finds the maximum value in the array and returns a const reference to it. The array is not modified.
         *
         * @return A const reference to the maximum value in the array.
         * @throws std::length_error If the array is empty.
         */
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

        /**
         * @brief Returns a const reference to the minimum value in the array.
         *
         * This function finds the minimum value in the array and returns a const reference to it. The array is not modified.
         *
         * @return A const reference to the minimum value in the array.
         * @throws std::length_error If the array is empty.
         */
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

        /**
         * @brief Returns the maximum value in the array, or a default-constructed value if the array is empty.
         *
         * This function finds the maximum value in the array and returns a copy of it. If the array is empty, it returns a default-constructed T object.
         *
         * @return A copy of the maximum value in the array, or a default-constructed T if the array is empty.
         */
        [[nodiscard]] constexpr T max_default() const
            requires std::copy_constructible<T> && std::totally_ordered<T>
        {
            if (!_size())
                return T{};
            const T* max = &operator[](0);
            for (const T& it : *this)
                if (*max < it)
                    max = &it;
            return *max;
        }

        /**
         * @brief Returns the minimum value in the array, or a default-constructed value if the array is empty.
         *
         * This function finds the minimum value in the array and returns a copy of it. If the array is empty, it returns a default-constructed T object.
         *
         * @return A copy of the minimum value in the array, or a default-constructed T if the array is empty.
         */
        [[nodiscard]] constexpr T min_default() const
            requires std::copy_constructible<T> && std::totally_ordered<T>
        {
            if (!_size())
                return T{};
            const T* min = &operator[](0);
            for (const T& it : *this)
                if (*min > it)
                    min = &it;
            return *min;
        }

/// @}
#pragma endregion
#pragma region to_...

        /// @name cast to_...
        /// @{
        /**
         * @brief  Converts the list_array into a contiguous array.
         *
         * Creates a new array and copies all elements from the list_array into it. 
         * This is a const reference version, suitable when you don't want to move
         * the original list_array's contents.
         *
         * @param alloc The allocator to use for the new array (defaults to std::allocator<T>).
         * @return T* A pointer to the newly created array.
         */
        template <class LocalAllocator>
        [[nodiscard]] constexpr T* to_array(LocalAllocator alloc = std::allocator<T>()) const&
            requires std::is_copy_constructible_v<T>
        {
            T* tmp = alloc.allocate(_size());
            for_each([tmp](size_t index, const T& it) { std::construct_at(tmp + index, it); });
            return tmp;
        }

        /**
         * @brief Converts the list_array into a contiguous array (move version).
         *
         * This version moves elements out of the list_array, potentially leaving
         * the list_array empty. This is more efficient when you don't need the 
         * original list_array anymore.
         *
         * @param alloc The allocator to use for the new array (defaults to std::allocator<T>).
         * @return T* A pointer to the newly created array.
         */
        template <class LocalAllocator>
        [[nodiscard]] constexpr T* to_array(LocalAllocator alloc = std::allocator<T>()) &&
            requires std::is_copy_constructible_v<T>
        {
            T* tmp = alloc.allocate(_size());
            for_each([tmp](size_t index, T&& it) { std::construct_at(tmp + index, std::move(it)); });
            return tmp;
        }

        /**
         * @brief Converts the list_array into a specified `Container` type.
         *
         * Copies elements from the list_array into a new `Container`. 
         * This is the const reference version.
         *
         * @return Container A new container of the specified type, filled with the list_array's elements.
         */
        template <class Container>
        [[nodiscard]] constexpr Container to_container() const&
            requires is_container_v<Container>
        {
            Container copy_container;
            copy_container.resize(size());
            for_each([&copy_container](size_t i, const T& it) {
                copy_container[i] = it;
            });
            return copy_container;
        }

        /**
         * @brief Converts the list_array into a specified `Container` type (move version).
         *
         * Moves elements from the list_array into a new `Container`.
         *
         * @return Container A new container of the specified type, filled with the list_array's elements.
         */
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

/// @}
#pragma endregion
#pragma region flip

        /// @name flip
        /// @{
        /**
         * @brief Reverses the order of elements in the list_array.
         *
         * This is the lvalue reference version, modifying the original list_array.
         *
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        constexpr list_array<T, Allocator>& flip() & {
            return *this = take().flip();
        }

        /**
         * @brief Reverses the order of elements in the list_array (move version).
         *
         * This version returns a new, reversed list_array, leaving the original
         * unmodified.
         *
         * @return list_array<T, Allocator> A new list_array with reversed order.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> flip() && {
            list_array<T, Allocator> cache(get_allocator());
            cache.reserve(size());
            take().for_each_reverse([&cache](T&& item) {
                cache.push_back(std::move(item));
            });
            return cache;
        }

/// @}
#pragma endregion
#pragma region ordered_insert

        /// @name ordered_insert
        /// @{
        /**
         * @brief Inserts an element while maintaining the sorted order of the container.
         *
         * @param value The element to insert.
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        constexpr list_array<T, Allocator>& ordered_insert(const T& value) &
            requires std::totally_ordered<T>
        {
            if (_size() == 0) {
                push_back(value);
                return *this;
            }
            insert(std::lower_bound(begin(), end(), value).absolute_index, value);
            return *this;
        }

        /**
         * @brief Inserts an element while maintaining the sorted order of the container, using move semantics.
         *
         * @param value The element to move into the array.
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
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

        /**
         * @brief Inserts an element while maintaining the sorted order of the container.
         *
         * @param value The element to insert.
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
        [[nodiscard]] constexpr list_array<T, Allocator> ordered_insert(const T& value) &&
            requires std::totally_ordered<T>
        {
            if (_size() == 0) {
                push_back(value);
                return std::move(*this);
            }
            insert(std::lower_bound(begin(), end(), value).absolute_index, value);
            return std::move(*this);
        }

        /**
         * @brief Inserts an element while maintaining the sorted order of the container, using move semantics.
         *
         * @param value The element to move into the array.
         * @return list_array<T, Allocator>& A reference to the modified list_array.
         */
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

        /// @}

#pragma endregion

        /**
         * @brief Returns the allocator associated with the container.
         *
         * @return A reference to the allocator object used by the container.
         */
        [[nodiscard]] constexpr Allocator& get_allocator() & noexcept {
            return allocator_and_size.get_allocator();
        }

        /**
         * @brief Returns the allocator associated with the container.
         *
         * @return A const reference to the allocator object used by the container.
         */
        [[nodiscard]] constexpr const Allocator& get_allocator() const& noexcept {
            return allocator_and_size.get_allocator();
        }
    };
}

template <class T, class Allocator = std::allocator<T>>
using list_array = _list_array_impl::list_array<T, Allocator>;

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

    constexpr bit_list_array& push_back(bool val) {
        if (end_bit == 8) {
            arr.push_back(0);
            end_bit = 0;
            return *this;
        }
        arr[arr.size() - 1] |= val << end_bit++;
        return *this;
    }

    constexpr bit_list_array& pop_back() {
        if (end_bit == 0) {
            arr.pop_back();
            end_bit = 8;
            return *this;
        }
        end_bit--;
        arr[arr.size() - 1] &= ~(1 << end_bit);
        return *this;
    }

    constexpr bit_list_array& push_front(bool val) {
        if (begin_bit == 0) {
            arr.push_front(0);
            begin_bit = 8;
        }
        arr[0] |= val << --begin_bit;
        return *this;
    }

    constexpr bit_list_array& pop_front() {
        if (begin_bit == 8) {
            arr.pop_front();
            begin_bit = 0;
            return *this;
        }
        arr[0] &= ~(1 << begin_bit++);
        return *this;
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

    constexpr bit_list_array& resize(size_t size) {
        arr.resize(size / 8 + (size % 8 ? 1 : 0));
        return *this;
    }

    constexpr bit_list_array& reserve_back(size_t size) {
        arr.reserve_back(size / 8 + (size % 8 ? 1 : 0));
        return *this;
    }

    constexpr bit_list_array& reserve_front(size_t size) {
        arr.reserve_front(size / 8 + (size % 8 ? 1 : 0));
    }

    constexpr bool need_commit() const {
        return begin_bit ? true : arr.need_commit();
    }

    constexpr bit_list_array& commit() {
        //if array contains unused bits, then shift all bits to the left
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
            uint8_t tmp = begin_bit;
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

    constexpr const list_array<uint8_t, Allocator>& data() const {
        return arr;
    }

    constexpr list_array<uint8_t, Allocator>& data() {
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

    template <class Allocator>
    struct hash<bit_list_array<Allocator>> {
        constexpr size_t operator()(const bit_list_array<Allocator>& list) {
            hash<list_array<uint8_t, Allocator>> hasher;
            return hasher(list.data());
        }
    };
}

#endif
