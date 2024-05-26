#ifndef LIST_ARRAY
#define LIST_ARRAY
#include <iterator>
#include <stdexcept>
#include <stdint.h>
#include <type_traits>
#include <utility>

#if __cplusplus >= 202002L || _MSVC_LANG >= 202002L
    #define req(require) requires(require)
    #define conexpr constexpr
    #define IN_CPP20(...) __VA_ARGS__
#else
    #define req(require)
    #define conexpr
    #define IN_CPP20(...)
#endif
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

    template <typename T>
    struct can_direct_index<
        T,
        std::conditional_t<
            false,
            conditions_helper<decltype(std::declval<T>().data())>, void>> : public std::true_type {};

    template <class T>
    class list_array;
    template <class T>
    class dynamic_arr;
    template <class T>
    class arr_block;

    template <class T>
    class reverse_iterator;
    template <class T>
    class const_iterator;
    template <class T>
    class const_reverse_iterator;

    template <class T>
    class iterator {
        friend class list_array<T>;
        friend class dynamic_arr<T>;
        friend class const_iterator<T>;
        friend class reverse_iterator<T>;
        friend class const_reverse_iterator<T>;
        arr_block<T>* block;
        size_t pos;

        conexpr bool _nextBlock() {
            block = block ? block->next_ : block;
            pos = 0;
            return block && bool(block ? block->next_ : nullptr);
        }

        conexpr void _fast_load(T* arr, size_t arr_size) {
            size_t j = pos;
            arr_block<T>* block_tmp = block;
            size_t block_size = block_tmp->_size;
            T* block_arr = block->arr_contain;

            for (size_t i = 0; i < arr_size;) {
                for (; i < arr_size && j < block_size; j++)
                    arr[i++] = block_arr[j];
                j = 0;
                block_tmp = block_tmp->next_;
                if (!block_tmp)
                    return;
                block_size = block_tmp->_size;
                block_arr = block_tmp->arr_contain;
            }
        }

    public:
        using iterator_category = std::bidirectional_iterator_tag;
        using value_type = T;
        using difference_type = ptrdiff_t;
        using pointer = T*;
        using reference = T&;

        conexpr iterator() {
            block = nullptr;
            pos = 0;
        }

        conexpr iterator& operator=(const iterator& setter) {
            block = setter.block;
            pos = setter.pos;
            return *this;
        }

        conexpr iterator(const iterator& copy) {
            *this = copy;
        }

        conexpr iterator(arr_block<T>* block_pos, size_t set_pos) {
            block = block_pos;
            pos = set_pos;
            if (block)
                if (block->_size == set_pos && block->next_) {
                    block = block->next_;
                    pos = 0;
                }
        }

        conexpr iterator& operator++() {
            if (block) {
                if (block->_size <= ++pos) {
                    block = block->next_;
                    pos = 0;
                }
            }
            return *this;
        }

        conexpr iterator operator++(int) {
            iterator tmp = *this;
            operator++();
            return tmp;
        }

        conexpr iterator& operator--() {
            if (block) {
                if (0 == --pos) {
                    block = block->_prev;
                    pos = block ? block->_size : 0;
                }
            }
            return *this;
        }

        conexpr iterator operator--(int) {
            iterator tmp = *this;
            operator--();
            return tmp;
        }

        conexpr bool operator==(const iterator& comparer) const {
            return block == comparer.block && pos == comparer.pos && block;
        }

        conexpr bool operator!=(const iterator& comparer) const {
            return (block != comparer.block || pos != comparer.pos) && block;
        }

        conexpr T& operator*() {
            return block->arr_contain[pos];
        }

        conexpr const T& operator*() const {
            return block->arr_contain[pos];
        }

        conexpr T* operator->() {
            return block->arr_contain + pos;
        }
    };

    template <class T>
    class const_iterator {
        friend class list_array<T>;
        friend class dynamic_arr<T>;
        friend class const_reverse_iterator<T>;
        const arr_block<T>* block;
        size_t pos;

        conexpr void _fast_load(T* arr, size_t arr_size) const {
            size_t j = pos;
            const arr_block<T>* block_tmp = block;
            size_t block_size = block_tmp->_size;
            T* block_arr = block->arr_contain;

            for (size_t i = 0; i < arr_size;) {
                for (; i < arr_size && j < block_size; j++)
                    arr[i++] = block_arr[j];
                j = 0;
                block_tmp = block_tmp->next_;
                if (!block_tmp)
                    return;
                block_size = block_tmp->_size;
                block_arr = block_tmp->arr_contain;
            }
        }

    public:
        using iterator_category = std::bidirectional_iterator_tag;
        using value_type = T;
        using difference_type = ptrdiff_t;
        using pointer = T*;
        using reference = T&;

        conexpr const_iterator() {
            block = nullptr;
            pos = 0;
        }

        conexpr const_iterator& operator=(const const_iterator& setter) {
            block = setter.block;
            pos = setter.pos;
            return *this;
        }

        conexpr const_iterator(const const_iterator& copy) {
            *this = copy;
        }

        conexpr const_iterator& operator=(const iterator<T>& setter) {
            block = setter.block;
            pos = setter.pos;
            return *this;
        }

        conexpr const_iterator(const iterator<T>& copy) {
            *this = copy;
        }

        conexpr const_iterator(arr_block<T>* block_pos, size_t set_pos) {
            block = block_pos;
            pos = set_pos;
        }

        conexpr const_iterator& operator++() {
            if (block) {
                if (block->_size <= ++pos) {
                    block = block->next_;
                    pos = 0;
                }
            }
            return *this;
        }

        conexpr const_iterator operator++(int) {
            const_iterator tmp = *this;
            operator++();
            return tmp;
        }

        conexpr const_iterator& operator--() {
            if (block) {
                if (0 == --pos) {
                    block = block->_prev;
                    pos = block ? block->_size : 0;
                }
            }
            return *this;
        }

        conexpr const_iterator operator--(int) {
            const_iterator tmp = *this;
            operator--();
            return tmp;
        }

        conexpr bool operator==(const const_iterator& comparer) const {
            return block == comparer.block && pos == comparer.pos;
        }

        conexpr bool operator!=(const const_iterator& comparer) const {
            return (block != comparer.block || pos != comparer.pos) && block;
        }

        conexpr const T& operator*() {
            return block->arr_contain[pos];
        }

        conexpr T* operator->() {
            return block->arr_contain + pos;
        }
    };

    template <class T>
    class reverse_iterator {
        friend class dynamic_arr<T>;
        friend class const_reverse_iterator<T>;
        arr_block<T>* block;
        size_t pos;

    public:
        using iterator_category = std::bidirectional_iterator_tag;
        using value_type = T;
        using difference_type = ptrdiff_t;
        using pointer = T*;
        using reference = T&;

        conexpr reverse_iterator() {
            block = nullptr;
            pos = 0;
        }

        conexpr reverse_iterator& operator=(const iterator<T>& setter) {
            block = setter.block;
            pos = setter.pos;
            return *this;
        }

        conexpr reverse_iterator(const iterator<T>& copy) {
            *this = copy;
        }

        conexpr reverse_iterator& operator=(const reverse_iterator& setter) {
            block = setter.block;
            pos = setter.pos;
            return *this;
        }

        conexpr reverse_iterator(const reverse_iterator& copy) {
            *this = copy;
        }

        conexpr reverse_iterator(arr_block<T>* block_pos, size_t set_pos) {
            block = block_pos;
            pos = set_pos;
        }

        conexpr reverse_iterator& operator++() {
            if (block) {
                if (0 == --pos) {
                    block = block->_prev;
                    pos = block ? block->_size : 0;
                }
            }
            return *this;
        }

        conexpr reverse_iterator operator++(int) {
            reverse_iterator tmp = *this;
            operator++();
            return tmp;
        }

        conexpr reverse_iterator& operator--() {
            if (block) {
                if (block->_size == ++pos) {
                    block = block->next_;
                    pos = 0;
                }
            }
            return *this;
        }

        conexpr reverse_iterator operator--(int) {
            reverse_iterator tmp = *this;
            operator--();
            return tmp;
        }

        conexpr bool operator==(const reverse_iterator& comparer) const {
            return block == comparer.block && pos == comparer.pos;
        }

        conexpr bool operator!=(const reverse_iterator& comparer) const {
            return (block != comparer.block || pos != comparer.pos) && block;
        }

        conexpr T& operator*() {
            return block->arr_contain[pos - 1];
        }

        conexpr const T& operator*() const {
            return block->arr_contain[pos - 1];
        }

        conexpr T* operator->() {
            return block->arr_contain + pos - 1;
        }
    };

    template <class T>
    class const_reverse_iterator {
        friend class dynamic_arr<T>;
        friend class const_iterator<T>;
        const arr_block<T>* block;
        size_t pos;

    public:
        using iterator_category = std::bidirectional_iterator_tag;
        using value_type = T;
        using difference_type = ptrdiff_t;
        using pointer = T*;
        using reference = T&;

        conexpr const_reverse_iterator() {
            block = nullptr;
            pos = 0;
        }

        conexpr const_reverse_iterator& operator=(const const_iterator<T>& setter) {
            block = setter.block;
            pos = setter.pos;
            return *this;
        }

        conexpr const_reverse_iterator(const const_iterator<T>& copy) {
            *this = copy;
        }

        conexpr const_reverse_iterator& operator=(const iterator<T>& setter) {
            block = setter.block;
            pos = setter.pos;
            return *this;
        }

        conexpr const_reverse_iterator(const iterator<T>& copy) {
            *this = copy;
        }

        conexpr const_reverse_iterator& operator=(const reverse_iterator<T>& setter) {
            block = setter.block;
            pos = setter.pos;
            return *this;
        }

        conexpr const_reverse_iterator(const reverse_iterator<T>& copy) {
            *this = copy;
        }

        conexpr const_reverse_iterator& operator=(const const_reverse_iterator& setter) {
            block = setter.block;
            pos = setter.pos;
            return *this;
        }

        conexpr const_reverse_iterator(const const_reverse_iterator& copy) {
            *this = copy;
        }

        conexpr const_reverse_iterator(arr_block<T>* block_pos, size_t set_pos) {
            block = block_pos;
            pos = set_pos;
        }

        conexpr const_reverse_iterator& operator++() {
            if (block) {
                if (0 == --pos) {
                    block = block->_prev;
                    pos = block ? block->_size : 0;
                }
            }
            return *this;
        }

        conexpr const_reverse_iterator operator++(int) {
            const_reverse_iterator tmp = *this;
            operator++();
            return tmp;
        }

        conexpr const_reverse_iterator& operator--() {
            if (block) {
                if (block->_size == ++pos) {
                    block = block->next_;
                    pos = 0;
                }
            }
            return *this;
        }

        conexpr const_reverse_iterator operator--(int) {
            const_reverse_iterator tmp = *this;
            operator--();
            return tmp;
        }

        conexpr bool operator==(const const_reverse_iterator& comparer) const {
            return block == comparer.block && pos == comparer.pos;
        }

        conexpr bool operator!=(const const_reverse_iterator& comparer) const {
            return (block != comparer.block || pos != comparer.pos) && block;
        }

        conexpr const T& operator*() const {
            return block->arr_contain[pos - 1];
        }

        conexpr T* operator->() {
            return block->arr_contain + pos - 1;
        }
    };

    template <class T>
    class arr_block {
        friend class list_array<T>;
        friend class dynamic_arr<T>;
        friend class iterator<T>;
        friend class const_iterator<T>;
        friend class reverse_iterator<T>;
        friend class const_reverse_iterator<T>;

        arr_block* _prev = nullptr;
        arr_block* next_ = nullptr;
        T* arr_contain = nullptr;
        size_t _size = 0;

        void good_bye_world() {
            if (_prev)
                _prev->next_ = next_;
            if (next_)
                next_->_prev = _prev;
            _prev = next_ = nullptr;
            delete this;
        }

        conexpr static T* cxx_resize(T* val, size_t old_size, size_t new_size) {
            if (new_size == old_size)
                return val;
            T* new_val = new T[new_size];
            if (old_size < new_size) {
                if conexpr (std::is_move_assignable<T>::value)
                    for (size_t i = 0; i < old_size; i++)
                        new_val[i] = std::move(val[i]);
                else
                    for (size_t i = 0; i < old_size; i++)
                        new_val[i] = val[i];
            } else {
                if conexpr (std::is_move_assignable<T>::value)
                    for (size_t i = 0; i < new_size; i++)
                        new_val[i] = std::move(val[i]);
                else
                    for (size_t i = 0; i < new_size; i++)
                        new_val[i] = val[i];
            }
            delete[] val;
            return new_val;
        }

    public:
        conexpr arr_block() = default;

        conexpr arr_block(const arr_block& copy) {
            operator=(copy);
        }

        conexpr arr_block(arr_block&& move) noexcept {
            operator=(std::move(move));
        }

        conexpr arr_block(arr_block* prev, size_t len, arr_block* next) {
            if ((_prev = prev))
                _prev->next_ = this;
            if ((next_ = next))
                next_->_prev = this;
            arr_contain = new T[len];
            _size = len;
        }

        conexpr ~arr_block() {
            if (_prev) {
                _prev->next_ = nullptr;
                delete _prev;
            } else if (next_) {
                next_->_prev = nullptr;
                delete next_;
            }
            if (arr_contain)
                delete[] arr_contain;
        }

        conexpr T& operator[](size_t pos) {
            return (pos < _size) ? arr_contain[pos] : (*next_)[pos - _size];
        }

        conexpr const T& operator[](size_t pos) const {
            return (pos < _size) ? arr_contain[pos] : (*next_)[pos - _size];
        }

        conexpr arr_block& operator=(const arr_block& copy) {
            if (this == &copy)
                return *this;
            if (arr_contain)
                delete[] arr_contain;
            _size = copy._size;
            arr_contain = new T[_size];
            for (size_t i = 0; i < _size; i++)
                arr_contain[i] = copy.arr_contain[i];
            return *this;
        }

        conexpr arr_block& operator=(arr_block&& move) noexcept {
            if (this == &move)
                return *this;
            arr_contain = move.arr_contain;
            _prev = move._prev;
            next_ = move.next_;
            _size = move._size;
            move._prev = move.next_ = nullptr;
            move.arr_contain = nullptr;
            return *this;
        }

        conexpr T& index_back(size_t pos) {
            return (pos < _size) ? arr_contain[_size - pos - 1] : _prev ? (*_prev).index_back(pos - _size)
                                                                        : throw std::out_of_range("list_array index out of range");
        }

        conexpr T& index_front(size_t pos) {
            return (pos < _size) ? arr_contain[pos] : next_ ? (*next_)[pos - _size]
                                                            : throw std::out_of_range("list_array index out of range");
        }

        conexpr const T& index_back(size_t pos) const {
            return (pos < _size) ? arr_contain[_size - pos - 1] : _prev ? (*_prev).index_back(pos - _size)
                                                                        : throw std::out_of_range("list_array index out of range");
        }

        conexpr const T& index_front(size_t pos) const {
            return (pos < _size) ? arr_contain[pos] : next_ ? (*next_)[pos - _size]
                                                            : throw std::out_of_range("list_array index out of range");
        }

        conexpr iterator<T> get_iterator(size_t pos) {
            if (pos < _size)
                return iterator<T>(this, pos);
            else if (next_)
                return (*next_).get_iterator(pos - _size);
            else
                return iterator<T>(nullptr, 0);
        }

        conexpr iterator<T> get_iterator_back(size_t pos) {
            if (pos < _size)
                return iterator<T>(this, _size - pos - 1);
            else if (_prev)
                return (*_prev).get_iterator_back(pos - _size);
            else
                return iterator<T>(nullptr, 0);
        }

        conexpr const const_iterator<T> get_iterator(size_t pos) const {
            if (pos < _size)
                return const_iterator<T>(this, pos);
            else if (next_)
                return (*next_).get_iterator(pos - _size);
            else
                return const_iterator<T>(nullptr, 0);
        }

        conexpr const const_iterator<T> get_iterator_back(size_t pos) const {
            if (pos < _size)
                return const_iterator<T>(this, _size - pos - 1);
            else if (_prev)
                return (*_prev).get_iterator_back(pos - _size);
            else
                return const_iterator<T>(nullptr, 0);
        }

        conexpr iterator<T> begin() {
            if (_prev)
                return _prev->begin();
            else
                return iterator<T>(this, 0);
        }

        conexpr iterator<T> end() {
            if (next_)
                return next_->end();
            else
                return iterator<T>(this, _size);
        }

        conexpr const_iterator<T> begin() const {
            if (_prev)
                return _prev->begin();
            else
                return const_iterator<T>(this, 0);
        }

        conexpr const_iterator<T> end() const {
            if (_prev)
                return _prev->end();
            else
                return const_iterator<T>(this, _size);
        }

        conexpr inline size_t size() const {
            return _size;
        }

        conexpr void resize_front(size_t siz) {
            if (!siz) {
                good_bye_world();
                return;
            }
            T* tmp = arr_contain;
            arr_contain = cxx_resize(arr_contain, _size, siz);
            if (arr_contain == nullptr) {
                arr_contain = tmp;
                throw std::bad_alloc();
            }
            _size = siz;
        }

        conexpr void resize_begin(size_t siz) {
            if (!siz) {
                good_bye_world();
                return;
            }
            T* new_arr = new T[siz];
            int64_t dif = _size - siz;
            if (dif > 0) {
                if conexpr (std::is_move_assignable<T>::value)
                    for (size_t i = 0; i < siz && i < _size; i++)
                        new_arr[i] = std::move(arr_contain[i + dif]);
                else
                    for (size_t i = 0; i < siz && i < _size; i++)
                        new_arr[i] = arr_contain[i + dif];
            } else {
                dif *= -1;
                if conexpr (std::is_move_assignable<T>::value)
                    for (size_t i = 0; i < siz && i < _size; i++)
                        new_arr[dif + i] = std::move(arr_contain[i]);
                else
                    for (size_t i = 0; i < siz && i < _size; i++)
                        new_arr[dif + i] = arr_contain[i];
            }
            delete[] arr_contain;
            arr_contain = new_arr;
            _size = siz;
        }
    };

    template <class T>
    class dynamic_arr {
        friend class list_array<T>;
        friend class iterator<T>;
        friend class const_iterator<T>;
        friend class reverse_iterator<T>;
        friend class const_reverse_iterator<T>;
        arr_block<T>* arr = nullptr;
        arr_block<T>* arr_end = nullptr;
        size_t _size = 0;

        conexpr void swap_block_with_blocks(arr_block<T>& this_block, arr_block<T>& first_block, arr_block<T>& second_block) {
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

        conexpr void remove_item_slow(iterator<T> block) {
            size_t pos = block.pos;
            auto& this_block = *block.block;
            if (pos > this_block._size / 2) {
                size_t mov_to = this_block._size - 1;
                for (size_t i = pos; i < mov_to; i++)
                    std::swap(this_block.arr_contain[i + 1], this_block.arr_contain[i]);
                this_block.resize_front(this_block._size - 1);
            } else {
                for (int64_t i = pos; i > 0; i--)
                    std::swap(this_block.arr_contain[i - 1], this_block.arr_contain[i]);
                this_block.resize_begin(this_block._size - 1);
            }
        }

        conexpr void remove_item_split(iterator<T> block) {
            size_t pos = block.pos;
            auto& this_block = *block.block;
            size_t block_size = this_block._size;
            arr_block<T>& first_block = *new arr_block<T>(nullptr, pos, nullptr);
            arr_block<T>& second_block = *new arr_block<T>(&first_block, block_size - pos - 1, nullptr);

            for (size_t i = 0; i < pos; i++)
                first_block.arr_contain[i] = this_block.arr_contain[i];

            size_t block_half_size = pos + 1;
            for (size_t i = block_half_size; i < block_size; i++)
                second_block.arr_contain[i - block_half_size] = this_block.arr_contain[i];

            swap_block_with_blocks(this_block, first_block, second_block);
        }

        conexpr void insert_item_slow(iterator<T> block, const T& item) {
            size_t pos = block.pos;
            auto& this_block = *block.block;
            if (pos > this_block._size / 2) {
                this_block.resize_front(this_block._size + 1);
                size_t mov_to = this_block._size - 1;
                for (int64_t i = mov_to - 1; i >= pos; i--)
                    std::swap(this_block.arr_contain[i + 1], this_block.arr_contain[i]);
                this_block.arr_contain[pos] = item;
            } else {
                this_block.resize_begin(this_block._size + 1);
                for (int64_t i = 0; i < pos; i++)
                    std::swap(this_block.arr_contain[i + 1], this_block.arr_contain[i]);
                this_block.arr_contain[pos] = item;
            }
        }

        conexpr void insert_item_split(iterator<T> block, const T& item) {
            size_t pos = block.pos;
            auto& this_block = *block.block;
            size_t block_size = this_block._size;
            size_t first_size = pos + 1;
            arr_block<T>& first_block = *new arr_block<T>(nullptr, first_size + 1, nullptr);
            arr_block<T>& second_block = *new arr_block<T>(&first_block, block_size - first_size, nullptr);

            for (size_t i = 0; i < pos; i++)
                first_block.arr_contain[i] = this_block.arr_contain[i];
            first_block[first_size] = item;
            for (size_t i = first_size; i < block_size; i++)
                second_block.arr_contain[i - first_size] = this_block.arr_contain[i];
            swap_block_with_blocks(this_block, first_block, second_block);
        }

        conexpr void insert_item_slow(iterator<T> block, T&& item) {
            size_t pos = block.pos;
            auto& this_block = *block.block;
            if (pos > this_block._size / 2) {
                this_block.resize_front(this_block._size + 1);
                size_t mov_to = this_block._size - 1;
                for (int64_t i = mov_to - 1; i >= pos; i--)
                    std::swap(this_block.arr_contain[i + 1], this_block.arr_contain[i]);
                this_block.arr_contain[pos] = std::move(item);
            } else {
                this_block.resize_begin(this_block._size + 1);
                for (int64_t i = 0; i < pos; i++)
                    std::swap(this_block.arr_contain[i + 1], this_block.arr_contain[i]);
                this_block.arr_contain[pos] = std::move(item);
            }
        }

        conexpr void insert_item_split(iterator<T> block, T&& item) {
            size_t pos = block.pos;
            auto& this_block = *block.block;
            size_t block_size = this_block._size;
            size_t first_size = pos + 1;
            arr_block<T>& first_block = *new arr_block<T>(nullptr, first_size + 1, nullptr);
            arr_block<T>& second_block = *new arr_block<T>(&first_block, block_size - first_size, nullptr);

            for (size_t i = 0; i < pos; i++)
                first_block.arr_contain[i] = this_block.arr_contain[i];
            first_block[first_size] = std::move(item);
            for (size_t i = first_size; i < block_size; i++)
                second_block.arr_contain[i - first_size] = this_block.arr_contain[i];
            swap_block_with_blocks(this_block, first_block, second_block);
        }

        conexpr void insert_block_split(iterator<T> block, const T* item, size_t item_size) {
            size_t pos = block.pos;
            if (block.block == nullptr)
                throw std::out_of_range("list_array index out of range");
            arr_block<T>& this_block = *block.block;
            size_t block_size = this_block._size;
            if (pos == 0) {
                auto insert = new arr_block<T>(this_block._prev, item_size, &this_block);
                for (size_t i = 0; i < item_size; i++)
                    insert->arr_contain[i] = item[i];
            } else if (pos == block_size) {
                auto insert = new arr_block<T>(&this_block, item_size, this_block.next_);
                for (size_t i = 0; i < item_size; i++)
                    insert->arr_contain[i] = item[i];
            } else {
                arr_block<T>& first_block = *new arr_block<T>(nullptr, pos, nullptr);
                arr_block<T>& second_block = *new arr_block<T>(nullptr, block_size - pos, nullptr);
                for (size_t i = 0; i < pos; i++)
                    first_block.arr_contain[i] = this_block.arr_contain[i];

                for (size_t i = pos; i < block_size; i++)
                    second_block.arr_contain[i - pos] = this_block.arr_contain[i];

                arr_block<T>& new_block_block = *new arr_block<T>(&first_block, item_size, &second_block);
                for (size_t i = 0; i < item_size; i++)
                    new_block_block.arr_contain[i] = item[i];
                swap_block_with_blocks(this_block, first_block, second_block);
            }
            _size += item_size;
        }

        conexpr size_t _remove_items(arr_block<T>* block, size_t start, size_t end) {
            size_t size = block->_size;
            size_t new_size = block->_size - (end - start);
            if (new_size == 0) {
                if (arr == block)
                    arr = block->next_;
                if (arr_end == block)
                    arr_end = block->_prev;
                block->good_bye_world();
            } else {
                T* new_arr = new T[new_size];
                T* arr_inter = block->arr_contain;
                size_t j = 0;
                for (size_t i = 0; j < new_size && i < size; i++) {
                    if (i == start)
                        i = end;
                    new_arr[j++] = arr_inter[i];
                }
                delete[] arr_inter;
                block->arr_contain = new_arr;
                block->_size = new_size;
            }
            return size - new_size;
        }

        template <class _Fn>
        conexpr size_t _remove_if(iterator<T>& block, _Fn func, size_t start, size_t end) {
            // value >> 3  ==  value / 8
            size_t size = block.block->_size;
            size_t rem_filt_siz =
                size > end ? (size >> 3) + (size & 7 ? 1 : 0) : (end >> 3) + (end & 7 ? 1 : 0);
            uint8_t* remove_filter = new uint8_t[rem_filt_siz]{0};
            T* arr_inter = block.block->arr_contain;
            size_t new_size = size;

            for (size_t i = start; i < end; i++) {
                if (func(arr_inter[i])) {
                    remove_filter[i >> 3] |= 1 << (i & 7);
                    new_size--;
                }
            }
            if (size != new_size) {
                if (new_size == 0 && size == end) {
                    arr_block<T>* next_block = block.block->next_;
                    if (arr == block.block)
                        arr = block.block->next_;
                    if (arr_end == block.block)
                        arr_end = block.block->_prev;
                    block.block->good_bye_world();
                    block.block = next_block;
                    block.pos = 0;
                } else {
                    T* new_arr = new T[new_size];
                    size_t j = 0;
                    for (size_t i = 0; j < new_size && i < end; i++)
                        if (!(remove_filter[i >> 3] & (1 << (i & 7))))
                            new_arr[j++] = arr_inter[i];
                    delete[] block.block->arr_contain;
                    block.block->arr_contain = new_arr;
                    block.block->_size = new_size;
                }
            }
            delete[] remove_filter;
            return size - new_size;
        }

    public:
        conexpr void clear() {
            arr_block<T>* blocks = arr;
            arr_block<T>* this_block;
            while (blocks != nullptr) {
                this_block = blocks;
                blocks = blocks->next_;
                this_block->_prev = nullptr;
                this_block->next_ = nullptr;
                delete this_block;
            }
            _size = 0;
            arr = arr_end = nullptr;
        }

        conexpr dynamic_arr() = default;

        conexpr dynamic_arr(const dynamic_arr& copy) {
            operator=(copy);
        }

        conexpr dynamic_arr(dynamic_arr&& move) noexcept(false) {
            operator=(std::move(move));
        }

        conexpr dynamic_arr& operator=(dynamic_arr&& move) noexcept(false) {
            if (this == &move)
                return *this;
            clear();
            arr = move.arr;
            arr_end = move.arr_end;
            _size = move._size;
            move.arr = move.arr_end = nullptr;
            return *this;
        }

        conexpr dynamic_arr& operator=(const dynamic_arr& copy) {
            if (this == &copy)
                return *this;
            clear();
            if (!copy._size)
                return *this;
            T* tmp = (arr = arr_end = new arr_block<T>(nullptr, _size = copy._size, nullptr))->arr_contain;
            size_t i = 0;
            for (auto& it : copy)
                tmp[i++] = it;
            return *this;
        }

        conexpr ~dynamic_arr() {
            clear();
        }

        conexpr T& operator[](size_t pos) {
            return (pos < (_size >> 1)) ? arr->operator[](pos) : arr_end->index_back(_size - pos - 1);
        }

        conexpr const T& operator[](size_t pos) const {
            return (pos < (_size >> 1)) ? arr->operator[](pos) : arr_end->index_back(_size - pos - 1);
        }

        conexpr T& index_back(size_t pos) {
            return arr_end->index_back(_size - pos - 1);
        }

        conexpr const T& index_back(size_t pos) const {
            return arr_end->index_back(_size - pos - 1);
        }

        conexpr iterator<T> get_iterator(size_t pos) {
            if (!_size)
                return iterator<T>(nullptr, 0);
            if (pos < (_size >> 1))
                return arr->get_iterator(pos);
            else {
                if (_size - pos - 1 == size_t(-1))
                    return iterator<T>(arr_end, arr_end ? arr_end->_size : 0);
                else
                    return arr_end->get_iterator_back(_size - pos - 1);
            }
        }

        conexpr const_iterator<T> get_iterator(size_t pos) const {
            if (!_size)
                return iterator<T>(nullptr, 0);
            if (pos < (_size >> 1))
                return arr->get_iterator(pos);
            else {
                if (_size - pos - 1 == size_t(-1))
                    return iterator<T>(arr_end, arr_end ? arr_end->_size : 0);
                else
                    return arr_end->get_iterator_back(_size - pos - 1);
            }
        }

        conexpr auto begin() {
            return arr->begin();
        }

        conexpr auto end() {
            return arr_end->end();
        }

        conexpr auto begin() const {
            return arr->begin();
        }

        conexpr auto end() const {
            return arr_end->end();
        }

        conexpr size_t size() const {
            return _size;
        }

        conexpr void resize_begin(size_t new_size) {
            size_t tsize = _size;
            if (tsize >= new_size) {
                if (!arr_end) {
                    if (arr) {
                        arr_end = arr;
                        while (arr_end->_prev)
                            arr_end = arr_end->_prev;
                    } else {
                        arr = arr_end = new arr_block<T>(nullptr, new_size, nullptr);
                        return;
                    }
                }
                for (size_t resize_to = tsize - new_size; resize_to > 0;) {
                    if (arr->_size > resize_to) {
                        _size = new_size;
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
                            _size = 0;
                            return;
                        }
                    }
                }
            } else {
                if (arr)
                    if (arr->_size + new_size <= _size >> 1) {
                        arr->resize_begin(new_size - tsize + arr->_size);
                        if (!arr_end)
                            arr_end = arr;
                        _size = new_size;
                        return;
                    }
                arr = new arr_block<T>(nullptr, new_size - tsize, arr);
                if (!arr_end)
                    arr_end = arr;
            }
            _size = new_size;
        }

        conexpr void resize_front(size_t new_size) {
            size_t tsize = _size;
            if (tsize >= new_size) {
                if (!arr_end) {
                    if (arr) {
                        arr_end = arr;
                        while (arr_end->_prev)
                            arr_end = arr_end->_prev;
                    } else {
                        arr = arr_end = new arr_block<T>(nullptr, new_size, nullptr);
                        return;
                    }
                }
                for (size_t resize_to = tsize - new_size; resize_to > 0;) {
                    if (arr_end->_size > resize_to) {
                        _size = new_size;
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
                            _size = 0;
                            return;
                        }
                    }
                }
            } else {
                if (arr_end)
                    if (arr_end->_size + new_size <= _size >> 1) {
                        arr_end->resize_front(new_size - tsize + arr_end->_size);
                        if (!arr)
                            arr = arr_end;
                        _size = new_size;
                        return;
                    }
                arr_end = new arr_block<T>(arr_end, new_size - tsize, nullptr);
                if (!arr)
                    arr = arr_end;
            }
            _size = new_size;
        }

        conexpr void insert_block(size_t pos, const T* item, size_t item_size) {
            if (pos == _size) {
                resize_front(_size + item_size);
                auto iterator = get_iterator(_size - item_size);
                for (size_t i = 0; i < item_size; i++) {
                    *iterator = item[i];
                    ++iterator;
                }
            } else if (pos == 0) {
                resize_begin(_size + item_size);
                auto iterator = get_iterator(0);
                for (size_t i = 0; i < item_size; i++) {
                    *iterator = item[i];
                    ++iterator;
                }
            } else
                insert_block_split(get_iterator(pos), item, item_size);
        }

        conexpr void insert_block(size_t pos, const arr_block<T>& item) {
            insert_block_split(get_iterator(pos), item.arr_contain, item._size);
        }

        conexpr void insert(size_t pos, const T& item) {
            if (!_size) {
                resize_front(1);
                operator[](0) = item;
                return;
            }
            iterator<T> inter = get_iterator(pos);
            arr_block<T>& this_block = *inter.block;
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
            _size++;
        }

        conexpr void insert(size_t pos, T&& item) {
            if (!_size) {
                resize_front(1);
                operator[](0) = std::move(item);
                return;
            }
            iterator<T> inter = get_iterator(pos);
            arr_block<T>& this_block = *inter.block;
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
            _size++;
        }

        conexpr void remove_item(size_t pos) {
            if (!_size)
                return;
            iterator<T> inter = get_iterator(pos);
            arr_block<T>& this_block = *inter.block;
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
            _size--;
        }

        conexpr size_t remove_items(size_t start_pos, size_t end_pos) {
            iterator<T> iterate = get_iterator(start_pos);
            iterator<T> _end = get_iterator(end_pos);
            size_t removed = 0;
            size_t to_remove = end_pos - start_pos;
            if (iterate.block == _end.block) {
                removed = _remove_items(iterate.block, iterate.pos, _end.pos);
                _size -= removed;
                return removed;
            }
            arr_block<T>* curr_block;
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
            _size -= removed;
            return removed;
        }

        template <class _Fn>
        conexpr size_t remove_if(size_t start, size_t end, _Fn func) {
            if (!_size)
                return 0;
            iterator<T> iterate = get_iterator(start);
            iterator<T> _end = get_iterator(end);
            size_t removed = 0;
            if (iterate.block == _end.block) {
                removed = _remove_if(iterate, func, iterate.pos, _end.pos);
                _size -= removed;
                return removed;
            }

            removed += _remove_if(iterate, func, iterate.pos, iterate.block->_size);
            for (;;) {
                if (!iterate._nextBlock())
                    break;
                removed += _remove_if(iterate, func, 0, iterate.block->_size);
            }

            _size -= removed;
            return removed;
        }

        conexpr void swap(dynamic_arr<T>& to_swap) noexcept {
            arr_block<T>* old_arr = arr;
            arr_block<T>* old_arr_end = arr_end;
            size_t old__size = _size;
            arr = to_swap.arr;
            arr_end = to_swap.arr_end;
            _size = to_swap._size;
            to_swap.arr = old_arr;
            to_swap.arr_end = old_arr_end;
            to_swap._size = old__size;
        }
    };

    template <class T>
    class list_array {
        dynamic_arr<T> arr;
        size_t reserved_begin = 0;
        size_t _size = 0;
        size_t reserved_end = 0;

        conexpr void steal_block_begin() {
            arr_block<T>* move_block = arr.arr;
            reserved_begin -= move_block->_size;
            reserved_end += move_block->_size;


            arr.arr = move_block->next_;

            move_block->next_ = nullptr;
            move_block->_prev = arr.arr_end;

            if (arr.arr_end->_prev == move_block)
                arr.arr->next_ = move_block;
            arr.arr_end = move_block;
            arr.arr->_prev = nullptr;
        }

        conexpr void steal_block_end() {
            arr_block<T>* move_block = arr.arr_end;
            reserved_end -= move_block->_size;
            reserved_begin += move_block->_size;

            arr.arr_end = move_block->_prev;
            arr.arr_end->next_ = nullptr;

            move_block->_prev = nullptr;
            move_block->next_ = arr.arr;

            if (arr.arr->next_ == move_block)
                arr.arr_end->_prev = move_block;
            arr.arr = move_block;
            arr.arr_end->_prev = nullptr;
        }

    public:
        using iterator = __list_array_impl::iterator<T>;
        using const_iterator = __list_array_impl::const_iterator<T>;
        using reverse_iterator = __list_array_impl::reverse_iterator<T>;
        using const_reverse_iterator = __list_array_impl::const_reverse_iterator<T>;
        using value_type = T;
        using reference = T&;
        using const_reference = const T&;
        using size_type = size_t;
        using difference_type = ptrdiff_t;
        static conexpr inline const size_t npos = -1;

        class range_provider {
            size_t _start;
            size_t _end;
            list_array<T>& ln;

        public:
            conexpr range_provider(list_array<T>& link, size_t start, size_t end)
                : ln(link) {
                _start = start;
                _end = end;
            }

            conexpr range_provider(const range_provider& copy)
                : ln(copy.ln) {
                _start = copy._start;
                _end = copy._end;
            }

            conexpr range_provider& operator=(const range_provider& copy) {
                reinterpret_cast<list_array<T>*&>(&ln) = reinterpret_cast<list_array<T>*&>(&copy.ln);
                _start = copy._start;
                _end = copy._end;
                return *this;
            }

            conexpr iterator begin() {
                return ln.get_iterator(_start);
            }

            conexpr iterator end() {
                return ln.get_iterator(_end);
            }

            conexpr const_iterator begin() const {
                return ln.get_iterator(_start);
            }

            conexpr const_iterator end() const {
                return ln.get_iterator(_end);
            }

            conexpr reverse_iterator rbegin() {
                return ln.get_iterator(_end);
            }

            conexpr reverse_iterator rend() {
                return ln.get_iterator(_start);
            }

            conexpr const_reverse_iterator rbegin() const {
                return ln.get_iterator(_end);
            }

            conexpr const_reverse_iterator rend() const {
                return ln.get_iterator(_start);
            }

            conexpr size_t range_start() const {
                return _start;
            }

            conexpr size_t range_end() const {
                return _end;
            }
        };

        class const_range_provider {
            size_t _start;
            size_t _end;
            const list_array<T>& ln;

        public:
            conexpr const_range_provider(const list_array<T>& link, size_t start, size_t end)
                : ln(link) {
                _start = start;
                _end = end;
            }

            conexpr const_range_provider(const const_range_provider& copy)
                : ln(copy.ln) {
                _start = copy._start;
                _end = copy._end;
            }

            conexpr const_range_provider& operator=(const const_range_provider& copy) {
                reinterpret_cast<list_array<T>*&>(&ln) = reinterpret_cast<list_array<T>*&>(&copy.ln);
                _start = copy._start;
                _end = copy._end;
                return *this;
            }

            conexpr const_iterator begin() const {
                return ln.get_iterator(_start);
            }

            conexpr const_iterator end() const {
                return ln.get_iterator(_end);
            }

            conexpr const_reverse_iterator rbegin() const {
                return ln.get_iterator(_end);
            }

            conexpr const_reverse_iterator rend() const {
                return ln.get_iterator(_start);
            }
        };

        class reverse_provider {
            reverse_iterator _begin;
            reverse_iterator _end;

        public:
            conexpr reverse_provider(range_provider& link) {
                _begin = link.rbegin();
                _end = link.rend();
            }

            conexpr reverse_provider(range_provider&& link) {
                _begin = link.rbegin();
                _end = link.rend();
            }

            conexpr reverse_provider(list_array<T>& link) {
                _begin = link.rbegin();
                _end = link.rend();
            }

            conexpr reverse_provider(const reverse_provider& copy) {
                _begin = copy._begin;
                _end = copy._end;
            }

            conexpr reverse_provider& operator=(const reverse_provider& copy) {
                _begin = copy._start;
                _end = copy._end;
                return *this;
            }

            conexpr reverse_iterator begin() {
                return _begin;
            }

            conexpr reverse_iterator end() {
                return _end;
            }

            conexpr const_reverse_iterator begin() const {
                return _begin;
            }

            conexpr const_reverse_iterator end() const {
                return _end;
            }
        };

        class const_reverse_provider {
            const_reverse_iterator _begin;
            const_reverse_iterator _end;

        public:
            conexpr const_reverse_provider(const range_provider& link) {
                _begin = link.rbegin();
                _end = link.rend();
            }

            conexpr const_reverse_provider(const range_provider&& link) {
                _begin = link.rbegin();
                _end = link.rend();
            }

            conexpr const_reverse_provider(const const_range_provider& link) {
                _begin = link.rbegin();
                _end = link.rend();
            }

            conexpr const_reverse_provider(const const_range_provider&& link) {
                _begin = link.rbegin();
                _end = link.rend();
            }

            conexpr const_reverse_provider(const list_array<T>& link) {
                _begin = link.rbegin();
                _end = link.rend();
            }

            conexpr const_reverse_provider(const const_reverse_provider& copy) {
                _begin = copy._begin;
                _end = copy._end;
            }

            conexpr const_reverse_provider& operator=(const const_reverse_provider& copy) {
                _begin = copy._start;
                _end = copy._end;
                return *this;
            }

            conexpr const_reverse_iterator begin() const {
                return _begin;
            }

            conexpr const_reverse_iterator end() const {
                return _end;
            }
        };

#pragma region constructors
        conexpr list_array() = default;

        IN_CPP20(
            template <class Container>
            conexpr list_array(const Container& cont) req(is_container<Container>::value) {
                if conexpr (can_direct_index<Container>::value) {
                    resize(cont.size());
                    auto it = cont.data();
                    for (size_t i = 0; i < _size; i++)
                        arr[i] = it[i];
                } else {
                    reserve(cont.size());
                    size_t i = 0;
                    for (const T& it : cont)
                        push_back(it);
                }
            }
        )

        conexpr list_array(const std::initializer_list<T>& vals) {
            resize(vals.size());
            auto iter = begin();
            for (const T& it : vals) {
                *iter = it;
                ++iter;
            }
        }

        template <class AnotherT>
        conexpr list_array(const std::initializer_list<AnotherT>& vals) {
            for (const AnotherT& it : vals)
                push_back(it);
        }

        template <size_t arr_size>
        conexpr list_array(const T (&arr)[arr_size]) {
            push_back(arr, arr_size);
        }

        conexpr list_array(const T* arr, size_t arr_size) {
            push_back(arr, arr_size);
        }

        template <typename Iterable>
        conexpr list_array(Iterable begin, Iterable end, size_t reserve_len = 0) {
            if conexpr (std::is_pointer<Iterable>::value) {
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

        conexpr list_array(size_t size) {
            resize(size);
        }

        conexpr list_array(size_t size, const T& default_init) {
            resize(size, default_init);
        }

        conexpr list_array(list_array&& move) noexcept {
            operator=(std::move(move));
        }

        conexpr list_array(const list_array& copy) {
            operator=(copy);
        }

        conexpr list_array(const list_array& copy, size_t start) {
            operator=(copy.copy(start, copy._size));
        }

        conexpr list_array(const list_array& copy, size_t start, size_t end) {
            operator=(copy.copy(start, end));
        }

#pragma endregion
#pragma region operators

        conexpr list_array& operator=(list_array&& move) noexcept {
            arr = std::move(move.arr);
            reserved_begin = move.reserved_begin;
            _size = move._size;
            reserved_end = move.reserved_end;
            return *this;
        }

        conexpr list_array& operator=(const list_array& copy) {
            arr = copy.arr;
            reserved_begin = copy.reserved_begin;
            _size = copy._size;
            reserved_end = copy.reserved_end;
            return *this;
        }

        conexpr bool operator==(const list_array<T>& to_cmp) const {
            if (arr.arr != to_cmp.arr.arr) {
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

        conexpr bool operator!=(const list_array<T>& to_cmp) const {
            return !operator==(to_cmp);
        }

#pragma endregion
#pragma region list operations

        conexpr void push_front(const T& copy_to) {
            if (reserved_begin) {
                arr[--reserved_begin] = copy_to;
                _size++;
                return;
            } else if (arr.arr_end) {
                if (arr.arr_end->_size <= reserved_end) {
                    steal_block_begin();
                    push_back(copy_to);
                    return;
                }
            }
            reserve_push_front(_size + 1);
            push_front(copy_to);
        }

        conexpr void push_front(T&& copy_to) {
            if (reserved_begin) {
                arr[--reserved_begin] = copy_to;
                _size++;
                return;
            } else if (arr.arr_end) {
                if (arr.arr_end->_size <= reserved_end) {
                    steal_block_begin();
                    push_back(std::move(copy_to));
                    return;
                }
            }
            reserve_push_front(_size + 1);
            push_front(std::move(copy_to));
        }

        conexpr void push_back(const T& copy_to) {
            if (reserved_end) {
                arr[reserved_begin + _size++] = copy_to;
                --reserved_end;
                return;
            } else if (arr.arr) {
                if (arr.arr->_size <= reserved_begin) {
                    steal_block_begin();
                    push_back(copy_to);
                    return;
                }
            }
            reserve_push_back(_size + 1);
            push_back(copy_to);
        }

        conexpr void push_back(T&& copy_to) {
            if (reserved_end) {
                arr[reserved_begin + _size++] = std::move(copy_to);
                --reserved_end;
                return;
            } else if (arr.arr) {
                if (arr.arr->_size <= reserved_begin) {
                    steal_block_begin();
                    push_back(std::move(copy_to));
                    return;
                }
            }
            reserve_push_back(_size + 1);
            push_back(std::move(copy_to));
        }

        conexpr void pop_back() {
            if (_size) {
                ++reserved_end;
                --_size;
            } else
                throw std::out_of_range("This list_array is empty");
        }

        conexpr void pop_front() {
            if (_size) {
                ++reserved_begin;
                --_size;
            } else
                throw std::out_of_range("This list_array is empty");
        }

        conexpr T take_back() {
            if (_size) {
                T tmp(std::move(operator[](_size - 1)));
                pop_back();
                return tmp;
            } else
                throw std::out_of_range("This list_array is empty");
        }

        conexpr T take_front() {
            if (_size) {
                T tmp(std::move(operator[](0)));
                pop_front();
                return tmp;
            } else
                throw std::out_of_range("This list_array is empty");
        }

        conexpr T& back() {
            return operator[](_size - 1);
        }

        conexpr T& front() {
            return operator[](0);
        }

        conexpr const T& back() const {
            return operator[](_size - 1);
        }

        conexpr const T& front() const {
            return operator[](0);
        }

        template <size_t arr_size>
        conexpr void push_front(const T (&array)[arr_size]) {
            push_front(array, arr_size);
        }

        template <size_t arr_size>
        conexpr void push_back(const T (&array)[arr_size]) {
            push_back(array, arr_size);
        }

        conexpr void push_front(const T* array, size_t arr_size) {
            insert(0, array, arr_size);
        }

        conexpr void push_back(const T* array, size_t arr_size) {
            insert(_size, array, arr_size);
        }

        conexpr void push_front(const list_array<T>& to_push) {
            insert(0, to_push);
        }

        conexpr void push_back(const list_array<T>& to_push) {
            insert(_size, to_push);
        }

#pragma endregion
#pragma region insert

        template <size_t arr_size>
        conexpr void insert(size_t pos, const T (&item)[arr_size]) {
            insert(pos, item, arr_size);
        }
        conexpr void insert(size_t pos, const T* item, size_t arr_size) {
            if (!arr_size)
                return;
            arr.insert_block(reserved_begin + pos, item, arr_size);
            _size += arr_size;
        }

        conexpr void insert(size_t pos, const list_array<T>& item) {
            if (!item._size)
                return;
            if (item.blocks_more(1)) {
                T* as_array = item.to_array();
                arr.insert_block(reserved_begin + pos, as_array, item._size);
                delete[] as_array;
            } else {
                arr.insert_block(reserved_begin + pos, item.data(), item._size);
            }
            _size += item._size;
        }
        conexpr void insert(size_t pos, const list_array<T>& item, size_t start, size_t end) {
            auto item_range = item.range(start, end);
            insert(pos, list_array<T>(item_range.begin(), item_range.end()));
        }

        conexpr void insert(size_t pos, const T& item) {
            if (pos == _size)
                return push_back(item);
            arr.insert(reserved_begin + pos, item);
            _size++;
        }

        conexpr void insert(size_t pos, T&& item) {
            if (pos == _size)
                return push_back(std::move(item));
            arr.insert(reserved_begin + pos, std::move(item));
            _size++;
        }

#pragma endregion
#pragma region ordered
        conexpr void ordered_insert(const T& item) {
            size_t i = 0;
            for (auto& it : *this) {
                if (it > item)
                    break;
                i++;
            }
            insert(i, item);
        }

        conexpr void ordered_insert(T&& item) {
            size_t i = 0;
            for (auto& it : *this) {
                if (it > item)
                    break;
                i++;
            }
            insert(i, std::move(item));
        }

        template <class _Fn>
        conexpr void ordered_insert(const T& item, _Fn order_checker) {
            size_t i = 0;
            for (auto& it : *this) {
                if (order_checker(it, item))
                    break;
                i++;
            }
            insert(i, item);
        }

        template <class _Fn>
        conexpr void ordered_insert(T&& item, _Fn order_checker) {
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

        conexpr bool contains(const T& value, size_t start = 0) const req(std::equality_comparable<T>) {
            return contains(value, start, _size);
        }

        conexpr bool contains(const T& value, size_t start, size_t end) const req(std::equality_comparable<T>) {
            return find(value, start, end) != npos;
        }

        template <size_t arr_size>
        conexpr bool contains(const T (&arr)[arr_size], size_t start = 0) const req(std::equality_comparable<T>) {
            return contains(arr, arr_size, start, _size);
        }

        template <size_t arr_size>
        conexpr bool contains(const T (&arr)[arr_size], size_t start, size_t end) const req(std::equality_comparable<T>) {
            return contains(arr, arr_size, start, end);
        }

        conexpr bool contains(const T* arr, size_t arr_size, size_t start = 0) const req(std::equality_comparable<T>) {
            return contains(arr, arr_size, start, _size);
        }

        conexpr bool contains(const T* arr, size_t arr_size, size_t start, size_t end) const req(std::equality_comparable<T>) {
            return find(arr, arr_size, start, end) != npos;
        }

        conexpr bool contains(const list_array<T>& value, size_t start = 0) const req(std::equality_comparable<T>) {
            return contains(value, 0, value._size, start, _size);
        }

        conexpr bool contains(const list_array<T>& value, size_t start, size_t end) const req(std::equality_comparable<T>) {
            return contains(value, 0, value._size, start, end);
        }

        conexpr bool contains(const list_array<T>& value, size_t value_start, size_t value_end, size_t start, size_t end) const req(std::equality_comparable<T>) {
            return find(value, value_start, value_end, start, end) != npos;
        }

        template <class _Fn>
        conexpr bool contains_one(_Fn check_function) const {
            return contains_one(0, _size, check_function);
        }

        template <class _Fn>
        conexpr size_t contains_multiply(_Fn check_function) const {
            return contains_multiply(0, _size, check_function);
        }

        template <class _Fn>
        conexpr bool contains_one(size_t start, _Fn check_function) const {
            return contains_one(start, _size, check_function);
        }

        template <class _Fn>
        conexpr size_t contains_multiply(size_t start, _Fn check_function) const {
            return contains_multiply(start, _size, check_function);
        }

        template <class _Fn>
        conexpr bool contains_one(size_t start, size_t end, _Fn check_function) const {
            for (const T& it : range(start, end))
                if (check_function(it))
                    return true;
            return false;
        }

        template <class _Fn>
        conexpr size_t contains_multiply(size_t start, size_t end, _Fn check_function) const {
            size_t i = 0;
            for (const T& it : range(start, end))
                if (check_function(it))
                    ++i;
            return i;
        }

#pragma endregion
#pragma region remove

        conexpr void remove(size_t pos) {
            if (pos >= _size)
                throw std::out_of_range("pos value out of size limit");
            arr.remove_item(reserved_begin + pos);
            _size--;
            if (_size == 0)
                clear();
        }

        conexpr void remove(size_t start_pos, size_t end_pos) {
            if (start_pos > end_pos)
                std::swap(start_pos, end_pos);
            if (end_pos > _size)
                throw std::out_of_range("end_pos value out of size limit");
            if (start_pos != end_pos)
                _size -= arr.remove_items(reserved_begin + start_pos, reserved_begin + end_pos);
        }

#pragma endregion
#pragma region remove_if

        template <class _Fn>
        conexpr size_t remove_if(_Fn check_function) {
            return remove_if(0, _size, check_function);
        }

        template <class _Fn>
        conexpr size_t remove_if(size_t start, _Fn check_function) {
            return remove_if(start, _size, check_function);
        }

        template <class _Fn>
        conexpr size_t remove_if(size_t start, size_t end, _Fn check_function) {
            if (start > end)
                std::swap(start, end);
            if (end > _size)
                throw std::out_of_range("end value out of size limit");
            if (start > _size)
                throw std::out_of_range("start value out of size limit");
            size_t res = arr.remove_if(reserved_begin + start, reserved_begin + end, check_function);
            _size -= res;
            return res;
        }

#pragma endregion
#pragma region remove_one
        template <class _Fn>
        conexpr bool remove_one(_Fn check_function) {
            return remove_one(0, _size, check_function);
        }

        template <class _Fn>
        conexpr bool remove_one(size_t start, _Fn check_function) {
            return remove_one(start, _size, check_function);
        }

        template <class _Fn>
        conexpr bool remove_one(size_t start, size_t end, _Fn check_function) {
            size_t item = find_it(start, end, check_function);
            if (item == npos)
                return false;
            remove(item);
            return true;
        }

#pragma endregion
#pragma region remove_same

        template <class _Fn>
        conexpr size_t remove_same(
            const T& val,
            size_t start,
            size_t end,
            _Fn comparer = [](const T& f, const T& s) { return f == s; }
        ) {
            if (end > _size)
                throw std::out_of_range("end value out of size limit");
            if (start > _size)
                throw std::out_of_range("start value out of size limit");
            if (start > end)
                std::swap(start, end);
            size_t res = arr.remove_if(reserved_begin + start, reserved_begin + end, [&comparer, &val](const T& cval) { return comparer(val, cval); });
            _size -= res;
            return res;
        }

        template <class _Fn>
        conexpr size_t remove_same(
            const T& val,
            size_t start,
            _Fn comparer = [](const T& f, const T& s) { return f == s; }
        ) {
            return remove_same(val, start, _size, comparer);
        }

        template <class _Fn>
        conexpr size_t remove_same(
            const T& val,
            _Fn comparer = [](const T& f, const T& s) { return f == s; }
        ) {
            size_t res = arr.remove_if(reserved_begin, reserved_begin + _size, [&comparer, &val](const T& cval) { return comparer(val, cval); });
            _size -= res;
            return res;
        }

        template <size_t arr_size>
        conexpr size_t remove_same(const T (&val)[arr_size], size_t start = 0) {
            return remove_same(val, arr_size, start, _size);
        }

        template <size_t arr_size>
        conexpr size_t remove_same(const T (&val)[arr_size], size_t start, size_t end) {
            return remove_same(val, arr_size, start, end);
        }

        conexpr size_t remove_same(const T* val, size_t arr_size, size_t start = 0) {
            return remove_same(val, arr_size, start, _size);
        }

        conexpr size_t remove_same(const T* val, size_t arr_size, size_t start, size_t end) {
            size_t old_size = _size;
            size_t pos = start;
            if (start <= end) {
                while (pos != npos) {
                    auto pos = find(val, arr_size, pos, end);
                    if (pos != npos) {
                        remove(pos, pos + arr_size);
                        end -= arr_size;
                    }
                }
            } else {
                while (pos != npos) {
                    auto pos = findr(val, arr_size, pos, end);
                    if (pos != npos) {
                        remove(pos, pos + arr_size);
                        end -= arr_size;
                    }
                }
            }
            return old_size - _size;
        }

        conexpr size_t remove_same(const list_array<T>& val, size_t start = 0) {
            return remove_same(val, 0, val._size, start, _size);
        }

        conexpr size_t remove_same(const list_array<T>& val, size_t start, size_t end) {
            return remove_same(val, 0, val._size, start, end);
        }

        conexpr size_t remove_same(const list_array<T>& val, size_t val_start, size_t val_end, size_t start, size_t end) {
            size_t old_size = _size;
            size_t pos = start;
            if (start <= end) {
                while (pos != npos) {
                    auto pos = find(val, val_start, val_end, pos, end);
                    if (pos != npos) {
                        remove(pos, pos + val_end - val_start);
                        end -= val_end - val_start;
                    }
                }
            } else {
                while (pos != npos) {
                    auto pos = findr(val, val_start, val_end, pos, end);
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

        conexpr size_t find(const T& value, size_t continue_from = 0) const req(std::equality_comparable<T>) {
            return find(value, continue_from, _size);
        }

        conexpr size_t find(const T& value, size_t continue_from, size_t end) const req(std::equality_comparable<T>) {
            size_t i = 0;
            for (auto& it : range(continue_from, end)) {
                if (it == value)
                    return i;
                ++i;
            }
            return npos;
        }

        template <size_t arr_size>
        conexpr size_t find(const T (&arr)[arr_size], size_t continue_from = 0) const req(std::equality_comparable<T>) {
            return find(arr, arr_size, continue_from, _size);
        }

        template <size_t arr_size>
        conexpr size_t find(const T (&arr)[arr_size], size_t continue_from, size_t end) const req(std::equality_comparable<T>) {
            return find(arr, arr_size, continue_from, end);
        }

        conexpr size_t find(const T* arr, size_t arr_size, size_t continue_from = 0) const req(std::equality_comparable<T>) {
            return find(arr, arr_size, continue_from, _size);
        }

        conexpr size_t find(const T* arr, size_t arr_size, size_t continue_from, size_t end) const req(std::equality_comparable<T>) {
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

        conexpr size_t find(const list_array<T>& value, size_t continue_from = 0) const req(std::equality_comparable<T>) {
            return find(value, 0, value._size, continue_from, _size);
        }

        conexpr size_t find(const list_array<T>& value, size_t continue_from, size_t end_pos) const req(std::equality_comparable<T>) {
            return find(value, 0, value._size, continue_from, end_pos);
        }

        conexpr size_t find(const list_array<T>& value, size_t value_start, size_t value_end, size_t continue_from, size_t end) const req(std::equality_comparable<T>) {
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

        conexpr size_t findr(const T& value) const req(std::equality_comparable<T>) {
            return findr(value, 0, _size);
        }

        conexpr size_t findr(const T& value, size_t continue_from, size_t begin = 0) const req(std::equality_comparable<T>) {
            size_t i = 0;
            for (auto& it : reverse_range(begin, continue_from)) {
                if (it == value)
                    return i;
                ++i;
            }
            return npos;
        }

        template <size_t arr_size>
        conexpr size_t findr(const T (&arr)[arr_size]) const req(std::equality_comparable<T>) {
            return findr(arr, arr_size, _size, 0);
        }

        template <size_t arr_size>
        conexpr size_t findr(const T (&arr)[arr_size], size_t continue_from, size_t begin = 0) const req(std::equality_comparable<T>) {
            return findr(arr, arr_size, continue_from, begin);
        }

        conexpr size_t findr(const T* arr, size_t arr_size) const req(std::equality_comparable<T>) {
            return findr(arr, arr_size, _size);
        }

        conexpr size_t findr(const T* arr, size_t arr_size, size_t continue_from, size_t begin = 0) const req(std::equality_comparable<T>) {
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

        conexpr size_t findr(const list_array<T>& value) const req(std::equality_comparable<T>) {
            return findr(value, 0, value._size, _size);
        }

        conexpr size_t findr(const list_array<T>& value, size_t continue_from, size_t begin = 0) const req(std::equality_comparable<T>) {
            return findr(value, 0, value._size, continue_from, begin);
        }

        conexpr size_t findr(const list_array<T>& value, size_t value_start, size_t value_end, size_t continue_from, size_t begin = 0) const req(std::equality_comparable<T>) {
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
        conexpr size_t find_it(_Fn find_func) const {
            return find_it(0, _size, find_func);
        }

        template <class _Fn>
        conexpr size_t find_it(size_t continue_from, _Fn find_func) const {
            return find_it(continue_from, _size, find_func);
        }

        template <class _Fn>
        conexpr size_t find_it(size_t continue_from, size_t end, _Fn find_func) const {
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
        conexpr size_t findr_it(_Fn find_func) const {
            return findr_it(0, _size, find_func);
        }

        template <class _Fn>
        conexpr size_t findr_it(size_t continue_from, _Fn find_func) const {
            return findr_it(continue_from, _size, find_func);
        }

        template <class _Fn>
        conexpr size_t findr_it(size_t continue_from, size_t begin, _Fn find_func) const {
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
        conexpr list_array<T> sort_copy() const {
            return list_array<T>(*this).sort();
        }

        conexpr list_array<T>& sort() {
            if conexpr (std::is_unsigned<T>::value) {
                const T& min_val = mmin();
                size_t dif = mmax() - min_val + 1;
                list_array<size_t> count_arr(dif);
                list_array<T> result(_size);
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
            } else if conexpr (std::is_signed_v<T> && sizeof(T) <= sizeof(size_t) && !std::is_floating_point_v<T>) {
                auto normalize = [](const T& to) {
                    conexpr const size_t to_shift = sizeof(T) * 4;
                    return size_t((SIZE_MAX >> to_shift) + to);
                };
                size_t min_val = normalize(mmin());
                size_t dif = normalize(mmax()) - min_val + 1;
                list_array<size_t> count_arr(dif, 0);
                list_array<T> result(_size);
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
                T* L = new T[_size / 2 + 1];
                T* M = new T[_size / 2 + 1];
                auto fix_size = [&L, &M, &curr_L_size, &curr_M_size](size_t start, size_t middle, size_t end) {
                    size_t l = middle - start + 1;
                    size_t m = end - middle + 1;
                    if (curr_L_size < l) {
                        delete[] L;
                        L = new T[l];
                        curr_L_size = l;
                    }
                    if (curr_M_size < m) {
                        delete[] M;
                        M = new T[m];
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
                delete[] L;
                delete[] M;
            }
            return *this;
        }

        template <class _FN>
        conexpr list_array<T> sort_copy(_FN compare) const {
            return list_array<T>(*this).sort(compare);
        }

        template <class _FN>
        conexpr list_array<T>& sort(_FN compare) {
            size_t curr_L_size = _size / 2 + 1;
            size_t curr_M_size = _size / 2 + 1;
            T* L = new T[_size / 2 + 1];
            T* M = new T[_size / 2 + 1];
            auto fix_size = [&L, &M, &curr_L_size, &curr_M_size](size_t start, size_t middle, size_t end) {
                size_t l = middle - start + 1;
                size_t m = end - middle + 1;
                if (curr_L_size < l) {
                    delete[] L;
                    L = new T[l];
                    curr_L_size = l;
                }
                if (curr_M_size < m) {
                    delete[] M;
                    M = new T[m];
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
            delete[] L;
            delete[] M;
            return *this;
        }

#pragma endregion
#pragma region split
        conexpr list_array<T> split(size_t split_pos) {
            if (_size <= split_pos)
                throw std::out_of_range("Fail split due small array or split_pos is equal with array size");
            list_array<T> res(_size - split_pos);
            size_t i = 0;
            for (auto& it : range(split_pos, _size))
                res[i++] = std::move(it);
            remove(split_pos, _size);
            return res;
        }

        conexpr std::pair<list_array<T>, list_array<T>> split_copy(size_t split_pos) const {
            list_array<T> tmp(*this);
            return {tmp, tmp.split()};
        }

        conexpr list_array<list_array<T>> split_by(const T& split_value) {
            list_array<list_array<T>> res;
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


        conexpr std::pair<list_array<T>, list_array<T>> split_by_copy(const T& split_value) const {
            return list_array<T>(*this).split_by(split_value);
        }

        template <size_t arr_size>
        conexpr list_array<list_array<T>> split_by(const T (&split_values)[arr_size]) {
            return split_by(split_values, arr_size);
        }

        template <size_t arr_size>
        conexpr std::pair<list_array<T>, list_array<T>> split_by_copy(const T (&split_values)[arr_size]) const {
            return split_by_copy(split_values, arr_size);
        }

        conexpr list_array<list_array<T>> split_by(const T* split_values, size_t split_values_size) {
            list_array<list_array<T>> res;
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

        conexpr std::pair<list_array<T>, list_array<T>> split_by_copy(const T* split_values, size_t split_values_size) const {
            return list_array<T>(*this).split_by(split_values, split_values_size);
        }

        conexpr list_array<list_array<T>> split_by(const list_array<T>& split_values) {
            list_array<list_array<T>> res;
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

        conexpr std::pair<list_array<T>, list_array<T>> split_by_copy(const list_array<T>& split_values) const {
            return list_array<T>(*this).split_by(split_values);
        }

        template <class _Fn>
        conexpr list_array<list_array<T>> split_if(_Fn split_function) {
            list_array<list_array<T>> res;
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
        conexpr std::pair<list_array<T>, list_array<T>> split_if_copy(_Fn split_function) const {
            return list_array<T>(*this).split_if(split_function);
        }
#pragma endregion
#pragma region take

        conexpr list_array<T> take() {
            list_array<T> res;
            res.swap(*this);
            return res;
        }

        conexpr T take(size_t take_pos) {
            if (_size <= take_pos)
                throw std::out_of_range("Fail take item due small array");
            T res(std::move(operator[](take_pos)));
            remove(take_pos);
            return res;
        }

        conexpr T* take_raw(size_t& size) {
            if (blocks_more(1))
                commit();
            size = _size;
            T* res = arr.arr->arr_contain;
            arr.arr->arr_contain = nullptr;
            clear();
            return res;
        }

        conexpr list_array<T> take(size_t start_pos, size_t end_pos) {
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (_size < end_pos)
                    throw std::out_of_range("Fail take items due small array");
                list_array<T> res(end_pos - start_pos);
                size_t i = 0;
                for (auto& it : reverse_range(start_pos, end_pos))
                    res[i++] = std::move(it);
                remove(start_pos, end_pos);
                return res;
            } else {
                if (_size < end_pos)
                    throw std::out_of_range("Fail take items due small array");
                list_array<T> res(end_pos - start_pos);
                size_t i = 0;
                for (auto& it : range(start_pos, end_pos))
                    res[i++] = std::move(it);
                remove(start_pos, end_pos);
                return res;
            }
        }

        template <class _Fn, std::enable_if<std::is_function<_Fn>::value>>
        conexpr list_array<T> take(_Fn select_fn) {
            return take(select_fn, 0, _size);
        }

        template <class _Fn, std::enable_if<std::is_function<_Fn>::value>>
        conexpr list_array<T> take(size_t start_pos, _Fn select_fn) {
            return take(select_fn, start_pos, _size);
        }

        template <class _Fn, std::enable_if<std::is_function<_Fn>::value>>
        conexpr list_array<T> take(size_t start_pos, size_t end_pos, _Fn select_fn) {
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
                list_array<T> res;
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
                list_array<T> res;
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
        conexpr list_array<T> copy(size_t start_pos, size_t end_pos) const {
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (_size < end_pos)
                    throw std::out_of_range("Fail take items due small array");
                list_array<T> res(end_pos - start_pos);
                size_t i = 0;
                for (auto& it : reverse_range(start_pos, end_pos))
                    res[i++] = it;
                return res;
            } else {
                if (_size < end_pos)
                    throw std::out_of_range("Fail take items due small array");
                list_array<T> res(end_pos - start_pos);
                size_t i = 0;
                for (auto& it : range(start_pos, end_pos))
                    res[i++] = it;
                return res;
            }
        }

        conexpr list_array<T> copy(size_t start_pos) const {
            return copy(start_pos, _size);
        }

        conexpr list_array<T> copy() const {
            return *this;
        }

        conexpr list_array<T>& swap(list_array<T>& to_swap) noexcept {
            if (arr.arr != to_swap.arr.arr) {
                arr.swap(to_swap.arr);
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
        conexpr size_t unique() {
            return unique(0, _size);
        }

        conexpr size_t unique(size_t start_pos, size_t end_pos) {
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
        conexpr size_t unique(_Fn compare_func) {
            return unique(compare_func, 0, _size);
        }

        template <class _Fn>
        conexpr size_t unique(size_t start_pos, size_t end_pos, _Fn compare_func) {
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
        conexpr size_t unify() {
            return unify(0, _size);
        }

        conexpr size_t unify(size_t start_pos, size_t end_pos) {
            list_array<T> tmp_arr;
            tmp_arr.reserve_push_back((_size >> 2) + 1);
            for (T& it : range(start_pos, end_pos))
                if (!tmp_arr.contains(it))
                    tmp_arr.push_back(it);
            tmp_arr.shrink_to_fit();
            swap(tmp_arr);
            return tmp_arr._size - _size;
        }

        //keep only unique from all array
        conexpr size_t alone() {
            return alone(0, _size);
        }

        conexpr size_t alone(size_t start_pos, size_t end_pos) {
            if (start_pos > end_pos)
                std::swap(start_pos, end_pos);
            if (start_pos + 1 >= end_pos)
                return 0;
            if (end_pos > _size)
                throw std::out_of_range("end_pos out of size limit");
            uint8_t* selector = new uint8_t[((end_pos - start_pos) >> 3) + 1]{0};
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
            delete[] selector;
            return result;
        }

#pragma endregion
#pragma region join

        template <class _FN = bool (*)(const T&)>
        conexpr list_array<T>& join(const T& insert_item, _FN where_join) {
            return operator=(join_copy(insert_item, 0, _size, where_join));
        }

        template <class _FN>
        conexpr list_array<T> join_copy(const T& insert_item, _FN where_join) const {
            return join_copy(insert_item, 0, _size, where_join);
        }

        template <class _FN>
        conexpr list_array<T> join_copy(const T& insert_item, size_t start_pos, _FN where_join) const {
            return join_copy(insert_item, start_pos, _size, where_join);
        }

        template <class _FN>
        conexpr list_array<T> join_copy(const T& insert_item, size_t start_pos, size_t end_pos, _FN where_join) const {
            list_array<T> res;
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

        template <class _FN>
        conexpr list_array<T>& join(const list_array<T>& insert_items, _FN where_join) {
            return operator=(join_copy(insert_items, 0, _size, where_join));
        }

        template <class _FN>
        conexpr list_array<T> join_copy(const list_array<T>& insert_items, _FN where_join) const {
            return join_copy(insert_items, 0, _size, where_join);
        }

        template <class _FN>
        conexpr list_array<T> join_copy(const list_array<T>& insert_items, size_t start_pos, _FN where_join) const {
            return join_copy(insert_items, start_pos, _size, where_join);
        }

        template <class _FN>
        conexpr list_array<T> join_copy(const list_array<T>& insert_items, size_t start_pos, size_t end_pos, _FN where_join) const {
            list_array<T> res;
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
        conexpr list_array<T>& join(const T (&insert_items)[arr_size], _FN where_join) {
            return operator=(join_copy(insert_items, arr_size, 0, _size, where_join));
        }

        template <size_t arr_size, class _FN>
        conexpr list_array<T> join_copy(const T (&insert_items)[arr_size], _FN where_join) const {
            return join_copy(insert_items, arr_size, 0, _size, where_join);
        }

        template <size_t arr_size, class _FN>
        conexpr list_array<T> join_copy(const T (&insert_items)[arr_size], size_t start_pos, _FN where_join) const {
            return join_copy(insert_items, arr_size, start_pos, _size, where_join);
        }

        template <size_t arr_size, class _FN>
        conexpr list_array<T> join_copy(const T (&insert_items)[arr_size], size_t start_pos, size_t end_pos, _FN where_join) const {
            return join_copy(insert_items, arr_size, start_pos, end_pos, where_join);
        }

        template <class _FN>
        conexpr list_array<T>& join(const T* insert_items, size_t items_count, _FN where_join) {
            return operator=(join_copy(insert_items, items_count, 0, _size, where_join));
        }

        template <class _FN>
        conexpr list_array<T> join_copy(const T* insert_items, size_t items_count, _FN where_join) const {
            return join_copy(insert_items, items_count, 0, _size, where_join);
        }

        template <class _FN>
        conexpr list_array<T> join_copy(const T* insert_items, size_t items_count, size_t start_pos, _FN where_join) const {
            return join_copy(insert_items, items_count, start_pos, _size, where_join);
        }

        template <class _FN>
        conexpr list_array<T> join_copy(const T* insert_items, size_t items_count, size_t start_pos, size_t end_pos, _FN where_join) const {
            list_array<T> res;
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

        conexpr list_array<T>& join(const T& insert_item) {
            return operator=(join_copy(insert_item, 0, _size));
        }

        conexpr list_array<T> join_copy(const T& insert_item) const {
            return join_copy(insert_item, 0, _size);
        }

        conexpr list_array<T> join_copy(const T& insert_item, size_t start_pos) const {
            return join_copy(insert_item, start_pos, _size);
        }

        conexpr list_array<T> join_copy(const T& insert_item, size_t start_pos, size_t end_pos) const {
            list_array<T> res;
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

        conexpr list_array<T>& join(const list_array<T>& insert_items) {
            return operator=(join_copy(insert_items, 0, _size));
        }

        conexpr list_array<T> join_copy(const list_array<T>& insert_items) const {
            return join_copy(insert_items, 0, _size);
        }

        conexpr list_array<T> join_copy(const list_array<T>& insert_items, size_t start_pos) const {
            return join_copy(insert_items, start_pos, _size);
        }

        conexpr list_array<T> join_copy(const list_array<T>& insert_items, size_t start_pos, size_t end_pos) const {
            list_array<T> res;
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

        conexpr list_array<T>& join(const T* insert_items, size_t items_count) {
            return operator=(join_copy(insert_items, items_count, 0, _size));
        }

        conexpr list_array<T> join_copy(const T* insert_items, size_t items_count) const {
            return join_copy(insert_items, items_count, 0, _size);
        }

        conexpr list_array<T> join_copy(const T* insert_items, size_t items_count, size_t start_pos) const {
            return join_copy(insert_items, items_count, start_pos, _size);
        }

        conexpr list_array<T> join_copy(const T* insert_items, size_t items_count, size_t start_pos, size_t end_pos) const {
            list_array<T> res;
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
        conexpr list_array<T>& join(const T (&insert_items)[arr_size]) {
            return operator=(join_copy(insert_items, arr_size, 0, _size));
        }

        template <size_t arr_size>
        conexpr list_array<T> join_copy(const T (&insert_items)[arr_size]) const {
            return join_copy(insert_items, arr_size, 0, _size);
        }

        template <size_t arr_size>
        conexpr list_array<T> join_copy(const T (&insert_items)[arr_size], size_t start_pos) const {
            return join_copy(insert_items, arr_size, start_pos, _size);
        }

        template <size_t arr_size>
        conexpr list_array<T> join_copy(const T (&insert_items)[arr_size], size_t start_pos, size_t end_pos) const {
            return join_copy(insert_items, arr_size, start_pos, end_pos);
        }

#pragma endregion
#pragma region concat

        static list_array<T> concat(const list_array<list_array<T>>& concat_arr) {
            for (auto& i : concat_arr)
                push_back(i);
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
        conexpr list_array<T> where(_Fn check_fn) const {
            return where(0, _size, check_fn);
        }

        template <class _Fn>
        conexpr list_array<T> where(size_t start_pos, _Fn check_fn) const {
            return where(start_pos, _size, check_fn);
        }

        template <class _Fn>
        conexpr list_array<T> where(size_t start_pos, size_t end_pos, _Fn check_fn) const {
            list_array<T> res;
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
        conexpr list_array<T> whereI(_Fn check_fn) const {
            return whereI(0, _size, check_fn);
        }

                template <class _Fn>
                conexpr list_array<T> whereI(size_t start_pos, _Fn check_fn) const {
                    return whereI(start_pos, _size, check_fn);
                }

                template <class _Fn>
                conexpr list_array<T> whereI(size_t start_pos, size_t end_pos, _Fn check_fn) const {
                    list_array<T> res;
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
        conexpr list_array<T>& forEach(_Fn iterate_fn) {
            for (T& i : *this)
                iterate_fn(i);
            return *this;
        }

        template <class _Fn>
        conexpr list_array<T>& forEachI(_Fn iterate_fn) {
            size_t pos = 0;
            for (T& i : *this)
                iterate_fn(i, pos++);
            return *this;
        }

        template <class _Fn>
        conexpr list_array<T>& forEachR(_Fn iterate_fn) {
            for (T& i : reverse_range(0, _size))
                iterate_fn(i);
            return *this;
        }

        template <class _Fn>
        conexpr list_array<T>& forEachIR(_Fn iterate_fn) {
            size_t pos = 0;
            for (T& i : reverse_range(0, _size))
                iterate_fn(i, pos++);
            return *this;
        }

        template <class _Fn>
        conexpr list_array<T>& forEach(size_t start_pos, size_t end_pos, _Fn iterate_fn) {
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
        conexpr list_array<T>& forEachI(size_t start_pos, size_t end_pos, _Fn iterate_fn) {
            if (start_pos > end_pos) {
                std::swap(start_pos, end_pos);
                if (end_pos > _size)
                    throw std::out_of_range("end_pos out of size limit");
                size_t pos = end_pos;
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

#pragma endregion
#pragma region convert

        template <class ConvertTo, class _Fn>
        conexpr list_array<ConvertTo> convert(_Fn iterate_fn) const& {
            return convert<ConvertTo>(0, _size, iterate_fn);
        }

        template <class ConvertTo, class _Fn>
        conexpr list_array<ConvertTo> convert(size_t start_pos, _Fn iterate_fn) const& {
            return convert<ConvertTo>(start_pos, _size, iterate_fn);
        }

        template <class ConvertTo, class _Fn>
        conexpr list_array<ConvertTo> convert(size_t start_pos, size_t end_pos, _Fn iterate_fn) const& {
            list_array<ConvertTo> res;
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

        template <class ConvertTo, class _Fn>
        conexpr list_array<ConvertTo> convert(_Fn iterate_fn) && {
            return convert_take<ConvertTo>(0, _size, iterate_fn);
        }

        template <class ConvertTo, class _Fn>
        conexpr list_array<ConvertTo> convert(size_t start_pos, _Fn iterate_fn) && {
            return convert_take<ConvertTo>(start_pos, _size, iterate_fn);
        }

        template <class ConvertTo, class _Fn>
        conexpr list_array<ConvertTo> convert(size_t start_pos, size_t end_pos, _Fn iterate_fn) && {
            return convert_take<ConvertTo>(start_pos, end_pos, iterate_fn);
        }

        template <class ConvertTo, class _Fn>
        conexpr list_array<ConvertTo> convert_take(_Fn iterate_fn) {
            return convert_take<ConvertTo>(0, _size, iterate_fn);
        }

        template <class ConvertTo, class _Fn>
        conexpr list_array<ConvertTo> convert_take(size_t start_pos, _Fn iterate_fn) {
            return convert_take<ConvertTo>(start_pos, _size, iterate_fn);
        }

        template <class ConvertTo, class _Fn>
        conexpr list_array<ConvertTo> convert_take(size_t start_pos, size_t end_pos, _Fn iterate_fn) {
            list_array<T> tmp = take(start_pos, end_pos);
            list_array<ConvertTo> res;
            res.reserve_push_back(tmp.size());
            for (T& i : tmp)
                res.push_back(iterate_fn(std::move(i)));
            return res;
        }

#pragma endregion
#pragma region erase

        conexpr size_t erase(const T& val, size_t start = 0) {
            return remove_if(start, [&val](const T& cmp) { return cmp == val; });
        }

        conexpr size_t erase(const T& val, size_t start_pos, size_t end_pos) {
            return remove_if(start_pos, end_pos, [&val](const T& cmp) { return cmp == val; });
        }

        template <size_t arr_size>
        conexpr size_t erase(const T (&val)[arr_size], size_t start_pos = 0) {
            return remove_same(val, 0, arr_size, start_pos, _size);
        }

        template <size_t arr_size>
        conexpr size_t erase(const T (&val)[arr_size], size_t start_pos, size_t end_pos) {
            return remove_same(val, 0, arr_size, start_pos, end_pos);
        }

        conexpr size_t erase(const T* val, size_t val_size, size_t start_pos = 0) {
            return remove_same(val, 0, val_size, start_pos, _size);
        }

        conexpr size_t erase(const T* val, size_t val_size, size_t start_pos, size_t end_pos) {
            return remove_same(val, 0, val_size, start_pos, end_pos);
        }

        conexpr size_t erase(const list_array<T>& range) {
            return remove_same(range, 0, range._size, 0, _size);
        }

        conexpr size_t erase(const list_array<T>& range, size_t start_pos, size_t end_pos) {
            return remove_same(range, 0, range._size, start_pos, end_pos);
        }

        conexpr size_t erase(const list_array<T>& range, size_t range_start, size_t range_end, size_t start_pos, size_t end_pos) {
            return remove_same(range, range_start, range_end, start_pos, end_pos);
        }

        conexpr size_t erase_one(const T& val, size_t start_pos = 0) {
            return remove_one(start_pos, _size, [&val](const T& cmp) { return cmp == val; });
        }

        conexpr size_t erase_one(const T& val, size_t start_pos, size_t end_pos) {
            return remove_one(start_pos, end_pos, [&val](const T& cmp) { return cmp == val; });
        }

        template <size_t arr_size>
        conexpr size_t erase_one(const T (&val)[arr_size], size_t start_pos = 0) {
            return erase_one(val, start_pos, _size);
        }

        template <size_t arr_size>
        conexpr size_t erase_one(const T (&val)[arr_size], size_t start_pos, size_t end_pos) {
            size_t pos = find(val, arr_size, start_pos, end_pos);
            if (pos != npos) {
                remove(pos, arr_size);
                return arr_size;
            }
            return 0;
        }

        conexpr size_t erase_one(const T* val, size_t val_size, size_t start_pos = 0) {
            return erase_one(val, val_size, start_pos, _size);
        }

        conexpr size_t erase_one(const T* val, size_t val_size, size_t start_pos, size_t end_pos) {
            size_t pos = find(val, val_size, start_pos, end_pos);
            if (pos != npos) {
                remove(pos, val_size);
                return val_size;
            }
            return 0;
        }

        conexpr size_t erase_one(const list_array<T>& range) {
            return erase_one(range, 0, range._size, 0, _size);
        }

        conexpr size_t erase_one(const list_array<T>& range, size_t start_pos, size_t end_pos) {
            return erase_one(range, 0, range._size, start_pos, end_pos);
        }

        conexpr size_t erase_one(const list_array<T>& range, size_t range_start, size_t range_end, size_t start_pos, size_t end_pos) {
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
        conexpr bool starts_with(const T (&condition)[condition_size], size_t start_pos = 0) const {
            return starts_with(condition, condition_size, start_pos);
        }

        conexpr bool starts_with(const T* condition, size_t condition_size, size_t start_pos = 0) const {
            if (start_pos >= _size)
                return false;
            if (condition_size > _size - start_pos)
                return false;
            for (size_t i = 0; i < condition_size; i++)
                if (operator[](start_pos + i) != condition[i])
                    return false;
            return true;
        }

        conexpr bool starts_with(const T& condition, size_t start_pos = 0) const {
            if (start_pos >= _size)
                return false;
            return operator[](start_pos) == condition;
        }

        conexpr bool starts_with(const list_array<T>& condition, size_t start_pos = 0) const {
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
        conexpr bool ends_with(const T (&condition)[condition_size]) const {
            return ends_with<condition_size>(condition, condition_size, _size);
        }

        template <size_t condition_size>
        conexpr bool ends_with(const T (&condition)[condition_size], size_t end_pos) const {
            return ends_with<condition_size>(condition, condition_size, end_pos);
        }

        conexpr bool ends_with(const T* condition, size_t condition_size) const {
            return ends_with(condition, condition_size, _size);
        }

        conexpr bool ends_with(const T* condition, size_t condition_size, size_t end_pos) const {
            if (end_pos >= condition_size)
                return false;
            if (condition_size > end_pos)
                return false;
            for (size_t i = 0; i < condition_size; i++)
                if (operator[](end_pos - i - 1) != condition[condition_size - i - 1])
                    return false;
            return true;
        }

        conexpr bool ends_with(const T& condition) const {
            return ends_with(condition, _size);
        }

        conexpr bool ends_with(const T& condition, size_t end_pos) const {
            if (end_pos >= _size)
                return false;
            return operator[](end_pos - 1) == condition;
        }

        conexpr bool ends_with(const list_array<T>& condition) const {
            return ends_with(condition, _size);
        }

        conexpr bool ends_with(const list_array<T>& condition, size_t end_pos) const {
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

        conexpr reverse_provider reverse() {
            return *this;
        }

        conexpr const_reverse_provider reverse() const {
            return *this;
        }

        conexpr range_provider range(size_t start, size_t end) {
            if (start > end)
                throw std::out_of_range("start > end");
            if (end > _size)
                throw std::out_of_range("end out of size limit");
            return range_provider(*this, start, end);
        }

        conexpr reverse_provider reverse_range(size_t start, size_t end) {
            if (start > end)
                throw std::out_of_range("start > end");
            if (end > _size)
                throw std::out_of_range("end out of size limit");
            return range_provider(*this, start, end);
        }

        conexpr const_range_provider range(size_t start, size_t end) const {
            if (start > end)
                throw std::out_of_range("start > end");
            if (end > _size)
                throw std::out_of_range("end out of size limit");

            return const_range_provider(*this, start, end);
        }

        conexpr const_reverse_provider reverse_range(size_t start, size_t end) const {
            if (start > end)
                throw std::out_of_range("start > end");
            if (end > _size)
                throw std::out_of_range("end out of size limit");
            return const_range_provider(*this, start, end);
        }

        conexpr iterator get_iterator(size_t pos) {
            if (pos > _size)
                throw std::out_of_range("pos out of size limit");
            return arr.get_iterator(reserved_begin + pos);
        }

        conexpr const_iterator get_iterator(size_t pos) const {
            if (pos > _size)
                throw std::out_of_range("pos out of size limit");
            return arr.get_iterator(reserved_begin + pos);
        }

        conexpr iterator begin() {
            return arr.get_iterator(reserved_begin);
        }

        conexpr iterator end() {
            return arr.get_iterator(reserved_begin + _size);
        }

        conexpr const_iterator begin() const {
            return arr.get_iterator(reserved_begin);
        }

        conexpr const_iterator end() const {
            return arr.get_iterator(reserved_begin + _size);
        }

        conexpr reverse_iterator rbegin() {
            return arr.get_iterator(reserved_begin + _size);
        }

        conexpr reverse_iterator rend() {
            return arr.get_iterator(reserved_begin);
        }

        conexpr const_reverse_iterator rbegin() const {
            return arr.get_iterator(reserved_begin + _size);
        }

        conexpr const_reverse_iterator rend() const {
            return arr.get_iterator(reserved_begin);
        }

        conexpr const_iterator cbegin() const {
            return arr.get_iterator(reserved_begin);
        }

        conexpr const_iterator cend() const {
            return arr.get_iterator(reserved_begin + _size);
        }

        conexpr const_reverse_iterator crbegin() const {
            return arr.get_iterator(reserved_begin + _size);
        }

        conexpr const_reverse_iterator crend() const {
            return arr.get_iterator(reserved_begin);
        }

#pragma endregion
#pragma region index

        conexpr inline T& operator[](size_t pos) {
            return arr[reserved_begin + pos];
        }

        conexpr inline const T& operator[](size_t pos) const {
            return arr[reserved_begin + pos];
        }

        conexpr T& at(size_t pos) {
            if (pos >= _size)
                throw std::out_of_range("pos out of size limit");
            return arr[reserved_begin + pos];
        }

        conexpr const T& at(size_t pos) const {
            if (pos >= _size)
                throw std::out_of_range("pos out of size limit");
            return arr[reserved_begin + pos];
        }

        conexpr T atDefault(size_t pos) const {
            if (pos >= _size)
                return T();
            return arr[reserved_begin + pos];
        }

#pragma endregion
#pragma region view

        //editing with size change is not allowed when used raw pointer
        conexpr T* data() {
            if (blocks_more(1))
                commit();
            return arr.arr->arr_contain;
        }

        conexpr const T* data() const {
            if (blocks_more(1))
                throw std::runtime_error("can't get const raw pointer when blocks more than 1");
            return arr.arr->arr_contain;
        }

        conexpr const T& mmax() const {
            if (!_size)
                throw std::length_error("This list_array size is zero");
            const T* max = &operator[](0);
            for (const T& it : *this)
                if (*max < it)
                    max = &it;
            return *max;
        }

        conexpr const T& mmin() const {
            if (!_size)
                throw std::length_error("This list_array size is zero");
            const T* min = &operator[](0);
            for (const T& it : *this)
                if (*min > it)
                    min = &it;
            return *min;
        }

        conexpr T max_default() const req(std::copy_constructible<T>) {
            if (!_size)
                return T();
            const T* max = &operator[](0);
            for (const T& it : *this)
                if (*max < it)
                    max = &it;
            return *max;
        }

        conexpr T min_default() const req(std::copy_constructible<T>) {
            if (!_size)
                return T();
            const T* min = &operator[](0);
            for (const T& it : *this)
                if (*min > it)
                    min = &it;
            return *min;
        }

#pragma endregion
#pragma region to_...

        conexpr T* to_array() const {
            T* tmp = new T[_size];
            begin()._fast_load(tmp, _size);
            return tmp;
        }

#pragma endregion
#pragma region flip

        conexpr list_array<T> flip_copy() const {
            list_array<T> larr(_size);
            size_t i = 0;
            for (auto item : reverse())
                larr[i++] = item;
            return larr;
        }

        conexpr list_array<T>& flip() {
            return operator=(flip_copy());
        }

#pragma endregion
#pragma region memory

        conexpr size_t allocated() const {
            return arr._size * sizeof(T);
        }

        conexpr size_t reserved() const {
            return reserved_begin + reserved_end;
        }

        conexpr size_t reserved_back() const {
            return reserved_begin;
        }

        conexpr size_t reserved_front() const {
            return reserved_end;
        }

        conexpr void reserve_push_front(size_t reserve_size) {
            if (!reserve_size)
                return;
            reserved_begin += reserve_size;
            arr.resize_begin(reserved_begin + _size + reserved_end);
        }

        conexpr void reserve_push_back(size_t reserve_size) {
            if (!reserve_size)
                return;
            reserved_end += reserve_size;
            arr.resize_front(reserved_begin + _size + reserved_end);
        }

        conexpr void reserve(size_t reserve_size) {
            reserve_push_back(reserve_size);
        }

        conexpr size_t size() const {
            return _size;
        }

        template <bool do_shrink = false>
        conexpr void resize(size_t new_size) {
            static_assert(std::is_default_constructible<T>::value, "This type not default constructable");
            resize<do_shrink>(new_size, T());
        }

        template <bool do_shrink = false>
        conexpr void resize(size_t new_size, const T& auto_init) {
            if (new_size == 0)
                clear();
            else {
                if (reserved_end || !reserved_begin)
                    arr.resize_front(reserved_begin + new_size);
                reserved_end = 0;
                if conexpr (do_shrink) {
                    if (reserved_begin)
                        arr.resize_begin(new_size);
                    reserved_begin = 0;
                }
                size_t old_size = _size;
                _size = new_size;
                for (auto& it : range(old_size, new_size))
                    it = auto_init;
            }
            _size = new_size;
        }

        conexpr bool empty() const {
            return !_size;
        }

        conexpr void clear() {
            arr.clear();
            _size = reserved_end = reserved_begin = 0;
        }

        conexpr void shrink_to_fit() {
            resize<true>(_size);
        }

        //index optimization
        conexpr void commit() {
            T* tmp = new T[_size];
            begin()._fast_load(tmp, _size);
            arr.clear();
            arr.arr = arr.arr_end = new arr_block<T>();
            arr.arr->arr_contain = tmp;
            arr.arr->_size = _size;
            arr._size = _size;
            reserved_begin = reserved_end = 0;
        }

        //insert and remove optimization
        conexpr void decommit(size_t total_blocks) {
            if (total_blocks > _size)
                throw std::out_of_range("blocks count more than elements count");
            if (total_blocks == 0)
                throw std::out_of_range("blocks count cannot be 0");
            if (total_blocks == 1)
                return commit();
            list_array<T> tmp;
            size_t avg_block_len = _size / total_blocks;
            size_t last_block_add_len = _size % total_blocks;
            tmp.arr.arr = tmp.arr.arr_end = new arr_block<T>(nullptr, avg_block_len, nullptr);
            size_t block_iterator = 0;
            size_t new_total_blocks = 1;
            auto cur_iterator = begin();
            for (size_t i = 0; i < _size; i++) {
                if (block_iterator >= avg_block_len) {
                    if (new_total_blocks >= total_blocks) {
                        tmp.arr.arr_end->resize_front(tmp.arr.arr_end->_size + last_block_add_len);
                        for (size_t j = 0; j < last_block_add_len; j++)
                            tmp.arr.arr_end->arr_contain[avg_block_len + j] = operator[](i++);
                        break;
                    } else {
                        block_iterator = 0;
                        new_total_blocks++;
                        tmp.arr.arr_end = new arr_block<T>(tmp.arr.arr_end, avg_block_len, nullptr);
                    }
                }
                tmp.arr.arr_end->arr_contain[block_iterator++] = (*cur_iterator);
                ++cur_iterator;
            }
            arr.clear();
            arr.arr = tmp.arr.arr;
            arr.arr_end = tmp.arr.arr_end;
            tmp.arr.arr = tmp.arr.arr_end = nullptr;
            arr._size = _size;
            reserved_begin = reserved_end = 0;
        }

        conexpr bool need_commit() const {
            return arr.arr != arr.arr_end && arr.arr->next_ != arr.arr_end;
        }

        conexpr bool blocks_more(size_t blocks_count) const {
            const arr_block<T>* block = arr.arr;
            size_t res = 0;
            while (block) {
                if (++res > blocks_count)
                    return true;
                block = block->next_;
            }
            return false;
        }

        conexpr size_t blocks_count() const {
            const arr_block<T>* block = arr.arr;
            size_t res = 0;
            while (block) {
                ++res;
                block = block->next_;
            }
            return res;
        }

#pragma endregion
    };
}

template <class T>
using list_array = __list_array_impl::list_array<T>;

struct bit_list_array {
    list_array<uint8_t> arr;
    uint8_t begin_bit : 4;
    uint8_t end_bit : 4;

    class bit_refrence {
        uint8_t& byte;
        uint8_t bit;

    public:
        conexpr bit_refrence(uint8_t& byte, uint8_t bit)
            : byte(byte), bit(bit) {}

        conexpr operator bool() const {
            return (byte >> bit) & 1;
        }

        conexpr bit_refrence& operator=(bool val) {
            if (val)
                byte |= 1 << bit;
            else
                byte &= ~(1 << bit);
            return *this;
        }

        conexpr bit_refrence& operator&=(bool val) {
            if (!val)
                byte &= ~(1 << bit);
            return *this;
        }

        conexpr bit_refrence& operator|=(bool val) {
            if (val)
                byte |= 1 << bit;
            return *this;
        }

        conexpr bit_refrence& operator^=(bool val) {
            if (val)
                byte ^= 1 << bit;
            return *this;
        }
    };

public:
    conexpr bit_list_array()
        : begin_bit(0), end_bit(0) {}

    conexpr bit_list_array(size_t size)
        : arr(size / 8 + (size % 8 ? 1 : 0)), begin_bit(0), end_bit(0) {}

    conexpr bit_list_array(const bit_list_array& copy)
        : arr(copy.arr), begin_bit(copy.begin_bit), end_bit(copy.end_bit) {}

    conexpr bit_list_array(bit_list_array&& move) noexcept {
        arr.swap(move.arr);
        begin_bit = move.begin_bit;
        end_bit = move.end_bit;
    }

    conexpr size_t size() const {
        return (arr.size() - 1) * 8 + end_bit - begin_bit;
    }

    conexpr bit_list_array& operator=(const bit_list_array& copy) {
        arr = copy.arr;
        begin_bit = copy.begin_bit;
        end_bit = copy.end_bit;
        return *this;
    }

    conexpr bit_list_array& operator=(bit_list_array&& move) noexcept {
        arr.swap(move.arr);
        begin_bit = move.begin_bit;
        end_bit = move.end_bit;
        return *this;
    }

    conexpr void push_back(bool val) {
        if (end_bit == 8) {
            arr.push_back(0);
            end_bit = 0;
        }
        arr[arr.size() - 1] |= val << end_bit++;
    }

    conexpr void pop_back() {
        if (end_bit == 0) {
            arr.pop_back();
            end_bit = 8;
            return;
        }
        end_bit--;
        arr[arr.size() - 1] &= ~(1 << end_bit);
    }

    conexpr void push_front(bool val) {
        if (begin_bit == 0) {
            arr.push_front(0);
            begin_bit = 8;
        }
        arr[0] |= val << --begin_bit;
    }

    conexpr void pop_front() {
        if (begin_bit == 8) {
            arr.pop_front();
            begin_bit = 0;
            return;
        }
        arr[0] &= ~(1 << begin_bit++);
    }

    conexpr bool at(size_t pos) const {
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

    conexpr bool set(size_t pos, bool val) {
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

    conexpr bit_refrence operator[](size_t pos) {
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

    conexpr void clear() {
        arr.clear();
        begin_bit = 0;
        end_bit = 0;
    }

    conexpr void resize(size_t size) {
        arr.resize(size / 8 + (size % 8 ? 1 : 0));
    }

    conexpr void reserve_push_back(size_t size) {
        arr.reserve_push_back(size / 8 + (size % 8 ? 1 : 0));
    }

    conexpr void reserve_push_front(size_t size) {
        arr.reserve_push_front(size / 8 + (size % 8 ? 1 : 0));
    }

    conexpr void commit() {
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

    conexpr void swap(bit_list_array& to_swap) noexcept {
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

    conexpr bit_list_array& flip() {
        for (size_t i = 0; i < arr.size(); i++)
            arr[i] = ~arr[i];
        return *this;
    }

    conexpr bit_list_array operator~() const {
        return bit_list_array(*this).flip();
    }

    conexpr bit_list_array& operator&=(const bit_list_array& to_and) {
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

    conexpr bit_list_array operator&(const bit_list_array& to_and) const {
        return bit_list_array(*this) &= to_and;
    }

    conexpr bit_list_array& operator|=(const bit_list_array& to_or) {
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

    conexpr bit_list_array operator|(const bit_list_array& to_or) const {
        return bit_list_array(*this) |= to_or;
    }

    conexpr bit_list_array& operator^=(const bit_list_array& to_xor) {
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

    conexpr bit_list_array operator^(const bit_list_array& to_xor) const {
        return bit_list_array(*this) ^= to_xor;
    }

    conexpr bool operator==(const bit_list_array& to_cmp) const {
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

    conexpr bool operator!=(const bit_list_array& to_cmp) const {
        return !operator==(to_cmp);
    }

    conexpr const list_array<uint8_t>& data() const {
        return arr;
    }

    conexpr list_array<uint8_t>& data() {
        return arr;
    }
};

namespace std {
    template <class B>
    struct hash<list_array<B>> {
        conexpr size_t operator()(const list_array<B>& list) {
            std::hash<B> hasher;
            size_t res = 0;
            for (auto& it : list)
                res ^= hasher(it) + 0x9e3779b9 + (res << 6) + (res >> 2);
            return res;
        }
    };
}

#undef req
#undef conexpr
#endif /* SRC_LIBRARY_LIST_ARRAY_LIST_ARRAY */
