#include <stdint.h>
#include <utility>
#include <type_traits>
#include <functional>
#include <stdexcept>
#include <iterator>


template<class T>
class list_array {
	template<class T>
	class dynamic_arr;
	template<class T>
	class arr_block; 
	template<class T>
	static T* cxxresize(T* val, size_t old_size, size_t new_size) {
		if (new_size == old_size)
			return val;
		T* new_val = new T[new_size];
		if (old_size < new_size) {
			if constexpr (std::is_move_assignable<T>::value)
				for (size_t i = 0; i < old_size; i++)
					new_val[i] = std::move(val[i]);
			else
				for (size_t i = 0; i < old_size; i++)
					new_val[i] = val[i];
		}
		else {
			if constexpr (std::is_move_assignable<T>::value)
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
	using value_type = T;
	using reference = T&;
	using const_reference = const T&;
	using size_type = size_t;
	using difference_type = ptrdiff_t;

	template<class T>
	class reverse_iterator;
	template<class T>
	class const_iterator;
	template<class T>
	class const_reverse_iterator;
	template<class T>
	class iterator : std::iterator<std::forward_iterator_tag,T> {
		friend class dynamic_arr<T>;
		friend class const_iterator<T>;
		friend class reverse_iterator<T>;
		friend class const_reverse_iterator<T>;
		arr_block<T>* block;
		size_t pos;
	public:
		iterator() { block = nullptr; pos = 0; }
		iterator& operator=(const iterator& seter) {
			block = seter.block;
			pos = seter.pos;
			return *this;
		}
		iterator(const iterator& copy) {
			*this = copy;
		}
		iterator(arr_block<T>* block_pos, size_t set_pos) {
			block = block_pos;
			pos = set_pos;
		}
		iterator& operator++() {
			if (block) {
				if (block->_size <= ++pos) {
					block = block->next_;
					pos = 0;
				}
			}
			return *this;
		}
		iterator operator++(int) const {
			iterator tmp = *this;
			++const_cast<iterator&>(*this);
			return tmp;
		}
		iterator& operator--() {
			if (block) {
				if (0 == --pos) {
					block = block->_prev;
					pos = block->_size;
				}
			}
			return *this;
		}
		iterator operator--(int) const {
			iterator tmp = *this;
			--const_cast<iterator&>(*this);
			return tmp;
		}
		bool operator==(const iterator& comparer) const {
			return block == comparer.block && pos == comparer.pos;
		}
		bool operator!=(const iterator& comparer) const {
			return !(*this == comparer) && block ? pos < block->_size : 0;
		}
		T& operator*() { return block->arr_contain[pos]; }
		const T& operator*() const { return block->arr_contain[pos]; }
		iterator& operator->() { return *this; }
		void fast_load(T* arr, size_t arr_size) {
			size_t j = pos;
			arr_block<T>* block_tmp = block;
			size_t block_size = block_tmp->_size;
			T* block_arr = block->arr_contain;

			for (size_t i = 0; i < arr_size;) {
				for (; i < arr_size && j < block_size; j++)
					arr[i++] = block_arr[j];
				j = 0;
				block_tmp = block_tmp->next_;
				if (!block_tmp)return;
				block_size = block_tmp->_size;
				block_arr = block_tmp->arr_contain;
			}
		}
		bool nextBlock() {
			block = block ? block->next_ : block;
			pos = 0;
			return block && (block ? block->next_ : block);
		}
	};
	template<class T>
	class const_iterator : std::iterator<std::forward_iterator_tag, T> {
		friend class dynamic_arr<T>;
		friend class const_reverse_iterator<T>;
		arr_block<T>* block;
		size_t pos;
	public:
		const_iterator() { block = nullptr; pos = 0; }
		const_iterator& operator=(const const_iterator& seter) {
			block = seter.block;
			pos = seter.pos;
			return *this;
		}
		const_iterator(const const_iterator& copy) {
			*this = copy;
		}
		const_iterator& operator=(const iterator<T>& seter) {
			block = seter.block;
			pos = seter.pos;
			return *this;
		}
		const_iterator(const iterator<T>& copy) {
			*this = copy;
		}
		const_iterator(arr_block<T>* block_pos, size_t set_pos) {
			block = block_pos;
			pos = set_pos;
		}
		const_iterator& operator++() {
			if (block) {
				if (block->_size <= ++pos) {
					block = block->next_;
					pos = 0;
				}
			}
			return *this;
		}
		const_iterator operator++(int) {
			const_iterator tmp = *this;
			++(*this);
			return tmp;
		}
		const_iterator& operator--() {
			if (block) {
				if (0 == --pos) {
					block = block->_prev;
					pos = block->_size;
				}
			}
			return *this;
		}
		const_iterator operator--(int) {
			const_iterator tmp = *this;
			--(*this);
			return tmp;
		}
		bool operator==(const const_iterator& comparer) const {
			return block == comparer.block && pos == comparer.pos;
		}
		bool operator!=(const const_iterator& comparer) const {
			return !(*this == comparer) && (block ? pos < block->_size : 0);
		}
		const T& operator*() { return block->arr_contain[pos]; }
		const_iterator operator->() { return *this; }
	};
	template<class T>
	class reverse_iterator : std::iterator<std::forward_iterator_tag, T> {
		friend class dynamic_arr<T>;
		friend class const_reverse_iterator<T>;
		arr_block<T>* block;
		size_t pos;
	public:
		reverse_iterator() { block = nullptr; pos = 0; }
		reverse_iterator& operator=(const iterator<T>& seter) {
			block = seter.block;
			pos = seter.pos;
			return *this;
		}
		reverse_iterator(const iterator<T>& copy) {
			*this = copy;
		}
		reverse_iterator& operator=(const reverse_iterator& seter) {
			block = seter.block;
			pos = seter.pos;
			return *this;
		}
		reverse_iterator(const reverse_iterator& copy) {
			*this = copy;
		}
		reverse_iterator(arr_block<T>* block_pos, size_t set_pos) {
			block = block_pos;
			pos = set_pos;
		}
		reverse_iterator& operator++() {
			if (block) {
				if (0 == pos--) {
					block = block->_prev;
					pos = block ? block->_size - 1 : 0;
				}
			}
			return *this;
		}
		reverse_iterator operator++(int) {
			reverse_iterator tmp = *this;
			++(*this);
			return tmp;
		}
		reverse_iterator& operator--() {
			if (block) {
				if (block->_size == ++pos) {
					block = block->next_;
					pos = 0;
				}
			}
			return *this;
		}
		reverse_iterator operator--(int) {
			reverse_iterator tmp = *this;
			--(*this);
			return tmp;
		}
		bool operator==(const reverse_iterator& comparer) const {
			return block == comparer.block && pos == comparer.pos;
		}
		bool operator!=(const reverse_iterator& comparer) const {
			return !(*this == comparer) && block;
		}
		T& operator*() {
			auto tmp = *this;
			++tmp;
			return tmp.block->arr_contain[tmp.pos];
		}
		const T& operator*() const {
			auto tmp = *this;
			++tmp;
			return tmp.block->arr_contain[tmp.pos];
		}
		reverse_iterator& operator->() {
			return *this;
		}
	};
	template<class T>
	class const_reverse_iterator : std::iterator<std::forward_iterator_tag, T> {
		friend class dynamic_arr<T>;
		friend class const_iterator<T>;
		arr_block<T>* block;
		size_t pos;
	public:
		const_reverse_iterator() { block = nullptr; pos = 0; }
		const_reverse_iterator& operator=(const const_iterator<T>& seter) {
			block = seter.block;
			pos = seter.pos;
			return *this;
		}
		const_reverse_iterator(const const_iterator<T>& copy) {
			*this = copy;
		}
		const_reverse_iterator& operator=(const iterator<T>& seter) {
			block = seter.block;
			pos = seter.pos;
			return *this;
		}
		const_reverse_iterator(const iterator<T>& copy) {
			*this = copy;
		}
		const_reverse_iterator& operator=(const reverse_iterator<T>& seter) {
			block = seter.block;
			pos = seter.pos;
			return *this;
		}
		const_reverse_iterator(const reverse_iterator<T>& copy) {
			*this = copy;
		}
		const_reverse_iterator& operator=(const const_reverse_iterator& seter) {
			block = seter.block;
			pos = seter.pos;
			return *this;
		}
		const_reverse_iterator(const const_reverse_iterator& copy) {
			*this = copy;
		}
		const_reverse_iterator(arr_block<T>* block_pos, size_t set_pos) {
			block = block_pos;
			pos = set_pos;
		}
		const_reverse_iterator& operator++() {
			if (block) {
				if (0 == pos--) {
					block = block->_prev;
					pos = block ? block->_size - 1: 0;
				}
			}
			return *this;
		}
		const_reverse_iterator operator++(int) {
			const_reverse_iterator tmp = *this;
			++(*this);
			return tmp;
		}
		const_reverse_iterator& operator--() {
			if (block) {
				if (block->_size == ++pos) {
					block = block->next_;
					pos = 0;
				}
			}
			return *this;
		}
		const_reverse_iterator operator--(int) {
			const_reverse_iterator tmp = *this;
			--(*this);
			return tmp;
		}
		bool operator==(const const_reverse_iterator& comparer) const {
			return block == comparer.block && pos == comparer.pos;
		}
		bool operator!=(const const_reverse_iterator& comparer) const {
			return !(*this == comparer) && block;
		}
		const T& operator*() const {
			auto tmp = *this;
			++tmp;
			return tmp.block->arr_contain[tmp.pos];
		}
		const_reverse_iterator& operator->() { return *this; }
	};

	class range_provider {
		size_t _start;
		size_t _end;
		list_array<T>& ln;
	public:
		range_provider(list_array<T>& link, size_t start, size_t end) : ln(link) { _start = start; _end = end; }
		range_provider(const range_provider& copy) : ln(copy.ln) { _start = copy._start; _end = copy._end; }
		iterator<T> begin() {
			return ln.get_iterator(_start);
		}
		iterator<T> end() {
			return ln.get_iterator(_end);
		}
		const_iterator<T> begin() const {
			return ln.get_iterator(_start);
		}
		const_iterator<T> end() const {
			return ln.get_iterator(_end);
		}
		reverse_iterator<T> rbegin() {
			return ln.get_iterator(_end);
		}
		reverse_iterator<T> rend() {
			return ln.get_iterator(_start);
		}
		const_reverse_iterator<T> rbegin() const {
			return ln.get_iterator(_end);
		}
		const_reverse_iterator<T> rend() const {
			return ln.get_iterator(_start);
		}
		size_t range_start() const {
			return _start;
		}
		size_t range_end() const {
			return _end;
		}
	};
	class const_range_provider {
		size_t _start;
		size_t _end;
		const list_array<T>& ln;
	public:
		const_range_provider(const list_array<T>& link, size_t start, size_t end) : ln(link) { _start = start; _end = end; }
		const_range_provider(const const_range_provider& copy) : ln(copy.ln) { _start = copy._start; _end = copy._end; }
		const_iterator<T> begin() const {
			return ln.get_iterator(_start);
		}
		const_iterator<T> end() const {
			return ln.get_iterator(_end);
		}
		const_reverse_iterator<T> rbegin() const {
			return ln.get_iterator(_end);
		}
		const_reverse_iterator<T> rend() const {
			return ln.get_iterator(_start);
		}
	};
 	class reverse_provider {
		reverse_iterator<T> _begin;
		reverse_iterator<T> _end;
	public:
		reverse_provider(range_provider& link) { _begin = link.rbegin(); _end = link.rend(); }
		reverse_provider(range_provider&& link) { _begin = link.rbegin(); _end = link.rend(); }
		reverse_provider(list_array<T>& link) { _begin = link.rbegin(); _end = link.rend(); }
		reverse_provider(const reverse_provider& copy) { _begin = copy._begin; _end = copy._end; }
		reverse_iterator<T> begin() {
			return _begin;
		}
		reverse_iterator<T> end() {
			return _end;
		}
		const_reverse_iterator<T> begin() const {
			return _begin;
		}
		const_reverse_iterator<T> end() const {
			return _end;
		}
	};
	class const_reverse_provider {
		const_reverse_iterator<T> _begin;
		const_reverse_iterator<T> _end;
	public:
		const_reverse_provider(const range_provider& link) { _begin = link.rbegin(); _end = link.rend(); }
		const_reverse_provider(const range_provider&& link) { _begin = link.rbegin(); _end = link.rend(); }
		const_reverse_provider(const const_range_provider& link) { _begin = link.rbegin(); _end = link.rend(); }
		const_reverse_provider(const const_range_provider&& link) { _begin = link.rbegin(); _end = link.rend(); }
		const_reverse_provider(const list_array<T>& link) { _begin = link.rbegin(); _end = link.rend(); }
		const_reverse_provider(const const_reverse_provider& copy) { _begin = copy._begin; _end = copy._end; }
		const_reverse_iterator<T> begin() const {
			return _begin;
		}
		const_reverse_iterator<T> end() const {
			return _end;
		}
	};
private:
	//block item abstraction
	template<class T>
	class arr_block {
		friend class list_array<T>;
		friend class dynamic_arr<T>;
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
	public:
		arr_block() {}
		arr_block(const arr_block& copy) { operator=(copy); }
		arr_block(arr_block&& move) noexcept { operator=(std::move(move)); }
		arr_block(arr_block* prev, size_t len, arr_block* next) {
			if (_prev = prev)
				_prev->next_ = this;
			if (next_ = next)
				next_->_prev = this;
			arr_contain = new T[len];
			_size = len;
		}
		~arr_block() {
			if (_prev) {
				_prev->next_ = nullptr;
				delete _prev;
			}
			else if (next_) {
				next_->_prev = nullptr;
				delete next_;
			}
			if (arr_contain)
				delete[] arr_contain;
		}
		T& operator[](size_t pos) {
			return (pos < _size) ? arr_contain[pos] : (*next_)[pos - _size];
		}
		const T& operator[](size_t pos) const {
			return (pos < _size) ? arr_contain[pos] : (*next_)[pos - _size];
		}
		arr_block& operator=(const arr_block& copy) {
			_size = copy._size;
			arr_contain = new T[_size];
			for (size_t i = 0; i < _size; i++)
				arr_contain[i] = copy.arr_contain[i];
		}
		arr_block& operator=(arr_block&& move) noexcept {
			arr_contain = move.arr_contain;
			_prev = move._prev;
			next_ = move.next_;
			_size = move._size;
			move._prev = move.next_ = nullptr;
			move.arr_contain = nullptr;
		}
		T& index_back(size_t pos) {
			return (pos < _size) ? arr_contain[_size - pos - 1] : (*_prev).index_back(pos - _size);
		}
		T& index_front(size_t pos) {
			return (pos < _size) ? arr_contain[pos] : (*next_)[pos - _size];
		}
		const T& index_back(size_t pos) const {
			return (pos < _size) ? arr_contain[_size - pos - 1] : (*_prev).index_back(pos - _size);
		}
		const T& index_front(size_t pos) const {
			return (pos < _size) ? arr_contain[pos] : (*next_)[pos - _size];
		}
		iterator<T> get_iterator(size_t pos) {
			if (!this) return iterator<T>(nullptr, pos);
			return
				this ?
				((pos < _size) ?
					iterator<T>(this, pos) :
					(*next_).get_iterator(pos - _size)
					) :
				iterator<T>(nullptr, 0);
		}
		iterator<T> get_iterator_back(size_t pos) {
			if (!this) return iterator<T>(nullptr, 0);
			return
				this ?
				((pos < _size) ?
					iterator<T>(this, _size - pos - 1) :
					(*_prev).get_iterator_back(pos - _size)
					) :
				iterator<T>(nullptr, 0);
		}
		const const_iterator<T> get_iterator(size_t pos) const {
			if (!this) return const_iterator<T>(nullptr, pos);
			return
				this ?
				((pos < _size) ?
					const_iterator<T>(this, pos) :
					(*next_).get_iterator(pos - _size)
					) :
				const_iterator<T>(nullptr, 0);
		}
		const const_iterator<T> get_iterator_back(size_t pos) const {
			if (!this) return const_iterator<T>(nullptr, 0);
			return
				this ?
				((pos < _size) ?
					const_iterator<T>(this, _size - pos - 1) :
					(*_prev).get_iterator_back(pos - _size)
					) :
				const_iterator<T>(nullptr, 0);
		}
		iterator<T> begin() {
			return
				this ? (
					_prev ?
					_prev->begin() :
					iterator<T>(this, 0)
					) :
				iterator<T>(nullptr, 0)
				;
		}
		iterator<T> end() {
			return
				this ? (
					next_ ?
					next_->end() :
					iterator<T>(this, _size)
					) :
				iterator<T>(nullptr, 0)
				;
		}
		const_iterator<T> begin() const {
			return
				this ? (
					_prev ?
					_prev->begin() :
					const_iterator<T>(this, 0)
					) :
				const_iterator<T>(nullptr, 0)
				;
		}
		const_iterator<T> end() const {
			return
				this ? (
					next_ ?
					next_->end() :
					iterator<T>(this, _size)
					) :
				iterator<T>(nullptr, 0)
				;
		}
		inline size_t size() const {
			return _size;
		}
		void resize_front(size_t siz) {
			if (!siz) {
				good_bye_world();
				return;
			}
			T* tmp = arr_contain;
			arr_contain = cxxresize<T>(arr_contain, _size, siz);
			if (arr_contain == nullptr)
			{
				arr_contain = tmp;
				throw std::bad_alloc();
			}
			_size = siz;
		}
		void resize_begin(size_t siz) {
			if (!siz) {
				good_bye_world();
				return;
			}
			T* narr = new T[siz];
			if (narr == nullptr)
				throw std::bad_alloc();

			int64_t dif = _size - siz;
			if (dif > 0)
				for (size_t i = 0; i < siz && i < _size; i++)
					narr[i] = arr_contain[i + dif];
			else {
				dif *= -1;
				for (size_t i = 0; i < siz && i < _size; i++)
					narr[dif + i] = arr_contain[i];
			}
			delete[] arr_contain;
			arr_contain = narr;
			_size = siz;
		}
	};
	//list of arrays abstraction
	template<class T>
	class dynamic_arr {
		friend class list_array<T>;
		arr_block<T>* arr = nullptr;
		arr_block<T>* arr_end = nullptr;
		size_t _size = 0;

		void swap_block_with_blocks(arr_block<T>& this_block, arr_block<T>& first_block, arr_block<T>& second_block) {
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


		void remove_item_slow(size_t pos, arr_block<T>& this_block) {
			if (pos > this_block._size / 2) {
				size_t mov_to = this_block._size - 1;
				for (int64_t i = pos; i < mov_to; i++)
					std::swap(this_block.arr_contain[i + 1], this_block.arr_contain[i]);
				this_block.resize_front(this_block._size - 1);
			}
			else
			{
				for (int64_t i = pos; i > 0; i--)
					std::swap(this_block.arr_contain[i - 1], this_block.arr_contain[i]);
				this_block.resize_begin(this_block._size - 1);
			}
		}
		void remove_item_split(size_t pos, arr_block<T>& this_block) {
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

		void insert_item_slow(size_t pos, arr_block<T>& this_block, const T& item) {
			if (pos > this_block._size / 2) {
				this_block.resize_front(this_block._size + 1);
				size_t mov_to = this_block._size - 1;
				for (int64_t i = mov_to - 1; i >= pos; i--)
					std::swap(this_block.arr_contain[i + 1], this_block.arr_contain[i]);
				this_block.arr_contain[pos] = item;
			}
			else
			{
				this_block.resize_begin(this_block._size + 1);
				for (int64_t i = 0; i < pos; i++)
					std::swap(this_block.arr_contain[i + 1], this_block.arr_contain[i]);
				this_block.arr_contain[pos] = item;
			}
		}
		void insert_item_split(size_t pos, arr_block<T>& this_block, const T& item) {
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
		void insert_item_slow(size_t pos, arr_block<T>& this_block, T&& item) {
			if (pos > this_block._size / 2) {
				this_block.resize_front(this_block._size + 1);
				size_t mov_to = this_block._size - 1;
				for (int64_t i = mov_to - 1; i >= pos; i--)
					std::swap(this_block.arr_contain[i + 1], this_block.arr_contain[i]);
				this_block.arr_contain[pos] = std::move(item);
			}
			else
			{
				this_block.resize_begin(this_block._size + 1);
				for (int64_t i = 0; i < pos; i++)
					std::swap(this_block.arr_contain[i + 1], this_block.arr_contain[i]);
				this_block.arr_contain[pos] = std::move(item);
			}
		}
		void insert_item_split(size_t pos, arr_block<T>& this_block, T&& item) {
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

		void insert_block_split(size_t pos, arr_block<T>& this_block, const T* item, size_t item_size) {
			size_t block_size = this_block._size;
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
			_size += item_size;
		}


		size_t _remove_items(arr_block<T>* block, size_t start, size_t end) {
			size_t size = block->_size;
			size_t new_size = block->_size - (end - start);
			if (new_size == 0) {
				if (arr == block)
					arr = block->next_;
				if (arr_end == block)
					arr_end = block->_prev;
				block->good_bye_world();
			}
			else {
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
		template<class _Fn>
		size_t _remove_if(arr_block<T>* block, _Fn func, size_t start, size_t end) {
			// value >> 3  ==  value / 8
			size_t size = block->_size;
			size_t rem_filt_siz =
				size > end ?
				(size >> 3) + (size & 7 ? 1 : 0) :
				(end >> 3) + (end & 7 ? 1 : 0);
			uint8_t* remove_filter = new uint8_t[rem_filt_siz](0);
			T* arr_inter = block->arr_contain;
			size_t new_size = size;

			for (size_t i = start; i < end; i++) {
				if (func(arr_inter[i])) {
					remove_filter[i >> 3] |= 1 << (i & 7);
					new_size--;
				}
			}
			if (size != new_size) {
				if (new_size == 0 && size == end) {
					if (arr == block)
						arr = block->next_;
					if (arr_end == block)
						arr_end = block->_prev;
					block->good_bye_world();
				}
				else {
					T* new_arr = new T[new_size];
					size_t j = 0;
					for (size_t i = 0; j < new_size && i < end; i++)
						if (!(remove_filter[i >> 3] & (1 << (i & 7))))
							new_arr[j++] = arr_inter[i];
					delete[] block->arr_contain;
					block->arr_contain = new_arr;
					block->_size = new_size;
				}
			}
			delete[] remove_filter;
			return size - new_size;
		}
	public:
		void safe_destruct() {
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
		}
		dynamic_arr() {}
		dynamic_arr(const dynamic_arr& copy) {
			operator=(copy);
		}
		dynamic_arr(dynamic_arr&& move) noexcept {
			operator=(std::move(move));
		}
		dynamic_arr& operator=(dynamic_arr&& move) noexcept {
			arr = move.arr;
			arr_end = move.arr_end;
			_size = move._size;
			move.arr = move.arr_end = nullptr;
			return *this;
		}
		dynamic_arr& operator=(const dynamic_arr& copy) {
			arr_block<T>& tmp = *(arr = arr_end = new arr_block<T>(nullptr, _size = copy._size, nullptr));
			size_t i = 0;
			for (auto& it : copy)
				tmp[i] = it;
		}
		~dynamic_arr() {
			safe_destruct();
		}
		T& operator[](size_t pos) {
			return
				(pos < (_size >> 1)) ?
				arr->operator[](pos) :
				arr_end->index_back(_size - pos - 1)
				;
		}
		const T& operator[](size_t pos) const {
			return
				(pos < (_size >> 1)) ?
				arr->operator[](pos) :
				arr_end->index_back(_size - pos - 1)
				;
		}
		T& index_back(size_t pos) {
			return arr_end->index_back(_size - pos - 1);
		}
		const T& index_back(size_t pos) const {
			return arr_end->index_back(_size - pos - 1);
		}
		iterator<T> get_iterator(size_t pos) {
			return
				(pos < (_size >> 1)) ?
				arr->get_iterator(pos) :
				arr_end->get_iterator_back(_size - pos - 1)
				;
		}
		const_iterator<T> get_iterator(size_t pos) const {
			return
				(pos < (_size >> 1)) ?
				arr->get_iterator(pos) :
				arr_end->get_iterator_back(_size - pos - 1)
				;
		}
		auto begin() {
			return arr->begin();
		}
		auto end() {
			return arr_end->end();
		}
		auto begin() const {
			return arr->begin();
		}
		auto end() const {
			return arr_end->end();
		}
		size_t size() const {
			return _size;
		}
		void resize_begin(size_t new_size) {
			size_t tsize = _size;
			if (tsize >= new_size) {
				for (int64_t resizer = tsize - new_size; resizer > 0;) {
					if (arr->_size > resizer) {
						_size = new_size;
						arr->resize_begin(arr->_size - resizer);
						return;
					}
					else {
						resizer -= arr->_size;
						if (auto tmp = arr->next_) {
							arr->next_->_prev = nullptr;
							arr->next_ = nullptr;
							delete arr;
							arr = tmp;
						}
						else {
							delete arr;
							arr_end = nullptr;
							arr = nullptr;
							_size = 0;
							return;
						}
					}
				}
			}
			else {
				if (arr)
					if (arr->_size + new_size <= _size >> 1) {
						arr->resize_begin(new_size - tsize + arr_end->_size);
						goto end;
					}
				arr = new arr_block<T>(nullptr, new_size - tsize, arr);
			end:
				if (!arr_end) arr_end = arr;
			}
			_size = new_size;
		}
		void resize_front(size_t new_size) {
			size_t tsize = _size;
			if (tsize >= new_size) {
				for (int64_t resizer = tsize - new_size; resizer > 0;) {
					if (arr_end->_size > resizer) {
						_size = new_size;
						arr_end->resize_front(arr_end->_size - resizer);
						return;
					}
					else {
						resizer -= arr_end->_size;
						if (auto tmp = arr_end->_prev) {
							arr_end->_prev->next_ = nullptr;
							arr_end->_prev = nullptr;
							delete arr_end;
							arr_end = tmp;
						}
						else {
							delete arr_end;
							arr_end = nullptr;
							arr = nullptr;
							_size = 0;
							return;
						}
					}
				}
			}
			else {
				if (arr_end)
					if (arr_end->_size + new_size <= _size >> 1) {
						arr_end->resize_front(new_size - tsize + arr_end->_size);
						goto end;
					}
				arr_end = new arr_block<T>(arr_end, new_size - tsize, nullptr);
			end:
				if (!arr) arr = arr_end;
			}
			_size = new_size;
		}
		void insert_block(size_t pos, const T* item, size_t item_size) {
			if (pos == _size) {
				resize_front(_size + item_size);
				auto iterator = get_iterator(_size - item_size);
				for (size_t i = 0; i < item_size; i++) {
					*iterator = item[i];
					++iterator;
				}
			}
			else if (pos == 0) {
				resize_begin(_size + item_size);
				auto iterator = get_iterator(0);
				for (size_t i = 0; i < item_size; i++) {
					*iterator = item[i];
					++iterator;
				}
			}
			else
				insert_block_split(pos, *get_iterator(pos).block, item, item_size);
		}
		void insert_block(size_t pos, const arr_block<T>& item) {
			insert_block_split(pos, *get_iterator(pos).block, item.arr_contain, item._size);
		}
		void insert(size_t pos, const T& item) {
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
			}
			else if (inter.pos == this_block._size - 1) {
				this_block.resize_front(this_block._size + 1);
				this_block[this_block._size - 1] = item;
			}
			else if (this_block._size <= 50000)
				insert_item_slow(inter.pos, this_block, item);
			else
				insert_item_split(inter.pos, this_block, item);
			_size++;
		}
		void insert(size_t pos, T&& item) {
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
			}
			else if (inter.pos == this_block._size - 1) {
				this_block.resize_front(this_block._size + 1);
				this_block[this_block._size - 1] = std::move(item);
			}
			else if (this_block._size <= 50000)
				insert_item_slow(inter.pos, this_block, std::move(item));
			else
				insert_item_split(inter.pos, this_block, std::move(item));
			_size++;
		}
		void remove_item(size_t pos) {
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
				}
				else
					this_block.resize_begin(this_block._size - 1);
			}
			else if (inter.pos == this_block._size - 1)
				this_block.resize_front(this_block._size - 1);
			else if (this_block._size <= 50000)
				remove_item_slow(inter.pos, this_block);
			else
				remove_item_split(inter.pos, this_block);
			_size--;
		}
		size_t remove_items(size_t start_pos, size_t end_pos) {
			iterator<T> interate = get_iterator(start_pos);
			iterator<T> _end = get_iterator(end_pos);
			size_t removed = 0;
			if (interate.block == _end.block) {
				removed = _remove_items(interate.block, interate.pos, _end.pos);
				_size -= removed;
				return removed;
			}

			removed += _remove_items(interate.block, interate.pos, interate.block->_size);
			for (;;) {
				if (!interate.nextBlock())
					break;
				removed += _remove_items(interate.block, 0, interate.block->_size);
			}

			if (_end.block)
				removed += _remove_items(_end.block, 0, _end.pos);
			else if (interate.block)
				removed += _remove_items(interate.block, 0, interate.block->_size);

			_size -= removed;
			return removed;
		}
		template<class _Fn>
		size_t remove_if(_Fn func,size_t start,size_t end) {
			if (!_size)
				return 0;
			iterator<T> interate = get_iterator(start);
			iterator<T> _end = get_iterator(end);
			size_t removed = 0;
			if (interate.block == _end.block) {
				removed = _remove_if(interate.block, func, interate.pos, _end.pos);
				_size -= removed;
				return removed;
			}

			removed += _remove_if(interate.block, func, interate.pos, interate.block->_size);
			for (;;) {
				if (!interate.nextBlock())
					break;
				removed += _remove_if(interate.block, func, 0, interate.block->_size);
			}

			if(_end.block)
				removed += _remove_if(_end.block, func, 0, _end.pos);
			else if (interate.block)
				removed += _remove_if(interate.block, func, 0, interate.block->_size);

			_size -= removed;
			return removed;
		}
	};
	dynamic_arr<T> arr;
	size_t reserved_begin = 0;
	size_t _size = 0;
	size_t reserved_end = 0;
	void safe_destruct() {
		arr.safe_destruct();
		_size = reserved_end = reserved_begin = 0;
	}
public:
	list_array() {}
	list_array(const std::initializer_list<T>& vals) {
		resize(vals.size());
		size_t i = 0;
		for (const T& it : vals)
			operator[](i++) = it;
	}
	list_array(size_t size) {
		resize(size);
	}
	list_array(list_array&& move) noexcept {
		operator=(std::move(move));
	}
	list_array(const list_array& copy) noexcept {
		operator=(copy);
	}
	list_array& operator=(list_array&& move) noexcept {
		arr = std::move(move.arr);
		reserved_begin = move.reserved_begin;
		_size = move._size;
		reserved_end = move.reserved_end;
		return *this;
	}
	list_array& operator=(const list_array& copy) {
		arr = copy.arr;
		reserved_begin = copy.reserved_begin;
		_size = copy._size;
		reserved_end = copy.reserved_end;
	}
	size_t alocated() const {
		return arr._size * sizeof(T);
	}
	size_t size() const {
		return _size;
	}
	template<bool do_shrink = false>
	auto resize(size_t new_size) {
		static_assert(std::is_default_constructible<T>::value, "This type not default constructable");
		resize<do_shrink>(new_size, T());
	}
	template<bool do_shrink = false>
	void resize(size_t new_size, const T& auto_init) {
		if (new_size == 0) safe_destruct();
		else {
			if (reserved_end || !reserved_begin) arr.resize_front(reserved_begin + new_size);
			reserved_end = 0;
			if constexpr (do_shrink) {
				if (reserved_begin) arr.resize_begin(new_size);
				reserved_begin = 0;
			}
			for (size_t i = _size; i < new_size; i++)
				(*this)[i] = auto_init;
		}
		_size = new_size;
	}

	void remove(size_t pos) {
		if (_size == 0)return;
		if (pos > _size)
			throw std::out_of_range("pos value(" + std::to_string(pos) + ") out of size limit(" + std::to_string(_size) + ")");
		arr.remove_item(reserved_begin + pos);
		_size--;
		if (_size == 0) return safe_destruct();
	}
	void remove(size_t start_pos, size_t end_pos) {
		if (start_pos > end_pos)
			std::swap(start_pos, end_pos);
		if (end_pos > _size)
			throw std::out_of_range("end_pos value(" + std::to_string(end_pos) + ") out of size limit(" + std::to_string(_size) + ")");
		if (start_pos > _size)
			throw std::out_of_range("start_pos value(" + std::to_string(start_pos) + ") out of size limit(" + std::to_string(_size) + ")");
		_size -= arr.remove_items(reserved_begin + start_pos, reserved_begin + end_pos);
	}

	void reserve_push_begin(size_t reserve_size) {
		reserved_begin += reserve_size;
		arr.resize_begin(reserved_begin + _size + reserved_end);
	}
	void push_begin(const T& copyer) {
		if (reserved_begin) {
			arr[--reserved_begin] = copyer;
			_size++;
		}
		else {
			reserve_push_begin(_size + 1);
			push_begin(copyer);
		}
	}
	void push_begin(T&& copyer) {
		if (reserved_begin) {
			arr[--reserved_begin] = copyer;
			_size++;
		}
		else {
			reserve_push_begin(_size + 1);
			push_begin(std::move(copyer));
		}
	}

	void reserve_push_front(size_t reserve_size) {
		reserved_end += reserve_size;
		arr.resize_front(reserved_begin + _size + reserved_end);
	}
	void push_front(const T& copyer) {
		if (reserved_end) {
			arr[reserved_begin + _size++] = copyer;
			--reserved_end;
		}
		else {
			reserve_push_front(_size + 1);
			push_front(copyer);
		}
	}
	void push_front(T&& copyer) {
		if (reserved_end) {
			arr[reserved_begin + _size++] = std::move(copyer);
			--reserved_end;
		}
		else {
			reserve_push_front(_size + 1);
			push_front(std::move(copyer));
		}
	}

	void pop_front() {
		if (_size)
			remove(_size - 1);
		else
			throw std::out_of_range("no items for remove");
	}
	void pop_begin() {
		if (_size)
			remove(0);
		else
			throw std::out_of_range("no items for remove");
	}

	void insert(size_t pos, const T* item, size_t arr_size) {
		if (!arr_size)
			return;
		arr.insert_block(reserved_begin + pos, item, arr_size);
		_size += arr_size;
	}
	void insert(size_t pos, const list_array<T>& item) {
		if (!item._size)
			return;
		T* as_array = item.to_array();
		arr.insert_block(reserved_begin + pos, as_array, item._size);
		_size += item._size;
		delete[] as_array;
	}
	void insert(size_t pos, const T& item) {
		if (pos == _size)
			return push_front(item);
		arr.insert(reserved_begin + pos, item);
		_size++;
	}
	void insert(size_t pos, T&& item) {
		if (pos == _size)
			return push_front(std::move(item));
		arr.insert(reserved_begin + pos, std::move(item));
		_size++;
	}

	void push_begin(T* array, size_t arr_size) {
		insert(0, array, arr_size);
	}
	void push_front(T* array, size_t arr_size) {
		insert(_size, array, arr_size);
	}
	void push_begin(const list_array<T>& to_push) {
		insert(0, to_push);
	}
	void push_front(const list_array<T>& to_push) {
		insert(_size, to_push);
	}

	//index optimization
	void commit() {
		T* tmp = new T[_size];
		begin().fast_load(tmp, _size);
		arr.safe_destruct();
		arr.arr = arr.arr_end = new arr_block<T>();
		arr.arr->arr_contain = tmp;
		arr.arr->_size = _size;
		arr._size = _size;
	}
	//insert and remove optimization
	void decommit(size_t total_blocks) {
		if (total_blocks > _size)
			throw std::out_of_range("blocks count more than elements count");
		if (total_blocks == 0)
			throw std::out_of_range("blocks count cannont be 0");
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
				}
				else {
					block_iterator = 0;
					new_total_blocks++;
					tmp.arr.arr_end = new arr_block<T>(tmp.arr.arr_end, avg_block_len, nullptr);
				}
			}
			tmp.arr.arr_end->arr_contain[block_iterator++] = (*cur_iterator);
			++cur_iterator;
		}
		arr.safe_destruct();
		arr.arr = tmp.arr.arr;
		arr.arr_end = tmp.arr.arr_end;
		tmp.arr.arr = tmp.arr.arr_end = nullptr;
		arr._size = _size;
		reserved_begin = reserved_end = 0;
	}
	bool need_commit() const {
		return arr.arr != arr.arr_end && arr.arr->next_ != arr.arr_end;
	}
	bool blocks_more(size_t blocks_count) const {
		const arr_block<T>* block = arr.arr;
		size_t res = 0;
		while (block) {
			if (++res > blocks_count)
				return true;
			block = block->next_;
		}
		return false;
	}
	size_t blocks_count() const {
		const arr_block<T>* block = arr.arr;
		size_t res = 0;
		while (block) {
			++res;
			block = block->next_;
		}
		return res;
	}
	//remove reserved memory
	void shrink() {
		resize<true>(_size);
	}

	auto contains(const T& value) -> decltype(std::declval<T>() == std::declval<T>()) const {
		for (const T& it : *this)
			if (it == value)
				return true;
		return false;
	}

	template<class _Fn>
	bool contains_one(_Fn check_functon) const {
		for (const T& it : *this)
			if (compare_functon(it))
				return true;
		return false;
	}
	template<class _Fn>
	size_t contains_multiply(_Fn check_functon) const {
		size_t i = 0;
		for (const T& it : *this)
			if (compare_functon(it))
				++i;
		return i;
	}

	template<class _Fn>
	void foreach(_Fn interate_function) {
		using res_t = decltype(std::function{ interate_function })::result_type;
		if constexpr (std::is_same<res_t, bool>::value || std::is_convertible<res_t, bool>::value) {
			for (auto& it : *this)
				if (interate_function(it))
					break;
		}
		else
			for (auto& it : *this)
				interate_function(it);
	}
	template<class _Fn>
	void foreach(_Fn interate_function) const {
		using res_t = decltype(std::function{ interate_function })::result_type;
		if constexpr (std::is_same<res_t, bool>::value || std::is_convertible<res_t, bool>::value) {
			for (auto& it : *this)
				if (interate_function(it))
					break;
		}
		else
			for (auto& it : *this)
				interate_function(it);
	}
	template<class _Fn>
	void foreach(_Fn interate_function, size_t start, size_t end) {
		using res_t = decltype(std::function{ interate_function })::result_type;
		if constexpr (std::is_same<res_t, bool>::value || std::is_convertible<res_t, bool>::value) {
			for (auto& it : range(start,end))
				if (interate_function(it))
					break;
		}
		else
			for (auto& it : range(start, end))
				interate_function(it);
	}
	template<class _Fn>
	void foreach(_Fn interate_function, size_t start, size_t end) const {
		using res_t = decltype(std::function{ interate_function })::result_type;
		if constexpr (std::is_same<res_t, bool>::value || std::is_convertible<res_t, bool>::value) {
			for (auto& it : range(start, end))
				if (interate_function(it))
					break;
		}
		else
			for (auto& it : range(start, end))
				interate_function(it);
	}

	template<class _Fn>
	void forreach(_Fn interate_function) {
		using res_t = decltype(std::function{ interate_function })::result_type;
		if constexpr (std::is_same<res_t, bool>::value || std::is_convertible<res_t, bool>::value) {
			for (auto& it : reverse())
				if (interate_function(it))
					break;
		}
		else
			for (auto& it : reverse())
				interate_function(it);
	}
	template<class _Fn>
	void forreach(_Fn interate_function) const {
		using res_t = decltype(std::function{ interate_function })::result_type;
		if constexpr (std::is_same<res_t, bool>::value || std::is_convertible<res_t, bool>::value) {
			for (auto& it : reverse())
				if (interate_function(it))
					break;
		}
		else
			for (auto& it : reverse())
				interate_function(it);
	}
	template<class _Fn>
	void forreach(_Fn interate_function, size_t start, size_t end) {
		using res_t = decltype(std::function{ interate_function })::result_type;
		if constexpr (std::is_same<res_t, bool>::value || std::is_convertible<res_t, bool>::value) {
			for (auto& it : reverse_range(start, end))
				if (interate_function(it))
					break;
		}
		else
			for (auto& it : reverse_range(start, end))
				interate_function(it);
	}
	template<class _Fn>
	void forreach(_Fn interate_function, size_t start, size_t end) const {
		using res_t = decltype(std::function{ interate_function })::result_type;
		if constexpr (std::is_same<res_t, bool>::value || std::is_convertible<res_t, bool>::value) {
			for (auto& it : reverse_range(start, end))
				if (interate_function(it))
					break;
		}
		else
			for (auto& it : reverse_range(start, end))
				interate_function(it);
	}

	template<class _Fn>
	size_t remove_if(_Fn check_function) {
		size_t res = arr.remove_if(check_function, reserved_begin, reserved_begin + _size);
		_size -= res;
		return res;
	}
	template<class _Fn>
	size_t remove_if(_Fn check_function, size_t start, size_t end) {
		if (start > end)
			std::swap(start, end);
		if (end > _size)
			throw std::out_of_range("end value(" + std::to_string(end) + ") out of size limit("+std::to_string(_size)+")");
		if (start > _size)
			throw std::out_of_range("start value(" + std::to_string(start) + ") out of size limit(" + std::to_string(_size) + ")");
		size_t res = arr.remove_if(check_function, reserved_begin + start, reserved_begin + end);
		_size -= res;
		return res;
	}

	list_array& flip_self() {
		list_array larr(_size);
		size_t i = 0;
		for (auto item : reverse())
			larr[i++] = item;
		resize(0);
		arr.arr = arr.arr_end = larr.arr.arr;
		_size = arr._size = larr._size;
		larr.arr.arr = larr.arr.arr_end = nullptr;
		larr._size = larr.arr._size = 0;
		return *this;
	}
	list_array flip_copy() const {
		list_array larr(_size);
		size_t i = 0;
		for (auto item : reverse())
			larr[i++] = item;
		return larr;
	}

	reverse_provider reverse() {
		return *this;
	}
	const_reverse_provider reverse() const {
		return *this;
	}

	range_provider range(size_t start, size_t end) {
		return range_provider(*this, start, end);
	}
	reverse_provider reverse_range(size_t start, size_t end) {
		return range_provider(*this, start, end);
	}
	const_range_provider range(size_t start, size_t end) const {
		return const_range_provider(*this, start, end);
	}
	const_reverse_provider reverse_range(size_t start, size_t end) const {
		return const_range_provider(*this, start, end);
	}

	iterator<T> get_iterator(size_t pos) {
		return arr.get_iterator(reserved_begin + pos);
	}
	iterator<T> begin() {
		return arr.get_iterator(reserved_begin);
	}
	iterator<T> end() {
		return arr.get_iterator(reserved_begin + _size);
	}
	const_iterator<T> get_iterator(size_t pos) const {
		return arr.get_iterator(reserved_begin + pos);
	}
	const_iterator<T> begin() const {
		return arr.get_iterator(reserved_begin);
	}
	const_iterator<T> end() const {
		return arr.get_iterator(reserved_begin + _size);
	}

	reverse_iterator<T> rbegin() {
		return arr.get_iterator(reserved_begin + _size);
	}
	reverse_iterator<T> rend() {
		return arr.get_iterator(reserved_begin);
	}
	const_reverse_iterator<T> rbegin() const {
		return arr.get_iterator(reserved_begin + _size);
	}
	const_reverse_iterator<T> rend() const {
		return arr.get_iterator(reserved_begin);
	}
	inline T& operator[](size_t pos) {
		return arr[reserved_begin + pos];
	}
	inline const T& operator[](size_t pos) const {
		return arr[reserved_begin + pos];
	}

	T* to_array() const {
		T* tmp = new T[_size];
		size_t i = 0;
		for (auto iterator : *this)
			tmp[i++] = iterator;
		return tmp;
	}
};
