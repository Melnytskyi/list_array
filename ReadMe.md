[![Language](https://img.shields.io/badge/C%2B%2B-20-blue.svg)](https://isocpp.org/)
[![License](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

# list_array: A Hybrid C++20 Array-List Container

`list_array` is a high-performance, single-header C++20 data structure that provides a novel alternative to `std::vector` and `std::list`. It blends the strengths of both, offering fast, random-like access without the expensive reallocation penalties of `std::vector` on insertion or deletion.

## Table of Contents

-   [The Problem with Standard Containers](#the-problem-with-standard-containers)
-   [How it works](#how-it-works)
-   [Performance Profile](#performance-profile)
-   [API and Feature Showcase](#api-and-feature-showcase)
    -   [Iteration](#iteration)
    -   [Block Management](#block-management)
    -   [Algorithms](#algorithms)
    -   [Filtering](#filtering)
    -   [Mutation](#mutation)
-   [Installation](#installation)
-   [Usage Example](#usage-example)
-   [Debugging with Visual Studio](#debugging-with-visual-studio)
-   [Building the Tests](#building-the-tests)
-   [Acknowledgements](#acknowledgements)

## The Problem with Standard Containers

-   `std::vector`: Offers excellent cache-friendliness and $O(1)$ random access. However, inserting or erasing elements in the middle is an $O(N)$ operation, and growing the vector can trigger a costly reallocation and copy of all elements.
-   `std::list`: Provides $O(1)$ insertion and erasure anywhere in the container (with a valid iterator), but lacks random access. Finding the $N^{th}$ element is an $O(N)$ operation.

`list_array` is designed for the middle ground, where both access patterns are common and performance is critical.

## How it works

`list_array` is implemented as a doubly-linked list where each node contains a dynamically allocated array (a "block") of elements.

```

[ block 1 ]  \<---\>  [ block 2 ]  \<---\>  [ block 3 ]
|                     |                     |
[ T, T, ..., T ]      [ T, T, ..., T ]      [ T, T, ..., T ]

```

This design provides the following benefits:
-   **Fast Insertions:** An insertion only requires shifting elements within one small block, not the entire container.
-   **No Reallocations:** The container grows by adding new blocks, not by reallocating a single monolithic memory chunk.
-   **Efficient Indexing:** To access `arr[i]`, the library traverses the list of blocks (a much smaller number than the total element count) to find the target block, then performs an $O(1)$ lookup inside it. The list is traversed from whichever end is closer to the target index.

## Performance Profile

| Operation             | `std::vector`      | `std::list`               | `list_array`                       | Notes                                                |
| --------------------- | ------------------ | ------------------------- | ---------------------------------- | ---------------------------------------------------- |
| **Indexing `[i]`**    | $O(1)$             | $O(N)$                    | $O(B)$                             | $B$ = number of blocks. `commit()` makes this $O(1)$.|
| **`push_back`**       | $O(1)$ (amortized) | $O(1)$                    | $O(1)$                             |                                                      |
| **`insert(middle)`**  | $O(N)$             | $O(1)$ (with iterator)    | $O(B + N_{\text{block}})$          | Find block, then shift elements in-block.            |
| **`erase(middle)`**   | $O(N)$             | $O(1)$ (with iterator)    | $O(B + N_{\text{block}})$          | Find block, then shift elements in-block.            |

## API and Feature Showcase

The library provides a rich, modern, and intuitive API.

### Iteration
The container multiple ways of iterating:
-   **Forward:** `for (auto& item : arr)`
-   **Reverse:** `for (auto& item : arr.reverse())`
-   **Ranged:** `for (auto& item : arr.range(start, end))` and `arr.reverse_range(start, end)`
-   **Lambda:** `arr.for_each([](auto& item) { /* ... */ })`
-   **Lambda Reverse:** `arr.for_each_reverse([](auto& item) { /* ... */ })`
-   **Lambda Ranged:** `arr.for_each(start, end, [](auto& item) { /* ... */ })` and `arr.reverse_for_each(start, end, [](auto& item) { /* ... */ })`

_The lambda function supports unpacking tuples and pairs to arguments._


### Block Management
These functions give you direct control over the internal layout for performance tuning.
-   `commit()`: Merges all data into a single, contiguous block. Use this when you are finished with major modifications and want the fastest possible indexing ($O(1)$).
-   `decommit(count)`: Re-distributes all elements evenly across a specified number of new blocks. Use this to prepare the container for a new wave of insertions.
-   `split(pos)`: Splits the container into two at the given position, returning the second part as a new `list_array`.

### Algorithms
Several useful algorithms are provided as member functions.
-   `sort()`: Sorts the elements in place.
-   `unique()`: Removes adjacent duplicate elements.
-   `unify()`: Removes duplicate elements.
-   `alone()`: Removes all elements that have duplicates, leaving only unique items.
-   `flip_copy()`: Returns a new `list_array` with the elements in reverse order.
-   `sum()`: Returns the sum of all elements in the container.(For numeric only)
-   `avg()`: Returns the average of all elements in the container.(For numeric only)
-   `max()`: Returns the max of all elements in the container.(For ordered types only)
-   `min()`: Returns the min of all elements in the container.(For ordered types only)
-   `max_default()`: Returns the max of all elements in the container, or a default value if empty.
-   `min_default()`: Returns the min of all elements in the container, or a default value if empty.
-   `count(function)`: Returns the sum returned by function for each element.

### Filtering
The library provides several convenient filtering functions.
-   `where(predicate)`: Returns a new `list_array` containing only the elements that satisfy the given predicate.
-   `remove_if(predicate)`: Removes all elements that satisfy the given predicate from the container.

### Mutation
-   `insert(index, value)`: Inserts a new element at the specified index.
-   `insert(index, values)`: Inserts multiple elements at the specified index.
-   `insert(index, list_array)`: Inserts another `list_array` at the specified index.
-   `ordered_insert(value)`: Inserts a new element in sorted order.(works if the container is sorted)
-   `erase(index)`: Removes the element at the specified index.
-   `erase(start, end)`: Removes all elements in the specified range.
-   `clear()`: Removes all elements from the container.
-   `push_back(value)`: Adds a new element to the end of the container.
-   `pop_back()`: Removes the last element from the container.
-   `push_front(value)`: Adds a new element to the front of the container.
-   `pop_front()`: Removes the first element from the container.
-   `take_back()`: Returns the last element and removes it from the container.
-   `take_front()`: Returns the first element and removes it from the container.
-   `take(index)`: Returns the element from index and removes from the container.
-   `take(start, end)`: Returns `list_array` with the range and removes them from the container.
-   `take(predicate)`: Returns `list_array` with all elements that satisfy the predicate and removes from the container.
-   `swap(other)`: Swaps the contents of this container with another `list_array`.
-   `transform(function)`: Applies a function to each element, modifying them in place.
-   `transform_with(list_array, function)`: Applies a function to each element, using corresponding elements from another `list_array`.
-   `replace(old, new)`: Replaces all occurrences of `old` value or sequence with `new` value or sequence.
-   `replace_if(predicate, new_value)`: Replaces all elements that satisfy the predicate with `new_value`.
-   `concat(list_array)`: Concat the arrays in the container and return them.

**and much more**


## Installation

`list_array` is a **single-header library**.

Just include `list_array.hpp` or `list_array_no_doc.hpp` into your project.

## Usage Example

```cpp
#include "list_array.hpp"
#include <iostream>
#include <vector>

int main() {
    list_array<int> fib {1, 1, 2, 3, 3, 3, 5, 5, 8, 13, 21};

    // Insert another list_array into the middle
    list_array<int> to_insert = {100, 200};
    fib.insert(3, to_insert); // fib is now {1, 1, 2, 100, 200, 3, 3, 3, 5, 5, 8, 13, 21}
    
    std::cout << "After insertion: ";
    for(int val : fib) std::cout << val << " ";
    std::cout << std::endl;

    // Use unify() to remove all duplicates
    fib.unify();
    
    std::cout << "After unify(): ";
    for(int val : fib) std::cout << val << " ";
    std::cout << std::endl; // Output: 1 2 100 200 3 5 8 13 21 

    return 0;
}
```

## Debugging with Visual Studio

This repository includes a `list_array.natvis` file. It provides a clean, human-readable display of the `list_array`'s contents in the debugger, showing its size and elements just like a standard container.

## Acknowledgements

[PVS-Studio](https://pvs-studio.com/pvs-studio/?utm_source=website&utm_medium=github&utm_campaign=open_source) - static analyzer for C, C++, C#, and Java code.
