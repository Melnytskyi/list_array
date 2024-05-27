# list_array

This is a simple, highly effective array implementation with easy-to-use functions for all popular cases, all contained in a single header file.

This implementation uses C++20.

## How it works

The `list_array` class creates array blocks and stores all blocks in a two-way list structure. This allows for faster element insertion than `std::vector`, and faster element indexing than `std::list`.

## Performance
The speed of indexing is proportional to half the number of blocks, which is why this implementation uses a two-way list.

The speed of inserting an item is equal to the size of the block into which it is being inserted. If a block contains more than 50,000 elements, it will be split.

The speed of removing an item is the same as the speed of insertion, and follows the same behavior.

To manage the number of blocks, you can use `commit()` to combine all blocks into one, or `decommit(block count)` to set your desired quantity.

## SAST Tools
[PVS-Studio](https://pvs-studio.com/pvs-studio/?utm_source=website&utm_medium=github&utm_campaign=open_source) - static analyzer for C, C++, C#, and Java code.

[![PVS-Studio](https://cdn.pvs-studio.com/static/favicon.ico)](https://pvs-studio.com/pvs-studio/?utm_source=website&utm_medium=github&utm_campaign=open_source)
