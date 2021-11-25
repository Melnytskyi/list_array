# list_array

The simple one header implementation heigh effectivelly array for universal cases in one header file

# How it work

list_array class create array blocks and store in list structure which allow push elements at begin same speed as last block array O(n) in bad case, and index lower speed than default list with same elements count

removing and inserting items in random place split array block and if array block is small just modify it without spliting 

# Usage
For bether performace i recomend do not combine pushing and indexing operations in same place
and call <code>commit()</code> for combine all blocks in one and <code>decomit(size_t blocks_count)</code> for optimize inserting and pushing (not recomend set heigh value)

by default performace this two operations balancing in O(1) and O(N) range