# list_array

The simple one header implementation heigh effectivelly array and easy usage functions for all popoular cases in one header file

this implementation use c++20 but has compatibility with c++17

# How it work

list_array class create array blocks and store all blocks in two way list structure which allow push elements anywhere faster than std::vector, and indexing elements faster than std::list

# Performace
indexing speed is half blocks count cause this implementation use two way list

inserting item speed is equal as block size which is inserting<br/>
if modifying block contains more than 50000 elements it will splited

removing speed is same speed as inserting and same behavior

for manage blocks count you can use <code>commit()</code> for combine all blocks to one or <code>decommit(block count)</code> for your desired quantity
