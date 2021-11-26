#include "list_array.hpp"
#include <iostream>
#include <chrono>
#include <assert.h>
void speed_test() {
	for (size_t i = 0; i < 1; i++) {
		std::chrono::steady_clock::time_point curr;

		list_array<uint32_t> test;
		std::cout << "Push only time: ";
		curr = std::chrono::high_resolution_clock::now();
		for (uint32_t i = 0; i < 1000000000; i++)test.push_front(i);
		std::cout << std::chrono::high_resolution_clock::now() - curr << std::endl;



		std::cout << "Commit to one block time: ";
		curr = std::chrono::high_resolution_clock::now();
		test.commit();
		std::cout << std::chrono::high_resolution_clock::now() - curr << std::endl;

		std::cout << "Default index 1000000000 elems time: ";
		curr = std::chrono::high_resolution_clock::now();
		for (uint32_t i = 0; i < 1000000000; i++) {
			if (test[i] != i)
				std::cout << "Err! [" << i << "] " << test[i] << std::endl;
		}
		std::cout << std::chrono::high_resolution_clock::now() - curr << std::endl;


		std::cout << "Foreach 1000000000 elems time: ";
		curr = std::chrono::high_resolution_clock::now();
		uint64_t a = 0;
		test.foreach([&a](uint32_t& it) { a += it; });
		std::cout << std::chrono::high_resolution_clock::now() - curr;

		std::cout << ", elems sum: " << a << std::endl;

		std::cout << "Remove if (half array): ";
		curr = std::chrono::high_resolution_clock::now();
		test.remove_if([](auto& it) { return (bool)(it & 1); });
		std::cout << std::chrono::high_resolution_clock::now() - curr << std::endl;


		std::cout << "After remove if foreach: ";
		curr = std::chrono::high_resolution_clock::now();
		a = 0;
		test.foreach([&a](uint32_t& it) { a += it; });
		std::cout << std::chrono::high_resolution_clock::now() - curr;
		std::cout << ", elems sum: " << a << std::endl;
	}
}
inline void index() {
	list_array<int> test(60);
	for (size_t i = 0; i < 60; i++)
		test[i] = i;
	for (size_t i = 0; i < 60; i++)
		assert(test[i] == i && "Index test failed [missmatch]");
}
inline void insert_ourselt_begin() {
	int test_arr[] = { 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59 };
	list_array<int> test(60);
	for (size_t i = 0; i < 60; i++)
		test[i] = i;
	test.insert(0, test);
	assert(test.size() == 120 && "Insert test 0 failed [size missmatch]");
	for (size_t i = 0; i < 120; i++)
		assert(test[i] == test_arr[i] && "Insert test 0 failed [put missmatch]");
	assert(test.blocks_count() == 2 && "Insert test 0 failed [blocks missmatch]");
	assert(!test.need_commit() && "Insert test 0 failed [need commit equlation missmatch]");
}
inline void insert_ourselt_end() {
	int test_arr[] = { 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59 };
	list_array<int> test(60);
	for (size_t i = 0; i < 60; i++)
		test[i] = i;
	test.insert(60, test);
	assert(test.size() == 120 && "Insert test 1 failed [size missmatch]");
	for (size_t i = 0; i < 120; i++)
		assert(test[i] == test_arr[i] && "Insert test 1 failed [put missmatch]");
	assert(test.blocks_count() == 2 && "Insert test 1 failed [blocks missmatch]");
	assert(!test.need_commit() && "Insert test 1 failed [need commit equlation missmatch]");
}
inline void insert_ourselt_mindle() {
	int test_arr[] = { 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59 };
	list_array<int> test(60);
	for (size_t i = 0; i < 60; i++)
		test[i] = i;
	test.insert(30, test);
	assert(test.size() == 120 && "Insert test 2 failed [size missmatch]");
	for (size_t i = 0; i < 120; i++)
		assert(test[i] == test_arr[i] && "Insert test 2 failed [put missmatch]");
	assert(test.blocks_count() == 3 && "Insert test 2 failed [blocks missmatch]");
	assert(test.need_commit() && "Insert test 2 failed [need commit equlation missmatch]");
}

void main() {
	index();
	insert_ourselt_begin();
	insert_ourselt_end();
	insert_ourselt_mindle();
	std::cout << "list array tests: passed" << std::endl;
	speed_test();
}