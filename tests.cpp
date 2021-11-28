#include <iostream>
#include <chrono>
#include <assert.h>
#include "list_array.hpp"

void speed_test() {
	for (size_t i = 0; i < 1; i++) {
		std::chrono::steady_clock::time_point curr;

		list_array<uint32_t> test;
		uint64_t a = 0;

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
		for (uint32_t i = 0; i < 1000000000; i++) 
			a += test[i];
		std::cout << std::chrono::high_resolution_clock::now() - curr << ", elems sum: " << a << std::endl;

		std::cout << "Remove speed: ";
		test.remove(test.size() - 100, test.size());
		std::cout << "Foreach 1000000000 elems time: ";
		curr = std::chrono::high_resolution_clock::now();
		a = 0;
		test.foreach([&a](uint32_t& it) { a += it; });
		std::cout << std::chrono::high_resolution_clock::now() - curr << ", elems sum: " << a << std::endl;

		std::cout << "Remove if (half array): " << test.size() << "   ";

		curr = std::chrono::high_resolution_clock::now();
		test.remove_if([](auto& it) { return (bool)(it & 1); });
		std::cout << std::chrono::high_resolution_clock::now() - curr << test.size() << std::endl << std::endl;

		std::cout << "After remove if foreach: ";
		curr = std::chrono::high_resolution_clock::now();
		a = 0;
		test.foreach([&a](uint32_t& it) { a += it; });
		std::cout << std::chrono::high_resolution_clock::now() - curr << ", elems sum: " << a << std::endl;
	}
}


void index() {
	list_array<int> test(60);
	for (size_t i = 0; i < 60; i++)
		test[i] = i;
	for (size_t i = 0; i < 60; i++)
		assert(test[i] == i && "Index test failed [missmatch]");
	std::cout << "Index test: passed" << std::endl;
}

void insert_ourselt_begin() {
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
	std::cout << "Insert ourself begin: passed" << std::endl;
}
void insert_ourselt_end() {
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
	std::cout << "Insert ourself end: passed" << std::endl;
}
void insert_ourselt_mindle() {
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
	std::cout << "Insert ourself mindle: passed" << std::endl;
}

void commit() {
	list_array<uint32_t> test;
	for (uint32_t i = 0; i < 1000; i++)test.push_front(i);
	test.commit();
	for (uint32_t i = 0; i < 1000; i++) {
		if (test[i] != i)
			assert(false && "Err! test[i] != i");
	}
	assert(test.size() == 1000 && "Commit failed [size missmatch]");
	assert(test.blocks_count() == 1 && "Commit failed failed [blocks missmatch]");
	std::cout << "Commit test: passed" << std::endl;
}
void decommit() {
	list_array<uint32_t> test;
	for (uint32_t i = 0; i < 1000; i++)test.push_front(i);
	test.decommit(4);
	for (uint32_t i = 0; i < 1000; i++) {
		if (test[i] != i)
			assert(false && "Err! test[i] != i");
	}


	assert(test.size() == 1000 && "Decommit failed [size missmatch]");
	assert(test.blocks_count() == 4 && "Decommit failed failed [blocks missmatch]");
	std::cout << "Decommit test: passed" << std::endl;
}

void remove_item() {
	list_array<uint32_t> test;
	for (uint32_t i = 0; i < 1000; i++)test.push_front(i);
	test.remove(500);
	for (uint32_t i = 0; i < 999; i++) {
		if (test[i] != i + (i >= 500))
			assert(false && "Err! test[i] != i");
	}
	assert(test.size() == 999 && "Remove item test: failed [size missmatch]");
	std::cout << "Remove item test: passed" << std::endl;
}
void remove_items() {
	list_array<uint32_t> test;
	for (uint32_t i = 0; i < 1000; i++)test.push_front(i);
	test.remove(400,600);
	for (uint32_t i = 0; i < 800; i++) {
		if (test[i] != i + (i >= 400 ? 200 : 0))
			assert(false && "Err! test[i] != i");
	}
	assert(test.size() == 800 && "Remove items test: failed [size missmatch]");
	std::cout << "Remove items test: passed" << std::endl;
}

void foreach_test() {
	list_array<uint32_t> test;
	for (uint32_t i = 0; i < 1000; i++)test.push_front(i);
	uint64_t res_0 = 0;
	uint64_t res_1 = 0;
	uint64_t res_2 = 0;
	uint64_t res_3 = 0;
	test.foreach([&res_0](uint32_t& it) { res_0 += it; });
	test.forreach([&res_1](uint32_t& it) { res_1 += it; });

	for (uint32_t& it : test)
		res_2 += it;
	
	for (uint32_t i = 0; i < 1000; i++)
		res_3 += test[i];

	assert(res_0 == res_1 && res_1 == res_2 && res_2 == res_3 && "For test failed [result missmatch]");
	std::cout << "For test: passed" << std::endl;
}
void reverse_foreach_test() {
	list_array<uint32_t> test;
	for (uint32_t i = 0; i < 1000; i++)test.push_front(i);
	uint32_t for_test = 900;
	for (uint32_t& it : test.reverse_range(0,900)) {
		if (it != --for_test)
			assert(false && "Err! test[i] != i");
	}
	assert(for_test == 0 && "Reverse for test failed [result missmatch]");
	std::cout << "Reverse for test: passed" << std::endl;
}

void flip() {
	list_array<uint32_t> test;
	for (uint32_t i = 0; i < 1000; i++)test.push_front(i);
	auto copy = test.flip_copy();
	uint32_t copy_test = 1000;
	copy.foreach([&copy_test](uint32_t it) { 
		assert(it == --copy_test && "Flip test failed cause missmatch");
	});
	std::cout << "Flip test: passed" << std::endl;
}
constexpr bool constexpr_flip() {
	list_array<uint32_t> test;
	for (uint32_t i = 0; i < 1000; i++)test.push_front(i);
	auto copy = test.flip_copy();
	uint32_t copy_test = 1000;
	bool result = true;
	copy.foreach([&copy_test, &result](uint32_t it) {
		if (it != --copy_test)
		{
			result = false;
			return true;
		}
		return false;
		});
	return result;
}

void forreach() {
	list_array<uint32_t> test;
	for (uint32_t i = 0; i < 1000; i++)test.push_front(i);
	auto copy = test.flip_copy();
	size_t i = 0;

	copy.forreach([&test,&i](uint32_t it) {
		assert(it != test[i++]  && "For revese each test failed cause missmatch");
	});
	std::cout << "For reverse each test: passed" << std::endl;
}

void forreach_range() {
	list_array<uint32_t> test;
	for (uint32_t i = 0; i < 1000; i++)test.push_front(i);
	auto copy = test.flip_copy();
	size_t i = 100;

	copy.forreach(
		[&test, &i](uint32_t it) {
			assert(it != test[i++] && "For revese each range test failed cause missmatch");
		},
		100,
		300
	);
	std::cout << "For reverse each range test: passed" << std::endl;
}


void main() {
	index();

	insert_ourselt_begin();
	insert_ourselt_end();
	insert_ourselt_mindle();

	commit();
	decommit();
	remove_item();
	remove_items();

	foreach_test();
	reverse_foreach_test();

	flip();
	std::cout << "constexpr flip test result:" << constexpr_flip() << std::endl;
	forreach();
	forreach_range();



	std::cout << "list array tests: passed" << std::endl;
	speed_test();
}