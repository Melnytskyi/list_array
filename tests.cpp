#include "list_array.hpp"
#include <assert.h>
#include <chrono>
#include <iostream>
#include <vector>

inline auto milisec_now_time() {
    return std::chrono::time_point_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now());
}

std::ostream& operator<<(std::ostream& os, std::chrono::time_point<std::chrono::steady_clock, std::chrono::milliseconds>::duration time) {
    return os << time.count() << "ms";
}

void speed_test() {
    for (size_t i = 0; i < 1; i++) {
        std::chrono::time_point<std::chrono::steady_clock, std::chrono::milliseconds> curr;

        list_array<uint32_t> test;
        uint64_t a = 0;

        std::cout << "Push only time: ";
        curr = milisec_now_time();
        for (uint32_t i = 0; i < 1000000000; i++)
            test.push_back(i);
        std::cout << milisec_now_time() - curr << std::endl;

        std::cout << "Commit to one block time: ";
        curr = milisec_now_time();
        test.commit();
        std::cout << milisec_now_time() - curr << std::endl;

        std::cout << "Default index 1000000000 elems time: ";
        curr = milisec_now_time();
        for (uint32_t i = 0; i < 1000000000; i++)
            a += test[i];
        std::cout << milisec_now_time() - curr << ", elems sum: " << a << std::endl;
        std::cout << "For 1000000000 elems time: ";
        curr = milisec_now_time();
        a = 0;
        for (uint32_t& it : test)
            a += it;
        std::cout << milisec_now_time() - curr << ", elems sum: " << a << std::endl;
        std::cout << "For reverse 1000000000 elems time: ";
        curr = milisec_now_time();
        a = 0;
        for (uint32_t& it : test.reverse())
            a += it;
        std::cout << milisec_now_time() - curr << ", elems sum: " << a << std::endl;

        std::cout << "Remove speed(100 items at end): ";
        curr = milisec_now_time();
        test.remove(test.size() - 100, test.size());
        std::cout << milisec_now_time() - curr << std::endl;

        std::cout << "Remove if (half array): \n\tpre remove sum: " << test.size() << "\n\t remove time:   ";
        curr = milisec_now_time();
        test.remove_if([](auto& it) { return (bool)(it & 1); });
        std::cout << milisec_now_time() - curr << "\n\tafter remove sum: " << test.size() << std::endl
                  << std::endl;

        std::cout << "After remove if foreach: ";
        curr = milisec_now_time();
        a = 0;
        for (uint32_t& it : test)
            a += it;
        std::cout << milisec_now_time() - curr << ", elems sum: " << a << std::endl;
    }
}

void index() {
    list_array<int> test(60);
    for (int i = 0; i < 60; i++)
        test[i] = i;
    for (size_t i = 0; i < 60; i++)
        assert(test[i] == i && "Index test failed [missmatch]");
    std::cout << "Index test: passed" << std::endl;
}

void insert_ourselt_begin() {
    int test_arr[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59};
    list_array<int> test(60);
    for (int i = 0; i < 60; i++)
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
    int test_arr[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59};
    list_array<int> test(60);
    for (int i = 0; i < 60; i++)
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
    int test_arr[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59};
    list_array<int> test(60);
    for (int i = 0; i < 60; i++)
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
    for (uint32_t i = 0; i < 1000; i++)
        test.push_back(i);
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
    for (uint32_t i = 0; i < 1000; i++)
        test.push_back(i);
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
    for (uint32_t i = 0; i < 1000; i++)
        test.push_back(i);
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
    for (uint32_t i = 0; i < 1000; i++)
        test.push_back(i);
    test.remove(400, 600);
    for (uint32_t i = 0; i < 800; i++) {
        if (test[i] != i + (i >= 400 ? 200 : 0))
            assert(false && "Err! test[i] != i");
    }
    assert(test.size() == 800 && "Remove items test: failed [size missmatch]");
    std::cout << "Remove items test: passed" << std::endl;
}

void foreach_test() {
    list_array<uint32_t> test;
    for (uint32_t i = 0; i < 1000; i++)
        test.push_back(i);
    uint64_t res_1 = 0;
    uint64_t res_2 = 0;
    uint64_t res_3 = 0;

    for (uint32_t& it : test)
        res_1 += it;

    uint64_t tt = 1000;
    for (uint32_t& it : test.reverse()) {
        assert(it == --tt);
        res_2 += it;
    }

    for (uint32_t i = 0; i < 1000; i++)
        res_3 += test[i];

    assert(res_1 == res_2 && res_2 == res_3 && "For test failed [result missmatch]");
    std::cout << "For test: passed" << std::endl;
}

void reverse_foreach_test() {
    list_array<uint32_t> test;
    for (uint32_t i = 0; i < 1000; i++)
        test.push_back(i);
    uint32_t for_test = 900;
    for (uint32_t& it : test.reverse_range(0, 900)) {
        if (it != --for_test)
            assert(false && "Err! test[i] != i");
    }
    assert(for_test == 0 && "Reverse for test failed [result missmatch]");
    std::cout << "Reverse for test: passed" << std::endl;
}

void flip() {
    list_array<uint32_t> test;
    for (uint32_t i = 0; i < 1000; i++)
        test.push_back(i);
    auto copy = test.flip_copy();
    uint32_t copy_test = 1000;
    for (uint32_t it : copy) {
        assert(it == --copy_test && "Flip test failed cause missmatch");
    }
    std::cout << "Flip test: passed" << std::endl;
}

constexpr bool constexpr_flip() {
    list_array<uint32_t> test;
    for (uint32_t i = 0; i < 1000; i++)
        test.push_back(i);
    auto copy = test.flip_copy();
    uint32_t copy_test = 1000;
    bool result = true;
    for (uint32_t it : test) {
        if (it != --copy_test) {
            result = false;
            return true;
        }
        return false;
    }
    return result;
}

void forreach() {
    list_array<uint32_t> test;
    for (uint32_t i = 0; i < 1000; i++)
        test.push_back(i);
    auto copy = test.flip_copy();
    size_t i = 0;
    for (uint32_t it : test.reverse())
        assert(it == copy[i++] && "For reverse each test failed cause missmatch");
    std::cout << "For reverse each test: passed" << std::endl;
}

void forreach_range() {
	list_array<uint32_t> test;
	for (uint32_t i = 0; i < 1000; i++)test.push_back(i);
	auto copy = test.flip_copy();
	size_t i = 300;

	for(uint32_t it : test.reverse_range(100,300)) 
		assert(it == test[--i] && "For revese each range test failed cause missmatch");

	std::cout << "For reverse each range test: passed" << std::endl;
}


void sort_test() {
	list_array<uint32_t> test(10000);
	for (uint32_t& it : test)
		it = rand();
	test.sort();

	size_t i = 0;
	uint32_t cmp = 0;
	for(uint32_t it : test) {
		if(cmp >it)
			assert(false && "Sort test failed cause missmatch");
		else 
			cmp = it;
		++i;
	}
 	std::cout << "Sort test: passed" << std::endl;
}

void split() {
	list_array<uint32_t> test;
	for (uint32_t i = 0; i < 1000; i++)test.push_back(i);
	auto copy = test.split(500);
	size_t i = 0;
	for(auto it:test)
		assert(it == i++ && "Split test failed cause missmatch");

	std::cout << "Split test: passed" << std::endl;
}

void unique() {
	list_array<int> test = { 1, 2, 2, 3, 3, 2, 1, 1, 2 };
	test.unique();
	list_array<int> check{ 1, 2, 3, 2, 1, 2 };
	assert(test == check);
	std::cout << "Unique test: passed" << std::endl;
}
void unify() {
	list_array<int> test = { 1, 2, 2, 3, 3, 2, 1, 1, 2 };
	test.unify();
	list_array<int> check{ 1, 2, 3 };
	assert(test == check);
	std::cout << "Unify test: passed" << std::endl;
}
void alone() {
	list_array<int> test = { 1, 2, 2, 3, 3, 2, 1, 1, 2,5 };
	test.alone();
	list_array<int> check{ 5 };
	assert(test == check);
	std::cout << "Alone test: passed" << std::endl;
}
void ini_list0() {
	list_array<std::string> test {"Hello word","Hello world" ,"ld" ,"Hrld" ,"Hld" ,"ld" ,"He" };
	test = { test, test, };
	test = { { test, test }, { test, test}, test };
	test.unify();
}
void ini_list1() {
	list_array<list_array<std::string>> test{ {"Hello word","Hello world"}, {"ld" ,"Hrld" ,"Hld" ,"ld" ,"He"} };
	test = { test, test, };
	test = { { test, test }, { test, test}, test };
	test.unify();
}

void convert_test() {
	std::vector<std::string> test{ "Hello word","Hello world" ,"ld" ,"Hrld" ,"Hld" ,"ld" ,"He" };
	list_array<std::string> check{ "Hello word","Hello world" ,"ld" ,"Hrld" ,"Hld" ,"ld" ,"He" };
	list_array<std::string> converted(test.begin(),test.end());
	assert(check == converted);
	std::cout << "Convert test: passed" << std::endl;
}

int main() {
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
	sort_test();
	split();

	unique();
	unify();
	alone();

	ini_list0();
	ini_list1();

	convert_test();

	std::cout << "list array tests: passed" << std::endl;
	speed_test();
	return 0;
}