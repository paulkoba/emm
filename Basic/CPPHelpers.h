//
// Created by fail on 7/25/22.
//

#ifndef EMMC_CPPHELPERS_H
#define EMMC_CPPHELPERS_H

#include <algorithm>
#include <array>
#include <cmath>
#include <cstdint>
#include <functional>
#include <iostream>
#include <iterator>
#include <limits>
#include <map>
#include <numeric>
#include <queue>
#include <regex>
#include <set>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <execinfo.h>
#include <cstdio>
#include <cstdlib>
#include <csignal>

template <typename T, typename S>
struct std::hash<std::pair<T, S>> {
	std::size_t operator()(const std::pair<T, S>& elem) const noexcept {
		return std::hash<T>{}(elem.first) ^ std::hash<S>{}(elem.second);
	}
};

template <typename T>
struct std::hash<std::vector<T>> {
	std::size_t operator()(const std::vector<T>& elem) const noexcept {
		std::size_t h = 0;
		for (const T& e : elem) h ^= std::hash<T>{}(e);
		return h;
	}
};

template <typename T, typename S>
[[maybe_unused]] std::ostream& operator<<(std::ostream& os, const std::pair<T, S>& f) {
	os << "{" << f.first << ", " << f.second << "}";
	return os;
}

template <typename T, typename S>
[[maybe_unused]] std::ostream& operator<<(std::ostream& os, const std::unordered_map<T, S>& f) {
	for (const auto& i : f) {
		os << "{" << i.first << ": " << i.second << "}\n";
	}

	return os;
}

template <typename T>
[[maybe_unused]] std::ostream& operator<<(std::ostream& os, const std::vector<T>& f) {
	if (f.empty()) {
		os << "{}";
		return os;
	}

	os << "{\n " << f[0];
	for (std::size_t i = 1; i < f.size(); ++i) os << ",\n " << f[i];
	os << "\n}\n";

	return os;
}

template <typename T>
[[maybe_unused]] std::ostream& operator<<(std::ostream& os, const std::priority_queue<T>& f) {
	std::priority_queue<T> cp = f;
	if (f.empty()) return os;
	os << "{" << cp.top();
	cp.pop();
	while (!cp.empty()) {
		os << ", " << cp.top();
		cp.pop();
	}
	os << "}";
	return os;
}

template <typename T, typename S>
[[maybe_unused]] std::istream& operator>>(std::istream& is, std::pair<T, S>& f) {
	is >> f.first >> f.second;
	return is;
}

std::vector<std::string> split(const std::string& s) {
	std::stringstream ss(s);
	std::istream_iterator<std::string> begin(ss);
	std::istream_iterator<std::string> end;

	return {begin, end};
}

void print_trace () {
    void *array[10];
    char **strings;
    int size, i;

    size = backtrace (array, 10);
    strings = backtrace_symbols (array, size);
    if (strings != nullptr) {
        printf ("Obtained %d stack frames.\n", size);
        for (i = 0; i < size; i++) printf ("%s\n", strings[i]);
    }

    free (strings);
}

#endif	// EMMC_CPPHELPERS_H
