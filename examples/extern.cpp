#include <cstdint>
#include <cstdio>

extern "C" int64_t print_i64(int64_t i) {
	printf("%ld\n", i);
	return i;
}

extern "C" uint64_t print_u64(uint64_t i) {
	printf("%lu\n", i);
	return i;
}

extern "C" double print_f64(double i) {
	printf("%f\n", i);
	return i;
}

extern "C" int8_t print_char(int8_t i) {
	printf("%c", (char)i);
	return i;
}

extern "C" int64_t* print_ptr(int64_t* i) {
    printf("%p\n", i);
    return i;
}

extern "C" char* print_str(char* i) {
    printf("%s\n", i);
    return i;
}