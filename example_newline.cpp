__attribute__ ((require_newline (1))) void test_func(const char* foo);
static inline __attribute__ ((require_newline (6))) void LogPrintf_(void*, void*, void*, void*, void*, const char* fmt, void*){}
const char* goodglobalstr = "good1\n";
const char* badglobalstr = "bad1";
void test_func3(){
    const char* goodlocalstr = "good2\n";
    const char* badlocalstr = "bad2";

    test_func(goodglobalstr);
    test_func(goodlocalstr);
    test_func("good\n");

    test_func(badglobalstr);
    test_func(badlocalstr);
    test_func("bad");

    LogPrintf_(nullptr, nullptr, nullptr, nullptr, nullptr, "bad", nullptr);
    LogPrintf_(nullptr, nullptr, nullptr, nullptr, nullptr, "good\n", nullptr);
}
