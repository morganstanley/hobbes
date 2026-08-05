// Standalone driver used when libFuzzer isn't available (or to replay crash
// reproducers with any compiler): runs each file named on the command line
// through the harness entry point once.

#include <cstdint>
#include <cstdio>
#include <vector>

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* data, size_t size);

int main(int argc, char** argv) {
  for (int i = 1; i < argc; ++i) {
    FILE* f = fopen(argv[i], "rb");
    if (f == nullptr) {
      fprintf(stderr, "can't open: %s\n", argv[i]);
      return 1;
    }
    std::vector<uint8_t> data;
    uint8_t buf[65536];
    size_t n;
    while ((n = fread(buf, 1, sizeof(buf), f)) > 0) {
      data.insert(data.end(), buf, buf + n);
    }
    fclose(f);

    printf("%s: %zu bytes\n", argv[i], data.size());
    LLVMFuzzerTestOneInput(data.data(), data.size());
  }
  printf("ok\n");
  return 0;
}
