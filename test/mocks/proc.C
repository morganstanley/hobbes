#include <csignal>
#include <cstdlib>
#include <iostream>
#include <unistd.h>

namespace {
struct Options {
  int timeout = -1;
  bool valid = false;
  bool writeStderr = false;
};

Options getOptions(int argc, char **argv) {
  if (argc != 2 && argc != 3) {
    return {};
  }
  std::size_t pos = 0;
  const auto g = std::string(argv[1]);
  const auto t = std::stoi(g, &pos);
  if (pos != g.size() || t <= 0) {
    return {};
  }
  const bool writeStderr = argc == 3 && std::string(argv[2]) == "stderr";
  if (argc == 3 && !writeStderr) {
    return {};
  }
  return {.timeout = t, .valid = true, .writeStderr = writeStderr};
}

extern "C" [[noreturn]] void exitApp(int /* sig */) {
  std::_Exit(EXIT_SUCCESS);
}
} // namespace

// argv[1] is the seconds before process exists
int main(int argc, char **argv) {
  const auto o = getOptions(argc, argv);
  if (!o.valid) {
    return EXIT_FAILURE;
  }

  std::signal(SIGTERM, exitApp);

  if (o.writeStderr) {
    static const char diagnostic[] = "mock child diagnostic\n";
    if (write(STDERR_FILENO, diagnostic, sizeof(diagnostic) - 1) == -1) {
      return EXIT_FAILURE;
    }
  }

  for (int i = 0; i < o.timeout; ++i) {
    sleep(1);
  }

  const int success = 1;
  const auto rc = write(STDOUT_FILENO, &success, sizeof(success));
  if (rc == -1) {
    return EXIT_FAILURE;
  }
}
