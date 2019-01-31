#ifndef HOG_STAT_H_INCLUDED
#define HOG_STAT_H_INCLUDED

#include <mutex>
#include <vector>
#include <functional>
#include <string>

#include <hobbes/hobbes.H>
#include <hobbes/reflect.H>
#include <hobbes/storage.H>
#include <hobbes/fregion.H>
#include <hobbes/util/time.H>
#include <hobbes/util/hash.H>

namespace hog {

DEFINE_ENUM(SessionType,
  (Normal),
  (Recovery)
);

DEFINE_STRUCT(ProcessEnvironment,
  (hobbes::datetimeT,           startTime),
  (std::string,                 sessionHash),
  (std::string,                 runMode),
  (std::vector<std::string>,    argv),
  (SessionType,                 sessionType)
);

DEFINE_ENUM(ReaderStatus,
  (Started),
  (Closed)
);

DEFINE_STRUCT(ReaderState,
  (hobbes::datetimeT,           datetime),
  (std::string,                 sessionHash),
  (hobbes::storage::ProcThread, id),
  (ReaderStatus,                status)
);

DEFINE_STRUCT(ReaderRegistration,
  (hobbes::datetimeT,           datetime),
  (std::string,                 sessionHash),
  (hobbes::storage::ProcThread, writerId),
  (hobbes::storage::ProcThread, readerId),
  (std::string,                 shmname),
  (std::string,                 groupName)
);

DEFINE_ENUM(SenderStatus,
  (Suspended),
  (Started),
  (Closed)
);

DEFINE_STRUCT(SenderState,
  (hobbes::datetimeT,           datetime),
  (std::string,                 sessionHash),
  (hobbes::storage::ProcThread, id),
  (SenderStatus,                status)
);

DEFINE_STRUCT(SenderRegistration,
  (hobbes::datetimeT,           datetime),
  (std::string,                 sessionHash),
  (hobbes::storage::ProcThread, readerId),
  (hobbes::storage::ProcThread, senderId),
  (std::string,                 directory),
  (std::vector<std::string>,    senderqueue)
);

class StatFile {
public:
  static StatFile& instance();

  template <typename T>
  void log(T&& value) {
    std::lock_guard<decltype(mutex)> _{mutex};
    statFile.series<T>(T::_hmeta_struct_type_name())(std::forward<T>(value));
  }

  template <typename T>
  void ensureAvailable() {
    (void)(statFile.series<T>(T::_hmeta_struct_type_name()));
  }

  const std::string& filename() const {
    return statFile.fileData()->path;
  }

  static std::string directory;

private:
  StatFile();
  ~StatFile() = default;
  StatFile(const StatFile&) = delete;
  StatFile& operator=(const StatFile&) = delete;

  hobbes::fregion::writer statFile;
  std::mutex mutex;
};

std::string createSessionHash(const hobbes::datetimeT& timestamp, const hobbes::storage::ProcThread& pt);

}

#endif

