#ifndef HOG_STAT_H_INCLUDED
#define HOG_STAT_H_INCLUDED

#include <mutex>

#include <hobbes/hobbes.H>
#include <hobbes/reflect.H>
#include <hobbes/storage.H>
#include <hobbes/fregion.H>

namespace hog {

DEFINE_STRUCT(ProcessEnvironment,
  (hobbes::datetimeT, startTime),
  (std::string,       runMode)
);

DEFINE_ENUM(ReaderStatus,
  (Started),
  (Closed)
);

DEFINE_STRUCT(ReaderState,
  (hobbes::datetimeT,           datetime),
  (hobbes::storage::ProcThread, readerId),
  (ReaderStatus,                status)
);

DEFINE_STRUCT(ReaderRegistration,
  (hobbes::datetimeT,           datetime),
  (hobbes::storage::ProcThread, writerId),
  (hobbes::storage::ProcThread, readerId),
  (std::string,                 shmname)
);

DEFINE_ENUM(SenderStatus,
  (Suspended),
  (Started),
  (Closed)
);

DEFINE_STRUCT(SenderState,
  (hobbes::datetimeT,           datetime),
  (hobbes::storage::ProcThread, id),
  (SenderStatus,                status)
);

DEFINE_STRUCT(SenderRegistration,
  (hobbes::datetimeT,           datetime),
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

  const std::string& filename() const {
    return statFile.fileData()->path;
  }

private:
  StatFile();
  ~StatFile() = default;
  StatFile(const StatFile&) = delete;
  StatFile& operator=(const StatFile&) = delete;

  hobbes::fregion::writer statFile;
  std::mutex mutex;
};

}

#endif

