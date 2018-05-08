#include <hobbes/ipc/net.H> // connectSocket
#include <hobbes/util/codec.H> // fdwrite
#include <hobbes/util/os.H> // BUILD_LINUX, BUILD_OSX

#include <sys/socket.h>
#include <sys/stat.h>
#include <stdexcept>
#include <unistd.h>

#ifdef BUILD_LINUX
#include <sys/sendfile.h>
#endif

#include "netconnection.H"

#ifndef BUILD_LINUX
/// @pre *o == 0
ssize_t sendfile(int toFD, int fromFD, off_t* o, off_t sz) {
  char buf[4096];
  size_t i = 0;
  do {
    ssize_t di = read(fromFD, buf, sizeof(buf));

    if (di < 0) {
      if (errno != EINTR) { return -1; }
    } else if (di == 0) {
      return 0;
    } else {
      try { hobbes::fdwrite(toFD, buf, di); } catch (std::exception&) { return -1; }
      *o += di;
    }
  } while (*o < sz);
  return 0;
}
#endif

int sendFlag() {
  #ifdef BUILD_LINUX
    return MSG_NOSIGNAL;
  #else // OSX does not support MSG_NOSIGNAL
    return 0;
  #endif
}

void initSocket(int fd) {
  #ifdef BUILD_OSX
    int set = 1;
    setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &set, sizeof(int));
  #else
    (void) fd;
  #endif
}


namespace hog {

DefaultNetConnection::DefaultNetConnection(const std::string& hostport)
  :_socket(hobbes::connectSocket(hostport))
{
  if (_socket < 0) {
    throw std::runtime_error("Failed to connect to: " + hostport);
  }

  initSocket(_socket);
}

DefaultNetConnection::DefaultNetConnection(int fd)
  :_socket(fd)
{
  initSocket(_socket);
}

DefaultNetConnection::~DefaultNetConnection()
{
  close(_socket);
}

bool DefaultNetConnection::send(const void* buf, size_t size) {
  const char* buffer = static_cast<const char*>(buf);
  size_t offset = 0;
  while (size > 0) {
    const ssize_t sent = ::send(_socket, buffer + offset, size, sendFlag());
    if (sent < 0) {
      if (errno != EINTR) {
        return false;
      }
    } else {
      offset += sent;
      size -= sent;
    }
  }

  return true;
}

bool DefaultNetConnection::sendFile(int fd) {
  struct stat sb;
  if (fstat(fd, &sb) < 0) {
    return false;
  }

  // let the remote side know how big this file is
  const uint64_t fsize = sb.st_size;
  if (!this->send(&fsize, sizeof(fsize))) {
    return false;
  }

  off_t size = sb.st_size;
  off_t offset = 0;

  while (size) {
    const ssize_t sent = sendfile(_socket, fd, &offset, size);
    if (sent >= 0) {
      size -= sent;
    }
    else if (errno != EAGAIN) {
      return false;
    }
  }

  return true;
}

bool DefaultNetConnection::receive(void* buf, size_t size) {
  char* buffer = static_cast<char*>(buf);
  size_t offset = 0;
  while (size > 0) {
    const ssize_t received = recv(_socket, buffer + offset, size, 0);
    if (received >= 0) {
      offset += received;
      size -= received;
    }
    else if (errno != EINTR) {
      return false;
    }
  }

  return true;
}

void sendString(NetConnection& c, const std::string& str) {
  uint64_t size = str.size();
  const bool ok = c.send(&size, sizeof(size)) && c.send(str.data(), size);
  if (!ok) { throw std::runtime_error("sendString failed:" + std::string(strerror(errno))); }
}

void receiveIntoBuffer(NetConnection& c, std::vector<uint8_t>* dst)
{
  uint64_t size = 0;
  const bool hok = c.receive(&size, sizeof(size));
  if (!hok) { throw std::runtime_error("receiveIntoBuffer failed to get size:" + std::string(strerror(errno))); }

  dst->resize(size);
  const bool bok = c.receive(dst->data(), size);
  if (!bok) { throw std::runtime_error("receiveIntoBuffer failed to get body:" + std::string(strerror(errno))); }
}

std::vector<uint8_t> receiveBuffer(NetConnection& c)
{
  std::vector<uint8_t> buffer;
  receiveIntoBuffer(c, &buffer);
  return buffer;
}

std::string receiveString(NetConnection& c)
{
  std::vector<uint8_t> buffer(receiveBuffer(c));
  return std::string(buffer.begin(), buffer.end());
}

} // namespace hog
