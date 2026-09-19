
#ifndef HOBBES_LANG_TYPEPREDS_NET_CONNECTION_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_NET_CONNECTION_HPP_INCLUDED

#include <set>
#include <string>

namespace hobbes {

class cc;
void initNetworkDefs(cc&);

// Resolving a (Connect "host:port" c) constraint opens a connection to a
// remote peer as a side effect of type-constraint resolution, not
// evaluation -- disabled by default. Call this to allow the exact
// "host:port" strings in allowedHostPorts to be connected to.
void enableRemoteConnections(cc&, const std::set<std::string>& allowedHostPorts);

// Resolving an (Invoke ...) constraint ships code to a remote peer and
// executes it there as a side effect of type-constraint resolution, not
// evaluation -- disabled by default, independent of enableRemoteConnections
// above (being willing to connect to a host does not imply trusting it to
// run code). Call this to allow the exact "host:port" strings in
// allowedHostPorts (matching Client::remoteHost()) to be invoked on.
void enableRemoteInvocation(cc&, const std::set<std::string>& allowedHostPorts);

}

#endif

