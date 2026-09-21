find_path(Readline_ROOT_DIR
	NAMES include/readline/readline.h
  HINTS /opt/homebrew/opt/readline/ /usr/local/opt/readline/
  NO_DEFAULT_PATH
)

find_path(Readline_INCLUDE_DIRS
  NAMES readline/readline.h
  HINTS ${Readline_ROOT_DIR}/include
)

find_library(Readline_LIBRARIES
  NAMES readline
  HINTS ${Readline_ROOT_DIR}/lib
)

if(Readline_INCLUDE_DIRS AND Readline_LIBRARIES AND Ncurses_LIBRARY)
  set(READLINE_FOUND TRUE)
else()
  find_library(Readline_LIBRARIES NAMES readline)
  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(Readline DEFAULT_MSG Readline_INCLUDE_DIRS Readline_LIBRARIES)
  mark_as_advanced(Readline_INCLUDE_DIRS Readline_LIBRARIES)
endif()

mark_as_advanced(
  Readline_ROOT_DIR
  Readline_INCLUDE_DIRS
  Readline_LIBRARIES
)

