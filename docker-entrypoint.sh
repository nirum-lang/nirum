#!/bin/bash
if [[ "${#@}" -lt 1 ]]; then
  echo "Nirum in Docker"
  echo
  echo "This image aims you to give a try easily Nirum without any" \
       "installation on your system.  However, if you consider using" \
       "seriously Nirum we recommend you to install Nirum on your system" \
       'because using Nirum CLI ("nirum") through "docker run" command is not' \
       "that convenient.  There are several ways to install Nirum including" \
       "statically linked binaries."
  echo
  echo "Due to the nature of Docker, you need to mount a volume to give" \
       "the input source files and find the output object files. " \
       'It could be done using -v/--volume option of "docker run" command. ' \
       "For example:"
  echo
  echo '  $ mkdir -p src build'
  # shellcheck disable=SC2016
  echo '  $ docker run --volume `pwd`/src:/tmp/src '\\
  # shellcheck disable=SC2016
  echo '               --volume `pwd`/build:/tmp/build '\\
  echo '               --rm -it '\\
  echo '               spoqa/nirum '\\
  echo '               -o /tmp/build /tmp/src'
  echo
fi
"$CMD" "${@}"
