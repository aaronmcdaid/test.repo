stack build && echo && echo == && echo && exe="./.stack-work/install/x86_64-linux/lts-9.11/8.0.2/bin/simple-exe --send-for 4 --wait-for 1" && {
    $exe slave  localhost 8081 &
    $exe slave  localhost 8082 &
    $exe slave  localhost 8083 &
    $exe master localhost 8080 --with-seed 1234
}
