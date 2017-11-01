stack build && echo && echo == && echo && exe="./.stack-work/install/x86_64-linux/lts-9.11/8.0.2/bin/simple-exe" && {
    opts="--send-for 2 --wait-for 3 --with-seed 1234"
    $exe slave  localhost 8081 $opts &
    $exe slave  localhost 8082 $opts &
    $exe slave  localhost 8083 $opts &
    $exe master localhost 8080 $opts
}
