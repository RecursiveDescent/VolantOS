chmod 0777 ./volant/bin/volant
#install-pkg libblocksruntime-dev libgc-dev libuv-dev
clear
./volant/bin/volant compile main.vo -clang "-g -Ifakegc fakegc/gc.c -m32 -nostdlib -nostdinc -fno-builtin -fno-stack-protector -nostartfiles" -packstructs
