cd ..
bash main.sh
mv 0main.vo.o OS
cd OS
nasm -f elf32 main.s
ld -T link.ld -melf_i386 main.o 0main.vo.o gc.o -o kernel.elf
mv kernel.elf iso/boot

genisoimage -R -b boot/grub/stage2_eltorito \
		 -no-emul-boot \
		-boot-load-size 4 \
                -A os \
                -input-charset utf8 \
                -quiet \
                -boot-info-table \
                -o os.iso \
                iso
