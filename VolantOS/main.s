 global loader

    MAGIC_NUMBER equ 0x1BADB002   
    FLAGS        equ 0x0            
    CHECKSUM     equ -MAGIC_NUMBER  
                                    

    section .text:                 
    align 4                        
        dd MAGIC_NUMBER             
        dd FLAGS                    
        dd CHECKSUM                 

    loader:
	extern main                   
        mov esp, kernel_stack + KERNEL_STACK_SIZE
	call main
    .loop:
        jmp .loop  


    KERNEL_STACK_SIZE equ 4096                  ; size of stack in bytes

    section .bss
    align 4                                     ; align at 4 bytes
    kernel_stack:                               ; label points to beginning of memory
        resb KERNEL_STACK_SIZE 
