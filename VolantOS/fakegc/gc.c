#include "gc.h"

char heap[1000];

unsigned long heapptr = 0;

void* GC_malloc(int size) {
    heapptr += size;

    return (void*) (heap + heapptr);
}

void* GC_realloc(void* ptr, int size) {
    return ptr;
}

void GC_free(void* ptr) {

}
