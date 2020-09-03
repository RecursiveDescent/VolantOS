#ifndef GC
#define GC

void* GC_malloc(int size);
void* GC_realloc(void* ptr, int size);
void GC_free(void* ptr);

#endif
