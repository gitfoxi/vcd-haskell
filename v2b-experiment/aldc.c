
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>

const char* lib_path = "./libaldc_el5_x86_64.so";

/* NOTE: Length must be a multiple of 8 bytes. Zero-pad if not */
const char src_buf[] =
{
    1, 1, 1, 1,
    1, 1, 1, 1,
    0, 0, 0, 0,
    0, 0, 0, 0
};
const long src_buf_len = 16;

int (*AldcEncInit)(unsigned long);
int (*AldcEncSrc)(const void *, long);
int (*AldcEncDest)(void *, long);
// int (*AldcEncStart)(void *, long);
int (*AldcEncStart)();
int (*AldcEncEnd)(int);

int error(char *s)
{
    fprintf(stderr, "%s\n", s);
    exit(1);
}

void print_result(const unsigned char* buf, const int len)
{
    printf("compressed bytes: ");
    int i;
    for(i = 0; i < len; i++) {
        printf("%02x ", buf[i]);
    }
    printf("\n");
}

int min(int a, int b) {
    if(a < b) return a;
    return b;
}

void encode(const char *src_buf, int src_buf_len, char **dest_buf_p, int *dest_buf_len_p)
{
    int r = (*AldcEncInit)(0xffffffff);
    if(r != 0) error("Failed AldcEncInit");

    int dest_buf_len = 2 * src_buf_len;
    char *dest_buf = malloc(dest_buf_len);

    r = (*AldcEncDest)(dest_buf, dest_buf_len);
    if(r != 0) error("Failed AldcEncDest");

    int start = 0;
    while(src_buf_len > 0) {
        int chunk_len = min(src_buf_len, 20000);
        src_buf_len -= chunk_len;

        r = (*AldcEncSrc)(src_buf + start, chunk_len);
        if(r != 0) error("Failed AldcEncSrc");

        r = (*AldcEncStart)(); // dest_buf, dest_buf_len); // maybe has no args
        if(r != 0xffffffff) error("Failed AldcEncStart");

        start += chunk_len;
    }

    int len = (*AldcEncEnd)(1);
    // printf("compressed len: %d\n", len);
    if(len <= 0) error("Failed AldcEncEnd");
    
    *dest_buf_len_p = len;
    *dest_buf_p = dest_buf;
}


int main(int argc, char** argv)
{
    void* lib = dlopen(lib_path, RTLD_NOW | RTLD_LOCAL);
    if(lib == NULL) error("Failed to open libaldc");

    *(void **)(&AldcEncInit) = dlsym(lib, "AldcEncInit");
    *(void **)(&AldcEncSrc) = dlsym(lib, "AldcEncSrc");
    *(void **)(&AldcEncDest) = dlsym(lib, "AldcEncDest");
    *(void **)(&AldcEncStart) = dlsym(lib, "AldcEncStart");
    *(void **)(&AldcEncEnd) = dlsym(lib, "AldcEncEnd");

    while(1)
    {
        if(feof(stdin)) exit(0);
        char *pin, *len_str;
        /* TODO: %m auto-allocating only works from GLIBC 2.7
         * Before that, use %a
         * Before that ... doesn't work */
        if( scanf("%ms %ms", &pin, &len_str) == EOF) exit(0);
        fgetc(stdin); /* burn extra space */
        int len = atoi(len_str);
        // printf("pin: %s len: %d\n", pin, len);
        char *dat = malloc(len);
        fread(dat, len, 1, stdin);
        // printf("data: ");
        // bindump(dat, len);
        int compressed_len;
        char *compressed_dat;
        encode(dat, len, &compressed_dat, &compressed_len);
        printf("%s %d ", pin, compressed_len);
        fwrite(compressed_dat, compressed_len, 1, stdout);
        fputc('\n', stdout);
        free(dat);
        free(compressed_dat);
    }

    dlclose(lib);
}


