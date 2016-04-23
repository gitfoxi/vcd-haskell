
#include <malloc.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <pcre.h>

#include <iostream>
#include <string>
#include <set>

#define chunk_size 64000

void filter(char *outp, const char * const inp, const size_t inp_bytes)
{
    const char *start_line = inp;
    const char *p = inp;
    const char *buf_end = inp + inp_bytes - 1;
    char first_char = 0;
    bool good = false;
    char *op = outp;
    for(;p <= buf_end; p++) {
        if(*p == '\n') {
            if(good == true) {
                size_t len = p - start_line + 1;
                memcpy(op, start_line, len);
                op += len;
            }
            start_line = p + 1;
            good = true;
        }
        if(p == start_line) {
            first_char = *p;
            /* Cheap parser to detect leading '#' */
            if(first_char == '#') good = true;
            else good = false;
        }
    }
    /* TODO: pass back unconsumed partial line */
    *op = 0;
    return;
}

/* caller needs to free the returned block */
char *read_block_to_newline(size_t *act_sz, size_t sz, FILE* f)
{
    char *inp = (char *)malloc(sz); 
    size_t inp_bytes = fread(inp, 1, chunk_size, f);
    char tail[1024];
    size_t tail_len;
    *act_sz = inp_bytes;
    if(inp_bytes < chunk_size) return inp;

    do {
        if(fgets(tail, 1024, f) == NULL);
            break; // optomistic
        tail_len = strlen(tail);
        inp = (char*)realloc((void *)inp, *act_sz + tail_len);
        memcpy(inp + *act_sz, tail, tail_len);
        *act_sz += tail_len;
    } while(tail_len == 1023); // go again if this is a really long line

    return inp;
}

void usage()
{
    fprintf(stderr, " \
Usage: \n\
   vcdstrip sig1 sig2 < my.vcd \n\
");
}

int main(int argc, char **argv) {
    if(argc < 2) {
        usage();
        return 255;
    }

    while(1) {
        char *inp;
        size_t inp_bytes;
        char outp[chunk_size+1]; // extra byte for 0 termination TODO: not sure about doing it this way
        inp = read_block_to_newline(&inp_bytes, chunk_size, stdin);
        if(inp_bytes == 0) break;
        filter(outp, inp, inp_bytes);
        free(inp);
        fputs(outp, stdout);
    }

    return 0;
}
