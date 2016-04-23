
#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <iostream>
#include <string>
#include <set>
#include <vector>

#include <boost/asio.hpp>
#include <boost/thread/thread.hpp>

/* TODO: based on the number of CPUS */
#define THREADS 24

#define chunk_size 64000

typedef enum enVcdLine {Command, TimeStamp, StateChange, Unknown} VcdLine;

/* caller must free outp */
char* filter(const char * const inp, const size_t inp_bytes,
    const std::set<std::string>& keep_ids)
{
    char *outp = (char *) malloc(inp_bytes + 1);
    const char *start_line = inp;
    const char *p = inp;
    const char *buf_end = inp + inp_bytes - 1;
    char first_char = 0;
    VcdLine vcdline = Unknown;
    char *op = outp;
    char id[2048]; // warning: buffer can overflow with really long id
    char prev_timestamp[2048]; // warning: buffer can overflow with really long timestamp
    unsigned  prev_timestamp_len=0;
    unsigned idx=0;
    for(;p <= buf_end; p++) {
        if(*p == '\n') { // warning: will drop last line in file if it doesn't end with \n
            id[idx] = 0;
            size_t len = p - start_line + 1;
            if(vcdline == TimeStamp)
            {
                memcpy(prev_timestamp, start_line, len);
                prev_timestamp_len = len;
            }
            else if(vcdline == StateChange)
            {
                if(keep_ids.count(id)) {
                    if(prev_timestamp_len) {
                        memcpy(op, prev_timestamp, prev_timestamp_len);
                        op += prev_timestamp_len;
                        prev_timestamp[0]=0;
                        prev_timestamp_len=0;
                    }

                    memcpy(op, start_line, len);
                    op += len;
                }
            }

            start_line = p + 1;
            idx = 0;
        }
        if(p == start_line) {
            first_char = *p;
            if(first_char == '#')
                vcdline = TimeStamp;
            else if(first_char == '$')
                vcdline = Command;
            else {
                vcdline = StateChange;
                idx = 0;
            }
        } else {
            id[idx++] = *p;
        }
    }
    *op = 0;
    return outp;
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

Strips everything but requested signals and necessary timestamps
");
}

class Filter
{
    public:
        char *output;

        /* I almost want to make keep_ids global rather than pass it around so much */
        Filter(char *inp, size_t inp_len, const std::set<std::string>& keep_ids) {
            done = false;
            output = 0;
            this->inp = inp;
            this->inp_len = inp_len;
            this->keep_ids = keep_ids
        }

        ~Filter() {
            if(output) free(output);
            free(inp);
        }

        void go()
        {
            output = filter(inp, inp_len, keep_ids);
            done = true;
        }

        bool isDone() { return done; }
    private:
        bool done;
        char *inp;
        size_t inp_len;
        const std::set<std::string> keep_ids;

}

void concat(std::vector<Filter*> blocks)
{
    for(std::vector<Filter*>::iterator i = blocks.begin();
            i != blocks.end();
            i++)
    {
        while(!(*i)->isDone()) usleep(1000);
        fputs((*i)->output, stdout);
        delete (*i);
    }
}

int main(int argc, char **argv) {
    if(argc < 2) {
        usage();
        return 255;
    }

    std::set<std::string> keep_ids;
    for(int i=1; i<argc; i++) keep_ids.insert(argv[i]);


    /*
     * Create an asio::io_service and a thread_group (through pool in essence)
     */
    boost::asio::io_service ioService;
    boost::thread_group threadpool;


    /*
     * This will start the ioService processing loop. All tasks 
     * assigned with ioService.post() will start executing. 
     */
    boost::asio::io_service::work work(ioService);

    /*
     * This will add 2 threads to the thread pool. (You could just put it in a for loop)
     */
    for(int i=0; i<THREADS; i++)
        threadpool.create_thread(
                boost::bind(&boost::asio::io_service::run, &ioService)
                );

    /*
     * This will assign tasks to the thread pool. 
     * More about boost::bind: "http://www.boost.org/doc/libs/1_54_0/libs/bind/bind.html#with_functions"
     */

    /*
    int returned = 1;
    ioService.post(boost::bind(myPrint, "Hello World!\n", &returned));
    */

    while(1) {
        char *inp, *outp;
        size_t inp_bytes;
        inp = read_block_to_newline(&inp_bytes, chunk_size, stdin);
        if(inp_bytes == 0) break;
        outp = filter(inp, inp_bytes, keep_ids);
    }

    /*
     * This will stop the ioService processing loop. Any tasks
     * you add behind this point will not execute.
     */
    usleep(100);
    ioService.stop();

    /*
     * Will wait till all the treads in the thread pool are finished with 
     * their assigned tasks and 'join' them. Just assume the threads inside
     * the threadpool will be destroyed by this method.
     */
    threadpool.join_all();

    printf("Returned: %d\n", returned);

    return 0;
}
