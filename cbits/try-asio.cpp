
/* compile:
 *    g++ try-asio.cpp -lboost_thread-mt -lboost_system-mt
*/

#include <boost/asio.hpp>
#include <boost/thread/thread.hpp>
#include <unistd.h> // usleep

/* TODO: based on the number of CPUS */
#define THREADS 24

int myPrint(const char *s, int *r) {
    *r = 42;
    return printf("%s", s);
}

int main()
{
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
    int returned = 1;
    ioService.post(boost::bind(myPrint, "Hello World!\n", &returned));

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
