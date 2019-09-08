#ifndef MONKEYWS_H
#define MONKEYWS_H
#include <emscripten.h>
#include <bbmonkey.h>

namespace monkeyWS{
    //using namespace std;
    int init_socket();

    int set_url(int sock, const char* URL);

    void add_protocol(int sock, const char* protocol );

    void connect(int sock);

    int ready_state(int sock);
    
    void send(int sock, const char* data);
    
    bool has_data(int sock);
    
    char* get_data(int sock);
    
    void close(int sock, int code, const char* reason );
    
    bool has_error(int sock);
    
    char* error_message(int sock);
    
    // [WARNING] real data type is: unsigned long
    int buffered_amount(int sock);
    
    char* binary_type(int sock);

}

#endif