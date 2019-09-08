#include "monkeyws.hpp"

int monkeyWS::init_socket(){
	EM_ASM(
        if (!window.monkeyWS) {
            // somewhere this needs to free the data allocated by _malloc.
            function toCString(jsString) {
                var lengthBytes = lengthBytesUTF8(jsString) + 1;
                var stringOnWasmHeap = _malloc(lengthBytes);
                stringToUTF8(jsString, stringOnWasmHeap, lengthBytes + 1);
                return stringOnWasmHeap;
            }
            function MonkeySocket() {
                this.url = "";
                this.protocols = [];
                this.Q = [];
                this.connected = false;
                // a fake socket to prevent errors caused by an uninstantiated socket
                this.sock = {};
                this.sock.readyState = 3;
                this.sock.binaryType = "undefined";
                this.hasError = false;
                this.errorMessage = toCString("");
            }

            MonkeySocket.prototype = {};
            var msp = MonkeySocket.prototype;
            msp.addProtocol = function (protocol) { this.protocols.push(protocol); };
            msp.connect = function () {
                this.sock = new WebSocket(this.url, this.protocols);
                this.sock.addEventListener('open', this.onConnected.bind(this));
                this.sock.addEventListener('message', this.onMessage.bind(this));
                this.sock.addEventListener('error', this.onError.bind(this));
                this.sock.addEventListener('close', this.onClose.bind(this));
            };
            msp.onMessage = function (data) { this.Q.push(data); };
            msp.onConnected = function () { this.connected = true; };
            msp.onClose = function () { this.connected = false; };
            msp.onError = function (ev) { 
                this.hasError = true; 
                this.errorMessage = toCString(ev.error.message);
            };
            msp.getData = function () {
                if (this.Q.length != 0) { return toCString(this.Q.shift().data); }
                return toCString("");
            };
            msp.hasData = function () { return this.Q.length > 0; };
            msp.close = function(code, reason){
                this.sock.close( code, reason );
            };
            msp.getBinaryType = function(){
                return toCString(this.sock.binaryType);
            };

            window.monkeyWS = {};
            monkeyWS.sockets = [];
            monkeyWS.createSocket = function createSocket() {
                this.sockets.push(new MonkeySocket());
                return this.sockets.length - 1;
            };
        }
	);

    return EM_ASM_INT_V({ return monkeyWS.createSocket(); });
}

int monkeyWS::set_url(int sock, const char* URL){
    EM_ASM_({ monkeyWS.sockets[$0].url = UTF8ToString($1); }, sock, URL );
    return 1;
}

void monkeyWS::add_protocol(int sock, const char* protocol ){
    EM_ASM_({ monkeyWS.sockets[$0].addProtocol(UTF8ToString($1)); }, sock, protocol );
}

void monkeyWS::connect(int sock){
    EM_ASM_({ monkeyWS.sockets[$0].connect(); }, sock);
}

int monkeyWS::ready_state(int sock){
    return EM_ASM_INT({return monkeyWS.sockets[$0].sock.readyState;}, sock );
}

void monkeyWS::send(int sock, const char* data){
    EM_ASM_({ monkeyWS.sockets[$0].sock.send(UTF8ToString($1)); }, sock, data );
    // return 1;
}

bool monkeyWS::has_data(int sock){
    return (bool)EM_ASM_INT({return monkeyWS.sockets[$0].hasData();}, sock );
}

char* monkeyWS::get_data(int sock){
    char *str = (char*)EM_ASM_INT({ return monkeyWS.sockets[$0].getData();}, sock);
    //free(str); -- need to free the data somewhere..
    return str;
}

void monkeyWS::close(int sock, int code, const char* reason ){
    EM_ASM_({ monkeyWS.sockets[$0].sock.close( $1, UTF8ToString($2) ); }, sock, code, reason );
}

bool monkeyWS::has_error(int sock){
    return (bool)EM_ASM_INT({return monkeyWS.sockets[$0].hasError;}, sock );
} 

char* monkeyWS::error_message(int sock){
    char *str = (char*)EM_ASM_INT({ return monkeyWS.sockets[$0].errorMessage; }, sock);
    //free(str); -- need to free the data somewhere..
    return str;
}

// [WARNING] real data type is: unsigned long
int monkeyWS::buffered_amount(int sock){
    return EM_ASM_INT({return monkeyWS.sockets[$0].sock.bufferedAmount; }, sock );
}

char* monkeyWS::binary_type(int sock){
    char *str = (char*)EM_ASM_INT({ return monkeyWS.sockets[$0].getBinaryType(); }, sock);
    //free(str); -- need to free the data somewhere..
    return str;
}