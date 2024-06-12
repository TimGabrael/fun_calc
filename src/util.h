#pragma once

#define ARRSIZE(arr) (sizeof(arr) / sizeof(*arr))

#define CALL_FUNCTION_MACRO(fnptr, data, count, output) \
    if(0 == count) {\
        output = reinterpret_cast<float(*)()>(fnptr)();\
    }\
    else if(1 == count) {\
        output = reinterpret_cast<float(*)(float)>(fnptr)(data[0]);\
    }\
    else if(2 == count) {\
        output = reinterpret_cast<float(*)(float, float)>(fnptr)(data[0], data[1]);\
    }\
    else if(3 == count) {\
        output = reinterpret_cast<float(*)(float, float, float)>(fnptr)(data[0], data[1], data[2]);\
    }\
    else if(4 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3]);\
    }\
    else if(5 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4]);\
    }\
    else if(6 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5]);\
    }\
    else if(7 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6]);\
    }\
    else if(8 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7]);\
    }\
    else if(9 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8]);\
    }\
    else if(10 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9]);\
    }\
    else if(11 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9], data[10]);\
    }\
    else if(12 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9], data[10], data[11]);\
    }\
    else if(13 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9], data[10], data[11], data[12]);\
    }\
    else if(14 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9], data[10], data[11], data[12], data[13]);\
    }\
    else if(15 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9], data[10], data[11], data[12], data[13], data[14]);\
    }\
    else if(16 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9], data[10], data[11], data[12], data[13], data[14], data[15]);\
    }\
    else if(17 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9], data[10], data[11], data[12], data[13], data[14], data[15], data[16]);\
    }\
    else if(18 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9], data[10], data[11], data[12], data[13], data[14], data[15], data[16], data[17]);\
    }\
    else if(19 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9], data[10], data[11], data[12], data[13], data[14], data[15], data[16], data[17], data[18]);\
    }\
    else if(20 == count) {\
        output = reinterpret_cast<float(*)(float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float)>(fnptr)(data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9], data[10], data[11], data[12], data[13], data[14], data[15], data[16], data[17], data[18], data[19]);\
    }


