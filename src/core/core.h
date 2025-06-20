#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
 
#define UNUSED __attribute__((unused))

#ifdef SI_DEBUG
    #include <signal.h>
    #define SIC_ERROR_DBG(msg)              \
        do                                  \
        {                                   \
            fprintf(stderr, "\033[31m");    \
            fprintf(stderr, msg);           \
            fprintf(stderr, "\033[0m\n");   \
            raise(SIGTRAP);                 \
        } while(0)
    #define SIC_ERROR_DBG_ARGS(msg, ...)        \
        do                                      \
        {                                       \
            fprintf(stderr, "\033[31m");        \
            fprintf(stderr, msg, __VA_ARGS__);  \
            fprintf(stderr, "\033[0m\n");       \
            raise(SIGTRAP);                     \
        } while(0)
    #define SIC_ASSERT(cond)                                        \
        do                                                          \
        {                                                           \
            if(!(cond))                                             \
                SIC_ERROR_DBG_ARGS("Assertion failed (%s: %d): %s", \
                                   __FILE__, __LINE__, #cond);      \
        } while(0)
    #define SIC_ASSERT_MSG(cond, msg)   \
        do                              \
        {                               \
            if(!(cond))                 \
                SIC_ERROR_DBG(msg);     \
        } while(0)
    #define SIC_ASSERT_ARGS(cond, msg, ...)             \
        do                                              \
        {                                               \
            if(!(cond))                                 \
                SIC_ERROR_DBG_ARGS(msg, __VA_ARGS__);   \
        } while(0)
#else
    #define SIC_ERROR_DBG(msg)
    #define SIC_ERROR_DBG_ARGS(msg, ...)
    #define SIC_ASSERT(cond)
    #define SIC_ASSERT_MSG(cond, msg)
    #define SIC_ASSERT_ARGS(cond, msg, ...)
#endif

void run_subprocess(char** cmd);
