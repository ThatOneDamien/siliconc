#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
 
#ifdef __GNUC__
    #define UNUSED __attribute__((unused))
    #define FALLTHROUGH __attribute__((fallthrough))
    #define NORETURN __attribute__((noreturn))
    #define PRINTF_FMT(fmt, va_args) __attribute__((format(printf, fmt, va_args)))
#else
    #define UNUSED
    #define FALLTHROUGH
    #define NORETURN
    #define PRINTF_FMT(fmt, va_args)
#endif

#ifdef SI_DEBUG
    #include <signal.h>
    #define SIC_ERROR_DBG(msg)                      \
        do                                          \
        {                                           \
            fprintf(stderr, "\033[31m[DEBUG]: ");   \
            fprintf(stderr, msg);                   \
            fprintf(stderr, "\033[0m\n");           \
            raise(SIGTRAP);                         \
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
