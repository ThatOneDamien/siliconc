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
        {                                   \
            fprintf(stderr, "\033[31m");    \
            fprintf(stderr, msg);           \
            fprintf(stderr, "\033[0m\n");   \
            raise(SIGTRAP);                 \
        }
    #define SIC_ERROR_DBG_ARGS(msg, ...)        \
        {                                       \
            fprintf(stderr, "\033[31m");        \
            fprintf(stderr, msg, __VA_ARGS__);  \
            fprintf(stderr, "\033[0m\n");       \
            raise(SIGTRAP);                     \
        }
    #define SIC_ASSERT(cond)                                        \
        {                                                           \
            if(!(cond))                                             \
                SIC_ERROR_DBG_ARGS("Assertion failed (%s: %d): %s", \
                                   __FILE__, __LINE__, #cond)       \
        }
    #define SIC_ASSERT_MSG(cond, msg)   \
        {                               \
            if(!(cond))                 \
                SIC_ERROR_DBG(msg)      \
        }
    #define SIC_ASSERT_ARGS(cond, msg, ...)             \
        {                                               \
            if(!(cond))                                 \
                SIC_ERROR_DBG_ARGS(msg, __VA_ARGS__)    \
        }
#else
    #define GEM_ERROR(msg)
    #define GEM_ERROR_ARGS(msg, ...)
    #define GEM_ASSERT(cond)
    #define GEM_ASSERT_MSG(cond, msg)
    #define GEM_ASSERT_ARGS(cond, msg, ...)
#endif

#ifdef __GNUC__
    #define alignas(alignment) __attribute__((packed,aligned(alignment)))
#else
    #define alignas(alignment) 
#endif
