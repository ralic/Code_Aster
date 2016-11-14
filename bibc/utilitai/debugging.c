/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2016  EDF R&D              WWW.CODE-ASTER.ORG */
/*                                                                    */
/* THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR      */
/* MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS     */
/* PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE */
/* LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.                    */
/* THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,    */
/* BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF     */
/* MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU   */
/* GENERAL PUBLIC LICENSE FOR MORE DETAILS.                           */
/*                                                                    */
/* YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE  */
/* ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,      */
/*    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     */
/* ================================================================== */
/* person_in_charge: mathieu.courtois at edf.fr */

#include <execinfo.h>
#include <stdio.h>
#include <stdlib.h>

#include "aster.h"

/*
 * This module defines a wrapper to call GNU libc backtrace functions.
 */
#define LEVEL 25

/* Obtain a backtrace and print it to stdout. */
void DEF0(PRINT_TRACE,print_trace)
{
#ifdef HAVE_BACKTRACE

    void *array[LEVEL];
    size_t size;
    char **strings;
    size_t i;

    size = backtrace(array, LEVEL);
    strings = backtrace_symbols(array, size);

    fprintf(stderr, "Traceback returned by GNU libc (last %zd stack frames):\n", size);

    for (i = 0; i < size; i++)
        fprintf(stderr, "%s\n", strings[i]);

    free(strings);

#endif
}
