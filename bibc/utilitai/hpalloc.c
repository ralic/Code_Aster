/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2015  EDF R&D              WWW.CODE-ASTER.ORG */
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
#include "aster.h"
#include <errno.h>
#include <sys/mman.h>
// <malloc.h> is linux-specific
// http://stackoverflow.com/questions/12973311/difference-between-stdlib-h-and-malloc-h
// <stdlib.h> should now be used instead
#ifdef GNU_LINUX
#include <malloc.h>
#endif /* GNU_LINUX */

/*
This function uses mmap() to prevent memory fragmentation with malloc() on Linux.
mmap() is not available on other platforms but should not be needed at least on OS X (darwin).
*/

void DEFPPPP(HPALLOC, hpalloc, void **addr,INTEGER *length, INTEGER *errcode, INTEGER *abrt)
{
    void abort();
#ifdef GNU_LINUX
    int ir;
#endif /* GNU_LINUX */
    if ( *length <= 0 ) {
        *errcode = -1;
    }
    else
    {
#ifdef GNU_LINUX
        ir=mallopt(M_MMAP_THRESHOLD,0);
        *addr = (void *)malloc(*length * sizeof(INTEGER));
        ir=mallopt(M_MMAP_THRESHOLD,128*1024);
        if ( *addr == (void *)-1 )
#else
        *addr = (void *)malloc(*length * sizeof(INTEGER));
        if ( *addr == (void *)0 )
#endif /* GNU_LINUX */
        {
            *errcode = -2;
        }
        else if ( *addr == NULL )
        {
            *errcode = -3;
        }
        else
        {
            *errcode = 0;
        }
    }
    if ( *errcode != 0 && *abrt != 0 )
    {
     abort();
    }
}
