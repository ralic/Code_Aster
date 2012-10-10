/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF HPALLOC UTILITAI  DATE 10/10/2012   AUTEUR LEFEBVRE J-P.LEFEBVRE */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2012  EDF R&D              WWW.CODE-ASTER.ORG */
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
/* ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,    */
/*    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     */
/* ================================================================== */
#include "aster.h"
#include <errno.h>
#include <sys/mman.h>
#include <malloc.h>

void DEFPPPP(HPALLOC, hpalloc, void **addr,INTEGER *length, INTEGER *errcode, INTEGER *abrt)
{
    void abort();
    int ir;
    if ( *length <= 0 ) {
        *errcode = -1;
    }
    else
    {
        ir=mallopt(M_MMAP_THRESHOLD,0);
        *addr = (void *)malloc(*length * sizeof(INTEGER)); 
        ir=mallopt(M_MMAP_THRESHOLD,128*1024);
        if ( *addr == (void *)-1 )
        {
            *errcode = -2;
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
