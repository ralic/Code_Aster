/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF libinfos supervis  DATE 12/10/2011   AUTEUR COURTOIS M.COURTOIS */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2011  EDF R&D              WWW.CODE-ASTER.ORG */
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
/* RESPONSABLE COURTOIS M.COURTOIS */

#include "aster.h"
#include "aster_utils.h"

#ifndef _DISABLE_HDF5
#include "hdf5.h"
#endif
#ifndef _DISABLE_MED
#include "med.h"
#endif
#ifndef _DISABLE_SCOTCH
#include "scotch.h"
#endif

void DEFPPP(LIHDFV,lihdfv, _OUT INTEGER *major, _OUT INTEGER *minor, _OUT INTEGER *patch )
{
    /* Retourne la version de HDF5 */
    int ier = 0;
    unsigned int n1=0, n2=0, n3=0;
#ifndef _DISABLE_HDF5
    ier = (int)H5get_libversion( &n1, &n2, &n3 );
#endif
    *major = (INTEGER)n1;
    *minor = (INTEGER)n2;
    *patch = (INTEGER)n3;
    return;
}

void DEFPPP(LIMEDV,limedv, _OUT INTEGER *major, _OUT INTEGER *minor, _OUT INTEGER *patch )
{
    /* Retourne la version de MED */
    int ier = 0;
#ifndef _DISABLE_MED
    med_int n1=0, n2=0, n3=0;
    ier = (int)MEDlibraryNumVersion( &n1, &n2, &n3 );
#endif
    *major = (INTEGER)n1;
    *minor = (INTEGER)n2;
    *patch = (INTEGER)n3;
    return;
}

void DEFPPP(LISCOV,liscov, _OUT INTEGER *major, _OUT INTEGER *minor, _OUT INTEGER *patch )
{
    /* Retourne la version de SCOTCH */
#ifndef _DISABLE_SCOTCH
    *major = (INTEGER)SCOTCH_VERSION;
    *minor = (INTEGER)SCOTCH_RELEASE;
    *patch = (INTEGER)SCOTCH_PATCHLEVEL;
#else
    *major = (INTEGER)0;
    *minor = (INTEGER)0;
    *patch = (INTEGER)0;
#endif
    return;
}

