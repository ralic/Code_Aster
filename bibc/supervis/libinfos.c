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
/* person_in_charge: mathieu.courtois at edf.fr */

#include "aster.h"
#include "aster_utils.h"

#ifndef _DISABLE_HDF5
#include "hdf5.h"
#endif
#ifndef _DISABLE_MED
#include "med.h"
#endif
#ifndef _DISABLE_SCOTCH
/* scotch.h may use int64_t without including <sys/types.h> */
#include <sys/types.h>
#include "scotch.h"
#endif

void DEFPPP(LIHDFV,lihdfv, _OUT ASTERINTEGER *major, _OUT ASTERINTEGER *minor,
            _OUT ASTERINTEGER *patch )
{
    /* Retourne la version de HDF5 */
    int ier = 0;
    unsigned int n1=0, n2=0, n3=0;
#ifndef _DISABLE_HDF5
    ier = (int)H5get_libversion( &n1, &n2, &n3 );
#endif
    *major = (ASTERINTEGER)n1;
    *minor = (ASTERINTEGER)n2;
    *patch = (ASTERINTEGER)n3;
    return;
}

void DEFPPP(LIMEDV,limedv, _OUT ASTERINTEGER *major, _OUT ASTERINTEGER *minor,
            _OUT ASTERINTEGER *patch )
{
    /* Retourne la version de MED */
    int ier = 0;
#ifndef _DISABLE_MED
    med_int n1=0, n2=0, n3=0;
    ier = (int)MEDlibraryNumVersion( &n1, &n2, &n3 );
#else
    unsigned int n1=0, n2=0, n3=0;
#endif
    *major = (ASTERINTEGER)n1;
    *minor = (ASTERINTEGER)n2;
    *patch = (ASTERINTEGER)n3;
    return;
}

void DEFPPP(LISCOV,liscov, _OUT ASTERINTEGER *major, _OUT ASTERINTEGER *minor,
            _OUT ASTERINTEGER *patch )
{
    /* Retourne la version de SCOTCH */
#ifndef _DISABLE_SCOTCH
    *major = (ASTERINTEGER)SCOTCH_VERSION;
    *minor = (ASTERINTEGER)SCOTCH_RELEASE;
    *patch = (ASTERINTEGER)SCOTCH_PATCHLEVEL;
#else
    *major = (ASTERINTEGER)0;
    *minor = (ASTERINTEGER)0;
    *patch = (ASTERINTEGER)0;
#endif
    return;
}
