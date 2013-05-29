/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
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
/* ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,      */
/*    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     */
/* ================================================================== */
#include "aster.h"
#include "aster_fort.h"
/*-----------------------------------------------------------------------------/
/ Fermeture d'un dataset HDF 
/  Paramètres :
/   - in  idfic : identificateur du dataset (hid_t)
/  Résultats :
/     0 = fermeture OK, -1 sinon (long)
/-----------------------------------------------------------------------------*/
#ifndef _DISABLE_HDF5
#include <hdf5.h>
#endif

INTEGER DEFP(HDFCLD, hdfcld, INTEGER *idd)
{
#ifndef _DISABLE_HDF5
    hid_t id;
    herr_t icode;
 
    id=(hid_t) *idd;
    if ((icode = H5Dclose(id)) < 0) 
        return -1 ;
#else
    CALL_U2MESS("F", "FERMETUR_3");
#endif
    return 0;
}     
