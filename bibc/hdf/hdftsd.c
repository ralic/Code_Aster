/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF hdftsd hdf  DATE 30/09/2008   AUTEUR COURTOIS M.COURTOIS */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2003  EDF R&D              WWW.CODE-ASTER.ORG */
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
/*-----------------------------------------------------------------------------/
/ Récupération du type et de la taille des valeurs stockées dans un dataset
/ au sein d'un fichier HDF 
/  Paramètres :
/   - in  iddat : identificateur du dataset (hid_t)
/   - out type  : type associé (char *)
/   - out ltype : longueur du type (long)
/   - out lv    : longueur du vecteur associé (dataspace de dim 1) (long)
/  Résultats :
/    =O OK, -1 sinon
/-----------------------------------------------------------------------------*/
#include "hdf5.h"
#define FALSE   0

INTEGER DEFPSPP(HDFTSD, hdftsd, INTEGER *iddat, char *type, STRING_SIZE lt, INTEGER *ltype, INTEGER *lv)
{
  hid_t id,datatype,class,dataspace;
  hsize_t dims_out[1];
  long iret=-1;
  int k,rank,status;

  id=(hid_t) *iddat;
  datatype  = H5Dget_type(id);     
  class     = H5Tget_class(datatype);
  if      (class == H5T_INTEGER)  *type='I';
  else if (class == H5T_FLOAT)    *type='R';
  else if (class == H5T_STRING)   *type='K';
  else                            *type='?';

  for (k=1;k<lt;k++) {
    *(type+k)=' ';
  }
  if ((*ltype = (int)H5Tget_size(datatype))>=0 ) {
    if ((dataspace = H5Dget_space(id))>=0 ) { 
      if ((rank = H5Sget_simple_extent_ndims(dataspace))==1) {
        status = H5Sget_simple_extent_dims(dataspace, dims_out, NULL);
        *lv = (long)dims_out[0];  
        H5Sclose(dataspace);
        iret=0;
      }
    }
  }
  return iret;
}
