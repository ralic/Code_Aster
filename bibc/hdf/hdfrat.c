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
/* person_in_charge: j-pierre.lefebvre at edf.fr */
#include "aster.h"
#include "aster_fort.h"
/*-----------------------------------------------------------------------------/
/ Lire un attribut de type chaine de caractères associé à un dataset 
/ au sein d'un fichier HDF 
/  Paramètres :
/   - in  iddat : identificateur du dataset (hid_t)
/   - in  nomat : nom de l'attribut (char *)
/   - in  nbv   : nombre de valeurs associées à l'attribut (long)
/   - out valat : valeur de l'attribut (char *)
/  Résultats :
/     =0 OK, =-1 problème 
/-----------------------------------------------------------------------------*/
#ifndef _DISABLE_HDF5
#include <hdf5.h>
#endif

INTEGER DEFPSPS(HDFRAT, hdfrat, INTEGER *iddat, char *nomat, STRING_SIZE ln, INTEGER *nbv,
                char *valat, STRING_SIZE lv)
{
  INTEGER iret=-1;
#ifndef _DISABLE_HDF5
  hid_t ida,attr,atyp,aspa;  
  herr_t ret;
  int k;
  int rank;
  size_t lt;
  hsize_t sdim[1]; 
  char *nom;
  void *malloc(size_t size); 
   
  ida=(hid_t) *iddat;
  nom = (char *) malloc((ln+1) * sizeof(char));
  for (k=0;k<ln;k++) {
     nom[k] = nomat[k];
  }
  k=ln-1;
  while (nom[k] == ' ') { k--;}
  nom[k+1] = '\0';
  if ( (attr = H5Aopen(ida,nom,H5P_DEFAULT)) >= 0) { 
    atyp = H5Aget_type(attr);
    lt=H5Tget_size(atyp);
    aspa = H5Aget_space(attr);
    if ( (rank = H5Sget_simple_extent_ndims(aspa)) == 1) {
      ret  = H5Sget_simple_extent_dims(aspa, sdim, NULL);
      ret  = H5Aread(attr, atyp, valat);
      iret = 0;
    } 
    ret  = H5Aclose(attr);
  } 
  free(nom);
#else
  CALL_U2MESS("F", "FERMETUR_3");
#endif
  return iret;
}
