/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF hdfrat utilitai  DATE 06/09/2003   AUTEUR D6BHHJP J.P.LEFEBVRE */
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
/*-----------------------------------------------------------------------------/
/ Lire un attribut de type chaine de caractères associé à un dataset 
/ au sein d'un fichier HDF 
/  Paramètres :
/   - in  iddat : identificateur du dataset (hid_t)
/   - in  nomat : nom de l'attribut (char *)
/   - in  nbv   : nombre de valeurs associées à l'attribut (long)
/   - in  valat : valeur de l'attribut (char *)
/  Résultats :
/     =0 OK, =-1 problème 
/-----------------------------------------------------------------------------*/
#include "hdf5.h"

#if defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
   long hdfrat_(long *iddat, char *nomat, long *nbv, char *valat, long ln, long lv )
#elif defined HPUX
   long hdfrat(long *iddat, char *nomat, long *nbv, char *valat, long ln, long lv )
#elif defined PPRO_NT
   extern long __stdcall HDFRAT(long *iddat, char *nomat, unsigned long ln, long *nbv, char *valat,  unsigned long lv )
#endif
{
  hid_t ida,attr,atyp,aspa;  
  herr_t ret;
  int k,rank;
  long iret=-1,lt;
  hsize_t sdim[1]; 
  char *nom;

  ida=(hid_t) *iddat;
  nom = (char *) malloc((ln+1) * sizeof(char));
  for (k=0;k<ln;k++) {
     nom[k] = nomat[k];
  }
  k=ln-1;
  while (nom[k] == ' ') { k--;}
  nom[k+1] = '\0';
  if ( (attr = H5Aopen_name(ida,nom)) >= 0) {
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
  return iret;
}
