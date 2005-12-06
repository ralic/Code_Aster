/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF hdfwat hdf  DATE 08/11/2005   AUTEUR D6BHHJP J.P.LEFEBVRE */
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
/ Ecrire un attribut de type chaine de caractères associé à un dataset 
/ au sein d'un fichier HDF 
/  Paramètres :
/   - in  iddat : identificateur du dataset (hid_t)
/   - in  nomat : nom de l'attribut (char *)
/   - in  nbv   : nombre de valeurs 
/   - in  valat : valeur de l'attribut (char *)
/  Résultats :
/     =0 OK, =-1 problème 
/-----------------------------------------------------------------------------*/
#include "hdf5.h"

#if defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
   long hdfwat_(long *iddat, char *nomat, long *nbv, char *valat, long ln, long lv )
#elif defined HPUX
   long hdfwat(long *iddat, char *nomat, long *nbv, char *valat, long ln, long lv )
#elif defined PPRO_NT
   extern long __stdcall HDFWAT(long *iddat, char *nomat, unsigned long ln, long *nbv, char *valat,  unsigned long lv )
#endif
{
  hid_t ida,aid,aty,att;  
  herr_t ret,retc; 
  hsize_t dimsf[1];
  int k,rank=1;
  long iret=-1;
  char *nom;
     
  ida=(hid_t) *iddat;
  dimsf[0]=*nbv;
  nom = (char *) malloc((ln+1) * sizeof(char));
  for (k=0;k<ln;k++) {
     nom[k] = nomat[k];
  }
  k=ln-1;
  while (nom[k] == ' ') { k--;}
  nom[k+1] = '\0';
  if ( (aid = H5Screate(H5S_SIMPLE)) >= 0) {  
    H5Sset_extent_simple( aid, rank, dimsf, NULL );
    if ( (aty = H5Tcopy(H5T_FORTRAN_S1)) >= 0) {
                H5Tset_size(aty, lv);
      if ( (att = H5Acreate(ida, nom, aty, aid, H5P_DEFAULT)) >= 0) {
        if ( (ret = H5Awrite(att, aty, valat)) >= 0) { 
          if ( (retc = H5Sclose(aid)) >= 0)  iret = 0;
        } 
      } 
    } 
  } 
  free(nom);
  return iret;
}
