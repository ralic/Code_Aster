/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF hdfopd hdf  DATE 08/11/2005   AUTEUR D6BHHJP J.P.LEFEBVRE */
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
/ Ouverture d'un dataset HDF, renvoie éventuellement une erreur  
/  Paramètres :
/   - in  idfic : identificateur du fichier hdf (hid_t = int)
/   - in  nomg : nom du groupe (char *)
/   - in  nomd : nom du dataset (char *)
/  Résultats :
/     identificateur du dataset, -1 sinon (hid_t = int)
/-----------------------------------------------------------------------------*/
#include <hdf5.h>

#if defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
   long hdfopd_( long *idf, char *nomg, char *nomd, long lg, long ln )
#elif defined HPUX
   long hdfopd ( long *idf, char *nomg, char *nomd, long lg, long ln )
#elif defined PPRO_NT
   extern long __stdcall hdfopd( long *idf, char *nomg, unsigned long lg, char *nomd, unsigned long ln )
#endif
{
  hid_t id,idfic; 
  int k,lg2;
  long iret=-1;
  char *nom;
 
  idfic=(hid_t) *idf;
  nom = (char *) malloc((lg+ln+2) * sizeof(char));
  for (k=0;k<lg;k++) {
     nom[k] = nomg[k];
  }
  k=lg-1;
  while (nom[k] == ' ' || nom[k] == '/') { k--;}
  nom[k+1] = '/';
  lg2=k+2;
  for (k=0;k<ln;k++) {
     nom[lg2+k] = nomd[k];
  }
  k=lg2+ln-1;
  while (nom[k] == ' ') { k--;}
  nom[k+1] = '\0';

  if ( (id = H5Dopen(idfic,nom)) >= 0) 
    iret = (long) id;

  free (nom);
  return iret;
}     
