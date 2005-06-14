/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF hdfnbo utilitai  DATE 06/09/2003   AUTEUR D6BHHJP J.P.LEFEBVRE */
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
/ Récupération du nombre de datasets et de groups contenus dans un groupe 
/ au sein d'un fichier HDF 
/  Paramètres :
/   - in  idfic : identificateur du fichier (hid_t)
/   - in  nomgr : nom du groupe (char *)
/  Résultats :
/     nombre de datasets et de groups
/-----------------------------------------------------------------------------*/
#include "hdf5.h"

#if defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
   long hdfnbo_(long *idf, char *nomgr, long ln )
#elif defined HPUX
   long hdfnbo(long *idf, char *nomgr, long ln )
#elif defined PPRO_NT
   extern long __stdcall HDFNBO(long *idf, char *nomgr, unsigned long ln  )
#endif
{
  hid_t idfic;
  char *nomg;
  int k,idx ;
  long nbobj=0;
  herr_t indiceNbName(hid_t loc_id, const char *name, void *opdata);

  idfic=(hid_t) *idf;

  nomg = (char *) malloc((ln+1) * sizeof(char));
  for (k=0;k<ln;k++) {
     nomg[k] = nomgr[k];
  }
  k=ln-1;
  while (nomg[k] == ' ') { k--;}
  nomg[k+1] = '\0';

  idx = H5Giterate(idfic, nomg, NULL, indiceNbName, &nbobj);
  free(nomg);
  return nbobj; 
   
}

herr_t indiceNbName(hid_t id,const char *nom, void *donnees)
{
  int *compteur;

  compteur = (int *) donnees;
  (*compteur)++;
  return 0;
}

