/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF hdfcrg hdf  DATE 27/06/2005   AUTEUR D6BHHJP J.P.LEFEBVRE */
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
/ Création d'un groupe HDF, renvoie une erreur si le groupe ne peut être créé 
/  Paramètres :
/   - in idfile : identificateur du fichier (hid_t)
/   - in  nomgp : nom du groupe père (contient toute l'arborescence depuis "/")
/   - in  nomgr : nom du groupe (char *) à créer 
/  Résultats :
/     identificateur du groupe, -1 sinon (hid_t = int)
/-----------------------------------------------------------------------------*/
#include <hdf5.h>

#if defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
   long hdfcrg_ ( long *idf, char *nomgp, char *nomgr, long lp, long ln )
#elif defined HPUX
   long hdfcrg ( long *idf, char *nomgp, char *nomgr, long lp, long ln )
#elif defined PPRO_NT
   extern long __stdcall HDFCRG ( long *idf, char *nomgp, unsigned long lp, char *nomgr, unsigned long ln )
#endif
{
  hid_t  idgrp,idfic;     
  char *nomd;
  int k,lg2;
  long iret=-1;

  idfic=(hid_t) *idf;
  nomd = (char *) malloc((lp+ln+2) * sizeof(char));
  for (k=0;k<lp;k++) {
     nomd[k] = nomgp[k];
  }
  k=lp-1;
  while (nomd[k] == ' ' || nomd[k] == '/') { k--;}
  nomd[k+1] = '/';
  lg2=k+1+1;
  for (k=0;k<ln;k++) {
     nomd[lg2+k] = nomgr[k];
  }
  k=lg2+ln-1;
  while (nomd[k] == ' ') { k--;}
  nomd[k+1] = '\0';
 
  if ((idgrp = H5Gcreate(idfic, nomd, 0)) >= 0) 
    iret = (long) idgrp;
  free (nomd);
  return iret;
}     
