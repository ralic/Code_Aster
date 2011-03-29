/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF hdfcrg hdf  DATE 29/03/2011   AUTEUR COURTOIS M.COURTOIS */
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
#include <stdlib.h>
#include "aster.h"
#include "aster_fort.h"
/*-----------------------------------------------------------------------------/
/ Création d'un groupe HDF, renvoie une erreur si le groupe ne peut être créé 
/  Paramètres :
/   - in idfile : identificateur du fichier (hid_t)
/   - in  nomgp : nom du groupe père (contient toute l'arborescence depuis "/")
/   - in  nomgr : nom du groupe (char *) à créer 
/  Résultats :
/     identificateur du groupe, -1 sinon (hid_t = int)
/-----------------------------------------------------------------------------*/
#ifndef _DISABLE_HDF5
#include <hdf5.h>
#endif

INTEGER DEFPSS(HDFCRG, hdfcrg, INTEGER *idf, char *nomgp, STRING_SIZE lp, char *nomgr, STRING_SIZE ln)
{
  INTEGER iret=-1;
#ifndef _DISABLE_HDF5
  hid_t  idgrp,idfic;     
  char *nomd;
  int k,lg2;
  void *malloc(size_t size);
    
  idfic=(hid_t) *idf;
  nomd = (char *) malloc((lp+ln+2) * sizeof(char));
  for (k=0;k<lp;k++) {
     nomd[k] = nomgp[k];
  }
  k=lp-1;
  while (k>=0){
     if(nomd[k] == ' ' || nomd[k] == '/') { k--;}
     else break;
  }
  nomd[k+1] = '/';
  lg2=k+1+1;
  for (k=0;k<ln;k++) {
     nomd[lg2+k] = nomgr[k];
  }
  k=lg2+ln-1;
  while (k>=0){
    if (nomd[k] == ' ') { k--;}
    else break;
  }
  nomd[k+1] = '\0';
 
  if ((idgrp = H5Gcreate(idfic, nomd, 0)) >= 0) 
    iret = (INTEGER) idgrp;
  free (nomd);
#else
  CALL_U2MESS("F", "FERMETUR_3");
#endif
  return iret;
}     
