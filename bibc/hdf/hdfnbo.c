/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF hdfnbo hdf  DATE 02/02/2011   AUTEUR COURTOIS M.COURTOIS */
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
/ Récupération du nombre de datasets et de groups contenus dans un groupe 
/ au sein d'un fichier HDF 
/  Paramètres :
/   - in  idfic : identificateur du fichier (hid_t)
/   - in  nomgr : nom du groupe (char *)
/  Résultats :
/     nombre de datasets et de groups
/-----------------------------------------------------------------------------*/
#ifndef _DISABLE_HDF5
#include <hdf5.h>
#endif

INTEGER DEFPS(HDFNBO, hdfnbo, INTEGER *idf, char *nomgr, STRING_SIZE ln)
{
  INTEGER nbobj=0;
#ifndef _DISABLE_HDF5
  hid_t idfic;
  char *nomg;
  int k;
  int idx ;
  void *malloc(size_t size);
  
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
#else
  CALL_U2MESS("F", "FERMETUR_3");
#endif
  return nbobj; 
}

#ifndef _DISABLE_HDF5
herr_t indiceNbName(hid_t id,const char *nom, void *donnees)
{
  int *compteur;

  compteur = (int *) donnees;
  (*compteur)++;
  return 0;
}
#endif
