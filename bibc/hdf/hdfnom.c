/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF hdfnom hdf  DATE 13/10/2009   AUTEUR COURTOIS M.COURTOIS */
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
#include <stdlib.h>
#include "aster.h"
/*-----------------------------------------------------------------------------/
/ Récupération des noms (dataset,group) de chaque entité contenu dans
/ d'un groupe donné au sein d'un fichier HDF 
/  Paramètres :
/   - in  idfic : identificateur du fichier (hid_t)
/   - in  nomgr : identificateur du fichier (hid_t)
/  Résultats :
/     nombre de datasets et de groups
/-----------------------------------------------------------------------------*/
#include "hdf5.h"

INTEGER DEFPSS(HDFNOM, hdfnom, INTEGER *idf, char *nomgr, STRING_SIZE ln, char *nom, STRING_SIZE lnm)
{
  hid_t idfic;
  char *nomg, *pnomdts, *pnom;
  int k, ll;
  int l,ind,indx;
  long nbobj=0;
  void *malloc(size_t size);
  
  herr_t indiceName(hid_t loc_id, const char *name, void *opdata);
  herr_t indiceNbName(hid_t loc_id, const char *name, void *opdata);

  idfic=(hid_t) *idf;
  nomg = (char *) malloc((ln+1) * sizeof(char));
  for (k=0;k<ln;k++) {
     nomg[k] = nomgr[k];
  }
  k=ln-1;
  while (nomg[k] == ' ') { k--;}
  nomg[k+1] = '\0';
  indx = H5Giterate(idfic, nomg, NULL, indiceNbName, &nbobj);
  
  pnomdts = (char *) malloc((lnm+1) * sizeof(char));
  pnom=nom;
  for (k=0;k<nbobj;k++) {
    ind =k;
    indx = H5Giterate(idfic, nomg, &ind, indiceName, pnomdts);
    ll=strlen(pnomdts);
    for (l=0;l<(int)ll;l++)
      *(pnom+l) = *(pnomdts+l);
    for (l=ll;l<(int)lnm;l++)
      *(pnom+l) = ' ';
    pnom=pnom+lnm;
  }
  free(nomg);
  free(pnomdts);
  return nbobj; 
}

herr_t indiceName(hid_t id, const char *nom, void *donnees)
{
  char *p;

  p=(char *) donnees;
  strcpy(p,nom);
  return 1;
}
