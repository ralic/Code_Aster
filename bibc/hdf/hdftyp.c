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
/ Récupération du type associé (dataset,group,etc) de chaque entité d'une liste 
/ de noms d'un groupe donné au sein d'un fichier HDF 
/  Paramètres :
/   - in  idfic : identificateur du fichier (hid_t)
/   - in  nomgr : identificateur du fichier (hid_t)
/   - in  lnom :  liste de noms (char *)
/   - out ltype : liste des types (char *)
/  Résultats :
/     nombre de datasets et de groups
/-----------------------------------------------------------------------------*/
#ifndef _DISABLE_HDF5
#include <hdf5.h>
#endif
#define FALSE   0

INTEGER DEFPSPS(HDFTYP, hdftyp, INTEGER *idf, char *nomgr, STRING_SIZE ln,
                                INTEGER *nbnom, char *typ, STRING_SIZE ltp)
{
#ifndef _DISABLE_HDF5
  hid_t idfic, bidon=0;
  herr_t indx;
  hsize_t ind;
  char *nomg, *pt, *ptype; 
  int j;
  int k, ll;
  void *malloc(size_t size);
  
  herr_t indiceType(hid_t loc_id, const char *name, const H5L_info_t *info, void *opdata);

  idfic=(hid_t) *idf;

  nomg = (char *) malloc((ln+1) * sizeof(char));
  for (k=0;k<ln;k++)  
     nomg[k] = nomgr[k];
  k=ln-1;
  while (nomg[k] == ' ') { k--;}
  nomg[k+1] = '\0';

  ptype=(char *) malloc((ltp+1) * sizeof(char));
  pt=typ;
  for (j=0;j<*nbnom;j++) {
    ind=(hsize_t)j;
    indx = H5Literate_by_name(idfic, nomg , H5_INDEX_NAME, H5_ITER_NATIVE, &ind,
                              indiceType, ptype, bidon);
    ll=strlen(ptype);
    for (k=0;k<ll;k++)
      *(pt+k) = *(ptype+k);
    for (k=ll;k<ltp;k++)
      *(pt+k) = ' ';
    pt=pt+ltp;
  }
  free(nomg);
  free(ptype);
#else
  CALL_U2MESS("F", "FERMETUR_3");
#endif
  return 0; 
}

/*  
    http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-Visit
    
    The protoype of the callback function op is as follows (as defined in the source
    code file H5Lpublic.h):
    herr_t (*H5L_iterate_t)( hid_t g_id, const char *name, const H5L_info_t *info, void *op_data)

    The parameters of this callback function have the following values or meanings:
            g_id    Group that serves as root of the iteration; same value as the H5Lvisit
                    group_id parameter
            name    Name of link, relative to g_id, being examined at current step of the iteration
            info    H5L_info_t struct containing information regarding that link
            op_data User-defined pointer to data required by the application in processing
                    the link; a pass-through of the op_data pointer provided with the H5Lvisit
                    function call

    The possible return values from the callback function, and the effect of each, are as follows:

        * Zero causes the visit iterator to continue, returning zero when all group members have
          been processed.
        * A positive value causes the visit iterator to immediately return that positive value,
          indicating short-circuit success. The iterator can be restarted at the next group member.
        * A negative value causes the visit iterator to immediately return that value, indicating
          failure. The iterator can be restarted at the next group member. 
*/

#ifndef _DISABLE_HDF5
herr_t indiceType(hid_t id, const char *nom, const H5L_info_t *info, void *donnees)
{
  herr_t status;
  H5O_info_t infobuf;
  char *retour;
  int iret;

  retour =(char *)donnees;
  status = H5Oget_info_by_name (id, nom, &infobuf, H5P_DEFAULT);

  switch (infobuf.type) {
  case H5O_TYPE_GROUP: 
     strcpy(retour,"group");
     break;
  case H5O_TYPE_DATASET: 
     strcpy(retour,"dataset");
     break;
  case H5O_TYPE_NAMED_DATATYPE: 
     strcpy(retour,"datatype");
     break;
  default:
     strcpy(retour,"unknown");
  }
  return 1;
}
#endif
