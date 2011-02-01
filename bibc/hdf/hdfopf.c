/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF hdfopf hdf  DATE 31/01/2011   AUTEUR COURTOIS M.COURTOIS */
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
/ Ouverture d'un fichier HDF, renvoie éventuellement une erreur  
/  Paramètres :
/   - in  nomfic : nom du fichier (char *)
/  Résultats :
/     identificateur du fichier, -1 sinon (hid_t = int)
/-----------------------------------------------------------------------------*/
#ifndef _DISABLE_HDF5
#include <hdf5.h>
#endif

INTEGER DEFS(HDFOPF, hdfopf, char *nomfic, STRING_SIZE ln)
{
  INTEGER iret=-1;
#ifndef _DISABLE_HDF5
  hid_t idfic; 
  int k;
  char *nomf;
  void *malloc(size_t size);
  
  nomf = (char *) malloc((ln+1) * sizeof(char));
  for (k=0;k<ln;k++) {
     nomf[k] = nomfic[k];
  }
  k=ln-1;
  while (nomf[k] == ' ') { k--;}
  nomf[k+1] = '\0';
  if ( (idfic = H5Fopen(nomf, H5F_ACC_RDONLY, H5P_DEFAULT)) >= 0) 
    iret = (INTEGER) idfic;
  free (nomf);
#else
  CALL_U2MESS("F", "FERMETUR_3");
#endif
  return iret;
}     
