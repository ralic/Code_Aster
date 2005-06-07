/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF hdfcrf utilitai  DATE 06/09/2003   AUTEUR D6BHHJP J.P.LEFEBVRE */
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
/ Ouverture d'un fichier HDF, renvoie éventuellement une erreur  
/  Paramètres :
/   - in  nomfic : nom du fichier (char *)
/  Résultats :
/     identificateur du fichier, -1 sinon (hid_t = int)
/-----------------------------------------------------------------------------*/
#include <hdf5.h>

#if defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
   long hdfcrf_( char *nomfic, long ln )
#elif defined HPUX
   long hdfcrf ( char *nomfic, long ln )
#elif defined PPRO_NT
   extern long __stdcall hdfcrf(char *nomfic, unsigned long ln )
#endif
{
  hid_t idfic; 
  int k;
  long iret;
  char *nomf;
       
  nomf = (char *) malloc((ln+1) * sizeof(char));
  for (k=0;k<ln;k++) {
     nomf[k] = nomfic[k];
  }
  k=ln-1;
  while (nomf[k] == ' ') { k--;}
  nomf[k+1] = '\0';

  if ( (idfic = H5Fcreate(nomf, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
    iret= -1 ; 
  } else {iret = (long) idfic;}

  free (nomf);
  return iret;
}     
