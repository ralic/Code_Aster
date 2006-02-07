/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF hdfrsv hdf  DATE 06/02/2006   AUTEUR ASSIRE A.ASSIRE */
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
/ Lecture sur un fichier HDF d'un segment de valeur associé à un objet JEVEUX
/  Paramètres : 
/   - in  idfic  : identificateur du dataset (hid_t)
/   - out  sv    : valeurs associées 
/   - in  lsv    : nombre de valeurs 
/   - in  icv    : active ou non la conversion Integer*8/integer*4
/  Résultats :
/     =0 OK, -1 sinon 
/-----------------------------------------------------------------------------*/
#include <hdf5.h>

#if defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
   long hdfrsv_( long *idat, long *lsv, void *sv, long *icv)
#elif defined HPUX
   long hdfrsv ( long *idat, long *lsv, void *sv, long *icv)
#elif defined PPRO_NT
   extern long __stdcall HDFRSV(long *idat, long *lsv, char *sv, unsigned long toto, long *icv)
#endif
{
  hid_t ida,datatype,dasp,bidon=0;
  herr_t ier;
  hsize_t dims[1];
  long iret=-1;
  int rank,status;

  ida = (hid_t) *idat;
  rank = 1;
  if ((datatype = H5Dget_type(ida))>=0 ) {     
    if ((dasp = H5Dget_space(ida))>=0 ) { 
      if ((rank = H5Sget_simple_extent_ndims(dasp))==1) {
        status = H5Sget_simple_extent_dims(dasp, dims, NULL);
      }
      if (*lsv >= (long) dims[0]) {
        if ((ier = H5Dread(ida, datatype, H5S_ALL, H5S_ALL, H5P_DEFAULT, sv))>=0 ) {
          if ( H5Tequal(H5T_STD_I32LE,datatype)>0 || H5Tequal(H5T_STD_I64LE,datatype)>0  ||
               H5Tequal(H5T_STD_I32BE,datatype)>0 || H5Tequal(H5T_STD_I64BE,datatype)>0 ) {
            if (*icv != 0) { 
	      if ((H5Tconvert(datatype,H5T_NATIVE_LONG,*lsv,sv,NULL,bidon)) >= 0) {
                iret = 0;
              }
            } else { iret = 0; }
          } else { iret = 0; }
          H5Tclose(datatype);
        }
      }   
      H5Sclose(dasp);
    }
  }
  return iret ; 
}
