/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF STRMOV UTILITAI  DATE 02/06/2006   AUTEUR MCOURTOI M.COURTOIS */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2001  EDF R&D              WWW.CODE-ASTER.ORG */
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
#include <string.h>
#if defined CRAY
/* on appelle la version systeme sur CRAY */
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
  void strmov_(char *src, long *isb,long *num,char *dest, long *idb)
#elif defined HPUX
  void strmov(char *src, long *isb,long *num,char *dest, long *idb)
#elif defined PPRO_NT
  void __stdcall STRMOV(char *src,unsigned long len_src, long *isb,long *num,char *dest, long *idb)
#endif
#if defined SOLARIS || PPRO_NT || HPUX || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
{
    memcpy(dest+*idb-1,src+*isb-1,*num);
}
#endif
