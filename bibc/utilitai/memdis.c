/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF MEMDIS UTILITAI  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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
/* effectue une allocation dynamique avec tests successifs par    */
/* pas de 0.25 mega mots (entier)                                 */
/* retourne dim zone effectivement disponible                     */
/* parametres                                                     */
/* L         : nombre de mots a allouer                           */
/* iadm      : adresse  de la zone allouee                        */
/* taille_mo : taille effectivement allouee en Mo                 */
/* lfree     :  =1 la zone est liberee                            */
/*             !=1 la zone est disponible a l'adresse iadm        */
#include <stdio.h>
#include <stdlib.h>
#ifdef CRAY
   long MEMDIS(long *L, long **iadm, long *taille_mo, long *lfree)
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
   long memdis_(long *L, long **iadm, long *taille_mo, long *lfree)
#elif defined HPUX
   long memdis(long *L, long **iadm, long *taille_mo, long *lfree)
#elif defined PPRO_NT
   long __stdcall MEMDIS(long *L, long **iadm, long *taille_mo, long *lfree)
#endif
{
long dim , delta ;
long *retour ;
dim = *L ;
delta = 262144 ;
do {
    retour = (long * ) malloc(dim*sizeof(long));
    dim  = dim - delta ;
    }
while ( (retour == NULL) && (dim > 0)) ;
dim=dim+delta;
if ( *lfree == 1 ) {
#ifdef CRAY
   cfree ( retour , dim , sizeof(long) ) ;
#elif defined SOLARIS || IRIX || PPRO_NT || HPUX || P_LINUX || TRU64 || SOLARIS64 
   free (retour );
#endif
   iadm = NULL;
   taille_mo = 0; 
   }
else {
   *iadm = retour;
   *taille_mo = (long) (dim*sizeof(long));
   }
return ( dim ) ;
}
