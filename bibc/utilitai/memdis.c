/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF MEMDIS UTILITAI  DATE 31/01/2011   AUTEUR COURTOIS M.COURTOIS */
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
#include "aster.h"

INTEGER DEFPPPP(MEMDIS, memdis, INTEGER *L, INTEGER **iadm, INTEGER *taille_mo, INTEGER *lfree)
{
   long dim , delta ;
   long *retour ;
   void *malloc(size_t size);
   dim = (long)*L ;
   delta = 262144 ;
   do {
      retour = (long * ) malloc(dim*sizeof(INTEGER));
      dim  = dim - delta ;
      }
   while ( (retour == NULL) && (dim > 0)) ;
   dim=dim+delta;
   if ( *lfree == 1 ) {
      free (retour );
      iadm = NULL;
      taille_mo = 0; 
      }
   else {
      *iadm = (INTEGER *)retour;
      *taille_mo = (INTEGER) (dim*sizeof(INTEGER));
      }
   return (INTEGER)( dim ) ;
}
