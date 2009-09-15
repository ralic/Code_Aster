/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF MUMMPI UTILITAI  DATE 14/09/2009   AUTEUR DESOZA T.DESOZA */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2007  EDF R&D              WWW.CODE-ASTER.ORG */
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

void DEFPPPSPP(MUMMPI, mummpi,
                   INTEGER *optmpi, INTEGER *ifm, INTEGER *niv, char *ach24, STRING_SIZE lach124,
                   INTEGER *argi1, INTEGER *argi2)

{
   char *ach24Null;
   ach24Null = (char *)malloc((lach124+1)*sizeof(char));
   strncpy(ach24Null, ach24, lach124);
   ach24Null[lach124]='\0';

#if defined _USE_MPI_MUMPS
   void DEFPPPSPP(MUMAM, mumam, INTEGER *, INTEGER *, INTEGER *, char *, STRING_SIZE,
                  INTEGER *, INTEGER *);
   #define CALL_MUMAM(a,b,c,d,e,f) CALLPPPSPP(MUMAM,mumam,a,b,c,d,e,f)

   CALL_MUMAM(optmpi, ifm, niv, ach24Null, argi1, argi2);

#else

   void DEFPPPSPP(MUMSM, mumsm, INTEGER *, INTEGER *, INTEGER *, char *, STRING_SIZE, 
                  INTEGER *, INTEGER *);
   #define CALL_MUMSM(a,b,c,d,e,f) CALLPPPSPP(MUMSM,mumsm,a,b,c,d,e,f)

   CALL_MUMSM(optmpi, ifm, niv, ach24Null, argi1, argi2);
#endif

   free(ach24Null);
}
