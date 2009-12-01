/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF memjob utilitai  DATE 07/04/2009   AUTEUR COURTOIS M.COURTOIS */
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
#include <stdio.h>
#include "aster.h"

#ifdef _POSIX
#include <sys/resource.h>
#include <sys/time.h>
#endif

extern char g_memory[];

INTEGER STDCALL(MEMJOB, memjob)()
/* Renvoie, en Mmots, la memoire definie pour le job */
{
   int imem;
   if ( strlen(g_memory) != '\0' ) {
        sscanf(g_memory,"%d",&imem);
        return ((INTEGER) imem );
   } 
   else {
        return( (INTEGER)16 );
   }
}
