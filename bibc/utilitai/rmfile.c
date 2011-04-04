/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF RMFILE UTILITAI  DATE 04/04/2011   AUTEUR COURTOIS M.COURTOIS */
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
/* appel de la commande systeme de destruction de fichier */
/* rm ou del suivant les plates-formes                    */
/* si info  = 1 mode bavard                               */
/* si info != 1 mode silencieux                           */
 

#include "aster.h"


void DEFSP(RMFILE, rmfile, char *nom1, STRING_SIZE lnom1, INTEGER *info)
{
   char nomcmd[85];char *ncmd;
   long i,l,ldeb,num;
   int ier;

   if (lnom1 > 80) { lnom1 = 80; }
#if defined _POSIX
   num = fflush(stderr);
   num = fflush(stdout);
   ncmd = "rm ";
   ldeb = 3;
#else
   num = _flushall();
   ncmd = "del ";
   ldeb = 4;
#endif
   for (i=0;i<ldeb;i++) {nomcmd[i]=ncmd[i];}
   l    = (long) lnom1;
   ncmd = nom1;
   if (l != 0) {
     for (i=0;i<l;i++) {nomcmd[i+ldeb]=ncmd[i];}
     i=l-1;
     while (ncmd[i] == ' ') {i--;}
     nomcmd[i+ldeb+1] ='\0';
     ldeb = ldeb+i+1;
   } else {
     i=0;
     while (ncmd[i] != ' ') { nomcmd[i+ldeb] = ncmd[i];i++;}
     nomcmd[i+ldeb] ='\0';
     ldeb = ldeb+i-1;
   }
   if ( *info == 1 ) { 
   fprintf(stdout,"\n\nLancement de la commande ->%s<-\n\n",nomcmd);
                     }
   ier=system(nomcmd);
   if ( ier == -1 ) {
        perror("\n<rmfile> code retour system");
   } 
#if defined _POSIX
   num = fflush(stderr);
   num = fflush(stdout);
#else
   num = _flushall();
#endif
}
