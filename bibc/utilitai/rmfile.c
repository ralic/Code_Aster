/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF RMFILE UTILITAI  DATE 02/06/2006   AUTEUR MCOURTOI M.COURTOIS */
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
/* appel de la commande systeme de destruction de fichier */
/* rm ou del suivant les plates-formes                    */
 
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

extern int errno;

#if defined CRAY
#include <fortran.h>
void rmfile(_fcd nom1F)

#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
void rmfile_(char *nom1, unsigned long lnom1)

#elif defined HPUX
void rmfile (char *nom1, unsigned long lnom1)

#elif defined PPRO_NT
void __stdcall RMFILE(char *nom1 ,unsigned long lnom1)

#endif
{
   char nomcmd[85];char *ncmd;
   long i,l,ldeb,num;
   int ier;
#if defined CRAY
   char *nom1;unsigned long lnom1;
   nom1  = _fcdtocp(nom1F);
   lnom1 = _fcdlen(nom1F);
#endif
if (lnom1 > 80) { lnom1 = 80; }
#if defined PPRO_NT
   num = _flushall();
   ncmd = "del ";
   ldeb = 4;
#else
   num = fflush(stderr);
   num = fflush(stdout);
   ncmd = "rm ";
   ldeb = 3;
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

   fprintf(stdout,"\n\nLancement de la commande ->%s<-\n\n",nomcmd);
   ier=system(nomcmd);
   if ( ier == -1 ) {
        perror("\n<rmfile> code retour system");
   } 
#if defined PPRO_NT
   num = _flushall();
#else
   num = fflush(stderr);
   num = fflush(stdout);
#endif
}
