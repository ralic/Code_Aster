/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF CPFILE UTILITAI  DATE 30/11/2004   AUTEUR D6BHHHH M.ASA */
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
/* appel de la commande systeme de copie de fichier */
/* cp ou copy suivant les plates-formes             */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
 
extern int errno;

#if defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
void cpfile_(char *action, char *nom1, char *nom2, unsigned long la, unsigned long lnom1, unsigned long lnom2)

#elif defined HPUX
void cpfile(char *action, char *nom1, char *nom2, unsigned long la, unsigned long lnom1, unsigned long lnom2)

#elif defined PPRO_NT
void __stdcall CPFILE(char *action, unsigned long la, char *nom1, unsigned long lnom1,char *nom2 ,unsigned long lnom2)

#endif
{
   char nomcmd[165];char *ncmd;
   long i,l,ldeb,num;
   int ier;
#if defined PPRO_NT
   num = _flushall();
   ldeb = 5;
   if ( *action == 'C' ) {ncmd = "copy ";}
   else if ( *action == 'M' ) {ncmd = "move ";}
   else {ncmd = " ? ";ldeb = 3;}
#else
   num = fflush(stderr);
   num = fflush(stdout);
   ldeb = 3;
   if ( *action == 'C' ) {ncmd = "cp ";}
   else if ( *action == 'M' ) {ncmd = "mv ";}
   else {ncmd = " ? ";ldeb = 3;}
#endif
   if (lnom1 > 80) { lnom1 = 80; }
   if (lnom2 > 80) { lnom2 = 80; }
   for (i=0;i<ldeb;i++) {nomcmd[i]=ncmd[i];}
   l    = (long) lnom1;
   ncmd = nom1;
   if (l != 0) {
     for (i=0;i<l;i++) {nomcmd[i+ldeb]=ncmd[i];}
     i=l-1;
     while (ncmd[i] == ' ') {i--;}
     nomcmd[i+ldeb+1] =' ';
     ldeb = ldeb+i+1;
   } else {
     i=0;
     while (ncmd[i] != ' ') { nomcmd[i+ldeb] = ncmd[i];i++;}
     nomcmd[i+ldeb] =' ';
     ldeb = ldeb+i-1;
   }
   nomcmd[ldeb+1]= ' ';
   ldeb = ldeb+1;
   l    = (long) lnom2;
   ncmd = nom2;
   if (l != 0) {
     for (i=0;i<l;i++) {nomcmd[i+ldeb]=ncmd[i];}
     i=l-1;
     while (ncmd[i] == ' ') {i--;}
     nomcmd[i+ldeb+1] ='\0';
   } else {
     i=0;
     while (ncmd[i] != ' ') { nomcmd[i+ldeb] = ncmd[i];i++;}
     nomcmd[i+ldeb] ='\0';
   }

   fprintf(stdout,"\n\nLancement de la commande ->%s<-\n\n",nomcmd);
   ier=system(nomcmd);
   if ( ier == -1 ) {
        perror("\n<cpfile> code retour system");
   } 
#if defined PPRO_NT
   num = _flushall();
#else
   num = fflush(stderr);
   num = fflush(stdout);
#endif

}
