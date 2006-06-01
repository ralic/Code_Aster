/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF APLEXT UTILITAI  DATE 02/06/2006   AUTEUR MCOURTOI M.COURTOIS */
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
/* Creation d'un process fils, appel d'un programme externe
   et retour au process initial */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

 extern int errno;

#if defined CRAY || SOLARIS || HPUX || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>
#endif

#if defined CRAY
#include <fortran.h>
void APLEXT(long *niv,long *nbd, _fcd nomF, long *ier)

#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
void aplext_(long *niv,long *nbd, char *nom, long *ier, unsigned long lnom)

#elif defined HPUX
void aplext(long *niv,long *nbd, char *nom, long *ier, unsigned long lnom)

#elif defined PPRO_NT
#include <process.h>
void __stdcall APLEXT(long *niv,long *nbd ,char *nom ,unsigned long lnom, long *ier)

#endif
{
   char *args[100];char nomcmd[81];char *ncmd,*msg;
   long i,k,num,ipid,l;
#ifndef PPRO_NT
   pid_t pid;
#endif
#if defined CRAY
   char *nom;unsigned long lnom;
   nom  = _fcdtocp(nomF);
   lnom = _fcdlen(nomF);
#endif
   *ier = 0;
   if (*nbd > 100){
   fprintf(stderr,"\nLe nombre d'arguments d'appel (%ld) est supérieur à 99\n",*nbd);
   fprintf(stdout,"\nLe nombre d'arguments d'appel (%ld) est supérieur à 99\n",*nbd);
   *ier = 1;
#if defined PPRO_NT
      num = _flushall();
#else
      fflush(stderr);
      fflush(stdout);
#endif
   return;
   }
/*
   Initialisation des pointeurs sur les arguments d'appel
*/
   for (k=1;k<100;k++) {args[k] = NULL;}
/*
   Construction du nom de la commande ou du programme externe à appeler
*/
   l    = (long) lnom;
   ncmd = nom;
   if (l != 0) {
     for (i=0;i<l;i++) {nomcmd[i]=ncmd[i];}
     i=l-1;
     while (ncmd[i] == ' ') {i--;}
     nomcmd[i+1] ='\0';
   } else {
     i=0;
     while (ncmd[i] != ' ') { nomcmd[i] = ncmd[i];i++;}
     nomcmd[i] ='\0';
   }

   if (*niv > 0){fprintf(stdout,"\n\nLancement de la commande ->%s<-\n",nomcmd);}
/*
   Recopie des arguments d'appel
*/
   args[0] = nomcmd;
   for (k=1;k<*nbd;k++) {
        ncmd = ncmd+lnom;
        i=lnom-1;
        while (ncmd[i] == ' ') {i--;}
        ncmd[i+1] ='\0';
        args[k] = ncmd;
   }

   args[*nbd+1] = NULL;

#ifndef PPRO_NT
   fflush(stderr);
   fflush(stdout);
   if ( (pid=fork()) < 0 ) {
     *ier=1;
     msg=strerror(errno);
     fprintf(stdout,"\n%s\n",msg);
     fprintf(stderr,"\n%s\n",msg);
   }
   else {
     if (pid == 0) {
/*
   Appel de l'application externe
*/
       execv(nomcmd,args);
       perror("\ncode retour execv");
       _exit(127);
     }
     else {
       int errnoSTAT;
       pid_t pidr;
/*
   Attente de la fin de l'execution de la commande

*/     do {errno = 0; 
          pidr=wait(&errnoSTAT);
       } while (errno==EINTR) ;
       if (pidr == -1) {
                perror("wait"); 
                *ier=1;
                msg=strerror(errno);
                fprintf(stdout,"\n%s\n",msg);
                fprintf(stderr,"\n%s\n",msg);
		}
       else {
/*
   Examen du code retour avec détection des signaux
*/
         long code;

         if (WIFEXITED(errnoSTAT)) {
            *ier=WEXITSTATUS(errnoSTAT);
            if (*niv > 0){fprintf(stderr,"Fin du processus avec code retour: %d\n",WEXITSTATUS(errnoSTAT));}
            if (*niv > 0){fprintf(stdout,"Fin du processus avec code retour: %d\n",WEXITSTATUS(errnoSTAT));}
            }
         else if (WIFSIGNALED(errnoSTAT)) {
            *ier=1;
            fprintf(stderr,"Fin du processus par signal : %d :",WTERMSIG(errnoSTAT));
            fprintf(stdout,"Fin du processus par signal : %d :",WTERMSIG(errnoSTAT));
            switch (WTERMSIG(errnoSTAT)) {
#endif
#ifdef CRAY
               case SIGORE :
                  fprintf(stderr,"operand range error");
                  fprintf(stdout,"operand range error");
                  break;
               case SIGCPULIM  :
                  fprintf(stderr,"cpu limit exceeded");
                  fprintf(stdout,"cpu limit exceeded");
                  break;
#endif
#if defined SOLARIS || HPUX || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
               case SIGXCPU  :
                  fprintf(stderr,"cpu limit exceeded");
                  fprintf(stdout,"cpu limit exceeded");
                  break;

               case SIGILL :
                  fprintf(stderr,"illegal instruction ");
                  fprintf(stdout,"illegal instruction ");
                  break;
               case SIGABRT :
                  fprintf(stderr,"abort");
                  fprintf(stdout,"abort");
                  break;
               case SIGFPE :
                  fprintf(stderr,"floating point exception");
                  fprintf(stdout,"floating point exception");
                  break;
#endif
#ifndef PPRO_NT
               }
            fprintf(stderr,"\n");
            fprintf(stdout,"\n");
            }
         else  {
            fprintf(stderr,"Raison inconnue\n");
            }
         }
      }
   }
#endif
#if defined PPRO_NT
   num = _flushall();
   ipid = _spawnv( _P_WAIT, nomcmd , args );
   perror("\ncode retour spawnv");
#endif

if (*niv > 0){fprintf(stdout,"\nRetour au Code_Aster \n\n");}

#if defined PPRO_NT
   num = _flushall();
#else
   fflush(stderr);
   fflush(stdout);
#endif

}

