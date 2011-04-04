/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF APLEXT UTILITAI  DATE 04/04/2011   AUTEUR COURTOIS M.COURTOIS */
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
/* Creation d'un process fils, appel d'un programme externe
   et retour au process initial */


#include "aster.h"


#ifdef _POSIX
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>

#elif defined(_WINDOWS)
#include <process.h>

#endif


void DEFPPSP(APLEXT, aplext, INTEGER *niv, INTEGER *nbd ,char *nom ,STRING_SIZE lnom, INTEGER *ier)
{
   char *args[100];
   char nomcmd[256+1];
   char *ncmd,*msg;
   long i,k,l;
#ifdef _POSIX
   pid_t pid;
#else
   long ipid,num;
#endif
   *ier = 0;
   if (*nbd > 100){
      fprintf(stderr,"\nLe nombre d'arguments d'appel (%ld) est superieur a 99\n",*nbd);
      fprintf(stdout,"\nLe nombre d'arguments d'appel (%ld) est superieur a 99\n",*nbd);
      *ier = 1;
#ifdef _POSIX
      fflush(stderr);
      fflush(stdout);
#else
      num = _flushall();
#endif
      return;
   }
   /*
      Initialisation des pointeurs sur les arguments d'appel
   */
   for (k=1;k<100;k++) {
      args[k] = NULL;
   }
   /*
      Construction du nom de la commande ou du programme externe à appeler
   */
   l    = (long) lnom;
   ncmd = nom;
   if (l != 0) {
     for (i=0;i<l;i++) {
       nomcmd[i]=ncmd[i];
     }
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

#ifdef _POSIX
   fflush(stderr);
   fflush(stdout);
   if ( (pid=fork()) < 0 ) {
     *ier=1;
     msg=(char *)strerror(errno);
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
                msg=(char *)strerror(errno);
                fprintf(stdout,"\n%s\n",msg);
                fprintf(stderr,"\n%s\n",msg);
        }
       else {
/*
   Examen du code retour avec détection des signaux
*/
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
#else
   num = _flushall();
   ipid = _spawnv( _P_WAIT, nomcmd , args );
   perror("\ncode retour spawnv");
#endif

   if (*niv > 0){
      fprintf(stdout,"\nRetour au Code_Aster \n\n");
   }

#ifdef _POSIX
   fflush(stderr);
   fflush(stdout);
#else
   num = _flushall();
#endif

}

