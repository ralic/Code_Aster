/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF IODR utilitai  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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
/* ------------------------------------------------------------------ */
#if defined CRAY
/* pour le moment, on appelle les routines fournies par le système 
   #ifdef CRAY
   #include <fortran.h>
   #define OFF_INIT 8
   #endif
*/
/* La variable OFF_INIT est utilisee pour indiquer la longueur du premier */
/* mot stocke en tete de fichier                                          */

#elif defined SOLARIS || PPRO_NT || HPUX || IRIX_32 || P_LINUX  
#define OFF_INIT 4
#elif defined IRIX_64 || TRU64 || SOLARIS64
#define OFF_INIT 8
#endif 
#if defined SOLARIS || PPRO_NT || HPUX || IRIX || P_LINUX || TRU64 || SOLARIS64 
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>

#define MAX_FAC 100

static FILE *fpfile[MAX_FAC];
static long   nenr[MAX_FAC];

static long nbFAC=0;
static char *nomFAC[MAX_FAC];


long ind_fac ( char *nom )
{
long i;
static long res;
res=-1;
i=0;
while ((i < nbFAC) && (strcmp(nom,nomFAC[i]) != 0)) i++;
if (i < nbFAC) res=i;

return(res);
}

long  open_fac ( char *nom )
{
long ifac;
static long res;

res=-1;
ifac=ind_fac(nom);
if (ifac < 0) {
   /* Creation */
   if (nbFAC < MAX_FAC) {
      nomFAC[nbFAC]=(char *) malloc(10);
      strcpy(nomFAC[nbFAC],nom);
      fpfile[nbFAC]=NULL;
      nenr[nbFAC]=-1;
      res=nbFAC;
      nbFAC++;
      }
   }
else {
   /* Reinitialisation */
   strcpy(nomFAC[ifac],nom);
   fpfile[ifac]=NULL;
   nenr[ifac]=-1;
   res=ifac;
   }
return(res);
}

void strcpyF2C (char *cname, char *fname)
{
char *p;
char *q;

p=fname;
q=cname;

while ((*p != '\0') && (*p != ' ')) {
   *q++=*p++;
   }
*q='\0';
}
#endif
#if defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
  void  opendr_ ( char *dfname,long *indx,long *lgti,long *indic,long *ierr,unsigned long len_dfname)
#elif defined HPUX
  void opendr ( char *dfname,long *indx,long *lgti,long *indic,long *ierr,unsigned long len_dfname)
#elif defined PPRO_NT
  void __stdcall OPENDR ( char *dfname,unsigned long len_dfname,long *indx,long *lgti,long *indic,long *ierr)
#endif
#if defined SOLARIS || PPRO_NT || HPUX || IRIX || P_LINUX || TRU64 || SOLARIS64 
{
    long iu,nbread;
    char fname[24];

    *ierr = 0;
    strcpyF2C(fname,dfname);
    iu=open_fac(fname);
    if ( iu < 0 )
    {
       *ierr = -1;
       return;
    }
    fpfile[iu] = fopen(fname,"rb+");
    if (fpfile[iu] == NULL  )
    {
       fpfile[iu] = fopen(fname,"wb+");
       if ( fpfile[iu] == NULL )
       {
           *ierr = -2;
       }
       else
       {
           nenr[iu] = -1;
           *ierr = 0;
       }
   }
    else
    {
           nbread=fread(&nenr[iu], OFF_INIT, 1, fpfile[iu]);
    }
}
#endif
#if defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
  void closdr_( char *dfname,long *ierr,unsigned long len_dfname)
#elif defined HPUX
  void closdr( char *dfname,long *ierr,unsigned long len_dfname)
#elif defined PPRO_NT
  void __stdcall CLOSDR( char *dfname,unsigned long len_dfname,long *ierr)
#endif
#if defined SOLARIS || PPRO_NT || HPUX || IRIX || P_LINUX || TRU64 || SOLARIS64 
{
    long iu;
    char fname[24];

    *ierr = 0;
    strcpyF2C(fname,dfname);
    iu=ind_fac(fname);
    if ( iu < 0 )
    {
       *ierr = -1;
    }
    else
    {
        fclose(fpfile[iu]);
        fpfile[iu] = NULL;
        nenr[iu]=-1;
        *ierr = 0;
    }
}
#endif
#if defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
  void readdr_( char *dfname,void *buf,long *nbytes,long *irec,long *ierr,unsigned long len_dfname)
#elif defined HPUX
  void readdr ( char *dfname,void *buf,long *nbytes,long *irec,long *ierr,unsigned long len_dfname)
#elif defined PPRO_NT
  void __stdcall READDR( char *dfname,unsigned long len_dfname,void *buf,long *nbytes,long *irec,long *ierr)
#endif
#if defined SOLARIS || PPRO_NT || HPUX || IRIX || P_LINUX || TRU64 || SOLARIS64 
{
    long offset;
    long iu,nbval,nbseek;
    char fname[24];

    *ierr = 0;
    strcpyF2C(fname,dfname);
    iu=ind_fac(fname);
    if ( iu < 0 )
    {
       *ierr = -1;
       return;
    }
    if ( nenr[iu] == -1 )
    {
        *ierr = -2;
        return;
    }
    if ( fpfile[iu] == NULL )
    {
        *ierr = -3;
        return;
    }
    offset = (*irec-1)*nenr[iu]+OFF_INIT;
    nbseek=fseek(fpfile[iu],offset, SEEK_SET);
    nbval=fread(buf,1,*nbytes,fpfile[iu]);
 if ( nbval != *nbytes ) {*ierr = -4;}

}
#endif
#if defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
  void writdr_( char *dfname,void *buf,long *nbytes,long *irec,long *indic,long *s,long *ierr,unsigned long len_dfname)
#elif defined HPUX
  void writdr( char *dfname,void *buf,long *nbytes,long *irec,long *indic,long *s,long *ierr,unsigned long len_dfname)
#elif defined PPRO_NT
  void __stdcall WRITDR( char *dfname,unsigned long len_dfname,void *buf,long *nbytes,long *irec,long *indic,long *s,long *ierr)
#endif
#if defined SOLARIS || PPRO_NT || HPUX || IRIX || P_LINUX || TRU64 || SOLARIS64 
{
    long offset;
    long iu,nbval,nbseek,nbwrite;
    char fname[24];


    *ierr = 0;
    strcpyF2C(fname,dfname);
    iu=ind_fac(fname);
    if ( iu < 0 )
    {
       *ierr = -1;
       return;
    }
    if ( fpfile[iu] == NULL )
    {
        *ierr = -3;
        return;
    }
    if ( nenr[iu] == -1 )
    {
       nenr[iu] = *nbytes;
       nbwrite=fwrite(&nenr[iu], OFF_INIT, 1, fpfile[iu]);
    }
    offset = (*irec-1)*(nenr[iu])+OFF_INIT;
    nbseek=fseek(fpfile[iu],offset, SEEK_SET);
    nbval=fwrite(buf,1,*nbytes,fpfile[iu]);

    if ( nbval != *nbytes ) {*ierr = -4;}
}
#endif
