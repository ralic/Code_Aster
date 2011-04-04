/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF IODR utilitai  DATE 04/04/2011   AUTEUR COURTOIS M.COURTOIS */
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

#include "aster.h"

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
   void *malloc(size_t size);
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


void DEFSPPPP(OPENDR, opendr, char *dfname, STRING_SIZE len_dfname, INTEGER *indx, INTEGER *lgti,
                         INTEGER *indic, INTEGER *ierr)
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

void DEFSP(CLOSDR, closdr, char *dfname, STRING_SIZE len_dfname, INTEGER *ierr)
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

void DEFSPPPP(READDR, readdr, char *dfname, STRING_SIZE len_dfname, void *buf,
                              INTEGER *nbytes, INTEGER *irec, INTEGER *ierr)
{
    long offset;
    long iu,nbseek;
    INTEGER nbval;
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
    nbval=(INTEGER)fread(buf,1,(size_t)(*nbytes),fpfile[iu]);
 if ( nbval != *nbytes ) {*ierr = -4;}

}

void DEFSPPPPPP(WRITDR, writdr, char *dfname, STRING_SIZE len_dfname, void *buf,
            INTEGER *nbytes, INTEGER *irec, INTEGER *indic, INTEGER *s, INTEGER *ierr)
{
    long offset;
    long iu,nbseek,nbwrite;
    INTEGER nbval;
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
    nbval=(INTEGER)fwrite(buf,1,(size_t)(*nbytes),fpfile[iu]);

    if ( nbval != *nbytes ) {*ierr = -4;}
}
