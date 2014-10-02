/* ------------------------------------------------------------------ */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2012  EDF R&D              WWW.CODE-ASTER.ORG */
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
/* ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,      */
/*    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     */
/* ================================================================== */

#include "aster.h"
#include "aster_utils.h"

#define MAX_FAC         256
#define LONG_NOM_FIC    513
#define OFF_INIT        ASTER_INT_SIZE

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
         nomFAC[nbFAC]=(char *) malloc(LONG_NOM_FIC);
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


void DEFSPP(OPENDR, opendr, char *dfname, STRING_SIZE len_dfname,
                            INTEGER *mode, INTEGER *ierr)
{
    /*
        mode = 0 : ro read only
        mode = 1 : rw read + write
        mode = 2 : write
     */
    long iu, nbread;
    char *fname, smode[4];
    int imode;

    imode = (int)(*mode);
    *ierr = 0;
    fname = MakeCStrFromFStr(dfname, len_dfname);
    iu=open_fac(fname);
    if ( iu < 0 ) {
       *ierr = -1;
       FreeStr(fname);
       return;
    }
    if ( imode == 0 ) {
        strncpy(smode, "rb", 2);
    } else if ( imode == 1 ) {
        strncpy(smode, "rb+", 3);
    } else {
        strncpy(smode, "wb+", 3);
    }
    printf("trying open file '%s' using mode '%s'...\n", fname, smode);
    fpfile[iu] = fopen(fname, smode);
    if (fpfile[iu] != NULL  ) {
        if ( imode == 2 ) {
            printf("open in write mode: %s\n", fname);
            nenr[iu] = -1;
            *ierr = 0;
        } else {
            printf("open in read mode: %s\n", fname);
            nbread=fread(&nenr[iu], OFF_INIT, 1, fpfile[iu]);
        }
    }
    else {
        printf("open failed: %s\n", fname);
        *ierr = -2;
    }
    FreeStr(fname);
}

void DEFSP(CLOSDR, closdr, char *dfname, STRING_SIZE len_dfname, INTEGER *ierr)
{
    long iu;
    char *fname;

    *ierr = 0;
    fname = MakeCStrFromFStr(dfname, len_dfname);
    iu=ind_fac(fname);
    if ( iu < 0 ) {
       *ierr = -1;
    }
    else {
        fclose(fpfile[iu]);
        fpfile[iu] = NULL;
        nenr[iu]=-1;
        *ierr = 0;
    }
    FreeStr(fname);
}

void DEFSPPPP(READDR, readdr, char *dfname, STRING_SIZE len_dfname, void *buf,
                              INTEGER *nbytes, INTEGER *irec, INTEGER *ierr)
{
    long offset;
    long iu,nbseek;
    INTEGER nbval;
    char *fname;

    *ierr = 0;
    fname = MakeCStrFromFStr(dfname, len_dfname);
    iu=ind_fac(fname);
    if ( iu < 0 ) {
       *ierr = -1;
       FreeStr(fname);
       return;
    }
    if ( nenr[iu] == -1 ) {
        *ierr = -2;
        FreeStr(fname);
        return;
    }
    if ( fpfile[iu] == NULL ) {
        *ierr = -3;
        FreeStr(fname);
        return;
    }
    offset = (*irec-1)*nenr[iu]+OFF_INIT;
    nbseek=fseek(fpfile[iu],offset, SEEK_SET);
    nbval=(INTEGER)fread(buf,1,(size_t)(*nbytes),fpfile[iu]);
    if ( nbval != *nbytes ) {
        *ierr = -4;
    }
    FreeStr(fname);
}

void DEFSPPPP(WRITDR, writdr, char *dfname, STRING_SIZE len_dfname, void *buf,
            INTEGER *nbytes, INTEGER *irec, INTEGER *ierr)
{
    long offset;
    long iu,nbseek,nbwrite;
    INTEGER nbval;
    char *fname;

    *ierr = 0;
    fname = MakeCStrFromFStr(dfname, len_dfname);
    iu=ind_fac(fname);
    if ( iu < 0 ) {
       *ierr = -1;
       FreeStr(fname);
       return;
    }
    if ( fpfile[iu] == NULL ) {
        *ierr = -3;
        FreeStr(fname);
        return;
    }
    if ( nenr[iu] == -1 ) {
       nenr[iu] = *nbytes;
       nbwrite=fwrite(&nenr[iu], OFF_INIT, 1, fpfile[iu]);
    }
    offset = (*irec-1)*(nenr[iu])+OFF_INIT;
    nbseek=fseek(fpfile[iu],offset, SEEK_SET);
    nbval=(INTEGER)fwrite(buf,1,(size_t)(*nbytes),fpfile[iu]);

    if ( nbval != *nbytes ) {
        *ierr = -4;
    }
    FreeStr(fname);
}
