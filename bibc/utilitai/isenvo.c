/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF ISENVO utilitai  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int entier;
typedef unsigned char UNIT;
typedef UNIT CLE[4];
typedef UNIT MASQUE[8];
#ifdef CRAY
   long ISENVO(long *val, char *str)
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
   long isenvo_(long *val, char *str, unsigned long l_str)
#elif defined HPUX
   long isenvo(long *val, char *str, unsigned long l_str)
#elif defined PPRO_NT
   long __stdcall ISENVO(long *val, char *str, unsigned long l_str)
#endif
/*
** Fonction pour positionner et interroger l'indicateur
** d'execution d'Aster en version officielle ou surchargee
** val -> si < 0  SENVO renvoie la valeur de l'indicateur
**        si > 0  positionne l'indicateur
**        si = 0  initialisation
*/
{
#ifndef VERSION_PORTEE
static long IND_ENVO=0;
static char VAL_REF[64]="";

if (*val == 0) {
   /*
   ** Initialisation du processus
   */

   char * p;

   if ((p=strrchr(str,'/')) == NULL) {
      strcpy(VAL_REF,str);
      }
   else {
      p++;
      strcpy(VAL_REF,p);
      }
   strcpy(VAL_REF,"execut");
   if ((long)strlen(VAL_REF) > 8) VAL_REF[8]='\0';
   }
else if (*val > 0) {
   /*
   ** Verification du codage
   */
   UNIT code[64];
   UNIT cle[64];
   UNIT sc[64];
   UNIT res[64];
   char * p;
   long i,j;
   char q[4];
   entier az;
   CLE lcle;
   MASQUE mask;
   long la,ls;

   /* Separation code/cle */
   p=str;
   for (i=0;i<16;i++) code[i]=*p++;
   code[i]='\0';
   for (i=0;i<8;i++)   cle[i]=*p++;
   cle[i]='\0';

   /* Conversion cle en caracteres representes en Hexa -> valeur numerique */
   p=(char *)cle;
   for (i=0;i<4;i++) {
      q[0]=*p++;
      q[1]=*p++;
      q[2]='\0';
      sscanf(q,"%x",&az);
      lcle[i]=(UNIT)az;
      }

   /* Creation du masque a 8 caracteres */
   for (i=0;i<4;i++) {
      mask[i]=lcle[i];
      mask[i+4]=lcle[3-i];
      }

   /* remplissage de la chaine a coder a 8 c */
   la=(long)strlen(VAL_REF);
   if (la > 8) la=8;
   ls=(long)strlen((char *)cle);
   if (8 > la) {
      for (i=0;i<(ls-la);i++) sc[i]=cle[ls-i-1];
      for (j=i;j<ls;j++)      sc[j]=VAL_REF[j-i];
      }
   sc[j]='\0';

   /* Masque avec ou exclusif */
   p=(char *)res;
   for (i=0;i<ls;i++) {
      az=sc[i] ^ mask[i];
      sprintf (p,"%02x", az);
      p+=2;
      }
   p='\0';

   if (strcmp((char *)res,(char *)code) == 0) IND_ENVO=1;
   else                       IND_ENVO=0;

   }
#else
static long IND_ENVO=2;
#endif

return(IND_ENVO);

}
