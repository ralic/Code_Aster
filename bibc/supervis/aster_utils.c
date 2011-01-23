/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF aster_utils supervis  DATE 25/01/2011   AUTEUR COURTOIS M.COURTOIS */
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

#include <stdlib.h>
#include "aster.h"


char * MakeCStrFromFStr( _IN char *fstr, _IN STRING_SIZE flen )
{
    /* Alloue et retourne une chaine C (terminant par \0) étant
     * la copie de la chaine Fortran sans les blancs finaux.
     * La chaine devra etre libérée par l'appelant.
     */
    char *cstr = NULL;
    int n;

    n = flen;
    while (fstr[n-1] == ' ') { n--; }
    
    cstr = (char*)malloc(n * sizeof(char));
    strncpy(cstr, fstr, n);
    cstr[n] ='\0';

    return cstr;
}

/* pour que ce soit clair */
void FreeCStr(char *cstr)
{
    free(cstr);
}
