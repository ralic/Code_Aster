/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2015  EDF R&D              WWW.CODE-ASTER.ORG */
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
/* appel de la commande systeme de destruction de fichier */
/* rm ou del suivant les plates-formes                    */
/* si info  = 1 mode bavard                               */
/* si info != 1 mode silencieux                           */
#include "aster.h"
#include "aster_utils.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void DEFSP(RMFILE, rmfile, char *nom1, STRING_SIZE lnom1, INTEGER *info)
{
    char *cmdline, *ncmd, *fname;
    size_t ldeb;
    int ier;
    
#if defined _POSIX
    ncmd = "rm -f ";
    ldeb = 6;
#else
    ncmd = "del ";
    ldeb = 4;
#endif

    cmdline = (char*)malloc((ldeb + lnom1 + 1) * sizeof(char));
    fname = MakeCStrFromFStr(nom1, lnom1);
    strncpy(cmdline, ncmd, (size_t)ldeb);
    strcpy(cmdline + ldeb, fname);

    if ( *info == 1 ) {
        printf("\n\nLancement de la commande : %s\n\n", cmdline);
    }

    ier = system(cmdline);
    free(cmdline);
    FreeStr(fname);

    if ( *info == 1 && ier == -1 ) {
        perror("\n<rmfile> code retour system");
    }
#if defined _POSIX
    fflush(stderr);
    fflush(stdout);
#else
    _flushall();
#endif
}
