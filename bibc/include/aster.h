/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF aster include  DATE 23/04/2012   AUTEUR COURTOIS M.COURTOIS */
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
/* ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,    */
/*    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     */
/* ================================================================== */

#ifndef ASTER_H
#define ASTER_H

#include <Python.h>

#include "aster_depend.h"
#include "definition.h"

/* -------------------------------------
   --          DEBUT aster.h          --
   ------------------------------------- */

#ifndef min
#define min(A,B)  ((A) < (B) ? (A) : (B))
#endif

/* pour indiquer le statut des arguments des fonctions. */

#define _IN
#define _OUT
#define _INOUT
#define _UNUSED

#if (PY_VERSION_HEX < 0x02050000)
typedef int Py_ssize_t;
#endif

/* pour preciser quel fichier affiche les  messages et les valeurs */
#define INTERRUPTION(code) { ICI ; fprintf(stderr,"INTERRUPTION - code retour %d\n",code) ;abort() ; }
#define ICI fflush(stdout);fprintf( stderr, "%s  %d : " , __FILE__ , __LINE__  ) ; fflush(stderr) ;

/* Utiliser -DUSE_ASSERT pour activer les ASSERT */
#ifdef USE_ASSERT
#define ASSERT(condition) if( !(condition) ){ ICI ; fprintf(stderr,"condition %s VIOLEE\n",#condition);INTERRUPTION(17);}
#else
#define ASSERT(condition)
#endif

#define PRINTERR if(PyErr_Occurred()){ \
            fprintf(stderr,"Warning: une exception n'a pas ete traitée\n"); \
            PyErr_Print(); \
            fprintf(stderr,"Warning: on l'annule pour continuer mais elle aurait du etre traitée avant\n"); \
            PyErr_Clear(); \
        }

#define EstValide(c) (isprint((int)c) && (isalnum((int)c) || (c=='_') || (c==' ')))


/* deprecated functions on Windows */
#ifdef _WINDOWS
#define strdup _strdup
#endif

/* -------------------------------------
   --           FIN  aster.h          --
   ------------------------------------- */
#endif
