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
/* person_in_charge: mathieu.courtois at edf.fr */

#ifndef ASTER_UTILS_H
#define ASTER_UTILS_H

#include "aster.h"

STRING_SIZE FStrlen( char *, STRING_SIZE );
char * MakeCStrFromFStr( char *, STRING_SIZE );
char * MakeFStrFromCStr( char *, STRING_SIZE );
void   CopyCStrToFStr( char *, char *, STRING_SIZE );
char * MakeTabFStr( int, STRING_SIZE );
void   SetTabFStr( char *, int, char *, STRING_SIZE );
void   BlankStr( char *, STRING_SIZE );
char * MakeBlankFStr( STRING_SIZE );
void   FreeStr( char * );

void _check_string_length( STRING_SIZE );


extern void convc8( _IN int, _IN PyObject *, _OUT DOUBLE *);
extern int conv_un_c8( _IN PyObject *, _OUT DOUBLE *);
extern void convr8( _IN int, _IN PyObject *, _OUT DOUBLE *);
extern void convert( _IN int, _IN PyObject *, _OUT INTEGER *);
extern void convertxt( _IN int, _IN PyObject *, _OUT char *, _IN STRING_SIZE);
extern void converltx( _IN int, _IN PyObject *, _OUT char *, _IN STRING_SIZE);


extern PyObject * MakeTupleString(long, char *, STRING_SIZE, INTEGER *);
extern PyObject * MakeListString(long, char *, STRING_SIZE);
extern PyObject * MakeTupleInt(long, INTEGER *);
extern PyObject * MakeListInt(long, INTEGER*);
extern PyObject * MakeTupleFloat(long, DOUBLE *);
extern PyObject * MakeListFloat(long, DOUBLE *);

#endif
