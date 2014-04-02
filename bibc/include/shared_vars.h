/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2014  EDF R&D              WWW.CODE-ASTER.ORG */
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

#ifndef SHARED_VARS_H
#define SHARED_VARS_H

#include "Python.h"

/*
 *   PUBLIC FUNCTIONS
 *
 */
/*! Register the JDC object as a global variable */
extern void register_sh_jdc(PyObject *);

/*! Register the CoreOptions object as a global variable */
extern void register_sh_coreopts(PyObject *);

/*! Register the MessageLog object as a global variable */
extern void register_sh_msglog(PyObject *);

/*! Register the aster_core module as a global variable */
extern void register_sh_pymod(PyObject *);

/*! Register the current 'etape' object as a global variable */
extern void register_sh_etape(PyObject *);

/*! Register the status of jeveux */
extern void register_sh_jeveux_status(int);

/*! Return the global JDC object */
extern PyObject * get_sh_jdc();

/*! Return the global CoreOptions object */
extern PyObject * get_sh_coreopts();

/*! Return the global MessageLog object */
extern PyObject * get_sh_msglog();

/*! Return the global aster_core python module */
extern PyObject * get_sh_pymod();

/*! Return the current 'etape' object */
extern PyObject * get_sh_etape();

/*! Return the status of jeveux */
extern int get_sh_jeveux_status();

/*! Initialize the stack of 'etape' objects */
extern void init_etape_stack();

/*! Append the given 'etape' object on stack */
extern PyObject * append_etape(PyObject *);

/*! Remove and return the last 'etape' object on stack */
extern PyObject * pop_etape();

/* FIN SHARED_VARS_H */
#endif
