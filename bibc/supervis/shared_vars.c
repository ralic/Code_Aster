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

/*
 * Because of "cyclic" dependencies between the python modules (aster, aster_core)
 * and libaster, libaster.so contains `modules`.o and `modules`.o are linked
 * against libaster.so.
 * So when a global variable is set from Python to C it is stored in the symbols
 * of the `module`. And when this same variable is accessed from Fortran it is
 * taken in the symbols of libaster.so.
 * 
 * Before a wider refactoring here are defined these exchanged variables with
 * their access methods.
 */

#include "Python.h"
#include <stdio.h>

#include "shared_vars.h"

/* Global variables */
/*! Global variable to handle the JDC object from libaster */
static PyObject* gJDC = (PyObject*)0;

/*! Global variable to handle the CoreOptions object from libaster */
static PyObject* gCoreOpts = (PyObject*)0;

/*! Global variable to handle the MessageLog object from libaster */
static PyObject* gMsgLog = (PyObject*)0;

/*! Global variable to handle the aster_core python module from libaster */
static PyObject* gPyMod = (PyObject*)0;


/*! gJeveuxStatus is:
      0 before aster_init,
      1 during the execution,
      0 after xfini.

   This allows to lock the call to jeveux functions out of the execution
*/
static int gJeveuxStatus = 0;

/*! Variable referencing the 'etape' object */
static PyObject *gEtape = (PyObject*)0;

/*! Stack of 'etape' objects */
static PyObject *gPileEtapes = (PyObject*)0;


/* register functions */
/*! Register the JDC object as a global variable */
void register_sh_jdc(PyObject *obj) {
    gJDC = obj;
    return;
}

/*! Register the CoreOptions object as a global variable */
void register_sh_coreopts(PyObject *obj) {
    gCoreOpts = obj;
    return;
}

/*! Register the MessageLog object as a global variable */
void register_sh_msglog(PyObject *obj) {
    gMsgLog = obj;
    return;
}

/*! Register the aster_core python module as a global variable */
void register_sh_pymod(PyObject *obj) {
    gPyMod = obj;
    return;
}

/*! Register the current 'etape' object as a global variable */
void register_sh_etape(PyObject *obj) {
    gEtape = obj;
    return;
}

/*! Register the status of jeveux */
void register_sh_jeveux_status(int obj) {
    gJeveuxStatus = obj;
    return;
}

/* get functions */
/*! Return the global JDC object */
PyObject * get_sh_jdc() {
    return gJDC;
}

/*! Return the global CoreOptions object */
PyObject * get_sh_coreopts() {
    return gCoreOpts;
}

/*! Return the global MessageLog object */
PyObject * get_sh_msglog() {
    return gMsgLog;
}

/*! Return the global aster_core python module */
PyObject * get_sh_pymod() {
    return gPyMod;
}

/*! Return the current 'etape' object */
PyObject * get_sh_etape() {
    return gEtape;
}

/*! Return the status of jeveux */
int get_sh_jeveux_status() {
    return gJeveuxStatus;
}

/*
 * Pour conserver le lien entre le code de calcul en Fortran et le superviseur
 * en Python, on memorise la commande courante.
 * Cette commande est celle que le fortran interroge lors de ses appels à l'API
 * GETXXX.
 * Cette commande courante est enregistrée dans une pile de commandes car avec le
 * mécanisme des macros, une commande peut avoir des sous commandes qui sont
 * appelées pendant l'exécution de la commande principale.
 * La fonction append_etape doit etre appelée avant l'exécution d'une commande (appel à oper,
 * par exemple) et la fonction pop_etape doit etre appelée après l'exécution de cette
 * commande.
 */

/*! Initialize the stack of 'etape' objects */
void init_etape_stack() {
    gPileEtapes = PyList_New(0);
}

/*! Append the given 'etape' object on stack */
PyObject * append_etape(PyObject *etape)
{
    /* PyList_Append incremente de 1 le compteur de references de c (commande courante) */
    PyList_Append(gPileEtapes, etape);
    return etape;
}

/*! Remove and return the last 'etape' object on stack */
PyObject * pop_etape()
{
    PyObject * etape;
    int l = PyList_Size(gPileEtapes);
    if (l == 0) {
        /* Pile vide */
        Py_INCREF(Py_None);
        return Py_None;
    }
    /* Derniere commande dans la pile */
    etape = PyList_GetItem(gPileEtapes, l-1);
    /* PyList_GetItem n incremente pas le compteur de ref de etape */
    /* On tronque la liste a la dimension l-1 */
    PyList_SetSlice(gPileEtapes, l-1, l, NULL);
    /* Le compteur de ref de etape est decremente de 1 */
    if (l == 1) {
        /* La pile tronquee est vide */
        Py_INCREF(Py_None);
        return Py_None;
    }
    /* On ne passe ici que pour les macros avec sous commandes
     * en mode commande par commande */
    /* On retourne la derniere commande de la pile */
    etape = PyList_GetItem(gPileEtapes, l-2);
    return etape;
}
