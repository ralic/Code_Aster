/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF astercore_module supervis  DATE 21/05/2012   AUTEUR COURTOIS M.COURTOIS */
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
/* RESPONSABLE COURTOIS */

/*
 * Module for the core functions of Code_Aster :
 *  - give access to the main object (for fortran calls) :
 *      - JDC : current JDC object,
 *      - CoreOptions : command line options + basic informations
 *      - MessageLog : utility to print message,
 *      - E_Global : a module to define functions more easily in Python than here.
 */

#include <Python.h>
#include "aster.h"
#include "aster_core.h"
#include "aster_module.h"
#include "aster_fort.h"
#include "aster_utils.h"

/*
 * Global variables registered by E_SUPERV.py (main python script)
 */
static PyObject* aster_core = (PyObject*)0;

static PyObject* gJDC = (PyObject*)0;
static PyObject* gCoreOpts = (PyObject*)0;
static PyObject* gMsgLog = (PyObject*)0;
static PyObject* gPyMod = (PyObject*)0;

static char register_jdc_doc[] = "Enregistre des objets globaux.";

static PyObject* register_jdc(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
    /*
     * Register the JDC object as a global variable
     */
    PyObject *val;
    if ( !PyArg_ParseTuple(args, "OOOO:register_jdc", &gJDC, &gCoreOpts, &gMsgLog, &gPyMod) )
        return NULL;

    // Add some wrappers for convenience
    val = PyObject_GetAttrString(gCoreOpts, "get_option");
    if ( PyObject_SetAttrString(aster_core, "get_option", val) < 0 )
        MYABORT("erreur lors de l'initialisation de 'aster_core.get_option'.");
    val = PyObject_GetAttrString(gCoreOpts, "set_info");
    if ( PyObject_SetAttrString(aster_core, "set_info", val) < 0 )
        MYABORT("erreur lors de l'initialisation de 'aster_core.set_info'.");

    Py_INCREF(gJDC);
    Py_INCREF(Py_None);
    return Py_None;
}

/*
 * Functions based on JDC object.
 */
INTEGER DEFS(JDCGET,jdcget,char *attr, STRING_SIZE l_attr)
{
    /*
     * Permet de récupérer la valeur entière d'un attribut du jdc.
     */
    PyObject *val;
    INTEGER value;

    val = PyObject_CallMethod(gJDC, "get_jdc_attr", "s#", attr, l_attr);
    if (val == NULL){
        printf("attribut inexistant dans le jdc : '%s'\n\n", attr);
        MYABORT("erreur dans JDCGET");
    }
    if (!PyInt_Check(val))
        MYABORT("Seuls les attributs de type entier peuvent etre recuperes !");

    value = (INTEGER)PyInt_AsLong(val);

    Py_XDECREF(val);
    return value;
}

void DEFSP(JDCSET,jdcset,char *attr, STRING_SIZE l_attr, INTEGER *value)
{
    /*
     * Permet de positionner la valeur entière d'un attribut du jdc à `value`.
     */
    PyObject *res;

    res = PyObject_CallMethod(gJDC, "set_jdc_attr", "s#i", attr, l_attr, (int)*value);
    if (res == NULL)
        MYABORT("erreur dans JDCSET");
    Py_XDECREF(res);
}

PyObject* GetJdcAttr(_IN char *attribut)
{
    /*
     * Retourne un attribut du 'jdc' en tant que PyObject.
     */
    PyObject *objattr;
    objattr = PyObject_GetAttrString(gJDC, attribut);
    /* traiter l'erreur "objattr == NULL" dans l'appelant */
    return objattr;
}

static double _cache_tpmax = -1.;

double get_tpmax()
{
    /* Retourne le temps maximum autorisé pour l'exécution
     * (on met la valeur en cache pour éviter le passage au Python à chaque appel de
     *  uttrst/uttcp0/uttcpu)
     */
    int iret;
    double tpmax;
    if ( _cache_tpmax < 0 ) {
        tpmax =  asterc_getopt_double("tpmax", &iret);
        if ( iret == 0 ) {
            _cache_tpmax = tpmax;
        }
    }
    return _cache_tpmax;
}

void DEFP(RDTMAX, rdtmax, _IN DOUBLE *tsub)
{
    /*
     * Réduit le temps maximum de l'exécution : tpmax = tpmax - tsub
     */
    PyObject *res;

    res = PyObject_CallMethod(gCoreOpts, "sub_tpmax", "d", (double)(*tsub));
    if (res == NULL)
        MYABORT("erreur dans RDTMAX");
    // reset du cache
    _cache_tpmax = -1;
    return;
}

/*
 * Functions based on CoreOpts object.
 */
PyObject* asterc_getopt(_IN char *option)
{
    /*
     * Interface Fortran/Python pour récupérer une option de la ligne de commande.
     * Retourne :
     *  iret = 0 : tout est ok
     *  iret > 0   erreur
     *      iret = 1 : longueur de valk insuffisante, valeur tronquée
     *      iret = 4 : option inexistante, type incorrect.
     */
    PyObject *res;

    res = PyObject_CallMethod(gCoreOpts, "get_option", "s", option);
    if ( !res ) MYABORT("erreur lors de l'appel a la methode CoreOptions.get_option");

    return res;
}

long asterc_getopt_long(_IN char *option, _OUT int *iret)
{
    /*
     * Interface C/Python pour récupérer une option de la ligne de commande.
     * Retourne la valeur et un code retour :
     *  iret = 0 : tout est ok
     *  iret = 4 : option inexistante, type incorrect.
     */
    PyObject *res;
    long value = 0 ;
    *iret = 4;
    res = asterc_getopt(option);
    if ( PyInt_Check(res) ) {
        value = PyInt_AsLong(res);
        *iret = 0;
    } else if ( PyLong_Check(res) ) {
        value = PyLong_AsLong(res);
        *iret = 0;
    }
    Py_DECREF(res);
    return value;
}

double asterc_getopt_double(_IN char *option, _OUT int *iret)
{
    /*
     * Interface C/Python pour récupérer une option de la ligne de commande.
     * Retourne la valeur et un code retour :
     *  iret = 0 : tout est ok
     *  iret = 4 : option inexistante, type incorrect.
     */
    PyObject *res;
    double value = 0.;
    *iret = 4;
    res = asterc_getopt(option);
    if ( PyFloat_Check(res) ) {
        value = PyFloat_AsDouble(res);
        *iret = 0;
    }
    Py_DECREF(res);
    return value;
}

char* asterc_getopt_string(_IN char *option, _OUT int *iret)
{
    /*
     * Interface C/Python pour récupérer une option de la ligne de commande.
     * Retourne la valeur et un code retour :
     *  iret = 0 : tout est ok
     *  iret = 4 : option inexistante, type incorrect.
     */
    PyObject *res;
    char *value = NULL, *stmp = NULL;
    STRING_SIZE lv;
    *iret = 4;
    res = asterc_getopt(option);
    if ( PyString_Check(res) ) {
        stmp = PyString_AsString(res);
        lv = strlen(stmp);
        value = MakeFStrFromCStr(stmp, strlen(stmp));
        *iret = 0;
    }
    Py_DECREF(res);
    return value;
}

void DEFSPP(GTOPTI,gtopti, _IN char *option, STRING_SIZE lopt,
            _OUT INTEGER *vali,
            _OUT INTEGER *iret)
{
    /*
     * Interface C/Python pour récupérer une option de la ligne de commande.
     * Retourne la valeur et un code retour :
     *  iret = 0 : tout est ok
     *  iret = 4 : option inexistante, type incorrect.
     */
    long value;
    int ret = 0;
    char *sopt;
    sopt = MakeCStrFromFStr(option, lopt);
    value = asterc_getopt_long(sopt, &ret);

    *vali = (INTEGER)value;
    *iret = (INTEGER)ret;
    FreeStr(sopt);
}

void DEFSPP(GTOPTR,gtoptr, _IN char *option, STRING_SIZE lopt,
            _OUT DOUBLE *valr,
            _OUT INTEGER *iret)
{
    /*
     * Interface C/Python pour récupérer une option de la ligne de commande.
     * Retourne la valeur et un code retour :
     *  iret = 0 : tout est ok
     *  iret = 4 : option inexistante, type incorrect.
     */
    double value;
    int ret = 0;
    char *sopt;
    sopt = MakeCStrFromFStr(option, lopt);
    value = asterc_getopt_double(sopt, &ret);

    *valr = (DOUBLE)value;
    *iret = (INTEGER)ret;
    FreeStr(sopt);
}

void DEFSSP(GTOPTK,gtoptk, _IN char *option, STRING_SIZE lopt,
            _OUT char *valk, STRING_SIZE lvalk,
            _OUT INTEGER *iret)
{
    /*
     * Interface C/Python pour récupérer une option de la ligne de commande.
     * Retourne la valeur et un code retour :
     *  iret = 0 : tout est ok
     *  iret = 1 : longueur de valk insuffisante, valeur tronquée
     *  iret = 4 : option inexistante, type incorrect.
     */
    char *value;
    int ret = 0;
    char *sopt;
    sopt = MakeCStrFromFStr(option, lopt);
    value = asterc_getopt_string(sopt, &ret);

    if ( ret == 0 ) {
        if ( strlen(value) > lvalk ) ret = 1;
        CopyCStrToFStr(valk, value, lvalk);
    }
    *iret = (INTEGER)ret;
    FreeStr(sopt);
    FreeStr(value);
}

/*
 * Functions based on MessageLog object.
 */
void DEFSPSPSPPPP(UTPRIN,utprin, _IN char *typmess, _IN STRING_SIZE ltype,
                                 _IN INTEGER *exc_typ,
                                 _IN char *idmess, _IN STRING_SIZE lidmess,
                                 _IN INTEGER *nbk, _IN char *valk, _IN STRING_SIZE lvk,
                                 _IN INTEGER *nbi, _IN INTEGER *vali,
                                 _IN INTEGER *nbr, _IN DOUBLE *valr)
{
   /*
      Interface Fortran/Python pour l'affichage des messages
   */
    PyObject *tup_valk, *tup_vali, *tup_valr, *res;
    char *kvar;
    int i;

    tup_valk = PyTuple_New( (Py_ssize_t)*nbk ) ;
    for(i=0;i<*nbk;i++){
       kvar = valk + i*lvk;
       PyTuple_SetItem( tup_valk, i, PyString_FromStringAndSize(kvar,(Py_ssize_t)lvk) ) ;
    }

    tup_vali = PyTuple_New( (Py_ssize_t)*nbi ) ;
    for(i=0;i<*nbi;i++){
       PyTuple_SetItem( tup_vali, i, PyInt_FromLong((long)vali[i]) ) ;
    }

    tup_valr = PyTuple_New( (Py_ssize_t)*nbr ) ;
    for(i=0;i<*nbr;i++){
       PyTuple_SetItem( tup_valr, i, PyFloat_FromDouble((double)valr[i]) ) ;
    }


    res = PyObject_CallMethod(gMsgLog, "print_message", "s#s#OOOi",
                              typmess, ltype, idmess, lidmess, tup_valk, tup_vali, tup_valr,
                              (int)*exc_typ);
    if (!res) {
       MYABORT("erreur lors de l'appel a MessageLog");
    }

    Py_DECREF(tup_valk);
    Py_DECREF(tup_vali);
    Py_DECREF(tup_valr);
    Py_DECREF(res);
}

void DEFPP(CHKMSG,chkmsg, _IN INTEGER *info_alarm, _OUT INTEGER *iret)
{
   /*
      Interface Fortran/Python pour la vérification que tout s'est bien
      passé, destinée à etre appelée dans FIN ou au cours d'une commande.
      Argument IN :
         info_alarm = 1  on vérifie si les alarmes ignorées ont été émises ou non.
                    = 0  on ne fait pas cette vérif
      Retourne :
         iret = 0 : tout est ok
         iret > 0   erreur
   */
    PyObject *res;

    res = PyObject_CallMethod(gMsgLog, "check_counter", "i", (int)*info_alarm);
    if ( !res ) MYABORT("erreur lors de l'appel a la methode MessageLog.check_counter");
    *iret = (INTEGER)PyLong_AsLong(res);

    Py_DECREF(res);
}

void DEFSS(UTALRM,utalrm, _IN char *bool, _IN STRING_SIZE lbool,
                          _IN char *idmess, _IN STRING_SIZE lidm )
{
    /* Interface Fortran/Python pour masquer et rétablir l'affichage d'une alarme.
     *
     * CALL UTALRM('OFF', 'CALCULEL5_7') == MasquerAlarme('CALCULEL5_7')
     * CALL UTALRM('ON', 'CALCULEL5_7') == RetablirAlarme('CALCULEL5_7')
     */
    char *onoff, *s_id;
    PyObject *res;

    onoff = MakeCStrFromFStr(bool, lbool);
    s_id = MakeCStrFromFStr(idmess, lidm);

    if ( ! strcmp(onoff, "OFF") ) {
        res = PyObject_CallMethod(gMsgLog, "disable_alarm", "s", s_id);
    } else {
        res = PyObject_CallMethod(gMsgLog, "reset_alarm", "s", s_id);
    }

    Py_DECREF(res);
    FreeStr(onoff);
}

/*
 * Functions defined in E_Core
 */
void DEF0(PRHEAD,prhead)
{
    /*
     * Interface Fortran/Python pour l'affichage des informations systèmes
     * en début d'exécution
     */
    PyObject *res;
    res = PyObject_CallMethod(gPyMod, "print_header", NULL);
    if (!res) MYABORT("erreur lors de l'appel a la fonction E_Global.print_header");
    Py_DECREF(res);
}

void DEFSSP(CHEKSD,cheksd,_IN char *nomsd,_IN STRING_SIZE lnom,
                          _IN char *typsd,_IN STRING_SIZE ltyp,
                          _OUT INTEGER *iret)
{
   /*
      Interface Fortran/C pour vérifier que la structure de données `nomsd`
      est conforme au type `typsd`.

      Exemple d'appel :
         CALL CHEKSD('MA', 'sd_maillage', IRET)
   */
   PyObject *res;

   res = PyObject_CallMethod(gPyMod, "checksd", "s#s#", nomsd, lnom, typsd, ltyp);
   if (!res) MYABORT("erreur lors de l'appel a la methode CHECKSD");
   *iret = (INTEGER)PyLong_AsLong(res);

   Py_DECREF(res);
}

/*
 * General functions - wrapper to fortran subroutines.
 */
static char matfpe_doc[] =
"Interface d'appel a la routine C matfpe.\n"
"   usage: matfpe(actif)\n"
"     matfpe(-1) : on desactive l'interception des erreurs numeriques,\n"
"     matfpe(1)  : on active l'interception des erreurs numeriques.\n";

PyObject* asterc_matfpe(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
   int iactif;
   INTEGER actif;

   if (!PyArg_ParseTuple(args, "i:matfpe", &iactif)) return NULL;

   if (iactif >= -1  && iactif <= 1) {
      actif = (INTEGER)iactif;
      CALL_MATFPE(&actif);
   } else {
      MYABORT("Valeur incorrecte : seuls -1 et 1 sont valides.");
   }
   Py_INCREF( Py_None ) ;
   return Py_None;
}

/*
 * Methods of the aster_core module.
 */
static PyMethodDef methods[] = {
    { "register",   register_jdc,       METH_VARARGS, register_jdc_doc },
    { "matfpe",     asterc_matfpe,      METH_VARARGS, matfpe_doc},
    // { "get_option",  ... } : method added in register_jdc
    // { "set_info",  ... } : method added in register_jdc
    { NULL, NULL, 0, NULL }
};

#ifndef _WITHOUT_PYMOD_
PyMODINIT_FUNC initaster_core(void)
{
    aster_core = Py_InitModule("aster_core", methods);

    // Add macro constant for dependance
#ifdef _USE_MPI
    PyModule_AddIntConstant(aster_core, "_USE_MPI", 1);
#else
    PyModule_AddIntConstant(aster_core, "_USE_MPI", 0);
#endif
#ifdef _USE_OPENMP
    PyModule_AddIntConstant(aster_core, "_USE_OPENMP", 1);
#else
    PyModule_AddIntConstant(aster_core, "_USE_OPENMP", 0);
#endif
#ifdef _USE_64_BITS
    PyModule_AddIntConstant(aster_core, "_USE_64_BITS", 1);
#else
    PyModule_AddIntConstant(aster_core, "_USE_64_BITS", 0);
#endif
#ifdef _POSIX
    PyModule_AddIntConstant(aster_core, "_POSIX", 1);
#else
    PyModule_AddIntConstant(aster_core, "_POSIX", 0);
#endif
#ifdef _NO_EXPIR
    PyModule_AddIntConstant(aster_core, "_NO_EXPIR", 1);
#else
    PyModule_AddIntConstant(aster_core, "_NO_EXPIR", 0);
#endif
    PyModule_AddIntMacro(aster_core, LONG_INTEGER_BITS);
    PyModule_AddIntMacro(aster_core, LONG_INTEGER_MOTS);

    // fill later (after the arguments have been read)
    PyModule_AddObject(aster_core, "__version__", Py_None);
}
#endif

