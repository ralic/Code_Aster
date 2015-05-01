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

/*
 * Module for the core functions of Code_Aster :
 *  - give access to the main object (for fortran calls) :
 *      - JDC : current JDC object,
 *      - CoreOptions : command line options + basic informations
 *      - MessageLog : utility to print message,
 *      - aster_core : pure python module that define functions more easily
 *        than here.
 *  - give informations about the execution.
 */

#include "Python.h"
#include "aster.h"
#include "aster_core_module.h"
#include "shared_vars.h"
#include "aster_module.h"
#include "aster_exceptions.h"
#include "aster_mpi.h"
#include "aster_fort.h"
#include "aster_utils.h"

/*! aster_core C module */
static PyObject* aster_core = (PyObject*)0;

static char register_jdc_doc[] = "Enregistre des objets globaux.";

static PyObject* register_jdc(PyObject *self, PyObject *args)
{
    /*
     * Register the Python instances for usage from fortran/libaster
     */
    PyObject *val;
    PyObject *jdc, *coreopts, *msglog, *pymod;
    if ( !PyArg_ParseTuple(args, "OOOO:register_jdc", &jdc, &coreopts, &msglog, &pymod) )
        return NULL;
    register_sh_jdc(jdc);
    register_sh_coreopts(coreopts);
    register_sh_msglog(msglog);
    register_sh_pymod(pymod);

    // Add some wrappers for convenience
    val = PyObject_GetAttrString(coreopts, "get_option");
    if ( PyObject_SetAttrString(aster_core, "get_option", val) < 0 )
        MYABORT("erreur lors de l'initialisation de 'aster_core.get_option'.");
    val = PyObject_GetAttrString(coreopts, "set_info");
    if ( PyObject_SetAttrString(aster_core, "set_info", val) < 0 )
        MYABORT("erreur lors de l'initialisation de 'aster_core.set_info'.");

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

    val = PyObject_CallMethod(get_sh_jdc(), "get_jdc_attr", "s#", attr, l_attr);
    if (val == NULL){
        printf("attribut inexistant dans le jdc : '%s'\n\n", attr);
        MYABORT("erreur dans JDCGET");
    }
    if (! (PyInt_Check(val) || PyLong_Check(val)) )
        MYABORT("Seuls les attributs de type entier peuvent etre recuperes !");

    value = (INTEGER)PyLong_AsLong(val);

    Py_XDECREF(val);
    return value;
}

void DEFSP(JDCSET,jdcset,char *attr, STRING_SIZE l_attr, INTEGER *value)
{
    /*
     * Permet de positionner la valeur entière d'un attribut du jdc à `value`.
     */
    PyObject *res;

    res = PyObject_CallMethod(get_sh_jdc(), "set_jdc_attr", "s#l", attr, l_attr, (long)*value);
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
    objattr = PyObject_GetAttrString(get_sh_jdc(), attribut);
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

    res = PyObject_CallMethod(get_sh_coreopts(), "sub_tpmax", "d", (double)(*tsub));
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

    res = PyObject_CallMethod(get_sh_coreopts(), "get_option", "s", option);
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

static char get_mem_stat_doc[] =
"Interface d'appel a la routine fortran UTGTME.\n";

static PyObject * asterc_get_mem_stat(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
    /*
     *  Interface d'appel à la routine fortran UTGTME
     */
    PyObject *t_valres, *t_res;
    int inbpar;
    INTEGER nbpar, codret;
    char *nompar;
    DOUBLE *valres;
    /* doit impérativement correspondre aux longueurs des chaines de caractères fortran */
    STRING_SIZE long_nompar = 8;
    void *malloc(size_t size);

    /* Conversion en tableaux de chaines */
    inbpar = (int)PyTuple_Size(args);
    nbpar = (INTEGER)inbpar;
    nompar = MakeTabFStr(inbpar, long_nompar);
    convertxt(inbpar, args, nompar, long_nompar);

    /* allocation des variables de sortie */
    valres = (DOUBLE *)malloc(nbpar*sizeof(DOUBLE));

    CALL_UTGTME(&nbpar, nompar, valres, &codret);

    t_valres = MakeTupleFloat((long)inbpar, valres);

    /* retour de la fonction */
    t_res = PyTuple_New(2);
    PyTuple_SetItem(t_res, 0, t_valres);
    PyTuple_SetItem(t_res, 1, PyInt_FromLong((long)codret));

    FreeStr(nompar);
    free(valres);

    return t_res;
}

static char set_mem_stat_doc[] =
"Interface d'appel a la routine fortran UTPTME.\n"
"   set_mem_stat(tuple_of_parameters, tuple_of_values)\n\n"
"   The number of values must be the same as the number of parameters.";

static PyObject * asterc_set_mem_stat(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
    /*
     *  Interface d'appel à la routine fortran UTGTME
     */
    PyObject *tup_par, *tup_val;
    PyObject *res;
    int inbpar, inbval;
    INTEGER nbpar, codret;
    char *nompar;
    DOUBLE *values;
    /* doit impérativement correspondre aux longueurs des chaines de caractères fortran */
    STRING_SIZE long_nompar = 8;
    void *malloc(size_t size);

    if ( !PyArg_ParseTuple(args, "OO:set_mem_stat", &tup_par, &tup_val) )
        return NULL;

    inbpar = (int)PyTuple_Size(tup_par);
    inbval = (int)PyTuple_Size(tup_val);
    if (inbpar != inbval) {
        MYABORT("sizes of the tuples of parameters & values mismatch\n");
    }

    /* Conversion en tableaux de chaines */
    nbpar = (INTEGER)inbpar;
    nompar = MakeTabFStr(inbpar, long_nompar);
    convertxt(inbpar, tup_par, nompar, long_nompar);

    /* allocation des valeurs des variables */
    values = (DOUBLE *)malloc(inbval*sizeof(DOUBLE));
    convr8(inbval, tup_val, values);

    CALL_UTPTME(&nbpar, nompar, values, &codret);

    /* retour de la fonction */
    res = PyInt_FromLong((long)codret);

    FreeStr(nompar);
    free(values);

    return res;
}

/*
 * Functions based on MessageLog object.
 */
void DEFSPSPSPPPPS(UTPRIN,utprin, _IN char *typmess, _IN STRING_SIZE ltype,
                                   _IN INTEGER *exc_typ,
                                   _IN char *idmess, _IN STRING_SIZE lidmess,
                                   _IN INTEGER *nbk, _IN char *valk, _IN STRING_SIZE lvk,
                                   _IN INTEGER *nbi, _IN INTEGER *vali,
                                   _IN INTEGER *nbr, _IN DOUBLE *valr,
                                   _IN char *fname, _IN STRING_SIZE lfn)
{
    /*
     * Fortran/Python interface to print the messages.
     *
     * WARNING: In the case that the error indicator has already been set, we must
     * restore it after PyObject_CallMethod.
     */
    char *kvar;
    int i, iexc=0, iret;
    PyObject *tup_valk, *tup_vali, *tup_valr, *res;
    PyObject *method, *args, *kwargs, *pyfname;
    PyObject *etype, *eval, *etb;

    if ( PyErr_Occurred() ) {
        iexc = 1;
        PyErr_Fetch(&etype, &eval, &etb);
    }

    tup_valk = PyTuple_New( (Py_ssize_t)*nbk ) ;
    for(i=0;i<*nbk;i++){
       kvar = valk + i*lvk;
       PyTuple_SetItem( tup_valk, i, PyString_FromStringAndSize(kvar,(Py_ssize_t)lvk) ) ;
    }

    tup_vali = PyTuple_New( (Py_ssize_t)*nbi ) ;
    for(i=0;i<*nbi;i++){
       PyTuple_SetItem( tup_vali, i, PyLong_FromLong((long)vali[i]) ) ;
    }

    tup_valr = PyTuple_New( (Py_ssize_t)*nbr ) ;
    for(i=0;i<*nbr;i++){
       PyTuple_SetItem( tup_valr, i, PyFloat_FromDouble((double)valr[i]) ) ;
    }

    method = PyObject_GetAttrString(get_sh_msglog(), "print_message");
    args = Py_BuildValue("s#s#OOOi", typmess, ltype, idmess, lidmess,
                         tup_valk, tup_vali, tup_valr, (int)*exc_typ),
    kwargs = PyDict_New();
    pyfname = PyString_FromStringAndSize(fname, lfn);
    iret = PyDict_SetItemString(kwargs, "files", pyfname);
    if (iret != 0) {
       MYABORT("error the given filename in utprin");
    }

    res = PyObject_Call(method, args, kwargs);
    if (!res) {
       MYABORT("erreur lors de l'appel a MessageLog");
    }
    if ( iexc == 1 ) {
        PyErr_Restore(etype, eval, etb);
    }

    Py_DECREF(pyfname);
    Py_DECREF(args);
    Py_XDECREF(kwargs);
    Py_DECREF(method);
    Py_DECREF(tup_valk);
    Py_DECREF(tup_vali);
    Py_DECREF(tup_valr);
    Py_DECREF(res);
}

void DEFPP(CHKMSG,chkmsg, _IN INTEGER *info_alarm, _OUT INTEGER *iret)
{
    /*
     * Interface Fortran/Python pour la vérification que tout s'est bien
     * passé, destinée à etre appelée dans FIN ou au cours d'une commande.
     * Argument IN :
     *    info_alarm = 1  on vérifie si les alarmes ignorées ont été émises ou non.
     *               = 0  on ne fait pas cette vérif
     * Retourne :
     *    iret = 0 : tout est ok
     *    iret > 0   erreur
     */
    PyObject *res;

    res = PyObject_CallMethod(get_sh_msglog(), "check_counter", "i", (int)*info_alarm);
    if ( !res ) MYABORT("erreur lors de l'appel a la methode MessageLog.check_counter");
    *iret = (INTEGER)PyLong_AsLong(res);

    Py_DECREF(res);
}

void DEFSS(UTALRM,utalrm, _IN char *bool, _IN STRING_SIZE lbool,
                          _IN char *idmess, _IN STRING_SIZE lidm )
{
    /* Interface Fortran/Python pour masquer et rétablir l'affichage d'une alarme.
     *
     * call utalrm('OFF', 'CALCULEL5_7') == MasquerAlarme('CALCULEL5_7')
     * call utalrm('ON', 'CALCULEL5_7') == RetablirAlarme('CALCULEL5_7')
     */
    char *onoff, *s_id;
    PyObject *res;

    onoff = MakeCStrFromFStr(bool, lbool);
    s_id = MakeCStrFromFStr(idmess, lidm);

    if ( ! strcmp(onoff, "OFF") ) {
        res = PyObject_CallMethod(get_sh_msglog(), "disable_alarm", "sO", s_id, Py_True);
    } else {
        res = PyObject_CallMethod(get_sh_msglog(), "reset_alarm", "sO", s_id, Py_True);
    }

    Py_DECREF(res);
    FreeStr(onoff);
    FreeStr(s_id);
}

void DEFP(GTALRM,gtalrm, _OUT INTEGER *nb)
{
    /* Interface Fortran/Python pour obtenir si des alarmes ont été émises.
     */
    PyObject *res;
    res = PyObject_CallMethod(get_sh_msglog(), "get_info_alarm_nb", "");
    if (!res) MYABORT("erreur lors de l'appel a la methode 'get_info_alarm'");
    *nb = (INTEGER)PyLong_AsLong(res);
    Py_DECREF(res);
}

/*
 * Functions defined in aster_core
 */
void DEFP(PRHEAD,prhead, _IN INTEGER *part)
{
    /*
     * Interface Fortran/Python pour l'affichage des informations systèmes
     * en début d'exécution
     * Voir help(aster_core.print_header)
     */
    PyObject *res;
    res = PyObject_CallMethod(get_sh_pymod(), "print_header", "i", (int)(*part));
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
         call cheksd('MA', 'sd_maillage', iret)
   */
   PyObject *res;

   res = PyObject_CallMethod(get_sh_pymod(), "checksd", "s#s#", nomsd, lnom, typsd, ltyp);
   if (!res) MYABORT("erreur lors de l'appel a la methode CHECKSD");
   *iret = (INTEGER)PyLong_AsLong(res);

   Py_DECREF(res);
}

void DEFSSPPPPPPPPPPPP(TESTRESU_PRINT,testresu_print,
            _IN char *refer, _IN STRING_SIZE lref,
            _IN char *legend, _IN STRING_SIZE lleg,
            _IN INTEGER *llab, _IN INTEGER *skip, _IN INTEGER *rela,
            _IN DOUBLE *tole,
            _IN INTEGER *typ,
            _IN DOUBLE *refr, _IN DOUBLE *valr,
            _IN INTEGER *refi, _IN INTEGER *vali,
            _IN DOUBLE *refc, _IN DOUBLE *valc,
            _IN DOUBLE *compare)
{
    /*
        Interface Fortran/C pour imprimer le résultat d'un TEST_RESU

        def testresu_print(type_ref, legend, label, skip, relative,
                           tole, ref, val, compare=1.):
    */
    PyObject *res, *func, *args, *kwargs, *ref, *val, *comp=NULL;
    int ityp;

    func = PyObject_GetAttrString(get_sh_pymod(), "testresu_print");
    /* positional arguments */
    args = Py_BuildValue("s#s#llld", refer, lref, legend, lleg,
                         (long)(*llab), (long)(*skip), (long)(*rela),
                         (double)(*tole));
    /* keyword arguments */
    ityp = (int)(*typ);
    switch ( ityp ) {
        case 1:
            ref = PyFloat_FromDouble((double)(*refr));
            val = PyFloat_FromDouble((double)(*valr));
            break;
        case 2:
            ref = PyInt_FromLong((long)(*refi));
            val = PyInt_FromLong((long)(*vali));
            break;
        case 3:
            ref = PyComplex_FromDoubles((double)(*refc), (double)(*(refc+1)));
            val = PyComplex_FromDoubles((double)(*valc), (double)(*(valc+1)));
            break;
    }
    kwargs = PyDict_New();
    PyDict_SetItemString(kwargs, "ref", ref);
    PyDict_SetItemString(kwargs, "val", val);
    if ( (float)(*compare) != 1. ) {
        comp = PyFloat_FromDouble((double)(*compare));
        PyDict_SetItemString(kwargs, "compare", comp);
    }

    res = PyObject_Call(func, args, kwargs);
    if (!res) {
       MYABORT("erreur lors de l'appel a testresu_print");
    }

    Py_DECREF(res);
    Py_XDECREF(func);
    Py_XDECREF(args);
    Py_XDECREF(kwargs);
    Py_XDECREF(ref);
    Py_XDECREF(val);
    Py_XDECREF(comp);
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
    /*
     * Interface Python d'appel à la routine matfpe.
     */
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
 * Functions to communicate the execution status in parallel
 */
static PyObject* aster_mpi_info(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
    /* Get MPI informations on the current communicator */
    PyObject *res;
    int rank, size;
    aster_get_mpi_info(aster_get_current_comm(), &rank, &size);
    res = Py_BuildValue("ii", rank, size);
    return res;
}

static PyObject* aster_mpi_warn(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
    /* call ASMPI_WARN */
    try {
        CALL_ASMPI_WARN();
    }
    exceptAll {
        raiseException();
    }
    endTry();
    Py_INCREF( Py_None );
    return Py_None;
}

static PyObject* aster_mpi_barrier(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
    int iret;
    /* Set a MPI barrier */
    if ( aster_set_mpi_barrier(aster_get_current_comm()) ) {
        return NULL;
    }
    Py_INCREF( Py_None );
    return Py_None;
}

static char aster_mpi_bcast_doc[] =
"values = MPI_Bcast(list_or_tuple, root)\n\n"
"Broadcasts a list/tuple from one process to all other processes.\n"
"All items of the list/tuple must have the same type (int or double).\n"
"Returns the values of the root processor.";

static PyObject* aster_mpi_bcast_py(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
    /*! Broadcasts a list/tuple from one process to all other processes
        All items of the list/tuple must have the same type (int or double).
     */
    PyObject *tupl, *item, *res;
    int count, root;
    int iret, idecref;
    MPI_Datatype type;
    void *buff;
    int i;

    if ( !PyArg_ParseTuple(args, "Oi:MPI_Bcast", &tupl, &root) ) return NULL;

    idecref = 0;
    if ( PyList_Check(tupl) ) {
        DEBUG_MPI("convert <%s> to <%s>\n", "list", "tuple");
        idecref = 1;
        tupl = PyList_AsTuple(tupl);
    }
    if ( ! PyTuple_Check(tupl) ) {
        return NULL;
    } else {
        count = (int)PyTuple_Size(tupl);
        item = PyTuple_GetItem(tupl, 0);
        if ( PyLong_Check(item) || PyInt_Check(item) ) {
            type = MPI_INTEGER8;
            buff = (void *)malloc((size_t)count * sizeof(INTEGER));
            convert(count, tupl, (INTEGER *)buff);
        } else if ( PyFloat_Check(item) ) {
            type = MPI_DOUBLE_PRECISION;
            buff = (void *)malloc((size_t)count * sizeof(DOUBLE));
            convr8(count, tupl, (DOUBLE *)buff);
        } else {
            // unsupported type
            return NULL;
        }
    }
    if (idecref == 1) {
        DEBUG_MPI("DECREF %s <%s>\n", "converted", "tuple");
        Py_DECREF(tupl);
    }

    /* `buff` is unchanged without MPI and directly used to create `res` */
    DEBUG_MPI("Broadcast from %d for %d values\n", root, count);
    iret = aster_mpi_bcast(buff, count, type, root, aster_get_current_comm());
    if ( iret != 0 ) {
        free(buff);
        return NULL;
    }
    if ( type == MPI_INTEGER8 ) {
        res = MakeTupleInt(count, (INTEGER *)buff);
    } else if ( type == MPI_DOUBLE_PRECISION ) {
        res = MakeTupleFloat(count, (DOUBLE *)buff);
    }
    return res;
}

static char aster_mpi_gather_str_doc[] =
"tuple_of_strings = MPI_GatherStr(string, root)\n\n"
"Gathers a string from a group of processes.\n"
"Returns a tuple of strings, one by processor.";

static PyObject* aster_mpi_gather_str(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
    /*! Gathers a string from a group of processes.
        Returns a tuple of strings, one by processor.
     */
    PyObject *tupl, *item, *res;
    char *instr, *arraystr, *wrk;
    int count, total, root, rank, size;
    int *length, *displ;
    int iret, idecref;
    MPI_Datatype type;
    void *buff;
    int i;
    aster_comm_t *node;

    if ( !PyArg_ParseTuple(args, "s#i:MPI_GatherStr", &instr, &count, &root) ) return NULL;

#ifdef _USE_MPI
    // root gathers the size of the string on each processor
    node = aster_get_current_comm();
    aster_get_mpi_info(node, &rank, &size);
    length = (int *)malloc(size * sizeof(int));
    iret = aster_mpi_gather((void *)&count, 1, MPI_INTEGER4,
                            (void *)length, 1, MPI_INTEGER4,
                            root, node);
    if ( iret != 0 ) {
        free(length);
        return NULL;
    }

    displ = (int *)malloc(size * sizeof(int));
    total = 0;
    if (rank == root) {
        displ[0] = 0;
        for (i = 0; i < size; i++) {
            total += length[i];
            if ( i < size -1 ) {
                displ[i + 1] = displ[i] + length[i];
            }
            DEBUG_MPI("string length = %d, displ = %d\n", length[i], displ[i]);
        }
        DEBUG_MPI("%s = %d\n", "total length", total);
    }

    arraystr = (char *)malloc(total * sizeof(char));
    iret = aster_mpi_gatherv((void *)instr, count, MPI_CHAR,
                             (void *)arraystr, length, displ, MPI_CHAR,
                             root, node);
    if ( iret != 0 ) {
        free(displ);
        free(length);
        return NULL;
    }

    if (rank == root) {
        res = PyTuple_New((Py_ssize_t)size);
        for (i = 0; i < size; i++) {
            wrk = (char*)malloc((length[i] + 1) * sizeof(char));
            strncpy(wrk, &arraystr[displ[i]], length[i]);
            wrk[length[i]] = '\0';
            DEBUG_MPI("arraystr: %s (len=%d)\n", wrk, (int)strlen(wrk));
            if( PyTuple_SetItem(res, i, PyString_FromString(wrk))) {
                Py_DECREF(res);
                free(wrk);
                free(displ);
                free(length);
                return NULL;
            }
            free(wrk);
        }
    } else {
        Py_INCREF(Py_None);
        res = Py_None;
    }
    free(length);
    free(arraystr);
#else
    // without MPI, create a tuple with in input string
    res = PyTuple_New((Py_ssize_t)1);
    if( PyTuple_SetItem(res, 0, PyString_FromString(instr))) {
        Py_DECREF(res);
        return NULL;
    }
#endif
    return res;
}


/*
 * Methods of the aster_core module.
 */
static PyMethodDef methods[] = {
    { "register",       register_jdc,        METH_VARARGS, register_jdc_doc },
    { "matfpe",         asterc_matfpe,       METH_VARARGS, matfpe_doc },
    { "get_mem_stat",   asterc_get_mem_stat, METH_VARARGS, get_mem_stat_doc },
    { "set_mem_stat",   asterc_set_mem_stat, METH_VARARGS, set_mem_stat_doc },
    { "MPI_CommRankSize", aster_mpi_info,    METH_VARARGS},
    { "MPI_Warn",       aster_mpi_warn,      METH_VARARGS},
    { "MPI_Barrier",    aster_mpi_barrier,   METH_VARARGS},
    { "MPI_Bcast",      aster_mpi_bcast_py,  METH_VARARGS, aster_mpi_bcast_doc},
    { "MPI_GatherStr",  aster_mpi_gather_str, METH_VARARGS, aster_mpi_gather_str_doc },
    // { "get_option",  ... } : method added in register_jdc
    // { "set_info",  ... } : method added in register_jdc
    { NULL, NULL, 0, NULL }
};

#ifndef _WITHOUT_PYMOD_
PyMODINIT_FUNC init_aster_core(void)
{
    aster_core = Py_InitModule("_aster_core", methods);

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
    PyModule_AddIntMacro(aster_core, ASTER_INT_SIZE);
}
#endif
