/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF astermodule supervis  DATE 02/02/2011   AUTEUR COURTOIS M.COURTOIS */
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
/* RESPONSABLE                                  COURTOIS M.COURTOIS   */
/* ------------------------------------------------------------------ */

#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include "Python.h"

#include "aster.h"
#include "aster_fort.h"

/* --- declarations des interfaces des fonctions de ce fichier --- */

static PyObject *aster_argv( _UNUSED  PyObject *self, _IN PyObject *args ) ;

int EstPret( _IN char *chaine , _IN STRING_SIZE longueur ) ;
STRING_SIZE FindLength( _IN char *chaineFortran , _IN STRING_SIZE longueur ) ;
void AfficheChaineFortran( _IN char *chaine , _IN STRING_SIZE longueur ) ;
void TraiteMessageErreur( _IN char* ) ;
void PRE_myabort( _IN const char *nomFichier , _IN const int numeroLigne , _IN const char *message ) ;
#define MYABORT(message) PRE_myabort( __FILE__ , __LINE__ , message )


char * fstr1( _IN char *s, _IN STRING_SIZE l) ;
char * fstr2( _IN char *s, _IN STRING_SIZE l) ;
char * fstr3( _IN char *s, _IN STRING_SIZE l) ;
void convert( _IN int nval, _IN PyObject *tup, _OUT INTEGER *val) ;
void convertxt( _IN int nval, _IN PyObject *tup, _OUT char *val, _IN STRING_SIZE taille) ;
void converltx( _IN int nval, _IN PyObject *tup, _OUT char *val, _IN STRING_SIZE taille) ;

void TraitementFinAster( _IN int val ) ;

PyObject * MakeTupleString(long nbval,char *kval,STRING_SIZE lkval,INTEGER *lval);
PyObject * MakeListString(long nbval,char *kval,STRING_SIZE lkval );
PyObject * MakeTupleInt(long nbval, INTEGER* kval);
PyObject * MakeListInt(long nbval, INTEGER* kval);
PyObject * MakeTupleFloat(long nbval,DOUBLE* kval);
PyObject * MakeListFloat(long nbval,DOUBLE* kval);

/* --- FIN declarations des interfaces des fonctions de ce fichier --- */



#define _UTILISATION_SETJMP_
/*
 *   Emulation d'exceptions en C : on utilise le couple de fonctions systemes setjmp/longjmp
 *   pour reproduire le comportement des exceptions en C.
 *   Pour initier une exception, le Fortran doit appeler la fonction XFINI
 *   avec en argument le code de l'exception.
 *   La fonction XFINI fait appel à longjmp pour effectuer le debranchement necessaire.
 *
 *   La variable exception_flag indique comment toute anomalie intervenant pendant
 *   le try doit etre traitée :  par une exception (si try(1)) ou par un abort (si try(0))
 */

#define CodeAbortAster     18
#define CodeFinAster       19

static PyObject* exception_args = NULL;

int exception_status=-1;

#define NIVMAX 10
static int niveau=0;

#ifdef _UTILISATION_SETJMP_
#include <setjmp.h>

static jmp_buf env[NIVMAX+1] ;   /* utilise par longjmp, le type jmp_buf est defini dans setjmp.h */
static int exception_flag[NIVMAX+1];

#define try(val) exception_flag[niveau]=val;if((exception_status = setjmp(env[niveau])) == 0)
#define catch(val) else if (exception_status == val)
#define throw(val) longjmp(env[niveau],val)
#define finally else

void TraiteErreur( _IN int code )
{
        if(exception_flag[niveau]==1){
          exception_flag[niveau]=0;
          throw(code);
        }
        else{
          abort();
        }
}

#else
#define try(val) if(1)
#define catch(val) else if (0)
#define throw(val)
#define finally else

void TraiteErreur( _IN int code )
{
        switch( code ){

        case CodeFinAster :
                exit(0);
                break ;
        default :
                INTERRUPTION(1) ;
                break ;
        }
}

#endif                /* #ifdef _UTILISATION_SETJMP_ */

/* Fin emulation exceptions en C */



/* --- liste des variables globales au fonctions  de ce fichier --- */

/* jeveux_status vaut :
      0 avant aster_init,
      1 pendant l'exécution,
      0 après xfini
   Cela permet de verrouiller l'appel à des fonctions jeveux hors de l'exécution
*/
static int jeveux_status = 0;

/* commande (la commande courante) est definie par les fonctions aster_debut et aster_oper */
static PyObject *commande       = (PyObject*)0 ;
static PyObject *pile_commandes = (PyObject*)0 ;
static PyObject *static_module  = (PyObject*)0 ;
static PyObject *except_module = NULL;

/* NomCas est initialise dans aster_debut() */
/* NomCas est initialise a blanc pour permettre la recuperation de la
   trace des commandes lors de l'appel a debut ou poursuite. On ne connait
   pas encore NomCas qui sera initialise lors de l'appel a RecupNomCas */
static char *NomCas          = "        ";

/* ------------------------------------------------------------------ */
#define CALL_ISJVUP() STDCALL(ISJVUP, isjvup)()

INTEGER STDCALL(ISJVUP, isjvup)()
{
   /* "is jeveux up ?" : retourne 1 si jeveux est démarré/initialisé, sinon 0. */
   return (INTEGER)jeveux_status;
}


/* ------------------------------------------------------------------ */
void STDCALL(XFINI,xfini)(_IN INTEGER *code)
{
   /* XFINI est n'appelé que par JEFINI avec code=19 (=CodeFinAster) */
   /* jeveux est fermé */
   jeveux_status = 0;

   TraiteErreur(*code);
}
/* ------------------------------------------------------------------ */

void initExceptions(PyObject *dict)
{
    /* 
     * Les exceptions du module 'aster' sont définies dans Execution/E_Exception.py.
     * Elles sont ajoutées au module par la fonction add_to_dict_module.
     */
    PyObject *res;
    
    except_module = PyImport_ImportModule("Execution.E_Exception");
    if ( ! except_module ) {
        fprintf(stderr, "\n\nWARNING:\n    ImportError of Execution.E_Exception module!\n");
        fprintf(stderr, "    No exception defined in the aster module.\n");
        fprintf(stderr, "    It may be unusable.\n\n");
        PyErr_Clear();
        Py_XDECREF(except_module);
        return;
    }
    
    /* affectation du dict par le module E_Exception */
    res = PyObject_CallMethod(except_module, "add_to_dict_module", "O", dict);
    Py_DECREF(res);
    Py_DECREF(except_module);
}

/* ------------------------------------------------------------------ */
void DEFPSPSPPPP(UEXCEP,uexcep, _IN INTEGER *exc_type,
                                _IN char *idmess, _IN STRING_SIZE lidmess,
                                _IN INTEGER *nbk, _IN char *valk, _IN STRING_SIZE lvk,
                                _IN INTEGER *nbi, _IN INTEGER *vali,
                                _IN INTEGER *nbr, _IN DOUBLE *valr)
{
   /*
      Interface Fortran/Python pour lever une exception avec les arguments complets
   */
    PyObject *tup_valk, *tup_vali, *tup_valr;
    char *kvar;
    int i;

    tup_valk = PyTuple_New( *nbk ) ;
    for(i=0;i<*nbk;i++){
       kvar = valk + i*lvk;
       PyTuple_SetItem( tup_valk, i, PyString_FromStringAndSize(kvar,lvk) ) ;
    }

    tup_vali = PyTuple_New( *nbi ) ;    
    for(i=0;i<*nbi;i++){
       PyTuple_SetItem( tup_vali, i, PyInt_FromLong(vali[i]) ) ;
    }

    tup_valr = PyTuple_New( *nbr ) ;
    for(i=0;i<*nbr;i++){
       PyTuple_SetItem( tup_valr, i, PyFloat_FromDouble(valr[i]) ) ;
    }

    exception_args = PyTuple_New( 4 );
    PyTuple_SetItem( exception_args, (Py_ssize_t)0, PyString_FromStringAndSize(idmess, lidmess) );
    PyTuple_SetItem( exception_args, (Py_ssize_t)1, tup_valk );
    PyTuple_SetItem( exception_args, (Py_ssize_t)2, tup_vali );
    PyTuple_SetItem( exception_args, (Py_ssize_t)3, tup_valr );

    TraiteErreur((int)*exc_type);
}



static char nom_fac[256];        /* utilise par fstr1 */
static char nom_cle[256];        /* utilise par fstr2 */
static char nom_cmd[256];        /* utilise par fstr3 */


/* --- FIN liste des variables globales au fonctions  de ce fichier --- */


/*
 *   Ce module crée de nombreux objets Python. Il doit respecter les règles
 *   générales de création des objets et en particulier les règles sur le
 *   compteur de références associé à chaque objet.
 *   Tous les objets sont partagés. Seules des références à des objets peuvent
 *   etre acquises.
 *   Si une fonction a acquis une référence sur un objet elle doit la traiter
 *   proprement, soit en la transférant (habituellement à l'appelant), soit en
 *   la relachant (par appel à Py_DECREF ou Py_XDECREF).
 *   Quand une fonction transfere la propriété d'une référence, l'appelant recoit
 *   une nouvelle référence. Quand la propriété n'est pas transférée, l'appelant
 *   emprunte la référence.
 *   Dans l'autre sens, quand un appelant passe une référence à une fonction, il y a
 *   deux possibilités : la fonction vole une référence à l'objet ou elle ne le fait
 *   pas. Peu de fonctions (de l'API Python) volent des références : les deux exceptions
 *   les plus notables sont PyList_SetItem() et PyTuple_SetItem() qui volent une
 *   référence à l'item qui est inséré dans la liste ou dans le tuple.
 *   Ces fonctions qui volent des références existent, en général, pour alléger
 *   la programmation.
 */
/* ------------------------------------------------------------------ */

void TraiteMessageErreur( _IN char * message )
{
        printf("%s\n",message);
        if(PyErr_Occurred())PyErr_Print();
        abort();
}

/* ------------------------------------------------------------------ */
void PRE_myabort( _IN const char *nomFichier , _IN const int numeroLigne , _IN const char *message )
{
        /*
        Procedure : PRE_myabort
        Intention
                Cette procedure prepare la chaine de caracteres affichee par TraiteMessageErreur()
                en ajoutant devant cette chaine, le nom du fichier source et le numero
                de la ligne a partir desquels PRE_myabort a ete appelee.
                Puis elle appelle elle-meme TraiteMessageErreur().
                Voir aussi la macro MYABORT qui permet de generer automatiquement le nom
                du fichier et le numero de la ligne.
        */
        char *chaine = (char*)0 ;
        int longueur = 0 ;
        void *malloc(size_t size);
                                                        ASSERT(numeroLigne>0);
                                                        ASSERT(((int)log10((float)numeroLigne))<=5);
                                                        ASSERT(nomFichier!=(char*)0) ;
        longueur += strlen( nomFichier ) ;
        longueur += 1 ; /* pour le blanc de separation */
        longueur += 5 ; /* pour le numero de la ligne */
        longueur += 3 ; /* pour les deux points entre deux blancs */
                                                        ASSERT(message!=(const char*)0);
        longueur += ( message != (const char*)0 ) ? strlen( message ) : 0 ;
        longueur += 1 ; /* pour le caractere de fin de chaine */

        chaine = (char*)(malloc(longueur*sizeof(char))) ;
                                                        ASSERT(chaine!=(char*)0);
        sprintf( chaine , "%s %u : %s" , nomFichier , numeroLigne , message ) ;
        TraiteMessageErreur( chaine ) ;

        free( chaine )   ;
        chaine=(char*)0 ;
        longueur = 0     ;
}

/* ------------------------------------------------------------------ */
#define CALL_GETLTX(a,b,c,d,e,f,g) CALLSSPPPPP(GETLTX,getltx,a,b,c,d,e,f,g)

void DEFSSPPPPP(GETLTX,getltx,_IN char *motfac,_IN STRING_SIZE lfac,
                              _IN char *motcle,_IN STRING_SIZE lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,_OUT INTEGER *isval, _OUT INTEGER *nbval )
{
        /*
        Procedure : getltx_ (appelee par le fortran sous le nom GETLTX)
        */
        PyObject *res = (PyObject*)0 ;
        PyObject *tup = (PyObject*)0 ;
        char *mfc     = (char*)0 ;
        char *mcs     = (char*)0 ;
        int ok        = 0 ;
        int nval      = 0 ;
        int ioc       = 0 ;

        mfc=fstr1(motfac,lfac);
                                                        ASSERT(mfc!=(char*)0);
                                                        ASSERT(EstPret(motcle,lcle)!=0);
        mcs=fstr2(motcle,lcle);
                                                        ASSERT(mcs!=(char*)0);
        ioc=(int)*iocc ;
        ioc=ioc-1 ;
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getltx","ssiii",mfc,mcs,ioc,(int)*iarg,(int)*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        ok = PyArg_ParseTuple(res,"iO",&nval,&tup);
        if (!ok)MYABORT("erreur dans la partie Python");

        *nbval = (INTEGER)nval;
        if( nval < 0 ) nval=(int)*mxval;
        convert(nval,tup,isval);
        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return ;
}


/* ------------------------------------------------------------------ */
char * fstr1( _IN char *s, _IN STRING_SIZE l)
{
        /*
        copie l caracteres d'une chaine de caracteres fortran s dans la chaine
        statique globale nom_fac, et retourne un pointeur sur nom_fac
        */
        strncpy(nom_fac, s, l );
        nom_fac[l]='\0';
        return nom_fac;
}
char * fstr2( _IN char *s, _IN STRING_SIZE l)
{
        /*
        copie l caracteres d'une chaine de caracteres fortran s dans la chaine
        statique globale nom_cle, et retourne un pointeur sur nom_cle
        */
        strncpy(nom_cle, s, l );
        nom_cle[l]='\0';
        return nom_cle;
}
char * fstr3( _IN char *s, _IN STRING_SIZE l)
{
        /*
        copie l caracteres d'une chaine de caracteres fortran s dans la chaine
        statique globale nom_cmd, et retourne un pointeur sur nom_cmd
        */
        strncpy(nom_cmd, s, l );
        nom_cmd[l]='\0';
        return nom_cmd;
}


/* ------------------------------------------------------------------ */
void DEFSP(GETFAC,getfac,_IN char *nomfac, _IN STRING_SIZE lfac, _OUT INTEGER *occu)
{
        /*
          Procedure GETFAC pour le FORTRAN : emule le fonctionnement
          de la procedure equivalente ASTER
          Entrees :
            le nom d un mot cle facteur : nomfac (string)
          Retourne :
            le nombre d occurence de ce mot cle dans les args : occu (entier)
            dans l'etape (ou la commande) courante
        */
        PyObject *res  = (PyObject*)0 ;
                                                        ASSERT(EstPret(nomfac,lfac)!=0);
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getfac","s",fstr1(nomfac,lfac));

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        *occu=(INTEGER)PyInt_AsLong(res);

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return ;
}


/* ------------------------------------------------------------------ */
void convc8( _IN int nval, _IN PyObject *tup, _OUT DOUBLE *val)
{
        /*
                  tup est un tuple de tuples internes, chaque tuple
                interne contenant le type et les deux parties du complexe.
        */
        int    i = 0 ;
        int    k = 0 ;
        int conv_un_c8( _IN PyObject *tup, _OUT DOUBLE *val) ;
                                                                    ASSERT(PyTuple_Check(tup)) ;
        if(nval != 0){
                PyObject *v = (PyObject*)0 ;
                                                                    ASSERT(nval>0) ;
                for(i=0;i<nval;i++){
                        v=PyTuple_GetItem(tup,i);
                        k += conv_un_c8( v , val+k ) ;
                }
        }
        return ;
}

/* ------------------------------------------------------------------ */
int conv_un_c8( _IN PyObject *tup, _OUT DOUBLE *val)
{

        /* Enrichissement des complexes stockes dans val a partir du tuple tup */

        char *repres = (char*)0 ; /* representation "RI" (reelle/imaginaire) ou "MP" (module phase) */
        double x = 0.0 ;
        double y = 0.0 ;
        double *rho = &x ;
        double *theta = &y ;
        if(PyComplex_Check(tup)||PyFloat_Check(tup)||PyLong_Check(tup)||PyInt_Check(tup)){
           /* On est dans le cas d'un objet Python complexe */
           /* representation : partie reelle/partie imaginaire */
           *val    =(DOUBLE)PyComplex_RealAsDouble(tup)  ;
           *(val+1)=(DOUBLE)PyComplex_ImagAsDouble(tup)  ;
        }
        else if(PyTuple_Check(tup)){
           /* On est dans le cas d'un complexe représenté par un triplet : "RI" ou "MP",x,y */
           if(!PyArg_ParseTuple(tup,"sdd",&repres,&x,&y))
                     MYABORT("erreur dans la partie Python");
                                                                                     ASSERT((strcmp(repres,"RI")==0)||(strcmp(repres,"MP")==0)) ;
           if (strcmp(repres,"RI")==0){
                /* representation : partie reelle/partie imaginaire */
                *val    =(DOUBLE)x ;
                *(val+1)=(DOUBLE)y ;
           }
           else{
                /* representation RHO,THETA (les angles sont fournis en degres) */
                *val    =(DOUBLE)(*rho * cos( *theta /180. * R8PI()) );
                *(val+1)=(DOUBLE)(*rho * sin( *theta /180. * R8PI()) );
           }
        }
        else {
           MYABORT("erreur dans la partie Python");
        }
        return 2 ;
}


/* ------------------------------------------------------------------ */
void convr8( _IN int nval, _IN PyObject *tup, _OUT DOUBLE *val)
{
        /* Convertit un Tuple en tableau de double */
        int i;
        PyObject *v = (PyObject*)0 ;
        if(nval == 0)return;
        if (!PyTuple_Check(tup)){
                printf("tup : ");
                PyObject_Print(tup, stdout, 0);
                printf("\n ");
                MYABORT("erreur sur le type : devrait etre un tuple");
        }
        for(i=0;i<nval;i++){
                v=PyTuple_GetItem(tup,i);
                val[i]=(DOUBLE)PyFloat_AsDouble(v);
        }
        return ;
}


/* ------------------------------------------------------------------ */
void convert( _IN int nval, _IN PyObject *tup, _OUT INTEGER *val)
{
        /* Convertit un Tuple en tableau d entier */
        int i;
        PyObject *v = (PyObject*)0 ;
        if(nval == 0)return;
        if (!PyTuple_Check(tup)){
                printf("tup : ");
                PyObject_Print(tup, stdout, 0);
                printf("\n ");
                MYABORT("erreur sur le type : devrait etre un tuple");
        }
        for(i=0;i<nval;i++){
                v=PyTuple_GetItem(tup,i);
                val[i]=(INTEGER)PyInt_AsLong(v);
        }
        return ;
}


/* ------------------------------------------------------------------ */
void convertxt( _IN int nval, _IN PyObject *tup, _OUT char *val, _IN STRING_SIZE taille)
{
        /*
        Convertit un Tuple en tableau de chaines
        Pour retour au Fortran : le tableau existe deja (val)
           nval   : indique le nombre d'elements du tuple a convertir
           tup    : est le tuple Python a convertir
           val    : est le tableau de chaines Fortran a remplir
           taille : indique la taille des chaines
        */
                                                                   ASSERT(PyTuple_Check(tup)) ;
        if(nval != 0){
                PyObject *v  = (PyObject*)0 ;
                int i;
                char *s      = (char*)0 ;
                char *val_i      = (char*)0 ;
                int longueur = 0 ;
                                                                   ASSERT(nval>0) ;
                                                                   ASSERT(taille>0) ;
                if (!PyTuple_Check(tup)){
                        printf("tup : ");
                        PyObject_Print(tup, stdout, 0);
                        printf("\n ");
                        MYABORT("erreur sur le type : devrait etre un tuple");
                }
                for(i=0;i<nval;i++){
                        v=PyTuple_GetItem(tup,i);
                        /*                               v=PySequence_GetItem(tup,i); */
                        s=PyString_AsString(v);
                        if(s == NULL){
                                printf("s : ");
                                PyObject_Print(v, stdout, 0);
                                printf("\n ");
                                MYABORT("erreur sur le type : devrait etre une string");
                        }

                        /* le fortran attend des chaines de caracteres completees par des blancs */
                        longueur=strlen(s);
                        val_i=&val[i*taille];
                        STRING_FCPY(val_i,taille,s,longueur);
                }
        }
}


/* ------------------------------------------------------------------ */
void converltx( _IN int nval, _IN PyObject *tup, _OUT char *val, _IN STRING_SIZE taille)
{
        /*
        Convertit une Liste  en tableau de chaines
        Pour retour au Fortran : le tableau existe deja (val)
        */
        PyObject *v = (PyObject*)0 ;
        int i;
        char *s = (char*)0 ;
        char *val_i      = (char*)0 ;
        int longueur=0 ;

        if(nval != 0){
                if (!PyList_Check(tup)){
                        printf("tup : ");
                        PyObject_Print(tup, stdout, 0);
                        printf("\n ");
                        MYABORT("erreur sur le type : devrait etre une liste");
                }
                for(i=0;i<nval;i++){
                        v=PyList_GetItem(tup,i);
                        /* v=PySequence_GetItem(tup,i); */
                        s=PyString_AsString(v);
                        if(s == NULL){
                                printf("s : ");
                                PyObject_Print(v, stdout, 0);
                                printf("\n ");
                                MYABORT("erreur sur le type : devrait etre une string");
                        }

                        /* le fortran attend des chaines de caracteres completees par des blancs */
                        longueur=strlen(s);
                        val_i=&val[i*taille];
                        STRING_FCPY(val_i,taille,s,longueur);
                }
        }
        return ;
}


/* ------------------------------------------------------------------ */
void STDCALL(GETRAN,getran)(_OUT DOUBLE *rval)
{
        /*
          Procedure GETRAN pour le FORTRAN : recupere un réel aleatoire (loi uniforme 0-1) du module python Random
          Entrees :
            neant
          Retourne :
            un reel tiré au hasard
        */
        PyObject *res  = (PyObject*)0 ;
        PyObject *val  = (PyObject*)0 ;
        int ok=0;

        res=PyObject_CallMethod(commande,"getran","");
        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        ok = PyArg_ParseTuple(res,"O",&val);
        if(!ok)MYABORT("erreur dans la partie Python");

        *rval=(DOUBLE)PyFloat_AsDouble(val);

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return ;
}


/* ------------------------------------------------------------------ */
void DEFP(INIRAN,iniran,_IN INTEGER *jump)
{
        /*
          Procedure INIRAN pour le FORTRAN : recupere un réel aleatoire (loi uniforme 0-1) du module python Random
          avec un shift eventuel de jump termes
        */
        PyObject *res  = (PyObject*)0 ;

        res=PyObject_CallMethod(commande,"iniran","i",(int)*jump);
        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return ;
}


/* ------------------------------------------------------------------ */
void DEFSS(GETTCO,gettco,_IN char *nomobj, _IN STRING_SIZE lnom,
                        _OUT char *typobj, _IN STRING_SIZE ltyp)
{
        /*
          retrouver le type "superviseur" du concept nomobj.
        */
        char *mcs      = (char*)0 ;
        PyObject *res  = (PyObject*)0 ;
        char *nomType  = (char*)0 ;
        int longueur   = 0 ;
                                                              ASSERT(lnom>0) ;
        mcs=fstr2(nomobj,lnom);

        /*
        recherche dans le jeu de commandes python du nom du type de
         du concept Aster de nom nomobj
        */
                                                              ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"gettco","s",mcs);
        if (res == (PyObject*)0)MYABORT("erreur dans la partie Python (gettco)");
                                                              ASSERT( PyString_Check(res) )
        nomType=PyString_AsString(res);
                                                              ASSERT(nomType!=(char*)0) ;
        longueur = strlen(nomType) ;
                                                              ASSERT(longueur>0) ;
                                                              ASSERT(longueur<=ltyp) ;
        STRING_FCPY(typobj,ltyp,nomType,longueur);
                                                              ASSERT(EstPret(typobj,ltyp)) ;
        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return ;
}


/* ------------------------------------------------------------------ */
void DEFPS(GETMAT,getmat,_OUT INTEGER *nbarg,_OUT char *motcle,_IN STRING_SIZE lcle)
{
        /*
          Procedure GETMAT pour le FORTRAN
          Routine a l usage de DEFI_MATERIAU : consultation du catalogue (et non de l etape)
          Retourne :
            le nombre de mots cles facteur sous la commande, y compris en eliminant les blocs
            la liste de leur noms
        */
        PyObject *res   = (PyObject*)0 ;
        PyObject *lnom  = (PyObject*)0 ; /* liste python des noms */
        int       nval = 0 ;
        int          k = 0 ;
                                                                        ASSERT(lcle>0);
        for ( k=0 ;k<lcle ; k++ ) motcle[k]=' ' ;
                                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getmat","");
        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");
        /*  si non impression du retour */

        if(!PyArg_ParseTuple(res,"O",&lnom)) MYABORT("erreur dans la partie Python");
        nval=PyList_Size(lnom);
        *nbarg = (INTEGER)nval ;

        if ( nval > 0 ){
                converltx(nval,lnom,motcle,lcle); /* conversion  */
        }

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return ;
}


/* ------------------------------------------------------------------ */
void DEFSPPSSP(GETMJM,getmjm,_IN char *nomfac,_IN STRING_SIZE lfac,
                             _IN INTEGER *iocc,_IN INTEGER *nbval,
                            _OUT char *motcle,_IN STRING_SIZE lcle,
                            _OUT char *type,_IN STRING_SIZE ltyp, _OUT INTEGER *nbarg)
{
        /*
          Procedure GETMJM : emule la procedure equivalente ASTER
           Retourne les nbval premiers mots cles du mot cle facteur nomfac du catalogue de la commande en cours
          Entrees :
           nomfac : nom du mot cle facteur
           iocc   : numero d occurence du mot cle facteur
           nbval  : nombre de mots cles facteurs demandes
          Retourne :
           motcle : liste des mots cles du mot cle facteur demande
           type   : liste des types des mots cles du mot cle facteur demande
                    R8 , R8L : un reel ou une liste de reels ;
                    C8 , C8L : un complexe ou une liste de complexes ;
                     ...
                    CO , COL : un concept ou une liste de concepts.
           nbarg  : nombre d arguments des mots cles du mot cle facteur
        */

        PyObject *res   = (PyObject*)0 ;
        PyObject *lnom  = (PyObject*)0 ;
        PyObject *lty   = (PyObject*)0 ; /* liste python des noms */
        int       nval = 0 ;
        int          k = 0 ;
        int        ioc = 0 ;
                                                                        ASSERT(ltyp>0);
        for ( k=0 ;k<ltyp ; k++ ) type[k]=' ' ;
        ioc=(int)*iocc ;
        ioc=ioc-1 ;
                                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getmjm","sii",fstr2(nomfac,lfac),ioc,(int)*nbval);
        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");
        /*  si non impression du retour */

        if(!PyArg_ParseTuple(res,"OO",&lnom,&lty)) MYABORT("erreur dans la partie Python");
        nval=(int)PyList_Size(lnom);
        *nbarg = (INTEGER)( (nval > *nbval) ? -nval : nval );
                                                                        ASSERT(((nval<=*nbval)&&(*nbarg==nval))||(*nbarg==-nval)) ;
        if(*nbarg < 0)nval=(int)*nbval;

        if ( nval > 0 ){
                converltx(nval,lnom,motcle,lcle); /* conversion  */
                converltx(nval,lty,type,ltyp);
       }

        /*
        A la demande des developpeurs (J. Pellet), le nom des concepts retourne par
        la methode EXECUTION.getmjm (par exemple grma) est ici remplace par
        la chaine CO (pour COncept).
        les types retournes sont donc parmi les valeurs : R8 , C8 , IS , TX et CO.
        */
        for( k=0 ; k<nval*ltyp ; k+=ltyp ){
                char     *mot = (char*)0 ;
                mot           = type+k ;
                if ( strncmp( mot , "R8" , 2 )!=0 && strncmp( mot , "IS" , 2 )!=0 && strncmp( mot , "TX" , 2 )!=0 && strncmp( mot , "C8" , 2 )!=0 ){
                        int j=0 ;

                        ASSERT(ltyp>2);
                        mot[0]='C' ;
                        mot[1]='O' ;
                        for ( j=2 ; j<ltyp ; j++ ) mot[j]=' ' ;
                }
        }
        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return ;
}


/* ------------------------------------------------------------------ */
INTEGER DEFSS( GETEXM, getexm, _IN char *motfac,_IN STRING_SIZE lfac,
                               _IN char *motcle,_IN STRING_SIZE lcle)
{
        /*
          Procedure GETEXM pour le FORTRAN : emule le fonctionnement
          de la procedure equivalente ASTER
          Entrees :
            le nom d un mot cle facteur : motfac (string)
            le nom d un mot cle simple ou sous mot cle : motcle (string)
          Retourne :
            0 si n existe pas 1 si existe
        */
        PyObject *res  = (PyObject*)0 ;
        INTEGER presence;
                                                                        ASSERT(motcle!=(char*)0);
                                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getexm","ss",
                                fstr1(motfac,lfac),fstr2(motcle,lcle));
        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");
        presence = (INTEGER)PyLong_AsLong(res);
        /*  decrement sur le refcount du retour */
        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return presence;
}


/* ------------------------------------------------------------------ */
void DEFSSS( GETRES ,getres, _OUT char *nomres, _IN STRING_SIZE lres,
                             _OUT char *concep, _IN STRING_SIZE lconc,
                             _OUT char *nomcmd, _IN STRING_SIZE lcmd)
{
        /*
          Procedure GETRES pour le FORTRAN : emule le fonctionnement
          de la procedure equivalente ASTER
          Retourne
            le nom utilisateur du resultat : nomres (string)
            le nom du concept resultat     : concep (string)
            le nom de la commande          : nomcmd (string)
        */
        PyObject *res  = (PyObject*)0 ;
        int ok;
        int s1,s2,s3;
        char *ss1,*ss2,*ss3;

        /* (MC) le 1er test ne me semble pas suffisant car entre deux commandes,
           commande n'est pas remis à (PyObject*)0... */
        if(commande == (PyObject*)0 || PyObject_HasAttrString(commande, "getres")==0) {
          /* Aucune commande n'est active on retourne des chaines blanches */
          BLANK(nomres,lres);
          BLANK(concep,lconc);
          BLANK(nomcmd,lcmd);
          return ;
        }
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getres","");
        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL){
          /* Aucune commande n'est active on retourne des chaines blanches */
          BLANK(nomres,lres);
          BLANK(concep,lconc);
          BLANK(nomcmd,lcmd);
          return ;
        }

        ok = PyArg_ParseTuple(res,"s#s#s#",&ss1,&s1,&ss2,&s2,&ss3,&s3);
        if (!ok)MYABORT("erreur dans la partie Python");

        /* le fortran attend des chaines de caracteres completees par des blancs */
        STRING_FCPY(nomres,lres,ss1,s1);
        STRING_FCPY(concep,lconc,ss2,s2);
        STRING_FCPY(nomcmd,lcmd,ss3,s3);

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return ;
}


/* ------------------------------------------------------------------ */
void DEFSSPPPPP(GETVC8,getvc8,_IN char *motfac,_IN STRING_SIZE lfac,
                              _IN char *motcle,_IN STRING_SIZE lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,
                           _INOUT DOUBLE *val,_OUT INTEGER *nbval)
{
        /*
          Procedure GETVC8 pour le FORTRAN : emule le fonctionnement
          de la procedure equivalente ASTER
          Entrees :
            le nom d un mot cle facteur : motfac (string)
            le nom d un mot cle simple ou sous mot cle : motcle (string)
            le numero de l occurence du mot cle facteur : iocc (entier)
            le numero de l argument demande (obsolete =1): iarg (entier)
            le nombre max de valeur attendues dans val : mxval (entier)
          Retourne :
            le tableau des valeurs attendues : val (2 reels (double) par complexe)
            le nombre de valeurs effectivement retournees : nbval (entier)
               si pas de valeur nbval =0
               si plus de valeur que mxval nbval <0 et valeur abs = nbre valeurs
               si moins de valeurs que mxval nbval>0 et egal au nombre retourne
        */
        PyObject *res  = (PyObject*)0 ;
        PyObject *tup  = (PyObject*)0 ;
        int ok         = 0 ;
        int nval       = 0 ;
        int ioc        = 0 ;
        char *mfc      = (char*)0 ;
        char *mcs      = (char*)0 ;
                                                        ASSERT(EstPret(motcle,lcle)!=0);
        mfc=fstr1(motfac,lfac);
                                                        ASSERT(mfc!=(char*)0);
        mcs=fstr2(motcle,lcle);
        /*
                VERIFICATION
                Si le mot-cle simple est recherche sous un mot-cle facteur et uniquement dans ce cas,
                le numero d'occurrence (*iocc) doit etre strictement positif.
                Si le mot-cle simple est recherche sans un mot-cle facteur iocc n'est pas utilise

        */
        if( isalpha(mfc[0])&&(*iocc<=0) )
        {
                printf( "<F> GETVC8 : le numero d'occurence (IOCC=%ld) est invalide\n",*iocc) ;
                printf( "             commande : %s\n",PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                printf( "             mot-cle facteur : %s\n",mfc) ;
                printf( "             mot-cle simple  : %s\n",mcs) ;
                MYABORT( "erreur d'utilisation detectee") ;
        }

        ioc=(int)*iocc ;
        ioc=ioc-1 ;
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvc8","ssiii",mfc,mcs,ioc,(int)*iarg,(int)*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");
                                                        ASSERT(PyTuple_Check(res)) ;
        ok = PyArg_ParseTuple(res,"iO",&nval,&tup);
        if(!ok)MYABORT("erreur dans la partie Python");

        *nbval = (INTEGER)nval;
        if ( nval < 0 ) nval=(int)*mxval;
        convc8(nval,tup,val);

        Py_DECREF(res);
        return ;
}


/* ------------------------------------------------------------------ */
void DEFSSPPPPP(GETVR8,getvr8,_IN char *motfac,_IN STRING_SIZE lfac,
                              _IN char *motcle,_IN STRING_SIZE lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT DOUBLE *val,_OUT INTEGER *nbval)
{
        /*
          Procedure GETVR8 pour le FORTRAN : emule le fonctionnement
          de la procedure equivalente ASTER
          Entrees :
            le nom d un mot cle facteur : motfac (string)
            le nom d un mot cle simple ou sous mot cle : motcle (string)
            le numero de l occurence du mot cle facteur : iocc (entier)
            le numero de l argument demande (obsolete =1): iarg (entier)
            le nombre max de valeur attendues dans val : mxval (entier)
          Retourne :
            le tableau des valeurs attendues : val (tableau de R8    )
            le nombre de valeurs effectivement retournees : nbval (entier)
               si pas de valeur nbval =0
               si plus de valeur que mxval nbval <0 et valeur abs = nbre valeurs
               si moins de valeurs que mxval nbval>0 et egal au nombre retourne
        */
        PyObject *res  = (PyObject*)0 ;
        PyObject *tup  = (PyObject*)0 ;
        int ok         = 0 ;
        int nval       = 0 ;
        int ioc        = 0 ;
        char *mfc      = (char*)0 ;
        char *mcs      = (char*)0 ;
                                                        ASSERT(EstPret(motcle,lcle)!=0);
        mfc=fstr1(motfac,lfac); /* conversion chaine fortran en chaine C */
                                                        ASSERT(mfc!=(char*)0);
        mcs=fstr2(motcle,lcle);
        /*
                VERIFICATION
                Si le mot-cle simple est recherche sous un mot-cle facteur et uniquement dans ce cas,
                le numero d'occurrence (*iocc) doit etre strictement positif.
                Si le mot-cle simple est recherche sans un mot-cle facteur iocc n'est pas utilise

        */
        if( isalpha(mfc[0])&&(*iocc<=0) )
        {
                printf( "<F> GETVR8 : le numero d'occurence (IOCC=%ld) est invalide\n",*iocc) ;
                printf( "             commande : %s\n",PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                printf( "             mot-cle facteur : %s\n",mfc) ;
                printf( "             mot-cle simple  : %s\n",mcs) ;
                MYABORT( "erreur d'utilisation detectee") ;
        }
        ioc=(int)*iocc ;
        ioc=ioc-1 ;
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvr8","ssiii",mfc,mcs,ioc,(int)*iarg,(int)*mxval);
        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");
                                                       ASSERT(PyTuple_Check(res)) ;
        ok = PyArg_ParseTuple(res,"iO",&nval,&tup);
        if(!ok)MYABORT("erreur dans la partie Python");

        *nbval=(INTEGER)nval;
        if ( nval < 0 ) nval=(int)*mxval;
        if ( nval>0 ){
                convr8(nval,tup,val);
        }

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return ;
}


/* ------------------------------------------------------------------ */
void DEFSSPSPPPP(UTPRIN,utprin, _IN char *typmess, _IN STRING_SIZE ltype,
                                _IN char *idmess, _IN STRING_SIZE lidmess,
                                _IN INTEGER *nbk, _IN char *valk, _IN STRING_SIZE lvk,
                                _IN INTEGER *nbi, _IN INTEGER *vali,
                                _IN INTEGER *nbr, _IN DOUBLE *valr)
{
   /*
      Interface Fortran/Python pour l'affichage des messages
   */
        PyObject *tup_valk,*tup_vali,*tup_valr,*res;
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

        res=PyObject_CallMethod(static_module,"MessageLog","s#s#OOO",typmess,ltype,idmess,lidmess,tup_valk,tup_vali,tup_valr);
        if (!res) {
           MYABORT("erreur lors de l'appel a MessageLog");
        }

        Py_DECREF(tup_valk);
        Py_DECREF(tup_vali);
        Py_DECREF(tup_valr);
        Py_DECREF(res);
}

/* ------------------------------------------------------------------ */
void DEFPP(CHKMSG,chkmsg, _IN INTEGER *info_alarm, _OUT INTEGER *iret)
{
   /*
      Interface Fortran/Python pour la vérification que tout s'est bien
      passé, destinée à etre appelée dans FIN ou au cours d'une commande.
      Argument IN :
         info_alarm = 1  on vérifie si les alarmes ignorées ont été émises ou non.
                    = 0  on ne fait pas cette vérif
      Retourne :
         iret = 0   tout est ok
         iret > 0   erreur
   */
   PyObject *res, *mess_log;

   mess_log = PyObject_GetAttrString(static_module, "MessageLog");
   if (!mess_log) {
      MYABORT("erreur lors de l'acces a l'objet MessageLog.");
   }

   res = PyObject_CallMethod(mess_log, "check_counter", "i", (int)*info_alarm);
   if (!res) {
      MYABORT("erreur lors de l'appel a la methode MessageLog.check_counter");
   }
   *iret = (INTEGER)PyLong_AsLong(res);

   Py_DECREF(res);
   Py_DECREF(mess_log);
}

/* ------------------------------------------------------------------ */
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

   res = PyObject_CallMethod(static_module,"checksd","s#s#",nomsd,lnom,typsd,ltyp);
   if (!res) {
      MYABORT("erreur lors de l'appel a la methode CHECKSD");
   }
   *iret = (INTEGER)PyLong_AsLong(res);

   Py_DECREF(res);
}

/* ------------------------------------------------------------------ */
void DEFSPSPPSP(FIINTF,fiintf,_IN char *nomfon,_IN STRING_SIZE lfon,
                              _IN INTEGER *nbpu,_IN char *param,_IN STRING_SIZE lpara,
                              _IN DOUBLE *val,
                             _OUT INTEGER *iret,
                             _OUT char *msgerr, _INOUT STRING_SIZE lmsg,
                             _OUT DOUBLE *resu)
{
        PyObject *tup3  = (PyObject*)0 ;
        PyObject *res, *txt, *piret;
        PyObject *tup_par;
        PyObject *tup_val;
        char *kvar, *sret;
        int lsret;
        int i;
                                                        ASSERT(commande!=(PyObject*)0);
        tup_par = PyTuple_New( (Py_ssize_t)*nbpu ) ;
        tup_val = PyTuple_New( (Py_ssize_t)*nbpu ) ;
        for(i=0;i<*nbpu;i++){
           kvar = param + i*lpara;
           PyTuple_SetItem( tup_par, i, PyString_FromStringAndSize(kvar,(Py_ssize_t)lpara) ) ;
        }
        for(i=0;i<*nbpu;i++){
           PyTuple_SetItem( tup_val, i, PyFloat_FromDouble((double)val[i]) ) ;
        }

        tup3 = PyObject_CallMethod(commande,"fiintf","s#OO",nomfon,lfon,tup_par,tup_val);

        if (tup3 == NULL)MYABORT("erreur dans la partie Python");
        piret = PyTuple_GetItem(tup3, 0);
        txt   = PyTuple_GetItem(tup3, 1);
        res   = PyTuple_GetItem(tup3, 2);

        *iret = (INTEGER)PyInt_AsLong(piret);
        *resu = (DOUBLE)0.;
        BLANK(msgerr, lmsg);
        if ( *iret == 0 ) {
           if (PyComplex_Check(res)) {
               *resu    = (DOUBLE)PyComplex_RealAsDouble(res);
               *(resu+1)= (DOUBLE)PyComplex_ImagAsDouble(res);
           } else if (PyFloat_Check(res) || PyLong_Check(res) || PyInt_Check(res)) {
               *resu    = (DOUBLE)PyFloat_AsDouble(res);
           } else {
              *iret = 4;
           }
        } else {
            sret = PyString_AsString(txt);
            lsret = strlen(sret);
            STRING_FCPY(msgerr, lmsg, sret, lsret);
        }

        Py_DECREF(tup_par);
        Py_DECREF(tup_val);
        Py_DECREF(tup3);
        return ;
}


/* ------------------------------------------------------------------ */
void DEFSSPPPPP(GETVIS,getvis,_IN char *motfac,_IN STRING_SIZE lfac,
                              _IN char *motcle,_IN STRING_SIZE lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,
                           _INOUT INTEGER *val,_OUT INTEGER *nbval )
{
        /*
          Procedure GETVIS pour le FORTRAN : emule le fonctionnement
          de la procedure equivalente ASTER
          Entrees :
            le nom d un mot cle facteur : motfac (string)
            le nom d un mot cle simple ou sous mot cle : motcle (string)
            le numero de l occurence du mot cle facteur : iocc (entier)
            le numero de l argument demande (obsolete =1): iarg (entier)
            le nombre max de valeur attendues dans val : mxval (entier)
          Retourne :
            le tableau des valeurs attendues : val (tableau d entier )
            le nombre de valeurs effectivement retournees : nbval (entier)
               si pas de valeur nbval =0
               si plus de valeur que mxval nbval <0 et valeur abs = nbre valeurs
               si moins de valeurs que mxval nbval>0 et egal au nombre retourne
        */
        PyObject *res  = (PyObject*)0 ;
        PyObject *tup  = (PyObject*)0 ;
        int ok         = 0 ;
        int nval       = 0 ;
        int ioc        = 0 ;
        char *mfc      = (char*)0 ;
        char *mcs      = (char*)0 ;
                                                        ASSERT((*iocc>0)||(FindLength(motfac,lfac)==0));
                                                        ASSERT(EstPret(motcle,lcle)!=0);
        mfc=fstr1(motfac,lfac); /* conversion chaine fortran en chaine C */
                                                        ASSERT(mfc!=(char*)0);
        mcs=fstr2(motcle,lcle);
        /*
                VERIFICATION
                Si le mot-cle simple est recherche sous un mot-cle facteur et uniquement dans ce cas,
                le numero d'occurrence (*iocc) doit etre strictement positif.
                Si le mot-cle simple est recherche sans un mot-cle facteur iocc n'est pas utilise

        */
        if( isalpha(mfc[0])&&(*iocc<=0) )
        {
                printf( "<F> GETVIS : le numero d'occurence (IOCC=%ld) est invalide\n",*iocc) ;
                printf( "             commande : %s\n",PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                printf( "             mot-cle facteur : %s\n",mfc) ;
                printf( "             mot-cle simple  : %s\n",mcs) ;
                MYABORT( "erreur d'utilisation detectee") ;
        }
        ioc=(int)*iocc ;
        ioc=ioc-1 ;
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvis","ssiii",mfc,mcs,ioc,*iarg,(int)*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        ok = PyArg_ParseTuple(res,"iO",&nval,&tup);
        if (!ok)MYABORT("erreur dans la partie Python");

        *nbval = (INTEGER)nval;
        if ( nval < 0 ) nval=(int)*mxval;
        convert(nval,tup,val);

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return ;
}


/* ------------------------------------------------------------------ */
void DEFS(GETVLI,getvli, _OUT char *cas , _IN STRING_SIZE lcas )
{
        /*
        Cette fonction est destinee a etre utilisee pour le fichier "*.code" (fort.15)
        */
                                                        ASSERT(NomCas!=(char*)0) ;
        CSTRING_FCPY(cas,lcas,NomCas);
        return ;
}


/* ------------------------------------------------------------------ */
void DEFSSPPPPP(GETVLS,getvls,_IN char *motfac,_IN STRING_SIZE lfac,
                              _IN char *motcle,_IN STRING_SIZE lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,
                           _INOUT INTEGER *val,_OUT INTEGER *nbval )
{
        /*
          Procedure GETVLS pour le FORTRAN : emule le fonctionnement
          de la procedure equivalente ASTER
          Entrees :
            le nom d un mot cle facteur : motfac (string)
            le nom d un mot cle simple ou sous mot cle : motcle (string)
            le numero de l occurence du mot cle facteur : iocc (entier)
            le numero de l argument demande (obsolete =1): iarg (entier)
            le nombre max de valeur attendues dans val : mxval (entier)
          Retourne :
            le tableau des valeurs attendues : val (tableau de logical )
            le nombre de valeurs effectivement retournees : nbval (entier)
               si pas de valeur nbval =0
               si plus de valeur que mxval nbval <0 et valeur abs = nbre valeurs
               si moins de valeurs que mxval nbval>0 et egal au nombre retourne
        */
        PyObject *res  = (PyObject*)0 ;
        PyObject *tup  = (PyObject*)0 ;
        int ok         = 0 ;
        int nval       = 0 ;
        int ioc        = 0 ;
        char *mfc      = (char*)0 ;
        char *mcs      = (char*)0 ;
                                                        ASSERT((*iocc>0)||(FindLength(motfac,lfac)==0));
                                                        ASSERT(EstPret(motcle,lcle)!=0);
        mfc=fstr1(motfac,lfac); /* conversion chaine fortran en chaine C */
                                                        ASSERT(mfc!=(char*)0);
        mcs=fstr2(motcle,lcle);
        /*
                VERIFICATION
                Si le mot-cle simple est recherche sous un mot-cle facteur et uniquement dans ce cas,
                le numero d'occurrence (*iocc) doit etre strictement positif.
                Si le mot-cle simple est recherche sans un mot-cle facteur iocc n'est pas utilise

        */
        if( isalpha(mfc[0])&&(*iocc<=0) )
        {
                printf( "<F> GETVLS : le numero d'occurence (IOCC=%ld) est invalide\n",*iocc) ;
                printf( "             commande : %s\n",PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                printf( "             mot-cle facteur : %s\n",mfc) ;
                printf( "             mot-cle simple  : %s\n",mcs) ;
                MYABORT( "erreur d'utilisation detectee") ;
        }

        ioc=(int)*iocc ;
        ioc=ioc-1 ;
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvls","ssiii",mfc,mcs,ioc,(int)*iarg,(int)*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        ok = PyArg_ParseTuple(res,"iO",&nval,&tup);
        if (!ok)MYABORT("erreur dans la partie Python");

        *nbval=(INTEGER)nval;
        if ( nval < 0 ) nval=(int)*mxval;
        convert(nval,tup,val);

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return ;
}


/* ------------------------------------------------------------------ */
#define CALL_GETVTX(a,b,c,d,e,f,g) CALLSSPPPSP(GETVTX,getvtx,a,b,c,d,e,f,g)

void DEFSSPPPSP(GETVTX,getvtx,_IN char *motfac,_IN STRING_SIZE lfac,
                              _IN char *motcle,_IN STRING_SIZE lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,
                           _INOUT char *txval,_IN STRING_SIZE ltx,_OUT INTEGER *nbval)
{
        /*
          Procedure GETVTX pour le FORTRAN : emule le fonctionnement
          de la procedure equivalente ASTER
          Entrees :
            le nom d un mot cle facteur : motfac (string)
            le nom d un mot cle simple ou sous mot cle : motcle (string)
            le numero de l occurence du mot cle facteur : iocc (entier)
            le numero de l argument demande (obsolete =1): iarg (entier)
            le nombre max de valeur attendues dans val : mxval (entier)
          Retourne :
            le tableau des valeurs attendues : txval (tableau de string)
            ATTENTION : txval arrive avec une valeur par defaut
            le nombre de valeurs effectivement retournees : nbval (entier)
               si pas de valeur nbval =0
               si plus de valeur que mxval nbval <0 et valeur abs = nbre valeurs
               si moins de valeurs que mxval nbval>0 et egal au nombre retourne

        */
        PyObject *res  = (PyObject*)0 ;
        PyObject *tup  = (PyObject*)0 ;
        int ok         = 0 ;
        int nval       = 0 ;
        int ioc        = 0 ;
        char *mfc      = (char*)0 ;
        char *mcs      = (char*)0 ;

        mfc=fstr1(motfac,lfac); /* conversion chaine fortran en chaine C */
                                                        ASSERT(mfc!=(char*)0);
        mcs=fstr2(motcle,lcle);
        /*
                VERIFICATION
                Si le mot-cle simple est recherche sous un mot-cle facteur et uniquement dans ce cas,
                le numero d'occurrence (*iocc) doit etre strictement positif.
                Si le mot-cle simple est recherche sans un mot-cle facteur iocc n'est pas utilise

        */
        if( isalpha(mfc[0])&&(*iocc<=0) )
        {
                printf( "<F> GETVTX : le numero d'occurence (IOCC=%ld) est invalide\n",*iocc) ;
                printf( "             commande : %s\n",PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                printf( "             mot-cle facteur : %s\n",mfc) ;
                printf( "             mot-cle simple  : %s\n",mcs) ;
                MYABORT( "erreur d'utilisation detectee") ;
        }
        ioc=(int)*iocc ;
        ioc=ioc-1 ;
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvtx","ssiii",mfc,mcs,ioc,(int)*iarg,(int)*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)
        {
                printf( "<F> GETVTX : numero d'occurence (IOCC=%ld) \n",*iocc) ;
                printf( "             commande : %s\n",PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                printf( "             mot-cle facteur : %s\n",mfc) ;
                printf( "             mot-cle simple  : %s\n",mcs) ;
                MYABORT("erreur dans la partie Python");
        }

        ok = PyArg_ParseTuple(res,"iO",&nval,&tup);
        if (!ok)MYABORT("erreur au decodage d'une chaine dans le module C aster.getvtx");

        *nbval=(INTEGER)nval;
        if ( nval < 0 ) nval=(int)*mxval;
        if ( nval > 0 ){
                convertxt(nval,tup,txval,ltx);
        }

        /* ATTENTION : il ne faut decrementer le compteur de references de res
         *             qu'apres en avoir fini avec l'utilisation de tup.
         *             NE PAS decrementer le compteur de references de tup car
         *             la disparition de res entrainera un decrement automatique
         *             du compteur de tup (res=(nbval,tup))
         */
        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return ;
}

/* ------------------------------------------------------------------ */
void DEFSSPPPSP(GETVID,getvid,_IN char *motfac,_IN STRING_SIZE lfac,
                              _IN char *motcle,_IN STRING_SIZE lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,
                           _INOUT char *txval,_IN STRING_SIZE ltx,_OUT INTEGER *nbval)
{
        /*
          Procedure GETVID pour le FORTRAN : emule le fonctionnement
          de la procedure equivalente ASTER
          Entrees :
            le nom d un mot cle facteur : motfac (string)
            le nom d un mot cle simple ou sous mot cle : motcle (string)
            le numero de l occurence du mot cle facteur : iocc (entier)
            le numero de l argument demande (obsolete =1): iarg (entier)
            le nombre max de valeur attendues dans val : mxval (entier)
          Retourne :
            le tableau des valeurs attendues : val (tableau de string)
            le nombre de valeurs effectivement retournees : nbval (entier)
               si pas de valeur nbval =0
               si plus de valeur que mxval nbval <0 et valeur abs = nbre valeurs
               si moins de valeurs que mxval nbval>0 et egal au nombre retourne
        */
        PyObject *res  = (PyObject*)0 ;
        PyObject *tup  = (PyObject*)0 ;
        int ok,nval,ioc ;
        char *mfc;
        char *mcs;
                                                        ASSERT((*iocc>0)||(FindLength(motfac,lfac)==0));
        mfc=fstr1(motfac,lfac); /* conversion chaine fortran en chaine C */
                                                        ASSERT(mfc!=(char*)0);
        mcs=fstr2(motcle,lcle);
        /*
                VERIFICATION
                Si le mot-cle simple est recherche sous un mot-cle facteur et uniquement dans ce cas,
                le numero d'occurrence (*iocc) doit etre strictement positif.
                Si le mot-cle simple est recherche sans un mot-cle facteur iocc n'est pas utilise

        */
        if( isalpha(mfc[0])&&(*iocc<=0) )
        {
                printf( "<F> GETVID : le numero d'occurence (IOCC=%ld) est invalide\n",*iocc) ;
                printf( "             commande : %s\n",PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                printf( "             mot-cle facteur : %s\n",mfc) ;
                printf( "             mot-cle simple  : %s\n",mcs) ;
                MYABORT( "erreur d'utilisation detectee") ;
        }
        ioc=(int)*iocc ;
        ioc=ioc-1 ;
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvid","ssiii",mfc,mcs,ioc,(int)*iarg,(int)*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        ok = PyArg_ParseTuple(res,"iO",&nval,&tup);
        if (!ok)MYABORT("erreur dans la partie Python");

        *nbval=(INTEGER)nval;
        if ( nval < 0 ) nval=(int)*mxval;
        if ( nval > 0 ){
                convertxt(nval,tup,txval,ltx);
        }

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return ;
}


/* ------------------------------------------------------------------ */
void DEFPPP(SMCDEL,smcdel,INTEGER *iold,INTEGER *inew,INTEGER *ierusr)
{
        /*
          Entrees:
            iold ancien numero d ordre de la commande
            inew nouveau numero d ordre de la commande (si inew < iold, commande detruite)
          Sorties:
            ierusr code retour d erreur incremente
        */
        PyObject *res  = (PyObject*)0 ;

        res=PyObject_CallMethod(commande,"smcdel","ii",(int)*iold,(int)*inew);

        /*
            Si le retour est NULL : une exception a ete levee dans le code Python appele
            Cette exception est a transferer normalement a l appelant mais FORTRAN ???
            On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)MYABORT("erreur a l appel de smcdel dans la partie Python");
        *ierusr = *ierusr + (INTEGER)PyInt_AsLong(res);
        Py_DECREF(res);
}


/* ------------------------------------------------------------------ */
STRING_SIZE FindLength( _IN char *chaineFortran , _IN STRING_SIZE longueur )
{
        /*
        Fonction  : FindLength
        Intention
                Retourne la taille exacte de la chaine de caracteres fortran
                chaineFortran contenant eventuellement des blancs de fin de ligne..
                La taille exacte est la longueur de la chaine du debut au
                dernier caractere non blanc.
        */

        int k = longueur-1 ;
        if ( ! chaineFortran ) return 0 ;

        while( (int)k>=0 && chaineFortran[k]==' ' ) k-- ;
        return k+1 ;
}


/* ------------------------------------------------------------------ */
PyObject * MakeTupleString(long nbval,char *kval,STRING_SIZE lkval,INTEGER *lval)
{
   /*
            Entrees:
               nbval nombre de chaines dans kval
               kval  tableau de nbval chaines FORTRAN
               lkval longueur des chaines FORTRAN (compilateur)
               lval  longueur des nbval chaines FORTRAN (utilisateur)
            Sorties:
               RETOUR fonction : tuple de string Python de longueur nbval
            Fonction:
               Convertir un tableau de chaines FORTRAN en un tuple de string Python de meme longueur
   */
   int i;
   int len;
   char *deb=kval;
   if(nbval == 1){
      if (lval) {
         len = lval[0];
      } else {
         len = lkval;
      }
      return PyString_FromStringAndSize(deb, FindLength(deb,len));
   }
   else{
      PyObject *t=PyTuple_New((Py_ssize_t)nbval);
      for(i=0;i<nbval;i++){
         if (lval) {
            len = (int)lval[i];
         } else {
            len = lkval;
         }
         if( PyTuple_SetItem(t,i,PyString_FromStringAndSize(deb,FindLength(deb,len)))) {
            Py_DECREF(t);
            return NULL;
         }
         deb=deb+lkval;
      }
      return t;
   }
}

/* ------------------------------------------------------------------ */
PyObject * MakeListString( long nbval,char *kval,STRING_SIZE lkval )
{
   /*
            Entrees:
               nbval nombre de chaines dans kval
               kval  tableau de nbval chaines FORTRAN
               lkval longueur des chaines FORTRAN (compilateur)
            Sorties:
               RETOUR fonction : tuple de string Python de longueur nbval les espaces terminant la
               chaine sont supprimes
            Fonction:
               Convertir un tableau de chaines FORTRAN en un tuple de string Python de meme longueur
   */
   int i;
   char *deb=kval;
   PyObject *l=PyList_New((Py_ssize_t)nbval);
   for(i=0;i<nbval;i++){
      if( PyList_SetItem(l,i,PyString_FromStringAndSize(deb,FindLength(deb,lkval)))) {
         Py_DECREF(l);
         return NULL;
      }
      deb=deb+lkval;
   }
   return l;
}


/* ------------------------------------------------------------------ */
PyObject * MakeTupleInt(long nbval,INTEGER* kval)
{
   /*
            Entrees:
               nbval nombre d'entiers dans kval
               kval  tableau de nbval INTEGER FORTRAN
            Sorties:
               RETOUR fonction : tuple de int Python de longueur nbval
            Fonction:
               Convertir un tableau de INTEGER FORTRAN en un tuple de int Python de meme longueur
   */
   int i;
   if(nbval == 1){
      return PyInt_FromLong(*kval);
   }
   else{
      PyObject * t=PyTuple_New((Py_ssize_t)nbval);
      for(i=0;i<nbval;i++){
         if(PyTuple_SetItem(t,i,PyInt_FromLong((long)kval[i]))) {
         Py_DECREF(t);
         return NULL;
         }
      }
      return t;
   }
}

/* ------------------------------------------------------------------ */
PyObject * MakeListInt(long nbval,INTEGER* kval)
{
   /*
            Entrees:
               nbval nombre d'entiers dans kval
               kval  tableau de nbval INTEGER FORTRAN
            Sorties:
               RETOUR fonction : liste de int Python de longueur nbval
            Fonction:
               Convertir un tableau de INTEGER FORTRAN en une liste de int Python de meme longueur
   */
   int i;
   PyObject *l=PyList_New((Py_ssize_t)nbval);
   for(i=0;i<nbval;i++){
      if (PyList_SetItem(l,i,PyInt_FromLong((long)kval[i]))) {
         Py_DECREF(l);
         return NULL;
      }
   }
   return l;
}

/* ------------------------------------------------------------------ */
PyObject * MakeTupleFloat(long nbval,DOUBLE * kval)
{
   /*
            Entrees:
               nbval nombre de reels dans kval
               kval  tableau de nbval double FORTRAN
            Sorties:
               RETOUR fonction : tuple de float Python de longueur nbval
            Fonction:
               Convertir un tableau de double FORTRAN en un tuple de float Python de meme longueur
   */
   int i;
   if(nbval == 1){
      return PyFloat_FromDouble((double)*kval);
   }
   else{
      PyObject * t=PyTuple_New((Py_ssize_t)nbval);
      for(i=0;i<nbval;i++){
         if(PyTuple_SetItem(t,i,PyFloat_FromDouble((double)kval[i]))) {
            Py_DECREF(t);
            return NULL;
         }
      }
      return t;
   }
}

/* ------------------------------------------------------------------ */
PyObject * MakeListFloat(long nbval,DOUBLE * kval)
{
   /*
            Entrees:
               nbval nombre de reels dans kval
               kval  tableau de nbval double FORTRAN
            Sorties:
               RETOUR fonction : list de float Python de longueur nbval
            Fonction:
               Convertir un tableau de double FORTRAN en une liste de float Python de meme longueur
   */
   int i;
   PyObject *l=PyTuple_New((Py_ssize_t)nbval);
   for(i=0;i<nbval;i++){
      if(PyList_SetItem(l,i,PyFloat_FromDouble((double)kval[i]))) {
         Py_DECREF(l);
         return NULL;
      }
   }
   return l;
}


/* ------------------------------------------------------------------ */
void STDCALL(PUTVIR,putvir) (_IN INTEGER *ival)
{
   /*
      Entrees:
         ival entier à affecter
      Fonction:
         renseigner l'attribut valeur associé à la sd
         n'est utile que pour DEFI_FICHIER
         cet attribut est ensuite évalué par la méthode traite_value
         de B_ETAPE.py
   */
   PyObject *res = (PyObject*)0 ;

   res = PyObject_CallMethod(commande,"putvir","i",(int)*ival);
   /*
         Si le retour est NULL : une exception a ete levee dans le code Python appele
         Cette exception est a transferer normalement a l appelant mais FORTRAN ???
         On produit donc un abort en ecrivant des messages sur la stdout
   */
   if (res == NULL)
      MYABORT("erreur a l appel de putvir dans la partie Python");

   Py_DECREF(res);
}


/* ------------------------------------------------------------------ */
void DEFSSP(GCUCON,gcucon, _IN char *resul, STRING_SIZE lresul,
                           _IN char *concep, STRING_SIZE lconcep, INTEGER *ier)
{
   /*
            Entrees:
               resul   nom du concept
               concep type du concept
            Sorties :
               ier     >0 le concept existe avant
                        =0 le concept n'existe pas avant
                        <0 le concept existe avant mais n'est pas du bon type
            Fonction:
               Verification de l existence du couple (resul,concep) dans les
               resultats produits par les etapes precedentes
   */
   PyObject * res = (PyObject*)0 ;
                                                                              ASSERT(lresul) ;
                                                                              ASSERT(lconcep) ;
   res = PyObject_CallMethod(commande,"gcucon","s#s#",resul,lresul,concep,lconcep);
   /*
               Si le retour est NULL : une exception a ete levee dans le code Python appele
               Cette exception est a transferer normalement a l appelant mais FORTRAN ???
               On produit donc un abort en ecrivant des messages sur la stdout
   */
   if (res == NULL)
            MYABORT("erreur a l appel de gcucon dans la partie Python");

   *ier = (INTEGER)PyInt_AsLong(res);
   Py_DECREF(res);
}


/* ------------------------------------------------------------------ */
void DEFPSP(GCUCDT,gcucdt,INTEGER *icmd,char *resul,STRING_SIZE lresul,INTEGER *ier)
{
        /*
        Emulation de la fonction ASTER correspondante

        Entrees:
                icmd    numero de la commande
                resul   nom du concept
                Sorties :
                ier     >0 le concept existe avant
                        =0 le concept n'existe pas avant
                        <0 le concept existe avant mais est detruit
        Fonction:
                VERIFICATION DE L'EXISTENCE D'UN CONCEPT DANS LES
                DECLARATIONS PRECEDENTES (IE JUSQU'A L'ORDRE ICMD-1)
        Commentaire :
                L'émulation de la fonction est seulement partielle. On utilise
                la fonction gcucon qui ne donne pas l'information sur les concepts
                detruits
        */
        PyObject * res = (PyObject*)0 ;
        res = PyObject_CallMethod(commande,"gcucon","is#s",(int)*icmd,resul,lresul,"");
        /*
           Si le retour est NULL : une exception a ete levee dans le code Python appele
            Cette exception est a transferer normalement a l appelant mais FORTRAN ???
            On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
                MYABORT("erreur a l appel de gcucdt dans la partie Python");

        *ier = (INTEGER)PyInt_AsLong(res);
        /*
                ier= -1 indique que le concept existe mais d'un autre type. On doit donc
                retourner 1
        */
        if(*ier==-1)*ier=1;
        Py_DECREF(res);
}


/* ------------------------------------------------------------------ */
void DEFSSPPP(GETTVC,gettvc,_IN char * nom,STRING_SIZE lnom,
                            _OUT char *ctyp,STRING_SIZE lctyp,
                            _OUT INTEGER *ival, _OUT DOUBLE *rval, _OUT INTEGER *ier)
{
        /*
                  Entrees:
                    nom    numero de la commande
                  Sorties :
                    ctyp    type de la constante (IS,R8,C8,LS)
                    ival    valeur de la constante si IS ou LS
                    rval    valeur de la constante si R8 ou C8 (dimension 2)
                    ier     >0 la constante existe
                            =0 la constante n'existe pas
                  Fonction:
                    Retourner la valeur de la constante nom si elle existe
                    si elle n existe pas ier = 0
                  Commentaire : RAS
        */
        PyObject * res = (PyObject*)0 ;
        PyObject * valeur = (PyObject*)0 ;
        int ok=0;
        int iret;
        *ier=0;
        res = PyObject_CallMethod(commande,"gettvc","s#",nom,lnom);
        /*
                    Si le retour est NULL : une exception a ete levee dans le code Python appele
                    Cette exception est a transferer normalement a l appelant mais FORTRAN ???
                    On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
                MYABORT("erreur a l appel de gettvc dans la partie Python");

        ok=PyArg_ParseTuple(res, "iO",&iret,&valeur);
        *ier=(INTEGER)iret;
        if(!ok)MYABORT("erreur dans gettvc_ ");
        if(PyInt_Check(valeur)){
          *ival=(INTEGER)PyInt_AsLong(valeur);
          strncpy(ctyp,"IS  ",4);
        }
        else if(PyFloat_Check(valeur)){
          *rval=(DOUBLE)PyFloat_AsDouble(valeur);
          strncpy(ctyp,"R8  ",4);
        }
        else{
          *ier=0;
        }

        Py_DECREF(res); /* le compteur de references de valeur sera automatiquement decremente */
}


/* ------------------------------------------------------------------ */
void DEFP(GCECDU,gcecdu, INTEGER *numint)
{
        /*
          Sortie :
            numint  numero de l operateur de la commande
          Fonction:
             Recuperation du numero de l operateur
        */
        PyObject * res = (PyObject*)0 ;
        res = PyObject_CallMethod(commande,"getoper","");
        /*
                    Si le retour est NULL : une exception a ete levee dans le code Python appele
                    Cette exception est a transferer normalement a l appelant mais FORTRAN ???
                    On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
                MYABORT("erreur a l appel de gcecdu dans la partie Python");

        *numint = (INTEGER)PyInt_AsLong(res);
        Py_DECREF(res);
}


/* ------------------------------------------------------------------ */
void gcncon2_(char *type,char *resul,STRING_SIZE ltype,int lresul)
{
/* CCAR : cette fonction devrait s appeler gcncon mais elle est utilisee par
          tous les operateurs (???) et pas seulement dans les macros
          Pour le moment il a ete decide de ne pas l'emuler dans le superviseur
          Python mais d'utiliser les fonctions FORTRAN existantes
          Ceci a l avantage d'assurer la coherence entre tous les operateurs
          et de conserver les fonctionnalites de poursuite pour les macros
*/
        /*
          Entrees:
            type vaut soit
                    '.' : le concept sera detruit en fin de job
                    '_' : le concept ne sera pas detruit

          Sorties:
            resul  nom d'un concept delivre par le superviseur
                   Ce nom est de la forme type // '000ijkl' ou ijkl est un nombre
                   incremente a chaque appel pour garantir l unicite des noms

          Fonction:
            Delivrer un nom de concept non encore utilise et unique
        */
        MYABORT("Cette procedure n est pas implementee");
}

/* ------------------------------------------------------------------ */
/*
 * Pour conserver le lien entre le code de calcul en Fortran et le superviseur
 * en Python, on memorise la commande courante.
 * Cette commande est celle que le fortran interroge lors de ses appels à l'API
 * GETXXX.
 * Cette commande courante est enregistrée dans une pile de commandes car avec le
 * mécanisme des macros, une commande peut avoir des sous commandes qui sont
 * appelées pendant l'exécution de la commande principale.
 * La fonction empile doit etre appelée avant l'exécution d'une commande (appel à oper,
 * par exemple) et la fonction depile doit etre appelée après l'exécution de cette
 * commande.
 */
static PyObject * empile(PyObject *c)
{
        /* PyList_Append incremente de 1 le compteur de references de c (commande courante) */
        PyList_Append(pile_commandes,c);
        niveau=niveau+1;
        if(NIVMAX < niveau){
          printf("Le nombre de niveau max prevus %d est insuffisant pour le nombre demande %d\n",NIVMAX,niveau);
          abort();
        }
        return c;
}

/* ------------------------------------------------------------------ */
static PyObject * depile()
{
        PyObject * com;
        int l=PyList_Size(pile_commandes);
        niveau=niveau-1;
        if(l == 0){
          /* Pile vide */
          Py_INCREF( Py_None ) ;
          return Py_None;
        }
        /* Derniere commande dans la pile */
        com = PyList_GetItem(pile_commandes,l-1);
        /* PyList_GetItem n incremente pas le compteur de ref de com */
        /* On tronque la liste a la dimension l-1 */
        PyList_SetSlice(pile_commandes,l-1,l,NULL);
        /* Le compteur de ref de com est decremente de 1 */
        if(l == 1){
          /* La pile tronquee est vide */
          Py_INCREF( Py_None ) ;
          return Py_None;
        }
        /* On ne passe ici que pour les macros avec sous commandes
         * en mode commande par commande */
        /* On retourne la derniere commande de la pile */
        com = PyList_GetItem(pile_commandes,l-2);
        return com;
}

/* ------------------------------------------------------------------ */
PyObject * get_active_command()
{
        /*
         * Retourne un pointeur sur la commande active
         */
   return commande;
}

/* ------------------------------------------------------------------ */
static PyObject* aster_prepcompcham(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        char *nomce;
        char *nomcs;
        char *nomcmp;
        char *ktype;
        char *groups;
        PyObject *list;
        int inval=0;
        INTEGER nval;
        int long_nomcham=8;
        int itopo;
        INTEGER topo;
        void *malloc(size_t size);

        if (!PyArg_ParseTuple(args, "ssssiO:prepcompcham",&nomce,&nomcs,&nomcmp,&ktype,&itopo,&list)) return NULL;

        inval=PyList_Size(list);
        nval=(INTEGER)inval;
        topo=(INTEGER)itopo;
        if (inval > 0) {
          groups = (char *)malloc(inval*long_nomcham*sizeof(char));
          converltx(inval,list,groups,long_nomcham); /* conversion  */
        }
        /* on ne peut passer a fortran une chaine non allouee
           a cause du strlen() que l'on va faire dessus au moment du passage
           c -> fortran
        */
        else {
          groups = (char *)malloc(long_nomcham*sizeof(char));
          BLANK(groups, long_nomcham);
        }

        try(1){
          CALL_JEMARQ();
          CALL_PRCOCH(nomce,nomcs,nomcmp,ktype,&topo,&nval,groups);
          Py_INCREF( Py_None ) ;
          CALL_JEDEMA();
          free(groups);
          return Py_None;
        }
        catch(CodeAbortAster){
          /* une exception a ete levee, elle est destinee a etre traitee dans l'appelant */
          PyErr_SetString(PyExc_KeyError, "Concept inexistant");
          CALL_JEDEMA();
          return NULL;
        }
   return NULL;
}

/* ------------------------------------------------------------------ */
static char getvectjev_doc[]=
"getvectjev(nomsd)->valsd      \n\
\n\
Retourne la valeur du concept nomsd \n\
dans un tuple.";

static PyObject* aster_getvectjev(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        char *nomsd, nomsd32[33];
        char nomob[9] = "         ";
        DOUBLE *f;
        INTEGER *l;
        INTEGER4 *i4;
        char *kvar;
        PyObject *tup=NULL;
        INTEGER lcon, iob;
        int ishf=0, ilng=0;
        INTEGER shf;
        INTEGER lng;
        INTEGER ctype=0;
        int i, ksize=0;
        char *iaddr;

        if (!PyArg_ParseTuple(args, "s|ii:getvectjev",&nomsd,&ishf,&ilng)) return NULL;
        nomsd32[32] = '\0';
        CSTRING_FCPY(nomsd32, 32, nomsd);
        nomob[8] = '\0';
        shf = (INTEGER)ishf;
        lng = (INTEGER)ilng;

        try(1){
          iob=0 ;
          CALL_JEMARQ();
          CALL_GETCON(nomsd32,&iob,&shf,&lng,&ctype,&lcon,&iaddr,nomob);
          if(ctype < 0){
            /* Erreur : vecteur jeveux inexistant, on retourne None */
            Py_INCREF( Py_None ) ;
            CALL_JEDEMA();
            return Py_None;
          }
          else if(ctype == 0){
            /* Liste vide */
            tup = PyTuple_New( 0 ) ;
          }
          else if(ctype == 1){
            /* REEL */
            f = (DOUBLE *)iaddr;
            tup = PyTuple_New( (Py_ssize_t)lcon ) ;
            for(i=0;i<lcon;i++){
               PyTuple_SetItem( tup, i, PyFloat_FromDouble((double)f[i]) ) ;
            }
          }
          else if(ctype == 2){
            /* ENTIER */
            l = (INTEGER*)iaddr;
            tup = PyTuple_New( (Py_ssize_t)lcon ) ;
            for(i=0;i<lcon;i++){
               PyTuple_SetItem( tup, i, PyInt_FromLong((long)l[i]) ) ;
            }
          }
          else if(ctype == 9){
            /* ENTIER COURT */
            i4 = (INTEGER4*)iaddr;
            tup = PyTuple_New( (Py_ssize_t)lcon ) ;
            for(i=0; i<lcon; i++){
               PyTuple_SetItem( tup, i, PyInt_FromLong((long)i4[i]) ) ;
            }
          }
          else if(ctype == 3){
            /* COMPLEXE */
            f = (DOUBLE *)iaddr;
            tup = PyTuple_New( (Py_ssize_t)lcon ) ;
            for(i=0;i<lcon;i++){
               PyTuple_SetItem( tup, i, PyComplex_FromDoubles((double)f[2*i], (double)f[2*i+1]) ) ;
            }
          }
          else if (ctype == 4 || ctype == 5 || ctype == 6 || ctype == 7 || ctype == 8) {
                switch ( ctype ) {
                    case 4 : ksize = 8;  break;
                    case 5 : ksize = 16; break;
                    case 6 : ksize = 24; break;
                    case 7 : ksize = 32; break;
                    case 8 : ksize = 80; break;
                }
                /* CHAINE DE CARACTERES */
                tup = PyTuple_New( (Py_ssize_t)lcon ) ;
                for(i=0; i<lcon; i++){
                   kvar = iaddr + i*ksize;
                   PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar, ksize) ) ;
                }
          }

          CALL_JEDETR("&&GETCON.PTEUR_NOM");
          CALL_JEDEMA();
          return tup;
        }
        catch(CodeAbortAster){
          /* une exception a ete levee, elle est destinee a etre traitee dans l'appelant */
          PyErr_SetString(PyExc_KeyError, "Concept inexistant");
          CALL_JEDEMA();
          return NULL;
        }
   return NULL;
}

static char getcolljev_doc[]=
"getcolljev(nomsd)->valsd      \n\
\n\
Retourne la valeur du concept nomsd \n\
dans un tuple.";

/* ------------------------------------------------------------------ */
static PyObject* aster_getcolljev(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        char *nomsd, *nom, nomsd32[33];
        char nomob[9] = "         ";
        DOUBLE *f;
        INTEGER *l;
        INTEGER4 *i4;
        char *kvar;
        PyObject *tup=NULL, *dico, *key;
        INTEGER iob,j,ishf,ilng;
        INTEGER lcon;
        INTEGER ctype=0;
        INTEGER *val, nbval;
        int i, ksize=0;
        char *iaddr;
        void *malloc(size_t size);

        if (!PyArg_ParseTuple(args, "s:getcolljev",&nomsd)) return NULL;
        nomsd32[32] = '\0';
        CSTRING_FCPY(nomsd32, 32, nomsd);
        nomob[8] = '\0';

/* Taille de la collection */
        nbval = 1;
        val = (INTEGER *)malloc((nbval)*sizeof(INTEGER));
        nom = (char *)malloc(24*sizeof(char));
        strcpy(nom, "LIST_COLLECTION");
        CALL_JEMARQ();
        CALL_TAILSD(nom, nomsd32, val, &nbval);
        iob=val[0];

        dico = PyDict_New();
        try(1){
          for(j=1;j<iob+1;j++){
                ishf=0 ;
                ilng=0 ;
                CALL_GETCON(nomsd32,&j,&ishf,&ilng,&ctype,&lcon,&iaddr,nomob);
                if(nomob[0] == ' '){
                    key=PyInt_FromLong( (long)j );
                }
                else {
                    key=PyString_FromStringAndSize(nomob,8);
                }
                if(ctype < 0){
                    /* Erreur */
                    PyErr_SetString(PyExc_KeyError, "Concept inexistant");
                    return NULL;
                }
                else if(ctype == 0){
                    Py_INCREF( Py_None );
                    PyDict_SetItem(dico,key,Py_None);
                }
                else if(ctype == 1){
                    /* REEL */
                    f = (DOUBLE *)iaddr;
                    tup = PyTuple_New( (Py_ssize_t)lcon ) ;
                    for(i=0;i<lcon;i++){
                       PyTuple_SetItem( tup, i, PyFloat_FromDouble((double)f[i]) ) ;
                    }
                    PyDict_SetItem(dico,key,tup);
                }
                else if(ctype == 2){
                    /* ENTIER */
                    l = (INTEGER*)iaddr;
                    tup = PyTuple_New( (Py_ssize_t)lcon ) ;
                    for(i=0;i<lcon;i++){
                       PyTuple_SetItem( tup, i, PyInt_FromLong((long)l[i]) ) ;
                    }
                    PyDict_SetItem(dico,key,tup);
                }
                else if(ctype == 9){
                    /* ENTIER COURT */
                    i4 = (INTEGER4*)iaddr;
                    tup = PyTuple_New( (Py_ssize_t)lcon ) ;
                    for(i=0; i<lcon; i++){
                       PyTuple_SetItem( tup, i, PyInt_FromLong((long)i4[i]) ) ;
                    }
                    PyDict_SetItem(dico,key,tup);
                }
                else if(ctype == 3){
                    /* COMPLEXE */
                    f = (DOUBLE *)iaddr;
                    tup = PyTuple_New( (Py_ssize_t)lcon ) ;
                    for(i=0;i<lcon;i++){
                       PyTuple_SetItem( tup, i, PyComplex_FromDoubles((double)f[2*i], (double)f[2*i+1]) ) ;
                    }
                    PyDict_SetItem(dico,key,tup);
                }
                else if (ctype == 4 || ctype == 5 || ctype == 6 || ctype == 7 || ctype == 8) {
                    switch ( ctype ) {
                        case 4 : ksize = 8;  break;
                        case 5 : ksize = 16; break;
                        case 6 : ksize = 24; break;
                        case 7 : ksize = 32; break;
                        case 8 : ksize = 80; break;
                    }
                    /* CHAINE DE CARACTERES */
                    tup = PyTuple_New( (Py_ssize_t)lcon ) ;
                    for(i=0; i<lcon; i++){
                       kvar = iaddr + i*ksize;
                       PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar, ksize) ) ;
                    }
                    PyDict_SetItem(dico,key,tup);
                }
                Py_XDECREF(key);
                Py_XDECREF(tup);
         }
         CALL_JEDETR("&&GETCON.PTEUR_NOM");
         CALL_JEDEMA();
         free(nom);
         free(val);
         return dico;
       }
        catch(CodeAbortAster){
          /* une exception a ete levee, elle est destinee a etre traitee dans l'appelant */
          PyErr_SetString(PyExc_KeyError, "Concept inexistant");
          CALL_JEDEMA();
          return NULL;
        }
   return NULL;
}


static char putvectjev_doc[]=
"putvectjev(nomsd, nbval, indices, reel, imag, num)\n\
\n\
Renvoie le contenu d'un objet python dans un vecteur jeveux.\n\
\
    nomsd   : nom du vecteur jeveux\n\
    nbval   : nombre de valeurs\n\
    indices : indices dans le vecteur (commence a 1, de longueur nbval)\n\
    reel    : valeurs reelles ou parties reelles en cas de complexes (de longueur nbval)\n\
    imag    : parties imaginaires en cas de complexes (de longueur nbval)\n\
    num     : 1.\n\
";

/* ------------------------------------------------------------------ */
static PyObject* aster_putvectjev(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *tupi  = (PyObject*)0 ;
        PyObject *tupr  = (PyObject*)0 ;
        PyObject *tupc  = (PyObject*)0 ;
        char *nomsd, nomsd32[33];
        DOUBLE *valr;
        DOUBLE *valc;
        INTEGER *ind;
        int nind, inum;
        INTEGER num;
        INTEGER nbind;
        int ok        = 0 ;
        INTEGER iret=0;
        void *malloc(size_t size);
    
        ok = PyArg_ParseTuple(args, "siOOOi",&nomsd,&nind,&tupi,&tupr,&tupc,&inum);
        if (!ok)MYABORT("erreur dans la partie Python");
        nomsd32[32] = '\0';
        CSTRING_FCPY(nomsd32, 32, nomsd);

        nbind=(INTEGER)nind;
        num=(INTEGER)inum;
        
        ind = (INTEGER *)malloc((size_t)nind*sizeof(INTEGER));
        valr = (DOUBLE *)malloc((size_t)nind*sizeof(DOUBLE));
        valc = (DOUBLE *)malloc((size_t)nind*sizeof(DOUBLE));

        if ( nind>0 ){
                 convert(nind,tupi,ind);
                 convr8(nind,tupr,valr);
                 convr8(nind,tupc,valc);
        }
        try(1){
          CALL_JEMARQ();
          CALL_PUTCON(nomsd32,&nbind,ind,valr,valc,&num,&iret);
          CALL_JEDEMA();

          if(iret == 0){
            /* Erreur */
            PyErr_SetString(PyExc_KeyError, "Concept inexistant");
            return NULL;
          }

          free((char *)valc);
          free((char *)valr);
          free((char *)ind);
        }
        catch(CodeAbortAster){
          /* une exception a ete levee, elle est destinee a etre traitee dans l'appelant */
          PyErr_SetString(PyExc_KeyError, "Concept inexistant");
          return NULL;
        }
        Py_INCREF( Py_None ) ;
        return Py_None;
}


static char putcolljev_doc[]=
"putcolljev(nomsd)->valsd      \n\
\n\
Renvoie le contenu d'un objet python dans  \n\
un vecteur jeveux.";

/* ------------------------------------------------------------------ */
static PyObject* aster_putcolljev(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *tupi  = (PyObject*)0 ;
        PyObject *tupr  = (PyObject*)0 ;
        PyObject *tupc  = (PyObject*)0 ;
        char *nomsd, nomsd32[33];
        DOUBLE *valr;
        DOUBLE *valc;
        INTEGER *ind;
        int nind, inum;
        INTEGER num;
        INTEGER nbind;
        int ok        = 0 ;
        INTEGER iret=0;
        void *malloc(size_t size);

        ok = PyArg_ParseTuple(args, "siOOOi",&nomsd,&nind,&tupi,&tupr,&tupc,&inum);
        if (!ok)MYABORT("erreur dans la partie Python");
        nomsd32[32] = '\0';
        CSTRING_FCPY(nomsd32, 32, nomsd);
        nbind=(INTEGER)nind;
        num=(INTEGER)inum;

        ind = (INTEGER *)malloc((size_t)nind*sizeof(INTEGER));
        valr = (DOUBLE *)malloc((size_t)nind*sizeof(DOUBLE));
        valc = (DOUBLE *)malloc((size_t)nind*sizeof(DOUBLE));

        if ( nind>0 ){
                 convert(nind,tupi,ind);
                 convr8(nind,tupr,valr);
                 convr8(nind,tupc,valc);
        }

        CALL_JEMARQ();
        CALL_PUTCON(nomsd32,&nbind,ind,valr,valc,&num,&iret);
        CALL_JEDEMA();

        if(iret == 0){
          /* Erreur */
          PyErr_SetString(PyExc_KeyError, "Concept inexistant");
          return NULL;
        }

        free((char *)valc);
        free((char *)valr);
        free((char *)ind);

        Py_INCREF( Py_None ) ;
        return Py_None;
}


/* ------------------------------------------------------------------ */
static PyObject* aster_GetResu(self, args)
PyObject *self; /* Not used */
PyObject *args;

/* Construit sous forme d'un dictionnaire Python l'architecture d'une SD resultat

   Arguments :
     IN Nom de la SD resultat
     IN Nature des informations recherchees
          CHAMPS      -> Champs de resultats
          COMPOSANTES -> Liste des composantes des champs
          VARI_ACCES  -> Variables d'acces
          PARAMETRES  -> Parametres


     OUT dico
       Si 'CHAMPS'
       dico['NOM_CHAM'] -> [] si le champ n'est pas calcule
                        -> Liste des numeros d'ordre ou le champ est calcule

       Si 'COMPOSANTES'
       dico['NOM_CHAM'] -> [] si le champ n'est pas calcule
                        -> Liste des composantes du champ (enveloppe sur tous les instants)

       Si 'VARI_ACCES'
       dico['NOM_VA']   -> Liste des valeurs de la variable d'acces

       Si 'PARAMETRES'
       dico['NOM_VA']   -> Liste des valeurs du parametre

*/
{
   INTEGER nbchmx, nbpamx, nbord, numch, numva, ier, nbcmp ;
   INTEGER *liord, *ival;
   INTEGER *val, nbval ;
   DOUBLE *rval;
   char *nomsd, *mode, *liscmp, *nom, nomsd32[33], *cmp;
   char *kval, *kvar;
   char nomch[16], nomva[16];
   int i, lo, ksize=0, ksizemax=80, inbord;
   INTEGER icode, ctype;
   PyObject *dico=NULL, *liste, *key;
   char blanc[80];
   void *malloc(size_t size);

   BLANK(blanc, 80);
   
   if (!PyArg_ParseTuple(args, "ss",&nomsd, &mode)) return NULL;
   nomsd32[32] = '\0';
   CSTRING_FCPY(nomsd32, 32, nomsd);

/* Identifiant de la SD resultat */
   nbval = 3;
   val = (INTEGER *)malloc((nbval)*sizeof(INTEGER));
   nom = (char *)malloc(24*sizeof(char));
   strcpy(nom, "LIST_RESULTAT");

/* Taille de la SD resultat : nbr champs, nbr paras, nbr numeros d'ordre */
   CALL_JEMARQ();
   CALL_TAILSD(nom, nomsd32, val, &nbval);
   nbchmx = val[0];
   nbpamx = val[1];
   nbord  = val[2];
   inbord = (int)nbord;

   if (strcmp(mode,"CHAMPS") == 0 || strcmp(mode,"COMPOSANTES") == 0) {
/* Construction du dictionnaire : cle d'acces = nom du champ */
     liord  = (INTEGER *)malloc(inbord*sizeof(INTEGER));
     liscmp = (char *)malloc(500*8*sizeof(char));
     dico = PyDict_New();
     for (numch=1; numch<=nbchmx; numch++) {
       CALL_RSACCH(nomsd32, &numch, nomch, &nbord, liord, &nbcmp, liscmp);
       inbord = (int)nbord;
       lo = 16;
       while (nomch[lo-1] == ' ')  lo--;
       key = PyString_FromStringAndSize(nomch,lo);
       liste = PyList_New(0);
       if (strcmp(mode,"CHAMPS") == 0) {
              for (i=0; i<inbord; i++)
                PyList_Append(liste,PyInt_FromLong((long)liord[i]));
       }

       if (strcmp(mode,"COMPOSANTES") == 0) {
         for (i=0; i<nbcmp; i++) {
            cmp = &(liscmp[i*8]);
            lo = 8; while (cmp[lo-1] == ' ')  lo--;
            PyList_Append(liste,PyString_FromStringAndSize(cmp,lo));
         }
       }

       PyDict_SetItem(dico,key,liste);
       Py_XDECREF(key);
       Py_XDECREF(liste);
     }

     free(liord);
     free(liscmp);
   }

   else if (strcmp(mode,"VARI_ACCES") == 0 || strcmp(mode,"PARAMETRES") == 0) {
        icode = 2;
        if (strcmp(mode,"VARI_ACCES") == 0) {
            icode = 0;
        }
/* Extraction des paramètres ou variables d'accès */
          ival  = (INTEGER *)malloc(inbord*sizeof(INTEGER));
          rval  = (DOUBLE *)malloc(inbord*sizeof(DOUBLE) );
          kval  = (char *)malloc(inbord*ksizemax*sizeof(char));

          dico = PyDict_New();
          for (numva=0; numva<=nbpamx; numva++)
            {
            CALL_RSACPA(nomsd32, &numva, &icode, nomva, &ctype, ival, rval, kval, &ier);
            if (ier != 0) continue;

            lo = 16;
            while (nomva[lo-1] == ' ') lo--;
            key = PyString_FromStringAndSize(nomva,lo);

            liste = PyList_New(0);
            if(ctype < 0){
                /* Erreur */
                PyErr_SetString(PyExc_KeyError, "Type incorrect");
                return NULL;
            }
            else if (ctype == 1) {
                for (i=0; i<inbord; i++) {
                    if (rval[i] != CALL_R8VIDE() ) {
                        PyList_Append(liste, PyFloat_FromDouble((double)rval[i]));
                    } else {
                        PyList_Append(liste, Py_None);
                    }
                }
            }
            else if (ctype == 2) {
                for (i=0; i<inbord; i++) {
                    if (ival[i] != CALL_ISNNEM() ) {
                        PyList_Append(liste, PyInt_FromLong((long)ival[i]));
                    } else {
                        PyList_Append(liste, Py_None);
                    }
                }
            }
            else if (ctype == 4 || ctype == 5 || ctype == 6 || ctype == 7 || ctype == 8) {
                switch ( ctype ) {
                    case 4 : ksize = 8;  break;
                    case 5 : ksize = 16; break;
                    case 6 : ksize = 24; break;
                    case 7 : ksize = 32; break;
                    case 8 : ksize = 80; break;
                }
                for (i=0; i<inbord; i++) {
                    kvar = kval + i*ksizemax;
                    if ( strncmp(kvar, blanc, ksize) != 0 ) {
                        PyList_Append(liste, PyString_FromStringAndSize(kvar, ksize));
                    } else {
                        PyList_Append(liste, Py_None);
                    }
                }
            }
            PyDict_SetItem(dico,key,liste);
            Py_XDECREF(key);
            Py_XDECREF(liste);
          }

          free(ival);
          free(rval);
          free(kval);
   }

   CALL_JEDEMA();
   free(nom);
   free(val);
   return dico;
}


/* ------------------------------------------------------------------ */
static PyObject* aster_oper(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp;
        INTEGER jxvrf=1 ; /* FORTRAN_TRUE */
        INTEGER iertot=0 ;
        INTEGER cmd=0 ;
        INTEGER pass=0 ;
        int ipass=0, icmd=0, ijxvrf;

        if (!PyArg_ParseTuple(args, "Oiii",&temp,&ijxvrf,&ipass,&icmd)) return NULL;
        jxvrf = (INTEGER)ijxvrf;
        pass = (INTEGER)ipass;
        cmd = (INTEGER)icmd;
        /* On empile le nouvel appel */
        commande=empile(temp);

        if(PyErr_Occurred()){
            fprintf(stderr,"Warning: une exception n'a pas ete traitee\n");
            PyErr_Print();
            fprintf(stderr,"Warning: on l'annule pour continuer mais elle aurait\n\
                            etre traitee avant\n");
            PyErr_Clear();
        }

        fflush(stderr) ;
        fflush(stdout) ;

        try(1){
                /*  appel du sous programme expass pour verif ou exec */
                CALL_EXPASS (&jxvrf,&pass,&cmd,&iertot);
                /* On depile l appel */
                commande = depile();
                return PyInt_FromLong((long)iertot); /*  retour de la fonction oper sous la forme d un entier */
        }
        finally{
                /* On depile l appel */
                commande = depile();
                /* une exception a ete levee, elle est destinee a etre traitee dans JDC.py */
                TraitementFinAster( exception_status ) ;
                return NULL;
        }
}

/* ------------------------------------------------------------------ */
static PyObject* aster_opsexe(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp;
        INTEGER ier=0 ;
        INTEGER cmd=0 ;
        INTEGER oper=0 ;
        INTEGER pass=0 ;
        int ipass=0, icmd=0, ioper=0;
        char *cmdusr="                                                                          ";

        if (!PyArg_ParseTuple(args, "Oiii",&temp,&icmd,&ipass,&ioper)) return NULL;
        pass=(INTEGER)ipass;
        cmd=(INTEGER)icmd;
        oper=(INTEGER)ioper;

        /* On empile le nouvel appel */
        commande=empile(temp);

        if(PyErr_Occurred()){
            fprintf(stderr,"Warning: une exception n'a pas ete traitee\n");
            PyErr_Print();
            fprintf(stderr,"Warning: on l'annule pour continuer mais elle aurait\n\
                            etre traitee avant\n");
            PyErr_Clear();
        }
        fflush(stderr) ;
        fflush(stdout) ;

        try(1){
                /*  appel du sous programme opsexe */
                CALL_OPSEXE (&cmd,&pass,&oper,cmdusr,&ier);
                /* On depile l appel */
                commande = depile();
                return PyInt_FromLong((long)ier); /*  retour de la fonction oper sous la forme d un entier */
        }
        finally{
                /* On depile l appel */
                commande = depile();
                /* une exception a ete levee, elle est destinee a etre traitee dans JDC.py */
                TraitementFinAster( exception_status ) ;
                return NULL;
        }
}


/* ------------------------------------------------------------------ */
static PyObject * aster_impers(self,args)
PyObject *self, *args; /* Not used */
{
   CALL_IMPERS ();
   Py_INCREF( Py_None ) ;
   return Py_None;
}

/* ------------------------------------------------------------------ */
static PyObject * aster_affich(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
      char *texte;
      char *nomfic;

      if (!PyArg_ParseTuple(args, "ss:affiche",&nomfic,&texte)) return NULL;
      CALL_AFFICH (nomfic,texte);

      Py_INCREF( Py_None ) ;
      return Py_None;
}

/* ------------------------------------------------------------------ */
INTEGER DEFS(JDCGET,jdcget,char *attr, STRING_SIZE l_attr)
{
/*
   Permet de récupérer la valeur entière d'un attribut du jdc.
*/
   PyObject *jdc, *val;
   INTEGER value;

   jdc = PyObject_GetAttrString(commande, "jdc");
   if (jdc == NULL)
      MYABORT("Impossible de recuperer l'attribut 'jdc' !");

   val = PyObject_CallMethod(jdc, "get_jdc_attr", "s#", attr, l_attr);
   if (val == NULL){
      printf("attribut inexistant dans le jdc : '%s'\n\n", attr);
      MYABORT("erreur dans JDCGET");
   }

   if (!PyInt_Check(val))
      MYABORT("Seuls les attributs de type entier peuvent etre recuperes !");

   value = (INTEGER)PyInt_AsLong(val);

   Py_XDECREF(jdc);
   Py_XDECREF(val);

   return value;
}

/* ------------------------------------------------------------------ */
void DEFSP(JDCSET,jdcset,char *attr, STRING_SIZE l_attr, INTEGER *value)
{
/*
   Permet de positionner la valeur entière d'un attribut du jdc à `value`.
*/
   PyObject *jdc, *res;

   jdc = PyObject_GetAttrString(commande, "jdc");
   if (jdc == NULL)
      MYABORT("Impossible de recuperer l'attribut 'jdc' !");

   res = PyObject_CallMethod(jdc, "set_jdc_attr", "s#i", attr, l_attr, (int)*value);
   if (res == NULL)
      MYABORT("erreur dans JDCSET");

   Py_XDECREF(jdc);
   Py_XDECREF(res);
}

/* ------------------------------------------------------------------ */
PyObject* GetJdcAttr(_IN char *attribut)
{
/*
   Retourne un attribut du 'jdc' en tant que PyObject.
*/
   PyObject *jdc, *objattr;

   jdc = PyObject_GetAttrString(commande, "jdc");
   if (jdc == NULL)
      MYABORT("Impossible de recuperer l'attribut 'jdc' !");

   objattr = PyObject_GetAttrString(jdc, attribut);
   /* traiter l'erreur "objattr == NULL" dans l'appelant */

   Py_XDECREF(jdc);
   return objattr;
}

/* ------------------------------------------------------------------ */
static PyObject * aster_onFatalError(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
/*
   Cette méthode définie le comportement lors des erreurs Fatales :

   aster.onFatalError('ABORT')
         => on s'arrête avec un JEFINI('ERREUR') dans UTFINM

   aster.onFatalError('EXCEPTION')
         => on lève l'exception aster.error

   aster.onFatalError()
         => retourne la valeur actuelle : 'ABORT' ou 'EXCEPTION'.
*/
      int len;
      INTEGER lng=0;
      char tmp[16+1];
      char *comport;
      PyObject *res=NULL;

      BLANK(tmp, 16);
      tmp[16] = '\0';
      len = -1;
      if (!PyArg_ParseTuple(args, "|s#:onFatalError",&comport ,&len)) return NULL;
      if (len == -1 || len == 0) {
            CALL_ONERRF(" ", tmp, &lng);
            res = PyString_FromStringAndSize(tmp, (Py_ssize_t)lng);

      } else if (strcmp(comport,"ABORT")==0 || strcmp(comport, "EXCEPTION")==0 || strcmp(comport, "EXCEPTION+VALID")==0 || strcmp(comport, "INIT")==0) {
            CALL_ONERRF(comport, tmp, &lng);
            Py_INCREF( Py_None ) ;
            res = Py_None;

      } else {
            printf("ERREUR : '%s' n'est pas une valeur autorisee.\n", comport);
            MYABORT("Argument incorrect dans onFatalError.");
      }
      return res;
}

/* ------------------------------------------------------------------ */
static PyObject * aster_ulopen(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        char *fichie;
        char *name;
        char *acces;
        char *autor;
        int iunit=0;
        INTEGER unit ;

        if (!PyArg_ParseTuple(args, "ssssi:ulopen",&fichie,&name,&acces,&autor,&iunit)) return NULL;
        unit=(INTEGER)iunit;
        CALL_ULOPEN (&unit,fichie,name,acces,autor);

        Py_INCREF( Py_None ) ;
        return Py_None;
}


/* ------------------------------------------------------------------ */
static PyObject * aster_fclose(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        int iunit=0;
        INTEGER unit ;

        if (!PyArg_ParseTuple(args, "i:fclose",&iunit)) return NULL;
        unit=(INTEGER)iunit;
        CALL_FCLOSE (&unit);

        Py_INCREF( Py_None ) ;
        return Py_None;
}


/* ------------------------------------------------------------------ */
static PyObject * aster_repout(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp = (PyObject*)0 ;
        INTEGER maj=1 ;
        INTEGER lnom=0;
        char nom[129];

        if (!PyArg_ParseTuple(args, "")) return NULL;
        BLANK(nom,128);
        nom[128]='\0';
        CALL_REPOUT (&maj,&lnom,nom);
        temp= PyString_FromStringAndSize(nom,FindLength(nom, (Py_ssize_t)lnom));
        return temp;
}

/* ------------------------------------------------------------------ */
static PyObject * aster_repdex(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp = (PyObject*)0 ;
        INTEGER maj=1 ;
        INTEGER lnom=0;
        char nom[129];

        if (!PyArg_ParseTuple(args, "")) return NULL;
        BLANK(nom,128);
        nom[128]='\0';
        CALL_REPDEX (&maj,&lnom,nom);
        temp= PyString_FromStringAndSize(nom,FindLength(nom, (Py_ssize_t)lnom));
        return temp;
}

/* ------------------------------------------------------------------ */
static PyObject * aster_gcncon(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
   PyObject *res;
   char *type, result[9];

   if (!PyArg_ParseTuple(args, "s", &type)) return NULL;
   BLANK(result, 8);
   result[8] = '\0';
   if (CALL_ISJVUP() == 1) {
      CALL_GCNCON (type, result);
   }
   res= PyString_FromStringAndSize(result,FindLength(result,8));
   return res;
}

/* ---------------------------------------------------------------------- */
static char rcvale_doc[] =
"Interface d'appel a la routine fortran RCVALE.\n"
"   Arguments : nommat, phenomene, nompar, valpar, nomres, stop\n"
"   Retourne  : valres, codret (tuples)\n"
" Aucune verification n'est faite sur les arguments d'entree (c'est l'appelant,\n"
" a priori mater_sdaster.rcvale, qui le fait)";

static PyObject * aster_rcvale(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
   char *nommat, *phenom;
   char *stop;
   PyObject *t_nompar, *t_valpar, *t_nomres;
   PyObject *t_valres, *t_codret;
   PyObject *t_res;
   int inbres, inbpar;
   INTEGER nbpar, nbres;
   char *nompar, *nomres, *codret;
   DOUBLE *valpar, *valres;
   int long_nompar = 8;       /* doivent impérativement correspondre aux  */
   int long_nomres = 8;       /* longueurs des chaines de caractères      */
   int long_codret = 2;       /* déclarées dans la routine fortran RCVALE */
   void *malloc(size_t size);

   if (!PyArg_ParseTuple(args, "ssOOOs", &nommat, &phenom, \
                  &t_nompar, &t_valpar, &t_nomres, &stop)) return NULL;

   /* Conversion en tableaux de chaines et réels */
   inbpar = PyTuple_Size(t_nompar);
   nbpar = (INTEGER)inbpar;
   nompar = (char *)malloc(long_nompar*inbpar*sizeof(char));
   convertxt(inbpar, t_nompar, nompar, long_nompar);

   valpar = (DOUBLE *)malloc(inbpar*sizeof(DOUBLE));
   convr8(inbpar, t_valpar, valpar);

   inbres = PyTuple_Size(t_nomres);
   nbres = (INTEGER)inbres;
   nomres = (char *)malloc(long_nomres*inbres*sizeof(char));
   convertxt(inbres, t_nomres, nomres, long_nomres);

   /* allocation des variables de sortie */
   valres = (DOUBLE *)malloc(inbres*sizeof(DOUBLE));
   codret = (char *)malloc(long_codret*inbres*sizeof(char));

   CALL_RCVALE(nommat, phenom, &nbpar, nompar, valpar, &nbres, nomres, valres, codret, stop);

   /* création des tuples de sortie */
   t_valres = MakeTupleFloat((long)inbres, valres);
   t_codret = MakeTupleString((long)inbres, codret, long_codret, NULL);

   /* retour de la fonction */
   t_res = PyTuple_New(2);
   PyTuple_SetItem(t_res, 0, t_valres);
   PyTuple_SetItem(t_res, 1, t_codret);

   free(nompar);
   free(valpar);
   free(nomres);
   free(valres);
   free(codret);

   return t_res;
}

/* ---------------------------------------------------------------------- */
static char dismoi_doc[] =
"Interface d'appel a la routine fortran DISMOI.\n"
"   usage: iret, repi, repk = aster.dismoi(codmes, question, concept, type_concept) \n\n"
"     codmes       :'F','E','A','I',...\n"
"     question     : texte de la question\n"
"     concept      : nom du concept\n"
"     type_concept : type du concept\n\n"
"   Retourne :\n"
"     iret         : 0 si ok, 1 en cas d'erreur\n"
"     repi         : reponse entiere\n"
"     repk         : reponse de type chaine de caracteres\n";

static PyObject * aster_dismoi(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
    char *codmes, *question, *concept, *typeconcept;
    INTEGER repi=0, iret;
    char repk[33], question32[33], concept32[33], typeconcept32[33], codme1[2];

    repk[32] = '\0';
    question32[32] = '\0';
    concept32[32] = '\0';
    typeconcept32[32] = '\0';
    codme1[1] = '\0';

    BLANK(repk, 32);
    if (!PyArg_ParseTuple(args, "ssss", &codmes, &question, &concept, &typeconcept))
        return NULL;

    CSTRING_FCPY(codme1, 1, codmes);
    CSTRING_FCPY(question32, 32, question);
    CSTRING_FCPY(concept32, 32, concept);
    CSTRING_FCPY(typeconcept32, 32, typeconcept);


    CALL_DISMOI(codme1, question32, concept32, typeconcept32, &repi, repk, &iret);

   return Py_BuildValue("iis", (int)iret, (int)repi, repk);
}

/* ---------------------------------------------------------------------- */
static char matfpe_doc[] =
"Interface d'appel a la routine C matfpe.\n"
"   usage: matfpe(actif)\n"
"     matfpe(-1) : on desactive l'interception des erreurs numeriques,\n"
"     matfpe(1)  : on active l'interception des erreurs numeriques.\n";

static PyObject * aster_matfpe(self, args)
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

/* ---------------------------------------------------------------------- */
static PyObject * aster_mdnoma(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp = (PyObject*)0 ;
        INTEGER lnomam=0;
        INTEGER codret=0;
        int lon1;
        char *nomast;
        char nomamd[33],n1[9];

        if (!PyArg_ParseTuple(args, "s#",&nomast,&lon1)) return NULL;
        BLANK(nomamd,32);
        nomamd[32]='\0';
        BLANK(n1,8);strncpy(n1,nomast,lon1);
        CALL_MDNOMA (nomamd,&lnomam,n1,&codret);

        temp= PyString_FromStringAndSize(nomamd,FindLength(nomamd, (Py_ssize_t)lnomam));
        return temp;
}

/* ------------------------------------------------------------------ */
static PyObject * aster_mdnoch(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
      PyObject *temp = (PyObject*)0 ;
      INTEGER lnochm=0;
      INTEGER lresu ;
      int ilresu;
      char *noresu;
      char *nomsym;
      char *nopase;
      INTEGER codret=0;
      int lon1,lon2,lon3;
      char nochmd[33],n1[33],n2[17],n3[9];

      if (!PyArg_ParseTuple(args, "is#s#s#",&ilresu,&noresu,&lon1,&nomsym,&lon2,&nopase,&lon3)) return NULL;
      BLANK(nochmd,32);
      nochmd[32]='\0';
      BLANK(n1,32);strncpy(n1,noresu,lon1);
      BLANK(n2,16);strncpy(n2,nomsym,lon2);
      BLANK(n3,8); strncpy(n3,nopase,lon3);
      lresu = (INTEGER)ilresu;
      CALL_MDNOCH (nochmd,&lnochm,&lresu,n1,n2,n3,&codret);
      temp= PyString_FromStringAndSize(nochmd,FindLength(nochmd, (Py_ssize_t)lnochm));
      return temp;
}


/* ------------------------------------------------------------------ */
void TraitementFinAster( _IN int val )
{
    PyObject *exc;
    
    if ( val == CodeFinAster ) {
        PyErr_SetString(PyExc_EOFError, "exit ASTER");
    } else {
        exc = PyObject_CallMethod(except_module, "get_exception", "i", val);
        //fprintf(stdout, "#DBG exception_args = ");
        //PyObject_Print(exception_args, stdout, 0);
        //fprintf(stdout, "\n");
        PyErr_SetObject(exc, exception_args);
    }
    
    return ;
}

/* ------------------------------------------------------------------ */
int RecupNomCas(void)
{
   /* recuperation du nom du cas */
   INTEGER un          = 1 ;
   INTEGER *iocc       = (INTEGER*)&un ;
   INTEGER *iarg       = (INTEGER*)&un ;
   INTEGER *mxval      = (INTEGER*)&un ;
   INTEGER nbval       = 1 ;
   int lng;
   INTEGER longueur;
   void *malloc(size_t size);
                                                   ASSERT(commande!=(PyObject*)0);
   CALL_GETLTX ( "CODE","NOM",iocc,iarg,mxval, &longueur ,&nbval) ;
   lng = (int)longueur;
   if(nbval == 0){
      /* Le mot cle NOM n'a pas ete fourni on donne un nom
         par defaut au nom du cas */
      NomCas = strdup("??????");
   }
   else if(nbval > 0){
                                                   ASSERT(lng>0);
      NomCas = (char*)(malloc((lng+1)*sizeof(char))) ;
      BLANK(NomCas, lng); /* initialisation a blanc */
      NomCas[lng]='\0';
                                                   ASSERT(NomCas!=(char*)0);
      CALL_GETVTX ( "CODE","NOM",iocc,iarg,mxval, NomCas ,&nbval) ;
   }
   else{
      /* Erreur  */
      PyErr_SetString(PyExc_KeyError, "Erreur a la recuperation du nom du cas");
      return -1;
   }
   return 0;
}

/* ------------------------------------------------------------------ */
static PyObject * aster_poursu(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        /*
        FONCTIONALITE : poursuite
        est appele par cata.POURSUITE (cf. ops.py)
        */
        PyObject *temp = (PyObject*)0 ;
        PyObject *concepts = (PyObject*)0 ;
        int ipass=0;
        INTEGER pass;
        INTEGER lot=1 ; /* FORTRAN_TRUE */
        INTEGER ier=0 ;
        INTEGER lonuti=0 ;
        static int nbPassages=0 ;
                                                                ASSERT((nbPassages==1)||(commande==(PyObject*)0));
        nbPassages++ ;
        if (!PyArg_ParseTuple(args, "Oi",&temp,&ipass)) return NULL;
        pass = (INTEGER)ipass;

        /* On empile le nouvel appel */
        commande=empile(temp);

        if(PyErr_Occurred()){
            fprintf(stderr,"Warning: une exception n'a pas ete traitee\n");
            PyErr_Print();
            fprintf(stderr,"Warning: on l'annule pour continuer mais elle aurait\n\
                            etre traitee avant\n");
            PyErr_Clear();
        }

        fflush(stderr) ;
        fflush(stdout) ;

        try(1){
                /*  appel de la commande debut (effectue dans POURSU) */
                /*  La routine fortran POURSU traite aussi le cas     */
                /*  de la poursuite de calcul (en retour lonuti       */
                /*  contient le nombre de concepts crees dans le      */
                /*  calcul precedent)                                 */

                CALL_POURSU (&pass,&ier,&lonuti);


                /* recuperation de la liste des concepts dans une     */
                /* string python                                      */

                concepts=PyString_FromStringAndSize(NULL, (Py_ssize_t)(lonuti*80) );
                CALL_GCCPTS (PyString_AsString(concepts), 80);
        }
        finally{
                /* On depile l appel */
                commande = depile();

                /* une exception a ete levee, elle est destinee a etre traitee dans JDC.py */
                TraitementFinAster( exception_status ) ;

                return NULL;
        }

        /* On recupere le nom du cas */
        if(RecupNomCas() == -1){
          /* Erreur a la recuperation */
          /* On depile l appel */
          commande = depile();

          return NULL;
        }
        else{
          /* On depile l appel */
          commande = depile();

          /*  retour de la fonction poursu sous la forme
           *  d'un tuple de trois entiers et un objet */
          return Py_BuildValue("(iiiN)", (int)lot, (int)ier, (int)lonuti, concepts );
        }
}

/* ------------------------------------------------------------------ */
static PyObject * aster_debut(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp = (PyObject*)0 ;
        int ipass=0;
        INTEGER pass;
        INTEGER lot=1 ; /* FORTRAN_TRUE */
        INTEGER ier=0 ;
        static int nbPassages=0 ;
                                                                ASSERT((nbPassages==1)||(commande==(PyObject*)0));
        nbPassages++ ;
        if (!PyArg_ParseTuple(args, "Oi",&temp,&ipass)) return NULL;
        pass = (INTEGER)ipass;

        /* On empile le nouvel appel */
        commande=empile(temp);

        if(PyErr_Occurred()){
            fprintf(stderr,"Warning: une exception n'a pas ete traitee\n");
            PyErr_Print();
            fprintf(stderr,"Warning: on l'annule pour continuer mais elle aurait\n\
                            etre traitee avant\n");
            PyErr_Clear();
        }

        fflush(stderr) ;
        fflush(stdout) ;

        try(1){
                /*  appel de la commande debut */
                CALL_DEBUT (&pass,&ier);
        }
        finally{
                /* On depile l appel */
                commande = depile();

                /* une exception a ete levee, elle est destinee a etre traitee dans JDC.py */
                TraitementFinAster( exception_status ) ;
                return NULL;
        }

        /* On recupere le nom du cas */
        if(RecupNomCas() == -1){
          /* Erreur a la recuperation */
          /* On depile l appel */
          commande = depile();
          return NULL;
        }
        else{
          /* On depile l appel */
          commande = depile();
          /*  retour de la fonction debut sous la forme d un tuple de deux entiers */
          return Py_BuildValue("(ii)", (int)lot, (int)ier );
        }
}

/* ------------------------------------------------------------------ */
static PyObject *aster_init(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
   INTEGER ier=0 ;
   int idbg=0;
   INTEGER dbg ; /* FORTRAN_FALSE */

   if (!PyArg_ParseTuple(args, "i",&idbg)) return NULL;
   dbg = (INTEGER)idbg;

   /* initialisation de la variable `static_module` */
   static_module = PyImport_ImportModule("Execution.E_Global");
   if (! static_module) {
      MYABORT("Impossible d'importer le module E_Global !");
   }

   fflush(stderr) ;
   fflush(stdout) ;

   CALL_IBMAIN(&dbg);

   /* jeveux est parti ! */
   jeveux_status = 1;

return PyInt_FromLong((long)ier);
}


/* ------------------------------------------------------------------ */
static PyObject *jeveux_getobjects( PyObject* self, PyObject* args)
{
    INTEGER nmax, total;
    char* base;
    PyObject* the_list, *pystr;
    char dummy[25];
    char *tmp, *ptr;
    int tmpsize, i;
    
    if (!PyArg_ParseTuple(args, "s",&base))
        return NULL;

    if (strlen(base)!=1) {
        MYABORT("le type de base doit etre 1 caractere" );
    }

    memset( dummy, ' ', 24 );
    dummy[24] = 0;
    nmax = 0;
    /* premier appel avec nmax==0 pour connaitre le total */
    CALL_JELST3( base, dummy, &nmax, &total );
    tmpsize = 24*(int)total*sizeof(char);
    tmp = (char*)malloc( tmpsize+1 );
    memset( tmp, ' ', tmpsize );
    tmp[tmpsize] = 0;
    nmax = total;
    /* second appel après allocation mémoire */
    CALL_JELST3( base, tmp, &nmax, &total );
    
    the_list = PyList_New( (Py_ssize_t)total);
    for( i=0, ptr=tmp; i<total;++i, ptr+=24 ) {
        pystr = PyString_FromStringAndSize( ptr, 24 );
        PyList_SetItem( the_list, i, pystr );
    }
    free( tmp );
    return the_list;
}


/* ------------------------------------------------------------------ */
static PyObject *jeveux_getattr( PyObject* self, PyObject* args)
{
    char *nomobj, *attr;
    char charval[34];
    INTEGER intval = 0;
    
    BLANK(charval, 33);
    charval[33] = '\0';
    if (!PyArg_ParseTuple(args, "ss",&nomobj,&attr))
        return NULL;
    CALL_JELIRA( nomobj, attr, &intval, charval );
    
    return Py_BuildValue( "is", (int)intval, charval );
}


static PyObject *jeveux_exists( PyObject* self, PyObject* args)
{
    char *nomobj;
    char tmpbuf[33];
    int l;
    INTEGER intval = 0;
    
    if (!PyArg_ParseTuple(args, "s",&nomobj))
        return NULL;
    strncpy( tmpbuf, nomobj, 32 );
    l = strlen( nomobj );
    memset( tmpbuf+l, ' ', 32-l );
    tmpbuf[32]=0;
        
    CALL_JEEXIN( tmpbuf, &intval );
    
    if (intval==0) {
        Py_INCREF( Py_False );
        return Py_False;
    } else {
        Py_INCREF( Py_True );
        return Py_True;
    }
}

/* ---------------------------------------------------------------------- */
static char jeinfo_doc[] =
"Interface d'appel a la routine fortran JEINFO.\n";

static PyObject * aster_jeinfo(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
   DOUBLE *rval;
   int i, longueur=9;
   PyObject *tup;

   rval = (DOUBLE *)malloc((longueur)*sizeof(DOUBLE));
   CALL_JEINFO(rval);

   tup = PyTuple_New( (Py_ssize_t)longueur ) ;
   for(i=0; i < longueur; i++) {
      PyTuple_SetItem( tup, i, PyFloat_FromDouble( (double)rval[i] )) ;
   }
   free((char *)rval);

   return tup;
}

/* ------------------------------------------------------------------ */
/*      Routines d'interface pour l'enregistrement d'un concept       */
/*                      dans la liste jeveux                          */
static PyObject* aster_co_register_jev(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
/*       usage : aster.co_register_jev(nomsd, typsd, nomcmd)
                 appele gcugen(0, ...) puis gcugen(1, ...)
*/
    char *nomsd, *typsd, *oper;
    INTEGER icmdt = 0, icode;
    
    if (!PyArg_ParseTuple(args, "sss", &nomsd, &typsd, &oper))
        return NULL;
    icode = 0;
    CALL_GCUGEN( &icode, nomsd, typsd, oper, &icmdt );
    icode = 1;
    CALL_GCUGEN( &icode, nomsd, typsd, oper, &icmdt );
    
    return PyInt_FromLong( (long)icmdt );
}

/* ------------------------------------------------------------------ */


/* ------------------------------------------------------------------ */
/*             Routines d'interface pour la sensibilité               */
void DEFSSSP(PSGENC,psgenc, _IN  char *nosimp, STRING_SIZE lnosimp,
                            _IN  char *nopase, STRING_SIZE lnopase,
                            _OUT char *nocomp, STRING_SIZE lnocomp,
                            _OUT INTEGER *iret)
{
/*       Récupère le nom composé bati sur un couple :
           ( structure de base , paramètre de sensibilité )

      IN  nosimp  : nom de la sd de base
      IN  nopase  : nom du parametre de sensibilite
      OUT nocomp  : nom de la sd derivee
      OUT iret    : code retour :
                     0 -> tout s'est bien passe
                     1 -> le couple (nosimp,nopase) n'est pas renseigne
*/
   PyObject *memo_sensi, *val;
   char *sret = (char*)0;
   int longueur;
   *iret = 0;

   memo_sensi = GetJdcAttr("memo_sensi");
   if (memo_sensi == NULL)
      MYABORT("Impossible de recuperer l'attribut 'memo_sensi' du jdc !");

   val = PyObject_CallMethod(memo_sensi, "get_nocomp", "s#s#", nosimp, lnosimp, nopase, lnopase);
   if (val == NULL){
      MYABORT("erreur lors de l'appel a memo_sensi.get_nocomp !");
   }

   sret = PyString_AsString(val);
   longueur = strlen(sret);
   STRING_FCPY(nocomp, lnocomp, sret, longueur);
   if ( strncmp(nocomp, "????????", 8) == 0 ) {
      *iret = 1;
   }

   Py_XDECREF(memo_sensi);
   Py_XDECREF(val);
}

/* ------------------------------------------------------------------ */
void DEFSSPSSS(PSGEMC,psgemc, _IN  char *nosimp, STRING_SIZE lnosimp,
                              _IN  char *nopase, STRING_SIZE lnopase,
                              _OUT INTEGER *nbmocl,
                              _OUT char *limocl, STRING_SIZE l1,
                              _OUT char *livale, STRING_SIZE l2,
                              _OUT char *limofa, STRING_SIZE l3)
{
/*       Récupère les mots-clés associés à un couple :
           ( structure de base , paramètre de sensibilité )

      IN  nosimp  : nom de la sd de base
      IN  nopase  : nom du parametre de sensibilite
      OUT nbmocl  : nombre de mots-cles associes a (nosimp,nopase)
      OUT limocl  : la structure k80 contenant les mots-cles concernes
      OUT livale  : la structure k80 contenant les valeurs concernees
      OUT limofa  : la structure k80 contenant les mots-cles facteurs

   Remarque 1 : Deux temps pour permettre à l'appelant de dimensionner les vecteurs de chaines.
                  1er appel avec nbmocl = 0
                     --> retourne nbmocl
                  2ème appel avec nbmocl à la bonne valeur (on le vérifie)
                     --> remplit limocl, livale, limofa

   Remarque 2 : AVANT limocl, livale et limofa étaient des noms de vecteurs dans ZK80.
                MAINTENANT ce sont des CHARACTER*80(nbmocl)
*/
   PyObject *memo_sensi;
   PyObject *tup3, *tmocl, *tvale, *tmofa;

   memo_sensi = GetJdcAttr("memo_sensi");
   if (memo_sensi == NULL)
      MYABORT("Impossible de recuperer l'attribut 'memo_sensi' du jdc !");

   tup3 = PyObject_CallMethod(memo_sensi, "get_mcle", "s#s#", nosimp, lnosimp, nopase, lnopase);
   if (tup3 == NULL) {
      MYABORT("erreur lors de l'appel a memo_sensi.get_mcle !");
   }
   tmocl = PyTuple_GetItem(tup3, 0);
   tvale = PyTuple_GetItem(tup3, 1);
   tmofa = PyTuple_GetItem(tup3, 2);

   if (*nbmocl == 0) {
      /* Seul nbmocl est modifié (1er appel) */
      *nbmocl = (INTEGER)PyTuple_Size(tmocl);
   } else {
      /* On remplit les vecteurs limocl, livale, limofa (2ème appel) */
      if (*nbmocl != (INTEGER)PyTuple_Size(tmocl)) {
         MYABORT("erreur dans psgemc : nbmocl n'a pas la bonne valeur !");
      }
      convertxt((int)*nbmocl, tmocl, limocl, l1);
      convertxt((int)*nbmocl, tvale, livale, l2);
      convertxt((int)*nbmocl, tmofa, limofa, l3);
   }

   Py_XDECREF(memo_sensi);
   Py_XDECREF(tup3);
}

/* ------------------------------------------------------------------ */
void DEFSPSP(PSINFO,psinfo, _IN  char *nomsd, STRING_SIZE lnomsd,
                            _OUT INTEGER *nbstse,
                            _OUT char *nostnc, STRING_SIZE lnostnc,
                            _OUT INTEGER *iderive)
{
/*    a. Si nomsd est un nom de concept donné, on récupère :
            . le nombre de structures sensibles associées
            . un tableau contenant les couples :
               (nom composé, nom du paramètre sensible)
            . iderive = 0

      b. si nomsd est une structure dérivée, on récupère :
            . nbstse = 1
            . un tableau contenant le couple :
                  (nom du concept, nom du paramètre sensible)
            . iderive = 1

      IN  nomsd   : nom de la sd de base
      IN/OUT nbstse : nombre de structures sensibles associees a nomsd
                     . -1 si nomsd est une structure derivee
                     . le nombre de structures sensibles associees, si nomsd
                     est derivee
                     . 0 sinon
      OUT nostnc  : structure qui contient les nbstse couples
                     (nom compose, nom du parametre sensible)
                     elle est allouee ici

   Remarque 1 : Deux temps pour permettre à l'appelant de dimensionner le vecteur de chaines.
                  1er appel avec nbstse = 0
                     --> retourne nbstse et iderive
                  2ème appel avec nbstse à la bonne valeur (on le vérifie)
                     --> remplit nostnc

   Remarque 2 : AVANT nostnc étaient le nom d'un vecteur dans ZK80.
                MAINTENANT c'est un CHARACTER*80(nbstse)

   Remarque 3 : si cette structure de mémorisation est inconnue, on en
                conclut qu'aucun calcul de sensibilité n'a été demandé.
*/
   PyObject *memo_sensi, *tup2;
   PyObject *indic, *result;
   int size_result;

   memo_sensi = GetJdcAttr("memo_sensi");
   if (memo_sensi == NULL)
      MYABORT("Impossible de recuperer l'attribut 'memo_sensi' du jdc !");

   tup2 = PyObject_CallMethod(memo_sensi, "psinfo", "s#", nomsd, lnomsd);
   indic = PyTuple_GetItem(tup2, 0);
   result = PyTuple_GetItem(tup2, 1);
   size_result = PyTuple_Size(result);

   *iderive = (INTEGER)PyInt_AsLong(indic);
   if (*nbstse == 0) {
      /* Seul nbstse est modifié (1er appel) */
      *nbstse = (INTEGER)size_result / 2;
   } else {
      /* On remplit le vecteur nostnc (2ème appel) */
      if (*nbstse != (INTEGER)size_result / 2) {
         MYABORT("erreur dans psinfo : nbstse n'a pas la bonne valeur !");
      }
      convertxt((int)size_result, result, nostnc, lnostnc);
   }

   Py_XDECREF(memo_sensi);
   Py_XDECREF(tup2);
}

/* ----------------------   FIN Sensibilité   ----------------------- */
/* ------------------------------------------------------------------ */


/* ------------------------------------------------------------------ */
/*   Routines d'interface pour le catalogue de loi de comportement    */
/* ------------------------------------------------------------------ */
void DEFPSS(LCCREE, lccree, _IN INTEGER *nbkit,
                            _IN char *lkit, STRING_SIZE llkit,
                            _OUT char *compor, STRING_SIZE lcompor)
{
/*
   Créer un assemblage de LC composé des comportements listés dans 'list_kit'
   et retourne le nom attribué automatiquement à ce comportement.

      CALL LCCREE(NBKIT, LKIT, COMPOR)
      ==> comport = catalc.create(*list_kit)
*/
   PyObject *catalc, *res, *tup_kit;
   char *scomp;
   int lsc;

   catalc = GetJdcAttr("catalc");
   /* transforme le tableau de chaines fortran en tuple */
   tup_kit = MakeTupleString((long)*nbkit, lkit, llkit, NULL);

   res = PyObject_CallMethod(catalc, "create", "O", tup_kit);
   if (res == NULL) {
      MYABORT("Echec lors de la creation du comportement (lccree/create) !");
   }

   scomp = PyString_AsString(res);
   lsc = strlen(scomp);
                                                              ASSERT(lsc <= lcompor) ;
   STRING_FCPY(compor, lcompor, scomp, lsc);

   Py_XDECREF(res);
   Py_XDECREF(tup_kit);
   Py_XDECREF(catalc);
}

/* ------------------------------------------------------------------ */
void DEFSS(LCALGO, lcalgo, _IN char *compor, STRING_SIZE lcompor,
                           _OUT char *algo, STRING_SIZE lalgo
                            )
{
/*
   Retourne le premier algorithme d'intégration

      CALL LCALGO(COMPOR, ALGO)
      ==> algo_inte = catalc.get_algo(COMPOR)
*/
   PyObject *catalc, *res;

   catalc = GetJdcAttr("catalc");
   res = PyObject_CallMethod(catalc, "get_algo", "s#", compor, lcompor);
   if (res == NULL) {
      MYABORT("Echec lors de la recuperation du premier algorithme d'integration (lcalgo/get_algo) !");
   }

   convertxt(1, res, algo, lalgo);

   Py_XDECREF(res);
   Py_XDECREF(catalc);
}

/* ------------------------------------------------------------------ */
void DEFSPP(LCINFO, lcinfo, _IN char *compor, STRING_SIZE lcompor,
                            _OUT INTEGER *numlc,
                            _OUT INTEGER *nbvari)
{
/*
   Retourne le numéro de routine et le nbre de variables internes

      CALL LCINFO(COMPOR, NUMLC, NBVARI)
      ==> num_lc, nb_vari = catalc.get_info(COMPOR)
*/
   PyObject *catalc, *res;

   catalc = GetJdcAttr("catalc");
   res = PyObject_CallMethod(catalc, "get_info", "s#", compor, lcompor);
   if (res == NULL) {
      MYABORT("Echec lors de la recuperation des informations sur le comportement (lcinfo/get_info) !");
   }

   *numlc  = (INTEGER)PyInt_AsLong(PyTuple_GetItem(res, 0));
   *nbvari = (INTEGER)PyInt_AsLong(PyTuple_GetItem(res, 1));

   Py_XDECREF(res);
   Py_XDECREF(catalc);
}

/* ------------------------------------------------------------------ */
void DEFSPS(LCVARI, lcvari, _IN char *compor, STRING_SIZE lcompor,
                            _IN INTEGER *nbvari,
                            _OUT char *nomvar, STRING_SIZE lnomvar)
{
/*
   Retourne la liste des variables internes

      CALL LCVARI(COMPOR, NBVARI, LVARI)
      ==> nom_vari = catalc.get_vari(COMPOR)
*/
   PyObject *catalc, *res;

   catalc = GetJdcAttr("catalc");
   res = PyObject_CallMethod(catalc, "get_vari", "s#", compor, lcompor);
   if (res == NULL) {
      MYABORT("Echec lors de la recuperation des noms des variables internes du comportement (lcvari/get_vari) !");
   }

   convertxt((int)*nbvari, res, nomvar, lnomvar);

   Py_XDECREF(res);
   Py_XDECREF(catalc);
}

/* ------------------------------------------------------------------ */
void DEFSSSP(LCTEST, lctest, _IN char *compor, STRING_SIZE lcompor,
                             _IN char *prop, STRING_SIZE lprop,
                             _IN char *valeur, STRING_SIZE lvaleur,
                             _OUT INTEGER *iret)
{
/*
   Est-ce que VALEUR est un valeur autorisée de PROPRIETE ?
         CALL LCTEST(COMPOR, PROPRIETE, VALEUR, IRET)
         ==> iret = catalc.query(COMPOR, PROPRIETE, VALEUR)
*/
   PyObject *catalc, *res;

   catalc = GetJdcAttr("catalc");
   res = PyObject_CallMethod(catalc, "query", "s#s#s#", compor, lcompor, prop, lprop, valeur, lvaleur);
   if (res == NULL) {
      MYABORT("Echec lors du test d'une propriete du comportement (lctest/query) !");
   }

   *iret = (INTEGER)PyInt_AsLong(res);

   Py_XDECREF(res);
   Py_XDECREF(catalc);
}

/* ----------   FIN catalogue de loi de comportement   -------------- */
/* ------------------------------------------------------------------ */



/* ------------------------------------------------------------------ */
static PyObject *aster_argv( _UNUSED  PyObject *self, _IN PyObject *args )
{
        /*
        A partir des arguments passes au script python, construction du
        tableau de chaines de caracteres C "char** argv" et initialisation
        de "argc" : la taille de ce tableau.
        Puis appel de Code_Aster en passant ces informations en argument.
        */
        int        k       = 0 ;
        long       argc    = 0 ;
        PyObject  *liste   = NULL ;
        PyObject  *string  = NULL ;
        char     **argv    = NULL ;

        void asterm( long , char** ) ;
        void *malloc(size_t size);
        /*
           la fonction aster_argv recoit un tuple d'arguments (ici de taille 1°
           dans lequel est stockee la liste, qui est extraite par l'appel a
           PyArg_ParseTuple.
        */
        if (!PyArg_ParseTuple(args, "O" , &liste )) return NULL;
        /*  Allocation dynamique de argv : on ajoute un argument NULL */

        argc=PyList_GET_SIZE(liste) ;

        argv = (char**)malloc((1+argc)*sizeof(char*)) ;
        argv[argc]=(char*)0 ;

        /* conversion de chaque element de la liste en une chaine */
        for ( k=0 ; (long)k<argc ; k++ ){
                string=PyList_GetItem(liste,k) ;
                                                                ASSERT(string!=NULL);
                                                                ASSERT(PyString_Check(string));
                argv[k]=PyString_AsString( string ) ;
                                                                ASSERT(argv[k]!=NULL);
        }

        /* Passage des arguments a Code_Aster */
        asterm(argc,argv) ;
                                                                ASSERT(argv) ;
        free(argv);
        argv=(char**)0 ;

        Py_INCREF( Py_None ) ;
        return Py_None;
}


/* ------------------------------------------------------------------ */
/* List of functions defined in the module */

static PyMethodDef aster_methods[] = {
                {"onFatalError", aster_onFatalError, METH_VARARGS},
                {"fclose",       aster_fclose,       METH_VARARGS},
                {"ulopen",       aster_ulopen,       METH_VARARGS},
                {"affiche",      aster_affich,       METH_VARARGS},
                {"init",         aster_init,         METH_VARARGS},
                {"debut",        aster_debut,        METH_VARARGS},
                {"poursu",       aster_poursu,       METH_VARARGS},
                {"oper",         aster_oper,         METH_VARARGS},
                {"opsexe",       aster_opsexe,       METH_VARARGS},
                {"repout",       aster_repout,       METH_VARARGS},
                {"impers",       aster_impers,       METH_VARARGS},
                {"repdex",       aster_repdex,       METH_VARARGS},
                {"mdnoma",       aster_mdnoma,       METH_VARARGS},
                {"mdnoch",       aster_mdnoch,       METH_VARARGS},
                {"rcvale",       aster_rcvale,       METH_VARARGS, rcvale_doc},
                {"dismoi",       aster_dismoi,       METH_VARARGS, dismoi_doc},
                {"matfpe",       aster_matfpe,       METH_VARARGS, matfpe_doc},
                {"argv",         aster_argv,         METH_VARARGS},
                {"prepcompcham", aster_prepcompcham, METH_VARARGS},
                {"getvectjev",   aster_getvectjev,   METH_VARARGS, getvectjev_doc},
                {"putvectjev",   aster_putvectjev,   METH_VARARGS, putvectjev_doc},
                {"putcolljev",   aster_putcolljev,   METH_VARARGS, putcolljev_doc},
                {"getcolljev",   aster_getcolljev,   METH_VARARGS, getcolljev_doc},
                {"GetResu",      aster_GetResu,      METH_VARARGS},
                {"co_register_jev", aster_co_register_jev, METH_VARARGS},
                {"jeveux_getobjects", jeveux_getobjects, METH_VARARGS},
                {"jeveux_getattr", jeveux_getattr,   METH_VARARGS},
                {"jeveux_exists", jeveux_exists,     METH_VARARGS},
                {"jeinfo",       aster_jeinfo,       METH_VARARGS, jeinfo_doc},
                {"get_nom_concept_unique", aster_gcncon, METH_VARARGS},
                {NULL,                NULL}/* sentinel */
};


/* ------------------------------------------------------------------ */
void initvers(PyObject *dict)
{
    PyObject *v;
    INTEGER vers,util,nivo;
    INTEGER exploi;
    char date[16+1];
    char rev[8];
    BLANK(date, 16);
    date[16] = '\0';

    CALL_VERSIO(&vers,&util,&nivo,date,&exploi);
    sprintf(rev,"%ld.%ld.%ld", (long)vers, (long)util, (long)nivo);
    PyDict_SetItemString(dict, "__version__", v = PyString_FromString(rev));
    Py_XDECREF(v);
}


/* Initialization function for the module (*must* be called initaster) */
static char aster_module_documentation[] =
"C implementation of the Python aster module\n"
"\n"
;

DL_EXPORT(void) initaster()
{
        PyObject *m = (PyObject*)0 ;
        PyObject *d = (PyObject*)0 ;

        /* Create the module and add the functions */
        m = Py_InitModule3("aster", aster_methods, aster_module_documentation);

        /* Add some symbolic constants to the module */
        d = PyModule_GetDict(m);

        initvers(d);
        initExceptions(d);

        /* Initialisation de la pile d appel des commandes */
        pile_commandes = PyList_New(0);
}



/* ------------------------------------------------------------------ */
void AfficheChaineFortran( _IN char *chaine , _IN STRING_SIZE longueur )
{
        /* Traitement des chaines fortran : pour le deboguage uniquement*/
        static FILE *strm ; /* le stream de sortie pointe sur la stderr */
        strm=stderr;

        if ( longueur ){
                int k=0 ;
                fprintf( strm , "'" ) ;
                for ( k=0 ; k<((longueur<=512)?longueur:512) ; k++ ){
                        fprintf( strm , "%c" , chaine[k] ) ;
                }
                fprintf( strm , "'\n" ) ;
                fflush(strm) ;
        }
        return ;
}

int EstPret( _IN char *chaine , _IN STRING_SIZE longueur )
{
        /*
        Fonction  : EstPret
        Intention
                dit si "chaine" destinee a etre exploitee par un module fortran,
                est une commande ASTER i.e. si elle est composee uniquement de lettres,
                de chiffres, de _ et de caracteres blancs et si elle contient un
                caractere non blanc.
        */
        int pret     = 0 ;
        int k        = 0 ;
        int taille   = 0 ;

        taille = ( longueur < 1024 ) ? FindLength( chaine , longueur ) : 1024 ;
        if ( (int)taille >= 0 ){
                pret = 1 ;
                if( isalpha(chaine[0]) ){
                        for( k=0 ; pret==1 && k<(int)longueur ; k++ ){
                                pret = ( EstValide(chaine[k] ) ) ? 1 : 0 ;
                                if ( pret != 1 ){
                                        fprintf( stderr , "CARACTERE %d INVALIDE '%c' %d\n" , (int)k , chaine[k] , (int)chaine[k]) ;
                                }
                        }
                }
                else{
                        fprintf( stderr , "PREMIER CARACTERE INVALIDE '%c' %d\n" , chaine[0] , (int)chaine[0]) ;
                }
                if ( pret != 1 ){
                }
        }
        return pret ;
}


/* ------------------------------------------------------------------ */
void DEFP(GETCMC,getcmc,INTEGER *icmc)
{
        /*
          Procedure GETCMC : emule la procedure equivalente ASTER

          Entrees : aucune
          Sorties :
            icmc   : numero de la commande
          Fonction :
            Retourne le numero de la commande courante

        */
        PyObject * res = (PyObject*)0 ;
        res = PyObject_GetAttrString(commande,"icmd");
        /*
                    Si le retour est NULL : une exception a ete levee dans le code Python appele
                    Cette exception est a transferer normalement a l appelant mais FORTRAN ???
                    On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
                MYABORT("erreur a l appel de getcmc dans la partie Python");

        *icmc = (INTEGER)PyInt_AsLong(res);
        Py_DECREF(res);
}
