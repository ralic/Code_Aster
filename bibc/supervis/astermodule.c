/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF astermodule supervis  DATE 16/05/2007   AUTEUR COURTOIS M.COURTOIS */
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
/* RESPONSABLE                                  COURTOIS M.COURTOIS   */
/* ------------------------------------------------------------------ */

#include <stdio.h>
#include "Python.h"
#include <math.h>
#include <ctype.h>
#include <string.h>
#include "aster.h"
#include <Numeric/arrayobject.h>

/* --- declarations des interfaces des fonctions de ce fichier --- */

static PyObject *aster_argv( _UNUSED  PyObject *self, _IN PyObject *args ) ;
const char *aster_ident() ;

int EstPret( _IN char *chaine , _IN int longueur ) ;
long FindLength( _IN char *chaineFortran , _IN INTEGER longueur ) ;
void AfficheChaineFortran( _IN char *chaine , _IN int longueur ) ;
void TraiteMessageErreur( _IN char* ) ;
void PRE_myabort( _IN const char *nomFichier , _IN const int numeroLigne , _IN const char *message ) ;
#define MYABORT(message) PRE_myabort( __FILE__ , __LINE__ , message )


char * fstr1( _IN char *s, _IN int l) ;
char * fstr2( _IN char *s, _IN int l) ;
char * fstr3( _IN char *s, _IN int l) ;
void convert( _IN int nval, _IN PyObject *tup, _OUT INTEGER *val) ;
void convertxt( _IN int nval, _IN PyObject *tup, _OUT char *val, _IN int taille) ;
void converltx( _IN int nval, _IN PyObject *tup, _OUT char *val, _IN int taille) ;

void AjoutChaineA( _INOUT char **base , _IN char *supplement ) ;

void TraitementFinAster( _IN int val ) ;

PyObject * MakeTupleString(long nbval,char *kval,int lkval,INTEGER *lval);
PyObject * MakeListString( long nbval,char *kval,int lkval );
PyObject * MakeTupleInt(long nbval,long* kval);
PyObject * MakeListInt(long nbval,long* kval);
PyObject * MakeTupleFloat(long nbval,double* kval);
PyObject * MakeListFloat(long nbval,double* kval);

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

#define CodeFinAster       19
#define CodeAbortAster     20
#define CodeErrorAster     21
#define CodeNonConvergenceAster           22
#define CodeEchecComportementAster        23
#define CodeBandeFrequenceVideAster       24
#define CodeMatriceSinguliereAster        25
#define CodeTraitementContactAster        26
#define CodeMatriceContactSinguliereAster 27
#define CodeArretCPUAster                 28

int exception_status=-1;
#define REASONMAX 800
static char exception_reason[REASONMAX+1];

#define NIVMAX 10
static int niveau=0;

#ifdef _UTILISATION_SETJMP_
#include <setjmp.h>

static jmp_buf env[NIVMAX+1] ;           /* utilise par longjmp, le type jmp_buf est defini dans setjmp.h */
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
        case CodeAbortAster :
                abort();
                break ;
        case CodeErrorAster :
                abort();
                break ;

        /* exceptions particularisées */
        case CodeNonConvergenceAster :
                abort();
                break ;
        case CodeEchecComportementAster :
                abort();
                break ;
        case CodeBandeFrequenceVideAster :
                abort();
                break ;
        case CodeMatriceSinguliereAster :
                abort();
                break ;
        case CodeTraitementContactAster :
                abort();
                break ;
        case CodeMatriceContactSinguliereAster :
                abort();
                break ;
        case CodeArretCPUAster :
                abort();
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

/* NomCas est initialise dans aster_debut() */
/* NomCas est initialise a blanc pour permettre la recuperation de la
   trace des commandes lors de l'appel a debut ou poursuite. On ne connait
   pas encore NomCas qui sera initialise lors de l'appel a RecupNomCas */
static char *NomCas          = "        ";

/* ------------------------------------------------------------------ */
void STDCALL(XFINI,xfini)(_IN INTEGER *code)
{
   /* jeveux est fermé */
   jeveux_status = 0;
   
   switch( *code ){
        case CodeFinAster :
                strcpy(exception_reason,"exit ASTER");
                break ;
        case CodeAbortAster :
                strcpy(exception_reason,"abort ASTER");
                break ;
        default:
                *code=CodeAbortAster;
                strcpy(exception_reason,"abort ASTER");
                break ;
        }
   TraiteErreur(*code);
}

/* ------------------------------------------------------------------ */
/*
    Les exceptions levees dans le Fortran par les developpeurs
    doivent etre des objets de la classe AsterError (numero equivalent 21) ou d'une classe
    derivee.
 */
/* exceptions de base */
static PyObject *AsterError = (PyObject*)0 ; /* Ce type d'exception est levee sur appel de XFINI avec le parametre 21 */
static PyObject *FatalError = (PyObject*)0 ; /* Ce type d'exception est levee sur appel de XFINI avec le parametre 20 */

/* exceptions particularisées */
static PyObject *NonConvergenceError = (PyObject*)0 ;           /* Exception non convergence */
static PyObject *EchecComportementError = (PyObject*)0 ;        /* Exception échec intégration du comportement */
static PyObject *BandeFrequenceVideError = (PyObject*)0 ;       /* Exception bande de fréquence vide */
static PyObject *MatriceSinguliereError = (PyObject*)0 ;        /* Exception matrice singuliere */
static PyObject *TraitementContactError = (PyObject*)0 ;        /* Exception échec de traitement du contact */
static PyObject *MatriceContactSinguliereError = (PyObject*)0 ; /* Exception matrice de contact non inversible */
static PyObject *ArretCPUError = (PyObject*)0 ;                 /* Exception manque de temps CPU */

void initExceptions(PyObject *dict)
{
        /* type d'exception ERREUR <S> */
        AsterError = PyErr_NewException("aster.error", NULL, NULL);
        if(AsterError != NULL) PyDict_SetItemString(dict, "error", AsterError);
        /* type d'exception ERREUR <F> */
        FatalError = PyErr_NewException("aster.FatalError", NULL, NULL);
        if(FatalError != NULL) PyDict_SetItemString(dict, "FatalError", FatalError);

        /* Exceptions particularisées */
        NonConvergenceError = PyErr_NewException("aster.NonConvergenceError", AsterError, NULL);
        if(NonConvergenceError != NULL) PyDict_SetItemString(dict, "NonConvergenceError", NonConvergenceError);

        EchecComportementError = PyErr_NewException("aster.EchecComportementError", AsterError, NULL);
        if(EchecComportementError != NULL) PyDict_SetItemString(dict, "EchecComportementError", EchecComportementError);

        BandeFrequenceVideError = PyErr_NewException("aster.BandeFrequenceVideError", AsterError, NULL);
        if(BandeFrequenceVideError != NULL) PyDict_SetItemString(dict, "BandeFrequenceVideError", BandeFrequenceVideError);

        MatriceSinguliereError = PyErr_NewException("aster.MatriceSinguliereError", AsterError, NULL);
        if(MatriceSinguliereError != NULL) PyDict_SetItemString(dict, "MatriceSinguliereError", MatriceSinguliereError);

        TraitementContactError = PyErr_NewException("aster.TraitementContactError", AsterError, NULL);
        if(TraitementContactError != NULL) PyDict_SetItemString(dict, "TraitementContactError", TraitementContactError);

        MatriceContactSinguliereError = PyErr_NewException("aster.MatriceContactSinguliereError", AsterError, NULL);
        if(MatriceContactSinguliereError != NULL) PyDict_SetItemString(dict, "MatriceContactSinguliereError", MatriceContactSinguliereError);

        ArretCPUError = PyErr_NewException("aster.ArretCPUError", AsterError, NULL);
        if(ArretCPUError != NULL) PyDict_SetItemString(dict, "ArretCPUError", ArretCPUError);
}

/* ------------------------------------------------------------------ */
/*
  Subroutine appelable depuis le Fortran pour demander la levee d'une exception de type exc_type
  Une chaine de charactere (reason) ajoute un commentaire au type d'exception
*/
void DEFPS(UEXCEP,uexcep,_IN INTEGER *exc_type,  _IN char *reason , _IN int lreason )
{
   int l;
   l=min(FindLength(reason,lreason),REASONMAX);
   strncpy(exception_reason,reason,l);
   exception_reason[l]='\0';
   TraiteErreur(*exc_type);
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
        if(exception_flag[niveau]==1){
          int l;
          exception_flag[niveau]=0;
          l=min(REASONMAX,strlen(message));
          strncpy(exception_reason,message,l);
          exception_reason[l+1]='\0';
          throw(CodeAbortAster);
        }
        else{
          abort();
        }
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
void DEFSSPPPPP(GETLTX,getltx,_IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle,_IN INTEGER *iocc,
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
        ioc=*iocc ;
        ioc=ioc-1 ;
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getltx","ssiii",mfc,mcs,ioc,*iarg,*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if (!ok)MYABORT("erreur dans la partie Python");

        nval=*nbval;
        if(*nbval < 0)nval=*mxval;
        convert(nval,tup,isval);
        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return ;
}


/* ------------------------------------------------------------------ */
char * fstr1( _IN char *s, _IN int l)
{
        /*
        copie l caracteres d'une chaine de caracteres fortran s dans la chaine
        statique globale nom_fac, et retourne un pointeur sur nom_fac
        */
        strncpy(nom_fac, s, l );
        nom_fac[l]='\0';
        return nom_fac;
}
char * fstr2( _IN char *s, _IN int l)
{
        /*
        copie l caracteres d'une chaine de caracteres fortran s dans la chaine
        statique globale nom_cle, et retourne un pointeur sur nom_cle
        */
        strncpy(nom_cle, s, l );
        nom_cle[l]='\0';
        return nom_cle;
}
char * fstr3( _IN char *s, _IN int l)
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
void DEFSP(GETFAC,getfac,_IN char *nomfac, _IN int lfac, _OUT INTEGER *occu)
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

        *occu=PyInt_AsLong(res);

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return ;
}


/* ------------------------------------------------------------------ */
void convc8( _IN int nval, _IN PyObject *tup, _OUT double *val)
{
        /*
                  tup est un tuple de tuples internes, chaque tuple
                interne contenant le type et les deux parties du complexe.
        */
        int    i = 0 ;
        int    k = 0 ;
        int conv_un_c8( _IN PyObject *tup, _OUT double *val) ;
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
int conv_un_c8( _IN PyObject *tup, _OUT double *val)
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
           *val    =PyComplex_RealAsDouble(tup)  ;
           *(val+1)=PyComplex_ImagAsDouble(tup)  ;
        }
        else if(PyTuple_Check(tup)){
           /* On est dans le cas d'un complexe représenté par un triplet : "RI" ou "MP",x,y */
           if(!PyArg_ParseTuple(tup,"sdd",&repres,&x,&y))
                     MYABORT("erreur dans la partie Python");
                                                                                     ASSERT((strcmp(repres,"RI")==0)||(strcmp(repres,"MP")==0)) ;
           if (strcmp(repres,"RI")==0){
                /* representation : partie reelle/partie imaginaire */
                *val    =x ;
                *(val+1)=y ;
           }
           else{
                /* representation RHO,THETA (les angles sont fournis en degres) */
                *val    =*rho * cos( *theta /180. * R8PI()) ;
                *(val+1)=*rho * sin( *theta /180. * R8PI()) ;
           }
        }
        else {
           MYABORT("erreur dans la partie Python");
        }
        return 2 ;
}


/* ------------------------------------------------------------------ */
void convr8( _IN int nval, _IN PyObject *tup, _OUT double *val)
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
                val[i]=PyFloat_AsDouble(v);
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
                val[i]=PyInt_AsLong(v);
        }
        return ;
}


/* ------------------------------------------------------------------ */
void convertxt( _IN int nval, _IN PyObject *tup, _OUT char *val, _IN int taille)
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
void converltx( _IN int nval, _IN PyObject *tup, _OUT char *val, _IN int taille)
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
void STDCALL(GETRAN,getran)(_OUT double *rval)
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

        *rval=PyFloat_AsDouble(val);

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

        res=PyObject_CallMethod(commande,"iniran","i",*jump);
        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return ;
}


/* ------------------------------------------------------------------ */
void DEFSS(GETTCO,gettco,_IN char *nomobj, _IN int lnom, _OUT char *typobj, _IN int ltyp)
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
void DEFPS(GETMAT,getmat,_OUT INTEGER *nbarg,_OUT char *motcle,_IN int lcle)
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
        *nbarg = nval ;

        if ( nval > 0 ){
                converltx(nval,lnom,motcle,lcle); /* conversion  */
        }

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return ;
}


/* ------------------------------------------------------------------ */
void DEFSPPSSP(GETMJM,getmjm,_IN char *nomfac,_IN int lfac,_IN INTEGER *iocc,_IN INTEGER *nbval,
                            _OUT char *motcle,_IN int lcle,_OUT char *type,_IN int ltyp, _OUT INTEGER *nbarg)
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
        ioc=*iocc ;
        ioc=ioc-1 ;
                                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getmjm","sii",fstr2(nomfac,lfac),ioc,*nbval);
        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");
        /*  si non impression du retour */

        if(!PyArg_ParseTuple(res,"OO",&lnom,&lty)) MYABORT("erreur dans la partie Python");
        nval=PyList_Size(lnom);
        *nbarg = (nval > *nbval) ? -nval : nval ;
                                                                        ASSERT(((nval<=*nbval)&&(*nbarg==nval))||(*nbarg==-nval)) ;
        if(*nbarg < 0)nval=*nbval;

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
FORTRAN_LOGICAL DEFSS( GETEXM ,getexm, _IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle)
{
        /*
          Procedure GETEXM pour le FORTRAN : emule le fonctionnement
          de la procedure equivalente ASTER
          Entrees :
            le nom d un mot cle facteur : motfac (string)
            le nom d un mot cle simple ou sous mot cle : motcle (string)
          Retourne :
            0 si n existe pas 1 si existe

          ATTENTION : la valeur C 0 correspond a le valeur Fortran .FORTRAN_FALSE.
        */
        PyObject *res  = (PyObject*)0 ;
        FORTRAN_LOGICAL presence     = FORTRAN_FALSE;
                                                                        ASSERT(motcle!=(char*)0);
                                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getexm","ss",
                                fstr1(motfac,lfac),fstr2(motcle,lcle));
        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");
        presence=PyInt_AsLong(res) ? FORTRAN_TRUE : FORTRAN_FALSE ;
        /*  decrement sur le refcount du retour */
        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return presence;
}


/* ------------------------------------------------------------------ */
void DEFSSS( GETRES ,getres, _OUT char *nomres, _IN int lres, _OUT char *concep, _IN int lconc, _OUT char *nomcmd, _IN int lcmd)
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
void DEFSSPPPPP(GETVC8,getvc8,_IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT double *val,_OUT INTEGER *nbval)
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

        ioc=*iocc ;
        ioc=ioc-1 ;
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvc8","ssiii",mfc,mcs,ioc,*iarg,*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");
                                                        ASSERT(PyTuple_Check(res)) ;
        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if(!ok)MYABORT("erreur dans la partie Python");

        nval=*nbval;
        if(*nbval < 0)nval=*mxval;
        convc8(nval,tup,val);

        Py_DECREF(res);
        return ;
}


/* ------------------------------------------------------------------ */
void DEFSSPPPPP(GETVR8,getvr8,_IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT double *val,_OUT INTEGER *nbval)
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
        ioc=*iocc ;
        ioc=ioc-1 ;
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvr8","ssiii",mfc,mcs,ioc,*iarg,*mxval);
        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");
                                                       ASSERT(PyTuple_Check(res)) ;
        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if(!ok)MYABORT("erreur dans la partie Python");

        nval=*nbval;
        if(*nbval < 0)nval=*mxval;
        if ( nval>0 ){
                convr8(nval,tup,val);
        }

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return ;
}


/* ------------------------------------------------------------------ */
void DEFSSPSPPPP(UTPRIN,utprin, _IN char *typmess, _IN int ltype,
                                _IN char *idmess, _IN int lidmess,
                                _IN INTEGER *nbk, _IN char *valk, _IN int lvk,
                                _IN INTEGER *nbi, _IN INTEGER *vali,
                                _IN INTEGER *nbr, _IN double *valr)
{
   /*
      Interface Fortran/Python pour l'affichage des messages
   */
        PyObject *tup_valk,*tup_vali,*tup_valr,*res;
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

        res=PyObject_CallMethod(static_module,"MessageLog","s#s#OOO",typmess,ltype,idmess,lidmess,tup_valk,tup_vali,tup_valr);
        if (!res) {
           MYABORT("erreur lors de l'appel à MessageLog");
        }

        Py_DECREF(tup_valk);
        Py_DECREF(tup_vali);
        Py_DECREF(tup_valr);
        Py_DECREF(res);
}

/* ------------------------------------------------------------------ */
void DEFP(CHKMSG,chkmsg, _OUT INTEGER *iret)
{
   /*
      Interface Fortran/Python pour la vérification que tout s'est bien
      passé, destinée à etre appelée dans FIN.
   */
   PyObject *res, *mess_log;
   
   mess_log = PyObject_GetAttrString(static_module, "MessageLog");
   if (!mess_log) {
      MYABORT("erreur lors de l'accès à l'objet MessageLog.");
   }
   
   res = PyObject_CallMethod(mess_log, "check_counter", NULL);
   if (!res) {
      MYABORT("erreur lors de l'appel à la méthode MessageLog.check_counter");
   }
   *iret = (INTEGER)PyLong_AsLong(res);
   
   Py_DECREF(res);
   Py_DECREF(mess_log);
}

/* ------------------------------------------------------------------ */
void DEFSSP(CHEKSD,cheksd,_IN char *nomsd,_IN int lnom, _IN char *typsd,_IN int ltyp,
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
      MYABORT("erreur lors de l'appel à la méthode CHECKSD");
   }
   *iret = (INTEGER)PyLong_AsLong(res);
   
   Py_DECREF(res);
}

/* ------------------------------------------------------------------ */
void DEFSPSPP(FIINTF,fiintf,_IN char *nomfon,_IN int lfon,_IN INTEGER *nbpu,_IN char *param,_IN int lpara,_IN double *val,
                     _OUT double *resu)
{
        PyObject *res  = (PyObject*)0 ;
        PyObject *tup_par;
        PyObject *tup_val;
        char *kvar;
        int i;
                                                        ASSERT(commande!=(PyObject*)0);
        tup_par = PyTuple_New( *nbpu ) ;
        tup_val = PyTuple_New( *nbpu ) ;
        for(i=0;i<*nbpu;i++){
           kvar = param + i*lpara;
           PyTuple_SetItem( tup_par, i, PyString_FromStringAndSize(kvar,lpara) ) ;
        }
        for(i=0;i<*nbpu;i++){
           PyTuple_SetItem( tup_val, i, PyFloat_FromDouble(val[i]) ) ;
        }

        res=PyObject_CallMethod(commande,"fiintf","s#OO",nomfon,lfon,tup_par,tup_val);

        if (res == NULL)MYABORT("erreur dans la partie Python");
                                                       ASSERT(PyFloat_Check(res)) ;
        *resu=PyFloat_AsDouble(res);

        Py_DECREF(tup_par);
        Py_DECREF(tup_val);
        Py_DECREF(res);           /*  decrement sur le refcount du retour */
        return ;
}


/* ------------------------------------------------------------------ */
void DEFSSPPPPP(GETVIS,getvis,_IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT INTEGER *val,_OUT INTEGER *nbval )
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
        ioc=*iocc ;
        ioc=ioc-1 ;
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvis","ssiii",mfc,mcs,ioc,*iarg,*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if (!ok)MYABORT("erreur dans la partie Python");

        nval=*nbval;
        if(*nbval < 0)nval=*mxval;
        convert(nval,tup,val);

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return ;
}


/* ------------------------------------------------------------------ */
void DEFPS(GETVLI,getvli,_OUT INTEGER *unite , _OUT char *cas , _IN int lcas )
{
        /*
        Cette fonction est destinee a etre utilisee pour le fichier "*.code" (fort.15)
        */
                                                        ASSERT(NomCas!=(char*)0) ;
        *unite = 15 ;
        CSTRING_FCPY(cas,lcas,NomCas);
        return ;
}


/* ------------------------------------------------------------------ */
void DEFSSPPPPP(GETVLS,getvls,_IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT INTEGER *val,_OUT INTEGER *nbval )
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

        ioc=*iocc ;
        ioc=ioc-1 ;
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvls","ssiii",mfc,mcs,ioc,*iarg,*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if (!ok)MYABORT("erreur dans la partie Python");

        nval=*nbval;
        if(*nbval < 0)nval=*mxval;
        convert(nval,tup,val);

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        return ;
}


/* ------------------------------------------------------------------ */
void DEFSSPPPSP(GETVTX,getvtx,_IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT char *txval,_IN int ltx,_OUT INTEGER *nbval)
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
        ioc=*iocc ;
        ioc=ioc-1 ;
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvtx","ssiii",mfc,mcs,ioc,*iarg,*mxval);

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

        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if (!ok)MYABORT("erreur au decodage d'une chaine dans le module C aster.getvtx");

        nval=*nbval;
        if(*nbval < 0)nval=*mxval;

        if( nval > 0 ){
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
void DEFSSPPPSP(GETVID,getvid,_IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT char *txval,_IN int ltx,_OUT INTEGER *nbval)
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
        ioc=*iocc ;
        ioc=ioc-1 ;
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvid","ssiii",mfc,mcs,ioc,*iarg,*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if (!ok)MYABORT("erreur dans la partie Python");

        nval=*nbval;
        if(*nbval < 0)nval=*mxval;
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

        /*
           Normalement on doit utiliser l dans le format pour des entiers de type long (INTEGER==long)
        */
        res=PyObject_CallMethod(commande,"smcdel","ll",*iold,*inew);

        /*
            Si le retour est NULL : une exception a ete levee dans le code Python appele
            Cette exception est a transferer normalement a l appelant mais FORTRAN ???
            On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)MYABORT("erreur a l appel de smcdel dans la partie Python");
        *ierusr=*ierusr+PyInt_AsLong(res);
        Py_DECREF(res);
}


/* ------------------------------------------------------------------ */
long FindLength( _IN char *chaineFortran , _IN INTEGER longueur )
{
        /*
        Fonction  : FindLength
        Intention
                Retourne la taille exacte de la chaine de caracteres fortran
                chaineFortran contenant eventuellement des blancs de fin de ligne..
                La taille exacte est la longueur de la chaine du debut au
                dernier caractere non blanc.
        */

        long k = longueur-1 ;
        if ( ! chaineFortran ) return 0 ;

        while( k>=0 && chaineFortran[k]==' ' ) k-- ;
        return k+1 ;
}


/* ------------------------------------------------------------------ */
PyObject * MakeTupleString(long nbval,char *kval,int lkval,INTEGER *lval)
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
   int i, len;
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
      PyObject *t=PyTuple_New(nbval);
      for(i=0;i<nbval;i++){
         if (lval) {
            len = lval[i];
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
PyObject * MakeListString( long nbval,char *kval,int lkval )
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
   PyObject *l=PyList_New(nbval);
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
PyObject * MakeTupleInt(long nbval,long* kval)
{
   /*
            Entrees:
               nbval nombre d'entiers dans kval
               kval  tableau de nbval long FORTRAN
            Sorties:
               RETOUR fonction : tuple de int Python de longueur nbval
            Fonction:
               Convertir un tableau de long FORTRAN en un tuple de int Python de meme longueur
   */
   int i;
   if(nbval == 1){
      return PyInt_FromLong(*kval);
   }
   else{
      PyObject * t=PyTuple_New(nbval);
      for(i=0;i<nbval;i++){
         if(PyTuple_SetItem(t,i,PyInt_FromLong(kval[i]))) {
         Py_DECREF(t);
         return NULL;
         }
      }
      return t;
   }
}

/* ------------------------------------------------------------------ */
PyObject * MakeListInt(long nbval,long* kval)
{
   /*
            Entrees:
               nbval nombre d'entiers dans kval
               kval  tableau de nbval long FORTRAN
            Sorties:
               RETOUR fonction : liste de int Python de longueur nbval
            Fonction:
               Convertir un tableau de long FORTRAN en une liste de int Python de meme longueur
   */
   int i;
   PyObject *l=PyList_New(nbval);
   for(i=0;i<nbval;i++){
      if (PyList_SetItem(l,i,PyInt_FromLong(kval[i]))) {
         Py_DECREF(l);
         return NULL;
      }
   }
   return l;
}

/* ------------------------------------------------------------------ */
PyObject * MakeTupleFloat(long nbval,double * kval)
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
      return PyFloat_FromDouble(*kval);
   }
   else{
      PyObject * t=PyTuple_New(nbval);
      for(i=0;i<nbval;i++){
         if(PyTuple_SetItem(t,i,PyFloat_FromDouble(kval[i]))) {
            Py_DECREF(t);
            return NULL;
         }
      }
      return t;
   }
}

/* ------------------------------------------------------------------ */
PyObject * MakeListFloat(long nbval,double * kval)
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
   PyObject *l=PyTuple_New(nbval);
   for(i=0;i<nbval;i++){
      if(PyList_SetItem(l,i,PyFloat_FromDouble(kval[i]))) {
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

   res = PyObject_CallMethod(commande,"putvir","i",*ival);
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
void DEFSSP(GCUCON,gcucon, char *resul, int lresul, char *concep, int lconcep, INTEGER *ier)
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

   *ier = PyInt_AsLong(res);
   Py_DECREF(res);
}


/* ------------------------------------------------------------------ */
void DEFPSP(GCUCDT,gcucdt,INTEGER *icmd,char *resul,int lresul,INTEGER *ier)
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
        res = PyObject_CallMethod(commande,"gcucon","ls#s",*icmd,resul,lresul,"");
        /*
           Si le retour est NULL : une exception a ete levee dans le code Python appele
            Cette exception est a transferer normalement a l appelant mais FORTRAN ???
            On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
                MYABORT("erreur a l appel de gcucdt dans la partie Python");

        *ier = PyInt_AsLong(res);
        /*
                ier= -1 indique que le concept existe mais d'un autre type. On doit donc
                retourner 1
        */
        if(*ier==-1)*ier=1;
        Py_DECREF(res);
}


/* ------------------------------------------------------------------ */
void DEFSSPPP(GETTVC,gettvc,char * nom,int lnom,char *ctyp,int lctyp,INTEGER *ival,double *rval,INTEGER *ier)
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
        *ier=0;
        res = PyObject_CallMethod(commande,"gettvc","s#",nom,lnom);
        /*
                    Si le retour est NULL : une exception a ete levee dans le code Python appele
                    Cette exception est a transferer normalement a l appelant mais FORTRAN ???
                    On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
                MYABORT("erreur a l appel de gettvc dans la partie Python");

        ok=PyArg_ParseTuple(res, "lO",ier,&valeur);
        if(!ok)MYABORT("erreur dans gettvc_ ");
        if(PyInt_Check(valeur)){
          *ival=PyInt_AsLong(valeur);
          strncpy(ctyp,"IS  ",4);
        }
        else if(PyFloat_Check(valeur)){
          *rval=PyFloat_AsDouble(valeur);
          strncpy(ctyp,"R8  ",4);
        }
        else{
          *ier=0;
        }

        Py_DECREF(res); /* le compteur de references de valeur sera automatiquement decremente */
}


/* ------------------------------------------------------------------ */
void DEFPPP(GCECDU,gcecdu,INTEGER *ul,INTEGER *icmdu, INTEGER *numint)
{
        /*
          Entrees:
            ul      unite logique pour les ecritures
            icmdu   numero de la commande
          Sorties :
            numint  numero de l operateur de la commande
          Fonction:
             Ecriture d'un operateur ou d une commande utilisateur avec ses arguments (pas implemente)
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

        *numint = PyInt_AsLong(res);
        Py_DECREF(res);
}


/* ------------------------------------------------------------------ */
void gcncon2_(char *type,char *resul,int ltype,int lresul)
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
/* -------------------- appels aux routines JEVEUX ------------------ */
#define CALL_JEMARQ() STDCALL(JEMARQ, jemarq)()
void CALL_JEMARQ();

#define CALL_JEDEMA() STDCALL(JEDEMA, jedema)()
void CALL_JEDEMA();

#define CALL_JEDETR(nom) CALLS(JEDETR, jedetr, nom)
void DEFS(JEDETR, jedetr, char *, int);
/* ------------------------------------------------------------------ */

#define CALL_PRCOCH(nomce,nomcs,nomcmp,ktype,itopo,nval,groups) CALLSSSSPPS(PRCOCH,prcoch,nomce,nomcs,nomcmp,ktype,itopo,nval,groups)
void DEFSSSSPPS(PRCOCH,prcoch,char *,int,char *,int,char *,int,char *,int,INTEGER *,INTEGER *,char *,int);

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
        INTEGER nval=0;
        int long_nomcham=8;
        INTEGER itopo;
        void *malloc(size_t size);

        if (!PyArg_ParseTuple(args, "sssslO:prepcompcham",&nomce,&nomcs,&nomcmp,&ktype,&itopo,&list)) return NULL;

        nval=PyList_Size(list);
        if (nval > 0) {
          groups = (char *)malloc(nval*long_nomcham*sizeof(char));
          converltx(nval,list,groups,long_nomcham); /* conversion  */
        }
        /* on ne peut passer a fortran une chaine non allouee
           a cause du strlen() que l'on va faire dessus au moment du passage
           c -> fortran
        */
        else {
          groups = (char *)malloc(long_nomcham*sizeof(char));
          groups = strcpy(groups,"        ");
        }

        try(1){
          CALL_JEMARQ();
          CALL_PRCOCH(nomce,nomcs,nomcmp,ktype,&itopo,&nval,groups);
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
#define CALL_GETCON(nomsd,iob,ishf,ilng,ctype,lcon,iaddr,nomob) CALLSPPPPPPS(GETCON,getcon,nomsd,iob,ishf,ilng,ctype,lcon,iaddr,nomob)
void DEFSPPPPPPS(GETCON,getcon,char *,int,INTEGER *,INTEGER *,INTEGER *,INTEGER *,INTEGER *,char **,char *,int);

static char getvectjev_doc[]=
"getvectjev(nomsd)->valsd      \n\
\n\
Retourne la valeur du concept nomsd \n\
dans un tuple.";

static PyObject* aster_getvectjev(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        char *nomsd;
        char nomob[8];
        double *f;
        INTEGER *l;
        char *kvar;
        PyObject *tup;
        INTEGER lcon, iob;
        INTEGER ishf=0;
        INTEGER ilng=0;
        INTEGER ctype=0;
        int i;
        char *iaddr;

        if (!PyArg_ParseTuple(args, "s|ll:getvectjev",&nomsd,&ishf,&ilng)) return NULL;

        try(1){
          iob=0 ;
          CALL_JEMARQ();
          CALL_GETCON(nomsd,&iob,&ishf,&ilng,&ctype,&lcon,&iaddr,nomob);
          if(ctype < 0){
            /* Erreur */
/*            PyErr_SetString(PyExc_KeyError, "Concept inexistant");
            CALL_JEDEMA();
            return NULL;*/
            /* vecteur jeveux inexistant : retourne None */
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
            f = (double *)iaddr;
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               PyTuple_SetItem( tup, i, PyFloat_FromDouble(f[i]) ) ;
            }
          }
          else if(ctype == 2){
            /* ENTIER */
            l = (INTEGER*)iaddr;
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               PyTuple_SetItem( tup, i, PyInt_FromLong(l[i]) ) ;
            }
          }
          else if(ctype == 3){
            /* COMPLEXE */
            f = (double *)iaddr;
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               PyTuple_SetItem( tup, i, PyComplex_FromDoubles(f[2*i],f[2*i+1]) ) ;
            }
          }
          else if(ctype == 4){
            /* CHAINE K8 */
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               kvar = iaddr + i*8;
               PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar,8) ) ;
            }
          }
          else if(ctype == 5){
            /* CHAINE K16 */
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               kvar = iaddr + i*16;
               PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar,16) ) ;
            }
          }
          else if(ctype == 6){
            /* CHAINE K24 */
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               kvar = iaddr + i*24;
               PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar,24) ) ;
            }
          }
          else if(ctype == 7){
            /* CHAINE K32 */
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               kvar = iaddr + i*32;
               PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar,32) ) ;
            }
          }
          else if(ctype == 8){
            /* CHAINE K80 */
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               kvar = iaddr + i*80;
               PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar,80) ) ;
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

#define CALL_TAILSD(nom, nomsd, val, nbval) CALLSSPP(TAILSD,tailsd,nom, nomsd, val, nbval)
void DEFSSPP(TAILSD,tailsd,char *,int,char *,int,INTEGER *, INTEGER *);

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
        char *nomsd, *nom;
        char nomob[8];
        double *f;
        INTEGER *l;
        char *kvar;
        PyObject *tup, *dico, *key;
        INTEGER iob,j,ishf,ilng;
        INTEGER lcon;
        INTEGER ctype=0;
        INTEGER *val, nbval;
        int i;
        char *iaddr;
        void *malloc(size_t size);

        if (!PyArg_ParseTuple(args, "s:getcolljev",&nomsd)) return NULL;

/* Taille de la collection */
        nbval = 1;
        val = (INTEGER *)malloc((nbval)*sizeof(INTEGER));
        nom = (char *)malloc(24*sizeof(char));
        strcpy(nom, "LIST_COLLECTION");
        CALL_JEMARQ();
        CALL_TAILSD(nom, nomsd, val, &nbval);
        iob=val[0];

        dico = PyDict_New();
        try(1){
          for(j=1;j<iob+1;j++){
          ishf=0 ;
          ilng=0 ;
          CALL_GETCON(nomsd,&j,&ishf,&ilng,&ctype,&lcon,&iaddr,nomob);
          if(nomob[0] == ' '){
             key=PyInt_FromLong(j);
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
            f = (double *)iaddr;
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               PyTuple_SetItem( tup, i, PyFloat_FromDouble(f[i]) ) ;
            }
            PyDict_SetItem(dico,key,tup);
          }
          else if(ctype == 2){
            /* ENTIER */
            l = (INTEGER*)iaddr;
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               PyTuple_SetItem( tup, i, PyInt_FromLong(l[i]) ) ;
            }
            PyDict_SetItem(dico,key,tup);
          }
          else if(ctype == 3){
            /* COMPLEXE */
            f = (double *)iaddr;
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               PyTuple_SetItem( tup, i, PyComplex_FromDoubles(f[2*i],f[2*i+1]) ) ;
            }
            PyDict_SetItem(dico,key,tup);
          }
          else if(ctype == 4){
            /* CHAINE K8 */
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               kvar = iaddr + i*8;
               PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar,8) ) ;
            }
            PyDict_SetItem(dico,key,tup);
          }
          else if(ctype == 5){
            /* CHAINE K16 */
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               kvar = iaddr + i*16;
               PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar,16) ) ;
            }
            PyDict_SetItem(dico,key,tup);
          }
          else if(ctype == 6){
            /* CHAINE K24 */
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               kvar = iaddr + i*24;
               PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar,24) ) ;
            }
            PyDict_SetItem(dico,key,tup);
          }
          else if(ctype == 7){
            /* CHAINE K32 */
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               kvar = iaddr + i*32;
               PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar,32) ) ;
            }
            PyDict_SetItem(dico,key,tup);
          }
          else if(ctype == 8){
            /* CHAINE K80 */
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               kvar = iaddr + i*80;
               PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar,80) ) ;
            }
            PyDict_SetItem(dico,key,tup);
          }
         }
         CALL_JEDETR("&&GETCON.PTEUR_NOM");
         CALL_JEDEMA();
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



#define CALL_PUTCON(nomsd,nbind,ind,valr,valc,num,iret) CALLSPPPPPP(PUTCON,putcon,nomsd,nbind,ind,valr,valc,num,iret)
void DEFSPPPPPP(PUTCON,putcon,char *,int,INTEGER *,INTEGER *,double *,double *,INTEGER *,INTEGER *);

static char putvectjev_doc[]=
"putvectjev(nomsd)->valsd      \n\
\n\
Renvoie le contenu d'un objet python dans  \n\
un vecteur jeveux.";

/* ------------------------------------------------------------------ */
static PyObject* aster_putvectjev(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *tupi  = (PyObject*)0 ;
        PyObject *tupr  = (PyObject*)0 ;
        PyObject *tupc  = (PyObject*)0 ;
        char *nomsd;
        double *valr;
        double *valc;
        INTEGER *ind;
        INTEGER num;
        INTEGER nbind;
        unsigned int nind  = 0 ;
        int ok        = 0 ;
        INTEGER iret=0;
        void *malloc(size_t size);
	
        ok = PyArg_ParseTuple(args, "slOOOl",&nomsd,&nbind,&tupi,&tupr,&tupc,&num);
        if (!ok)MYABORT("erreur dans la partie Python");

        nind = (unsigned int)(nbind);

        ind = (INTEGER *)malloc((nind)*sizeof(INTEGER));
        valr = (double *)malloc((nind)*sizeof(double));
        valc = (double *)malloc((nind)*sizeof(double));

        if ( nind>0 ){
                 convert(nind,tupi,ind);
                 convr8(nind,tupr,valr);
                 convr8(nind,tupc,valc);
        }
        try(1){
          CALL_JEMARQ();
          CALL_PUTCON(nomsd,&nbind,ind,valr,valc,&num,&iret);
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
        char *nomsd;
        double *valr;
        double *valc;
        INTEGER *ind;
        INTEGER num;
        INTEGER nbind;
        unsigned int nind  = 0 ;
        int ok        = 0 ;
        INTEGER iret=0;
        void *malloc(size_t size);

        ok = PyArg_ParseTuple(args, "slOOOl",&nomsd,&nbind,&tupi,&tupr,&tupc,&num);
        if (!ok)MYABORT("erreur dans la partie Python");

        nind = (unsigned int)(nbind);

        ind = (INTEGER *)malloc((nind)*sizeof(INTEGER));
        valr = (double *)malloc((nind)*sizeof(double));
        valc = (double *)malloc((nind)*sizeof(double));

        if ( nind>0 ){
                 convert(nind,tupi,ind);
                 convr8(nind,tupr,valr);
                 convr8(nind,tupc,valc);
        }

        CALL_JEMARQ();
        CALL_PUTCON(nomsd,&nbind,ind,valr,valc,&num,&iret);
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
void DEFSPSPPPS(RSACCH,rsacch,char *, int, INTEGER *, char *,int,INTEGER *, INTEGER *, INTEGER *, char *,int);
void DEFSPSSPPP(RSACVA,rsacva,char *, int,INTEGER *, char *,int,char *,int,INTEGER *, double *,INTEGER *);
void DEFSPSSPPP(RSACPA,rsacpa,char *, int,INTEGER *, char *,int,char *,int,INTEGER *, double *,INTEGER *);

/* particulier car on passe les longueurs des chaines en dur */
#ifdef _POSIX
#define CALL_RSACCH(nomsd, numch, nomch, nbord, liord, nbcmp, liscmp) \
                  F_FUNC(RSACCH,rsacch)(nomsd,numch,nomch,nbord,liord,nbcmp,liscmp, strlen(nomsd),16,8)
#else
#define CALL_RSACCH(nomsd, numch, nomch, nbord, liord, nbcmp, liscmp) \
                  F_FUNC(RSACCH,rsacch)(nomsd,strlen(nomsd),numch, nomch,16,nbord, liord, nbcmp, liscmp,8)
#endif

#define CALL_RSACVA(nomsd, numva, nomva, ctype, ival, rval, ier) \
                  CALLSPSSPPP(RSACVA,rsacva,nomsd, numva, nomva, ctype, ival, rval, ier)
#define CALL_RSACPA(nomsd, numva, nomva, ctype, ival, rval, ier) \
                  CALLSPSSPPP(RSACPA,rsacpa,nomsd, numva, nomva, ctype, ival, rval, ier)

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
   double *rval;
   char *nomsd, *mode, *liscmp, *nom ;
   char nomch[16], ctype, nomva[16];
   int i, lo;
   PyObject *dico, *liste, *key;
   void *malloc(size_t size);

   if (!PyArg_ParseTuple(args, "ss",&nomsd, &mode)) return NULL;

/* Identifiant de la SD resultat */
   nbval = 1;
   val = (INTEGER *)malloc((nbval)*sizeof(INTEGER));
   nom = (char *)malloc(24*sizeof(char));
   strcpy(nom, "LIST_RESULTAT");

/* Taille de la SD resultat : nbr champs, nbr paras, nbr numeros d'ordre */
   CALL_JEMARQ();
   CALL_TAILSD(nom, nomsd, val, &nbval);
   nbchmx = val[0];
   nbpamx = val[1];
   nbord  = val[2];

   if (strcmp(mode,"CHAMPS") == 0 || strcmp(mode,"COMPOSANTES") == 0) {
/* Construction du dictionnaire : cle d'acces = nom du champ */
     liord  = (INTEGER *)malloc(nbord*sizeof(INTEGER));
     liscmp = (char *)malloc(500*8*sizeof(char));
     dico = PyDict_New();
     for (numch=1; numch<=nbchmx; numch++) {
       CALL_RSACCH(nomsd, &numch, nomch, &nbord, liord, &nbcmp, liscmp);
       lo = 16;
       while (nomch[lo-1] == ' ')  lo--;
       key = PyString_FromStringAndSize(nomch,lo);
       liste = PyList_New(0);
       if (strcmp(mode,"CHAMPS") == 0) {
              for (i=0; i<nbord; i++)
                PyList_Append(liste,PyInt_FromLong(liord[i]));
       }

       if (strcmp(mode,"COMPOSANTES") == 0) {
         for (i=0; i<nbcmp; i++) {
            nom = &(liscmp[i*8]);
            lo = 8; while (nom[lo-1] == ' ')  lo--;
            PyList_Append(liste,PyString_FromStringAndSize(nom,lo));
         }
       }

     PyDict_SetItem(dico,key,liste);
     }

   free(liord);
   }


   else if (strcmp(mode,"VARI_ACCES") == 0 ) {
/* Extraction des variables d'acces */
          ival  = (INTEGER *)malloc(nbord*sizeof(INTEGER));
          rval  = (double * )malloc(nbord*sizeof(double) );

          dico = PyDict_New();
          for (numva=0; numva<=nbpamx; numva++)
            {
            CALL_RSACVA(nomsd, &numva, nomva, &ctype, ival, rval, &ier);
            if (ier != 0) continue;

            lo = 16;
            while (nomva[lo-1] == ' ') lo--;
            key = PyString_FromStringAndSize(nomva,lo);

            liste = PyList_New(0);
            if (ctype == 'I')
              for (i=0; i<nbord; i++)
                PyList_Append(liste,PyInt_FromLong(ival[i]));
            else
              for (i=0; i<nbord; i++)
                PyList_Append(liste,PyFloat_FromDouble(rval[i]));

            PyDict_SetItem(dico,key,liste);
            };

          free(ival);
          free(rval);
   }
   else if (strcmp(mode,"PARAMETRES") == 0 ) {
/* Extraction des parametres */
          ival  = (INTEGER *)malloc(nbord*sizeof(INTEGER));
          rval  = (double * )malloc(nbord*sizeof(double) );

          dico = PyDict_New();
          for (numva=0; numva<=nbpamx; numva++) {
            CALL_RSACPA(nomsd, &numva, nomva, &ctype, ival, rval, &ier);
            if (ier != 0) continue;

            lo = 16;
            while (nomva[lo-1] == ' ') lo--;
            key = PyString_FromStringAndSize(nomva,lo);

            liste = PyList_New(0);
            if (ctype == 'I')
              for (i=0; i<nbord; i++)
                PyList_Append(liste,PyInt_FromLong(ival[i]));
            else
              for (i=0; i<nbord; i++)
                PyList_Append(liste,PyFloat_FromDouble(rval[i]));

            PyDict_SetItem(dico,key,liste);
          }

      free(ival);
      free(rval);
   }

   CALL_JEDEMA();
   return dico;
}


/* ------------------------------------------------------------------ */
#define CALL_EXPASS(a,b,c,d)  F_FUNC(EXPASS,expass)(a,b,c,d)
extern void STDCALL(EXPASS,expass)(INTEGER* , INTEGER* , INTEGER* , INTEGER*);

static PyObject* aster_oper(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp;
        INTEGER jxvrf=1 ; /* FORTRAN_TRUE */
        INTEGER iertot=0 ;
        INTEGER icmd=0 ;
        INTEGER ipass=0 ;

        if (!PyArg_ParseTuple(args, "Olll",&temp,&jxvrf,&ipass,&icmd)) return NULL;
        /* On empile le nouvel appel */
        commande=empile(temp);

        if(PyErr_Occurred()){
            fprintf(stderr,"Warning: une exception n'a pas ete traitée\n");
            PyErr_Print();
            fprintf(stderr,"Warning: on l'annule pour continuer mais elle aurait\n\
                            etre traitée avant\n");
            PyErr_Clear();
        }

        fflush(stderr) ;
        fflush(stdout) ;

        try(1){
                /*  appel du sous programme expass pour verif ou exec */
                CALL_EXPASS (&jxvrf,&ipass,&icmd,&iertot);
                /* On depile l appel */
                commande = depile();
                return PyInt_FromLong(iertot); /*  retour de la fonction oper sous la forme d un entier */
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
#define CALL_OPSEXE(a,b,c,d,e)  CALLPPPSP(OPSEXE,opsexe,a,b,c,d,e)
extern void DEFPPPSP(OPSEXE,opsexe,INTEGER* , INTEGER* , INTEGER* , char *,int ,INTEGER* ) ;

static PyObject* aster_opsexe(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp;
        INTEGER ier=0 ;
        INTEGER icmd=0 ;
        INTEGER oper=0 ;
        INTEGER ipass=0 ;
        char *cmdusr="                                                                          ";

        if (!PyArg_ParseTuple(args, "Olll",&temp,&icmd,&ipass,&oper)) return NULL;

        /* On empile le nouvel appel */
        commande=empile(temp);

        if(PyErr_Occurred()){
            fprintf(stderr,"Warning: une exception n'a pas ete traitée\n");
            PyErr_Print();
            fprintf(stderr,"Warning: on l'annule pour continuer mais elle aurait\n\
                            etre traitée avant\n");
            PyErr_Clear();
        }
        fflush(stderr) ;
        fflush(stdout) ;

        try(1){
                /*  appel du sous programme opsexe */
                CALL_OPSEXE (&icmd,&ipass,&oper,cmdusr,&ier);
                /* On depile l appel */
                commande = depile();
                return PyInt_FromLong(ier); /*  retour de la fonction oper sous la forme d un entier */
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
extern void STDCALL(IMPERS,impers)();
#define CALL_IMPERS() F_FUNC(IMPERS,impers)()

static PyObject * aster_impers(self,args)
PyObject *self; /* Not used */
{
   CALL_IMPERS ();
   Py_INCREF( Py_None ) ;
   return Py_None;
}

/* ------------------------------------------------------------------ */
void DEFSS(AFFICH,affich,char *,int,char *,int);
#define CALL_AFFICH(a,b) CALLSS(AFFICH,affich,a,b)

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
INTEGER DEFS(JDCGET,jdcget,char *attr, int l_attr)
{
/*
   Permet de récupérer la valeur d'un attribut du jdc.
*/
   PyObject *jdc, *val;
   INTEGER value;
   
   jdc = PyObject_GetAttrString(commande, "jdc");
   if (jdc == NULL)
      MYABORT("Impossible de récupérer l'attribut 'jdc' !");

   val = PyObject_CallMethod(jdc, "get_jdc_attr", "s#", attr, l_attr);
   if (val == NULL){
      printf("attribut inexistant dans le jdc : '%s'\n\n", attr);
      MYABORT("erreur dans JDCGET");
   }
   
   if (!PyInt_Check(val))
      MYABORT("Seuls les attributs de type entier peuvent etre récupérés !");
   
   value = PyInt_AsLong(val);
   
   Py_XDECREF(jdc);
   Py_XDECREF(val);
   
   return value;
}

/* ------------------------------------------------------------------ */
void DEFSP(JDCSET,jdcset,char *attr, int l_attr, INTEGER *value)
{
/*
   Permet de positionner la valeur d'un attribut du jdc à `value`.
*/
   PyObject *jdc, *res;
   
   jdc = PyObject_GetAttrString(commande, "jdc");
   if (jdc == NULL)
      MYABORT("Impossible de récupérer l'attribut 'jdc' !");
   
   res = PyObject_CallMethod(jdc, "set_jdc_attr", "s#l", attr, l_attr, *value);
   if (res == NULL)
      MYABORT("erreur dans JDCSET");
   
   Py_XDECREF(jdc);
   Py_XDECREF(res);
}

/* ------------------------------------------------------------------ */
void DEFSSP(ONERRF,onerrf,char *,int, _OUT char *,int, _OUT INTEGER *);
#define CALL_ONERRF(a,b,c) CALLSSP(ONERRF,onerrf,a,b,c)

static PyObject * aster_onFatalError(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
/*
   Cette méthode définie le comportement lors des erreurs Fatales :

   aster.onFatalError('ABORT')
         => on s'arrête avec un JEFINI('ERREUR') dans UTFINM

   aster.onFatalError('EXCEPTION')
         => on lève l'exception FatalError

   aster.onFatalError()
         => retourne la valeur actuelle : 'ABORT' ou 'EXCEPTION'.
*/
      int len=-1;
      INTEGER lng;
      char tmp[16+1];
      char *comport;
      PyObject *res;

      if (!PyArg_ParseTuple(args, "|s#:onFatalError",&comport ,&len)) return NULL;
      if (len == -1) {
            CALL_ONERRF(" ", tmp, &lng);
            res = PyString_FromStringAndSize(tmp, (int)lng);
            return res;

      } else if (strcmp(comport,"ABORT")==0 || strcmp(comport, "EXCEPTION")==0) {
            CALL_ONERRF(comport, tmp, &lng);
            Py_INCREF( Py_None ) ;
            return Py_None;

      } else {
            MYABORT("Seules ABORT et EXCEPTION sont des valeurs valides.");
      }
}

/* ------------------------------------------------------------------ */
void DEFPSSSS(ULOPEN,ulopen,INTEGER *,char *,int,char *,int,char *,int,char *,int);
#define CALL_ULOPEN(a,b,c,d,e) CALLPSSSS(ULOPEN,ulopen,a,b,c,d,e)

static PyObject * aster_ulopen(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        char *fichie;
        char *name;
        char *acces;
        char *autor;
        INTEGER unit=0 ;

        if (!PyArg_ParseTuple(args, "ssssl:ulopen",&fichie,&name,&acces,&autor,&unit)) return NULL;
        CALL_ULOPEN (&unit,fichie,name,acces,autor);

        Py_INCREF( Py_None ) ;
        return Py_None;
}


/* ------------------------------------------------------------------ */
void DEFP(FCLOSE,fclose,INTEGER *);
#define CALL_FCLOSE(a) CALLP(FCLOSE,fclose,a)

static PyObject * aster_fclose(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        INTEGER unit=0 ;

        if (!PyArg_ParseTuple(args, "l:fclose",&unit)) return NULL;
        CALL_FCLOSE (&unit);

        Py_INCREF( Py_None ) ;
        return Py_None;
}


/* ------------------------------------------------------------------ */
void DEFPPS(REPOUT,repout,INTEGER *,INTEGER *,char *,int);
#define CALL_REPOUT(a,b,c) CALLPPS(REPOUT,repout,a,b,c)


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
        temp= PyString_FromStringAndSize(nom,FindLength(nom,lnom));
        return temp;
}

/* ------------------------------------------------------------------ */
void DEFPPS(REPDEX,repdex,INTEGER *,INTEGER *,char *,int);
#define CALL_REPDEX(a,b,c) CALLPPS(REPDEX,repdex,a,b,c)


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
        temp= PyString_FromStringAndSize(nom,FindLength(nom,lnom));
        return temp;
}

/* ------------------------------------------------------------------ */
void DEFSS(GCNCON,gcncon,char *,int,char *,int);
#define CALL_GCNCON(a,b) CALLSS(GCNCON,gcncon,a,b)


static PyObject * aster_gcncon(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
   PyObject *res;
   char *type, result[9];

   if (!PyArg_ParseTuple(args, "s", &type)) return NULL;
   BLANK(result, 8);
   result[8] = '\0';
   if (jeveux_status == 1) {
      CALL_GCNCON (type, result);
   }
   res= PyString_FromStringAndSize(result,FindLength(result,8));
   return res;
}

/* ---------------------------------------------------------------------- */
void DEFSSPSPPSPSS(RCVALE, rcvale, char *,int, char *,int, INTEGER *, char *,int, double *, INTEGER *, char *,int, double *, char *, int, char *, int);
#define CALL_RCVALE(a,b,c,d,e,f,g,h,i,j) CALLSSPSPPSPSS(RCVALE,rcvale,a,b,c,d,e,f,g,h,i,j)

static char rcvale_doc[] =
"Interface d'appel à la routine fortran RCVALE.\n"
"   Arguments : nommat, phenomene, nompar, valpar, nomres, stop\n"
"   Retourne  : valres, codret (tuples)\n"
" Aucune vérification n'est faite sur les arguments d'entrée (c'est l'appelant,\n"
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
   INTEGER nbpar, nbres;
   char *nompar, *nomres, *codret;
   double *valpar, *valres;
   int long_nompar = 8;       /* doivent impérativement correspondre aux  */
   int long_nomres = 8;       /* longueurs des chaines de caractères      */
   int long_codret = 2;       /* déclarées dans la routine fortran RCVALE */
   void *malloc(size_t size);

   if (!PyArg_ParseTuple(args, "ssOOOs", &nommat, &phenom, \
                  &t_nompar, &t_valpar, &t_nomres, &stop)) return NULL;

   /* Conversion en tableaux de chaines et réels */
   nbpar = PyTuple_Size(t_nompar);
   nompar = (char *)malloc(long_nompar*nbpar*sizeof(char));
   convertxt(nbpar, t_nompar, nompar, long_nompar);

   valpar = (double *)malloc(nbpar*sizeof(double));
   convr8(nbpar, t_valpar, valpar);

   nbres = PyTuple_Size(t_nomres);
   nomres = (char *)malloc(long_nomres*nbres*sizeof(char));
   convertxt(nbres, t_nomres, nomres, long_nomres);

   /* allocation des variables de sortie */
   valres = (double *)malloc(nbres*sizeof(double));
   codret = (char *)malloc(long_codret*nbres*sizeof(char));

   CALL_RCVALE(nommat, phenom, &nbpar, nompar, valpar, &nbres, nomres, valres, codret, stop);

   /* création des tuples de sortie */
   t_valres = MakeTupleFloat(nbres, valres);
   t_codret = MakeTupleString(nbres, codret, long_codret, NULL);

   /* retour de la fonction */
   t_res = PyTuple_New(2);
   PyTuple_SetItem(t_res, 0, t_valres);
   PyTuple_SetItem(t_res, 1, t_codret);

   free(nompar);
   free(valpar);
   free(valres);
   free(codret);

   return t_res;
}

/* ---------------------------------------------------------------------- */
void DEFP(MATFPE, matfpe, INTEGER *);
#define CALL_MATFPE(a) CALLP(MATFPE,matfpe,a)

static char matfpe_doc[] =
"Interface d'appel à la routine C matfpe.\n"
"   usage: matfpe(actif)\n"
"     matfpe(-1) : on désactive l'interception des erreurs numériques,\n"
"     matfpe(1)  : on active l'interception des erreurs numériques.\n";

static PyObject * aster_matfpe(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
   long actif;
   
   if (!PyArg_ParseTuple(args, "l:matfpe", &actif)) return NULL;
   
   if (actif >= -1  && actif <= 1) {
      CALL_MATFPE(&actif);
   } else {
      MYABORT("Valeur incorrecte : seuls -1 et 1 sont valides.");
   }
   Py_INCREF( Py_None ) ;
   return Py_None;
}

/* ---------------------------------------------------------------------- */
void DEFSPSP(MDNOMA,mdnoma,char *,int,INTEGER *,char *,int,INTEGER *);
#define CALL_MDNOMA(a,b,c,d) CALLSPSP(MDNOMA,mdnoma,a,b,c,d)


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

        temp= PyString_FromStringAndSize(nomamd,FindLength(nomamd,lnomam));
        return temp;
}

/* ------------------------------------------------------------------ */
void DEFSPPSSSP(MDNOCH,mdnoch,char *,int,INTEGER *,INTEGER *,char *,int,char *,int,char *,int,INTEGER *);
#define CALL_MDNOCH(a,b,c,d,e,f,g) CALLSPPSSSP(MDNOCH,mdnoch,a,b,c,d,e,f,g)

static PyObject * aster_mdnoch(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
      PyObject *temp = (PyObject*)0 ;
      INTEGER lnochm=0;
      INTEGER lresu ;
      char *noresu;
      char *nomsym;
      char *nopase;
      INTEGER codret=0;
      int lon1,lon2,lon3;
      char nochmd[33],n1[33],n2[17],n3[9];

      if (!PyArg_ParseTuple(args, "ls#s#s#",&lresu,&noresu,&lon1,&nomsym,&lon2,&nopase,&lon3)) return NULL;
      BLANK(nochmd,32);
      nochmd[32]='\0';
      BLANK(n1,32);strncpy(n1,noresu,lon1);
      BLANK(n2,16);strncpy(n2,nomsym,lon2);
      BLANK(n3,8); strncpy(n3,nopase,lon3);
      CALL_MDNOCH (nochmd,&lnochm,&lresu,n1,n2,n3,&codret);
      temp= PyString_FromStringAndSize(nochmd,FindLength(nochmd,lnochm));
      return temp;
}


/* ------------------------------------------------------------------ */
void TraitementFinAster( _IN int val )
{
        switch( val ){
        case CodeFinAster :
                PyErr_SetString(PyExc_EOFError, "exit ASTER");
                break ;
        case CodeAbortAster :
                PyErr_SetString(FatalError, exception_reason);
                break ;
        case CodeErrorAster :
                PyErr_SetString(AsterError, exception_reason);
                break ;

        /* exceptions particularisées */
        case CodeNonConvergenceAster :
                PyErr_SetString(NonConvergenceError, exception_reason);
                break ;
        case CodeEchecComportementAster :
                PyErr_SetString(EchecComportementError, exception_reason);
                break ;
        case CodeBandeFrequenceVideAster :
                PyErr_SetString(BandeFrequenceVideError, exception_reason);
                break ;
        case CodeMatriceSinguliereAster :
                PyErr_SetString(MatriceSinguliereError, exception_reason);
                break ;
        case CodeTraitementContactAster :
                PyErr_SetString(TraitementContactError, exception_reason);
                break ;
        case CodeMatriceContactSinguliereAster :
                PyErr_SetString(MatriceContactSinguliereError, exception_reason);
                break ;
        case CodeArretCPUAster :
                PyErr_SetString(ArretCPUError, exception_reason);
                break ;

        default :
                INTERRUPTION(1) ;
                break ;
        }
        return ;
}

/* ------------------------------------------------------------------ */
#define CALL_GETLTX(a,b,c,d,e,f,g) CALLSSPPPPP(GETLTX,getltx,a,b,c,d,e,f,g)
#define CALL_GETVTX(a,b,c,d,e,f,g) CALLSSPPPSP(GETVTX,getvtx,a,b,c,d,e,f,g)

int RecupNomCas(void)
{
   /* recuperation du nom du cas */
   INTEGER un          = 1 ;
   INTEGER *iocc       = (INTEGER*)&un ;
   INTEGER *iarg       = (INTEGER*)&un ;
   INTEGER *mxval      = (INTEGER*)&un ;
   INTEGER nbval       = 1 ;
   int ltx       = 8 ;
   INTEGER longueur[1] ;
   void *malloc(size_t size);
                                                   ASSERT(commande!=(PyObject*)0);
   CALL_GETLTX ( "CODE","NOM",iocc,iarg,mxval, longueur ,&nbval) ;
   if(nbval == 0){
      /* Le mot cle NOM n'a pas ete fourni on donne un nom
         par defaut au nom du cas */
      NomCas = strdup("??????");
   }
   else if(nbval > 0){
                                                   ASSERT(longueur[0]>0);
      NomCas = (char*)(malloc((longueur[0]+1)*sizeof(char))) ;
      BLANK(NomCas,longueur[0]); /* initialisation a blanc */
      NomCas[longueur[0]]='\0';
                                                   ASSERT(NomCas!=(char*)0);
      ltx = longueur[0];
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
void DEFPPPP(POURSU,poursu,INTEGER* , INTEGER* , INTEGER* ,INTEGER*) ;
void DEFS(GCCPTS,gccpts,char *, int );

#define CALL_POURSU(a,b,c,d) CALLPPPP(POURSU,poursu,a,b,c,d)
#define CALL_GCCPTS(a,la) F_FUNC(GCCPTS,gccpts)(a,la)

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
        INTEGER ipass=0;
        INTEGER lot=1 ; /* FORTRAN_TRUE */
        INTEGER ier=0 ;
        INTEGER lonuti=0 ;
        static int nbPassages=0 ;
                                                                ASSERT((nbPassages==1)||(commande==(PyObject*)0));
        nbPassages++ ;
        if (!PyArg_ParseTuple(args, "Ol",&temp,&ipass)) return NULL;

        /* On empile le nouvel appel */
        commande=empile(temp);

        if(PyErr_Occurred()){
            fprintf(stderr,"Warning: une exception n'a pas ete traitée\n");
            PyErr_Print();
            fprintf(stderr,"Warning: on l'annule pour continuer mais elle aurait\n\
                            etre traitée avant\n");
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

                CALL_POURSU (&lot,&ipass,&ier,&lonuti);


                /* recuperation de la liste des concepts dans une     */
                /* string python                                      */

                concepts=PyString_FromStringAndSize(NULL,lonuti*80);
                CALL_GCCPTS (PyString_AsString(concepts),80);
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
          return Py_BuildValue("(iiiN)",lot ,ier,lonuti,concepts );
        }
}

/* ------------------------------------------------------------------ */
#define CALL_DEBUT(a,b,c)  F_FUNC(DEBUT,debut)(a,b,c)
extern void STDCALL(DEBUT,debut)(INTEGER* , INTEGER* , INTEGER* );

static PyObject * aster_debut(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp = (PyObject*)0 ;
        INTEGER ipass=0;
        INTEGER lot=1 ; /* FORTRAN_TRUE */
        INTEGER ier=0 ;
        static int nbPassages=0 ;
                                                                ASSERT((nbPassages==1)||(commande==(PyObject*)0));
        nbPassages++ ;
        if (!PyArg_ParseTuple(args, "Ol",&temp,&ipass)) return NULL;

        /* On empile le nouvel appel */
        commande=empile(temp);

        if(PyErr_Occurred()){
            fprintf(stderr,"Warning: une exception n'a pas ete traitée\n");
            PyErr_Print();
            fprintf(stderr,"Warning: on l'annule pour continuer mais elle aurait\n\
                            etre traitée avant\n");
            PyErr_Clear();
        }

        fflush(stderr) ;
        fflush(stdout) ;

        try(1){
                /*  appel de la commande debut */
                CALL_DEBUT (&lot,&ipass,&ier);
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
          return Py_BuildValue("(ii)",lot ,ier );
        }
}

/* ------------------------------------------------------------------ */
#define CALL_IBMAIN(a,b,c)  F_FUNC(IBMAIN,ibmain)(a,b,c)
extern void STDCALL(IBMAIN,ibmain)(INTEGER* , INTEGER* , INTEGER* );

static PyObject *aster_init(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
   INTEGER lot=1 ; /* FORTRAN_TRUE */
   INTEGER ier=0 ;
   INTEGER dbg=0 ; /* FORTRAN_FALSE */

   if (!PyArg_ParseTuple(args, "l",&dbg)) return NULL;

   /* initialisation de la variable `static_module` */
   static_module = PyImport_ImportModule("Execution/E_Global");
   if (! static_module) {
      MYABORT("Impossible d'importer le module E_Global !");
   }

   fflush(stderr) ;
   fflush(stdout) ;

   CALL_IBMAIN (&lot,&ier,&dbg);

   /* jeveux est parti ! */
   jeveux_status = 1;

return PyInt_FromLong(ier);
}


/* ------------------------------------------------------------------ */
#define CALL_JELST3(a,b,c,d)  CALLSSPP(JELST3,jelst3,a,b,c,d)
DEFSSPP(JELST3,jelst3, char* base, int base_len, char*dest, int dest_len,
	INTEGER* nmax, INTEGER* total );


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
	tmpsize = 24*total*sizeof(char);
	tmp = (char*)malloc( tmpsize+1 );
	memset( tmp, ' ', tmpsize );
	tmp[tmpsize] = 0;
	nmax = total;
	/* second appel après allocation mémoire */
	CALL_JELST3( base, tmp, &nmax, &total );
	
	the_list = PyList_New(total);
	for( i=0, ptr=tmp; i<total;++i, ptr+=24 ) {
		pystr = PyString_FromStringAndSize( ptr, 24 );
		PyList_SetItem( the_list, i, pystr );
	}
	free( tmp );
	return the_list;
}

#define CALL_JELIRA(a,b,c,d)  CALLSSPS(JELIRA,jelira,a,b,c,d)
DEFSSPS(JELIRA,jelira, char* base, int base_len, char*dest, int dest_len,
                       INTEGER* nmax, char* res, int res_len );


/* ------------------------------------------------------------------ */
static PyObject *jeveux_getattr( PyObject* self, PyObject* args)
{
	char *nomobj, *attr;
	char charval[33];
	INTEGER intval = 0;
	
	memset( charval, ' ', 32 );
	charval[32]=0;
	if (!PyArg_ParseTuple(args, "ss",&nomobj,&attr))
		return NULL;
	CALL_JELIRA( nomobj, attr, &intval, charval );
	
	return Py_BuildValue( "is", intval, charval );
}

#define CALL_JEEXIN(a,b)  CALLSP(JEEXIN,jeexin,a,b)
DEFSP(JEEXIN,jeexin, char* nomlu, int nomlu_len, INTEGER* iret );


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

/* ------------------------------------------------------------------ */
/*             Routines d'interface pour la sensibilité               */

void DEFSSSP(PSGENC,psgenc, _IN  char *nosimp, int lnosimp,
                            _IN  char *nopase, int lnopase,
                            _OUT char *nocomp, int lnocomp,
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
   PyObject *jdc, *memo_sensi, *val;
   char *sret = (char*)0;
   int ier, longueur;
   *iret = 0;
   
   jdc = PyObject_GetAttrString(commande, "jdc");
   if (jdc == NULL)
      MYABORT("Impossible de récupérer l'attribut 'jdc' !");

   memo_sensi = PyObject_GetAttrString(jdc, "memo_sensi");
   if (memo_sensi == NULL)
      MYABORT("Impossible de récupérer l'attribut 'memo_sensi' du jdc !");

   val = PyObject_CallMethod(memo_sensi, "get_nocomp", "s#s#", nosimp, lnosimp, nopase, lnopase);
   if (val == NULL){
      MYABORT("erreur lors de l'appel à memo_sensi.get_nocomp !");
   }
   
   sret = PyString_AsString(val);
   longueur = strlen(sret);
   STRING_FCPY(nocomp, lnocomp, sret, longueur);
   if ( strncmp(nocomp, "????????", 8) == 0 ) {
      *iret = 1;
   }
   
   Py_XDECREF(jdc);
   Py_XDECREF(memo_sensi);
   Py_XDECREF(val);
}

/* ------------------------------------------------------------------ */
void DEFSSPSSS(PSGEMC,psgemc, _IN  char *nosimp, int lnosimp,
                              _IN  char *nopase, int lnopase,
                              _OUT INTEGER *nbmocl,
                              _OUT char *limocl, int l1,
                              _OUT char *livale, int l2,
                              _OUT char *limofa, int l3)
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
   PyObject *jdc, *memo_sensi;
   PyObject *tup3, *tmocl, *tvale, *tmofa;
   int ichk;
   
   jdc = PyObject_GetAttrString(commande, "jdc");
   if (jdc == NULL)
      MYABORT("Impossible de récupérer l'attribut 'jdc' !");

   memo_sensi = PyObject_GetAttrString(jdc, "memo_sensi");
   if (memo_sensi == NULL)
      MYABORT("Impossible de récupérer l'attribut 'memo_sensi' du jdc !");

   tup3 = PyObject_CallMethod(memo_sensi, "get_mcle", "s#s#", nosimp, lnosimp, nopase, lnopase);
   if (tup3 == NULL) {
      MYABORT("erreur lors de l'appel à memo_sensi.get_mcle !");
   }
   tmocl = PyTuple_GetItem(tup3, 0);
   tvale = PyTuple_GetItem(tup3, 1);
   tmofa = PyTuple_GetItem(tup3, 2);
   
   if (*nbmocl == 0) {
      /* Seul nbmocl est modifié (1er appel) */
      *nbmocl = PyTuple_Size(tmocl);
   } else {
      /* On remplit les vecteurs limocl, livale, limofa (2ème appel) */
      if (*nbmocl != PyTuple_Size(tmocl)) {
         MYABORT("erreur dans psgemc : nbmocl n'a pas la bonne valeur !");
      }
      convertxt(*nbmocl, tmocl, limocl, l1);
      convertxt(*nbmocl, tvale, livale, l2);
      convertxt(*nbmocl, tmofa, limofa, l3);
   }
   
   Py_XDECREF(jdc);
   Py_XDECREF(memo_sensi);
   Py_XDECREF(tup3);
}

/* ------------------------------------------------------------------ */
void DEFSPSP(PSINFO,psinfo, _IN  char *nomsd, int lnomsd,
                            _OUT INTEGER *nbstse,
                            _OUT char *nostnc, int lnostnc,
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
   PyObject *jdc, *memo_sensi, *tup2;
   PyObject *indic, *result;
   int size_result;
   
   jdc = PyObject_GetAttrString(commande, "jdc");
   if (jdc == NULL)
      MYABORT("Impossible de récupérer l'attribut 'jdc' !");

   memo_sensi = PyObject_GetAttrString(jdc, "memo_sensi");
   if (memo_sensi == NULL)
      MYABORT("Impossible de récupérer l'attribut 'memo_sensi' du jdc !");

   tup2 = PyObject_CallMethod(memo_sensi, "psinfo", "s#", nomsd, lnomsd);
   indic = PyTuple_GetItem(tup2, 0);
   result = PyTuple_GetItem(tup2, 1);
   size_result = PyTuple_Size(result);
   
   *iderive = PyInt_AsLong(indic);
   if (*nbstse == 0) {
      /* Seul nbstse est modifié (1er appel) */
      *nbstse = size_result / 2;
   } else {
      /* On remplit le vecteur nostnc (2ème appel) */
      if (*nbstse != size_result / 2) {
         MYABORT("erreur dans psinfo : nbstse n'a pas la bonne valeur !");
      }
      convertxt(size_result, result, nostnc, lnostnc);
   }
   
   Py_XDECREF(jdc);
   Py_XDECREF(memo_sensi);
   Py_XDECREF(tup2);
}

/* ----------------------   FIN Sensibilité   ----------------------- */
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

        argv = (char**)malloc(1+argc*sizeof(char*)) ;
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
                {"matfpe",       aster_matfpe,       METH_VARARGS, matfpe_doc},
                {"argv",         aster_argv,         METH_VARARGS},
                {"prepcompcham", aster_prepcompcham, METH_VARARGS},
                {"getvectjev",   aster_getvectjev,   METH_VARARGS, getvectjev_doc},
                {"putvectjev",   aster_putvectjev,   METH_VARARGS, putvectjev_doc},
                {"putcolljev",   aster_putcolljev,   METH_VARARGS, putcolljev_doc},
                {"getcolljev",   aster_getcolljev,   METH_VARARGS, getcolljev_doc},
                {"GetResu",      aster_GetResu,      METH_VARARGS},
                {"jeveux_getobjects", jeveux_getobjects, METH_VARARGS},
                {"jeveux_getattr", jeveux_getattr,   METH_VARARGS},
                {"jeveux_exists", jeveux_exists,     METH_VARARGS},
                {"get_nom_concept_unique", aster_gcncon, METH_VARARGS},
                {NULL,                NULL}/* sentinel */
};


/* ------------------------------------------------------------------ */
void DEFPPPSP(VERSIO,versio,INTEGER *,INTEGER *,INTEGER *,char *,int,INTEGER *);
#define CALL_VERSIO(a,b,c,d,e) CALLPPPSP(VERSIO,versio,a,b,c,d,e)

void initvers(PyObject *dict)
{
    PyObject *v;
    INTEGER vers,util,nivo;
    INTEGER exploi;
    char date[20];
    char rev[8];

    CALL_VERSIO(&vers,&util,&nivo,date,&exploi);
    sprintf(rev,"%ld.%ld.%ld",vers,util,nivo);
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
        m = Py_InitModule3("aster", aster_methods,aster_module_documentation);

        /* Add some symbolic constants to the module */
        d = PyModule_GetDict(m);

        initvers(d);
        initExceptions(d);

        /* Initialisation de la pile d appel des commandes */
        pile_commandes = PyList_New(0);
}



/* ------------------------------------------------------------------ */
void AfficheChaineFortran( _IN char *chaine , _IN int longueur )
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

int EstPret( _IN char *chaine , _IN int longueur )
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
        if ( taille >= 0 ){
                pret = 1 ;
                if( isalpha(chaine[0]) ){
                        for( k=0 ; pret==1 && k<longueur ; k++ ){
                                pret = ( EstValide(chaine[k] ) ) ? 1 : 0 ;
                                if ( pret != 1 ){
                                        fprintf( stderr , "CARACTERE %d INVALIDE '%c' %d\n" , k , chaine[k] , (int)chaine[k]) ;
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
void AjoutChaineA( _INOUT char **base , _IN char *supplement )
{
        /*
        Procedure  : AjoutChaineA
        Intention
                la chaine de caractere "base" est agrandie de la chaine
                "supplement". La zone memoire occupee par base est reallouee
                et la valeur du pointeur *base est donc modifiee.
                Dans cette operation tous les caracteres sont significatifs
                sauf le caractere NUL ('\0').

        PRENEZ GARDE ! : base doit etre une zone allouee DYNAMIQUEMENT

        */

        char *resultat = (char*)0 ;
        int ajout      = 0 ;
        int taille     = 0 ;
        int total      = 0 ;
        void *malloc(size_t size);
	
        taille = ( *base ) ? strlen( *base ) : 0 ;

        ajout = ( supplement ) ? strlen( supplement ) : 0 ;
        total = taille + ajout + 1 /* caractere de fin de chaine */;
        if ( ajout > 0 ){
                if ( taille > 0 ){
                        resultat = (char*)(malloc(total)) ;
                        ASSERT(resultat!=NULL) ;
                        strcpy(resultat,*base) ;
                        strcat(resultat,supplement) ;
                }
                else{
                        resultat = (char*)(malloc(total)) ;
                        ASSERT(resultat!=NULL) ;
                        strcpy(resultat,supplement) ;
                }
        }
        else{
                if ( taille > 0 ){
                        resultat = (char*)(malloc(total)) ;
                        strcpy(resultat,*base) ;
                }
        }
        if( *base ){
                ASSERT(strlen(*base)==taille) /* verification INVARIANT !! */
                free(*base) ;
                *base=(char*)0 ;
        }
        *base = resultat ;
}


/* ------------------------------------------------------------------ */
const char *aster_ident()
{
        const char *identCVS = "$Id: astermodule.c,v 1.59.12.1.2.1 2001/05/16 16:14:54 iliade Exp $ $Name:  $" ;
        return identCVS ;
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

        *icmc = PyInt_AsLong(res);
        Py_DECREF(res);
}
