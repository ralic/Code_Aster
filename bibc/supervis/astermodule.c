/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF astermodule supervis  DATE 09/10/2002   AUTEUR DURAND C.DURAND */
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
/* RESPONSABLE                                 D6BHHJP J.P.LEFEBVRE   */
/* ------------------------------------------------------------------ */

#if defined HPUX
#define R8PI   r8pi
        extern double R8PI();
#elif defined PPRO_NT
        extern double __stdcall R8PI();
#elif defined SOLARIS || IRIX || TRU64 || SOLARIS64 || P_LINUX
#define R8PI    r8pi_
        extern double R8PI();
#endif


#ifndef min
#define min(A,B)  ((A) < (B) ? (A) : (B))
#endif

#define VARIABLE_LEN 16

#include <stdio.h>
#include "Python.h"
#include <math.h>
#include <ctype.h>



#ifndef UTILITES_H        /*{*/
#define UTILITES_H

/* FORTRAN_TRUE = -1 sur HP-UX avec l'option de compilation +DAportable +apollo */
enum ENUM_LOGICAL { FORTRAN_TRUE=-1, FORTRAN_FALSE=0} ;
#define FORTRAN_LOGICAL enum ENUM_LOGICAL

/* pour indiquer le  statut des arguments des fonctions. */

#ifdef _IN
#error  _IN est deja definie
#endif
#define _IN

#ifdef _OUT
#error  _OUT est deja definie
#endif
#define _OUT

#ifdef _INOUT
#error  _INOUT est deja definie
#endif
#define _INOUT

#ifdef _UNUSED
#error  _UNUSED est deja definie
#endif
#define _UNUSED




/* pour representer les entiers sur toutes les stations. */

#if defined(SOLARIS) || defined(P_LINUX) || defined(PPRO_NT) || defined(HPUX)
#define INTEGER long
#else
#if defined IRIX || TRU64 || SOLARIS64
#define INTEGER long
#else
#error Environnement INDEFINI pour INTEGER
#endif
#endif




/* pour preciser quel fichier affiche les  messages et les valeurs */

#define INTERRUPTION(code) { ICI ; fprintf(stderr,"INTERRUPTION - code retour %d\n",code) ;abort() ; }
#ifdef _DEBUT
#error _DEBUT est deja definie
#endif
#ifdef _FIN
#error _FIN est deja definie
#endif

#ifdef _DEBOG_        /*{*/

#define ICI fflush(stdout);fprintf( stderr, "%s  %d : " , __FILE__ , __LINE__  ) ; fflush(stderr) ;
#define MESSAGE(chaine) ICI ; fprintf( stderr , "%s\n" , chaine ) ; fflush(stderr) ;

#ifndef ASSERT        /*{*/
#define ASSERT(condition) if( !(condition) ){ ICI ; fprintf(stderr,"condition %s VIOLEE\n",#condition);INTERRUPTION(17);}
#endif                /*}# ifndef ASSERT*/

#define TAB fflush(stdout);fprintf( stderr, "\t" );ICI
#define RES fflush(stdout);fprintf( stderr, "\t RESULTAT >> " );ICI
#define ISCRUTE(entier) TAB ; fprintf(stderr,"%s = %ld\n",#entier,(INTEGER)entier) ; fflush(stderr);
#define REFSCRUTE(objet) ISCRUTE(objet->ob_refcnt) ;
#define DSCRUTE(reel) TAB ; fprintf(stderr,"%s = %f\n",#reel,reel) ; fflush(stderr);
#define SSCRUTE(chaine) TAB ; fprintf(stderr,"%s = ",#chaine) ; if (chaine){fprintf(stderr,"\"%s\"\n",chaine);}else{fprintf(stderr,"(char*)0\n");} ; fflush(stderr);
#define FSSCRUTE(chaine,longueur) TAB ; fprintf(stderr,"%s = ",#chaine) ; fflush(stderr) ; AfficheChaineFortran(chaine,longueur) ;
#define _DEBUT(nom) fprintf( stderr , "\n\n\n") ; ICI ; fprintf( stderr , "{ DEBUT %s\n" , #nom ) ; fflush(stderr) ;
#define _FIN(nom) ICI ; fprintf( stderr , "} FIN %s\n\n\n" , #nom ) ; fflush(stderr) ;

#else                /*}# ifdef _DEBOG_{*/

#define ICI
#define TAB
#define RES
#define MESSAGE(chaine)
#define ISCRUTE(entier)
#define DSCRUTE(reel)
#define SSCRUTE(chaine)
#define REFSCRUTE(objet)
#define FSSCRUTE(chaine,longueur)
#define ASSERT(condition)
#define _DEBUT(nom)
#define _FIN(nom)

#endif                /*}# ifdef _DEBOG_*/
#endif                /*}# ifndef _UTILITES_*/


/* fin du fichier UTILITE.h */






#define EstValide(c) (isprint((int)c) && (isalnum((int)c) || (c=='_') || (c==' ')))




/* --- declarations des interfaces des fonctions de ce fichier --- */
/*{*/

static PyObject *aster_argv( _UNUSED  PyObject *self, _IN PyObject *args ) ;
const char *aster_ident() ;

int EstPret( _IN char *chaine , _IN int longueur ) ;
long FindLength( _IN char *chaineFortran , _IN INTEGER longueur ) ;
void AfficheChaineFortran( _IN char *chaine , _IN int longueur ) ;
void TraiteMessageErreur( _IN char* ) ;
void (*myabort)( char* ) = TraiteMessageErreur ;
void PRE_myabort( _IN const char *nomFichier , _IN const int numeroLigne , _IN const char *message ) ;
#define MYABORT(message) PRE_myabort( __FILE__ , __LINE__ , message )

PyObject * MakeTupleString(long nbval,char *kval,int lkval,INTEGER *lval) ;
PyObject * MakeTupleString_pour_putvid(long nbval,char *kval,int lkval) ;


char * fstring2c( _IN char *s, _IN int l) ;
char * fstr1( _IN char *s, _IN int l) ;
char * fstr2( _IN char *s, _IN int l) ;
char * fstr3( _IN char *s, _IN int l) ;
void convert( _IN int nval, _IN PyObject *tup, _OUT INTEGER *val) ;
void convertxt( _IN int nval, _IN PyObject *tup, _OUT char *val, _IN int taille) ;
void converltx( _IN int nval, _IN PyObject *tup, _OUT char *val, _IN int taille) ;

void AjoutChaineA( _INOUT char **base , _IN char *supplement ) ;

void TraitementFinAster( _IN int val ) ;



/*}*/
/* --- FIN declarations des interfaces des fonctions de ce fichier --- */





#define _FORT15_        /* La definition de cette macro conditionne la production d'un fichier fort.15 */

#ifdef _FORT15_        /*{*/

char *Pour_fort15            = (char*)0 ;      /* voir ConstruitFort15() et Fort15() */
const int Taille_fort15      = 256 ;
int ConstruitFort15( _IN char *motcle , _IN char *motfac ) ;
void Fort15( void ) ;

#endif                        /*}*/ /* # ifdef _FORT15_ */






#define _UTILISATION_SETJMP_
#ifdef _UTILISATION_SETJMP_
#include <setjmp.h>

#define try(val) exception_flag=val;if((exception_status = setjmp(env)) == 0)
#define catch(val) else if (exception_status == val)
#define throw(val) longjmp(env,val)
#define finally else

int exception_status=-1;
int exception_flag=0;

jmp_buf env ;                        /* utilise par longjmp, le type jmp_buf est defini dans setjmp.h */
const int CodeFinAster=19 ;
const int CodeAbortAster=20 ;


#endif                /* #ifdef _UTILISATION_SETJMP_ */





/* --- liste des variables globales au fonctions  de ce fichier --- */ /*{*/



/* commande (la commande courante) est definie par les fonctions aster_debut et aster_oper */
static PyObject *commande       = (PyObject*)0 ;
static PyObject *pile_commandes = (PyObject*)0 ;

/* NomCas est initialise dans aster_debut() */
/* NomCas est initialise a blanc pour permettre la recuperation de la
   trace des commandes lors de l'appel a debut ou poursuite. On ne connait
   pas encore NomCas qui sera initialise lors de l'appel a RecupNomCas */
static char *NomCas          = "        " ;

static PyObject *ErrorObject = (PyObject*)0 ;
static char * blan="                                                                                                                                                                                    ";



static char nom_fac[256];        /* utilise par fstr1 */
static char nom_cle[256];        /* utilise par fstr2 */
static char nom_cmd[256];        /* utilise par fstr3 */


/*}*/ /* --- FIN liste des variables globales au fonctions  de ce fichier --- */







void TraiteMessageErreur( _IN char * message )
{
        printf("%s\n",message);
        if(PyErr_Occurred())PyErr_Print();
        if(exception_flag==1){
          throw(CodeAbortAster);
        }
        else{
          abort();
        }
}





void PRE_myabort( _IN const char *nomFichier , _IN const int numeroLigne , _IN const char *message )
{

        /*
        Procedure : PRE_myabort
        Intention
                Cette procedure prepare la chaine de caracteres affichee par myabort()
                en ajoutant devant cette chaine, le nom du fichier source et le numero
                de la ligne a partir desquels PRE_myabort a ete appelee.
                Puis elle appelle elle-meme myabort().
                Voir aussi la macro MYABORT qui permet de generer automaitquement le nom
                du fichier et le numero de la ligne.
        */

        char *chaine = (char*)0 ;
        int longueur = 0 ;

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
        myabort( chaine ) ;

        free( chaine )   ;
        chaine=(char*)0 ;
        longueur = 0     ;
}





#if defined SOLARIS || IRIX || TRU64 || SOLARIS64 || P_LINUX
#define GETLTX getltx_
void getltx_( _IN char *motfac, _IN char *motcle, _IN INTEGER *iocc, _IN INTEGER *iarg, _IN INTEGER *mxval, _OUT INTEGER *isval, _OUT INTEGER *nbval, _IN int lfac, _IN int lcle )
#elif defined HPUX
#define GETLTX getltx
void getltx ( _IN char *motfac, _IN char *motcle, _IN INTEGER *iocc, _IN INTEGER *iarg, _IN INTEGER *mxval, _OUT INTEGER *isval, _OUT INTEGER *nbval, _IN int lfac, _IN int lcle )
#elif defined PPRO_NT
void __stdcall GETLTX ( _IN char *motfac, _IN int lfac, _IN char *motcle, _IN int lcle , _IN INTEGER *iocc, _IN INTEGER *iarg, _IN INTEGER *mxval, _OUT INTEGER *isval, _OUT INTEGER *nbval )
#endif
{
        /*
        Procedure : getltx_ (appelee par le fortran sous le nom GETLTX)
        Intention

        */
        PyObject *res = (PyObject*)0 ;
        PyObject *tup = (PyObject*)0 ;
        char *mfc     = (char*)0 ;
        char *mcs     = (char*)0 ;
        int ok        = 0 ;
        int nval      = 0 ;

        _DEBUT("getltx_") ;
        FSSCRUTE(motfac,lfac) ; FSSCRUTE(motcle,lcle) ; ISCRUTE(*iocc) ;

        mfc=fstr1(motfac,lfac);
                                                        ASSERT(mfc!=(char*)0);

                                                        ASSERT(EstPret(motcle,lcle)!=0);
        mcs=fstr2(motcle,lcle);
                                                        ASSERT(mcs!=(char*)0);

                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getltx","ssiii",mfc,mcs,*iocc,*iarg,*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");


        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if (!ok)MYABORT("erreur dans la partie Python");

        nval=*nbval;
        if(*nbval < 0)nval=*mxval;
        convert(nval,tup,isval);

#ifdef _DEBOG_
        if(nval > 0){
                ICI ; printf("\tGETLTX : *nbval %ld\n",*nbval);
                ICI ; printf("\tGETLTX : isval %ld\n",isval[0]);
        }
#endif

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getltx_) ;
        return ;
}




char * fstring2c( _IN char *s, _IN int l)
{
        char *fs;
                                                        ASSERT(EstPret(s,l)!=0);
        fs=(char *)malloc(l+1);
        if(fs == NULL){
                MYABORT("impossible d allouer de la memoire");
        }
        strncpy(fs, s, l );
        fs[l]='\0';
        return fs;
}







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










#if defined HPUX
void getfac ( _IN char *nomfac, _OUT INTEGER *occu, _IN int lfac)
#elif defined PPRO_NT
void __stdcall GETFAC ( _IN char *nomfac, _IN int lfac, _OUT INTEGER *occu)
#else
void getfac_( _IN char *nomfac, _OUT INTEGER *occu, _IN int lfac)
#endif
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

        _DEBUT(getfac_) ;

                                                        ASSERT(EstPret(nomfac,lfac)!=0);
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getfac","s",fstr1(nomfac,lfac));

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        *occu=PyInt_AsLong(res);

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getfac_) ;
        return ;
}




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
                        ISCRUTE(i) ;
                        v=PyTuple_GetItem(tup,i);
                        k += conv_un_c8( v , val+k ) ;
                }
        }
        return ;
}
int conv_un_c8( _IN PyObject *tup, _OUT double *val)
{

        /* Enrichissement des complexes stockes dans val a partir du tuple tup */

        char *repres = (char*)0 ; /* representation "RI" (reelle/imaginaire) ou "MP" (module phase) */


        double x = 0.0 ;
        double y = 0.0 ;
        double *rho = &x ;
        double *theta = &y ;

        ASSERT(PyTuple_Check(tup)) ;

        if(!PyArg_ParseTuple(tup,"sdd",&repres,&x,&y)) MYABORT("erreur dans la partie Python");
        SSCRUTE(repres) ;
        ASSERT((strcmp(repres,"RI")==0)||(strcmp(repres,"MP")==0)) ;
        DSCRUTE(x) ;
        DSCRUTE(y) ;
        ISCRUTE(strcmp(repres,"RI"))


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

        return 2 ;
}




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





void convertxt( _IN int nval, _IN PyObject *tup, _OUT char *val, _IN int taille)
{
        /*
        Convertit un Tuple en tableau de chaines
        Pour retour au Fortran : le tableau existe deja (val)
        */
        ASSERT(PyTuple_Check(tup)) ;
        if(nval != 0){
                PyObject *v  = (PyObject*)0 ;
                int i;
                char *s      = (char*)0 ;
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

                        strncpy(&val[i*taille],blan,taille);        /* initialisation a blanc */
                                                        ASSERT(PyString_Size(v)==strlen(s));
                        longueur=min(taille,PyString_Size(v)) ; /* pour tronquer */
                        ASSERT(longueur<=taille) ;                /* troncature interdite */

                        strncpy(&val[i*taille],s,longueur);        /* copie des caracteres */
                }
        }
}




void converltx( _IN int nval, _IN PyObject *tup, _OUT char *val, _IN int taille)
{
        /*
        Convertit une Liste  en tableau de chaines
        Pour retour au Fortran : le tableau existe deja (val)
        */

        PyObject *v = (PyObject*)0 ;
        int i;
        char *s = (char*)0 ;
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

                        strncpy(&val[i*taille],blan,taille);        /* initialisation a blanc */
                                                        ASSERT(PyString_Size(v)==strlen(s));
                        longueur=min(taille,PyString_Size(v)) ;
                        strncpy(&val[i*taille],s,longueur);        /* copie des caracteres */
                }
        }
        return ;
}




#if defined HPUX
    void getran (_OUT double *rval)
#elif defined PPRO_NT
    void __stdcall GETRAN (_OUT double *rval)
#else
    void getran_(_OUT double *rval)
#endif
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
        int nval=0;
        int nbval=0;

        _DEBUT(getran) ;

        res=PyObject_CallMethod(commande,"getran","");

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        ok = PyArg_ParseTuple(res,"O",&val);
        if(!ok)MYABORT("erreur dans la partie Python");

        printf("val : ");
        PyObject_Print(val, stdout, 0);
        printf("\n ");
        *rval=PyFloat_AsDouble(val);

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getran) ;
        return ;
}





#if defined HPUX
    void iniran ()
#elif defined PPRO_NT
    void __stdcall INIRAN ()
#else
    void iniran_()
#endif
{
        /*
          Procedure INIRAN pour le FORTRAN : recupere un réel aleatoire (loi uniforme 0-1) du module python Random
        */

        PyObject *res  = (PyObject*)0 ;

        _DEBUT(iniran) ;
        res=PyObject_CallMethod(commande,"iniran","");
        _FIN(iniran) ;
        return ;
}





#if defined HPUX
void gettco ( _IN char *nomobj, _OUT char *typobj, _IN int lnom, _IN int ltyp)
#elif defined PPRO_NT
void __stdcall GETTCO ( _IN char *nomobj, _IN int lnom, _OUT char *typobj, _IN int ltyp)
#else
void gettco_( _IN char *nomobj, _OUT char *typobj, _IN int lnom, _IN int ltyp)
#endif
{
        /*
        Procedure gettco_
          remplace le sous-programme fortran  GETTCO

         BUT :
          retrouver le type "superviseur" du concept nomobj.

        cf. cas : hpla100a
        */

        char *mcs      = (char*)0 ;
        PyObject *res  = (PyObject*)0 ;
        PyObject *tup  = (PyObject*)0 ;
        char *nomType  = (char*)0 ;
        int longueur   = 0 ;
        int ok         = 0 ;
        int nval       = 1 ;
        int k          = 0 ;

        _DEBUT("gettco_") ;
        ASSERT(lnom>0) ;
        FSSCRUTE(nomobj,lnom);
        mcs=fstr2(nomobj,lnom);

        /*
        recherche dans le jeu de commandes python du nom du type de
         du concept Aster de nom nomobj
        */
                                                        ASSERT(commande!=(PyObject*)0);
        SSCRUTE(mcs) ;
        res=PyObject_CallMethod(commande,"gettco","s",mcs);
        if (res == (PyObject*)0)MYABORT("erreur dans la partie Python (gettco)");

                                                         ASSERT( PyString_Check(res) )
        nomType=PyString_AsString(res);
                                                        ASSERT(nomType!=(char*)0) ;
        longueur = strlen(nomType) ;
                                                        ASSERT(longueur>0) ;
                                                        ASSERT(longueur<=ltyp) ;
        strncpy( typobj , nomType , longueur ) ;
                                                        ASSERT(EstPret(typobj,longueur)) ;
        for (k=longueur ; k<ltyp ; k++ ) typobj[k]=' ' ; /* complete typobj[k] par des blancs pour le fortran */

                                                        ASSERT(EstPret(typobj,ltyp)) ;

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN("gettco_") ;
        return ;
}





#if defined HPUX
void getmnb (_OUT INTEGER *nbmfac,_OUT INTEGER *nbobm)
#elif defined PPRO_NT
void __stdcall GETMNB (_OUT INTEGER *nbmfac,_OUT INTEGER *nbobm)
#else
void getmnb_(_OUT INTEGER *nbmfac,_OUT INTEGER *nbobm)
#endif
{
        /*
          Procedure GETMNB : emule la procedure equivalente ASTER
           Retourne des informations generales sur le catalogue de la commande courante
          Retourne :
           nbmfac : nombre de mots cles facteur
           nbobm  : nombre de descripteurs
        */

        long nbmc         = 0 ;
        PyObject *res    = (PyObject*)0 ;

        _DEBUT(getmnb_) ;


                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getmnb","");
        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        if(!PyArg_ParseTuple(res,"lll",nbmfac,&nbmc,nbobm)) MYABORT("erreur dans la partie Python");


        ISCRUTE(*nbmfac) ;
        ISCRUTE(*nbobm) ;
        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getmnb_) ;
        return ;
}





#if defined HPUX
void getmfa (_IN char *nomcmd,_IN INTEGER *irc,_OUT char *nomrc,_OUT INTEGER *nbmocl,_IN int lcmd,_IN int lrc)
#elif defined PPRO_NT
void __stdcall GETMFA (_IN char *nomcmd,_IN int lcmd,_IN INTEGER *irc,_OUT char *nomrc,_IN int lrc,_OUT INTEGER *nbmocl)
#else
void getmfa_(_IN char *nomcmd,_IN INTEGER *irc,_OUT char *nomrc,_OUT INTEGER *nbmocl,_IN int lcmd,_IN int lrc)
#endif
{
        /*
          Procedure GETMFA : emule la procedure equivalente ASTER
           Retourne le ieme mot cle facteur du catalogue de la commande nomcmd
          Entrees :
           nomcmd : nom de la commande
           irc    : numero du mot cle demande
          Retourne :
           nomrc  : nom du mot cle facteur
           nbmocl : nombre total de mots cles
           nbarg  : nombre total des arguments
        */
        long nbarg=0 ;  /* ancien argument devenu obsolete (J. PELLET) */

        PyObject *res  = (PyObject*)0 ;
        char *ss1;



        _DEBUT(getmfa_) ;
        FSSCRUTE(nomcmd,lcmd) ;
        ISCRUTE(*irc) ;



                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getmfa","si",fstr1(nomcmd,lcmd),*irc);
        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        strncpy(nomrc,blan,lrc);
        if(!PyArg_ParseTuple(res,"sll",&ss1 ,nbmocl,&nbarg)) MYABORT("erreur dans la partie Python");
                                                        ASSERT(ss1!=(char*)0) ;
        strncpy(nomrc,ss1,(size_t)min(strlen(ss1),lrc));


        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        FSSCRUTE(nomrc,lrc) ;
        ISCRUTE(*nbmocl) ;
        _FIN(getmfa_) ;
        return ;
}




#if defined HPUX
void getmfm (_IN char *nomfac,_IN INTEGER *nbval,_OUT char *motcle,_OUT char *type,_OUT INTEGER *nbarg,
             _IN int lfac,_IN int lcle,_IN int ltyp)
#elif defined PPRO_NT
void __stdcall GETMFM (_IN char *nomfac,_IN int lfac,_IN INTEGER *nbval,_OUT char *motcle,
                       _IN int lcle,_OUT char *type,_IN int ltyp,_OUT INTEGER *nbarg)
#else
void getmfm_(_IN char *nomfac,_IN INTEGER *nbval,_OUT char *motcle,_OUT char *type,_OUT INTEGER *nbarg,
             _IN int lfac,_IN int lcle,_IN int ltyp)
#endif
{

        /*
          Procedure GETMFM : emule la procedure equivalente ASTER
           Retourne les nbval premiers mots cles du mot cle facteur nomfac du catalogue de la commande en cours
          Entrees :
           nomfac : nom du mot cle facteur
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


        _DEBUT(getmfm_) ;
        ISCRUTE(*nbval);
        FSSCRUTE(nomfac,lfac) ; ISCRUTE(ltyp);
                                                                        ASSERT(ltyp>0);
        for ( k=0 ;k<ltyp ; k++ ) type[k]=' ' ;
                                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getmfm","si",fstr2(nomfac,lfac),*nbval);
        ISCRUTE(*nbval);
        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");
        /*  si non impression du retour */


        if(!PyArg_ParseTuple(res,"OO",&lnom,&lty)) MYABORT("erreur dans la partie Python");
        nval=PyList_Size(lnom);

        ISCRUTE(nval) ; ISCRUTE(*nbval) ;

        *nbarg = (nval > *nbval) ? -nval : nval ;
        ISCRUTE(*nbarg) ;
        ASSERT(((nval<=*nbval)&&(*nbarg==nval))||(*nbarg==-nval)) ;

        if(*nbarg < 0)nval=*nbval;
        ISCRUTE(nval) ;

        if ( nval > 0 ){
                converltx(nval,lnom,motcle,lcle); /* conversion  */
                converltx(nval,lty,type,ltyp);
        }


        /*
        A.Y.
        A la demande des developpeurs (J. Pellet), le nom des concepts retourne par
        la methode EXECUTION.getmfm (par exemple grma) est ici remplace par
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
        _FIN("getmfm_") ;
        return ;
}




#if defined HPUX
FORTRAN_LOGICAL getexm (_IN char *motfac,_IN char *motcle,_IN int lfac,_IN int lcle)
#elif defined PPRO_NT
FORTRAN_LOGICAL __stdcall GETEXM (_IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle)
#else
FORTRAN_LOGICAL getexm_(_IN char *motfac,_IN char *motcle,_IN int lfac,_IN int lcle)
#endif
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

        _DEBUT(getexm_) ;
        FSSCRUTE(motfac,lfac) ; FSSCRUTE(motcle,lcle) ;

                                                                        ASSERT(motcle!=(char*)0);
                                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getexm","ss",
            fstr1(motfac,lfac),fstr2(motcle,lcle));
        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");
#ifdef _DEBOG_
        /*  si non impression du retour */
            PyObject_Print(res, stdout, 0);
            printf("\n");
#endif
        presence=PyInt_AsLong(res) ? FORTRAN_TRUE : FORTRAN_FALSE ;
        /*  decrement sur le refcount du retour */

#ifdef _DEBOG_
        RES ; printf("\tGETEXM : presence %d\n",presence); FSSCRUTE(motcle,lcle) ;
#endif


        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getexm_) ;
        return presence;
}




#if defined SOLARIS || IRIX || TRU64 || SOLARIS64 || P_LINUX
#define GETRES getres_
void getres_( _OUT char *nomres, _OUT char *concep, _OUT char *nomcmd, _IN int lres, _IN int lconc, _IN int lcmd)
#elif defined HPUX
#define GETRES getres
void getres ( _OUT char *nomres, _OUT char *concep, _OUT char *nomcmd, _IN int lres, _IN int lconc, _IN int lcmd)
#elif defined PPRO_NT
void __stdcall GETRES ( _OUT char *nomres, _IN int lres, _OUT char *concep, _IN int lconc, _OUT char *nomcmd, _IN int lcmd)
#endif
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
        int k=0 ;

        _DEBUT(getres_) ;
        ISCRUTE(lres) ; ISCRUTE(lconc) ; ISCRUTE(lcmd) ;

        nomcmd[lcmd-1] =  ( nomcmd[lcmd-1]==' ' ) ? '\0' : nomcmd[lcmd-1] ;

        if(commande == (PyObject*)0){
          /* Aucune commande n'est active on retourne des chaines blanches */
          strncpy(nomres,blan,lres);
          strncpy(concep,blan,lconc);
          strncpy(nomcmd,blan,lcmd);
          return ;
        }
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getres","");
        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        ok = PyArg_ParseTuple(res,"s#s#s#",&ss1,&s1,&ss2,&s2,&ss3,&s3);
        if (!ok)MYABORT("erreur dans la partie Python");


        /* le fortran attend des chaines de caracteres completees par des blancs */

        s1=min(s1,lres) ;
        ISCRUTE(s1) ; SSCRUTE(ss1) ;
        strncpy(nomres,blan,lres);
        if( s1 ) strncpy(nomres,ss1,s1);
        /*for( k=s1 ; k<lres ; k++) nomres[k]=' ' ;*/
        FSSCRUTE(nomres,lres) ;


        ISCRUTE(lconc) ; ISCRUTE(s2) ;
        s2=min(s2,lconc) ;
        ISCRUTE(lconc) ; ISCRUTE(s2) ; SSCRUTE(ss2) ;
        strncpy(concep,blan,lconc);
        if( s2 ) strncpy(concep,ss2,s2);
        /*for( k=s2 ; k<lconc ; k++) concep[k]=' ' ;*/
        FSSCRUTE(concep,lconc) ;


        s3=min(s3,lcmd) ;
        ISCRUTE(s3) ; SSCRUTE(ss3) ;
        strncpy(nomcmd,blan,lcmd);
        if( s3 ) strncpy(nomcmd,ss3,s3);
        /*for( k=s3 ; k<lcmd ; k++) nomcmd[k]=' ' ;*/
        FSSCRUTE(nomcmd,lcmd) ;


        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getres_) ;
        return ;
}





#if defined HPUX
void getvc8 ( _IN char *motfac,_IN char *motcle,_IN INTEGER *iocc,_IN INTEGER *iarg,_IN INTEGER *mxval,
              _INOUT double *val,_OUT INTEGER *nbval,
              _IN int lfac,_IN int lcle
            )
#elif defined PPRO_NT
void __stdcall GETVC8 ( _IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle,_IN INTEGER *iocc,
                        _IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT double *val,_OUT INTEGER *nbval
            )
#else
void getvc8_( _IN char *motfac,_IN char *motcle,_IN INTEGER *iocc,_IN INTEGER *iarg,_IN INTEGER *mxval,
              _INOUT double *val,_OUT INTEGER *nbval,
              _IN int lfac,_IN int lcle
            )
#endif
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
        char *mfc      = (char*)0 ;
        char *mcs      = (char*)0 ;

        _DEBUT(getvc8_)
        SSCRUTE(PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
        FSSCRUTE(motfac,lfac) ; FSSCRUTE(motcle,lcle) ; ISCRUTE(*iocc) ;
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
                printf( "<F> GETVC8 : le numero d'occurence (IOCC=%d) est invalide\n",*iocc) ;
                printf( "             commande : %s\n",PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                printf( "             mot-cle facteur : %s\n",mfc) ;
                printf( "             mot-cle simple  : %s\n",mcs) ;
                MYABORT( "erreur d'utilisation detectee") ;
        }

                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvc8","ssiii",
            mfc,mcs,*iocc,*iarg,*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");
        ASSERT(PyTuple_Check(res)) ;


        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if(!ok)MYABORT("erreur dans la partie Python");
        ISCRUTE(*nbval) ;


        nval=*nbval;
        if(*nbval < 0)nval=*mxval;
        ISCRUTE(nval) ;

        convc8(nval,tup,val);



#ifdef _FORT15_        /*{*/

        /* --- construction du message pour fort.15 --- */

        if ( nval > 0 ){
                int taille  = 0 ;
                if ( (taille=ConstruitFort15( mcs , mfc )) ){
                        char s[16] ;
                        int k       = 0 ;
                        for(k=0;k<2*nval && k<10;k++){
                                sprintf( s , " %12.5E" , val[k] ) ;
                                AjoutChaineA( &Pour_fort15 , s ) ;

                                                        ASSERT(s!=(char*)0) ;
                                taille += strlen(s) ;
                                ASSERT(taille==strlen(Pour_fort15)) ;
                        }
                }
        }

        /* --- FIN construction du message pour fort.15 --- */

        if ( Pour_fort15 != (char*)0 ) Fort15() ;

#endif                        /*}*/ /* # ifdef _FORT15_ */



        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getvc8_) ;
        return ;
}





#if defined HPUX
    void getvr8 (_IN char *motfac,_IN char *motcle,_IN INTEGER *iocc,_IN INTEGER *iarg,_IN INTEGER *mxval,
                 _INOUT double *val,_OUT INTEGER *nbval,_IN int lfac,_IN int lcle )
#elif defined PPRO_NT
    void __stdcall GETVR8 (_IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle,_IN INTEGER *iocc,
                           _IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT double *val,_OUT INTEGER *nbval )
#else
void getvr8_(_IN char *motfac,_IN char *motcle,_IN INTEGER *iocc,_IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT double *val,_OUT INTEGER *nbval,_IN int lfac,_IN int lcle )
#endif
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
        char *mfc      = (char*)0 ;
        char *mcs      = (char*)0 ;

        _DEBUT(getvr8) ;
        SSCRUTE(PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
        FSSCRUTE(motfac,lfac) ; FSSCRUTE(motcle,lcle) ; ISCRUTE(*iocc) ;
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
                printf( "<F> GETVR8 : le numero d'occurence (IOCC=%d) est invalide\n",*iocc) ;
                printf( "             commande : %s\n",PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                printf( "             mot-cle facteur : %s\n",mfc) ;
                printf( "             mot-cle simple  : %s\n",mcs) ;
                MYABORT( "erreur d'utilisation detectee") ;
        }


                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvr8","ssiii",
            mfc,mcs,*iocc,*iarg,*mxval);

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



#ifdef _FORT15_        /*{*/

        /* --- construction du message pour fort.15 --- */

        if ( nval > 0 ){
                int taille  = 0 ;
                if ( (taille=ConstruitFort15( mcs , mfc )) ){
                        char s[16] ;
                        int k       = 0 ;
                        for(k=0;k<nval && k<5;k++){
                                sprintf( s , " %12.5E" , val[k] ) ;
                                AjoutChaineA( &Pour_fort15 , s ) ;
                                                        ASSERT(s!=(char*)0) ;
                                taille += strlen(s) ;
                                                        ASSERT(Pour_fort15!=(char*)0) ;
                                                        ASSERT(taille==strlen(Pour_fort15)) ;
                        }
                }
        }

        /* --- FIN construction du message pour fort.15 --- */

        if ( Pour_fort15 != (char*)0 ) Fort15() ;

#endif                        /*}*/ /* # ifdef _FORT15_ */


        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getvr8) ;
        return ;
}




#if defined HPUX
void getvis (_IN char *motfac,_IN char *motcle,_IN INTEGER *iocc,_IN INTEGER *iarg,_IN INTEGER *mxval,
             _INOUT INTEGER *val,_OUT INTEGER *nbval,_IN int lfac,_IN int lcle )
#elif defined PPRO_NT
void __stdcall GETVIS (_IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle,_IN INTEGER *iocc,
                       _IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT INTEGER *val,_OUT INTEGER *nbval )
#else
void getvis_(_IN char *motfac,_IN char *motcle,_IN INTEGER *iocc,_IN INTEGER *iarg,_IN INTEGER *mxval,
             _INOUT INTEGER *val,_OUT INTEGER *nbval,_IN int lfac,_IN int lcle )
#endif
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
        char *mfc      = (char*)0 ;
        char *mcs      = (char*)0 ;

        _DEBUT(getvis_) ;
        FSSCRUTE(motfac,lfac) ; FSSCRUTE(motcle,lcle) ;
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
                printf( "<F> GETVIS : le numero d'occurence (IOCC=%d) est invalide\n",*iocc) ;
                printf( "             commande : %s\n",PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                printf( "             mot-cle facteur : %s\n",mfc) ;
                printf( "             mot-cle simple  : %s\n",mcs) ;
                MYABORT( "erreur d'utilisation detectee") ;
        }

                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvis","ssiii",
            mfc,mcs,*iocc,*iarg,*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if (!ok)MYABORT("erreur dans la partie Python");

        nval=*nbval;
        if(*nbval < 0)nval=*mxval;
        convert(nval,tup,val);

#ifdef _DEBOG_
        if(nval > 0)printf("\tGETVIS : val %ld\n",val[0]);
#endif



#ifdef _FORT15_        /*{*/

        /* --- construction du message pour fort.15 --- */

        if ( nval > 0 ){
                int taille  = 0 ;
                if ( (taille=ConstruitFort15( mcs , mfc )) ){
                        char s[9] ;
                        int k       = 0 ;
                        for(k=0;k<nval && k<5;k++){
                                sprintf( s , " %d" , val[k] ) ;
                                AjoutChaineA( &Pour_fort15 , s ) ;
                                                        ASSERT(s!=(char*)0) ;
                                taille += strlen(s) ;
                                                        ASSERT(Pour_fort15!=(char*)0) ;
                                                        ASSERT(taille==strlen(Pour_fort15)) ;
                        }
                }
        }

        /* --- FIN construction du message pour fort.15 --- */

        if ( Pour_fort15 != (char*)0 ) Fort15() ;

#endif                        /*}*/ /* # ifdef _FORT15_ */


        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getvis_) ;
        return ;
}


#if defined HPUX
void getvli( _OUT INTEGER *unite , _OUT char *cas , _IN int lcas )
#elif defined PPRO_NT
void __stdcall GETVLI( _OUT INTEGER *unite , _OUT char *cas , _IN int lcas )
#else
void getvli_ ( _OUT INTEGER *unite , _OUT char *cas , _IN int lcas )
#endif
{
        /*
        Cette fonction est destinee a etre utilisee pour le fichier "*.code" (fort.15)
        */
        int k=0 ;

        *unite = 15 ;
        for ( k=0 ; k<lcas ; k++ ) cas[k] = ' ' ;
                                                        ASSERT(NomCas!=(char*)0) ;
        strncpy( cas , NomCas , (size_t)min(lcas,strlen(NomCas)) ) ;
        return ;
}




#if defined HPUX
void getvls (char *motfac,char *motcle,INTEGER *iocc,INTEGER *iarg,INTEGER *mxval,INTEGER *val,INTEGER *nbval,int lfac,int lcle )
#elif defined PPRO_NT
void __stdcall GETVLS (char *motfac,int lfac,char *motcle,int lcle,INTEGER *iocc,INTEGER *iarg,INTEGER *mxval,INTEGER *val,INTEGER *nbval )
#else
void getvls_(char *motfac,char *motcle,INTEGER *iocc,INTEGER *iarg,INTEGER *mxval,INTEGER *val,INTEGER *nbval,int lfac,int lcle )
#endif
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
        char *mfc      = (char*)0 ;
        char *mcs      = (char*)0 ;

        _DEBUT(getvls_) ;
        FSSCRUTE(motfac,lfac) ; FSSCRUTE(motcle,lcle) ;
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
                printf( "<F> GETVLS : le numero d'occurence (IOCC=%d) est invalide\n",*iocc) ;
                printf( "             commande : %s\n",PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                printf( "             mot-cle facteur : %s\n",mfc) ;
                printf( "             mot-cle simple  : %s\n",mcs) ;
                MYABORT( "erreur d'utilisation detectee") ;
        }

                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvls","ssiii",
            mfc,mcs,*iocc,*iarg,*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if (!ok)MYABORT("erreur dans la partie Python");

        nval=*nbval;
        if(*nbval < 0)nval=*mxval;
        convert(nval,tup,val);

#ifdef _DEBOG_
        if(nval > 0)printf("\tGETVLS : val %ld\n",val[0]);
#endif



#ifdef _FORT15_        /*{*/

        /* --- construction du message pour fort.15 --- */

        if ( nval > 0 ){
                int taille  = 0 ;
                if ( (taille=ConstruitFort15( mcs , mfc )) ){
                        char s[9] ;
                        int k       = 0 ;
                        for(k=0;k<nval && k<5;k++){
                                sprintf( s , " %d" , val[k] ) ;
                                AjoutChaineA( &Pour_fort15 , s ) ;
                                                        ASSERT(s!=(char*)0) ;
                                taille += strlen(s) ;
                                                        ASSERT(Pour_fort15!=(char*)0) ;
                                                        ASSERT(taille==strlen(Pour_fort15)) ;
                        }
                }
        }

        /* --- FIN construction du message pour fort.15 --- */

        if ( Pour_fort15 != (char*)0 ) Fort15() ;

#endif                        /*}*/ /* # ifdef _FORT15_ */


        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getvls_) ;
        return ;
}




#if defined SOLARIS || IRIX || TRU64 || SOLARIS64 || P_LINUX
#define GETVTX getvtx_
void getvtx_(_IN char *motfac,_IN char *motcle,_IN INTEGER *iocc,_IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT char *txval,_OUT INTEGER *nbval, _IN int lfac, _IN int lcle, _IN int ltx)
#elif defined HPUX
#define GETVTX getvtx
void getvtx (_IN char *motfac,_IN char *motcle,_IN INTEGER *iocc,_IN INTEGER *iarg,_IN INTEGER *mxval,
             _INOUT char *txval,_OUT INTEGER *nbval, _IN int lfac, _IN int lcle, _IN int ltx)
#elif defined PPRO_NT
void __stdcall GETVTX (_IN char *motfac,_IN int lfac,_IN char *motcle, _IN int lcle,_IN INTEGER *iocc,
                       _IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT char *txval,_IN int ltx,_OUT INTEGER *nbval)
#endif
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
        int k          = 0 ;
        char *mfc      = (char*)0 ;
        char *mcs      = (char*)0 ;

        _DEBUT(getvtx_) ;
        SSCRUTE(PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
        FSSCRUTE(motfac,lfac) ; FSSCRUTE(motcle,lcle) ; ISCRUTE(ltx) ; ISCRUTE(*iocc) ;
                                                        /*ASSERT((*iocc>0)||(FindLength(motfac,lfac)==0));*/


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
                printf( "<F> GETVTX : le numero d'occurence (IOCC=%d) est invalide\n",*iocc) ;
                printf( "             commande : %s\n",PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                printf( "             mot-cle facteur : %s\n",mfc) ;
                printf( "             mot-cle simple  : %s\n",mcs) ;
                MYABORT( "erreur d'utilisation detectee") ;
        }

                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvtx","ssiii",mfc,mcs,*iocc,*iarg,*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

#ifdef _DEBOG_
        /*  si non impression du retour */
        TAB ; PyObject_Print(res, stdout, 0); printf("\n");
#endif

        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if (!ok)MYABORT("erreur au decodage d'une chaine dans le module C aster.getvtx");

#ifdef _DEBOG_
         printf("\tGETVTX : nbval %ld : ",*nbval);
         PyObject_Print(tup, stdout, 0);
         printf("\n");
#endif
        nval=*nbval;
        if(*nbval < 0)nval=*mxval;

        if( nval > 0 ){
                convertxt(nval,tup,txval,ltx);
                RES ; FSSCRUTE(txval,ltx) ;
        }


#ifdef _FORT15_        /*{*/

        /* --- construction du message pour fort.15 --- */

        if ( nval > 0 ){
                int taille  = 0 ;
                if ( (taille=ConstruitFort15( mcs , mfc )) ){
                        PyObject* v = (PyObject*)0 ;
                        char *s     = (char*)0 ;
                                                        ASSERT(Pour_fort15!=(char*)0) ;
                                                        ASSERT(taille==strlen(Pour_fort15)) ;
                        for(k=0;k<nval && k<5 ;k++){

                                AjoutChaineA( &Pour_fort15 , " '" ) ;
                                taille += 2 ;
                                                        ASSERT(taille==strlen(Pour_fort15)) ;


                                v=PyTuple_GetItem(tup,k);
                                s=PyString_AsString(v);
                                                        ASSERT(s!=(char*)0) ;

                                AjoutChaineA( &Pour_fort15 , s ) ;
                                                        ASSERT(s!=(char*)0) ;
                                taille += strlen(s) ;
                                                        ASSERT(taille==strlen(Pour_fort15)) ;


                                AjoutChaineA( &Pour_fort15 , "'" ) ;
                                taille += 1 ;
                                                        ASSERT(taille==strlen(Pour_fort15)) ;
                        }
                }
        }

        /* --- FIN construction du message pour fort.15 --- */

        if ( Pour_fort15 != (char*)0 ) Fort15() ;

#endif                        /*}*/ /* # ifdef _FORT15_ */

#ifdef _DEBOG_
        if( *iocc <= 0 ){
                ISCRUTE(*nbval) ;
        }
#endif

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getvtx_) ;
        return ;
}




#if defined HPUX
void getftx (_IN char *motfac,_IN char *motcle,_IN INTEGER *iocc,_IN INTEGER *iarg,_IN INTEGER *mxval,
             _INOUT char *txval,_OUT INTEGER *nbval, _IN int lfac, _IN int lcle, _IN int ltx)
#elif defined PPRO_NT
void __stdcall GETFTX (_IN char *motfac, _IN int lfac,_IN char *motcle,_IN int lcle,_IN INTEGER *iocc,
                       _IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT char *txval,_IN int ltx,_OUT INTEGER *nbval)
#else
void getftx_(_IN char *motfac,_IN char *motcle,_IN INTEGER *iocc,_IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT char *txval,_OUT INTEGER *nbval, _IN int lfac, _IN int lcle, _IN int ltx)
#endif
{
        /*

          Procedure GETFTX pour le FORTRAN : destinee a l'usage (exclusif) par OPS005
          Cette fonction retourne - dans une tableau de chaines de caracteres fortran -
          une formule ASTER stockee dans le jeu de commande.

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
        int k          = 0 ;
        char *mfc      = (char*)0 ;
        char *mcs      = (char*)0 ;

        _DEBUT(getftx_) ;
        SSCRUTE(PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
        FSSCRUTE(motfac,lfac); FSSCRUTE(motcle,lcle); ISCRUTE(*mxval);ISCRUTE(ltx) ; ISCRUTE(*iocc);
                                                        /*ASSERT((*iocc>0)||(FindLength(motfac,lfac)==0));*/

        mfc=fstr1(motfac,lfac);
        mcs=fstr2(motcle,lcle);
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvtx","ssiii",mfc,mcs,*iocc,*iarg,*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

#ifdef _DEBOG_
        /*  si non impression du retour */
        TAB ; PyObject_Print(res, stdout, 0); printf("\n");
#endif

        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if (!ok)MYABORT("erreur au decodage d'une chaine dans le module C aster.getftx");

#ifdef _DEBOG_
         printf("\tGETFTX : nbval %ld : ",*nbval);
         PyObject_Print(tup, stdout, 0);
         printf("\n");
#endif
        nval=*nbval;
        if(*nbval < 0)nval=*mxval;

        if( nval > 0 ){
                /*
                le tableau de mxval chaines et interprete comme une chaine de mxval*ltx
                caracteres.
                */
                ISCRUTE(PyString_Size(PyTuple_GetItem(tup,0))) ;
                ASSERT(PyString_Size(PyTuple_GetItem(tup,0))<=(*mxval)*ltx) ;
                convertxt(1,tup,txval,(*mxval)*ltx);
                FSSCRUTE(txval,(*mxval)*ltx) ;
        }

#ifdef _DEBOG_
        if( *iocc <= 0 ){
                ISCRUTE(*nbval) ;
        }
#endif

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getftx_) ;
        return ;
}





#ifdef _FORT15_        /*{*/


int ConstruitFort15( _IN char *motcle , _IN char *motfac )
{
        /*
        INTENTION : initialiser la chaine de caracteres affichee dans le fichier Aster : fort.15
                    avec :
                       - le nom du cas (NomCas)
                       - le nom de la commande courante
                       - le nom du mot-cle simple passe en argument
                       - le nom du mot-cle facteur passe en argument (ou les caracteres "__")

                    la chaine sera affichee par le sous programme fortran AFCHAI par
                    l'intermediaire de la fonction C Fort15() definie dans le present fichier.
        */

        int taille = 0 ;
        char *ptrChaine = (char*)0 ;


        if( NomCas ){

                PyObject *res = (PyObject*)0 ;
                char *nomCommande = (char*)0 ;

                int ll = -1 ;
                int k ;
                const int format    = 20 ;
                                                                ASSERT(motcle!=(char*)0);
                                                                ASSERT(motfac!=(char*)0);
                                                                ASSERT(Pour_fort15==(char*)0);


                /*---------------------------- nom du cas ------------------------------*/

                AjoutChaineA( &Pour_fort15 , NomCas ) ;
                taille = (Pour_fort15!=(char*)0) ? strlen(Pour_fort15) : 0 ;
                                                                ASSERT(taille==strlen(Pour_fort15));
                                                                ASSERT((taille+2)<=Taille_fort15);
                AjoutChaineA( &Pour_fort15 , "  " ) ; taille += 2 ;
                                                                ASSERT(taille==strlen(Pour_fort15));



                /*------------------------- nom de la commande  -------------------------*/

                res=PyObject_CallMethod(commande,"retnom","");
                if (res == NULL)MYABORT("erreur dans la partie Python");
                ASSERT(PyString_Check(res)) ;
                nomCommande=PyString_AsString(res);
                ASSERT(nomCommande!=(char*)0) ;

                AjoutChaineA( &Pour_fort15 , nomCommande ) ;
                taille += strlen(nomCommande) ;                        ASSERT(taille==strlen(Pour_fort15));
                for( k=strlen(nomCommande) ; k<format ; k++ ){
                        AjoutChaineA( &Pour_fort15 , " " ) ;
                        taille++ ;                                ASSERT(taille==strlen(Pour_fort15));
                }



                /*--------------------------- nom du mot-cle simple ----------------------------*/

                AjoutChaineA( &Pour_fort15 , motcle ) ;
                taille += strlen(motcle) ;                        ASSERT(taille==strlen(Pour_fort15));
                for( k=strlen(motcle) ; k<format ; k++ ){
                        AjoutChaineA( &Pour_fort15 , " " ) ;
                        taille++ ;                                ASSERT(taille==strlen(Pour_fort15));
                }



                /*--------------------------- nom du mot-cle facteur ----------------------------*/

                                                                ASSERT((taille+2)<=Taille_fort15);
                AjoutChaineA( &Pour_fort15 , "  " ) ; taille += 2 ;
                if( motfac[0]==' ' ){
                        ll=2 ;                                        ASSERT((taille+ll)<=Taille_fort15);
                        AjoutChaineA( &Pour_fort15 , "__" )  ;
                }
                else{
                        ll=strlen(motfac) ;                        ASSERT((taille+ll)<=Taille_fort15);
                        AjoutChaineA( &Pour_fort15 , motfac )  ;
                }
                taille += ll ;
                                                                ASSERT(taille==strlen(Pour_fort15));

                for( k=ll ; k<format ; k++ ){
                                                                ASSERT((taille+1)<=Taille_fort15);
                        AjoutChaineA( &Pour_fort15 , " " ) ;
                        taille++ ;                                ASSERT(taille==strlen(Pour_fort15));
                }
                ptrChaine=(char*)Pour_fort15 ;
                                                                ASSERT((taille+4)<=Taille_fort15);
                AjoutChaineA( &Pour_fort15 , "      " ) ;
                taille += 6 ;
                ASSERT(taille==strlen(Pour_fort15));

        }
        return taille ;
}




void Fort15( void )
{

        /*
        INTENTION : ecrire dans fort.15 la chaine de caracteres (Pour_fort15) reservee et
                    initialisee par ConstruitFort15,
                    en appelant la routine fortran AFCHAI (afchai_).

                    la chaine globale Pour_fort15 est declaree au debut du fichier courant.
        */

#if defined PPRO_NT
void __stdcall AFCHAI ( char * , int , long* ) ; /* sous-programme fortran */
#elif defined HPUX
#define AFCHAI afchai
void afchai( char * , long* , int ) ;
#else
#define AFCHAI afchai_
void afchai_( char * , long* , int ) ;
#endif
        int longueur ;
        long lu = 15 ;

                                                        ASSERT(Pour_fort15!=(char*)0)
        longueur = strlen(Pour_fort15) ;
        ISCRUTE(longueur) ;
                                                        ASSERT(longueur>=0)
#if defined PPRO_NT
        if ( longueur>0 ) AFCHAI ( Pour_fort15 , longueur , &lu ) ; /* voir afchai.f */
#else
        if ( longueur>0 ) AFCHAI ( Pour_fort15 , &lu , longueur ) ;
#endif
        free(Pour_fort15) ;
        Pour_fort15 = (char*)0 ;
}

#endif                        /*}*/ /* # ifdef _FORT15_ */




#if defined HPUX
void getvid (char *motfac,char *motcle,INTEGER *iocc,INTEGER *iarg,INTEGER *mxval,char *txval,INTEGER *nbval,int lfac,int lcle ,int ltx)
#elif defined PPRO_NT
void __stdcall GETVID (char *motfac,int lfac,char *motcle,int lcle,INTEGER *iocc,INTEGER *iarg,INTEGER *mxval,char *txval,int ltx,INTEGER *nbval)
#else
void getvid_(char *motfac,char *motcle,INTEGER *iocc,INTEGER *iarg,INTEGER *mxval,char *txval,INTEGER *nbval,int lfac,int lcle,int ltx)
#endif
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
        int ok,nval;
        char *mfc;
        char *mcs;


        _DEBUT(getvid_) ;
        SSCRUTE(PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
        FSSCRUTE(motfac,lfac) ; FSSCRUTE(motcle,lcle) ;
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
                printf( "<F> GETVID : le numero d'occurence (IOCC=%d) est invalide\n",*iocc) ;
                printf( "             commande : %s\n",PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                printf( "             mot-cle facteur : %s\n",mfc) ;
                printf( "             mot-cle simple  : %s\n",mcs) ;
                MYABORT( "erreur d'utilisation detectee") ;
        }



                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvid","ssiii",mfc,mcs,*iocc,*iarg,*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

#ifdef _DEBOG_
        /*  si non impression du retour */
        TAB ;
        PyObject_Print(res, stdout, 0);
        printf("\n");
#endif
        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if (!ok)MYABORT("erreur dans la partie Python");

        ISCRUTE((INTEGER)*nbval) ;


        nval=*nbval;
        if(*nbval < 0)nval=*mxval;
        ISCRUTE(nval) ;
        if ( nval > 0 ){
                convertxt(nval,tup,txval,ltx);
                ISCRUTE(ltx) ;
                ISCRUTE(nval*ltx) ;
                FSSCRUTE(txval,nval*ltx) ;
        }

#ifdef _FORT15_        /*{*/

        /* --- construction du message pour fort.15 --- */

        if ( nval > 0 ){
                int taille  = 0 ;
                if ( (taille=ConstruitFort15( mcs , mfc )) ){
                        PyObject* v = (PyObject*)0 ;
                        char *s     = (char*)0 ;
                        int k       = 0 ;
                        for(k=0;k<nval && k<5;k++){
                                v=PyTuple_GetItem(tup,k);
                                s=PyString_AsString(v);
                                ASSERT(s!=(char*)0) ;

                                AjoutChaineA( &Pour_fort15 , " " ) ;
                                taille += 1 ;
                                ASSERT(taille==strlen(Pour_fort15)) ;

                                AjoutChaineA( &Pour_fort15 , s ) ;
                                                        ASSERT(s!=(char*)0) ;
                                taille += strlen(s) ;
                                                        ASSERT(Pour_fort15!=(char*)0) ;
                                                        ASSERT(taille==strlen(Pour_fort15)) ;
                        }
                }
        }

        /* --- FIN construction du message pour fort.15 --- */

        if ( Pour_fort15 != (char*)0 ) Fort15() ;

#endif                        /*}*/ /* # ifdef _FORT15_ */


        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getvid_) ;
        return ;
}




#if defined HPUX
void smcdel (INTEGER *iold,INTEGER *inew,INTEGER *ierusr)
#elif defined PPRO_NT
void __stdcall SMCDEL (INTEGER *iold,INTEGER *inew,INTEGER *ierusr)
#else
void smcdel_(INTEGER *iold,INTEGER *inew,INTEGER *ierusr)
#endif
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
        _DEBUT("smcdel_")
        res=PyObject_CallMethod(commande,"smcdel","ll",*iold,*inew);

        /*
            Si le retour est NULL : une exception a ete levee dans le code Python appele
            Cette exception est a transferer normalement a l appelant mais FORTRAN ???
            On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)MYABORT("erreur a l appel de smcdel dans la partie Python");
        *ierusr=*ierusr+PyInt_AsLong(res);
        Py_DECREF(res);
        _FIN("smcdel_")
}


#if defined HPUX
void smdcmd (INTEGER *jcmd,char *result,char *comman,INTEGER *ierusr,int lresult,int lcomman)
#elif defined PPRO_NT
void __stdcall SMDCMD (INTEGER *jcmd,char *result,int lresult,char *comman,int lcomman,INTEGER *ierusr)
#else
void smdcmd_(INTEGER *jcmd,char *result,char *comman,INTEGER *ierusr,int lresult,int lcomman)
#endif
{
        /*
          Entrees:
            jcmd numero d ordre de la commande en cours
            result nom du resultat produit par la commande
            comman nom de la commande a debuter

          Sorties:
            ierusr code retour d erreur incremente
        */
        _DEBUT("smdcmd_"){
        PyObject * res = PyObject_CallMethod(commande,"smdcmd","ls#s#",*jcmd,result,lresult,comman,lcomman);
        /*
            Si le retour est NULL : une exception a ete levee dans le code Python appele
            Cette exception est a transferer normalement a l appelant mais FORTRAN ???
            On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
           MYABORT("erreur a l appel de smdcmd dans la partie Python");
        *ierusr=*ierusr+PyInt_AsLong(res);
        Py_DECREF(res);
        }_FIN("smdcmd_") ;
}


#if defined HPUX
void smfcmd(INTEGER *ierusr)
#elif defined PPRO_NT
void __stdcall SMFCMD(INTEGER *ierusr)
#else
void smfcmd_(INTEGER *ierusr)
#endif
{
        /*
          Entrees:

          Sorties:
            ierusr code retour d erreur incremente
        */
        _DEBUT("smfcmd_"){
        PyObject *res = PyObject_CallMethod(commande,"smfcmd","");
        /*
            Si le retour est NULL : une exception a ete levee dans le code Python appele
            Cette exception est a transferer normalement a l appelant mais FORTRAN ???
            On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
           MYABORT("erreur a l appel de smfcmd dans la partie Python");
        *ierusr=*ierusr+PyInt_AsLong(res);
        Py_DECREF(res);
        }_FIN("smfcmd_" ) ;
}


#if defined HPUX
void smdmcf (char * motf,INTEGER *ierusr,int lmotf)
#elif defined PPRO_NT
void __stdcall SMDMCF (char * motf,int lmotf,INTEGER *ierusr)
#else
void smdmcf_(char * motf,INTEGER *ierusr,int lmotf)
#endif
{
        /*
          Entrees:
            motf mot cle facteur a debuter

          Sorties:
            ierusr code retour d erreur incremente
        */
        _DEBUT("smdmcf_"){
        PyObject * res = PyObject_CallMethod(commande,"smdmcf","s#",motf,lmotf);
        /*
            Si le retour est NULL : une exception a ete levee dans le code Python appele
            Cette exception est a transferer normalement a l appelant mais FORTRAN ???
            On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
           MYABORT("erreur a l appel de smdmcf dans la partie Python");
        *ierusr=*ierusr+PyInt_AsLong(res);
        Py_DECREF(res);
        }_FIN("smdmcf_") ;
}


#if defined HPUX
void smfmcf (INTEGER *ierusr)
#elif defined PPRO_NT
void __stdcall SMFMCF (INTEGER *ierusr)
#else
void smfmcf_(INTEGER *ierusr)
#endif
{
        /*
          Entrees:

          Sorties:
            ierusr code retour d erreur incremente
        */
        _DEBUT("smfmcf_"){
        PyObject * res = PyObject_CallMethod(commande,"smfmcf","");
        /*
            Si le retour est NULL : une exception a ete levee dans le code Python appele
            Cette exception est a transferer normalement a l appelant mais FORTRAN ???
            On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
           MYABORT("erreur a l appel de smfmcf dans la partie Python");
        *ierusr=*ierusr+PyInt_AsLong(res);
        Py_DECREF(res);
        }_FIN("smfmcf_") ;
}

long FindLength( _IN char *chaineFortran , _IN INTEGER longueur )
{
        /*
        Fonction  : FindLength
        Intention
                Retourne la taille exacte de la chaine de caracteres fortran
                chaineFortran contenant eventuellement des bmancs de fin de ligne..
                La taille exacte est la longueur de la chaine du debut au
                dernier caractere non blanc.
        */

        long k = longueur-1 ;
        if ( ! chaineFortran ) return 0 ;

        while( k>=0 && chaineFortran[k]==' ' ) k-- ;
        return k+1 ;
}

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
        int i;
        char *deb=kval;
        if(nbval == 1){
                return PyString_FromStringAndSize(deb,FindLength(deb,*lval));
        }
        else{
                PyObject *t=PyTuple_New(nbval);
                for(i=0;i<nbval;i++){
                        if(PyTuple_SetItem(t,i,PyString_FromStringAndSize(deb,FindLength(deb,lval[i]))))return NULL;
                        deb=deb+lkval;
                }
                return t;
        }
}
PyObject * MakeTupleString_pour_putvid(long nbval,char *kval,int lkval)
{
        /*
                  Entrees:
                    nbval nombre de chaines dans kval
                    kval  tableau de nbval chaines FORTRAN
                    lkval longueur des chaines FORTRAN (compilateur)
                    lval  longueur des chaines FORTRAN (utilisateur)
                  Sorties:
                    RETOUR fonction : tuple de string Python de longueur nbval
                  Fonction:
                    Convertir un tableau de chaines FORTRAN en un tuple de string Python de meme longueur
        */
        int i;
        char *deb=kval;
        if(nbval == 1){
                return PyString_FromStringAndSize(deb,FindLength(deb,lkval));
        }
        else{
                PyObject *t=PyTuple_New(nbval);
                for(i=0;i<nbval;i++){
                        if(PyTuple_SetItem(t,i,PyString_FromStringAndSize(deb,FindLength(deb,lkval))))return NULL;
                        deb=deb+lkval;
                }
                return t;
        }
}



PyObject * MakeTupleInt(long nbval,long* kval)
{
        /*
                  Entrees:
                    nbval nombre de chaines dans kval
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
                        if(PyTuple_SetItem(t,i,PyInt_FromLong(kval[i])))return NULL;
                }
                return t;
        }
}

PyObject * MakeTupleFloat(long nbval,double * kval)
{
        /*
                  Entrees:
                    nbval nombre de chaines dans kval
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
                        if(PyTuple_SetItem(t,i,PyFloat_FromDouble(kval[i])))return NULL;
                }
                return t;
        }
}


#if defined HPUX
void putvid (char *motcle,INTEGER *nbval,char *kval,INTEGER *ierusr,int lmotcle,int lkval)
#elif defined PPRO_NT
void __stdcall PUTVID (char *motcle,int lmotcle,INTEGER *nbval,char *kval,int lkval,INTEGER *ierusr)
#else
void putvid_(char *motcle,INTEGER *nbval,char *kval,INTEGER *ierusr,int lmotcle,int lkval)
#endif
{
        /*
                  Entrees:
                    motcle nom du mot cle
                    nbval nombre de concepts
                    kval liste des concepts
                  Sorties:
                    ierusr code retour d erreur incremente
                  Fonction:
                    Rentrer une liste d argument de type identificateur (concept)
        */
        PyObject *res = (PyObject*)0 ;
        _DEBUT("putvid_") ;{
        if(*nbval>0)
        {
                PyObject * t=MakeTupleString_pour_putvid(*nbval,kval,lkval);
                if (t == NULL)
                        MYABORT("erreur a l appel de putvid : impossible de creer un tuple");

                ASSERT(motcle!=NULL) ;
                ASSERT(lmotcle>0) ;
                res = PyObject_CallMethod(commande,"putvid","s#lO",motcle,FindLength(motcle,lmotcle),*nbval,t);
                Py_DECREF(t);
                ISCRUTE(t->ob_refcnt) ;
                /*
                            Si le retour est NULL : une exception a ete levee dans le code Python appele
                            Cette exception est a transferer normalement a l appelant mais FORTRAN ???
                            On produit donc un abort en ecrivant des messages sur la stdout
                */
                if (res == NULL)
                        MYABORT("erreur a l appel de putvid dans la partie Python");

                REFSCRUTE(t);
                *ierusr=*ierusr+PyInt_AsLong(res);
                Py_DECREF(res);
        }
        }_FIN("putvid_") ;
}


#if defined HPUX
void putvis (char *motcle,INTEGER *nbval,INTEGER *ival,INTEGER *ierusr,int lmotcle)
#elif defined PPRO_NT
void __stdcall PUTVIS (char *motcle,int lmotcle,INTEGER *nbval,INTEGER *ival,INTEGER *ierusr)
#else
void putvis_(char *motcle,INTEGER *nbval,INTEGER *ival,INTEGER *ierusr,int lmotcle)
#endif
{
        /*
                  Entrees:
                    motcle nom du mot cle
                    nbval nombre d entiers
                    ival liste des entiers
                  Sorties:
                    ierusr code retour d erreur incremente
                  Fonction:
                    Rentrer une liste d argument de type  entier
        */
        PyObject *res = (PyObject*)0 ;
        _DEBUT("putvis_") ;{
        PyObject * t=MakeTupleInt(*nbval,ival);
        if (t == NULL)
                MYABORT("erreur a l appel de putvis : impossible de creer un tuple");

        res = PyObject_CallMethod(commande,"putvis","s#lO",motcle,FindLength(motcle,lmotcle),*nbval,t);
        Py_DECREF(t);
        /*
                    Si le retour est NULL : une exception a ete levee dans le code Python appele
                    Cette exception est a transferer normalement a l appelant mais FORTRAN ???
                    On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
                MYABORT("erreur a l appel de putvis dans la partie Python");

        REFSCRUTE(t);

        *ierusr=*ierusr+PyInt_AsLong(res);
        Py_DECREF(res);
        }_FIN("putvis_") ;
}



#if defined HPUX
void putvr8 (char *motcle,INTEGER *nbval,double *rval,INTEGER *ierusr,int lmotcle)
#elif defined PPRO_NT
void __stdcall PUTVR8 (char *motcle,int lmotcle,INTEGER *nbval,double *rval,INTEGER *ierusr)
#else
void putvr8_(char *motcle,INTEGER *nbval,double *rval,INTEGER *ierusr,int lmotcle)
#endif
{
        /*
                  Entrees:
                    motcle nom du mot cle
                    nbval nombre de reels
                    rval liste des reels
                  Sorties:
                    ierusr code retour d erreur incremente
                  Fonction:
                    Rentrer une liste d argument de type  reel
        */
        PyObject *res = (PyObject*)0 ;
        _DEBUT("putvr8_") ;{
        PyObject * t=MakeTupleFloat(*nbval,rval);
        if (t == NULL)
                MYABORT("erreur a l appel de putvr8 : impossible de creer un tuple");

        res = PyObject_CallMethod(commande,"putvr8","s#lO",motcle,FindLength(motcle,lmotcle),*nbval,t);
        Py_DECREF(t);
        /*
                    Si le retour est NULL : une exception a ete levee dans le code Python appele
                    Cette exception est a transferer normalement a l appelant mais FORTRAN ???
                    On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
                MYABORT("erreur a l appel de putvr8 dans la partie Python");

        REFSCRUTE(t);

        *ierusr=*ierusr+PyInt_AsLong(res);
        Py_DECREF(res);
        }_FIN("putvr8_") ;
}




#if defined HPUX
void putvtx (char *motcle,INTEGER *nbval,char *kval,INTEGER *ival,INTEGER *ierusr,int lmotcle,int lkval)
#elif defined PPRO_NT
void __stdcall PUTVTX (char *motcle,int lmotcle,INTEGER *nbval,char *kval,int lkval,INTEGER *ival,INTEGER *ierusr)
#else
void putvtx_(char *motcle,INTEGER *nbval,char *kval,INTEGER *ival,INTEGER *ierusr,int lmotcle,int lkval)
#endif
{
        /*
                  Entrees:
                    motcle nom du mot cle
                    nbval nombre de texte
                    kval liste des nbval textes
                    ival liste des nbval longueurs de texte
                  Sorties:
                    ierusr code retour d erreur incremente
                  Fonction:
                    Rentrer une liste d argument de type texte
        */
        PyObject * res = (PyObject*)0 ;
        _DEBUT("putvtx_") ; FSSCRUTE(motcle,lmotcle);FSSCRUTE(kval,lkval);ISCRUTE(*ival) ; ISCRUTE(lkval);{

        PyObject * t=MakeTupleString(*nbval,kval,lkval,ival);
        if (t == NULL)
                MYABORT("erreur a l appel de putvtx : impossible de creer un tuple");

        res = PyObject_CallMethod(commande,"putvtx","s#lOl",motcle,FindLength(motcle,lmotcle),*nbval,t,*ival);
        Py_DECREF(t);
        /*
                    Si le retour est NULL : une exception a ete levee dans le code Python appele
                    Cette exception est a transferer normalement a l appelant mais FORTRAN ???
                    On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
                MYABORT("erreur a l appel de putvtx dans la partie Python");

        REFSCRUTE(t);
        *ierusr=*ierusr+PyInt_AsLong(res);
        Py_DECREF(res);
        }_FIN("putvtx_") ;
}


#if defined SOLARIS || IRIX || TRU64 || SOLARIS64 || P_LINUX
#define GCUCON gcucon_
void gcucon_(INTEGER *icmd, char *resul, char *concep, INTEGER *ier, int lresul, int lconcep)
#elif defined HPUX
#define GCUCON gcucon
void gcucon (INTEGER *icmd, char *resul, char *concep, INTEGER *ier, int lresul, int lconcep)
#elif defined PPRO_NT
void __stdcall GCUCON (INTEGER *icmd, char *resul, int lresul, char *concep, int lconcep, INTEGER *ier)
#else
#endif
{
        /*
                  Entrees:
                    icmd    numero de la commande
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
        _DEBUT("gcucon_") ;
        ASSERT(lresul) ;
        ASSERT(lconcep) ;
        res = PyObject_CallMethod(commande,"gcucon","ls#s#",*icmd,resul,lresul,concep,lconcep);
        /*
                    Si le retour est NULL : une exception a ete levee dans le code Python appele
                    Cette exception est a transferer normalement a l appelant mais FORTRAN ???
                    On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
                MYABORT("erreur a l appel de gcucon dans la partie Python");

        *ier = PyInt_AsLong(res);
        Py_DECREF(res);
        _FIN("gcucon_") ;
}


#if defined HPUX
void gcucdt (INTEGER *icmd,char *resul,INTEGER *ier,int lresul)
#elif defined PPRO_NT
void __stdcall GCUCDT (INTEGER *icmd,char *resul,int lresul,INTEGER *ier)
#else
void gcucdt_(INTEGER *icmd,char *resul,INTEGER *ier,int lresul)
#endif
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
        _DEBUT("gcucdt_") ;
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
                -1 indique que le concept existe mais d'un autre type. On doit donc
                retourner 1
        */
        if(*ier==-1)*ier=1;
        ICI; ISCRUTE(*ier);

        Py_DECREF(res);
        _FIN("gcucdt_") ;
}


#if defined HPUX
void gettvc (char * nom,char *ctyp,INTEGER *ival,double *rval,INTEGER *ier,int lnom,int lctyp)
#elif defined PPRO_NT
void __stdcall GETTVC (char * nom,int lnom,char *ctyp,int lctyp,INTEGER *ival,double *rval,INTEGER *ier)
#else
void gettvc_(char * nom,char *ctyp,INTEGER *ival,double *rval,INTEGER *ier,int lnom,int lctyp)
#endif
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
        _DEBUT("gettvc_") ;
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

        ISCRUTE(valeur->ob_refcnt) ;
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

        Py_DECREF(res);
        _FIN("gettvc_") ;
}


#if defined HPUX
#define gcecdu_ gcecdu
void gcecdu_(INTEGER *ul,INTEGER *icmdu, INTEGER *numint)
#elif defined PPRO_NT
void __stdcall GCECDU (INTEGER *ul,INTEGER *icmdu, INTEGER *numint)
#else
void gcecdu_(INTEGER *ul,INTEGER *icmdu, INTEGER *numint)
#endif
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
        _DEBUT("gcecdu_") ;
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
        _FIN("gcecdu_") ;
}



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

static PyObject * empile(PyObject *c)
{
        _DEBUT(empile) ;
#ifdef _DEBOG_
        /*  impression de la commande courante */
        PyObject_Print(c, stdout, 0);
        printf("\n");
#endif
        REFSCRUTE(c) ;
        /* PyList_Append incremente de 1 le compteur de ref de c */
        PyList_Append(pile_commandes,c);
        REFSCRUTE(c) ;
        _FIN(empile) ;
        return c;
}

static PyObject * depile()
{
        PyObject * com;
        int l=PyList_Size(pile_commandes);
        _DEBUT(depile) ;
        ISCRUTE(l) ;
        if(l == 0){
          /* Pile vide */
          Py_INCREF( Py_None ) ;
          _FIN(depile) ;
          return Py_None;
        }
        /* Derniere commande dans la pile */
        com = PyList_GetItem(pile_commandes,l-1);
        /* PyList_GetItem n incremente pas le compteur de ref de com */
        REFSCRUTE(com) ;
        /* On tronque la liste a la dimension l-1 */
        PyList_SetSlice(pile_commandes,l-1,l,NULL);
        /* Le compteur de ref de com est decremente de 1 */
        REFSCRUTE(com) ;
        if(l == 1){
          /* La pile tronquee est vide */
          Py_INCREF( Py_None ) ;
          _FIN(depile) ;
          return Py_None;
        }
        /* On ne passe ici que pour les macros avec sous commandes
         * en mode commande par commande */
        /* On retourne la derniere commande de la pile */
        com = PyList_GetItem(pile_commandes,l-2);
        REFSCRUTE(com) ;
        _FIN(depile) ;
        return com;
}


static PyObject* aster_getpara(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        char *nomsd,*nompa,*typsd;
        double rval[2];
        INTEGER ival;
        INTEGER iordr;
        INTEGER ctype=0;
        char kval[80];

#if defined SOLARIS || IRIX || TRU64 || SOLARIS64 || P_LINUX
#define CALL_SDVAPA(nomsd,typsd,nompa,iordr,itype,rval,ival,kval) \
  sdvapa_(nomsd,typsd,nompa,iordr,itype,rval,ival,kval,strlen(nomsd),strlen(typsd),strlen(nompa),80)
  void sdvapa_(char *,char *,char *,INTEGER *,INTEGER *,double *,INTEGER *,char *,int,int,int,int);
#elif defined HPUX
#define CALL_SDVAPA(nomsd,typsd,nompa,iordr,itype,rval,ival,kval) \
  sdvapa(nomsd,typsd,nompa,iordr,itype,rval,ival,kval,strlen(nomsd),strlen(typsd),strlen(nompa),80)
  void sdvapa(char *,char *,char *,INTEGER *,INTEGER *,double *,INTEGER *,char *,int,int,int,int);
#elif defined PPRO_NT
#define CALL_SDVAPA(nomsd,typsd,nompa,iordr,itype,rval,ival,kval) \
  SDVAPA(nomsd,strlen(nomsd),typsd,strlen(typsd),nompa,strlen(nompa),iordr,itype,rval,ival,kval,80)
  void __stdcall SDVAPA(char *,int,char *,int,char *,int,INTEGER *,INTEGER *,double *,INTEGER *,char *,int);
#endif
        if (!PyArg_ParseTuple(args, "sssl",&nomsd,&typsd,&nompa,&iordr)) return NULL;
        try(1){
          CALL_SDVAPA(nomsd,typsd,nompa,&iordr,&ctype,rval,&ival,kval);
          if(ctype == 0){
            Py_INCREF( Py_None ) ;
            return Py_None;
          }
          else if(ctype == 1){
            return PyFloat_FromDouble(rval[0]);
          }
          else if(ctype == 2){
            return PyInt_FromLong(ival);
          }
          else if(ctype == 3){
            return PyComplex_FromDoubles(rval[0],rval[1]);
          }
          else if(ctype == 4){
            return PyString_FromStringAndSize(kval,8);
          }
          else if(ctype == 5){
            return PyString_FromStringAndSize(kval,16);
          }
          else if(ctype == 6){
            return PyString_FromStringAndSize(kval,24);
          }
          else if(ctype == 7){
            return PyString_FromStringAndSize(kval,32);
          }
          else if(ctype == 8){
            return PyString_FromStringAndSize(kval,80);
          }
          else{
            PyErr_SetString(PyExc_KeyError, "Parametre inexistant");
            return NULL;
          }
        }
        catch(CodeAbortAster){
          /* une exception a ete levee, elle est destinee a etre traitee dans l'appelant */
          PyErr_SetString(PyExc_KeyError, "Type SD probablement inexistant");
          return NULL;
        }
}


static PyObject* aster_oper(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp;
        INTEGER lot=1 ; /* FORTRAN_TRUE */
        INTEGER iertot=0 ;
        INTEGER icmd=0 ;
        INTEGER ipass=0 ;
        int val=-1 ;
#if defined SOLARIS || IRIX || TRU64 || SOLARIS64 || P_LINUX
#define EXPASS expass_
        void expass_( INTEGER* , INTEGER* , INTEGER* , INTEGER* ) ;
#elif defined PPRO_NT
        void __stdcall EXPASS( INTEGER* , INTEGER* , INTEGER* , INTEGER* ) ;
#elif defined HPUX
#define EXPASS expass
        void expass( INTEGER* , INTEGER* , INTEGER* , INTEGER* ) ;
#endif
        _DEBUT(aster_oper) ;

        if (!PyArg_ParseTuple(args, "Olll",&temp,&lot,&ipass,&icmd)) return NULL;

        /* On empile le nouvel appel */
        commande=empile(temp);

        PyErr_Clear() ;
        fflush(stderr) ;
        fflush(stdout) ;

#ifdef _UTILISATION_SETJMP_

        if ( (val=setjmp(env)) == 0 ){

                /*  appel du sous programme expass pour verif ou exec */


                EXPASS (&lot,&ipass,&icmd,&iertot);

                _FIN(aster_oper) ;

                /* On depile l appel */
                commande = depile();
                return PyInt_FromLong(iertot); /*  retour de la fonction oper sous la forme d un entier */
        }
        else{
                /* une exception a ete levee, elle est destinee a etre traitee dans JDC.py */

                TraitementFinAster( val ) ;
                _FIN(aster_oper) ;

                /* On depile l appel */
                commande = depile();
                return NULL;
        }
#else                /* #ifdef _UTILISATION_SETJMP_ */

                EXPASS (&lot,&ipass,&icmd,&iertot);
                /* On depile l appel */
                commande = depile();

#endif                /* #ifdef _UTILISATION_SETJMP_ */
}



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

        int val=-1 ;
#if defined SOLARIS || IRIX || TRU64 || SOLARIS64 || P_LINUX
#define OPSEXE opsexe_
        void opsexe_( INTEGER* , INTEGER* , INTEGER* , char *,INTEGER* ,int) ;
#elif defined HPUX
#define OPSEXE opsexe
        void opsexe_( INTEGER* , INTEGER* , INTEGER* , char *,INTEGER* ,int) ;
#elif defined PPRO_NT
        void __stdcall OPSEXE ( INTEGER* , INTEGER* , INTEGER* , char *,int ,INTEGER* ) ;
#endif

        _DEBUT(aster_opsexe) ;

        if (!PyArg_ParseTuple(args, "Olll",&temp,&icmd,&ipass,&oper)) return NULL;

        /* On empile le nouvel appel */
        commande=empile(temp);

        PyErr_Clear() ;

#ifdef _UTILISATION_SETJMP_
        if ( (val=setjmp(env)) == 0 ){

                /*  appel du sous programme opsexe */
#if defined PPRO_NT
                OPSEXE (&icmd,&ipass,&oper,cmdusr,32,&ier);
#else
                OPSEXE (&icmd,&ipass,&oper,cmdusr,&ier,32);
#endif
                /* On depile l appel */
                commande = depile();
                _FIN(aster_opsexe) ;
                return PyInt_FromLong(ier); /*  retour de la fonction oper sous la forme d un entier */
        }
        else{
                /* une exception a ete levee, elle est destinee a etre traitee dans JDC.py */

                /* On depile l appel */
                commande = depile();
                TraitementFinAster( val ) ;
                _FIN(aster_opsexe) ;
                return NULL;
        }
#else                /* #ifdef _UTILISATION_SETJMP_ */

#if defined PPRO_NT
                OPSEXE (&icmd,&ipass,&oper,cmdusr,32,&ier);
#else
                OPSEXE (&icmd,&ipass,&oper,cmdusr,&ier,32);
#endif
                /* On depile l appel */
                commande = depile();

#endif                /* #ifdef _UTILISATION_SETJMP_ */
}




static PyObject * aster_repout(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp = (PyObject*)0 ;
        INTEGER maj=1 ;
        INTEGER lnom=0;
        unsigned long ll=129 ;
        char toto[129], *nom ;
#if defined SOLARIS || IRIX || TRU64 || SOLARIS64 || P_LINUX
#define REPOUT repout_
        void repout_(INTEGER *,INTEGER *,char *,int);
#elif defined HPUX
#define REPOUT repout
        void repout(INTEGER *,INTEGER *,char *,int);
#elif defined PPRO_NT
        void __stdcall REPOUT(INTEGER *,INTEGER *,char *,int);
#endif
        _DEBUT(aster_repout) ;
        nom=&toto[0] ;
        if (!PyArg_ParseTuple(args, "")) return NULL;

        REPOUT (&maj,&lnom,nom,ll);

        _FIN(aster_repout)
        return PyString_FromStringAndSize(nom,FindLength(nom,lnom));
}


static PyObject* aster_myeval(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp = (PyObject*)0 ;
        INTEGER ier=0 ;
        INTEGER iclass=0 ;
        INTEGER ival=0 ;
        double rval[2] ; /* contient un nombre reel ou un nombre complexe */
        char *cmdusr="                                                                          ";

        int val=-1 ;
#if defined SOLARIS || IRIX || TRU64 || SOLARIS64 || P_LINUX
#define MYEVAL myeval_
        void myeval_( char* , INTEGER* , INTEGER* , double *,INTEGER* ,int) ;
#elif defined HPUX
#define MYEVAL myeval
        void myeval ( char* , INTEGER* , INTEGER* , double *,INTEGER* ,int) ;
#elif defined PPRO_NT
        void __stdcall MYEVAL ( char* ,int , INTEGER* , INTEGER* , double *,INTEGER* ) ;
#endif
        _DEBUT(aster_myeval) ;
        if (!PyArg_ParseTuple(args, "Os",&temp,&cmdusr)) return NULL;

        /* On empile le nouvel appel */
        commande=empile(temp);

        PyErr_Clear() ;

        if ( (val=setjmp(env)) == 0 ){

                /*  appel du sous programme myeval */
#if defined PPRO_NT
                MYEVAL (cmdusr,strlen(cmdusr),&iclass,&ival,rval,&ier);
#else
                MYEVAL (cmdusr,&iclass,&ival,rval,&ier,strlen(cmdusr));
#endif
                /* On depile l appel */
                commande = depile();
                if(ier != 0){
                        ISCRUTE(ier) ;
                        PyErr_SetString(PyExc_ValueError, "erreur evaluation ASTER");
                        return NULL;
                }
                if(iclass == 1){
                        return PyInt_FromLong(ival);
                }
                else if(iclass == 2){
                        return PyFloat_FromDouble(rval[0]);
                }
                else if(iclass == 5){

                        PyObject *complexe  = (PyObject*)0 ;
                        PyObject *objet     = (PyObject*)0 ;
                        const char *repr    = "RI" ; /* type de representation */

                        DSCRUTE(rval[0]) ; DSCRUTE(rval[1]) ;

                        /* ici, rval contient la partie reelle et la partie imaginaire  du  */
                        /* complexe a stocker dans un tuple                                 */
                        /* (repre,reelle,imaginaire)), destine a etre traite par la         */
                        /* methode Traite_DEFI_VALEUR de la classe EXECUTION (commandes.py) */


                        /* Creation d'une liste et stockage du type et des deux reels */

                        complexe = PyTuple_New( 3 ) ;

                        PyTuple_SetItem( complexe, 0, (objet=PyString_FromString(repr)) ) ;
                        REFSCRUTE(objet) ;
                        ASSERT(objet->ob_refcnt==1) ;
                        PyTuple_SetItem( complexe, 1, PyFloat_FromDouble(rval[0]) ) ;
                        PyTuple_SetItem( complexe, 2, PyFloat_FromDouble(rval[1]) ) ;

                        return complexe ;
                }
                else if(iclass == 6){
                        return PyInt_FromLong(ival);
                }
                else{
                        PyErr_SetString(PyExc_ValueError, "erreur evaluation ASTER");
                        ISCRUTE(iclass) ;
                        return NULL;
                }
        }
        else{
                /* une exception a ete levee, elle est destinee a etre traitee dans JDC.py */

                /* On depile l appel */
                commande = depile();
                TraitementFinAster( val ) ;
                _FIN(aster_myeval) ;
                return NULL;
        }
}



void TraitementFinAster( _IN int val )
{
        _DEBUT("TraitementFinAster") ;
        ISCRUTE(val) ;
        ASSERT(CodeFinAster==19) ;
        ASSERT(CodeAbortAster==20) ;
        switch( val ){
        case 19 :
                PyErr_SetString(PyExc_ValueError, "exit ASTER");
                break ;
        case 20 :
                PyErr_SetString(PyExc_ValueError, "abort ASTER");
                break ;
        default :
                MESSAGE("code erreur INCONNU !!!!") ;
                ISCRUTE(val) ;
                INTERRUPTION(1) ;
                break ;
        }
        _FIN("TraitementFinAster") ;
        return ;
}


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
                                                                ASSERT(commande!=(PyObject*)0);
#if defined PPRO_NT
                GETLTX ( "CODE",4,"NOM",3,iocc,iarg,mxval, longueur ,&nbval) ;
#else
                GETLTX ( "CODE","NOM",iocc,iarg,mxval, longueur ,&nbval,4,3) ;
#endif
                if(nbval == 0){
                  /* Le mot cle NOM n'a pas ete fourni on donne un nom
                   * par defaut au nom du cas */
                  NomCas = strdup("??????");
                }
                else if(nbval > 0){
                  ISCRUTE(longueur[0]) ;
                                                                ASSERT(longueur[0]>0);
                  NomCas = (char*)(malloc((longueur[0]+1)*sizeof(char))) ;
                  strncpy(NomCas,blan,longueur[0]);        /* initialisation a blanc */
                  NomCas[longueur[0]]='\0'; /*  Ne pas oublier la fin de chaine pour Construire_Fort15 */
                                                                ASSERT(NomCas!=(char*)0);
                  ltx = longueur[0];
#if defined PPRO_NT
                  GETVTX ( "CODE",4,"NOM",3,iocc,iarg,mxval, NomCas, ltx ,&nbval) ;
#else
                  GETVTX ( "CODE","NOM",iocc,iarg,mxval, NomCas ,&nbval,4,3,ltx) ;
#endif
                }
                else{
                  /* Erreur  */
                  PyErr_SetString(PyExc_KeyError, "Erreur a la recuperation du nom du cas");
                  return -1;
                }
                SSCRUTE(NomCas) ;
                return 0;
}


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
        int val ;
        static int nbPassages=0 ;

        void poursu_( INTEGER* , INTEGER* , INTEGER* ,INTEGER*) ;
#if defined SOLARIS || IRIX || TRU64 || SOLARIS64 || P_LINUX
#define POURSU poursu_
#define GCCPTS gccpts_
        void poursu_( INTEGER* , INTEGER* , INTEGER* ,INTEGER*) ;
        void gccpts_( char *, int );
#elif defined HPUX
#define POURSU poursu
#define GCCPTS gccpts
        void poursu ( INTEGER* , INTEGER* , INTEGER* ,INTEGER*) ;
        void gccpts ( char *, int );
#elif defined PPRO_NT
        void __stdcall POURSU ( INTEGER* , INTEGER* , INTEGER* ,INTEGER*) ;
        void __stdcall GCCPTS ( char *, int );
#endif

        _DEBUT(aster_poursu) ;
        SSCRUTE(aster_ident()) ;
                                                                ASSERT((nbPassages==1)||(commande==(PyObject*)0));
        nbPassages++ ;
        if (!PyArg_ParseTuple(args, "Ol",&temp,&ipass)) return NULL;

        /* On empile le nouvel appel */
        commande=empile(temp);

        PyErr_Clear() ;


#ifdef _UTILISATION_SETJMP_
        if ( (val=setjmp(env)) == 0 ){

                /*  appel de la commande debut (effectue dans POURSU) */
                /*  La routine fortran POURSU traite aussi le cas     */
                /*  de la poursuite de calcul (en retour lonuti       */
                /*  contient le nombre de concepts crees dans le      */
                /*  calcul precedent)                                 */

                POURSU (&lot,&ipass,&ier,&lonuti);


                /* recuperation de la liste des concepts dans une     */
                /* string python                                      */

                concepts=PyString_FromStringAndSize(NULL,lonuti*80);
                GCCPTS (PyString_AsString(concepts),80);
        }
        else{
                /* une exception a ete levee, elle est destinee a etre traitee dans JDC.py */

                /* On depile l appel */
                commande = depile();
                _FIN(aster_poursu) ;
                TraitementFinAster( val ) ;
                return NULL;
        }
#else                /* #ifdef _UTILISATION_SETJMP_ */
                POURSU (&lot,&ipass,&ier,&lonuti);
                concepts=PyString_FromStringAndSize(NULL,lonuti*80);
                GCCPTS (PyString_AsString(concepts),80);
#endif                /* #ifdef _UTILISATION_SETJMP_ */


        /* On recupere le nom du cas */
        if(RecupNomCas() == -1){
          /* Erreur a la recuperation */
          /* On depile l appel */
          commande = depile();
          _FIN(aster_poursu) ;
          return NULL;
        }
        else{
          /* On depile l appel */
          commande = depile();
          /*  retour de la fonction poursu sous la forme
           *  d'un tuple de trois entiers et un objet */
          _FIN(aster_poursu) ;
          return Py_BuildValue("(iiiO)",lot ,ier,lonuti,concepts );
        }
}


static PyObject * aster_debut(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp = (PyObject*)0 ;
        INTEGER ipass=0;
        INTEGER lot=1 ; /* FORTRAN_TRUE */
        INTEGER ier=0 ;
        int val ;
        static int nbPassages=0 ;
#if defined SOLARIS || IRIX || TRU64 || SOLARIS64 || P_LINUX
#define  DEBUT    debut_
        void  DEBUT ( INTEGER* , INTEGER* , INTEGER* ) ;
#elif HPUX
#define  DEBUT    debut
        void  DEBUT ( INTEGER* , INTEGER* , INTEGER* ) ;
#elif defined PPRO_NT
        void  __stdcall DEBUT ( INTEGER* , INTEGER* , INTEGER* ) ;
#endif

        _DEBUT(aster_debut) ;
        SSCRUTE(aster_ident()) ;
                                                                ASSERT((nbPassages==1)||(commande==(PyObject*)0));
        nbPassages++ ;
        if (!PyArg_ParseTuple(args, "Ol",&temp,&ipass)) return NULL;

        /* On empile le nouvel appel */
        commande=empile(temp);

        PyErr_Clear() ;


#ifdef _UTILISATION_SETJMP_
        if ( (val=setjmp(env)) == 0 ){

                /*  appel de la commande debut */

                DEBUT (&lot,&ipass,&ier);
        }
        else{
                /* une exception a ete levee, elle est destinee a etre traitee dans JDC.py */

                /* On depile l appel */
                commande = depile();
                _FIN(aster_debut) ;
                TraitementFinAster( val ) ;
                return NULL;
        }
#else                /* #ifdef _UTILISATION_SETJMP_ */
                DEBUT (&lot,&ipass,&ier);
#endif                /* #ifdef _UTILISATION_SETJMP_ */



        /* On recupere le nom du cas */
        if(RecupNomCas() == -1){
          /* Erreur a la recuperation */
          /* On depile l appel */
          commande = depile();
          _FIN(aster_debut) ;
          return NULL;
        }
        else{
          /* On depile l appel */
          commande = depile();
          /*  retour de la fonction debut sous la forme d un tuple de deux entiers */
          _FIN(aster_debut) ;
          return Py_BuildValue("(ii)",lot ,ier );
        }
}


static PyObject *
aster_init(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        INTEGER lot=1 ; /* FORTRAN_TRUE */
        INTEGER ier=0 ;
        INTEGER dbg=0 ; /* FORTRAN_FALSE */
#if defined SOLARIS || IRIX || TRU64 || SOLARIS64 || P_LINUX
#define IBMAIN ibmain_
   void ibmain_(INTEGER * lot , INTEGER * ier , INTEGER * dbg) ;
#elif defined HPUX
#define IBMAIN ibmain
   void ibmain (INTEGER * lot , INTEGER * ier , INTEGER * dbg) ;
#elif defined PPRO_NT
   void __stdcall IBMAIN (INTEGER * lot , INTEGER * ier , INTEGER * dbg) ;
#endif

        _DEBUT(aster_init)
        if (!PyArg_ParseTuple(args, "l",&dbg)) return NULL;

        IBMAIN (&lot,&ier,&dbg);

        _FIN(aster_init)
        return PyInt_FromLong(ier);
}





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
        char      *chaine  = NULL ;
        PyObject  *liste   = NULL ;
        PyObject  *string  = NULL ;
        char     **argv    = NULL ;

        void asterm( long , char** ) ;

        _DEBUT("aster_argv") ;


        /*
           la fonction aster_argv recoit un tuple d'arguments (ici de taille 1°
           dans lequel est stockee la liste, qui est extraite par l'appel a
           PyArg_ParseTuple.
        */

        ISCRUTE((INTEGER)PyTuple_Size(args)) ;
        if (!PyArg_ParseTuple(args, "O" , &liste )) return NULL;


        /*  Allocation dynamique de argv : on ajoute un argument NULL */

        argc=PyList_GET_SIZE(liste) ;
        ISCRUTE((INTEGER)argc) ;

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
#ifdef _DEBOG_
        for ( k=0 ; (long)k<argc ; k++ ){
                ISCRUTE(k);SSCRUTE(argv[k]) ;
        }
#endif


        /* Passage des arguments a Code_Aster */

        asterm(argc,argv) ;


        ASSERT(argv) ;
        free(argv);
        argv=(char**)0 ;

        Py_INCREF( Py_None ) ;
        _FIN("aster_argv") ;
        return Py_None;
}




#ifdef _UTILISATION_SETJMP_
#if defined HPUX
void xfini ( _IN INTEGER *codeRetour )
#elif defined PPRO_NT
void __stdcall XFINI ( _IN INTEGER *codeRetour )
#else
void xfini_( _IN INTEGER *codeRetour )
#endif
{
        /* cette fonction est appelee par la routine fortran JEFINI */

        int retour ;

        _DEBUT("xfini_") ;
        switch( *codeRetour ){
        case 0 : retour=CodeFinAster ; break ;
        default : retour=CodeAbortAster ; break ;
        }

        longjmp( env , retour ) ;

        /* passage INTERDIT */
        _FIN("xfini_") ;
        INTERRUPTION(1);
}
#endif                /* #ifdef _UTILISATION_SETJMP_ */





/* List of functions defined in the module */

static PyMethodDef aster_methods[] = {
                {"init",        aster_init ,              METH_VARARGS},
                {"debut",       aster_debut ,             METH_VARARGS},
                {"poursu",      aster_poursu ,            METH_VARARGS},
                {"oper" ,       aster_oper ,              METH_VARARGS},
                {"opsexe" ,     aster_opsexe ,            METH_VARARGS},
                {"repout" ,     aster_repout ,            METH_VARARGS},
                {"myeval" ,     aster_myeval ,            METH_VARARGS},
                {"argv" ,       aster_argv ,              METH_VARARGS},
                {"getpara" ,    aster_getpara ,           METH_VARARGS},
                {NULL,                NULL}/* sentinel */
};






/* Initialization function for the module (*must* be called initaster) */

DL_EXPORT(void)
initaster()
{
        PyObject *m = (PyObject*)0 ;
        PyObject *d = (PyObject*)0 ;

        _DEBUT(initaster) ;

        /* Create the module and add the functions */
        m = Py_InitModule("aster", aster_methods);

        /* Add some symbolic constants to the module */
        d = PyModule_GetDict(m);
        ErrorObject = PyErr_NewException("aster.error", NULL, NULL);
        PyDict_SetItemString(d, "error", ErrorObject);

        /* Initialisation de la pile d appel des commandes */
        pile_commandes = PyList_New(0);

        _FIN(initaster) ;
}





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
        ASSERT(taille <= longueur ) ;

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
                        FSSCRUTE(chaine,longueur) ;
                }
        }
        return pret ;
}


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

        taille = ( *base ) ? strlen( *base ) : 0 ;

        ajout = ( supplement ) ? strlen( supplement ) : 0 ;

        if ( ajout > 0 ){
                if ( taille > 0 ){
                        total = taille + ajout ;
                        total += 1 ; /* caractere de fin de chaine */
                        resultat = (char*)(malloc(total)) ;
                        ASSERT(resultat!=NULL) ;
                        strcpy(resultat,*base) ;
                        strcat(resultat,supplement) ;
                }
                else{
                        total = ajout ;
                        total += 1 ; /* caractere de fin de chaine */
                        resultat = (char*)(malloc(total)) ;
                        ASSERT(resultat!=NULL) ;
                        strcpy(resultat,supplement) ;
                }
        }
        else{
                if ( taille > 0 ){
                        total = taille  ;
                        total += 1 ; /* caractere de fin de chaine */
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







const char *aster_ident()
{
        const char *identCVS = "$Id: astermodule.c,v 1.59.12.1.2.1 2001/05/16 16:14:54 iliade Exp $ $Name:  $" ;
        return identCVS ;
}





#if defined SOLARIS || IRIX || TRU64 || SOLARIS64 || P_LINUX
#define GETCMC getcmc_
void getcmc_(INTEGER *icmc)
#elif defined HPUX
#define GETCMC getcmc
void getcmc (INTEGER *icmc)
#elif defined PPRO_NT
void __stdcall GETCMC (INTEGER *icmc)
#endif
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
        _DEBUT("getcmc_") ;
        res = PyObject_GetAttrString(commande,"icmd");
        /*
                    Si le retour est NULL : une exception a ete levee dans le code Python appele
                    Cette exception est a transferer normalement a l appelant mais FORTRAN ???
                    On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
                MYABORT("erreur a l appel de getcmc dans la partie Python");

        *icmc = PyInt_AsLong(res);
        ISCRUTE(*icmc) ;
        Py_DECREF(res);
        _FIN("getcmc_") ;
}






#if defined HPUX
void getcmd ( _OUT char *nomres, _OUT char *concep, _OUT char *nomcmd,
              _OUT char *statu, _OUT INTEGER *inum,int lres,int lconc,int lcmd,int lstat)
#elif defined PPRO_NT
void __stdcall GETCMD ( _OUT char *nomres,int lres,_OUT char *concep,int lconc, _OUT char *nomcmd,
                       int lcmd,_OUT char *statu,int lstat, _OUT INTEGER *inum)
#else
void getcmd_( _OUT char *nomres, _OUT char *concep, _OUT char *nomcmd,
              _OUT char *statu, _OUT INTEGER *inum,int lres,int lconc,int lcmd,int lstat)
#endif
{
        /*
          Procedure GETCMD : emule la procedure equivalente ASTER
          Retourne des infos sur la commande courante

          Entrees : RAS

          Sorties :
            le nom du concept produit                 : nomres (string)
            le type du concept produit                : concep (string)
            le nom de la commande en cours            : nomcmd (string)
            le statut du concept produit              : statu (string)
                                   'NOUVEAU' : concept produit nouveau
                                   'MODIFIE' : concept produit modifie
                                   'ERRONE'  : concept produit existe mais pas du bon type
            le numero d'ordre de la commande courante : inum
          Fonction:
            Retourne le numero de la commande courante, le nom de la commande
                le nom du concept produit, son type, le statut du concept

          Commentaires:
            Dans l'ancienne version, "statu" qualifiait la donnee dans la memoire JEVEUX !
        */

        int k=0 ;
        INTEGER ier ;

        _DEBUT("getcmd_") ;

#if defined SOLARIS || HPUX || IRIX || TRU64 || SOLARIS64 || P_LINUX
        GETRES ( nomres , concep , nomcmd , lres ,  lconc , lcmd ) ;
        GETCMC ( inum ) ;
        GCUCON ( inum , nomres , concep , &ier , lres , lconc ) ;
#elif defined PPRO_NT
        GETRES ( nomres , lres , concep ,  lconc , nomcmd , lcmd ) ;
        GETCMC ( inum ) ;
        GCUCON ( inum , nomres , lres , concep , lconc , &ier ) ;
#endif

        ISCRUTE(ier) ;
        switch( ier )
        {
        case 0 :
                {
                        strncpy(statu,"NOUVEAU",min(7,lstat)) ;
                        for(k=7;k<lstat;k++) statu[k] =  ' ' ;
                }
                break ;
        default :
                if ( ier>0 ){
                        strncpy(statu,"MODIFIE",min(7,lstat)) ;
                        for(k=7;k<lstat;k++) statu[k] =  ' ' ;

                }
                else{
                        strncpy(statu,"ERRONE",min(6,lstat)) ;
                        for(k=6;k<lstat;k++) statu[k] =  ' ' ;
                }
                break ;
        }


        FSSCRUTE(nomres,lres) ;
        FSSCRUTE(concep,lconc) ;
        FSSCRUTE(nomcmd,lcmd) ;
        FSSCRUTE(statu,lstat) ;
        ISCRUTE(*inum) ;
        _FIN("getcmd_") ;
        return ;
}
