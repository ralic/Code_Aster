subroutine mecoel(iacti2)
    implicit none
    integer :: iacti2
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!
!    CETTE ROUTINE SERT SURTOUT A CONSERVER DANS LE SOURCE
!    LA DOC DES COMMON DE LA ROUTINE CALCUL
!
!    ELLE SERT EGALEMENT A POSITIONNER IACTIF=0/1
!======================================================================
! ARGUMENTS :
!   IACTI2 : / 1 : ON EST "SOUS" LA ROUTINE CALCUL
!            / 0 : ON N'EST PAS "SOUS" LA ROUTINE CALCUL
!======================================================================
    character(len=16) :: option, nomte, nomtm
    common /cakk01/option,nomte,nomtm
!     CE COMMON EST ECRIT  PAR CALCUL :
!         OPTION : OPTION CALCULEE
!         NOMTE  : TYPE_ELEMENT COURANT
!         NOMTM  : TYPE_MAILLE ASSOCIE AU TYPE_ELEMENT
!
!======================================================================
    integer :: igd, nec, ncmpmx, iachin, iachlo, iichin, ianueq, lprno
    integer :: ilchlo, itypgd
    common /caii01/igd,nec,ncmpmx,iachin,iachlo,iichin,ianueq,lprno,&
     &       ilchlo,itypgd
!
    character(len=8) :: typegd
    common /cakk02/typegd
!     CES COMMONS SONT MIS A JOUR PAR EXTRAI.
!     IGD : NUMERO DE LA GRANDEUR ASSOCIEE AU CHAMP A EXTRAIRE
!     NEC : NOMBRE D'ENTIERS CODES DE IGD
!     NCMPMX: NOMBRE MAX DE CMPS POUR IGD
!     IACHIN: ADRESSE JEVEUX DE CHIN.VALE
!     IACHLO: ADRESSE JEVEUX DE CHLOC//".VALE"  (&&CALCUL.NOMPAR)
!     ILCHLO: ADRESSE JEVEUX DE CHLOC//".EXIS"  (&&CALCUL.NOMPAR)
!     IICHIN: NUMERO DU CHAMP CHIN DANS LA LISTE LCHIN.
!     IANUEQ: ADRESSE DE L'OBJET .NUEQ DU PROF_CHNO ASSOCIE EVENTUELLE
!            -MENT AU CHAMP CHIN. (SI LPRNO=1).
!     LPRNO : 1-> L'OBJET .NUEQ EST A PRENDRE EN COMPTE
!                 (CHAM_NO A PROF_CHNO)
!             0-> L'OBJET .NUEQ N'EST PAS A PRENDRE EN COMPTE
!                 (CHAM_NO A REPRESENTATION CONSTANTE OU AUTRE CHAMP)
!     TYPEGD: TYPE SCALAIRE DE LA GRANDEUR IGD : 'R', 'I', 'K8', ...
!     ITYPGD: TYPE SCALAIRE DE LA GRANDEUR IGD : 1,2,...
!             SERT A EVITER DES COMPARAISONS DE CHAINES
!             LA CONVENTION EST ECRITE DANS EXTRA1
!
!======================================================================
    integer :: iaoptt, lgco, iaopmo, ilopmo, iaopno, ilopno, iaopds
    integer :: iaoppa, npario, nparin, iamloc, ilmloc, iadsgd
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
!     CE COMMON EST INITIALISE PAR DEBCA1
!     CE COMMON EST UTILISE UN PEU PARTOUT
!     IAOPTT : ADRESSE DE L'OBJET DU CATALOGUE : '&CATA.TE.OPTTE'
!     LGCO   : LONGUEUR D'UNE COLONNE DE '&CATA.TE.OPTTE'
!              ( NOMBRE TOTAL D'OPTIONS POSSIBLES DU CATALOGUE)
!     IAOPMO : ADRESSE DE '&CATA.TE.OPTMOD'
!     ILOPMO : ADRESSE DU PT_LONG DE '&CATA.TE.OPTMOD'
!     IAOPNO : ADRESSE DE '&CATA.TE.OPTNOM'
!     ILOPNO : ADRESSE DU PT_LONG DE '&CATA.TE.OPTNOM'
!     IAOPDS : ADRESSE DE '&CATA.OP.DESCOPT(OPT)'
!     IAOPPA : ADRESSE DE '&CATA.OP.OPTPARA(OPT)'
!     NPARIO : LONGUEUR DE '&CATA.OP.OPTPARA(OPT)'
!              = NB_PARAM "IN" + NB_PARAM "OUT"
!     NPARIN : NOMBRE DE PARAMETRES "IN" POUR L'OPTION OPT.
!     CE NOMBRE PERMET DE SAVOIR SI UN PARAMETRE EST "IN" OU "OUT"
!              (IPAR <= NPARIN) <=> (IPAR EST "IN")
!     IAMLOC : ADRESSE DE '&CATA.TE.MODELOC'
!     ILMLOC : ADRESSE DU PT_LONG DE '&CATA.TE.MODELOC'
!     IADSGD : ADRESSE DE '&CATA.GD.DESCRIGD'
!
!======================================================================
    integer :: iamaco, ilmaco, iamsco, ilmsco, ialiel, illiel
    common /caii03/iamaco,ilmaco,iamsco,ilmsco,ialiel,illiel
!     CE COMMON EST INITIALISE PAR DEBCA1
!     CE COMMON EST MIS A JOUR PAR DEBCAL (OU TERLIG)
!     CE COMMON EST UTILISE DANS NUMAIL,EXCHNO,...
!     IAMACO  : ADRESSE DE LA CONNECTIVITE DU MAILLAGE
!     ILMACO  : ADRESSE DU POINTEUR DE LONGUEUR DE IAMACO
!     IAMSCO  : ADRESSE DE LA CONNECTIVITE DES MAILLES SUPPL. D'1 LIGREL
!     ILMSCO  : ADRESSE DU POINTEUR DE LONGUEUR DE IAMSCO
!     IALIEL  : ADRESSE DE L'OBJET '.LIEL' DU LIGREL.
!     ILLIEL  : ADRESSE DU POINTEUR DE LONGUEUR DE '.LIEL'.
!
!======================================================================
    integer :: iachii, iachik, iachix
    common /caii04/iachii,iachik,iachix
!     CE COMMON EST MIS A JOUR PAR DEBCAL
!     CE COMMON EST UTILISE DANS EXTRAI,EXCHNO,EXCART,EXRESL,EXCHML
!                               ,DCHLMX
!     IACHII : ADRESSE DE '&&CALCUL.LCHIN_I'
!     IACHIK : ADRESSE DE '&&CALCUL.LCHIN_K8'
!     IACHIX : ADRESSE DE '&&CALCUL.LCHIN_EXI'
!
!     '&&CALCUL.LCHIN_EXI' ::= V(L)    (DIM = NIN)
!             V(1) :  .FALSE.    : LE CHAMP PARAMETRE N'EXISTE PAS.
!
!     '&&CALCUL.LCHIN_K8'  ::= V(K8)    (DIM = NIN*2)
!             V(1) :  TYPE_CHAMP : 'CHNO','CART','CHML' OU 'RESL'.
!             V(2) :  TYPE_GD    : 'C', 'R', 'I', 'K8', ...
!
!     '&&CALCUL.LCHIN_I'  ::= V(I)     (DIM = NIN*11)
!             V(1) :  IGD   GRANDEUR ASSOCIEE A LCHIN(I)
!             V(2) :  NEC   NOMBRE D'ENTIERS CODES
!             V(3) :  NCMPMX NOMBRE MAX DE CMP POUR IGD
!             V(4) :  IADESC ADRESSE DE .DESC  (OU .CELD)
!             V(5) :  IAVALE
!                     SI CHAM_NO OU CARTE : ADRESSE DE .VALE
!                     SI CHAM_ELEM        : ADRESSE DE .CELV
!             V(6) :  IAPTMA ADRESSE DE .PTMA (POUR 1 CARTE)
!             V(7) :  IAPTMS ADRESSE DE .PTMS (POUR 1 CARTE)
!             V(8) :  IAPRN1 ADRESSE DU PRNO($MAILLA) (POUR 1 CHAM_NO)
!             V(9) :  IAPRN2 ADRESSE DU PRNO(LIGREL)  (POUR 1 CHAM_NO)
!             V(10):  IANUEQ ADRESSE    .NUEQ         (POUR 1 CHAM_NO)
!             V(11):  LPRNO  (DIT SI IANUEQ EST UTILISE POUR 1 CHAM_NO)
!
!======================================================================
    integer :: ianoop, ianote, nbobtr, iaobtr, nbobmx
    common /caii05/ianoop,ianote,nbobtr,iaobtr,nbobmx
!     CE COMMON EST INITIALISE PAR DEBCA1
!     CE COMMON EST MIS A JOUR PAR DEBCAL
!     CE COMMON EST UTILISE DANS TE0000 POUR
!     IANOOP : ADRESSE DANS ZK16 DE '&&CALCUL.NOMOP' V(K16)
!          V(IOP) --> NOM DE L'OPTION IOP
!     IANOTE : ADRESSE DANS ZK16 DE '&&CALCUL.NOMTE' V(K16)
!          V(ITE) --> NOM DU TYPE_ELEMENT ITE
!     CE COMMON EST UTILISE DANS ALCHLO,ALRSLT ET CALCUL POUR :
!          NBOBTR : NOMBRE D'OBJETS DE TRAVAIL '&&CALCUL....' QUI
!                   DEVRONT ETRE DETRUITS A LA FIN DE CALCUL.
!          IAOBTR : ADRESSE DANS ZK24 DE L'OBJET '&&CALCUL.OBJETS_TRAV'
!          NBOBMX : LONGUEUR DE L'OBJET '&&CALCUL.OBJETS_TRAV'
!
!======================================================================
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
!
!
!     NBGR   : NOMBRE DE GREL DU LIGREL
!     IGR    : NUMERO DU GREL COURANT
!     NBELGR : NOMBRE D'ELEMENTS DANS LE GREL IGR
!
!     JCTEAT : ADRESSE DANS ZK16 DE L'OBJET &CATA.TE.CTE_ATTR(NOMTE)
!     LCTEAT : LONGUEUR DE L'OBJET &CATA.TE.CTE_ATTR(NOMTE)
!       REMARQUE : SI NOMTE N'A PAS D'ATTRIBUT : JCTEAT=LCTEAT=0
!
!  IAWLOC : ADRESSE DANS ZI DE '&&CALCUL.IA_CHLOC' V(I)
!           CET OBJET CONTIENT DES INFORMATIONS SUR LES CHAMPS LOCAUX
!   V(3*(IPAR-1)+1) : IACHLO
!      ADRESSE DU CHAMP_LOCAL '&&CALCUL.//NOMPAR(IPAR)
!      =-1 <=> / LE CHAMP "IN" N'EXISTE PAS :
!                 / NOMPAR N'APPARTIENT PAS A LPAIN
!                 / CHIN//'.DESC' (OU .CELD) N'EXISTE PAS
!              / NOMPAR N'APPARTIENT PAS A LPAOUT
!      =-2 <=> AUCUN TYPE_ELEM DU LIGREL NE DECLARE NOMPAR
!
!   V(3*(IPAR-1)+2) : ILCHLO :
!      ADRESSE D'UN VECTEUR DE BOOLEENS ( // CHAMP_LOCAL)
!      DE NOM : '&&CALCUL.//NOMPAR(IPAR)//'.EXIS'
!      SI ILCHLO = -1 :
!          =>  LE CHAMP LOCAL EST "OUT"
!              ET/OU LE CHAMP GLOBAL N'EXISTE PAS
!              ET/OU LE PARAMETRE N'EST PAS UTILISE
!   V(3*(IPAR-1)+3) : ICH : NUMERO DU CHAMP ASSOCIE AU PARAMETRE.
!      I.E : INDICE DANS LCHIN (OU LCHOUT SELON LE CAS)
!      ICH = 0 S'IL N'Y A PAS DE CHAMP ASSOCIE A IPAR
!
!
!  IAWLO2 : ADRESSE DANS ZI DE '&&CALCUL.IA_CHLO2' V(I)
!           CET OBJET CONTIENT DES INFORMATIONS SUR LES CHAMPS LOCAUX
!   V(5*(NBGR*(IPAR-1)+IGR-1)+1):
!      MODE LOCAL POUR (IPAR,IGR)
!   V(5*(NBGR*(IPAR-1)+IGR-1)+2):
!      LONGUEUR DU CHAMP_LOCAL POUR 1 ELEMENT (LUE DANS LE CATALOGUE).
!      CETTE LONGUEUR NE TIENT PAS COMPTE DE NBSPT ET NCDYN.
!      =-1 <=> LE PARAMETRE N'EST PAS UTILISE PAR LE TYPE_ELEMENT
!   V(5*(NBGR*(IPAR-1)+IGR-1)+3):
!      NOMBRE DE POINTS DE DISCRETIS. DU CHAMP_LOCAL
!      0 SI RESUELEM
!   V(5*(NBGR*(IPAR-1)+IGR-1)+4):
!      LONGUEUR DU CHAMP LOCAL POUR LE GREL IGR
!   V(5*(NBGR*(IPAR-1)+IGR-1)+5):
!      ADRESSE DU DEBUT DU GREL DANS LE CHAMP_LOCAL (= 1 SI CALVOI=0)
!
!   IAWTYP : ADRESSE DANS ZK8 DE '&&CALCUL.TYPE_SCA' V(K8)
!          V(IPAR) --> TYPE_SCALAIRE DU CHAMP_LOCAL
!
!======================================================================
    integer :: iachoi, iachok
    common /caii07/iachoi,iachok
!     CE COMMON EST MIS A JOUR PAR ALRSLT
!     CE COMMON EST UTILISE DANS MONTEE ,DCHLMX
!     IACHOI : ADRESSE DE '&&CALCUL.LCHOU_I'
!     IACHOK : ADRESSE DE '&&CALCUL.LCHOU_K8'
!
!     '&&CALCUL.LCHOU_K8'  ::= V(K8)    (DIM = NIN*2)
!             V(1) :  TYPE_CHAMP : 'CHML' OU 'RESL'.
!             V(2) :  TYPE_GD    : 'C', 'R'
!
!     '&&CALCUL.LCHOU_I'  ::= V(I)     (DIM = NOUT*3)
!         -- SI CHML :
!             V(1) :  ADRESSE DE L_CHOUT(I).CELD
!             V(2) :  ADRESSE DE L_CHOUT(I).CELV
!         -- SI RESL :
!             V(1) :  ADRESSE DE L_CHOUT(I).DESC
!             V(2) :  ADRESSE DE L_CHOUT(I).RSVI
!             V(3) :  ADRESSE DE LONCUM DE L_CHOUT(I).RSVI
!
!======================================================================
    integer :: iel
    common /caii08/iel
!     IEL    : NUMERO DE L'ELEMENT COURANT (DANS LE GREL IGR)
!         (IEL EST MIS A JOUR PAR EXTRAI,TE0000,MONTEE,...)
!
!======================================================================
    integer :: nbobj, iainel, ininel
    common /caii09/nbobj,iainel,ininel
!     NBOBJ  : NOMBRE D'OBJETS '&INEL.XXXX' CREE PAR L'INITIALISATION
!              DU TYPE_ELEM
!     ININEL : ADRESSE DANS ZK24 DE L'OBJET '&&CALCUL.NOM_&INEL'
!              QUI CONTIENT LES NOMS DES OBJETS '&INEL.XXXX'
!     IAINEL : ADRESSE DANS ZI DE L'OBJET '&&CALCUL.IAD_&INEL'
!              QUI CONTIENT LES ADRESSES DES OBJETS '&INEL.XXXX'
!     CE COMMON EST INITIALISE PAR DEBCA1
!     CE COMMON EST UTILISE PAR CALCUL ET JEVETE
!
!======================================================================
    integer :: icaeli, icaelk
    common /caii10/icaeli,icaelk
!     CE COMMON EST CREE PAR DEBCA1.
!     IL EST UTILISE PAR TECAEL
!     ICAELK EST L'ADRESSE D'UN VECTEUR DE K24 CONTENANT :
!       V(1) : NOM DU MAILLAGE  (K8)
!       V(2) : NOM DU LIGREL    (K19)
!       V(3) : NOM DE LA MAILLE    (K8)
!       V(3+  1) : NOM DU 1ER NOEUD DE LA MAILLE  (K8)
!       V(3+  I) : ...
!       V(3+NBNO) : NOM DU DER NOEUD DE LA MAILLE  (K8)
!       V(3+NBNO+1) : NOM DU TYPE_ELEMENT (K16)
!       V(3+NBNO+2) : NOM DE L'OPTION     (K16)
!     ICAELI EST L'ADRESSE D'UN VECTEUR DE IS CONTENANT :
!       V(1) : NUMERO DE LA MAILLE
!       V(2) : NOMBRE DE NOEUDS DE LA MAILLE (NBNO)
!       V(2+   1) : NUMERO DU 1ER NOEUD DE LA MAILLE
!       V(2+NBNO) : NUMERO DU DER NOEUD DE LA MAILLE
!       V(2+NBNO +1) : NUMERO DU GREL
!       V(2+NBNO +2) : NUMERO DE L'ELEMENT DANS LE GREL
!
!======================================================================
    integer :: nute, jnbelr, jnoelr, iactif, jpnlfp, jnolfp, nblfpg
    common /caii11/nute,jnbelr,jnoelr,iactif,jpnlfp,jnolfp,nblfpg
!     CE COMMON EST INITIALISE PAR DEBCA1 (JNBELR,JNOELR,JPNLFP,JNOLFP)
!                              ET CALCUL (NUTE,IACTIF).
!     NUTE : NUMERO DU TYPE_ELEM NOMTE.
!     JNBELR : ADRESSE DANS ZI  DE '&CATA.TE.NBELREFE'
!     JNOELR : ADRESSE DANS ZK8 DE '&CATA.TE.NOELREFE'
!     IACTIF :  1 : LA ROUTINE CALCUL EST ACTIVE
!               0 : LA ROUTINE CALCUL EST INACTIVE.
!               2 : LA ROUTINE OP0033 EST ACTIVE.
!         CE "BOOLEEN" PERMET D'ARRETER LES UTILISATIONS DES
!         ROUTINES QUI DOIVENT ETRE APPELLEES "SOUS" LES TE00IJ:
!         JEVECH,TECACH,TECAEL,ELREF1,...
!     JPNLFP : ADRESSE DANS ZK32 DE '&CATA.TE.PNLOCFPG'
!     JNOLFP : ADRESSE DANS ZI DE '&CATA.TE.NOLOCFPG'
!     NBLFPG : DIMENSION DES OBJETS PNLOCFPG ET NOLOCFPG
!======================================================================
    integer :: caindz(512), capoiz
    common /caii12/caindz,capoiz
!     CE COMMON EST UTILISE POUR GAGNER DU TEMPS DANS JEVECH ET TECACH
!======================================================================
    integer :: nbsav
    common /caii13/nbsav
!     CE COMMON EST INITIALISE PAR DEBCA1
!     CE COMMON EST UTILISE POUR GAGNER DU TEMPS DANS ELREF6
!======================================================================
    integer :: nbcvrc, jvcnom
    common /caii14/nbcvrc,jvcnom
!     CE COMMON EST UTILISE POUR LES VARIABLES DE COMMANDE :
!       ROUTINES : RCMFMC,RCVARC,RCVALB
!     NBCVRC : NOMBRE DE CVRC (VARIABLE DE COMMANDE SCALAIRE)
!     JVCNOM : ADRESSE DANS ZK8 DES NOMS DES CVRC
!======================================================================
    integer :: nfpgmx
    parameter (nfpgmx=10)
    integer :: nfpg, jfpgl, decala(nfpgmx), km, kp, kr, iredec
    common /caii17/nfpg,jfpgl,decala,km,kp,kr,iredec
!     CE COMMON DECRIT LES FAMILLES DE PG DE LA FAMILLE "MATER"
!       ROUTINES : VRCDEC, RCVARC, REDECE
!     NFPG   : NOMBRE DE FAMILLES DE LA FAMILLE LISTE "MATER"
!     JFPGL  : ADRESSE DANS ZK8 DE LA LISTE DES NOMS DES FAMILLES
!     DECALA : TABLEAU DES DECALAGE DES NUMEROS DES PG :
!         DECALA(1) : 0
!         DECALA(K) : NOMBRE CUMULE DES PG DES FAMILLES (1:K-1)
!     KM,KP,KR : SAUVEGARDE DES NUMEROS D'ELEMENTS UTILISES DANS
!                RCVARC. ILS EVITENT DES APPELS COUTEUX A TECACH.
!     IREDEC : 0 : ON N'EST PAS "SOUS" REDECE.F
!              1 : ON EST  "SOUS" REDECE.F  (VOIR CARR01 CI-DESSOUS)
!======================================================================
    real(kind=8) :: timed1, timef1, td1, tf1
    common /carr01/timed1,timef1,td1,tf1
!     CE COMMON SERT POUR LE REDECOUPAGE EVENTUEL DU PAS DE TEMPS
!     PAR REDECE.F
!     IL EST UTILISE PAR LES ROUTINES :  REDECE ET RCVARC
!     ATTENTION : IL N'EST PAS VALABLE SI L'ON N'EST PAS "SOUS" REDECE
!     TIMED1  : VALEUR DE L'INSTANT "-" DU "GROS" PAS DE TEMPS
!     TIMEF1  : VALEUR DE L'INSTANT "+" DU "GROS" PAS DE TEMPS
!     TD      : VALEUR DE L'INSTANT "-" DU "PETIT" PAS DE TEMPS
!     TF      : VALEUR DE L'INSTANT "+" DU "PETIT" PAS DE TEMPS
!======================================================================
    integer :: evfini, calvoi, jrepe, jptvoi, jelvoi
    common /caii19/evfini,calvoi,jrepe,jptvoi,jelvoi
!     CE COMMON EST INITIALISE PAR DEBCA1
!
!     EVFINI  = 1 : LE LIGREL CONTIENT DES "VOLUMES FINIS"
!     EVFINI  = 0 : SINON
!     CALVOI  = 1 : DANS UNE ROUTINE TE00IJ, ON VEUT POUVOIR ACCEDER
!                   AUX CHAMPS DES ELEMENTS VOISINS (TECAC2.F)
!     CALVOI  = 0 : SINON
!
!     LES 3 ADRESSES SUIVANTES SONT REMPLIES SI CALVOI=1 OU EVFINI=1 :
!      * JREPE   : ADRESSE JEVEUX DE LIGREL.REPE
!      * JPTVOI  : ADRESSE JEVEUX DE MAILLAGE.VGE.PTVOIS
!      * JELVOI  : ADRESSE JEVEUX DE MAILLAGE.VGE.ELVOIS
!
    iactif=iacti2
!
end subroutine
