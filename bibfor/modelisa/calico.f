      SUBROUTINE CALICO(CHARZ,NOMAZ,NOMOZ,NDIM,MOTFAZ)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 07/01/2003   AUTEUR PABHHHH N.TARDIEU 
C TOLE CRP_20
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT NONE
      INTEGER NDIM
      CHARACTER*(*) CHARZ,NOMAZ,NOMOZ,MOTFAZ
C ======================================================================
C ROUTINE APPELEE PAR : CHARME -----------------------------------------
C ======================================================================
C TRAITEMENT DU MOT-FACTEUR 'CONTACT' DANS AFFE_CHAR_MECA.
C LES RESULTATS SONT STOCKES DANS LA SD CHAR(1:8)//'.CONTACT'//
C '.METHCO'  : CARACTERISTIQUES DE LA METHODE                  (METHCO)
C '.CHAMCO'  : CHAMP AFFECTE PAR LA RELATION UNILATERALE       (CHAMCO)
C '.COEFCO'  : COEFFICIENT POUR CHAQUE ZONE                    (COEFCO)
C '.DIRCO'   : DIRECTION FIXE D'APPARIEMENT PAR ZONE           (DIRCO)
C '.PZONECO' : POINTEUR ASSOCIE AUX ZONES DE CONTACT           (PZONE)
C '.MAILCO'  : LISTE DES MAILLES DES DIFFERENTES SURFACES      (CONTMA)
C '.PSUMACO' : POINTEUR ASSOCIE                                (PSURMA)
C '.NOEUCO'  : LISTE DES NOEUDS DES DIFFERENTES SURFACES       (CONTNO)
C '.PSUNOCO' : POINTEUR ASSOCIE                                (PSURNO)
C '.SSNOCO'  : LISTE DES NOEUDS INTERDITS COMME ESCLAVES       (SANSNO)
C '.PSSNOCO' : POINTEUR ASSOCIE (ZONE PAR ZONE)                (PSANS)
C '.MANOCO'  : LISTE DES MAILLES CONTENANT UN NOEUD DE CONTACT (MANOCO)
C '.PMANOCO' : POINTEUR ASSOCIE                                (PMANO)
C '.NOMACO'  : LISTE DES NOEUDS DES MAILLES DE CONTACT         (NOMACO)
C '.PNOMACO' : POINTEUR ASSOCIE                                (PNOMA)
C '.MAMACO'  : INDICE DES MAILLES ADJACENTES A UNE MAILLE      (MAMACO)
C '.PMAMACO' : POINTEUR ASSOCIE                                (PMAMA)
C '.NORMCO'  : COMPOSANTES DES NORMALES AUX NOEUDS DE CONTACT  (NORMCO)
C '.NDIMCO'  : LISTE D'ENTIERS UTILES (DIMENSION DES VECTEURS) (NDIMCO)
C '.JSUPCO'  : JEU FICTIF (A SOUSTRAIRE AU VRAI JEU)           (JEUSUP)
C '.JFO1CO'  : JEU FICTIF FONCTION (A SOUSTRAIRE AU VRAI JEU)  (JEUFO1)
C '.JFO2CO'  : JEU FICTIF FONCTION (A SOUSTRAIRE AU VRAI JEU)  (JEUFO2)
C '.NOZOCO'  : NUMERO DE LA ZONE POUR CHAQUE NOEUD DE CONTACT  (NOZOCO)
C ======================================================================
C VOIR LE DOCUMENT D4.06.14 POUR TOUS LES DETAILS. ---------------------
C ======================================================================
C IN  CHARZ  K24  : NOM UTILISATEUR DU CONCEPT DE CHARGE
C IN  NOMOZ  K24  : NOM DU MODELE
C IN  MOTFAC K16  : MOT CLE FACTEUR A TRAITER
C ======================================================================
C ------------------------- VARIABLES LOCALES --------------------------
C ======================================================================
C NZOCO  : NOMBRE DE ZONES DE CONTACT (OCCURENCES DU MOT-CLE CONTACT)
C NSUCO  : NOMBRE TOTAL DE SURFACES DE CONTACT
C NMACO  : NOMBRE TOTAL DE MAILLES DES SURFACES
C NNOCO  : NOMBRE TOTAL DE NOEUDS DES SURFACES
C METHCO : TABLEAU DONNANT LES CARACTERISTIQUES DE LA METHODE
C          DE TRAITEMENT DU CONTACT UNILATERAL
C       METHCO (1) = NOMBRE DE ZONES DE CONTACT NZOCO
C       POUR LA ZONE N :
C       METHCO(1+9*(N-1)+1) = TYPE D'APPARIEMENT
C         - -1 SI 'NON'
C         -  0 SI 'NODAL' OU 'NODAL_SYME'
C         -  1 SI 'MAIT_ESCL' OU 'MAIT_ESCL_SYME'
C         -  2 SI 'TERRITOIRE'
C         -  3 SI 'HIERARCHIQUE'
C         -  4 SI NODAL ET DIRECTION FIXEE DANS LE FICHIER DE COMMANDES
C       METHCO(1+9*(N-1)+2) = DEFINITION D UNE DIRECTION TANGENTE
C                            (POUR LE FROTTEMENT)
C         - 0 SI NON DEFINIE
C         - 1 SI DEFINIE
C       METHCO(1+9*(N-1)+3) = ORDRE MAITRE-ESCLAVE
C         - 1 SI UTILISE     (APPARIEMENT SYMETRISE)
C         - 0 SI PAS UTILISE (APPARIEMENT NON SYMETRISE)
C       METHCO(1+9*(N-1)+4) = TYPE DE PROJECTION
C         - 1 SI LINEAIRE
C         - 2 SI QUADRATIQUE
C       METHCO(1+9*(N-1)+5) = TYPE DE REACTUALISATION
C         - +   1 SI PAR DOUBLE BOUCLE SYSTEMATIQUE
C         - +/- 2 SI PAR VOISINAGE
C         - +/- 3 SI PAR BOITE
C         - > 0 SI ON PASSE D'ABORD PAR LE NOEUD LE PLUS PROCHE
C         - < 0 SI ON CHERCHE DIRECTEMENT LA MAILLE LA PLUS PROCHE
C       METHCO(1+9*(N-1)+6) = ALGORITHME UTILISE
C         - -1    PENALISATION    CONTACT                    2D/3D
C         -  0    CONTRAINTE      CONTACT                    2D/3D
C         -  1    LAGRANGIEN      CONTACT                    2D/3D
C         -  2    LAGRANGIEN      CONTACT ET FROTTEMENT      2D
C         -  3    PENALISATION    CONTACT ET FROTTEMENT      2D/3D
C         -  4    LAGRANGIEN      CONTACT ET FROTTEMENT      3D
C         -  5    PENALISATION    SEULEMENT SUR FROTTEMENT   2D/3D
C         -  6    CONTINUE        CONTACT ET FROTTEMENT      2D/3D
C       METHCO(1+9*(N-1)+7) = FREQUENCE DE REACTUALISATION GEOMETRIQUE
C         -  0    AUCUNE
C         -  1    1 PAR PAS DE CHARGE
C         -  2    2 PAR PAS DE CHARGE
C       METHCO(1+9*(N-1)+8) = NORMALE UTILISEE
C         -  0    NORMALE MAITRE
C         -  1    MOYENNE NORMALE MAITRE+ESCLAVE
C       METHCO(1+9*(N-1)+9) = STOP SI MATRICE DE CONTACT SINGULIERE
C         -  0    OUI
C         -  1    NON
C CHAMCO : CHAMP IMPACTE PAR LA CONDITION UNILATERALE POUR CHAQUE ZONE
C          (DIM NZOCO)
C         - +/-1 : DEPLACEMENT
C         -   -2 : PRESSION (ELEMENTS THM UNIQUEMENT)
C         -   -3 : TEMPERATURE (ELEMENTS THM UNIQUEMENT)
C NORLI : UTILISATION DU LISSAGE DES NORMALES OU NON
C         -   0 : PAS DE LISSAGE
C         -   1 : LISSAGE
C COEFCO : COEFFICIENT MULTIPLICATEUR DE LA CONDITION UNILATERALE
C          POUR LA PRESSION OU LA TEMPERATURE (DIM NZOCO)
C DIRCO  : DANS LE CAS D'UN APPARIEMENT NODAL DANS UNE DIRECTION FIXE
C          (PRESENCE DU MOT-CLE FACTEUR DIRECTION), CONTIENT POUR
C          CHAQUE ZONE LES 3 COMPOSANTES DEFINISSANT LA DIRECTION
C          UTILISEE POUR APPARIEMENT ET ECRITURE DE LA RELATION DE
C          NON PENETRATION.
C PZONE  : POINTEUR DES ZONES DE CONTACT (DIM NZOCO+1)
C CONTMA : LISTE DES MAILLES DE CONTACT (DIM NMACO)
C PSURMA : POINTEUR ASSOCIE (DELIMITANT LES SURFACES) (DIM NSUCO+1)
C CONTNO : LISTE DES NOEUDS DE CONTACT (DIM NNOCO)
C PSURNO : POINTEUR ASSOCIE (DELIMITANT LES SURFACES) (DIM NSUCO+1)
C SANSNO : LISTE DES NOEUDS INTERDITS COMME ESCLAVES (MOTS-CLES
C          SANS_NOEUD ET SANS_GROUP_NO)
C PSANS  : POINTEUR ASSOCIE (DIM NZOCO+1)
C MANOCO : TABLEAU DONNANT POUR CHAQUE NOEUD DE CONTACT LA LISTE DES
C          MAILLES DE CONTACT DE LA MEME SURFACE LE CONTENANT
C PMANO  : POINTEUR DE CE TABLEAU (DIM NNOCO+1)
C NMANO  : DIMENSION DE MANOCO
C NOMACO : TABLEAU DONNANT POUR CHAQUE MAILLE DE CONTACT LA LISTE DES
C          NOEUDS DE CONTACT DE LA MEME SURFACE QU'ELLE CONTIENT
C PNOMA  : POINTEUR DE CE TABLEAU (DIM NMACO+1)
C NNOMA  : DIMENSION DE NOMACO
C NORMCO : COMPOSANTES DES NORMALES INITIALES AUX NOEUDS DE CONTACT
C MAMACO : TABLEAU DONNANT POUR CHAQUE MAILLE DE CONTACT LA LISTE DES
C          MAILLES DE CONTACT DE LA MEME SURFACE ADJACENTES
C          (INDICE DANS CONTMA, PAS LE NUMERO DE MAILLE ABSOLU)
C PMAMA  : POINTEUR DE CE TABLEAU (DIM NMACO+1)
C NMAMA  : DIMENSION DE MAMACO
C NDIMCO : (NDIM NZOCO NSUCO NMACO NNOCO NMANO NNOMA NMAMA NESMAX ...)
C          OU NDIM   = DIM DE L'ESPACE (2 OU 3)
C          ET NESMAX = NOMBRE MAXIMAL DE NOEUDS ESCLAVES
C JEUSUP : TABLEAU DONNANT ZONE PAR ZONE LE JEU FICTIF R8 (A SOUSTRAIRE
C          AU VRAI JEU : SOMME DES JEUX DONNES PAR L'UTILISATEUR SOUS
C          DIST_1 ET DIST_2), ZONE PAR ZONE (DIM NZOCO).
C JEUFO1 : TABLEAU DE FONCTIONS DONNANT ZONE PAR ZONE LE JEU FICTIF (A
C          SOUSTRAIRE AU VRAI JEU) DONNE PAR L'UTILISATEUR SOUS LE MOT-
C          CLE DIST_1 DANS AFFE_CHAR_MECA_F POUR CHAQUE ZONE (DIM NZOCO)
C JEUFO2 : TABLEAU DE FONCTIONS DONNANT ZONE PAR ZONE LE JEU FICTIF (A
C          SOUSTRAIRE AU VRAI JEU) DONNE PAR L'UTILISATEUR SOUS LE MOT-
C          CLE DIST_2 DANS AFFE_CHAR_MECA_F POUR CHAQUE ZONE (DIM NZOCO)
C NOZOCO : TABLEAU DONNANT LE NUMERO DE ZONE POUR CHAQUE NOEUD DE
C          CONTACT (DIM NNOCO)
C CARACF : DIVERSES INFORMATIONS POUR LES METHODES
C          "CONTINUE" (ECP),"LAGRANGIEN" (LAG) ET "PENALISATION" (PENA)
C   CARACF (1) = NOMBRE DE ZONES DE CONTACT NZOCO (ECP)
C   POUR LA ZONE N :
C   CARACF(1+6*(N-1)+1) = TYPE D'INTEGRATION (ECP)
C     -  1. SI INTEGRATION AU NOEUD
C     -  2. SI INTEGRATION AU POINT DE GAUSS
C   CARACF(1+6*(N-1)+2) = COEF. DE REGULARISATION DU CONTACT (ECP,PENA)
C   CARACF(1+6*(N-1)+3) = COEF. DE REGULARISATION DU FROTMT (ECP,PENA)
C   CARACF(1+6*(N-1)+4) = COEF. DE  FROTTEMENT DE COULOMB (ECP,LAG,PENA)
C   CARACF(1+6*(N-1)+5) = INDICATEUR DE PRESENCE DE FROTTEMENT (ECP)
C     -  1. SI PAS DE FROTTEMENT
C     -  3. SI PRESENCE DE FROTTEMENT
C   CARACF(1+6*(N-1)+6) = VALEUR DE COEF_MATR_FROT (LAG,PENA)
C ECPDON : INFORMATIONS PROPRES A LA METHODE CONTINUE (CONTACT ECP)
C   ECPDON (1) = INDICATEUR SI ON UTILISE CETTE METHODE
C     -  0 SI NON
C     -  1 SI OUI
C   POUR LA ZONE N :
C   ECPDON(1+5*(N-1)+1) = MODELE AXIS?
C     -  0 SI NON
C     -  1 SI OUI
C   ECPDON(1+5*(N-1)+2) = NB MAX D'ITERATION DE CONTACT
C   ECPDON(1+5*(N-1)+3) = NB MAX D'ITERATION DE FROTTEMENT
C   ECPDON(1+5*(N-1)+4) = NB MAX D'ITERATION DE FROTTEMENT
C   ECPDON(1+5*(N-1)+5) = NB MAX D'ITERATION DE SEUIL
C ======================================================================
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C ======================================================================
      CHARACTER*32 JEXNUM,JEXNOM
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C ======================================================================
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C ======================================================================
      INTEGER NZOCO,NSUCO,NMACO,NNOCO,NMANO,NNOMA,NMAMA,NESMAX
      INTEGER NBMA,NBNO,NTRAV,IOC,NOC,JDIM,NSURF,INDMTH,IBID,IER
      INTEGER NBNO1,NBNO2,I1,I2,NESM,JCHAM,JCOEF,JDIR,NBMA1
      INTEGER JMETH,JZONE,JSUMA,JSUNO,JMACO,JNOCO,JDECMA,JDECNO
      INTEGER JJSUP,JJFO1,JJFO2,JTANG,IFRO,IPENA,ICOMA,JCMCF,JECPD
      INTEGER REACCA,REACBS,REACBG,REACSI,JNOESC,JTGDEF,NOCC,NOCN
      INTEGER NNOQUA,JNOQUA,JNOQU,NBNOQU,JDECNQ,JNORLI,NBREAC
      REAL*8 DIST1,DIST2,COEF,SEUIL,DIR(3),DIR1(3)
      REAL*8 COEFRO,COEFPN,COEFPT,COEFTE
      REAL*8 COCAU,COFAU
      CHARACTER*8 K8BID,CHAR,NOMA,JEUF1,JEUF2,NOMO,TYMOCL(2),REAC,ISTO
      CHARACTER*16 MOTFAC,APPAR,PROJ,RECH,CHAM,K16BID,NOMCMD,TYPF,INTER
      CHARACTER*16 MODAX,TYPM,TYPN,PHENOM,MODELI,MOTCLE(2)
      CHARACTER*24 METHCO,CONTMA,CONTNO,PSURMA,PSURNO,PZONE,SANSNO,PSANS
      CHARACTER*24 MANOCO,PMANO,NOMACO,PNOMA,NORMCO,MAMACO,PMAMA,NDIMCO
      CHARACTER*24 JEUSUP,JEUFO1,JEUFO2,CHAMCO,COEFCO,NOZOCO,DIRCO
      CHARACTER*24 CARACF,ECPDON,TANGCO,FROTE,PENAL,COMAFO,DEFICO,TANDEF
      CHARACTER*24 NOESCL,PNOQUA,CONOQU,NORLIS,LISMA
C ======================================================================
C --- INITIALISATION ---------------------------------------------------
C ======================================================================
      CALL JEMARQ()
C ======================================================================
      CALL GETRES(K8BID,K16BID,NOMCMD)
      CHAR = CHARZ
      NOMA = NOMAZ
      NOMO = NOMOZ
      MOTFAC = MOTFAZ
C ======================================================================
C --- RECUPERATION DU NOM DU PHENOMENE ET DE LA  MODELISATION          -
C ======================================================================
      CALL DISMOI('F','PHENOMENE',NOMO,'MODELE',IBID,PHENOM,IER)
      CALL DISMOI('F','MODELISATION',NOMO,'MODELE',IBID,MODELI,IER)
C ======================================================================
C --- RECUPERATION DU NOMBRE DE ZONES DE CONTACT (NOMBRE D'OCCURENCES) -
C ======================================================================
      NZOCO = 0
      IF (MOTFAC.EQ.'CONTACT') THEN
        CALL GETFAC(MOTFAC,NZOCO)
      ELSE
        CALL UTMESS('F','CALICO_00',' MOT CLE FACTEUR INCONNU :'//
     &              MOTFAC)
      END IF
      IF (NZOCO.EQ.0) GO TO 40
C ======================================================================
C --- PREMIERE PASSE ---------------------------------------------------
C --- DETERMINATION DU NOMBRE TOTAL DE SURFACES, DE MAILLES ET DE NOEUDS
C --- REMPLISSAGE DES POINTEURS PZONE, PSURMA ET PSURNO ----------------
C ======================================================================
      PZONE = CHAR(1:8)//'.CONTACT.PZONECO'
      PSURMA = CHAR(1:8)//'.CONTACT.PSUMACO'
      PSURNO = CHAR(1:8)//'.CONTACT.PSUNOCO'
      PNOQUA = CHAR(1:8)//'.CONTACT.PNOEUQU'
      CALL POINCO(CHAR,MOTFAC,NOMA,NZOCO,NSUCO,NMACO,NNOCO,NNOQUA,PZONE,
     &            PSURMA,PSURNO,PNOQUA,NTRAV)
      CALL JEVEUO(PZONE,'L',JZONE)
      CALL JEVEUO(PSURMA,'L',JSUMA)
      CALL JEVEUO(PSURNO,'L',JSUNO)
      CALL JEVEUO(PNOQUA,'L',JNOQUA)
C ======================================================================
C --- SECONDE PASSE ----------------------------------------------------
C --- REMPLISSAGE DES TABLEAUX METHCO, JEUSUP, CONTMA ET CONTNO --------
C ======================================================================
      METHCO = CHAR(1:8)//'.CONTACT.METHCO'
      CONTMA = CHAR(1:8)//'.CONTACT.MAILCO'
      CONTNO = CHAR(1:8)//'.CONTACT.NOEUCO'
      JEUSUP = CHAR(1:8)//'.CONTACT.JSUPCO'
      JEUFO1 = CHAR(1:8)//'.CONTACT.JFO1CO'
      JEUFO2 = CHAR(1:8)//'.CONTACT.JFO2CO'
      CHAMCO = CHAR(1:8)//'.CONTACT.CHAMCO'
      COEFCO = CHAR(1:8)//'.CONTACT.COEFCO'
      DIRCO = CHAR(1:8)//'.CONTACT.DIRCO'
      FROTE = CHAR(1:8)//'.CONTACT.FROTE'
      PENAL = CHAR(1:8)//'.CONTACT.PENAL'
      COMAFO = CHAR(1:8)//'.CONTACT.COMAFO'
      CARACF = CHAR(1:8)//'.CONTACT.CARACF'
      ECPDON = CHAR(1:8)//'.CONTACT.ECPDON'
      TANDEF = CHAR(1:8)//'.CONTACT.TANDEF'
      NOESCL = CHAR(1:8)//'.CONTACT.NOESCL'
      CONOQU = CHAR(1:8)//'.CONTACT.NOEUQU'
      NORLIS = CHAR(1:8)//'.CONTACT.NORLIS'
C ======================================================================
      CALL WKVECT(METHCO,'G V I',9*NZOCO+1,JMETH)
      CALL WKVECT(CONTMA,'G V I',NMACO,JMACO)
      CALL WKVECT(CONTNO,'G V I',NNOCO,JNOCO)
      CALL WKVECT(JEUSUP,'G V R',NZOCO,JJSUP)
      CALL WKVECT(JEUFO1,'G V K8',NZOCO,JJFO1)
      CALL WKVECT(JEUFO2,'G V K8',NZOCO,JJFO2)
      CALL WKVECT(CHAMCO,'G V I',NZOCO,JCHAM)
      CALL WKVECT(COEFCO,'G V R',NZOCO,JCOEF)
      CALL WKVECT(DIRCO,'G V R',3*NZOCO,JDIR)
      CALL WKVECT(CARACF,'G V R',6*NZOCO+1,JCMCF)
      CALL WKVECT(ECPDON,'G V I',5*NZOCO+1,JECPD)
      CALL WKVECT(NORLIS,'G V I',NZOCO+1,JNORLI)
      CALL WKVECT(TANDEF,'G V R',3*NZOCO,JTGDEF)
      IF (NNOQUA.NE.0) THEN
        CALL WKVECT(CONOQU,'G V I',3*NNOQUA,JNOQU)
      END IF
C ======================================================================
      ZI(JMETH) = NZOCO
      ZR(JCMCF) = NZOCO
      ZI(JECPD) = 0
      INDMTH = 1
C ======================================================================
C --- RECUPERATION DES CARACTERISTIQUES DE LA ZONE IOC -----------------
C ======================================================================
      DO 10 IOC = 1,NZOCO
C ======================================================================
C --- RECUPERATION DU TYPE D'APPARIEMENT -------------------------------
C ======================================================================
        CALL GETVTX(MOTFAC,'APPARIEMENT',IOC,1,1,APPAR,NOC)
        IF (APPAR(1:3).EQ.'NON') ZI(JMETH+9* (IOC-1)+1) = -1
        IF (APPAR(1:5).EQ.'NODAL') ZI(JMETH+9* (IOC-1)+1) = 0
        IF (APPAR(1:9).EQ.'MAIT_ESCL') ZI(JMETH+9* (IOC-1)+1) = 1
        IF (APPAR(1:10).EQ.'TERRITOIRE') ZI(JMETH+9* (IOC-1)+1) = 2
        IF (APPAR(1:12).EQ.'HIERARCHIQUE') ZI(JMETH+9* (IOC-1)+1) = 3
C ======================================================================
C --- APPARIEMENT SYMETRIQUE ? -----------------------------------------
C ======================================================================
        ZI(JMETH+9* (IOC-1)+3) = 0
        IF (APPAR.EQ.'NODAL_SYME') ZI(JMETH+9* (IOC-1)+3) = 1
        IF (APPAR.EQ.'MAIT_ESCL_SYME') ZI(JMETH+9* (IOC-1)+3) = 1
C ======================================================================
C --- RECUPERATION DU TYPE DE PROJECTION -------------------------------
C ======================================================================
        CALL GETVTX(MOTFAC,'PROJECTION',IOC,1,1,PROJ,NOC)
        ZI(JMETH+9* (IOC-1)+4) = 1
        IF (PROJ.EQ.'QUADRATIQUE') ZI(JMETH+9* (IOC-1)+4) = 2
C ======================================================================
C --- RECUPERATION DE L'ENVIRONNEMENT DE RECHERCHE ---------------------
C ======================================================================
        CALL GETVTX(MOTFAC,'RECHERCHE',IOC,1,1,RECH,NOC)
        ZI(JMETH+9* (IOC-1)+5) = 1
        IF (APPAR.EQ.'NON') ZI(JMETH+9* (IOC-1)+5) = 0
        IF (RECH(1:12).EQ.'NOEUD_BOUCLE') ZI(JMETH+9* (IOC-1)+5) = 1
        IF (RECH(1:11).EQ.'NOEUD_VOISIN') ZI(JMETH+9* (IOC-1)+5) = 2
        IF (RECH(1:12).EQ.'MAILLE_VOISIN') ZI(JMETH+9* (IOC-1)+5) = -2
        IF (RECH(1:11).EQ.'NOEUD_BOITE') ZI(JMETH+9* (IOC-1)+5) = 3
        IF (RECH(1:12).EQ.'MAILLE_BOITE') ZI(JMETH+9* (IOC-1)+5) = -3
C ======================================================================
C --- RECUPERATION DE LA METHODE ---------------------------------------
C ======================================================================
        CALL GETVTX(MOTFAC,'METHODE',IOC,1,1,TYPM,NOC)
        IF (TYPM(1:8).EQ.'PENALISA') THEN
          ZI(JMETH+9* (IOC-1)+6) = -1
          CALL GETVR8(MOTFAC,'E_N',1,1,1,COEFPN,NOCN)
          ZR(JCMCF+6* (IOC-1)+2) = COEFPN
          INDMTH = 0
        ELSE IF (TYPM(1:8).EQ.'LAGRANGI') THEN
          ZI(JMETH+9* (IOC-1)+6) = 1
          INDMTH = 0
        ELSE IF (TYPM(1:8).EQ.'CONTRAIN') THEN
          ZI(JMETH+9* (IOC-1)+6) = 0
          INDMTH = 0
        ELSE IF (TYPM(1:8).EQ.'CONTINUE') THEN
          ZI(JMETH+9* (IOC-1)+6) = 6
          ZI(JECPD) = 1
        ELSE
          CALL UTMESS('F','CALICO',
     &            'NE CORRESPOND A AUCUNE METHODE DE CONTACT-FROTTEMENT'
     &                )
        END IF
C ======================================================================
C --- MOT-CLES ASSOCIES A LA METHODE PENALISEE -------------------------
C --- MOT-CLES ASSOCIES A LA METHODE LAGRANGIENNE ----------------------
C --- MOT-CLES ASSOCIES A LA METHODE DES CONTRAINTES ACTIVES -----------
C ======================================================================
        IF (TYPM(1:8).EQ.'CONTRAIN' .OR. TYPM(1:8).EQ.'LAGRANGI' .OR.
     &      TYPM(1:8).EQ.'PENALISA') THEN
C ======================================================================
C --- REACTUALISATION GEOMETRIQUE --------------------------------------
C ======================================================================
          CALL GETVTX(MOTFAC,'REAC_GEOM',IOC,1,1,REAC,NOC)
          IF (REAC.EQ.'SANS') THEN
             ZI(JMETH+9*(IOC-1)+7) =  0
          ELSE IF (REAC.EQ.'AUTOMATI') THEN
             ZI(JMETH+9*(IOC-1)+7) = -1
          ELSE IF (REAC.EQ.'CONTROLE') THEN
             CALL GETVIS(MOTFAC,'NB_REAC_GEOM',IOC,1,1,NBREAC,NOC)
             ZI(JMETH+9*(IOC-1)+7) = NBREAC
          ENDIF
C ======================================================================
C --- RECUPERATION DU JEU SUPPLEMENTAIRE MECANIQUE POUR LA ZONE IOC ----
C --- LE JEU TOTAL SERA JEU - JEUSUP (SOIT : D - DIST1 - DIST2) --------
C ======================================================================
          DIST1 = 0.D0
          DIST2 = 0.D0
          ZR(JJSUP+IOC-1) = 0.D0
          ZK8(JJFO1+IOC-1) = ' '
          ZK8(JJFO2+IOC-1) = ' '
C ======================================================================
C --- CAS D'UN JEU SUPPLEMENTAIRE REEL (AFFE_CHAR_MECA) ----------------
C ======================================================================
          IF (NOMCMD.EQ.'AFFE_CHAR_MECA') THEN
            CALL GETVR8(MOTFAC,'DIST_1',IOC,1,1,DIST1,NOC)
            CALL GETVR8(MOTFAC,'DIST_2',IOC,1,1,DIST2,NOC)
            ZR(JJSUP+IOC-1) = DIST1 + DIST2
          END IF
C ======================================================================
C --- CAS D'UN JEU SUPPLEMENTAIRE FONCTION (AFFE_CHAR_MECA_F) ----------
C ======================================================================
          IF (NOMCMD.EQ.'AFFE_CHAR_MECA_F') THEN
            CALL GETVID(MOTFAC,'DIST_1',IOC,1,1,JEUF1,NOC)
            IF (NOC.NE.0) ZK8(JJFO1+IOC-1) = JEUF1
            CALL GETVID(MOTFAC,'DIST_2',IOC,1,1,JEUF2,NOC)
            IF (NOC.NE.0) ZK8(JJFO2+IOC-1) = JEUF2
          END IF
C ======================================================================
C --- ARRET OU PAS SI MATRICE DE CONTACT SINGULIERE --------------------
C ======================================================================
          CALL GETVTX(MOTFAC,'STOP_SINGULIER',IOC,1,1,ISTO,NOC)
          IF (ISTO.EQ.'OUI') THEN
             ZI(JMETH+9*(IOC-1)+9) =  0
          ELSE 
             ZI(JMETH+9*(IOC-1)+9) =  1
             ZI(JMETH+9) =  1
          ENDIF
C ======================================================================
C --- MOT-CLES ASSOCIES A LA METHODE CONTINUE --------------------------
C ======================================================================
        ELSE IF (TYPM(1:8).EQ.'CONTINUE') THEN
          CALL GETVTX(MOTFAC,'INTEGRATION',IOC,1,1,INTER,NOC)
          IF (INTER(1:5).EQ.'NOEUD') ZR(JCMCF+6* (IOC-1)+1) = 1.D0
          IF (INTER(1:5).EQ.'GAUSS') ZR(JCMCF+6* (IOC-1)+1) = 2.D0
          IF (INTER(1:7).EQ.'SIMPSON')  ZR(JCMCF+6* (IOC-1)+1) = 3.D0
          IF (INTER(1:8).EQ.'SIMPSON1') ZR(JCMCF+6* (IOC-1)+1) = 4.D0
          IF (INTER(1:8).EQ.'SIMPSON2') ZR(JCMCF+6* (IOC-1)+1) = 5.D0
          CALL GETVR8(MOTFAC,'COEF_REGU_CONT',IOC,1,1,COCAU,NOC)
          ZR(JCMCF+6* (IOC-1)+2) = COCAU
C ======================================================================
C --- PAR DEFAUT SANS FROTTEMENT (CONTACT ECP) -------------------------
C ======================================================================
          ZR(JCMCF+6* (IOC-1)+5) = 1.D0
          CALL GETVTX(MOTFAC,'MODL_AXIS',IOC,1,1,MODAX,NOC)
          IF (MODAX(1:3).EQ.'OUI') ZI(JECPD+5* (IOC-1)+1) = 1
          IF (MODAX(1:3).EQ.'NON') ZI(JECPD+5* (IOC-1)+1) = 0
          CALL GETVIS(MOTFAC,'ITER_CONT_MAXI',IOC,1,1,REACCA,NOC)
          ZI(JECPD+5* (IOC-1)+2) = REACCA
          CALL GETVIS(MOTFAC,'ITER_FROT_MAXI',IOC,1,1,REACBS,NOC)
          ZI(JECPD+5* (IOC-1)+3) = REACBS
          CALL GETVIS(MOTFAC,'ITER_GEOM_MAXI',IOC,1,1,REACBG,NOC)
          ZI(JECPD+5* (IOC-1)+4) = REACBG
          CALL GETVIS(MOTFAC,'SEUIL_INIT',IOC,1,1,REACSI,NOC)
          ZI(JECPD+5* (IOC-1)+5) = REACSI
          CALL GETVR8(MOTFAC,'DIRE_APPA',IOC,1,3,DIR1,NOC)
          MOTCLE(1) = 'GROUP_MA_1'
          ZR(JDIR+3* (IOC-1)) = DIR1(1)
          ZR(JDIR+3* (IOC-1)+1) = DIR1(2)
          IF (NDIM.EQ.3) THEN
            ZR(JDIR+3* (IOC-1)+2) = DIR1(3)
          ELSE
            ZR(JDIR+3* (IOC-1)+2) = 0.D0
          END IF            
          MOTCLE(2) = 'MAILLE_1'
          TYMOCL(1) = 'GROUP_MA'
          TYMOCL(2) = 'MAILLE'
          LISMA = '&&CALICO.LISTE_MAILLES_1'
          CALL RELIEM(NOMO,NOMA,'NU_MAILLE',MOTFAC,IOC,2,MOTCLE,
     &                TYMOCL,LISMA,NBMA1)

          IF (NDIM.EQ.2) THEN
            MODELI = 'CONT_DVP_2D'
          ELSE IF (NDIM.EQ.3) THEN
            MODELI = 'CONT_DVP_3D'
          END IF

          CALL AJELLT('&&CALICO.LIGRET',NOMA,NBMA1,LISMA,' ',PHENOM,
     &                MODELI,0,' ')
        END IF
C ======================================================================
C --- PRESENCE DE LISSAGE ? --------------------------------------------
C ======================================================================
        CALL GETVTX(MOTFAC,'LISSAGE',IOC,1,1,INTER,NOC)
        IF (INTER(1:3).EQ.'OUI') THEN
          ZI(JNORLI+ (IOC-1)+1) = 1
        ELSE IF (INTER(1:3).EQ.'NON') THEN
          ZI(JNORLI+ (IOC-1)+1) = 0
        END IF
C ======================================================================
C --- TYPE DE NORMALE --------------------------------------------------
C ======================================================================
        CALL GETVTX(MOTFAC,'NORMALE',IOC,1,1,TYPN,NOC)
        IF (TYPN(1:6).EQ.'MAIT') THEN
          ZI(JMETH+9* (IOC-1)+8) = 0
        ELSE IF (TYPN(1:9).EQ.'MAIT_ESCL') THEN
          ZI(JMETH+9* (IOC-1)+8) = 1
        ELSE
          CALL UTMESS('F','CALICO',
     &               'NE CORRESPOND A AUCUNE METHODE DU MOT CLE NORMALE'
     &                )
        END IF
C ======================================================================
C --- INFORMATION SUR LA MODELISATION DU FROTTEMENT --------------------
C ======================================================================
        CALL GETVTX(MOTFAC,'FROTTEMENT',IOC,1,1,TYPF,NOCC)
        IF (TYPF.EQ.'COULOMB') THEN
          ZR(JCMCF+6* (IOC-1)+5) = 3.D0
          CALL GETVR8(MOTFAC,'COULOMB',IOC,1,1,COEFRO,NOC)
          ZR(JCMCF+6* (IOC-1)+4) = COEFRO
          IF (TYPM(1:8).EQ.'CONTINUE') THEN
            CALL GETVR8(MOTFAC,'COEF_REGU_FROT',IOC,1,1,COFAU,NOC)
            ZR(JCMCF+6* (IOC-1)+3) = COFAU
          ELSE
            CALL GETVR8(MOTFAC,'E_T',IOC,1,1,COEFPT,NOC)
            ZR(JCMCF+6* (IOC-1)+3) = COEFPT
            CALL GETVR8(MOTFAC,'COEF_MATR_FROT',IOC,1,1,COEFTE,NOC)
            ZR(JCMCF+6* (IOC-1)+6) = COEFTE
          ENDIF
          IF (NDIM.EQ.2) THEN
            IF (TYPM(1:8).EQ.'PENALISA') THEN
              ZI(JMETH+9* (IOC-1)+6) = 3
              IF (NOCN.NE.0) ZI(JMETH+9* (IOC-1)+6) = 5
            ELSE IF (TYPM(1:8).EQ.'LAGRANGI') THEN
              ZI(JMETH+9* (IOC-1)+6) = 2
            END IF
          ELSE IF (NDIM.EQ.3) THEN
            IF (TYPM(1:8).EQ.'PENALISA') THEN
              ZI(JMETH+9* (IOC-1)+6) = 3
              IF (NOCN.NE.0) ZI(JMETH+9* (IOC-1)+6) = 5
            ELSE IF (TYPM(1:8).EQ.'LAGRANGI') THEN
              ZI(JMETH+9* (IOC-1)+6) = 4
            END IF
          END IF
        END IF
        IF (IOC.GT.1) THEN
          IF (ZI(JMETH+9* (IOC-1)+6).NE.ZI(JMETH+9* (IOC-2)+6)) THEN
            CALL UTMESS('F','CALICO',
     &                  'METHODE ET LOIS DE FRICTION IDENTIQUES POUR '//
     &                  'TOUTES LES ZONES CONTACT-FROTTEMENT')
          END IF
        END IF
C ======================================================================
C --- MOT-CLE VECT_NORM_2 ----------------------------------------------
C ======================================================================
        CALL GETVR8(MOTFAC,'VECT_NORM_2',IOC,1,3,DIR,NOC)
        IF (NOC.NE.0) THEN
          IF (APPAR(1:5).NE.'NODAL') THEN
            CALL UTMESS('F','CALICO_01',' ON NE PEUT PAS UTILISER '//
     &                  'UNE DIRECTION D''APPARIEMENT FIXE SI '//
     &                  'L''APPARIEMENT N''EST PAS NODAL')
          END IF
          ZI(JMETH+9* (IOC-1)+1) = 4
          ZR(JDIR+3* (IOC-1)) = DIR(1)
          ZR(JDIR+3* (IOC-1)+1) = DIR(2)
          IF (NDIM.EQ.3) THEN
            ZR(JDIR+3* (IOC-1)+2) = DIR(3)
          ELSE
            ZR(JDIR+3* (IOC-1)+2) = 0.D0
          END IF
        END IF
C ======================================================================
C --- MOT-CLE VECT_Y ---------------------------------------------------
C ======================================================================
        ZI(JMETH+9* (IOC-1)+2) = 0
        IF (TYPM(1:8).EQ.'LAGRANGI' .OR. TYPM(1:8).EQ.'PENALISA' .OR.
     &      TYPM(1:8).EQ.'CONTRAIN') THEN
          IF (NOCC.NE.0) THEN
            CALL GETVR8(MOTFAC,'VECT_Y',IOC,1,3,DIR,NOC)
C            ZI(JMETH+9* (IOC-1)+2) = 0
            IF (NOC.NE.0) THEN
              IF (NDIM.EQ.2) THEN
                CALL UTMESS('A','CALICO','LA COMMANDE VECT_Y'//
     &                      ' N''INTERVIENT PAS EN 2D.')
              ELSE
                ZI(JMETH+9* (IOC-1)+2) = 1
                ZR(JTGDEF+ (IOC-1)*3  ) = DIR(1)
                ZR(JTGDEF+ (IOC-1)*3+1) = DIR(2)
                ZR(JTGDEF+ (IOC-1)*3+2) = DIR(3)
              END IF
            END IF
C          ELSE
C            CALL GETVR8(MOTFAC,'VECT_Y',IOC,1,3,DIR,NOC)
C            IF (NOC.NE.0) THEN
C              CALL UTMESS('F','CALICO',
C     &                    'DEFINITION DE LA COMMANDE VECT_Y SANS '//
C     &                    'PRESENCE DE FROTTEMENT OU SEG')
C            END IF
          END IF
        END IF
C ======================================================================
C --- RECUPERATION DES GRANDEURS UTILISEES (DEPLACEMENT, PRESSION OU ---
C --- TEMPERATURE) ET DES COEFFICIENTS POUR LA LIAISON SANS APPARIEMENT-
C --- DANS LA ZONE IOC -------------------------------------------------
C ======================================================================
        CALL GETVTX(MOTFAC,'NOM_CHAM',IOC,1,1,CHAM,NOC)
        IF (CHAM(1:4).EQ.'DEPL') THEN
          IF (APPAR(1:3).EQ.'NON') THEN
            ZI(JCHAM+IOC-1) = -1
          ELSE
            ZI(JCHAM+IOC-1) = 1
          END IF
        END IF
        IF (CHAM(1:4).EQ.'PRES') ZI(JCHAM+IOC-1) = -2
        IF (CHAM(1:4).EQ.'TEMP') ZI(JCHAM+IOC-1) = -3

        IF (APPAR(1:3).EQ.'NON') THEN
          CALL GETVTX(MOTFAC,'GROUP_MA_1',1,1,1,K16BID,NOC)
          IF (NOC.NE.0) CALL UTMESS('F','CALICO',
     &                              'AVEC APPARIEMENT=NON, IL NE'//
     &                              ' FAUT PAS RENSEIGNER GROUP_MA_1')
          CALL GETVTX(MOTFAC,'MAILLE_1',1,1,1,K16BID,NOC)
          IF (NOC.NE.0) CALL UTMESS('F','CALICO',
     &                              'AVEC APPARIEMENT=NON, IL NE FAUT'//
     &                              ' PAS RENSEIGNER MAILLE_1')
          CALL GETVR8(MOTFAC,'COEF_MULT_2',IOC,1,1,COEF,NOC)
          ZR(JCOEF+IOC-1) = COEF
          CALL GETVR8(MOTFAC,'COEF_IMPO',IOC,1,1,SEUIL,NOC)
          ZR(JJSUP+IOC-1) = SEUIL
        ELSE
          ZR(JCOEF+IOC-1) = 1.D0
        END IF
C ======================================================================
C --- REMPLISSAGE DES TABLEAUX -----------------------------------------
C ======================================================================
C --- NOMBRE DE MAILLES ET DE NOEUDS DE LA ZONE IOC --------------------
C ======================================================================
        NBMA = ZI(JSUMA+ZI(JZONE+IOC)) - ZI(JSUMA+ZI(JZONE+IOC-1))
        NBNO = ZI(JSUNO+ZI(JZONE+IOC)) - ZI(JSUNO+ZI(JZONE+IOC-1))
        NBNOQU = ZI(JNOQUA+ZI(JZONE+IOC)) - ZI(JNOQUA+ZI(JZONE+IOC-1))
C ======================================================================
C --- ADRESSE DE DEBUT DE RANGEMENT DES MAILLES ET NOEUDS DE LA ZONE IOC
C --- DANS LES TABLEAUX CONTMA ET CONTNO -------------------------------
C ======================================================================
        JDECMA = ZI(JSUMA+ZI(JZONE+IOC-1)) + 1
        JDECNO = ZI(JSUNO+ZI(JZONE+IOC-1)) + 1
        JDECNQ = ZI(JNOQUA+ZI(JZONE+IOC-1)) + 1
C ======================================================================
C --- LECTURE DES MAILLES ET NOEUDS DE LA ZONE IOC ---------------------
C ======================================================================
        CALL LISTCO(MOTFAC,NOMO,NOMA,IOC,NTRAV,NBMA,NBNO,NBNOQU,
     &              ZI(JMACO+JDECMA-1),ZI(JNOCO+JDECNO-1),
     &              ZI(JNOQU-1+3* (JDECNQ-1)+1),CHAR)
C ======================================================================
   10 CONTINUE
C ======================================================================
C --- ELIMINATION DES REDONDANCES DE NOEUDS ET DE MAILLES AU SEIN ------
C --- D'UNE MEME SURFACE. MODIFICATION DES POINTEURS. ------------------
C --- STOCKAGE DES NOEUDS DECLARES SOUS 'SANS_NOEUD' ET 'SANS_GROUP_NO'.
C ======================================================================
      SANSNO = CHAR(1:8)//'.CONTACT.SSNOCO'
      PSANS = CHAR(1:8)//'.CONTACT.PSSNOCO'
      CALL ELIMCO(NOMA,PZONE,NZOCO,NSUCO,NMACO,NNOCO,NNOQUA,PSURMA,
     &            PSURNO,PNOQUA,CONTMA,CONTNO,CONOQU,SANSNO,PSANS)
C ======================================================================
C --- IMPRESSIONS APRES ELIMINATION ------------------------------------
C ======================================================================
      CALL SURFCO(NOMA,NZOCO,NSUCO,NMACO,NNOCO,PZONE,PSURMA,PSURNO,
     &            CONTMA,CONTNO,METHCO,CHAMCO,COEFCO)
C ======================================================================
C TABLEAU MANOCO : LISTE DES MAILLES DE CONTACT AUXQUELLES APPARTIENT --
C --------------   CHAQUE NOEUD DE CONTACT, POUR CHAQUE SURFACE. -------
C TABLEAU NOMACO : LISTE DES NOEUDS DE CHAQUE MAILLE DE CONTACT. -------
C TABLEAU MAMACO : POSITION DANS CONTMA DES MAILLES DE CONTACT ---------
C --------------   ADJACENTES A UNE MAILLE DONNEE DANS LA MEME SURFACE.-
C TABLEAU NOZOCO : NUMERO DE LA ZONE A LAQUELLE APPARTIENT CHAQUE NOEUD-
C --------------   DE CONTACT. -----------------------------------------
C ======================================================================
      MANOCO = CHAR(1:8)//'.CONTACT.MANOCO'
      PMANO = CHAR(1:8)//'.CONTACT.PMANOCO'
      NOMACO = CHAR(1:8)//'.CONTACT.NOMACO'
      PNOMA = CHAR(1:8)//'.CONTACT.PNOMACO'
      NORMCO = CHAR(1:8)//'.CONTACT.NORMCO'
      TANGCO = CHAR(1:8)//'.CONTACT.TANGCO'
      MAMACO = CHAR(1:8)//'.CONTACT.MAMACO'
      PMAMA = CHAR(1:8)//'.CONTACT.PMAMACO'
      NOZOCO = CHAR(1:8)//'.CONTACT.NOZOCO'
      CALL WKVECT(TANGCO,'G V R',6*NNOCO,JTANG)
      CALL TABLCO(NOMA,NDIM,PZONE,NZOCO,NSUCO,NMACO,NNOCO,PSURMA,PSURNO,
     &            CONTMA,CONTNO,MANOCO,PMANO,NMANO,NOMACO,PNOMA,NNOMA,
     &            NORMCO,MAMACO,PMAMA,NMAMA,NOZOCO)
C ======================================================================
C --- TABLEAU CONTENANT LES LONGUEURS DES DIFFERENTS VECTEURS ----------
C --- ET LE NOMBRE DE NOEUDS ESCLAVES MAXIMUM POUR CHAQUE ZONE ---------
C ======================================================================
      NDIMCO = CHAR(1:8)//'.CONTACT.NDIMCO'
      CALL WKVECT(NDIMCO,'G V I',9+NZOCO,JDIM)
      ZI(JDIM) = NDIM
      ZI(JDIM+1) = NZOCO
      ZI(JDIM+2) = NSUCO
      ZI(JDIM+3) = NMACO
      ZI(JDIM+4) = NNOCO
      ZI(JDIM+5) = NMANO
      ZI(JDIM+6) = NNOMA
      ZI(JDIM+7) = NMAMA
C ======================================================================
C --- CALCUL DU NOMBRE MAXIMAL DE NOEUDS ESCLAVES DANS CHAQUE ZONE -----
C --- ON COMPTE LES NOEUDS DE LA 2E SURFACE POUR APPARIEMENT 'NON', ----
C --- 'NODAL' ET 'MAIT_ESCL' -------------------------------------------
C --- LE MAX DES NOEUDS DES 1ERE ET 2EME SURFACE POUR 'MAIT_ESCL_SYME'--
C --- TOUS LES NOEUDS DES DIFFERENTES SURFACES POUR 'TERRITOIRE'--------
C --- ET 'HIERARCHIQUE'-------------------------------------------------
C ======================================================================
      NESMAX = 0
      DO 20 IOC = 1,NZOCO
        NESM = 0
        I1 = ZI(JZONE+IOC-1) + 1
        I2 = ZI(JZONE+IOC)
        NSURF = I2 - I1 + 1
        IF (NSURF.EQ.2) THEN
          NBNO1 = ZI(JSUNO+I1) - ZI(JSUNO+I1-1)
          NBNO2 = ZI(JSUNO+I2) - ZI(JSUNO+I2-1)
          IF (ZI(JMETH+9* (IOC-1)+3).EQ.0) THEN
            NESM = NESM + NBNO2
            NESMAX = NESMAX + NBNO2
          ELSE
            NESM = NESM + MAX(NBNO1,NBNO2)
            NESMAX = NESMAX + MAX(NBNO1,NBNO2)
          END IF
        ELSE
          NBNO1 = ZI(JSUNO+I2) - ZI(JSUNO+I1-1)
          NESM = NESM + NBNO1
          NESMAX = NESMAX + NBNO1
        END IF
        ZI(JDIM+8+IOC) = NESM
   20 CONTINUE
C ======================================================================
      ZI(JDIM+8) = NESMAX
C ======================================================================
C --- DIMENSIONNEMENT DES TABLEAUX CONTENANT LES INFORMATIONS ----------
C --- POUR METHODES "PENALISATION" ET "LAGRANGIEN" ---------------------
C ======================================================================
      CALL WKVECT(FROTE,'G V R',NESMAX,IFRO)
      CALL WKVECT(PENAL,'G V R',2*NESMAX,IPENA)
      CALL WKVECT(COMAFO,'G V R',NESMAX,ICOMA)

      IF ((ZI(JECPD).EQ.1) .AND. (INDMTH.EQ.0)) CALL UTMESS('F',
     &    'CALICO',' IL FAUT UTILISER UNE SEULE METHODE ')
      IF (ZI(JECPD).EQ.1) THEN
        DEFICO = CHAR(1:8)//'.CONTACT'
        CALL WKVECT(NOESCL,'G V R',10*NNOCO+1,JNOESC)
        ZR(JNOESC) = NNOCO
        DO 30 IOC = 1,NNOCO
          ZR(JNOESC+10* (IOC-1)+1) = 0.D0
   30   CONTINUE
        CALL MMACON(NOMA,DEFICO)
      END IF
   40 CONTINUE
C ======================================================================
      CALL JEDEMA()
      END
