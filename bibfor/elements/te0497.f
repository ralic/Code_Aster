      SUBROUTINE TE0497(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE MEUNIER S.MEUNIER
C TOLE CRP_20
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:  CALCUL DE L'ESTIMATEUR D'ERREUR EN RESIDU
C      SUR UN ELEMENT ISOPARAMETRIQUE 2D, VIA L'OPTION
C     'ERRE_ELEM_SIGM' POUR LA MODELISATION HM PERMANENTE
C IN OPTION : NOM DE L'OPTION
C IN NOMTE  : NOM DU TYPE D'ELEMENT
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES :
C       MESSAGE : UTMESS.
C       JEVEUX : JEMARQ,JEDEMA.
C       CHAMPS LOCAUX : JEVECH,TECACH,TECAEL.
C       MATERIAUX/CHARGES : RCVALA,RCCOMA.
C       DEDIEES A TE0497 : CAETHM,UTHK,ERHMV2,CALNOR,ERHMS2,ERHMB2,
C                          RESROT,UTJAC
C     FONCTIONS INTRINSEQUES :
C       SQRT.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS :
C       01/07/06 (SM): CREATION EN S'INSPIRANT DE TE0003.F ET DE
C                      TE0377.F .
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      CHARACTER*16 OPTION,NOMTE

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

C DECLARATION VARIABLES LOCALES
C
C
      INTEGER NBRES
      PARAMETER ( NBRES = 3 )
C
      INTEGER INO,JNO,NNO,NPG,NDIM,IGEOM,JGANO,
     &        IP,IR,IROT,IMATE,IVOIS,IERRE,NBS,IRET,NIV,
     &        ISIELN,IDEPLA,JKP,NBNA,ITAB(7),IFOR,IPES,IBID,
     &        IAGD,IATYMA,TYP,TYPV,IACMP,IREF1,IREF2,NBCMP
      INTEGER TBIAUX(1)
      INTEGER IADE2,IAVA2,IAPTM2,IGD2,NCMPM2
      INTEGER IADE3,IAVA3,IAPTM3,IGD3,NCMPM3
      INTEGER INST
      INTEGER IAUX, JAUX
      INTEGER DIMDEP,DIMDEF,DIMCON
      INTEGER IPOIDS,IVF,IDFDE,IPOID2,IVF2,IDFDE2
      INTEGER NMEC,NPI,NP1,NP2,NNOS,NNOM,NDDLS,NDDLM
      INTEGER MECANI(5),PRESS1(7),PRESS2(7),TEMPE(5),DIMUEL
      INTEGER ADSIP
      INTEGER YAMEC,ADDEME,ADCOME,YATE,ADDETE
      INTEGER YAP1,ADDEP1,ADCP11
      INTEGER YAP2,ADDEP2
C
      REAL*8 VALRES(NBRES)
      REAL*8 VALPAR(1)
      REAL*8 ORIEN,NX(3),NY(3),NZ(3),TX(3),TY(3)
      REAL*8 FPX,FPY,FRX(9),FRY(9)
      REAL*8 BIOT,JAC(27),HK,HF,TERVOM,TERSAM,TERSAH,
     &       TERCLM,TERCLH
      REAL*8 RHOHOM
      REAL*8 ERREST
C
C     REMARQUE : CES DIMENSIONS DOIVENT ETRE LES MEMES QUE DANS TE0600
      REAL*8 DFDI(20,3),DFDI2(20,3),B(21,120)
C
      LOGICAL     LAXI,PERMAN
C
      CHARACTER*2  CODMES(NBRES),FORMV,FORM,NOEU
      CHARACTER*3  MODINT
      CHARACTER*4  NOMPAR(1)
      CHARACTER*8  TYPEMA,TYPMAV
      CHARACTER*8  TYPMOD(2)
      CHARACTER*8  NOMRES(NBRES)
C
      DATA NOMRES / 'RHO','BIOT_COE','PERM_IN' /
C
C ------------------------------------------------------------------
C
      CALL JEMARQ()

C =====================================================================
C RECUPERATION D'INFORMATIONS SUR L'ELEMENT THM
C =====================================================================
      CALL CAETHM(NOMTE,LAXI,PERMAN,
     &            TYPMOD,MODINT,MECANI,PRESS1,PRESS2,TEMPE,
     &            DIMDEP,DIMDEF,DIMCON,NMEC,NP1,NP2,NDIM,NNO,
     &            NNOS,NNOM,NPI,NPG,NDDLS,NDDLM,DIMUEL,
     &            IPOIDS,IVF,IDFDE,IPOID2,IVF2,IDFDE2,JGANO)

C =====================================================================
C --- DETERMINATION DES VARIABLES CARACTERISANT LE MILIEU -------------
C =====================================================================
      YAMEC  = MECANI(1)
      ADDEME = MECANI(2)
      ADCOME = MECANI(3)
      YAP1   = PRESS1(1)
      ADDEP1 = PRESS1(3)
      ADCP11 = PRESS1(4)
      YAP2   = PRESS2(1)
      ADDEP2 = PRESS2(3)
      YATE   = TEMPE(1)
      ADDETE = TEMPE(2)
      ADSIP  = ADCP11 - ADCOME
C--------------------------------------------------------------------
C 1. INITIALISATIONS/RECUPERATION DE LA GEOMETRIE ET DES CHAMPS LOCAUX
C--------------------------------------------------------------------
      CALL TECACH('ONN','PTEMPSR',1,INST,IRET)
C
C ENTREES : LES FORCES VOLUMIQUES ET DES DEPLACEMENTS GENERALISES
C
      CALL JEVECH('PFRVOLU','L', IFOR)
      CALL JEVECH('PDEPLAR','L',IDEPLA)
C
C LES CHAMPS LOCAUX ASSOCIES AUX PARAMETRES:
C GEOMETRIE (IGEOM),P), MATERIAU (IMATE) ET CONTRAINTES
C AUX NOEUDS (ISIELN)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL TECACH('ONN','PCONTNO',3,ITAB,IRET)
      ISIELN   = ITAB(1)
      NBCMP = ITAB(2)/NNO
C
C--------------------------------------------------------------------
C 2. RECHERCHE DE LA VALEUR DU COEFFICIENT DE BIOT ET
C    DE LA MASSE VOLUMIQUE HOMOGENEISEE RHOHOM
C--------------------------------------------------------------------
      NOMPAR(1) = 'INST'
      VALPAR(1) = ZR(INST)
      CALL RCVALA ( ZI(IMATE), ' ', 'THM_DIFFU', 1, NOMPAR, VALPAR,
     &              NBRES, NOMRES, VALRES, CODMES, 'FM' )
C
      IF ( CODMES(1).EQ.'OK' .AND. CODMES(2).EQ.'OK' .AND.
     &     CODMES(3).EQ.'OK' ) THEN
        RHOHOM   = VALRES(1)
        BIOT     = VALRES(2)
      ELSE
        CALL U2MESK('F','ELEMENTS4_4',1,NOMRES(1)//NOMRES(2)//NOMRES(3))
      ENDIF
C
C--------------------------------------------------------------------
C 3. INITIALISATION DES FORCES DE PESANTEUR :
C . SOIT A PARTIR D'UNE CARTE
C . SOIT A ZERO
C--------------------------------------------------------------------
C
      CALL TECACH('ONN','PPESANR',1,IP,IRET)
C
      IF (IP .NE. 0) THEN
        CALL JEVECH('PPESANR','L',IPES)
        FPX = RHOHOM * ZR(IPES) * ZR(IPES+1)
        FPY = RHOHOM * ZR(IPES) * ZR(IPES+2)
      ELSE
        FPX = 0.D0
        FPY = 0.D0
      ENDIF
C
C--------------------------------------------------------------------
C 4. INITIALISATION DES FORCES DE ROTATION
C . SOIT A PARTIR D'UNE CARTE
C . SOIT A ZERO
C--------------------------------------------------------------------
C
      CALL TECACH('ONN','PROTATR',1,IR,IRET)
C
C CALCUL DE LA FORCE DE ROTATION AUX POINTS DE GAUSS
C REMARQUE : LE TABLEAU EST REMPLI A 9 CASES CAR C'EST LE MAX DE POINTS
C            DE GAUSS POSSIBLE EN 2D
C
      IF (IR .NE. 0) THEN
C
        CALL JEVECH('PROTATR','L',IROT)
        CALL RESROT (ZR(IROT),ZR(IGEOM),ZR(IVF),RHOHOM,NNO,NPG,
     &               FRX, FRY)
      ELSE
C
        DO 40 JKP =1, 9
          FRX(JKP) = 0.D0
          FRY(JKP) = 0.D0
   40   CONTINUE
C
      ENDIF
C
C------------------------------------------------------------------
C 5. CALCUL DES TERMES VOLUMIQUES
C------------------------------------------------------------------
C
C CALCUL DU DIAMETRE HK DE L'ELEMENT K
C
      NIV = 1
      CALL UTHK(NOMTE,IGEOM,HK,NDIM,IBID,IBID,IBID,IBID,NIV,IBID)
C
      CALL ERHMV2(LAXI,HK,
     &            DIMDEP,DIMDEF,NMEC,NP1,NP2,NDIM,NNO,
     &            NNOS,NNOM,NPI,NPG,NDDLS,NDDLM,DIMUEL,
     &            IPOIDS,IVF,IDFDE,IPOID2,IVF2,IDFDE2,
     &            ZR(IGEOM),ZR(IFOR),
     &            ZR(IDEPLA),ZR(ISIELN),NBCMP,
     &            BIOT,
     &            FPX,FPY,FRX,FRY,
     &            YAMEC,ADDEME,YAP1,ADDEP1,YAP2,ADDEP2,
     &            YATE, ADDETE,
     &            DFDI, DFDI2, B,
     &            TERVOM)
C
C------------------------------------------------------------------
C 6. CALCUL DES TERMES SURFACIQUES
C------------------------------------------------------------------
C 6.1. PHASE DE PREPARATION : ON RECUPERE LES ADRESSES NECESSAIRES
C                             AUX CALCULS
C -----------------------------------------------------------------
C
C ON RECUPERE L'ADRESSE DES VOISINS
      CALL JEVECH('PVOISIN','L',IVOIS)
C ON RECUPERE L'ADRESSE DES CHARGEMENTS DE TYPE FORCE_FACE
      CALL JEVECH('PFORCE','L',IREF1)
C ON RECUPERE L'ADRESSE DES CHARGEMENTS DE TYPE PRES_REP
      CALL JEVECH('PPRESS','L',IREF2)
C
C RECHERCHE DES ADRESSES POUR LES CHARGES SUR LES SEGMENTS
C
      IAGD  = ZI(IREF1+4)
      IACMP = ZI(IREF1+5)
C
      IADE2 = ZI(IREF2+4)
      IAVA2 = ZI(IREF2+5)
      IAPTM2 = ZI(IREF2+6)
      IF (IADE2 .NE. 0) THEN
        IGD2 = ZI(IADE2)
        NCMPM2 = ZI(IACMP-1+IGD2)
      ENDIF
C
      IADE3 = ZI(IREF2+8)
      IAVA3 = ZI(IREF2+9)
      IAPTM3 = ZI(IREF2+10)
      IF (IADE3 .NE. 0) THEN
        IGD3 = ZI(IADE3)
        NCMPM3 = ZI(IACMP-1+IGD3)
      ENDIF
C
C------------------------------------------------------------------
C 6.2. CARACTERISATIONS DE LA MAILLE COURANTE
C      . ORIENTATION
C -----------------------------------------------------------------
C
C TYPE DE LA MAILLE COURANTE
C
      TYP = ZI(IVOIS+7)
C
C ADRESSE DU VECTEUR TYPE MAILLE
C
      IATYMA = ZI(IREF1+3)
      TYPEMA=ZK8(IATYMA-1+TYP)
      FORM=TYPEMA(1:2)
C
C NOMBRE DE NOEUDS SOMMETS ET NOMBRE DE NOEUDS DES ARETES
C
      IF (FORM.EQ.'TR') THEN
        NBS=3
      ELSE
        NBS=4
      ENDIF
C
      NOEU=TYPEMA(5:5)
C
      IF (NOEU.EQ.'6'.OR.NOEU.EQ.'8'.OR.NOEU.EQ.'9') THEN
        NBNA=3
      ELSE
        NBNA=2
      ENDIF
C
C ------- CALCUL DE L'ORIENTATION DE LA MAILLE 2D ----------------------
C     REMARQUE : ON APPELLE LE PROGRAMME GENERIQUE POUR LE PREMIER POINT
C                DE GAUSS, SACHANT QUE L'ORIENTATION NE DOIT PAS CHANGER
C
      JKP = 1
      CALL UTJAC(.TRUE.,IGEOM,JKP,IDFDE,0,IBID,NNO,ORIEN)
C
C------------------------------------------------------------------
C 6.3. CALCUL DES TERMES LIES AUX ARETES
C------------------------------------------------------------------
C ON INITIALISE LES TERMES DE SAUT ET LES TERMES PROVENANT DES
C CONDITIONS AUX LIMITES (HYDRAULIQUE + MECANIQUE)
C
      TERSAM = 0.D0
      TERSAH = 0.D0
      TERCLM = 0.D0
      TERCLH = 0.D0
C
C BOUCLE SUR LES ARETES : IMPLICITEMENT, ON NUMEROTE LOCALEMENT LES
C                         ARETES COMME LES NOEUDS SOMMETS
C . DONC LE PREMIER NOEUD DE L'ARETE A LE MEME NUMERO QUE L'ARETE : INO
C . LE NOEUD SUIVANT ETS INO+1, SAUF SI ON EST SUR LA DERNIERE ARETE ;
C   LE NOEUD SUIVANT EST ALORS LE PREMIER, 1.
C . L'EVENTUEL NOEUD MILIEU EST LE 1ER NOEUD, DECALE DU NOMBRE DE NOEUDS
C   SOMMETS : INO + NBS
C
      DO 63 , INO = 1,NBS
C
C ------- NUMEROS LOCAUX DES NOEUDS DE L'ARETE
C
        IF (INO.EQ.NBS) THEN
          JNO = 1
        ELSE
          JNO = INO+1
        ENDIF
C
C ------- CALCUL DE LA LONGUEUR DE L'ARETE --------
C
        IAUX = IGEOM+2*(INO-1)
        JAUX = IGEOM+2*(JNO-1)
        HF=SQRT((ZR(IAUX)-ZR(JAUX))**2+(ZR(IAUX+1)-ZR(JAUX+1))**2)
C
C --- CALCUL DES NORMALES, TANGENTES ET JACOBIENS AUX POINTS DE L'ARETE
C
        IAUX = INO
        CALL CALNOR ( '2D', IAUX, NNO, IBID, NBS, NBNA, TBIAUX,
     &                IGEOM, IBID,
     &                IBID, IBID, ORIEN, HF,
     &                JAC, NX, NY, NZ, TX, TY )
C
C TEST DU TYPE DE VOISIN : TYPV VAUT 0 POUR UN BORD LIBRE ==> ON NE SAIT
C PAS FAIRE AUJOURD'HUI
C
        TYPV=ZI(IVOIS+7+INO)
        IF (TYPV.NE.0) THEN
C
          TYPMAV=ZK8(IATYMA-1+TYPV)
          FORMV=TYPMAV(1:2)
C
          IF (FORMV.EQ.'SE') THEN
C --------------------------------------------------------------
C 1. CALCUL DES TERMES DE VERIFICATION DES CONDITIONS DE BORD
C --------------------------------------------------------------
C
            CALL ERHMB2( INO,NBS,NBNA,HF,NDIM,
     &                   JAC,NX,NY,TX,TY,
     &                   NBCMP,ZR(IGEOM),IVOIS,
     &                   ZR(ISIELN),ADSIP,
     &                   IAGD,ZI(IREF2),IADE2,IAVA2,NCMPM2,IAPTM2,
     &                   IADE3,IAVA3,NCMPM3,IAPTM3,
     &                   TERCLM,TERCLH)
C
          ELSE IF (FORMV.EQ.'TR'.OR.FORMV.EQ.'QU') THEN
C --------------------------------------------------------------
C 2. CALCUL DES TERMES DE SAUT A TRAVERS LES FACES INTERIEURES
C DE LA MAILLE
C --------------------------------------------------------------
            CALL ERHMS2( INO,NBS,NBNA,HF,JAC,NX,NY,
     &                   ZR(ISIELN),ADSIP,
     &                   NBCMP,TYPMAV,ZI(IREF1),IVOIS,
     &                   TERSAM,TERSAH)
          ENDIF
C
        ENDIF
C
 63   CONTINUE
C------------------------------------------------------------------
C C. MISE EN FORME DES DIFFERENTS TERMES DE L'ERREUR
C------------------------------------------------------------------
C
      CALL JEVECH('PERREUR','E',IERRE)
C
      ERREST = TERVOM+TERSAM+TERSAH+TERCLM+TERCLH
C
      ZR(IERRE  ) = ERREST
      ZR(IERRE+1) = 0.D0
      ZR(IERRE+2) = 0.D0
C
      ZR(IERRE+3) = TERVOM
      ZR(IERRE+4) = 0.D0
C
      ZR(IERRE+5) = TERCLM+TERCLH
      ZR(IERRE+6) = 0.D0
C
      ZR(IERRE+7) = TERSAM+TERSAH
      ZR(IERRE+8) = 0.D0
C
      CALL JEDEMA()
C
      END
