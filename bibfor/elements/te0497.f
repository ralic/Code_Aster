      SUBROUTINE TE0497(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 23/04/2007   AUTEUR GNICOLAS G.NICOLAS 
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
C      SUR UN ELEMENT ISOPARAMETRIQUE 2D, VIA L'OPTION 'ERRE_ELEM_SIGM'
C      POUR LES MODELISATIONS HM SATUREES
C IN OPTION : NOM DE L'OPTION
C IN NOMTE  : NOM DU TYPE D'ELEMENT
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES :
C       MESSAGE           : U2MESS,U2MESK.
C       JEVEUX            : JEMARQ,JEDEMA.
C       CHAMPS LOCAUX     : JEVECH,TECACH,TECAEL.
C       ENVIMA            : R8MIEM.
C       MATERIAUX/CHARGES : RCVALA,RCCOMA.
C       DEDIEES A TE0497  : CAETHM,UTHK,ERHMTE,ERHMV2,CALNOR,ERHMS2,
C                           ERHMB2,RESROT,UTJAC
C     FONCTIONS INTRINSEQUES :
C       SQRT.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS :
C       03/07/06 (SM): CREATION EN S'INSPIRANT DE TE0003.F ET DE
C                      TE0377.F .
C                      CALCUL INDICATEURS EN STATIONNAIRE
C       01/05/07 (SM): ADIMENSIONNEMENT DES INDICATEURS EN
C                      STATIONNAIRE .
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
      INTEGER INO,JNO,NNO,NPG,NDIM,IGEOM,JGANO,
     &        IMATE,IVOIS,IERRE,NBS,IRET,NIV,
     &        ISIENP,IDEPLP,JKP,NBNA,
     &        ITAB(7),IFOR,IBID,IAGD,IATYMA,TYP,TYPV,IACMP,
     &        IREF1,IREF2,NBCMP
      INTEGER IADE2,IAVA2,IAPTM2,IGD2,NCMPM2
      INTEGER IADE3,IAVA3,IAPTM3,IGD3,NCMPM3
      INTEGER IAUX, JAUX, IGRDCA
      INTEGER DIMDEP,DIMDEF,DIMCON
      INTEGER IPOIDS,IVF,IDFDE,IPOID2,IVF2,IDFDE2
      INTEGER NMEC,NPI,NP1,NP2,NNOS,NNOM,NDDLS,NDDLM
      INTEGER MECANI(5),PRESS1(7),PRESS2(7),TEMPE(5),DIMUEL
      INTEGER ADSIP
      INTEGER YAMEC,ADDEME,ADCOME,YATE,ADDETE
      INTEGER YAP1,ADDEP1,ADCP11
      INTEGER YAP2,ADDEP2
      INTEGER IADZI,IAZK24
C
      REAL*8 R8MIEM,OVFL
      REAL*8 R8BID
      REAL*8 VALPAR(1)
      REAL*8 ORIEN,NX(3),NY(3),NZ(3),TX(3),TY(3)
      REAL*8 FPX,FPY,FRX(9),FRY(9)
      REAL*8 INSTP
      REAL*8 BIOT,RHOLIQ,UNSURK,JAC(27)
      REAL*8 HK,HF
      REAL*8 RHOHOM,PERMIN,VISCLI
      REAL*8 TERVOM,TERSAM,TERSAH,TERCLM,TERCLH
      REAL*8 LONGC,PRESC,ADVOM,ADSAM,ADSAH,ADCLM,ADCLH
      REAL*8 ERREST
C
      LOGICAL     LAXI,PERMAN
C
      CHARACTER*2  FORMV,FORM,NOEU
      CHARACTER*3  MODINT
      CHARACTER*4  NOMPAR(1)
      CHARACTER*8  TYPEMA,TYPMAV
      CHARACTER*8  TYPMOD(2)
C
      INTEGER     NBRE1,NBRE2
      PARAMETER ( NBRE1 = 3 , NBRE2 = 2 )
C
      REAL*8       VALRE1(NBRE1),VALRE2(NBRE2)
C
      CHARACTER*2  CODME1(NBRE1),CODME2(NBRE2)
      CHARACTER*8  NOMRE1(NBRE1),NOMRE2(NBRE2)
      CHARACTER*8 VALK
C
      DATA NOMRE1 / 'RHO','BIOT_COE','PERM_IN' /
      DATA NOMRE2 / 'RHO','VISC' /
C
C ------------------------------------------------------------------
C
      CALL TECAEL(IADZI,IAZK24)
C
      CALL JEMARQ()
      OVFL = R8MIEM()

C =====================================================================
C A. --- RECUPERATION D'INFORMATIONS SUR L'ELEMENT THM ----------------
C =====================================================================
      CALL CAETHM(NOMTE ,LAXI  ,PERMAN,
     &            TYPMOD,MODINT,MECANI,PRESS1,PRESS2,TEMPE ,
     &            DIMDEP,DIMDEF,DIMCON,NMEC  ,NP1   ,NP2   ,NDIM  ,NNO,
     &            NNOS  ,NNOM  ,NPI   ,NPG   ,NDDLS ,NDDLM ,DIMUEL,
     &            IPOIDS,IVF   ,IDFDE ,IPOID2,IVF2  ,IDFDE2,JGANO)
C
        R8BID = DBLE(NDIM)/2.D0
C =====================================================================
C B. --- DETERMINATION DES VARIABLES CARACTERISANT LE MILIEU ----------
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

C =====================================================================
C C. --- RECUPERATION DES DONNEES NECESSAIRES AU CALCUL ---------------
C =====================================================================
C--------------------------------------------------------------------
C 1. EVENTUELS PARAMETRES TEMPORELS
C--------------------------------------------------------------------
      CALL TECACH('ONN','PTEMPSR',1,ITAB,IRET)
      IF ( IRET.EQ.0 ) THEN
        INSTP = ZR(ITAB(1))
      ELSE
        CALL U2MESS('F','INDICATEUR_11')
      ENDIF
C--------------------------------------------------------------------
C 2. INITIALISATIONS/RECUPERATION DE LA GEOMETRIE ET DES CHAMPS LOCAUX
C--------------------------------------------------------------------
C
C 2.1. GEOMETRIE (IGEOM), MATERIAU (IMATE) :
C
      CALL JEVECH('PGEOMER','L',IGEOM)
C
      CALL JEVECH('PMATERC','L',IMATE)
C
C 2.2. LES DEPLACEMENTS A L'INSTANT COURANT --> IDEPLP
C
      CALL JEVECH('PDEPLAR','L',IDEPLP)
C
C 2.3. CONTRAINTES AUX NOEUDS PAR ELEMENTS A L'INSTANT ACTUEL --> ISIENP
C
      CALL TECACH('ONN','PCONTNO',3,ITAB,IRET)
      ISIENP = ITAB(1)
      NBCMP  = ITAB(2)/NNO
C
C 2.4. LES FORCES VOLUMIQUES :
C
      CALL JEVECH('PFRVOLU','L', IFOR)
C--------------------------------------------------------------------
C 3. RECHERCHE DES VALEURS NECESSAIRES AU CALCUL DE L'INDICATEUR
C     . COEFFICIENT DE BIOT 
C     . MASSE VOLUMIQUE HOMOGENEISEE RHOHOM
C     . MASSE VOLUMIQUE DU LIQUIDE RHOLIQ
C     . CONDUCTIVITE HYDRAULIQUE
C-------------------------------------------------------------------- 
      NOMPAR(1) = 'INST'
      VALPAR(1) = INSTP
C
      CALL RCVALA ( ZI(IMATE), ' ', 'THM_DIFFU', 1, NOMPAR, VALPAR,
     &              NBRE1, NOMRE1, VALRE1, CODME1, 'FM' )
C
      IF ( CODME1(1).EQ.'OK' .AND. CODME1(2).EQ.'OK' .AND.
     &     CODME1(3).EQ.'OK' ) THEN
        RHOHOM = VALRE1(1)
        BIOT   = VALRE1(2)
        PERMIN = VALRE1(3)
      ELSE
        CALL U2MESK('F','ELEMENTS4_78',1,
     &              NOMRE1(1)//NOMRE1(2)//NOMRE1(3))
      ENDIF
C
      CALL RCVALA ( ZI(IMATE), ' ', 'THM_LIQU', 1, NOMPAR, VALPAR,
     &              NBRE2, NOMRE2, VALRE2, CODME2, 'FM' )
C      
      IF ( ( CODME2(1).EQ.'OK' ) .AND. ( CODME2(2).EQ.'OK' ) ) THEN
        RHOLIQ = VALRE2(1)
        VISCLI = VALRE2(2)
      ELSE
        CALL U2MESK('F','ELEMENTS4_69',1,NOMRE2(1)//NOMRE2(2))
      ENDIF
C
      IF ( PERMIN.GT.OVFL ) THEN
        UNSURK = VISCLI/PERMIN
      ELSE
        CALL U2MESS('F','INDICATEUR_20')
      ENDIF

C--------------------------------------------------------------------
C 4. INITIALISATION DES FORCES
C--------------------------------------------------------------------
C 4.1. FORCES DE PESANTEUR :
C . SOIT A PARTIR D'UNE CARTE
C . SOIT A ZERO
C
      CALL TECACH('ONN','PPESANR',1,ITAB,IRET)
C
      IF ( ITAB(1).NE.0 ) THEN
        FPX = RHOHOM * ZR(ITAB(1)) * ZR(ITAB(1)+1)
        FPY = RHOHOM * ZR(ITAB(1)) * ZR(ITAB(1)+2)
      ELSE
        FPX = 0.D0
        FPY = 0.D0
      ENDIF
C
C 4.2. FORCES DE ROTATION
C . SOIT A PARTIR D'UNE CARTE
C . SOIT A ZERO
C CALCUL DE LA FORCE DE ROTATION AUX POINTS DE GAUSS
C REMARQUE : LE TABLEAU EST DIMENSIONNE A 9 CASES CAR 
C C'EST LE MAX DE POINTS DE GAUSS POSSIBLE EN 2D
C
      CALL TECACH('ONN','PROTATR',1,ITAB,IRET)   
C
      IF ( ITAB(1).NE.0 ) THEN
        CALL RESROT (ZR(ITAB(1)),ZR(IGEOM),ZR(IVF),RHOHOM,NNO,NPG,
     &               FRX, FRY)
      ELSE
C
        DO 10 JKP =1, 9
          FRX(JKP) = 0.D0
          FRY(JKP) = 0.D0
   10   CONTINUE
C
      ENDIF
C--------------------------------------------------------------------
C 5. RECUPERATION DES GRANDEURS CARACTERISTIQUES
C--------------------------------------------------------------------
      CALL JEVECH('PGRDCA','L',IGRDCA)
      LONGC = ZR(IGRDCA)
      PRESC = ZR(IGRDCA+1)
C
      IAUX = 0
      IF ( PRESC.LE.OVFL ) THEN
        IAUX = 1
        VALK = 'pression'
      ELSEIF ( LONGC.LE.OVFL ) THEN
        IAUX = 1
        VALK = 'longueur'
      ENDIF
C
      IF ( IAUX.NE.0 ) THEN
        CALL U2MESK('F', 'INDICATEUR_21', 1, VALK )
      ENDIF

C =====================================================================
C D. --- CALCUL DES INDICATEURS ---------------------------------------
C =====================================================================
C
C------------------------------------------------------------------
C 1. CALCUL DES TERMES VOLUMIQUES
C------------------------------------------------------------------
C
C CALCUL DU DIAMETRE HK DE L'ELEMENT K
C
      NIV = 1
      CALL UTHK(NOMTE,IGEOM,HK,NDIM,IBID,IBID,IBID,IBID,NIV,IBID)
C
      CALL ERHMV2(LAXI      , HK    ,
     &            DIMDEP    , DIMDEF    , NMEC  , NP1    , NP2 ,
     &            NDIM      , NNO       , NNOS  , NNOM   , NPI , NPG   ,
     &            NDDLS     , NDDLM     , DIMUEL,
     &            IPOIDS    , IVF       , IDFDE , IPOID2 , IVF2, IDFDE2,
     &            ZR(IGEOM) , ZR(IFOR)  ,
     &            ZR(IDEPLP),
     &            ZR(ISIENP), NBCMP     ,
     &            BIOT      ,
     &            FPX       , FPY       , FRX   , FRY    ,
     &            YAMEC     , ADDEME    , YAP1  , ADDEP1 , YAP2, ADDEP2,
     &            YATE      , ADDETE    ,
     &            TERVOM)

C ON ADIMENSIONNE LES INDICATEURS VOLUMIQUES
C
      ADVOM  = 1.D0/(PRESC*LONGC**R8BID)
      TERVOM = ADVOM*TERVOM
C
C------------------------------------------------------------------
C 2. CALCUL DES TERMES SURFACIQUES
C------------------------------------------------------------------
C 2.1. PHASE DE PREPARATION : ON RECUPERE LES ADRESSES NECESSAIRES
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
      IADE2  = ZI(IREF2+4)
      IAVA2  = ZI(IREF2+5)
      IAPTM2 = ZI(IREF2+6)
      IF (IADE2 .NE. 0) THEN
        IGD2   = ZI(IADE2)
        NCMPM2 = ZI(IACMP-1+IGD2)
      ENDIF
C
      IADE3  = ZI(IREF2+8)
      IAVA3  = ZI(IREF2+9)
      IAPTM3 = ZI(IREF2+10)
      IF (IADE3 .NE. 0) THEN
        IGD3   = ZI(IADE3)
        NCMPM3 = ZI(IACMP-1+IGD3)
      ENDIF
C
C------------------------------------------------------------------
C 2.2. CARACTERISATIONS DE LA MAILLE COURANTE
C -----------------------------------------------------------------
C
C TYPE DE LA MAILLE COURANTE
C
      TYP = ZI(IVOIS+7)
C
C ADRESSE DU VECTEUR TYPE MAILLE
C
      IATYMA = ZI(IREF1+3)
      TYPEMA = ZK8(IATYMA-1+TYP)
      FORM   = TYPEMA(1:2)
C
C NOMBRE DE NOEUDS SOMMETS ET NOMBRE DE NOEUDS DES ARETES
C
      IF (FORM.EQ.'TR') THEN
        NBS = 3
      ELSE
        NBS = 4
      ENDIF
C
      NOEU = TYPEMA(5:5)
C
      IF (NOEU.EQ.'6'.OR.NOEU.EQ.'8'.OR.NOEU.EQ.'9') THEN
        NBNA = 3
      ELSE
        NBNA = 2
      ENDIF
C
C CALCUL DE L'ORIENTATION DE LA MAILLE 2D
C     REMARQUE : ON APPELLE LE PROGRAMME GENERIQUE POUR LE PREMIER POINT
C                DE GAUSS, SACHANT QUE L'ORIENTATION NE DOIT PAS CHANGER
C
      JKP = 1
      CALL UTJAC(.TRUE.,IGEOM,JKP,IDFDE,0,IBID,NNO,ORIEN)
C
C------------------------------------------------------------------
C 2.3. CALCUL DES TERMES LIES AUX ARETES
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
C . LE NOEUD SUIVANT EST INO+1, SAUF SI ON EST SUR LA DERNIERE ARETE ;
C   LE NOEUD SUIVANT EST ALORS LE PREMIER, 1.
C . L'EVENTUEL NOEUD MILIEU EST LE 1ER NOEUD, DECALE DU NOMBRE DE NOEUDS
C   SOMMETS : INO + NBS
C
      DO 20 , INO = 1,NBS
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
        HF = SQRT((ZR(IAUX)-ZR(JAUX))**2+(ZR(IAUX+1)-ZR(JAUX+1))**2)
C
C --- CALCUL DES NORMALES, TANGENTES ET JACOBIENS AUX POINTS DE L'ARETE
C
        IAUX = INO
        CALL CALNOR ( '2D' , IAUX, NNO  , IBID, NBS, NBNA, ITAB,
     &                IGEOM, IBID,
     &                IBID , IBID, ORIEN, HF  ,
     &                JAC  , NX  , NY   , NZ  , TX , TY          )
C
C TEST DU TYPE DE VOISIN : TYPV VAUT 0 POUR UN BORD LIBRE ==> ON NE SAIT
C PAS FAIRE AUJOURD'HUI
C
        TYPV = ZI(IVOIS+7+INO)
        IF (TYPV.NE.0) THEN
C
          TYPMAV = ZK8(IATYMA-1+TYPV)
          FORMV  = TYPMAV(1:2)
C
          IF (FORMV.EQ.'SE') THEN
C ---------------------------------------------------------------
C 2.4. CALCUL DES TERMES DE VERIFICATION DES CONDITIONS DE BORD
C ---------------------------------------------------------------

            CALL ERHMB2( INO        ,NBS       ,NBNA   ,HF,
     &                   NDIM       ,JAC       ,NX     ,NY,
     &                   TX         ,TY        ,NBCMP  ,ZR(IGEOM),
     &                   IVOIS      ,
     &                   ZR(ISIENP) ,ADSIP     ,
     &                   IAGD       ,ZI(IREF2) ,IADE2  ,IAVA2    ,
     &                   NCMPM2     ,IAPTM2    ,
     &                   IADE3      ,IAVA3     ,NCMPM3 ,IAPTM3   ,
     &                   TERCLM     ,TERCLH)
C
          ELSE IF (FORMV.EQ.'TR'.OR.FORMV.EQ.'QU') THEN
C ----------------------------------------------------------------
C 2.5. CALCUL DES TERMES DE SAUT A TRAVERS LES FACES INTERIEURES
C DE LA MAILLE
C ----------------------------------------------------------------
            CALL ERHMS2( INO       ,NBS              ,NBNA     ,HF   ,
     &                   JAC       ,NX    ,NY        ,
     &                   ZR(ISIENP),ADSIP ,
     &                   NBCMP     ,TYPMAV,ZI(IREF1) ,IVOIS,
     &                   TERSAM    ,TERSAH)
C
          ENDIF
C
        ENDIF
C
 20   CONTINUE

C =====================================================================
C E. --- MISE EN FORME DES INDICATEURS --------------------------------
C =====================================================================
C
C ON ADIMENSIONNE LES INDICATEURS
C
      ADCLM = 1.D0/(PRESC*LONGC**R8BID)
      ADCLH = UNSURK*LONGC**(1.D0-R8BID)/(PRESC*RHOLIQ)
C
      ADSAM = 1.D0/(PRESC*LONGC**R8BID)
      ADSAH = UNSURK*LONGC**(1.D0-R8BID)/(PRESC*RHOLIQ)
C
      TERCLM = ADCLM*TERCLM
      TERCLH = ADCLH*TERCLH
C
      TERSAM = ADSAM*TERSAM
      TERSAH = ADSAH*TERSAH
C
C ON STOCKE LES INDICATEURS
C
      CALL JEVECH('PERREUR','E',IERRE)
C
      ERREST = TERVOM + TERSAM + TERSAH + TERCLM + TERCLH
C
      ZR(IERRE  ) = ERREST
      ZR(IERRE+1) = 0.D0
      ZR(IERRE+2) = 0.D0
C
      ZR(IERRE+3) = TERVOM
      ZR(IERRE+4) = 0.D0
C
      ZR(IERRE+5) = TERCLM + TERCLH
      ZR(IERRE+6) = 0.D0
C
      ZR(IERRE+7) = TERSAM + TERSAH
      ZR(IERRE+8) = 0.D0
C
      CALL JEDEMA()
C
      END
