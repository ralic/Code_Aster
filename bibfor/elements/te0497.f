      SUBROUTINE TE0497(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 02/06/2008   AUTEUR MEUNIER S.MEUNIER 
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
C       CHAMPS LOCAUX     : JEVECH,TECACH.
C       ENVIMA            : R8MIEM.
C       MATERIAUX/CHARGES : RCVALA.
C       DEDIEES A TE0497  : CAETHM,UTHK,ERHMV2,CALNOR,ERHMS2,
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
      INTEGER INO    ,NNO   ,NPG   ,NDIM  ,IGEOM ,JGANO,
     &        IMATE  ,IVOIS ,IERRE ,IERRM ,NBS   ,IRET  ,NIV  ,
     &        ISIENP ,ISIENM,IDEPLP,IDEPLM,JKP   ,NBNA ,
     &        ITAB(7),IFOR  ,IBID  ,IAGD  ,IATYMA,TYP  ,
     &        TYPV   ,IACMP ,IREF1 ,IREF2 ,NBCMP
      INTEGER IADE2,IAVA2,IAPTM2,IGD2,NCMPM2
      INTEGER IADE3,IAVA3,IAPTM3,IGD3,NCMPM3
      INTEGER IAUX,IGRDCA
      INTEGER DIMDEP,DIMDEF,DIMCON
      INTEGER IPOIDS,IVF,IDFDE,IPOID2,IVF2,IDFDE2
      INTEGER NMEC,NPI,NP1,NP2,NNOS,NNOM,NDDLS,NDDLM
      INTEGER MECANI(5),PRESS1(7),PRESS2(7),TEMPE(5),DIMUEL
      INTEGER ADSIP
      INTEGER YAMEC,ADDEME,ADCOME,YATE,ADDETE
      INTEGER YAP1,ADDEP1,ADCP11
      INTEGER YAP2,ADDEP2
      INTEGER II
C
      REAL*8 R8MIEM,OVFL,R8BID
      REAL*8 VALPAR(1)
      REAL*8 ORIEN,NX(3),NY(3),NZ(3),TX(3),TY(3)
      REAL*8 FPX,FPY,FRX(9),FRY(9)
      REAL*8 INSTPM(2)
      REAL*8 BIOT,RHOLIQ,UNSURK,UNSURM,JAC(27)
      REAL*8 HT,DELTAT,THETA
      REAL*8 CYOUNG,RHOHOM,PERMIN,VISCLI,POROSI,POISSO
      REAL*8 TM2H1V(3),TM2H1B(3),TM2H1S(3)
      REAL*8 TSIVOM,TDEVOM,TSIVOH,TSIBOM,TDEBOM,TSIBSH,TSIBBH,
     &       TSISAM,TDESAM,TSISSH,TSISBH,DENOMI
      REAL*8 LONGC,PRESC,ADMEC,ADHY0,ADHY1,ADV1H,ADHYMD
C
      LOGICAL      LAXI,PERMAN
C
      CHARACTER*2  FORMV,FORM,NOEU
      CHARACTER*3  MODINT
      CHARACTER*4  NOMPAR(1)
      CHARACTER*8  TYPEMA,TYPMAV
      CHARACTER*8  TYPMOD(2)
C
      INTEGER     NBRE1     , NBRE2    , NBRE3     , NBRE4
      PARAMETER ( NBRE1 = 3 , NBRE2 = 2, NBRE3 = 1 , NBRE4 = 2 )
C
      REAL*8       VALRE1(NBRE1),VALRE2(NBRE2),
     &             VALRE3(NBRE3),VALRE4(NBRE4)
C
      CHARACTER*2  CODME1(NBRE1),CODME2(NBRE2),
     &             CODME3(NBRE3),CODME4(NBRE4)
      CHARACTER*8  NOMRE1(NBRE1),NOMRE2(NBRE2),
     &             NOMRE3(NBRE3),NOMRE4(NBRE4)
      CHARACTER*8 VALK
C
      DATA NOMRE1 / 'RHO','BIOT_COE','PERM_IN' /
      DATA NOMRE2 / 'RHO','VISC' /
      DATA NOMRE3 / 'PORO'       /
      DATA NOMRE4 / 'E', 'NU'    /
C
C ------------------------------------------------------------------
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
        INSTPM(1) = ZR(ITAB(1))
        IF ( .NOT.PERMAN ) THEN
          DELTAT     = ZR(ITAB(1)+1)
          THETA      = ZR(ITAB(1)+2)
          INSTPM(2)  = INSTPM(1)-DELTAT
        ENDIF
      ELSE
        CALL U2MESS('F','INDICATEUR_11')
      ENDIF
C--------------------------------------------------------------------
C 2. RECUPERATION DE LA GEOMETRIE, DU MATERIAU ET DES CHAMPS LOCAUX
C--------------------------------------------------------------------
C
C 2.1. GEOMETRIE (IGEOM), MATERIAU (IMATE) :
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
C
C 2.2. LES DEPLACEMENTS A L'INSTANT COURANT
C      1. TOUJOURS A L'INSTANT COURANT --> IDEPLP
C      2. SI TRANSITOIRE, A L'INSTANT PRECEDENT --> IDEPLM
C
      CALL JEVECH('PDEPLAR','L',IDEPLP)
C
      IF ( .NOT. PERMAN ) THEN
        CALL JEVECH('PDEPLMR','L',IDEPLM)
      ENDIF
C
C 2.3. CONTRAINTES AUX NOEUDS PAR ELEMENTS A L'INSTANT ACTUEL
C      1. TOUJOURS A L'INSTANT ACTUEL --> ISIENP
C      2. SI TRANSITOIRE, A L'INSTANT PRECEDENT --> ISIENM
C
      CALL TECACH('ONN','PCONTNO',3,ITAB,IRET)
      ISIENP = ITAB(1)
      NBCMP  = ITAB(2)/NNO
      IF ( .NOT. PERMAN ) THEN
        CALL TECACH('ONN','PCONTNM',3,ITAB,IRET)
        ISIENM   = ITAB(1)
      ENDIF
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
      VALPAR(1) = INSTPM(1)
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
      IF ( PERMIN .GT. OVFL ) THEN
        UNSURK = VISCLI/PERMIN
      ELSE
        CALL U2MESS('F','INDICATEUR_20')
      ENDIF
C
C--------------------------------------------------------------------
C 4. SI INSTATIONNAIRE, ON RECUPERE DES COEFFICIENTS SUPPLEMENTAIRES
C     . MODULE DE BIOT
C     . MODULE DE YOUNG
C--------------------------------------------------------------------
C
      IF ( .NOT. PERMAN ) THEN
C
C 4.1. RECHERCHE DE LA POROSITE INITIALE
C
        CALL RCVALA ( ZI(IMATE), ' ', 'THM_INIT', 1, NOMPAR, VALPAR,
     &                NBRE3, NOMRE3, VALRE3, CODME3, 'FM' )
C
        IF ( CODME3(1).EQ.'OK' ) THEN
          POROSI = VALRE3(1)
        ELSE
          CALL U2MESK('F','ELEMENTS4_70',1,NOMRE3(1))
        ENDIF
C
C 4.2. RECHERCHE DU COEFFICIENT DE POISSON ET DU MODULE DE YOUNG
C
        CALL RCVALA ( ZI(IMATE), ' ', 'ELAS', 1, NOMPAR, VALPAR,
     &                NBRE4, NOMRE4, VALRE4, CODME4, 'FM' )
C
        IF (( CODME4(1).EQ.'OK' ).AND. ( CODME4(2).EQ.'OK' )) THEN
          CYOUNG = VALRE4(1)
          POISSO = VALRE4(2)
        ELSE
          CALL U2MESK('F','ELEMENTS4_71',1,NOMRE4(1)//NOMRE4(2))
        ENDIF
C
C 4.4. ON CALCULE L'INVERSE DU MODULE DE BIOT
C
        IF ( CYOUNG .GT. OVFL ) THEN
          UNSURM = 3.D0*(BIOT-POROSI)*(1.D0-BIOT)*(1.D0-2*POISSO)/CYOUNG

        ELSE
          CALL U2MESS('F','ELEMENTS4_67')
        ENDIF
C
      ENDIF

C--------------------------------------------------------------------
C 5. INITIALISATION DES FORCES
C--------------------------------------------------------------------
C 5.1. FORCES DE PESANTEUR :
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
C 5.2. FORCES DE ROTATION
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
C 6. RECUPERATION DES GRANDEURS CARACTERISTIQUES
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
C CALCUL DU DIAMETRE HT DE L'ELEMENT T
C
      NIV = 1
      CALL UTHK(NOMTE,IGEOM,HT,NDIM,IBID,IBID,IBID,IBID,NIV,IBID)
C
      CALL ERHMV2(LAXI      , PERMAN    , DELTAT  , DIMDEP,
     &            DIMDEF    , NMEC      , NP1     , NP2   , NDIM  ,
     &            NNO       , NNOS      , NNOM    , NPI   ,
     &            NPG       , NDDLS     , NDDLM   , DIMUEL,
     &            IPOIDS    , IVF       , IDFDE   , IPOID2, IVF2  ,
     &            IDFDE2    , ZR(IGEOM) , ZR(IFOR),
     &            ZR(IDEPLP), ZR(IDEPLM),
     &            ZR(ISIENP), ZR(ISIENM), NBCMP   , BIOT  , UNSURM,
     &            FPX       , FPY       , FRX     , FRY   ,
     &            YAMEC     , ADDEME    , YAP1    , ADDEP1,
     &            YAP2      , ADDEP2    , YATE    , ADDETE, TM2H1V )

C ON ADIMENSIONNE LES INDICATEURS VOLUMIQUES
C
      ADMEC = 1.D0/(PRESC**2*LONGC**NDIM)
      TSIVOM =  HT**2 * ADMEC * TM2H1V(1)
C
      IF ( .NOT. PERMAN ) THEN
C
        TDEVOM = HT**2 * ADMEC * TM2H1V(2)
C
        ADV1H     = CYOUNG*UNSURK*ADMEC
        TSIVOH = DELTAT * HT**2 * ADV1H * TM2H1V(3)
C
      ENDIF
C
C------------------------------------------------------------------
C 2. CALCUL DES TERMES SURFACIQUES
C------------------------------------------------------------------
C 2.1. PHASE DE PREPARATION : ON RECUPERE LES ADRESSES NECESSAIRES
C                             AUX CALCULS
C -----------------------------------------------------------------
C
C ON RECUPERE LES ADRESSES
C    1. VOISINS
C    2. CHARGEMENTS DE TYPE FORCE_FACE
C    3. CHARGEMENTS DE TYPE PRES_REP
      CALL JEVECH('PVOISIN','L',IVOIS)
      CALL JEVECH('PFORCE','L',IREF1)
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
      DO 11 , II = 1 , 3
C
        TM2H1B(II) = 0.D0
        TM2H1S(II) = 0.D0
C
 11   CONTINUE
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
C --- CALCUL DES NORMALES, TANGENTES ET JACOBIENS AUX POINTS DE L'ARETE
C
        IAUX = INO
        CALL CALNOR ( '2D' , IAUX, NNO  , IBID , NBS, NBNA, ITAB,
     &                IGEOM, IBID,
     &                IBID , IBID, ORIEN,
     &                R8BID, JAC , NX   , NY   , NZ , TX  , TY  )
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

            CALL ERHMB2( PERMAN    , INO       , NBS   , NBNA  ,
     &                   NDIM      , THETA     , INSTPM, JAC   , NX,
     &                   NY        , TX        , TY    , NBCMP ,
     &                   ZR(IGEOM) , IVOIS     ,
     &                   ZR(ISIENP), ZR(ISIENM), ADSIP , IAGD  ,
     &                   ZI(IREF2) , IADE2     , IAVA2 , NCMPM2, IAPTM2,
     &                   IADE3     , IAVA3     , NCMPM3, IAPTM3, TM2H1B)
C
          ELSE IF (FORMV.EQ.'TR'.OR.FORMV.EQ.'QU') THEN
C ----------------------------------------------------------------
C 2.5. CALCUL DES TERMES DE SAUT A TRAVERS LES FACES INTERIEURES
C DE LA MAILLE
C ----------------------------------------------------------------
            CALL ERHMS2( PERMAN    , INO      , NBS       , NBNA ,
     &                   THETA     , JAC      , NX        , NY   ,
     &                   ZR(ISIENP), ADSIP    , ZR(ISIENM), NBCMP,
     &                   TYPMAV    , ZI(IREF1), ZI(IREF2) , IVOIS,
     &                   TM2H1S                                   )
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
      DENOMI = PRESC**2*LONGC**(NDIM-2)*RHOLIQ**2
      ADHY0  = UNSURK**2/DENOMI
      ADHY1  = ADHY0/(LONGC**2)
C
      TSISAM = HT * ADMEC * TM2H1S(1)
      TSIBOM = HT * ADMEC * TM2H1B(1)
C
      IF (PERMAN) THEN
C
        TSIBSH = HT * ADHY0 * TM2H1B(3)
        TSIBBH = HT**3 * ADHY1 * TM2H1B(3)
C
        TSISSH = HT * ADHY0 * TM2H1S(3)
        TSISBH = HT**3 * ADHY1 * TM2H1S(3)
C
      ELSE
C
        CALL JEVECH('PERREM','L',IERRM)
C
        TDEBOM = HT * ADMEC * TM2H1B(2)
        TDESAM = HT * ADMEC * TM2H1S(2)
C
        ADHYMD = DELTAT * HT * CYOUNG * PERMIN/VISCLI * ADHY1
        TSIBSH = ADHYMD * TM2H1B(3)
        TSISSH = ADHYMD * TM2H1S(3)
C
      ENDIF
C
C ON STOCKE LES INDICATEURS
C
      CALL JEVECH('PERREUR','E',IERRE)
C
      IF ( PERMAN ) THEN
C
        ZR(IERRE  ) =   SQRT(TSIVOM + TSIBOM + TSISAM)
     &                + SQRT(TSIBSH + TSISSH)
        ZR(IERRE+1) =   SQRT(TSIVOM + TSIBOM + TSISAM)
     &                + SQRT(TSIBBH + TSISBH)
        ZR(IERRE+2) = TSIBSH + TSISSH
        ZR(IERRE+3) = TSIVOM + TSIBOM + TSISAM
        ZR(IERRE+4) = TSIBBH + TSISBH
C
      ELSE
C
        ZR(IERRE+1) = TSIVOM + TSISAM + TSIBOM
        ZR(IERRE+2) = TDEVOM + TDEBOM + TDESAM
        ZR(IERRE+3) = TSIVOH + TSIBSH + TSISSH
C
        ZR(IERRE+4) = MAX(ZR(IERRM+4),SQRT(ZR(IERRE+1)))
        ZR(IERRE+5) = ZR(IERRM+5) + SQRT(ZR(IERRE+2))
        ZR(IERRE+6) = SQRT(ZR(IERRM+6)**2+ZR(IERRE+3))
        ZR(IERRE)   = ZR(IERRE+4)+ZR(IERRE+5)+ZR(IERRE+6)
C
      ENDIF
C
      CALL JEDEMA()
C
      END
