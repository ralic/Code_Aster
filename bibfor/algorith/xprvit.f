      SUBROUTINE XPRVIT(NOMA,FISS,NDIM,NVIT,NBETA,LCMIN,CNSVT,CNSVN,
     &                  VPOINT,CNSBL,CNSDIS,DISFR,CNSBET,LISTP,DAMAX,
     &                  LOCDOM,RDIMP,RDTOR,DELTA,UCNSLT,UCNSLN)

      IMPLICIT NONE
      CHARACTER*8    NOMA,FISS

      CHARACTER*19   CNSVT,CNSVN,VPOINT,DISFR,CNSBL,CNSDIS,CNSBET,LISTP,
     &               DELTA,UCNSLT,UCNSLN
      CHARACTER*24   NVIT,NBETA
      INTEGER        NDIM
      REAL*8         LCMIN,DAMAX,RDIMP,RDTOR
      LOGICAL        LOCDOM

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/10/2011   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE MASSIN P.MASSIN
C     ------------------------------------------------------------------
C
C       XPRVIT   : X-FEM PROPAGATION : EXTENSION DU CHAMP DE VITESSES
C       ------     -     --                                  ---
C    CALCUL DE LA VITESSE DE PROPAGATION DE FISSURE SUR LE FOND
C    ET EXTENSION DU CHAMP DE VITESSE A TOUS LES NOEUDS DU MAILLAGE
C
C    ENTREE
C        NOMA    : NOM DU CONCEPT MAILLAGE
C        FISS    : NOM DU CONCEPT FISSURE X-FEM
C                  (FISSURE INITIALE DONT ON EXTRAIT LE FOND DE FISSURE)
C        NDIM    : DIMENSION DU MODELE (2=2D OU 3=3D)
C        NVIT    : VECTEUR DES VITESSES DE PROPAGATION POUR CHAQUE POINT
C                  DU FOND DE LA FISSURE (NOM DU CONCEPT)
C        NBETA   : VECTEUR DES ANGLES DE PROPAGATION POUR CHAQUE POINT
C                  DU FOND DE LA FISSURE (NOM DU CONCEPT)
C        LCMIN   : LONGUEUR DE LA PLUS PETITE ARETE DU MAILLAGE
C        DAMAX   : AVANCEMEMT MAXIMAL DE LA FISSURE
C        RADIMP  : RAYON DE LOCALISATION DES LEVEL SETS
C        LOCDOM  : LOCALISATION DES LEVELS SETS ACTIVEe
C
C    SORTIE
C        CNSVT   : CHAM_NO_S VITESSE TANGENTIELLE DE PROPAGATION
C        CNSVN   : CHAM_NO_S VITESSE NORMALE DE PROPAGATION
C        VPOINT  : VECTEUR DES VITESSES DE PROPAGATION EN CHAQUE POINT
C                  DU DOMAINE DE CALCUL (MODULE DE LA VITESSE DU POINT
C                  PROJETE SUR LE FOND DE LA FISSURE)
C        CNSBL   : CHAM_NO_S BASE LOCALE POUR CHAQUE NODE DU MAILLAGE
C                  (AXE NORMALE ET AXE TANGENTE AU PLANE DE LA FISSURE)
C        CNSDIS  : CHAM_NO_S VECTEUR DISTANCE ENTRE CHAQUE NODE DU
C                  MAILLAGE ET SON PROJECTION SUR LE FOND DE FISSURE
C        DISFR   : VECTEUR INDIQUANT LA DISTANCE^2 ENTRE CHAQUE NODE DU
C                  MAILLAGE NOMA ET LE FOND DU FISSURE
C        CNSBET  : VECTEUR DES ANGLES DE BIFURCATION DE LA FISSURE
C                  EN CHAQUE POINT DU DOMAINE DE CALCUL (ANGLE AU POINT
C                  PROJETE SUR LE FOND DE LA FISSURE)
C        LISTP   : VECTEUR (A 3 COMPOSANTES) OU LES CORDONNEES DU
C                  PROJETE DE CHAQUE POINT DU DOMAINE DE CALCUL SUR LE
C                  FOND DE LA FISSURE SONT STOCKEES
C       RADIMP   : RAYON DE LA ZONE DE REACTUALISATION DES LEVELS SETS
C       RADTOR   : RAYON DE LA ZONE DE REPROJECTION DES LEVELS SETS
C       DAMAX    : AVANCEMENT MAXIMUM DU FRONT DE FISSURE
C       DELTA    : VECTEUR CONTENANT LES CORRECTION DES LEVELS SETS 
C                 TANGENTES ET NORMALES
C       UCNLSN   : CHAM_NO_S  LEVEL SET NORMALE AU NOEUDS
C       UCNLSN   : CHAM_NO_S  LEVEL SET TANGENTE AU NOEUDS
C
C     ------------------------------------------------------------------
C TOLE CRP_20 CRP_6 CRP_21

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      INTEGER        I,J,JCOOR,IRET,NBNO,JMIN,NBPTFF,IBID,JDELTA,LSN,
     &               LST,JFONF,JVTFF,JVNFF,JVTL,JVTV,JVNL,JVNV,IFM,
     &               NIV,JVIT,JBETA,JDISFR,CFV,BFV,VFV,AFV,NFV
      REAL*8         EPS,XM,YM,ZM,R8MAEM,DMIN,SMIN,
     &               XI1,YI1,ZI1,XJ1,YJ1,ZJ1,XIJ,YIJ,ZIJ,XIM,YIM,ZIM,S,
     &               NORM2,XN,YN,ZN,D,R8PREM,RADIMP,RADTOR   
      CHARACTER*8    K8B,TYPCMP(6),METHOD
      INTEGER        JVFF,JBASEF,JBL,JDIS,K

      REAL*8         BAST(3),TAST(3),N(3),T(3),B(3),MTAST,PI(3),
     &               NORMIJ,LSNTH(2),LSTTH(2),NORMKL,MODNOR,MODTAN
      LOGICAL        GRILLE,FONVIR,FVIRTU

      CHARACTER*19   COVIR,BAVIR,VITVIR,ANGVIR,NUMVIR              

C     EULER AXIS AND EULER ANGLE CALCULATIONS
      INTEGER        JEULER,JCNSB,JLISTP,JVP
      REAL*8         NI(3),TI(3),BI(3),NJ(3),TJ(3),BJ(3),RIJ(3,3),
     &               TPL(3),NPL(3),BPL(3),AXEUL(3),CALFA,SALFA,MODVEC
      REAL*8         T0,T180,ALFA
      PARAMETER      (T0 = 0.5D0/180.D0*3.1415D0)
      PARAMETER      (T180 = 179.5D0/180.D0*3.1415D0)
      LOGICAL        ENDPNT

C     MULTIPLE CRACK FRONTS
      INTEGER        JFMULT,NUMFON,FON

C     BISECTION METHOD AND VELOCITY INTERPOLATION
      REAL*8         TOLLD,DPREC,DS,VP,BETAP
      INTEGER        MAXITE,JLIMSX,JLIMDX
      INTEGER      IARG

C-----------------------------------------------------------------------
C     DEBUT
C-----------------------------------------------------------------------
     
C     Recuperation des points des caracteristique du maillage et du
C     fond de fissure

      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)

      RADTOR = RDTOR
      RADIMP = RDIMP

C     RECUPERATION DE LA METHODE DE REINITIALISATION A EMPLOYER
      CALL GETVTX(' ','METHODE',1,IARG,1,METHOD,IBID)

C     RECUPERATION DES CARACTERISTIQUES DU MAILLAGE
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,K8B,IRET)
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)

C     RECUPERATION DU FOND DE FISSURE
      CALL JEVEUO(FISS//'.FONDFISS','L',JFONF)
      CALL DISMOI('F','NB_POINT_FOND',FISS,'FISS_XFEM',NBPTFF,K8B,IRET)

C     RETRIEVE THE DIFFERENT PIECES OF THE CRACK FRONT
      CALL JEVEUO(FISS//'.FONDMULT','L',JFMULT)
      CALL DISMOI('F','NB_FOND',FISS,'FISS_XFEM',NUMFON,K8B,IRET)

C     RETRIEVE THE LOCAL REFERENCE SYSTEM FOR EACH NODE ON THE FRONT
      CALL JEVEUO(FISS//'.BASEFOND','E',JBASEF)

C     RETRIEVE THE CRACK'S SPEED AND PROPAGATION ANGLE FOR EACH NODE ON
C     THE FRONT
      CALL JEVEUO(NVIT,'E',JVIT)
      CALL JEVEUO(NBETA,'E',JBETA)

      FVIRTU = .FALSE.

C     CHECK if an auxilliary grid is used 

      CALL JEEXIN(FISS//'.GRI.MODELE',IBID)
      IF (IBID.EQ.0) THEN
C        NO AUXILIARY GRID USED
         GRILLE=.FALSE.
      ELSE
         GRILLE=.TRUE.
      ENDIF
      
C      On utilise un fond virtuel si une grille auxilliaire 
C      est utilisee et que l on travaille sur un modele 3D

      IF ((GRILLE).AND.(NDIM.EQ.3).AND.(METHOD.NE.'GEOMETRI')) THEN

        FVIRTU = .TRUE.

C     EXTRACTION des valeurs des levels sets au noeud
        CALL JEVEUO(UCNSLN//'.CNSV','L',LSN)
        CALL JEVEUO(UCNSLT//'.CNSV','L',LST)   

C     Creation de vecteur ou sont stocke les coordonnes et les
C     bases associees du font de fissure virtuel
        COVIR='&&XPRVIT.COVIR'
        BAVIR='&&XPRVIT.BAVIR'
        VITVIR='&&XPRVIT.VITVIR'
        ANGVIR='&&XPRVIT.ANGVIR'
        NUMVIR='&&XPRVIT.NUMVIR'

         CALL WKVECT(COVIR,'V V R8',
     &                4*(NBPTFF+2*NUMFON),CFV)
         CALL WKVECT(BAVIR,'V V R8',
     &             6*(NBPTFF+2*NUMFON),BFV)
         CALL WKVECT(VITVIR,'V V R8',
     &               (NBPTFF+2*NUMFON),VFV)
         CALL WKVECT(ANGVIR,'V V R8',
     &                   (NBPTFF+2*NUMFON),AFV)
         CALL WKVECT(NUMVIR,'V V I',
     &                   (2*NUMFON),NFV)

C      OPERATION SUR RADIMP ET RADTOR
         IF (RADIMP.GT.0.D0) THEN
               RADIMP=SQRT(RADIMP)
         ENDIF
         IF (RADTOR.GT.0.D0) THEN
               RADTOR=SQRT(RADTOR)
         ENDIF

            
         IF (NUMFON.GT.1) THEN
C        ON VERIFIE QUE LE FOND DE FISSURE EST BIEN ORIENTE
C        SINON ON MODIFIE LA NUMEROTATION DU FOND DE FISSURE 
           CALL  XPRFON(NOMA,FISS,NUMFON,NVIT,NBETA)
         ENDIF 

C       DEFINITION DU FOND VIRTUEL                 
         CALL  XPRVIR(FISS,COVIR,BAVIR,VITVIR,ANGVIR,NUMVIR,NUMFON,
     &          NVIT,NBETA,NBPTFF,RADIMP,RADTOR,DAMAX,NOMA,LOCDOM)  

C       ON POINTE LES ROUTINES DESTINEES AU FRONT PHYSIQUE
C       SUR LE FRONT VIRTUEL
            JFONF=CFV
            JBASEF=BFV
            JVIT=VFV
            JBETA=AFV
            JFMULT=NFV 
      ENDIF

C     CREATE THE CHAMP_NO_S WHERE THE LOCAL REFERENCE SYSTEM IS
C     STORED FOR EACH NODE IN THE MESH.
C     CREATE ALSO THE CHAMP_NO_S WHERE THE DISTANCE BETWEEN EACH
C     NODE AND ITS PROJECTION ON THE CRACK FRONT IS STORED.
      TYPCMP(1)='X1'
      TYPCMP(2)='X2'
      TYPCMP(3)='X3'
      TYPCMP(4)='X4'
      TYPCMP(5)='X5'
      TYPCMP(6)='X6'
      IF (NDIM.EQ.2) THEN
C        2D CASE: EACH VECTOR IN THE FIELD HAS 2 COMPONENTS ONLY
         CALL CNSCRE(NOMA,'NEUT_R',4,TYPCMP,'V',CNSBL)
         CALL CNSCRE(NOMA,'NEUT_R',4,TYPCMP,'V',CNSDIS)
      ELSE
C        3D CASE: EACH VECTOR IN THE FIELD HAS 3 COMPONENTS
         CALL CNSCRE(NOMA,'NEUT_R',6,TYPCMP,'V',CNSBL)
         CALL CNSCRE(NOMA,'NEUT_R',6,TYPCMP,'V',CNSDIS)
      ENDIF

      CALL JEVEUO(CNSBL//'.CNSV','E',JBL)
      CALL JEVEUO(CNSDIS//'.CNSV','E',JDIS)

C     CREATION DES CHAM_NO_S CONTENANT LES COMPOSANTES DU VECTEUR V
C     (VT & VN)
      CALL CNSCRE(NOMA,'NEUT_R',1,'X1','V',CNSVT)
      CALL CNSCRE(NOMA,'NEUT_R',1,'X1','V',CNSVN)

C     CREATE THE VECTOR WHERE THE MODULE OF THE PROPAGATION SPEED IS
C     STORED FOR EACH POINT (=SQRT(CNSVT**2+CNSVN**2))
      CALL WKVECT(VPOINT,'V V R8',NBNO,JVP)

C     CREATION DES VECTEURS DE VITESSE DE PROPAGATION EN FOND
C     DE FISSURE

      CALL WKVECT('&&XPRVIT.V_PROPA_FF','V V R8',NDIM*NBPTFF,JVFF)
      CALL WKVECT('&&XPRVIT.VT_PROPA_FF','V V R8',NBPTFF,JVTFF)
      CALL WKVECT('&&XPRVIT.VN_PROPA_FF','V V R8',NBPTFF,JVNFF)

C     CREATE THE VECTOR WHERE THE DISTANCE BETWEEN EACH NODE AND THE
C     CRACK FRONT IS STORED
      CALL WKVECT(DISFR,'V V R8',NBNO,JDISFR)

C     CREATE THE VECTOR WHERE THE PROPAGATION ANGLE IS STORED FOR EACH
C     POINT
      CALL WKVECT(CNSBET,'V V R8',NBNO,JCNSB)

C     CREATE THE VECTOR WHERE THE COORDINATES OF THE PROJECTED POINT
C     ARE STORED FOR EACH POINT
      CALL WKVECT(LISTP,'V V R8',3*NBNO,JLISTP)

      CALL JEVEUO(CNSVT//'.CNSV','E',JVTV)
      CALL JEVEUO(CNSVT//'.CNSL','E',JVTL)
      CALL JEVEUO(CNSVN//'.CNSV','E',JVNV)
      CALL JEVEUO(CNSVN//'.CNSL','E',JVNL)

C     CREATE THE VECTOR WHERE THE EULER AXIS AND THE EULER ANGLE ARE
C     STORED. THESE OBJECTS ARE STORED IN THE FOLLOWING ORDER:
C     ELEMENT 1: EULER ANGLE FOR THE FIRST POINT ON THE CRACK FRONT
C     ELEMENT 2: X COMPONENT OF THE EULER AXIS FOR THE FIRST POINT ON
C                THE CRACK FRONT
C     ELEMENT 3: Y COMPONENT OF THE EULER AXIS FOR THE FIRST POINT ON
C                THE CRACK FRONT
C     ELEMENT 4: Z COMPONENT OF THE EULER AXIS FOR THE FIRST POINT ON
C                THE CRACK FRONT
C     ELEMENT 5: X COMPONENT OF THE LOCAL BASE FOR THE FIRST POINT ON
C                THE CRACK FRONT
C     ELEMENT 6: Y COMPONENT OF THE LOCAL BASE FOR THE FIRST POINT ON
C                THE CRACK FRONT
C     ELEMENT 7: Z COMPONENT OF THE LOCAL BASE FOR THE FIRST POINT ON
C                THE CRACK FRONT
C     THIS BLOCK IS REPEATED FOR ALL THE POINTS ON THE CRACK FRONT WITH
C     THE EXCEPTION OF THE LAST ONE.
      CALL WKVECT('&&XPRVIT.EULER','V V R8',7*NBPTFF,JEULER)

C     creation du vecteur contenant les modifications a apporter au
C     level set avant application du fond virtuel
       CALL WKVECT(DELTA,'V V R8',2*NBNO,JDELTA)

C    Initialisation du vecteur delta
        DO 742 I=1,2*NBNO
           ZR(JDELTA+I-1)=0.D0     
 742   CONTINUE

C ***************************************************************
C ELABORATE EACH POINT ON THE CRACK FRONT IN ORDER TO CALCULATE
C THE FOLLOWING:
C - PROPAGATION SPEED VECTOR V AND ITS NORMAL AND TANGENTIAL COMPONENTS
C - PROPAGATION ANGLE
C - EULER AXIS AND ANGLE BETWEEN TWO CONSECUTIVE REFERENCE SYSTEMS ON
C   THE CRACK FRONT
C ***************************************************************

      DO 100 I=1,NBPTFF

C        ***************************************************************
C        CALCULATE THE NORMAL AND THE TANGENTIAL PROPAGATION SPEED AT
C        THE ACTUAL POINT ON THE CRACK FRONT
C        ***************************************************************

         ZR(JVTFF-1+I)=ZR(JVIT-1+I)*COS(ZR(JBETA-1+I))
         ZR(JVNFF-1+I)=ZR(JVIT-1+I)*SIN(ZR(JBETA-1+I))

C        ***************************************************************
C        RECALCULATE THE LOCAL REFERENCE SYSTEM IN THE ACTUAL CRACK
C        FRONT POINT IN ORDER TO BE SURE THAT THE THREE AXES ARE
C        ORTHOGONAL EACH OTHER
C        ***************************************************************

         IF(NDIM.EQ.2) THEN
C          NORMAL AXIS (NORMAL TO THE CRACK PLANE)
           N(1) = ZR(JBASEF-1+2*NDIM*(I-1)+1)
           N(2) = ZR(JBASEF-1+2*NDIM*(I-1)+2)
           N(3) = 0
C          TANGENTIAL AXIS (TANGENT TO THE CRACK PLANE)
           T(1) = ZR(JBASEF-1+2*NDIM*(I-1)+3)
           T(2) = ZR(JBASEF-1+2*NDIM*(I-1)+4)
           T(3) = 0
         ELSE
C          NORMAL AXIS
           N(1) = ZR(JBASEF-1+2*NDIM*(I-1)+1)
           N(2) = ZR(JBASEF-1+2*NDIM*(I-1)+2)
           N(3) = ZR(JBASEF-1+2*NDIM*(I-1)+3)
C          TANGENTIAL AXIS
           T(1) = ZR(JBASEF-1+2*NDIM*(I-1)+4)
           T(2) = ZR(JBASEF-1+2*NDIM*(I-1)+5)
           T(3) = ZR(JBASEF-1+2*NDIM*(I-1)+6)
         ENDIF

C        CALCULATE THE BINORMAL AXIS AS THE VECTORIAL PRODUCT BETWEEN
C        THE TANGENTIAL AND NORMAL AXES. THIS AXIS IS TANGENT TO THE
C        CRACK FRONT.
         BAST(1) = N(2)*T(3)-N(3)*T(2)
         BAST(2) = N(3)*T(1)-N(1)*T(3)
         BAST(3) = N(1)*T(2)-N(2)*T(1)

C        RECALCULATE THE TANGENTIAL AXIS AS THE VECTORIAL PRODUCT
C        BETWEEN THE BINORMAL AND THE NORMAL AXES.
         TAST(1) = BAST(2)*N(3)-BAST(3)*N(2)
         TAST(2) = BAST(3)*N(1)-BAST(1)*N(3)
         TAST(3) = BAST(1)*N(2)-BAST(2)*N(1)

C        CALCULATE THE MODULE OF THE NEW TANGENTIAL AXIS
         MTAST = (TAST(1)**2.D0+TAST(2)**2.D0+TAST(3)**2.D0)**0.5D0

C        THE MODULE OF THE NEW TANGENTIAL AXIS SHOULD ALWAYS BE GREATER
C        THAN ZERO... EXCEPT IN THE CASE THE NORMAL AND TANGENTIAL AXIS
C        ARE PARALLEL! IN THIS CASE SOMETHING NASTY HAS HAPPENED TO THE
C        LEVEL SETS (OUTSIDE THIS SUBROUTINE)!
         CALL ASSERT(MTAST.GT.0.D0)

C        CALCULATE THE UNIT VECTOR FOR THE NEW TANGENTIAL AXIS AND STORE
C        IT
         IF(NDIM.EQ.2) THEN
            ZR(JBASEF-1+2*NDIM*(I-1)+3) = TAST(1)/MTAST
            ZR(JBASEF-1+2*NDIM*(I-1)+4) = TAST(2)/MTAST
         ELSE
            ZR(JBASEF-1+2*NDIM*(I-1)+4) = TAST(1)/MTAST
            ZR(JBASEF-1+2*NDIM*(I-1)+5) = TAST(2)/MTAST
            ZR(JBASEF-1+2*NDIM*(I-1)+6) = TAST(3)/MTAST
         ENDIF

C        CALCULATE THE UNIT VECTOR FOR THE NORMAL AXIS AND STORE IT
         MTAST = (N(1)**2.D0+N(2)**2.D0+N(3)**2.D0)**0.5D0
         IF(NDIM.EQ.2) THEN
            ZR(JBASEF-1+2*NDIM*(I-1)+1) = N(1)/MTAST
            ZR(JBASEF-1+2*NDIM*(I-1)+2) = N(2)/MTAST
         ELSE
            ZR(JBASEF-1+2*NDIM*(I-1)+1) = N(1)/MTAST
            ZR(JBASEF-1+2*NDIM*(I-1)+2) = N(2)/MTAST
            ZR(JBASEF-1+2*NDIM*(I-1)+3) = N(3)/MTAST
         ENDIF

C        ***************************************************************
C        EVALUATE THE PROPAGATION SPEED VECTOR V AT THE POINT
C        ***************************************************************

C        CALCULATE THE MODULE OF THE NORMAL AND TANGENTIAL AXES FOR THE
C        LOCAL REFERENCE SYSTEM AT THE POINT
         IF (NDIM.EQ.2) THEN
            MODNOR = SQRT(ZR(JBASEF-1+2*NDIM*(I-1)+1)**2.D0+
     &                    ZR(JBASEF-1+2*NDIM*(I-1)+2)**2.D0)
            MODTAN = SQRT(ZR(JBASEF-1+2*NDIM*(I-1)+3)**2.D0+
     &                    ZR(JBASEF-1+2*NDIM*(I-1)+4)**2.D0)
         ELSE
            MODNOR = SQRT(ZR(JBASEF-1+2*NDIM*(I-1)+1)**2.D0+
     &                    ZR(JBASEF-1+2*NDIM*(I-1)+2)**2.D0+
     &                    ZR(JBASEF-1+2*NDIM*(I-1)+3)**2.D0)
            MODTAN = SQRT(ZR(JBASEF-1+2*NDIM*(I-1)+4)**2.D0+
     &                    ZR(JBASEF-1+2*NDIM*(I-1)+5)**2.D0+
     &                    ZR(JBASEF-1+2*NDIM*(I-1)+6)**2.D0)
         ENDIF

         CALL ASSERT(MODNOR.GT.0.D0)
         CALL ASSERT(MODTAN.GT.0.D0)

C        CALCULATE THE PROPAGATION SPEED VECTOR V IN THE GLOBAL
C        REFERENCE SYSTEM USED FOR THE MESH
         DO 50 J=1,NDIM
            ZR(JVFF-1+NDIM*(I-1)+J) =
     &            ZR(JVNFF-1+I)*ZR(JBASEF-1+2*NDIM*(I-1)+J)/MODNOR+
     &            ZR(JVTFF-1+I)*ZR(JBASEF-1+2*NDIM*(I-1)+J+NDIM)/MODTAN
50       CONTINUE

C        ***************************************************************
C        EVALUATE THE EULER AXIS AND ANGLE FOR THE ROTATION BETWEEN
C        THE LOCAL BASE IN THE PREVIOUS POINT AND THE LOCAL BASE IN THE
C        ACTUAL POINT
C        ***************************************************************

         IF ((NDIM.EQ.3).AND.(I.GT.1)) THEN

C           RETRIEVE THE LOCAL BASE FOR THE PREVIOUS POINT
            NI(1) = ZR(JBASEF-1+2*NDIM*(I-1-1)+1)
            NI(2) = ZR(JBASEF-1+2*NDIM*(I-1-1)+2)
            NI(3) = ZR(JBASEF-1+2*NDIM*(I-1-1)+3)
            TI(1) = ZR(JBASEF-1+2*NDIM*(I-1-1)+4)
            TI(2) = ZR(JBASEF-1+2*NDIM*(I-1-1)+5)
            TI(3) = ZR(JBASEF-1+2*NDIM*(I-1-1)+6)
C           THE BINORMAL AXIS MUST BE CALCULATED AS THE VECTORIAL
C           PRODUCT BETWEEN TANGENTIAL AND NORMAL AXES (T,N,B)
            BI(1) = TI(2)*NI(3)-TI(3)*NI(2)
            BI(2) = TI(3)*NI(1)-TI(1)*NI(3)
            BI(3) = TI(1)*NI(2)-TI(2)*NI(1)

C           STORE THE BINORMAL AXIS
            ZR(JEULER-1+7*(I-1-1)+5) = BI(1)
            ZR(JEULER-1+7*(I-1-1)+6) = BI(2)
            ZR(JEULER-1+7*(I-1-1)+7) = BI(3)

C           RETRIEVE THE LOCAL BASE FOR THE ACTUAL POINT
            NJ(1) = ZR(JBASEF-1+2*NDIM*(I-1)+1)
            NJ(2) = ZR(JBASEF-1+2*NDIM*(I-1)+2)
            NJ(3) = ZR(JBASEF-1+2*NDIM*(I-1)+3)
            TJ(1) = ZR(JBASEF-1+2*NDIM*(I-1)+4)
            TJ(2) = ZR(JBASEF-1+2*NDIM*(I-1)+5)
            TJ(3) = ZR(JBASEF-1+2*NDIM*(I-1)+6)
C           THE BINORMAL AXIS MUST BE CALCULATED AS THE VECTORIAL
C           PRODUCT BETWEEN TANGENTIAL AND NORMAL AXES (T,N,B)
            BJ(1) = TJ(2)*NJ(3)-TJ(3)*NJ(2)
            BJ(2) = TJ(3)*NJ(1)-TJ(1)*NJ(3)
            BJ(3) = TJ(1)*NJ(2)-TJ(2)*NJ(1)

C           CALCULATE THE ROTATION MATRIX
            RIJ(1,1) = TJ(1)*TI(1)+TJ(2)*TI(2)+TJ(3)*TI(3)
            RIJ(2,1) = TJ(1)*NI(1)+TJ(2)*NI(2)+TJ(3)*NI(3)
            RIJ(3,1) = TJ(1)*BI(1)+TJ(2)*BI(2)+TJ(3)*BI(3)

            RIJ(1,2) = NJ(1)*TI(1)+NJ(2)*TI(2)+NJ(3)*TI(3)
            RIJ(2,2) = NJ(1)*NI(1)+NJ(2)*NI(2)+NJ(3)*NI(3)
            RIJ(3,2) = NJ(1)*BI(1)+NJ(2)*BI(2)+NJ(3)*BI(3)

            RIJ(1,3) = BJ(1)*TI(1)+BJ(2)*TI(2)+BJ(3)*TI(3)
            RIJ(2,3) = BJ(1)*NI(1)+BJ(2)*NI(2)+BJ(3)*NI(3)
            RIJ(3,3) = BJ(1)*BI(1)+BJ(2)*BI(2)+BJ(3)*BI(3)

C           CALCULATE THE EULER ANGLE
            ALFA = 0.5D0*(RIJ(1,1)+RIJ(2,2)+RIJ(3,3)-1)
            IF (ALFA.GT.1.D0) ALFA=1.D0
            IF (ALFA.LT.-1.D0) ALFA=-1.D0
            ALFA = ACOS(ALFA)
            ZR(JEULER-1+7*(I-1-1)+1) = ALFA

C           CALCULATE THE EULER VECTOR
            IF ((ALFA.GT.T0).AND.(ALFA.LT.T180)) THEN
            ZR(JEULER-1+7*(I-1-1)+2) =
     &                               0.5D0*(RIJ(2,3)-RIJ(3,2))/SIN(ALFA)
            ZR(JEULER-1+7*(I-1-1)+3) =
     &                               0.5D0*(RIJ(3,1)-RIJ(1,3))/SIN(ALFA)
            ZR(JEULER-1+7*(I-1-1)+4) =
     &                               0.5D0*(RIJ(1,2)-RIJ(2,1))/SIN(ALFA)
            ENDIF

         ENDIF

100   CONTINUE

C ***************************************************************
C ELABORATE EACH NODE IN THE MESH IN ORDER TO CALCULATE THE FOLLOWING:
C - PROPAGATION SPEED VECTOR
C - LOCAL REFERENCE SYSTEM (NORMAL AND TANGENTIAL AXES WITH RESPECT
C                           TO THE CRACK PLANE)
C - DISTANCE VECTOR BETWEEN THE NODE AND ITS PROJECTION ON THE FRONT
C ***************************************************************

C     ***************************************************************
C     EVALUATE THE PROJECTION OF EACH NODE ON THE CRACK FRONT
C     ***************************************************************

C     THESE ARE THE VALUES FOR THE 2D CASE. IN FACT ONLY FOR THE 3D CASE
C     THE PROJECTION OF THE VELOCITY IS NECESSARY. THE COORDINATES OF
C     THE CRACK TIP (ONLY ONE POINT!) FOR THE 2D CASE ARE STORED
C     IN XI1,YI1,ZI1.
      JMIN = 1
      SMIN = 0.D0

C     BOUCLE SUR LES NOEUDS M DU MAILLAGE POUR CALCULER PROJ(V)=V
      EPS = 1.D-12
      DO 200 I=1,NBNO

C        COORD DU NOEUD M DU MAILLAGE
         XM=ZR(JCOOR-1+(I-1)*3+1)
         YM=ZR(JCOOR-1+(I-1)*3+2)
         ZM=ZR(JCOOR-1+(I-1)*3+3)

C        THE PROJ(V)=V IS NEEDED ONLY FOR THE 3D CASE
         IF (NDIM.EQ.3) THEN
C          INITIALISATION
           DMIN = R8MAEM()
           JMIN = 0
           SMIN = 0.D0
C          BOUCLE SUR PT DE FONFIS
           DO 210 J=1,NBPTFF-1

             IF (.NOT.FVIRTU) THEN
C               CHECK IF THE CURRENT SEGMENT ON THE FRONT IS OUTSIDE
C               THE MODEL (ONLY IF THERE ARE MORE THAN ONE PIECE
C               FORMING THE FRONT)
                DO 213 FON=1,NUMFON
                   IF (J.EQ.ZI(JFMULT-1+2*FON)) GOTO 210
213             CONTINUE
             ENDIF

C            COORD PT I, ET J
             XI1 = ZR(JFONF-1+4*(J-1)+1)
             YI1 = ZR(JFONF-1+4*(J-1)+2)
             ZI1 = ZR(JFONF-1+4*(J-1)+3)
             XJ1 = ZR(JFONF-1+4*(J-1+1)+1)
             YJ1 = ZR(JFONF-1+4*(J-1+1)+2)
             ZJ1 = ZR(JFONF-1+4*(J-1+1)+3)
C            VECTEUR IJ ET IM
             XIJ = XJ1-XI1
             YIJ = YJ1-YI1
             ZIJ = ZJ1-ZI1
             XIM = XM-XI1
             YIM = YM-YI1
             ZIM = ZM-ZI1

C            PARAM S (PRODUIT SCALAIRE...)
             S   = XIJ*XIM + YIJ*YIM + ZIJ*ZIM
             NORM2 = XIJ*XIJ + YIJ*YIJ + ZIJ*ZIJ
             S     = S/NORM2
C            SI N=P(M) SORT DU SEGMENT
             IF((S-1).GE.EPS) S = 1.D0
             IF(S.LE.EPS)     S = 0.D0
C            COORD DE N
             XN = S*XIJ+XI1
             YN = S*YIJ+YI1
             ZN = S*ZIJ+ZI1
C            DISTANCE MN
C            SAVE CPU TIME: THE SQUARE OF THE DISTANCE IS EVALUATED!
             D = (XN-XM)*(XN-XM)+(YN-YM)*(YN-YM)+(ZN-ZM)*(ZN-ZM)
             IF(D.LT.DMIN) THEN
               DMIN = D
               JMIN = J
               SMIN = S
C              STORE THE DISTANCE VECTOR
               ZR(JDIS-1+3*(I-1)+1) = XM-XN
               ZR(JDIS-1+3*(I-1)+2) = YM-YN
               ZR(JDIS-1+3*(I-1)+3) = ZM-ZN
             ENDIF
 210       CONTINUE

         ELSE

             DMIN = R8MAEM()

C            2D CASE - ONLY ONE POINT AT THE CRACK TIP!
             DO 214 J=1,NBPTFF

                XI1 = ZR(JFONF-1+4*(J-1)+1)
                YI1 = ZR(JFONF-1+4*(J-1)+2)
                ZI1 = ZR(JFONF-1+4*(J-1)+3)

C               SAVE CPU TIME: THE SQUARE OF THE DISTANCE IS EVALUATED!
                D = (XI1-XM)*(XI1-XM)+(YI1-YM)*(YI1-YM)+
     &              (ZI1-ZM)*(ZI1-ZM)
                IF(D.LT.DMIN) THEN
                  DMIN = D
                  JMIN = J
C                 STORE THE DISTANCE VECTOR
                  ZR(JDIS-1+2*(I-1)+1) = XM-XI1
                  ZR(JDIS-1+2*(I-1)+2) = YM-YI1

C                 STORE THE PROJECTED POINT
                  ZR(JLISTP-1+3*(I-1)+1) = XI1
                  ZR(JLISTP-1+3*(I-1)+2) = YI1
                  ZR(JLISTP-1+3*(I-1)+3) = ZI1
                ENDIF

214          CONTINUE

         ENDIF

C        ***************************************************************
C        SMOOTH THE PROJECTION OF THE VELOCITY
C        ***************************************************************

         IF (NDIM.EQ.3) THEN

C           MAXIMUM NUMBER OF ITERATIONS
            MAXITE=25
C           INITIAL VALUE FOR DS
            DS= 2.0D-1
C           TOLERANCE TO CHECK THE CONVERGENCE
            TOLLD =  1.0D-2*LCMIN
C           POINT PROJECTED ON ONE END OF THE FRONT FLAG
            ENDPNT = .FALSE.

C           CALCULATE THE LIMITS FOR JMIN ON THE ACTUAL CRACK FRONT
            IF (.NOT.FVIRTU) THEN
               JLIMSX = 0
               JLIMDX = 0
               DO 205 FON=1,NUMFON
                  IF ((JMIN.GE.ZI(JFMULT-1+2*FON-1)).AND.
     &                (JMIN.LE.ZI(JFMULT-1+2*FON))) THEN
                     JLIMSX = ZI(JFMULT-1+2*FON-1)
                     JLIMDX = ZI(JFMULT-1+2*FON)-1
                     GOTO 204
                  ENDIF
205            CONTINUE
            ELSE
               JLIMSX = 1
               JLIMDX = ZI(JFMULT-1+2*NUMFON)-1
            ENDIF

            CALL ASSERT(2.GT.1)

204         CONTINUE

C           SEARCH THE PROJECTED POINT BY THE BISECTION METHOD
            DO 206 J=1,MAXITE

C              COORDINATES OF THE POINT AT THE END OF THE CRACK FRONT
C              SEGMENT
               XI1 = ZR(JFONF-1+4*(JMIN-1)+1)
               YI1 = ZR(JFONF-1+4*(JMIN-1)+2)
               ZI1 = ZR(JFONF-1+4*(JMIN-1)+3)
               XJ1 = ZR(JFONF-1+4*(JMIN-1+1)+1)
               YJ1 = ZR(JFONF-1+4*(JMIN-1+1)+2)
               ZJ1 = ZR(JFONF-1+4*(JMIN-1+1)+3)

C              VECTEUR IJ
               XIJ = XJ1-XI1
               YIJ = YJ1-YI1
               ZIJ = ZJ1-ZI1

C              RETREIVE THE LENGTH OF THE CRACK FRONT SEGMENT
               NORM2 = SQRT(XIJ*XIJ + YIJ*YIJ + ZIJ*ZIJ)

C              COORD DU NOEUD M DU MAILLAGE
               XM=ZR(JCOOR-1+(I-1)*3+1)
               YM=ZR(JCOOR-1+(I-1)*3+2)
               ZM=ZR(JCOOR-1+(I-1)*3+3)

C              CALCULATE THE EULER ANGLE FOR THE NODE
               ALFA = ZR(JEULER-1+7*(JMIN-1)+1)*SMIN

               IF ((ALFA.GT.T0).AND.(ALFA.LT.T180)) THEN

C                 CALCULATE COS(ALFA) AND SIN(ALFA) TO SPEED UP THE CODE
                  CALFA = COS(ALFA)
                  SALFA = SIN(ALFA)

C                 RETRIEVE THE EULER AXIS
                  AXEUL(1) = ZR(JEULER-1+7*(JMIN-1)+2)
                  AXEUL(2) = ZR(JEULER-1+7*(JMIN-1)+3)
                  AXEUL(3) = ZR(JEULER-1+7*(JMIN-1)+4)

C                 RETRIEVE THE LOCAL BASE IN THE PREVIOUS POINT ON THE
C                 FRONT (SMIN=0)
                  NI(1) = ZR(JBASEF-1+2*NDIM*(JMIN-1)+1)
                  NI(2) = ZR(JBASEF-1+2*NDIM*(JMIN-1)+2)
                  NI(3) = ZR(JBASEF-1+2*NDIM*(JMIN-1)+3)
                  TI(1) = ZR(JBASEF-1+2*NDIM*(JMIN-1)+4)
                  TI(2) = ZR(JBASEF-1+2*NDIM*(JMIN-1)+5)
                  TI(3) = ZR(JBASEF-1+2*NDIM*(JMIN-1)+6)
                  BI(1) = ZR(JEULER-1+7*(JMIN-1)+5)
                  BI(2) = ZR(JEULER-1+7*(JMIN-1)+6)
                  BI(3) = ZR(JEULER-1+7*(JMIN-1)+7)

C                 CALCULATE THE LOCAL BASE IN THE NODE WITH RESPECT TO
C                 THELOCAL BASE OF THE PREVIOUS POINT ON THE CRACK FRONT
C                 (SMIN=0)
                  BPL(1) = (1-CALFA)*AXEUL(1)*AXEUL(3)-AXEUL(2)*SALFA
                  BPL(2) = (1-CALFA)*AXEUL(2)*AXEUL(3)+AXEUL(1)*SALFA
                  BPL(3) = CALFA+(1-CALFA)*AXEUL(3)**2

C                 CALCULATE THE LOCAL BASE IN THE NODE WITH RESPECT TO
C                 THE GLOBAL REFERENCE SYSTEM OF THE MESH
                  B(1) = BPL(1)*TI(1)+BPL(2)*NI(1)+BPL(3)*BI(1)
                  B(2) = BPL(1)*TI(2)+BPL(2)*NI(2)+BPL(3)*BI(2)
                  B(3) = BPL(1)*TI(3)+BPL(2)*NI(3)+BPL(3)*BI(3)

C                 CALCULATE THE UNIT VECTOR FOR THE NORMAL AXIS
                  MODVEC = (B(1)**2+B(2)**2+B(3)**2)**0.5D0

                  CALL ASSERT(MODVEC.GT.R8PREM())

                  B(1) = B(1)/MODVEC
                  B(2) = B(2)/MODVEC
                  B(3) = B(3)/MODVEC

               ELSE

                  IF (ALFA.LT.T0) THEN
                      B(1) = ZR(JEULER-1+7*(JMIN-1)+5)
                      B(2) = ZR(JEULER-1+7*(JMIN-1)+6)
                      B(3) = ZR(JEULER-1+7*(JMIN-1)+7)
                  ENDIF

                  IF (ALFA.GT.T180) THEN
                      B(1) = ZR(JEULER-1+7*(JMIN-1+1)+5)
                      B(2) = ZR(JEULER-1+7*(JMIN-1+1)+6)
                      B(3) = ZR(JEULER-1+7*(JMIN-1+1)+7)
                  ENDIF

               ENDIF

C              COORD DE N
               XN = SMIN*XIJ+XI1
               YN = SMIN*YIJ+YI1
               ZN = SMIN*ZIJ+ZI1

C              DISTANCE OF POINT M TO THE PLANE N-T
               D=(XM-XN)*B(1)+(YM-YN)*B(2)+(ZM-ZN)*B(3)

               IF (ABS(D).LT.TOLLD) GOTO 207

C              INJECTION OF THE GOOD DPREC FOR THE FIRST ITERATION
               IF (J.EQ.1) THEN
                  DS=DS*SIGN(1.D0,D)*
     &               SIGN(1.D0,(B(1)*XIJ+B(2)*YIJ+B(3)*ZIJ))
                  DPREC=D
               ENDIF

C              CHANGE IN THE SEARCH DIRECTION
               IF ((D*DPREC).LT.0.D0) DS=DS*(-0.5D0)

C              UPDATE THE PROJECTED POINT POSITION ON THE FRONT
               SMIN=SMIN+DS

C              MANAGE THE CHANGING OF THE CRACK FRONT SEGMENT
               IF ((SMIN.LT.0.D0).AND.(JMIN.GT.JLIMSX)) THEN
                  JMIN=JMIN-1
                  SMIN=1.D0
               ELSE IF ((SMIN.LT.0.D0).AND.(JMIN.EQ.JLIMSX)) THEN
                  SMIN=0.D0
                  ENDPNT = .TRUE.
                  GOTO 207
               ENDIF

               IF ((SMIN.GT.1.D0).AND.(JMIN.LT.JLIMDX)) THEN
                  JMIN=JMIN+1
                  SMIN=0.D0
               ELSE IF ((SMIN.GT.1.D0).AND.(JMIN.EQ.JLIMDX)) THEN
                  SMIN=1.D0
                  ENDPNT = .TRUE.
                  GOTO 207
               ENDIF

               DPREC=D

206         CONTINUE

207         CONTINUE

C           CALCULATE THE PROJECTED POINT COORDINATES
            XN = SMIN*XIJ+XI1
            YN = SMIN*YIJ+YI1
            ZN = SMIN*ZIJ+ZI1
            D = (XN-XM)*(XN-XM)+(YN-YM)*(YN-YM)+(ZN-ZM)*(ZN-ZM)
            DMIN = D

C           STORE THE COORDINATES OF THE PROJECTED POINT
            ZR(JLISTP-1+3*(I-1)+1) = XN
            ZR(JLISTP-1+3*(I-1)+2) = YN
            ZR(JLISTP-1+3*(I-1)+3) = ZN

C           STORE THE DISTANCE VECTOR
            ZR(JDIS-1+3*(I-1)+1) = XM-XN
            ZR(JDIS-1+3*(I-1)+2) = YM-YN
            ZR(JDIS-1+3*(I-1)+3) = ZM-ZN
            ZR(JDISFR+I-1) = DMIN

C          SI ON UTILISE LA METHODE UPWIND AVEC UNE GRILLE ET QU'IL
C          Y A PLUSIEURS FOND DE FISSURE, ON REACALCULE LES
C          LEVELS SETS
           IF (FVIRTU.AND.(NUMFON.GT.1)) THEN          
            FONVIR=.FALSE.

C        1: ON DETERMINE SI LE NOEUDS EST PROJETE SUR UN SEGMENT DU
C            FRONT VIRTUEL A L'INTERIEUR D'UN TROU (SEGMENT DU FRONT
C            VIRTUEL DE TYPES 2 ET 3)            
  
             DO 861  K=1,(NUMFON-1)
               IF ((JMIN.EQ.(ZI(NFV+2*K-1)-1))
     &            .OR.(JMIN.EQ.(ZI(NFV+2*K)))) THEN
                 FONVIR=.TRUE.
                ELSE
                  GOTO 861
               ENDIF
861          CONTINUE
           
C         2: SI OUI, ON CALCULE LA CORRECTION A APPORTER
    
              IF (FONVIR) THEN
C          CALCUL DE LA VALEUR DE LA LEVEL SET NORMALE RECHERCHEE     
             LSNTH(1)= ZR(JDIS-1+3*(I-1)+1)*ZR(JBASEF+6*(JMIN-1)-1+1)
     &     +ZR(JDIS-1+3*(I-1)+2)*ZR(JBASEF+6*(JMIN-1)-1+2)
     &     +ZR(JDIS-1+3*(I-1)+3)*ZR(JBASEF+6*(JMIN-1)-1+3)

C          CALCUL DE LA VALEUR DE LA LEVEL SET TANGENTEE RECHERCHEE    
             LSTTH(1)= ZR(JDIS-1+3*(I-1)+1)*ZR(JBASEF+6*
     &     (JMIN-1)-1+4)+ZR(JDIS-1+3*(I-1)+2)*ZR(JBASEF+6*(JMIN-1)-1+5)
     &      +ZR(JDIS-1+3*(I-1)+3)*ZR(JBASEF+6*(JMIN-1)-1+6)

C          CALCUL DE LA CORRECTION A APPORTER
               ZR(JDELTA+2*(I-1))= LSNTH(1)-ZR(LSN+I-1)
               ZR(JDELTA+2*(I-1)+1)= LSTTH(1)-ZR(LST+I-1)
             ENDIF

           ENDIF

C           STORE THE DISTANCE VECTOR

C           attention: la distance du point au fond de fissure doit
C           etre calcule depuis le front de fissure reel et non le
C           front de fissure virtuel 
         IF ((GRILLE).AND.(NDIM.EQ.3)) THEN             

            IF (JMIN.EQ.1) THEN
C           Le projete est sur un segment virtuel du fond de
C           fissure (segment1)
               PI(1)= XM-ZR(CFV+4-1+1)
               PI(2) = YM-ZR(CFV+4-1+2)
               PI(3) = ZM-ZR(CFV+4-1+3)
               ZR(JDISFR+I-1) = PI(1)**2+PI(2)**2+PI(3)**2
            ELSE IF (JMIN.EQ.(NBPTFF-1)) THEN
              PI(1) = XM-ZR(CFV+4*(NBPTFF-2)-1+1)
              PI(2) = YM-ZR(CFV+4*(NBPTFF-2)-1+2)
              PI(3) = ZM-ZR(CFV+4*(NBPTFF-2)-1+3)
               ZR(JDISFR+I-1) = PI(1)**2+PI(2)**2+PI(3)**2
            ELSE
               DO 61 K=1,(NUMFON-1)
C            Le point est projete sur un segment virtuel de type 1
                   IF ((JMIN).EQ.(ZI(NFV+2*K-1)-1)) THEN
                    PI(1) = XM-ZR(CFV-1+4*(JMIN-1)+1)
                    PI(2) = YM-ZR(CFV-1+4*(JMIN-1)+2)
                    PI(3) = ZM-ZR(CFV-1+4*(JMIN-1)+3)
                    ZR(JDISFR+I-1) = PI(1)**2+PI(2)**2+PI(3)**2   
                    GOTO 61
                   ELSE IF ((JMIN).EQ.(ZI(NFV+2*K))) THEN
C              Le point est projete sur un segment virtuel de type 3
                    PI(1) = XM-ZR(CFV-1+4*(JMIN-0)+1)
                    PI(2) = YM-ZR(CFV-1+4*(JMIN-0)+2)
                    PI(3) = ZM-ZR(CFV-1+4*(JMIN-0)+3)
                    ZR(JDISFR+I-1) = PI(1)**2+PI(2)**2+PI(3)**2   
                   ELSE IF ((JMIN).EQ.(ZI(NFV+2*K-1))) THEN
C              Le point est projete sur un segment virtuel de type 2
                    NORMKL=(XM-ZR(CFV-1+4*(JMIN+1)+1))**2
     &              +(YM-ZR(CFV-1+4*(JMIN+1)+2))**2
     &              +(ZM-ZR(CFV-1+4*(JMIN+1)+3))**2

                    NORMIJ=(XM-ZR(CFV-1+4*(JMIN-2)+1))**2
     &              +(YM-ZR(CFV-1+4*(JMIN-2)+2))**2
     &              +(ZM-ZR(CFV-1+4*(JMIN-2)+3))**2
                    
                    IF (NORMKL.GE.NORMIJ) THEN 
C                   On est plus proche du fond de fissure a gauche
                     PI(1) = XM-ZR(CFV-1+4*(JMIN-2)+1)
                     PI(2) = YM-ZR(CFV-1+4*(JMIN-2)+2)
                     PI(3) = ZM-ZR(CFV-1+4*(JMIN-2)+3)
                     ZR(JDISFR+I-1) = PI(1)**2+PI(2)**2+PI(3)**2 
                     ELSE  
C                   On est plus proche du fond de fissure a droite
                     PI(1) = XM-ZR(CFV-1+4*(JMIN+1)+1)
                     PI(2) = YM-ZR(CFV-1+4*(JMIN+1)+2)
                     PI(3) = ZM-ZR(CFV-1+4*(JMIN+1)+3)
                     ZR(JDISFR+I-1) = PI(1)**2+PI(2)**2+PI(3)**2 
                    ENDIF                
                    GOTO 61
                    ENDIF             
61             CONTINUE      
              ENDIF   
            ENDIF

         ENDIF

C        ***************************************************************
C        EVALUATE THE LOCAL REFERENCE SYSTEM IN THE NODE
C        ***************************************************************

         IF (NDIM.EQ.2) THEN

C           IN THE 2D CASE THERE'S NO NEED TO CALCULATE THE INTERMEDIATE
C           LOCAL BASE
C           N-AXIS
            ZR(JBL-1+2*NDIM*(I-1)+1) = ZR(JBASEF-1+2*NDIM*(JMIN-1)+1)
            ZR(JBL-1+2*NDIM*(I-1)+2) = ZR(JBASEF-1+2*NDIM*(JMIN-1)+2)
C           T-AXIS
          ZR(JBL-1+2*NDIM*(I-1)+3) = ZR(JBASEF-1+2*NDIM*(JMIN-1)+NDIM+1)
          ZR(JBL-1+2*NDIM*(I-1)+4) = ZR(JBASEF-1+2*NDIM*(JMIN-1)+NDIM+2)

         ELSE

C           CALCULATE THE EULER ANGLE FOR THE NODE
            ALFA = ZR(JEULER-1+7*(JMIN-1)+1)*SMIN

            IF ((ALFA.GT.T0).AND.(ALFA.LT.T180)) THEN

C              CALCULATE COS(ALFA) AND SIN(ALFA) TO SPEED UP THE CODE
               CALFA = COS(ALFA)
               SALFA = SIN(ALFA)

C              RETRIEVE THE EULER AXIS
               AXEUL(1) = ZR(JEULER-1+7*(JMIN-1)+2)
               AXEUL(2) = ZR(JEULER-1+7*(JMIN-1)+3)
               AXEUL(3) = ZR(JEULER-1+7*(JMIN-1)+4)

C             RETRIEVE THE LOCAL BASE IN THE PREVIOUS POINT ON THE FRONT
C              (SMIN=0)
               NI(1) = ZR(JBASEF-1+2*NDIM*(JMIN-1)+1)
               NI(2) = ZR(JBASEF-1+2*NDIM*(JMIN-1)+2)
               NI(3) = ZR(JBASEF-1+2*NDIM*(JMIN-1)+3)
               TI(1) = ZR(JBASEF-1+2*NDIM*(JMIN-1)+4)
               TI(2) = ZR(JBASEF-1+2*NDIM*(JMIN-1)+5)
               TI(3) = ZR(JBASEF-1+2*NDIM*(JMIN-1)+6)
               BI(1) = ZR(JEULER-1+7*(JMIN-1)+5)
               BI(2) = ZR(JEULER-1+7*(JMIN-1)+6)
               BI(3) = ZR(JEULER-1+7*(JMIN-1)+7)

C              CALCULATE THE LOCAL BASE IN THE NODE WITH RESPECT TO THE
C              LOCAL BASE OF THE PREVIOUS POINT ON THE CRACK FRONT
C              (SMIN=0)
               TPL(1) = CALFA+(1-CALFA)*AXEUL(1)**2
               TPL(2) = (1-CALFA)*AXEUL(1)*AXEUL(2)-AXEUL(3)*SALFA
               TPL(3) = (1-CALFA)*AXEUL(1)*AXEUL(3)+AXEUL(2)*SALFA
               NPL(1) = (1-CALFA)*AXEUL(1)*AXEUL(2)+AXEUL(3)*SALFA
               NPL(2) = CALFA+(1-CALFA)*AXEUL(2)**2
               NPL(3) = (1-CALFA)*AXEUL(2)*AXEUL(3)-AXEUL(1)*SALFA

C              CALCULATE THE LOCAL BASE IN THE NODE WITH RESPECT TO THE
C              GLOBAL REFERENCE SYSTEM OF THE MESH
               T(1) = TPL(1)*TI(1)+TPL(2)*NI(1)+TPL(3)*BI(1)
               T(2) = TPL(1)*TI(2)+TPL(2)*NI(2)+TPL(3)*BI(2)
               T(3) = TPL(1)*TI(3)+TPL(2)*NI(3)+TPL(3)*BI(3)
               N(1) = NPL(1)*TI(1)+NPL(2)*NI(1)+NPL(3)*BI(1)
               N(2) = NPL(1)*TI(2)+NPL(2)*NI(2)+NPL(3)*BI(2)
               N(3) = NPL(1)*TI(3)+NPL(2)*NI(3)+NPL(3)*BI(3)

C              CALCULATE THE UNIT VECTOR FOR THE TANGENTIAL AND NORMAL
C              AXIS (THEIR MODULE COULD BE SLIGHTLY DIFFERENT THAN 1 DUE
C              TO NUMERICAL APPROXIMATION)
C              N-AXIS
               MODVEC = (T(1)**2+T(2)**2+T(3)**2)**0.5D0

               CALL ASSERT(MODVEC.GT.R8PREM())

               ZR(JBL-1+2*NDIM*(I-1)+1) = N(1)/MODVEC
               ZR(JBL-1+2*NDIM*(I-1)+2) = N(2)/MODVEC
               ZR(JBL-1+2*NDIM*(I-1)+3) = N(3)/MODVEC
C              T-AXIS
               MODVEC = (N(1)**2+N(2)**2+N(3)**2)**0.5D0

               CALL ASSERT(MODVEC.GT.R8PREM())

               ZR(JBL-1+2*NDIM*(I-1)+4) = T(1)/MODVEC
               ZR(JBL-1+2*NDIM*(I-1)+5) = T(2)/MODVEC
               ZR(JBL-1+2*NDIM*(I-1)+6) = T(3)/MODVEC

            ELSE

               IF (ALFA.GT.T180) JMIN=JMIN+1

C              N-AXIS
               ZR(JBL-1+2*NDIM*(I-1)+1) = ZR(JBASEF-1+2*NDIM*(JMIN-1)+1)
               ZR(JBL-1+2*NDIM*(I-1)+2) = ZR(JBASEF-1+2*NDIM*(JMIN-1)+2)
               ZR(JBL-1+2*NDIM*(I-1)+3) = ZR(JBASEF-1+2*NDIM*(JMIN-1)+3)
C              T-AXIS
               ZR(JBL-1+2*NDIM*(I-1)+4) =
     &                               ZR(JBASEF-1+2*NDIM*(JMIN-1)+NDIM+1)
               ZR(JBL-1+2*NDIM*(I-1)+5) =
     &                               ZR(JBASEF-1+2*NDIM*(JMIN-1)+NDIM+2)
               ZR(JBL-1+2*NDIM*(I-1)+6) =
     &                               ZR(JBASEF-1+2*NDIM*(JMIN-1)+NDIM+3)

            ENDIF

C           CORRECTION OF THE LOCAL BASE FOR THE POINTS PROJECTED ON
C           ONE END OF THE CRACK FRONT
            IF ((METHOD.EQ.'GEOMETRI').AND.ENDPNT) THEN

C              NORMAL AXIS OF THE LOCAL BASE
               N(1) = ZR(JBL-1+2*NDIM*(I-1)+1)
               N(2) = ZR(JBL-1+2*NDIM*(I-1)+2)
               N(3) = ZR(JBL-1+2*NDIM*(I-1)+3)

C              Q->P
               MODVEC =(XM-XN)*N(1)+(YM-YN)*N(2)+(ZM-ZN)*N(3)
               BI(1) = MODVEC*N(1)
               BI(2) = MODVEC*N(2)
               BI(3) = MODVEC*N(3)

C              NEW T-AXIS
               T(1) = (XM-XN)-BI(1)
               T(2) = (YM-YN)-BI(2)
               T(3) = (ZM-ZN)-BI(3)
               
C              CHECK THE DIRECTION OF THE NEW T-AXIS WITH RESPECT TO THE
C              ORIGINAL T-AXIS
               MODVEC=ZR(JBL-1+2*NDIM*(I-1)+4)*T(1)+
     &                ZR(JBL-1+2*NDIM*(I-1)+5)*T(2)+
     &                ZR(JBL-1+2*NDIM*(I-1)+6)*T(3)
               IF (MODVEC.LT.0.D0) THEN
C                 MODULUS OF THE NEW T-AXIS. ITS DIRECTION MUST BE 
C                 CHANGED (-1)
                  MODVEC = -1*(T(1)**2+T(2)**2+T(3)**2)**0.5D0     
               ELSE
C                 MODULUS OF THE NEW T-AXIS. ITS DIRECTION IS OK.
                  MODVEC = (T(1)**2+T(2)**2+T(3)**2)**0.5D0             
               ENDIF
               
C              STORE THE NEW T-AXIS (THE CORRECT DIRECTION IS DETERMINED
C              BY THE SIGN OF MODVEC)
               ZR(JBL-1+2*NDIM*(I-1)+4) = T(1)/MODVEC
               ZR(JBL-1+2*NDIM*(I-1)+5) = T(2)/MODVEC
               ZR(JBL-1+2*NDIM*(I-1)+6) = T(3)/MODVEC

            ENDIF

            IF (FVIRTU.AND.(NUMFON.GT.1)) THEN 
C           Calcul de la correction a apporter au level set pour les
C           vecteurs projetes sur le front virtuel( SEGMENT DE TYPE 2)

             DO 864 K=1,(NUMFON-1)
              IF ((JMIN).EQ.(ZI(NFV+2*K-1))) THEN

              LSNTH(1)= ZR(JDIS-1+3*(I-1)+1)*ZR(JBL-1+2*NDIM*(I-1)+1)
     &                 +ZR(JDIS-1+3*(I-1)+2)*ZR(JBL-1+2*NDIM*(I-1)+2)
     &                 +ZR(JDIS-1+3*(I-1)+3)*ZR(JBL-1+2*NDIM*(I-1)+3)

              LSTTH(1)= ZR(JDIS-1+3*(I-1)+1)*ZR(JBL-1+2*NDIM*(I-1)+4)
     &                 +ZR(JDIS-1+3*(I-1)+2)*ZR(JBL-1+2*NDIM*(I-1)+5)
     &                 +ZR(JDIS-1+3*(I-1)+3)*ZR(JBL-1+2*NDIM*(I-1)+6)

                ZR(JDELTA+2*(I-1))= LSNTH(1)-ZR(LSN+I-1)
                ZR(JDELTA+2*(I-1)+1)= LSTTH(1)-ZR(LST+I-1)
        
              ENDIF
864          CONTINUE
            ENDIF
         ENDIF

C        ***************************************************************
C        EVALUATE THE NORM. AND TANG. COMPONENTS OF THE PROPAGATION
C        SPEED IN THE NODE WITH RESPECT TO THE LOCAL REFERENCE SYSTEM
C        ***************************************************************

         BETAP=(ZR(JBETA-1+JMIN+1)-ZR(JBETA-1+JMIN))*SMIN+
     &          ZR(JBETA-1+JMIN)

         VP=(ZR(JVIT-1+JMIN+1)-ZR(JVIT-1+JMIN))*SMIN+ZR(JVIT-1+JMIN)

         ZR(JVNV+I-1)=VP*SIN(BETAP)
         ZR(JVTV+I-1)=VP*COS(BETAP)
         ZL(JVTL+I-1) = .TRUE.
         ZL(JVNL+I-1) = .TRUE.

C        STORE THE NORM OF THE PROPAGATION VELOCITY AT THE PROJECTED
C        POINT
         ZR(JVP+I-1)  = VP

         ZR(JDISFR+I-1) = DMIN

C        STORE THE PROPAGATION ANGLE AT THE PROJECTED POINT
         ZR(JCNSB+I-1) = BETAP


200   CONTINUE

C ***************************************************************
C PRINT SOME INFORMATIONS
C ***************************************************************

C  IMPRESSION DES VITESSES DE PROPAGATION EN INFO=2
      IF (NIV.GE.1) THEN
         WRITE(IFM,*) ' '
         WRITE(IFM,*) 'VITESSE DE PROPAGATION EN FOND DE FISSURE'

         DO 310 I=1,NBPTFF

            DO 312 J=1,NUMFON

               IF (I.EQ.ZI(JFMULT-1+2*J-1)) THEN
                  WRITE(IFM,*) ' '
                  IF (NUMFON.GT.1) WRITE(IFM,313) J
                  WRITE(IFM,*)  ' NUM_PT    VITESSE         '
     &               //'BETA          VT            VN'
               ENDIF
          
 312        CONTINUE

            WRITE(IFM,311) I,ZR(JVIT-1+I),ZR(JBETA-1+I),ZR(JVTFF+I-1),
     &                     ZR(JVNFF+I-1)

 310      CONTINUE

 311      FORMAT(4X,I2,4X,4(D11.5,3X))
 313      FORMAT(1X,' FOND DE FISSURE ',I2)

      ENDIF


      IF (FVIRTU) THEN 
         NBPTFF=NBPTFF-2*NUMFON
         CALL JEDETR(COVIR)
         CALL JEDETR(BAVIR)
         CALL JEDETR(VITVIR)
         CALL JEDETR(ANGVIR)
         CALL JEDETR(NUMVIR)
      ENDIF

      CALL JEDETR('&&XPRVIT.V_PROPA_FF')
      CALL JEDETR('&&XPRVIT.VT_PROPA_FF')
      CALL JEDETR('&&XPRVIT.VN_PROPA_FF')
      CALL JEDETR('&&XPRVIT.EULER')

C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------
      CALL JEDEMA()
      END
