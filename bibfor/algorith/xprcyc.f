      SUBROUTINE XPRCYC(NOMO,NOMFIS,NDIM,DAMAX,VVIT,VBETA,DT,DAFISS)
      IMPLICIT NONE

      CHARACTER*8    NOMO,NOMFIS
      INTEGER        NDIM
      CHARACTER*24   VVIT,VBETA
      REAL*8         DAMAX,DT,DAFISS

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/01/2011   AUTEUR MACOCCO K.MACOCCO 
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
C RESPONSABLE GENIAUT S.GENIAUT
C TOLE CRP_20 CRP_6

C
C       XPRCYC: X-FEM PROPAGATION CALCUL DES CYCLES DE FATIGUE
C               -     --                     ---
C
C   DANS LE CADRE DE LA PROPAGATION X-FEM ET ON PRESENCE DE PLUSIERS
C   FISSURES, ON CALCULE CECI:
C
C   - LA FISSURE A PROPAGER
C
C   - LE NOMBRE DE CYCLES DE FATIGUE POUR AVOIR L'AVANCE MAXIMALE DONNE
C     PAR L'UTILISATEUR (POUR LA FISSURE QUI AVANCE PLUS VIT)
C
C   - LES FACTEURS D'INTENSITE DES CONTRAINTES POUR LES POINTS DU FOND
C     DE LA FISSURE A PROPAGER ET LE RAPPORT DE CHARGE EFFECTIF
C
C
C    ENTREE
C    ------
C      NOMO   = NOM DU MODELE FISSURE
C      NOMFIS = NOM DE LA FISSURE A TRAITER
C      NDIM   = DIMENSION DU MODELE (2=2D OU 3=3D)
C      DAMAX  = AVANCE MAXIMALE DES FISSURES DU MODELE
C
C    SORTIE
C    ------
C      VVIT   = VECTEUR DES VITESSES DE PROPAGATION POUR CHAQUE POINT
C               DU FOND DE LA FISSURE A TRAITER (NOM DU CONCEPT)
C      VBETA  = VECTEUR DES ANGLES DE PROPAGATION POUR CHAQUE POINT
C               DU FOND DE LA FISSURE A TRAITER (NOM DU CONCEPT)
C      DT     = TEMPS TOTAL DE INTEGRATION POUR AVOIR "DAMAX"
C      DAFISS = AVANCE MAXIMALE DE LA FISSURE "NOMFIS" A PROPAGER
C
C     ------------------------------------------------------------------

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
      CHARACTER*32    JEXNUM,JEXATR,JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

C     JEVEUX AND GENERAL PURPOSE
      INTEGER      IBID,IFM,NIV,VALI,IRET,I,J,VINT(3),IB
      REAL*8       R8B,R8PREM,VREAL,JR
      COMPLEX*16   CBID,VALC
      CHARACTER*8  K8B,CTYPE,VALK
      CHARACTER*16 VCHAR
      CHARACTER*1  K1B
      CHARACTER*8  TEST,MSGOUT(2)

C     INPUT DATA
      INTEGER      NFISS,JNFIS,MSGINT(2),JFISS,NUMFIS

C     MATERIAL DATA
      REAL*8       C,N,M,MATE,MATNU
      CHARACTER*8  TABLE,LOI
      INTEGER      JSIF,NUMTAB

C     SINGLE CRACK
      INTEGER      CRACK,JNFON,NFON,PIECE,NUMSIF,JFFIS,NI,NF,VAPACR(3),
     &             NBPTFF,ACTPOI,MAXACT,JBASEF,NUMLST,JLISTE,VAPAC2(2)
      CHARACTER*8  NCRACK,FISLST
      CHARACTER*16 LIPAC2(2),LIPACR(3)
      REAL*8       G1,G2,VPNT,VMAX,KI1,KI2,KII1,KII2,BETA1,BETA2,VMFISS,
     &             VNORM(2),VTAN(2),ZLOC
      LOGICAL      STORE,R0CYC

C     PROPAGATION DATA
      INTEGER      JVIT,JBETA
      LOGICAL      BETCHG
      REAL*8       BETTOL,R
      PARAMETER    (BETTOL=5.D0/180.D0*3.1415D0)

C     OPTION NB_POINT_FOND
      LOGICAL      NBPTFO,NPFMOR,NPFCUR
      INTEGER      NPF,JNPF,SIFVAL,JVITD,JBETAD,JABSC,JMIN,NPFPNT,NPFPOS
      REAL*8       ABSMAX,ABSCUR

C     OPTION COMP_LINE
      REAL*8       CMAX,CMIN
      LOGICAL      CHLINE

C-----------------------------------------------------------------------
C     DEBUT
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)

C **********************************************************************
C     RETRIEVE THE INPUT DATA
C **********************************************************************

C     CHECK THAT A CRACK HAS BEEN DEFINED ON THE MODEL
C     AND RETRIEVE THE NUMBER OF CRACKS IN THE MODEL 
      CALL DISMOI('F','NB_FISS_XFEM',NOMO,'MODELE',NFISS,K8B,IRET)
      IF (NFISS.EQ.0) CALL U2MESK('F','XFEM2_93',1,NOMO)

C     RETRIEVE THE NAME OF THE DATA STRUCTURE CONTAINING EACH CRACK
      CALL JEVEUO(NOMO//'.FISS','L',JFISS)

      NUMFIS=0

C     SEARCH FOR THE CRACK THAT MUST BE PROPAGATED
      DO 3000 CRACK=1,NFISS

         NCRACK = ZK8(JFISS-1+CRACK)
         IF (NCRACK.EQ.NOMFIS) NUMFIS=CRACK

3000  CONTINUE

      IF (NUMFIS.EQ.0) THEN
         MSGOUT(1) = NOMFIS
         MSGOUT(2) = NOMO
         CALL U2MESK('F','XFEM2_89',2,MSGOUT)
      ENDIF

C     RETRIEVE THE NUMBER OF CRACKS IN THE MODEL THAT MUST PROPAGATE
C     IN THIS ITERATION (ONE SINGLE PROPA_FISS CALL)
      CALL GETVID(' ','LISTE_FISS',1,1,0,FISLST,NUMLST)
      NUMLST = NUMLST*(-1)
      CALL WKVECT('&&XPRCYC.LISTFISS','V V K8',NUMLST,JLISTE)
      CALL GETVID(' ','LISTE_FISS',1,1,NUMLST,ZK8(JLISTE),IBID)

C     RETRIEVE THE VALUE FOR THE "TEST_MAIL" PARAMETER
      CALL GETVTX(' ','TEST_MAIL',1,1,1,TEST,IBID)

C     ISSUE AN ALARM FOR THE USER
      IF (TEST(1:3).EQ.'OUI') THEN

         IF (NDIM.EQ.2) CALL U2MESS('F','XFEM2_87')

C        DEACTIVATE THE FLAG FOR THE NB_POINT_FOND OPTION
         NBPTFO = .FALSE.

      ELSE

C        RETRIEVE THE ELASTIC CONSTANTS AND THE PROPAGATION LAW OF THE
C        MATERIAL
         CALL GETVTX('LOI_PROPA','LOI',1,1,1,LOI,IBID)
         IF (LOI.EQ.'PARIS') THEN
             CALL GETVR8('LOI_PROPA','C',1,1,1,C,IBID)
             CALL GETVR8('LOI_PROPA','N',1,1,1,N,IBID)
             CALL GETVR8('LOI_PROPA','M',1,1,1,M,IBID)
             CALL GETVR8('LOI_PROPA','E',1,1,1,MATE,IBID)
             CALL GETVR8('LOI_PROPA','NU',1,1,1,MATNU,IBID)
         ENDIF

C        RETRIEVE THE LIST OF THE SIF TABLES
         CALL GETVID(' ','TABLE',1,1,0,TABLE,NUMTAB)
         NUMTAB = NUMTAB*(-1)
         CALL WKVECT('&&XPRCYC.SIF','V V K8',NUMTAB,JSIF)
         CALL GETVID(' ','TABLE',1,1,NUMTAB,ZK8(JSIF),IBID)

C        CHECK IF THE NB_POINT_FOND OPTION HAS BEEN USED
         CALL GETVIS(' ','NB_POINT_FOND',1,1,0,NPF,IBID)
         IF (IBID.EQ.0) THEN
C           DEACTIVATE THE FLAG FOR THE NB_POINT_FOND OPTION
            NBPTFO = .FALSE.
         ELSE
C           RETRIEVE THE NUMBER OF VALUES PASSED TO THE NB_POINT_FOND
C           OPTION
            NPF = IBID*(-1)
            CALL WKVECT('&&XPRCYC.NPF','V V I',NPF,JNPF)
            CALL GETVIS(' ','NB_POINT_FOND',1,1,NPF,ZI(JNPF),IBID)

C           CHECK THAT THE OPTION HAS BEEN USED FOR AT LEAST ONE PIECE
C           OF ONE PROPAGATING CRACK
            I=0
            DO 2500 J=1,NPF
               I=I+ZI(JNPF-1+J)
2500        CONTINUE
            
            IF (I.GT.(-1*NPF)) THEN
C              YES. THE USER HAS USED THE OPTION.
C              ACTIVATE THE FLAG FOR THE NB_POINT_FOND OPTION
               NBPTFO = .TRUE.
C              THE OPTION HAS NO MEANING IN THE 2D CASE
               IF (NDIM.EQ.2) CALL U2MESS('F','XFEM2_73')
            ELSE
C              NO. THE USER HASN'T USED THE OPTION. THE OPTION IS THERE
C              BECAUSE IT HAS BEEN CREATED BY PROPA_FISS MACRO.
               NBPTFO = .FALSE.
            ENDIF
         ENDIF

C        CHECK IF THE OPTION COMP_LINE HAS BEEN USED
         CALL GETVR8('COMP_LINE','COEF_MULT_MAXI',1,1,1,CMAX,IBID)
         IF (IBID.EQ.0) THEN
C           DEACTIVATE THE FLAG FOR THE COMP_LINE OPTION
            CHLINE=.FALSE.
         ELSE
C           ACTIVATE THE FLAG FOR THE COMP_LINE OPTION
            CHLINE=.TRUE.
            CALL GETVR8('COMP_LINE','COEF_MULT_MINI',1,1,1,CMIN,IBID)
            IF ((ABS(CMAX).LT.R8PREM()).AND.(ABS(CMIN).LT.R8PREM()))
     &         CALL U2MESS('F','XFEM2_81')
         ENDIF

      ENDIF

C     PRINT SOME INFORMATIONS
      WRITE(IFM,*)
      WRITE(IFM,*)'-------------------------------------------'
      WRITE(IFM,*)'FISSURE A TRAITER  : ',ZK8(JFISS-1+NUMFIS)
      WRITE(IFM,*)
      WRITE(IFM,*)'FISSURES DU MODELE : ',ZK8(JFISS-1+1)
      IF (NFISS.GT.1) THEN
         DO 2000 I=2,NFISS
            WRITE(IFM,*)'                     ',ZK8(JFISS-1+I)
2000     CONTINUE
      ENDIF
      WRITE(IFM,*)'-------------------------------------------'
      WRITE(IFM,*)

      IF (CHLINE) THEN
         WRITE(IFM,*)'OPTION COMPORTAMENT LINEAIRE ACTIVE:'
         WRITE(IFM,*)'   COEF_MULT_MINI = ',CMIN
         WRITE(IFM,*)'   COEF_MULT_MAXI = ',CMAX
         WRITE(IFM,*)
      ENDIF

C **********************************************************************
C     FOR EACH CRACK IN THE MODEL THAT MUST PROPAGATE IN THE CURRENT
C     ITERATION, EVALUATE THE MAXIMUM VALUE OF THE PROPAGATION SPEED
C     ON THE FRONT.
C     AT THE SAME TIME, WHEN THE CRACK TO BE ELABORATED IS CONSIDERED,
C     EVALUATE THE CRACK SPEED COMPONENTS (NORMAL AND TANGENTIAL) AND
C     THE CRACK PROPAGATION ANGLE AT THE INTERSECTION POINTS BETWEEN
C     THE CRACK FRONT AND THE FACES OF THE ELEMENTS. IN SUCH A MANNER
C     THE OPTION "NB_POINT_FOND" IS CORRECTLY CONSIDERED.
C **********************************************************************

C     INITIALIZE A POINTER TO THE SIF TABLE LIST.
      NUMSIF = 1

C     INITIALIZE A POINTER TO THE NB_POINT_FOND LIST. THIS POINTER IS
C     USED TO TRACK THE PROPAGATING CRACKS FOR WHICH THE OPTION HAS NOT
C     BEEN USED (VALUE -1)
      NPFPOS = 1

C     INITIALIZE THE VARIABLE WHERE THE MAXIMUM VALUE OF THE SPEED IS
C     STORED
      VMAX = 0.D0

C     INITIALIZE THE VARIABLE WHERE THE MAXIMUM VALUE OF THE SPEED FOR
C     THE SELECTED CRACK IS STORED
      VMFISS = 0.D0

C     INITIALIZE THE FLAG USED TO ISSUE A WARNING IF THE PROPAGATION
C     ANGLE CHANGES IN THE LOAD CYCLE
      BETCHG = .FALSE.

C     INITIALIZE THE FLAG USED TO ISSUE A WARNING IF THE LOAD RATIO IS
C     ASSUMED EQUAL TO ZERO
      R0CYC = .FALSE.

C     INITIALIZE THE PARAMETERS USED FOR THE SIF SEARCHING
C     3D CASE
      LIPACR(1)='NUME_ORDRE'
      LIPACR(2)='NUM_PT'
      LIPACR(3)='NUME_FOND'
C     2D CASE
      LIPAC2(1)='NUME_ORDRE'
      LIPAC2(2)='NUME_FOND'

C     ELABORATE EACH CRACK IN THE MODEL THAT MUST PROPAGATE IN THE
C     CURRENT ITERATION
      DO 1000 CRACK=1,NUMLST

C        RETRIEVE THE CRACK DATA STRUCTURE
         NCRACK = ZK8(JLISTE-1+CRACK)

C        RETRIEVE THE NUMBER OF DIFFERENT PIECES (NFON) FORMING THE
C        CURRENT FRONT 
         CALL DISMOI('F','NB_FOND',NCRACK,'FISS_XFEM',NFON,K8B,IB)
         CALL JEVEUO(NCRACK//'.FONDMULT','L',JNFON)

C        RETRIEVE THE LIST OF THE POINTS OF THE WHOLE FRONT
         CALL JEVEUO(NCRACK//'.FONDFISS','L',JFFIS)

C        IF THE ACTUAL CRACK IS THE ONE THAT HAS BEEN SELECTED FOR
C        PROPAGATION, PREPARE THE DATA STRUCTURE WHERE THE CRACK
C        PROPAGATION SPEED AND ANGLE ARE STORED
         IF (NCRACK.EQ.NOMFIS) THEN

            CALL DISMOI('F','NB_POINT_FOND',NCRACK,'FISS_XFEM',
     &                                                    NBPTFF,K1B,IB)
            CALL WKVECT(VVIT,'V V R8',NBPTFF,JVIT)
            CALL WKVECT(VBETA,'V V R8',NBPTFF,JBETA)
            MAXACT = NBPTFF

C           INITIALIZE THE POINTER TO TRACK THE ACTUAL POINT ON THE
C           FRONT
            ACTPOI=1

C           MARK THE ACTUAL CRACK AS THE ONE THAT MUST BE ELABORATED
            STORE = .TRUE.

C           RETREIVE THE LOCAL REFERENCE SYSTEM FOR THE POINTS OF THE
C           CRACK FRONT. THIS INFORMATION WILL BE USED LATER FOR THE 2D
C           CASE TO CHECK THE CONSISTENCY BETWEEN THE SIGN OF THE
C           PROPAGATION ANGLE USED BY CALC_G AND THE ONE USED HERE
C           (BASED ON THE LEVEL SETS)
            IF (NDIM.EQ.2) THEN
               CALL JEVEUO(NCRACK//'.BASEFOND','L',JBASEF)
            ENDIF

         ELSE

            STORE = .FALSE.

         ENDIF

C        RETRIEVE THE SIF TABLE NAME FOR THE ACTUAL CRACK
         IF (TEST.EQ.'NON') THEN
C           RETRIEVE THE NAME OF THE TABLE
            IF (NUMSIF.GT.NUMTAB) CALL U2MESS('F','XFEM2_71')
            TABLE = ZK8(JSIF-1+NUMSIF)
            NUMSIF = NUMSIF+1
         ENDIF

C        ELABORATE EACH SINGLE PIECE OF THE FRONT
         DO 500 PIECE=1,NFON

C           **********************************************************
C           EVALUATE THE CRACK ADVANCEMENT SPEED FOR EACH NODE OF 
C           THE PIECE
C           **********************************************************

C           RETRIEVE THE SIF TABLE AND EVALUATE THE DELTA_K FOR EACH
C           NODE OF THE PIECE
            IF (NDIM.EQ.2) THEN

C             --------------------------------------------------------
C             2D CASE
C             --------------------------------------------------------

              IF (TEST.EQ.'NON') THEN

C               CHECK THAT THE COLUMN 'NUME_FOND' IS PRESENT IN THE
C               TABLE
                CALL TBLIVA(TABLE,1,'NUME_ORDRE',1,R8B,CBID,K8B,K8B,
     &                      R8B,'NUME_FOND',CTYPE,VALI,G1,VALC,VALK,
     &                      IRET)
                IF (IRET.EQ.1) CALL U2MESS('F','XFEM2_90')

C               RETRIEVE THE FIRST VALUE OF G
                VAPAC2(1)=1
                VAPAC2(2)=PIECE
                CALL TBLIVA(TABLE,2,LIPAC2,VAPAC2,R8B,CBID,K8B,K8B,R8B,
     &                      'G',CTYPE,VALI,G1,VALC,VALK,IRET)
                CALL ASSERT(IRET.EQ.0)

                IF (.NOT.(CHLINE)) THEN
C                  THE COMP_LINE OPTION HAS NOT BEEN USED. THEREFORE THE
C                  VALUES OF G MUST BE READ FROM THE SIF TABLES.

C                  RETRIEVE THE SECOND VALUE OF G
                   VAPAC2(1)=2
                   CALL TBLIVA(TABLE,2,LIPAC2,VAPAC2,R8B,CBID,K8B,K8B,
     &                         R8B,'G',CTYPE,VALI,G2,VALC,VALK,IRET)
                   IF (IRET.EQ.2) THEN
                      G2=0
                      IRET=0
                      R0CYC=.TRUE.
                   ENDIF
                   CALL ASSERT(IRET.EQ.0)

C                  CHECK THAT ONLY TWO VALUES ARE GIVEN FOR EACH POINT
C                  ON THE FRONT
                   VAPAC2(1)=3
                   CALL TBLIVA(TABLE,2,LIPAC2,VAPAC2,R8B,CBID,K8B,K8B,
     &                         R8B,'G',CTYPE,VALI,JR,VALC,VALK,IRET)
                   IF (IRET.EQ.0) CALL U2MESS('F','XFEM2_68')

                ELSE

C                  THE COMP_LINE OPTION HAS BEEN USED. THEREFORE THE
C                  VALUES OF G MUST BE CALCULATED FROM THE REFERENCE
C                  CONDITION.

C                  CHECK THAT ONLY ONE VALUE IS GIVEN FOR EACH POINT ON
C                  THE FRONT
                   VAPAC2(1)=2
                   CALL TBLIVA(TABLE,2,LIPAC2,VAPAC2,R8B,CBID,K8B,K8B,
     &                         R8B,'G',CTYPE,VALI,JR,VALC,VALK,IRET)

                   IF (IRET.EQ.0) CALL U2MESS('F','XFEM2_82')

C                  CALCULATE THE VALUES OF G FOR THE REAL LOADING
C                  CONDITIONS
                   G2 = (CMIN**2)*G1
                   G1 = (CMAX**2)*G1

                ENDIF

C               CALCULATE THE VALUE OF THE CRACK ADVANCEMENT SPEED IN
C               THE CRACK FRONT USING THE PARIS LAW
                IF ((G1.LT.0.D0).OR.(G2.LT.0.D0)) 
     &                                   CALL U2MESS('F','XFEM2_66')

                IF ((G1.LT.R8PREM()).AND.(G2.LT.R8PREM()))
     &                                   CALL U2MESS('F','XFEM2_83')

                IF (ABS(G1-G2).LT.R8PREM())
     &                                   CALL U2MESS('F','XFEM2_84')

                IF (R0CYC) THEN
                   R = 0
                ELSE
                   IF (G1.GT.G2) THEN
                       R = SQRT(G2/G1)
                   ELSE
                       R = SQRT(G1/G2)
                   ENDIF
                ENDIF
                   
                VPNT = (C/(1-R)**N)*
     &               (SQRT(MATE/(1-MATNU**2))*ABS(SQRT(G1)-SQRT(G2)))**M

              ELSE

                VPNT = DAMAX
                BETA1 = 0

              ENDIF

C             IF THE ACTUAL CRACK IS THE ONE THAT HAS BEEN SELECTED FOR
C             PROPAGATION, STORE THE VALUES
              IF (STORE) THEN

C                -------------------------------------------------------
C                RETRIEVE THE VALUES OF KI AND KII IN ORDER TO CALCULATE
C                THE PROPAGATION ANGLE
C                -------------------------------------------------------

                 IF (TEST.EQ.'NON') THEN
C                  RETRIEVE THE FIRST VALUE OF KI
                   VAPAC2(1)=1
                   CALL TBLIVA(TABLE,2,LIPAC2,VAPAC2,R8B,CBID,K8B,K8B,
     &                         R8B,'K1',CTYPE,VALI,KI1,VALC,VALK,IRET)
                   CALL ASSERT(IRET.EQ.0)

C                  RETRIEVE THE FIRST VALUE OF KII
                   VAPAC2(1)=1
                   CALL TBLIVA(TABLE,2,LIPAC2,VAPAC2,R8B,CBID,K8B,K8B,
     &                         R8B,'K2',CTYPE,VALI,KII1,VALC,VALK,IRET)
                   CALL ASSERT(IRET.EQ.0)

                   IF (.NOT.(CHLINE)) THEN
C                     RETRIEVE THE SECOND VALUE OF KI
                      VAPAC2(1)=1
                      CALL TBLIVA(TABLE,2,LIPAC2,VAPAC2,R8B,CBID,K8B,
     &                            K8B,R8B,'K1',CTYPE,VALI,KI2,VALC,VALK,
     &                            IRET)
                      IF (IRET.EQ.2) KI2=KI1

C                     RETRIEVE THE SECOND VALUE OF KII
                      VAPAC2(1)=2
                      CALL TBLIVA(TABLE,2,LIPAC2,VAPAC2,R8B,CBID,K8B,
     &                            K8B,R8B,'K2',CTYPE,VALI,KII2,VALC,
     &                            VALK,IRET)
                      IF (IRET.EQ.2) KII2=KII1
                   ELSE
C                     IF THE COMP_LINE OPTION HAS BEEN USED, THE
C                     REFERENCE LOAD CASE MUST BE USED
                      KI2=KI1
                      KII2=KII1
                   ENDIF

C                  RETRIEVE THE LOCAL REFERENCE SYSTEM FOR THE POINT OF
C                  THE CRACK FRONT
                   VNORM(1)= ZR(JBASEF-1+4*(PIECE-1)+1)
                   VNORM(2)= ZR(JBASEF-1+4*(PIECE-1)+2)
                   VTAN(1) = ZR(JBASEF-1+4*(PIECE-1)+3)
                   VTAN(2) = ZR(JBASEF-1+4*(PIECE-1)+4)

C                  CALCULATE THE VECTORIAL PRODUCT BETWEEN THE NORMAL
C                  AND TANGENTIAL AXIS OF THE LOCAL REFERENCE SYSTEM.
C                  ONLY THE Z-AXIS COMPONENT IS IMPORTANT.
                   ZLOC=VTAN(1)*VNORM(2)-VTAN(2)*VNORM(1)

C                  CHECK IF THIS AXIS HAS THE SAME DIRECTION OF THE
C                  Z-AXIS OF THE REFERECE SYSTEM USED FOR THE MESH
                   IF (ZLOC.LT.R8PREM()) THEN
C                      NO. THEN I CHANGE THE CONVENTION ON THE ANGLE
                       KII1=KII1*(-1)
                       KII2=KII2*(-1)
                   ENDIF

C                  CALCULATE THE PROPAGATION ANGLE
                   IF(ABS(KII1).GT.R8PREM()) THEN
                     IF(KII1.GT.0) THEN
                       BETA1 = 2*ATAN(0.25D0*
     &                            (KI1/KII1-SQRT((KI1/KII1)**2+8)))
                     ELSE
                       BETA1 = 2*ATAN(0.25D0*
     &                            (KI1/KII1+SQRT((KI1/KII1)**2+8)))
                     ENDIF
                   ELSE
                     BETA1=0
                   ENDIF

                   IF(ABS(KII2).GT.R8PREM()) THEN
                     IF(KII2.GT.0) THEN
                       BETA2 = 2*ATAN(0.25D0*
     &                         (KI2/KII2-SQRT((KI2/KII2)**2+8)))
                     ELSE
                       BETA2 = 2*ATAN(0.25D0*
     &                         (KI2/KII2+SQRT((KI2/KII2)**2+8)))
                     ENDIF
                   ELSE
                     BETA2=0
                   ENDIF

C                  ACTIVATE THE WARNING FLAG IF THE CHANGING IN THE
C                  PROPAGATION ANGLE IS GREATER THEN THE TOLERANCE
                   IF (ABS(BETA1-BETA2).GT.BETTOL) BETCHG=.TRUE.

                 ENDIF

C                STORE THE VALUES
                 CALL ASSERT(ACTPOI.LE.MAXACT)
                 ZR(JVIT-1+ACTPOI)  = VPNT
                 ZR(JBETA-1+ACTPOI) = BETA1

C                INCREMENT THE POINTER FOR THE ACTUAL POINT ON THE FRONT
                 ACTPOI = ACTPOI+1

C                CHECK THE MAXIMUM VALUE OF THE PROPAGATION SPEED FOR
C                THE SELECTED CRACK
                 IF (VPNT.GT.VMFISS) VMFISS=VPNT

              ENDIF

C             CHECK THE MAXIMUM SPEED VALUE
              IF (VPNT.GT.VMAX) VMAX=VPNT

            ELSE

C             --------------------------------------------------------
C             3D CASE
C             --------------------------------------------------------

C             CALCULATE THE FIRST NODE OF THE PIECE
              NI=ZI(JNFON-1+2*(PIECE-1)+1)
C             CALCULATE THE LAST NODE OF THE PIECE
              NF=ZI(JNFON-1+2*(PIECE-1)+2)

              VAPACR(3)=PIECE

C             CALCULATE THE NUMBER OF VALUES EXPECTED IN THE SIF TABLES
              IF (NBPTFO) THEN
C                IF THE NB_POINT_FOND HAS BEEN USED, THIS VALUE MUST BE
C                RETRIEVED BY THE LIST GIVEN BY THE USER

C                ACTIVATE THE FLAG TO INDICATE THAT NB_POINT_FOND IS
C                ACTIVE FOR THE CURRENT CRACK
                 NPFCUR=.TRUE.

C                RETREIVE THE VALUE GIVEN BY THE USER (NB_POINT_FOND)
                 IF (NPFPOS.GT.NPF) THEN
                      VCHAR = 'INFERIEUR'
                      CALL U2MESK('F','XFEM2_78',1,VCHAR)
                 ENDIF
                 SIFVAL = ZI(JNPF-1+NPFPOS)

                 IF (SIFVAL.EQ.-1) THEN
C                   THE OPTION HAS NOT BEEN USED FOR THE ACTUAL CRACK.
C                   THEREFORE THE FLAG FOR THE CURRENT PIECE IS
C                   DEACTIVATED
                    NPFMOR=.FALSE.
C                   THE OPTION IS NOT ACTIVE FOR THE CURRENT CRACK ALSO
                    NPFCUR=.FALSE.

                    SIFVAL=NF-NI+1

                 ELSE

C                   OK. THE USER HAS PASSED A VALUE FOR THE ACTUAL
C                   PIECE OF CRACK.
                    IF (NPFCUR.AND.(PIECE.EQ.1).AND.STORE) THEN
C                      PRINT SOME INFORMATIONS IF THE NB_POINT_FOND
C                      OPTION HAS BEEN USED BY THE USER
                       WRITE(IFM,*)'NB_POINT_FOND EST UTILISE '//
     &                       'POUR CETTE FISSURE:'
                    ENDIF

C                   CHECK THAT THIS VALUE IS MEANINGFUL
                    IF ((SIFVAL.LT.0).OR.(SIFVAL.EQ.1))
     &                              CALL U2MESS('F','XFEM2_79')

C                   IF IT'S ZERO, NB_POINT_FOND HAS NOT BEEN USED FOR
C                   THIS PIECE OF THE CRACK
                    IF (SIFVAL.EQ.0) THEN
C                      DEACTIVATE THE FLAG FOR THE CURRENT PIECE
                       NPFMOR=.FALSE.
                       SIFVAL=NF-NI+1
C                      PRINT SOME INFORMATIONS
                       IF (STORE) WRITE(IFM,5000)PIECE
                    ELSE
C                      ACTIVATE THE FLAG FOR THE CURRENT PIECE
                       NPFMOR=.TRUE.
C                      PRINT SOME INFORMATIONS
                       IF (STORE) WRITE(IFM,5001)PIECE,SIFVAL
                    ENDIF

C                   INCREMENT THE POINTER TO THE LIST OF NB_POINT_FOND
C                   VALUES
                    NPFPOS = NPFPOS+1

                 ENDIF

              ELSE

C                IF THE NB_POINT_FOND HAS NOT BEEN USED, THIS VALUE
C                MUST BE RETRIEVED FROM THE CRACK DATA STRUCTURE
                 NPFMOR=.FALSE.
                 SIFVAL=NF-NI+1

              ENDIF

              IF ((NPFMOR).AND.(STORE)) THEN

C                INITIALIZE THE POINTER TO TRACK THE ACTUAL VIRTUAL
C                POINT ON THE CRACK FRONT PIECE IF THE NB_POINT_FOND
C                OPTION HAS BEEN USED FOR THIS PIECE
                 NPFPNT=1

C                ALLOCATE A TEMPORARY VECTOR FOR THE SIF VALUES READ
C                FROM THE TABLES
                 CALL WKVECT('&&XPRCYC.VITESSE','V V R8',SIFVAL,JVITD)
                 CALL WKVECT('&&XPRCYC.ANGLE','V V R8',SIFVAL,JBETAD)

              ENDIF

C             LOOP ON EACH POINT OF THE PIECE
              DO 400 I=1,SIFVAL

                 IF (TEST.EQ.'NON') THEN

C                CHECK THAT THE COLUMN 'NUME_FOND' IS PRESENT IN THE
C                TABLE
                 CALL TBLIVA(TABLE,1,'NUME_ORDRE',1,R8B,CBID,K8B,K8B,
     &                       R8B,'NUME_FOND',CTYPE,VALI,G1,VALC,VALK,
     &                       IRET)
                 IF (IRET.EQ.1) CALL U2MESS('F','XFEM2_90')

C                RETRIEVE THE FIRST VALUE OF G FOR THE CURRENT POINT
                   VAPACR(1)=1
                   VAPACR(2)=I
                   CALL TBLIVA(TABLE,3,LIPACR,VAPACR,R8B,CBID,K8B,K8B,
     &                      R8B,'G',CTYPE,VALI,G1,VALC,VALK,IRET)
                   IF (IRET.EQ.2) THEN
                      VINT(1) = NFON
                      VINT(2) = PIECE
                      VINT(3) = I
                     CALL U2MESG('F','XFEM2_77',1,NCRACK,3,VINT,0,VREAL)
                   ENDIF
 
                   CALL ASSERT(IRET.EQ.0)

C                  RETREIVE THE FIRST VALUE OF BETA FOR THE CURRENT
C                  POINT
                   IF (STORE) THEN
                   CALL TBLIVA(TABLE,3,LIPACR,VAPACR,R8B,CBID,K8B,K8B,
     &                R8B,'BETA',CTYPE,VALI,BETA1,VALC,VALK,IRET)
                   CALL ASSERT(IRET.EQ.0)
                   ENDIF

                   IF (.NOT.(CHLINE)) THEN

C                     RETRIEVE THE SECOND VALUE OF G FOR THE CURRENT
C                     POINT
                      VAPACR(1)=2
                      CALL TBLIVA(TABLE,3,LIPACR,VAPACR,R8B,CBID,K8B,
     &                          K8B,R8B,'G',CTYPE,VALI,G2,VALC,
     &                          VALK,IRET)
                      IF (IRET.EQ.2) THEN
                         G2=0
                         IRET=0
                         R0CYC=.TRUE.
                      ENDIF

                      CALL ASSERT(IRET.EQ.0)

C                     RETREIVE THE SECOND VALUE OF BETA FOR THE CURRENT
C                     POINT
                      IF (STORE) THEN
                         CALL TBLIVA(TABLE,3,LIPACR,VAPACR,R8B,CBID,K8B,
     &                       K8B,R8B,'BETA',CTYPE,VALI,BETA2,VALC,
     &                       VALK,IRET)
                         IF (IRET.EQ.2) THEN
                            BETA2=BETA1
                            IRET = 0
                         ENDIF

                         CALL ASSERT(IRET.EQ.0)
                      ENDIF

C                     CHECK THAT ONLY TWO VALUES ARE GIVEN FOR EACH
C                     POINT ON THE FRONT
                      VAPACR(1)=3
                      CALL TBLIVA(TABLE,3,LIPACR,VAPACR,R8B,CBID,K8B,
     &                           K8B,R8B,'G',CTYPE,VALI,JR,VALC,
     &                           VALK,IRET)
                      IF (IRET.EQ.0) CALL U2MESS('F','XFEM2_68')

                   ELSE

C                     THE COMP_LINE OPTION HAS BEEN USED. THEREFORE ONLY
C                     ONE VALUE MUST BE GIVEN IN THE SIF TABLE
C                     (REFERENCE LOAD CASE).
                      VAPACR(1)=2
                      CALL TBLIVA(TABLE,3,LIPACR,VAPACR,R8B,CBID,K8B,
     &                           K8B,R8B,'G',CTYPE,VALI,JR,VALC,
     &                           VALK,IRET)
                      IF (IRET.EQ.0) CALL U2MESS('F','XFEM2_82')

C                     THE VALUES OF G MUST BE CALCULATED FROM THE VALUE
C                     OF THE REFERENCE LOAD CASE
                      G2 = (CMIN**2)*G1
                      G1 = (CMAX**2)*G1
                      BETA2 = BETA1
                   ENDIF

C                  CALCULATE THE VALUE OF THE CRACK ADVANCEMENT SPEED IN
C                  THE CRACK FRONT USING THE PARIS LAW
                   IF ((G1.LT.0.D0).OR.(G2.LT.0.D0)) 
     &                                    CALL U2MESS('F','XFEM2_66')

                   IF ((G1.LT.R8PREM()).AND.(G2.LT.R8PREM()))
     &                                    CALL U2MESS('F','XFEM2_83')

                   IF (ABS(G1-G2).LT.R8PREM())
     &                                    CALL U2MESS('F','XFEM2_84')

                   IF (R0CYC) THEN
                       R = 0
                   ELSE
                      IF (G1.GT.G2) THEN
                          R = SQRT(G2/G1)
                      ELSE
                          R = SQRT(G1/G2)
                      ENDIF
                   ENDIF
                   
                   VPNT = (C/(1-R)**N)*
     &               (SQRT(MATE/(1-MATNU**2))*ABS(SQRT(G1)-SQRT(G2)))**M

                 ELSE

                   IF (TEST(1:3).EQ.'OUI') THEN
                       VPNT  = DAMAX
                       BETA1 = 0
                       BETA2 = 0
                   ENDIF

                 ENDIF

C                IF THE ACTUAL CRACK IS THE ONE THAT HAS BEEN SELECTED
C                FOR PROPAGATION, STORE THE VALUES
                 IF (STORE) THEN

C                   ACTIVATE THE WARNING FLAG IF THE CHANGING IN THE
C                   PROPAGATION ANGLE IS GREATER THEN THE TOLERANCE
                    IF (ABS(BETA1-BETA2).GT.BETTOL) BETCHG=.TRUE.

                    IF (.NOT.(NPFMOR)) THEN

C                      STORE THE VALUES
                       CALL ASSERT(ACTPOI.LE.MAXACT)
                       ZR(JVIT-1+ACTPOI)  = VPNT
                       ZR(JBETA-1+ACTPOI) = BETA1

C                      INCREMENT THE POINTER FOR THE ACTUAL POINT ON THE
C                      FRONT
                       ACTPOI = ACTPOI+1

C                      CHECK THE MAXIMUM VALUE OF THE PROPAGATION SPEED
C                      FOR THE SELECTED CRACK
                       IF (VPNT.GT.VMFISS) VMFISS=VPNT

                    ELSE

C                      STORE THE TEMPORARY VALUES
                       ZR(JVITD-1+NPFPNT)  = VPNT
                       ZR(JBETAD-1+NPFPNT) = BETA1

C                      INCREMENT THE POINTER
                       NPFPNT = NPFPNT+1

                    ENDIF

                 ENDIF

C                CHECK THE MAXIMUM SPEED VALUE
                 IF (VPNT.GT.VMAX) VMAX=VPNT

400           CONTINUE

              IF (TEST.EQ.'NON') THEN
C               CHECK THAT THE TABLE DOES NOT CONTAIN ANY OTHER VALUE
                VAPACR(1)=1
                VAPACR(2)=SIFVAL+1
                CALL TBLIVA(TABLE,3,LIPACR,VAPACR,R8B,CBID,K8B,K8B,R8B,
     &                      'G',CTYPE,VALI,G1,VALC,VALK,IRET)
                IF (IRET.EQ.0) CALL U2MESS('F','XFEM2_80')
              ENDIF

C             IF THE NB_POINT_FOND HAS BEEN USED FOR THE ACTUAL PIECE
C             OF THE CRACK FRONT, THEN THE VALUES AT THE PHYSICAL POINT
C             OF THE PIECE ARE EXTRAPOLATED LINEARLY
              IF ((NPFMOR).AND.(STORE)) THEN

C                CREATE THE TEMPORARY DATA STRUCTURE WHERE THE 
C                CURVILINEAR ABSCISSA FOR EACH VIRTUAL POINT CREATED 
C                BY NB_POINT_FOND IS STORED
                 CALL WKVECT('&&XPRCYC.ABSC','V V R8',SIFVAL,JABSC)

C                RETRIEVE THE MAXIMUM VALUE OF THE ABSCISSA FOR THE
C                PHYSICAL POINTS OF THE FRONT
                 ABSMAX = ZR(JFFIS-1+4*(NF-1)+4)

C                CALCULATE THE ABSCISSA FOR THE VIRTUAL POINTS
                 DO 450 I=1,SIFVAL
                    ZR(JABSC-1+I) = ABSMAX/(SIFVAL-1)*(I-1)
450              CONTINUE

C                LOOP ON THE PHYSICAL POINTS OF THE PIECE OF FRONT
                 DO 460 I=1,(NF-NI+1)

                    JMIN=0
                    ABSCUR = ZR(JFFIS-1+4*(I-1)+4)

C                   LOCATE THE VIRTUAL POINTS DEFINING THE SEGMENT
C                   ON THE FRONT TO WHICH THE PHYSICAL POINT BELONGS
                    DO 465 J=1,SIFVAL
                       IF (ZR(JABSC-1+J).LE.ABSCUR) JMIN=J
465                 CONTINUE

C                   CALCULATE THE VALUES OF THE PROPAGATION SPEED AND
C                   ANGLE IN THE PHYSICAL POINT BY LINEAR INTERPOLATION
                    IF (JMIN.EQ.SIFVAL) THEN
                       CALL ASSERT(ACTPOI.LE.MAXACT)
                       ZR(JVIT-1+ACTPOI)  = ZR(JVITD-1+SIFVAL)
                       ZR(JBETA-1+ACTPOI) = ZR(JBETAD-1+SIFVAL)

                    ELSE
                       CALL ASSERT(ACTPOI.LE.MAXACT)
                       ZR(JVIT-1+ACTPOI) = ZR(JVITD-1+JMIN) + 
     &                   (ZR(JVITD-1+JMIN+1)-ZR(JVITD-1+JMIN))/
     &                   (ZR(JABSC-1+JMIN+1)-ZR(JABSC-1+JMIN))*
     &                   (ABSCUR-ZR(JABSC-1+JMIN))
                       ZR(JBETA-1+ACTPOI) = ZR(JBETAD-1+JMIN) + 
     &                   (ZR(JBETAD-1+JMIN+1)-ZR(JBETAD-1+JMIN))/
     &                   (ZR(JABSC-1+JMIN+1)-ZR(JABSC-1+JMIN))*
     &                   (ABSCUR-ZR(JABSC-1+JMIN))

                    ENDIF

                    IF (ZR(JVIT-1+ACTPOI).GT.VMFISS)
     &                                    VMFISS=ZR(JVIT-1+ACTPOI)

                    ACTPOI = ACTPOI+1


460              CONTINUE

C                DESTROY THE TEMPORARY DATA STRUCTURES
                 CALL JEDETR('&&XPRCYC.ABSC')
                 CALL JEDETR('&&XPRCYC.VITESSE')
                 CALL JEDETR('&&XPRCYC.ANGLE')

              ENDIF

            ENDIF

500      CONTINUE

C        INCREMENT THE POINTER FOR THE NB_POINT_FOND LIST IF THIS
C        OPTION HAS NOT BEEN USED FOR THE ACTUAL CRACK (VALUE -1)
         IF ((NBPTFO).AND.(.NOT.(NPFCUR))) NPFPOS=NPFPOS+1

1000  CONTINUE

C     CHECK THAT ALL THE VALUES OF NB_POINT_FOND LIST HAVE BEEN USED
      IF (NBPTFO) THEN
         IF ((NPFPOS-1).LT.NPF) THEN
              VCHAR = 'SUPERIEUR'
              CALL U2MESK('F','XFEM2_78',1,VCHAR)
         ENDIF
      ENDIF

C     CALCULATE THE NUMBER OF CYCLES FOR THE INTEGRATION
      IF (VMAX.LT.R8PREM()) CALL U2MESS('F','XFEM2_74')
      DT = DAMAX/VMAX

C     CALCULATE THE MAXIMUM ADVANCEMENT FOR THE SELECTED CRACK
      DAFISS = DT*VMFISS

C     PRINT SOME INFORMATIONS
      IF (NIV.GT.0) THEN
         WRITE(IFM,*)'AVANCE MAXIMALE DES FISSURES  : ',DAMAX
         WRITE(IFM,*)'NOMBRE DE CYCLES DE FATIGUE   : ',DT
         WRITE(IFM,*)
      ENDIF

C     ISSUE A WARNING MESSAGE IF THE PROPAGATION ANGLE CHANGES IN THE
C     CYCLE
      IF (BETCHG) CALL U2MESS('A','XFEM2_72')

C     ISSUE A WARNING MESSAGE IF THE LOAD RATIO HAS BEEN SET TO ZERO
      IF (R0CYC) CALL U2MESS('A','XFEM2_76')

      IF (TEST.EQ.'NON') THEN
         CALL JEDETR('&&XPRCYC.SIF')
         CALL JEDETR('&&XPRCYC.LISTFISS')
         IF (NBPTFO) CALL JEDETR('&&XPRCYC.NPF')
      ENDIF

5000  FORMAT(4X,'MORCEAU ',I2,': PAS UTILISE')
5001  FORMAT(4X,'MORCEAU ',I2,': ',I3,' POINTS UTILISES')

C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------
      CALL JEDEMA()
      END
