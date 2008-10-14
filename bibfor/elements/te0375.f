      SUBROUTINE TE0375(OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 14/10/2008   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C     BUT:
C         CALCUL DE L'INDICATEUR D'ERREUR EN MECANIQUE
C         SUR UN ELEMENT 3D AVEC LA METHODE DES RESIDUS EXPLICITES.
C         OPTION : 'ERRE_ELEM_SIGM'
C
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN   OPTION : OPTION DE CALCUL
C IN   NOMTE  : NOM DU TYPE ELEMENT
C
C      SORTIE :
C-------------
C
C ......................................................................
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
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
C
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER NPG,I,IPG,TYV,IPOIDS,IVF,IDFDE,IGEOM,IFM,IUNIFI
      INTEGER IAD,IFOR,IERR,IPES,IROT,IMATE
      INTEGER IVOIS,NBNV
      INTEGER NBCMP,ITAB(7),IBID,IP,IR
      INTEGER NOE(9,6,3),IPGF,NDI
      INTEGER NDIM,NNO,NNOS,NNO2,JGANO,IRET,JTIME,NIV,IREF1,IREF2
      INTEGER NBF,NSOMM,ITYP,NDEGRE,NPGB,NPG2,IFF2,IDFDE2,IFA,IATYMA

      REAL*8 DFDX(27),DFDY(27),DFDZ(27),HE,POIDS
      REAL*8 FORX,FORY,FORZ,FPX,FPY,FPZ
      REAL*8 FRX(27),FRY(27),FRZ(27)
      REAL*8 POIDS1(9),POIDS2(9),POIDSF(9),RHO
      REAL*8 DSX,DSY,DSZ,R8BID
      REAL*8 ERREST,NOR,NORSIG,SIGCAL,NUEST,COEFF
      REAL*8 TER1,TER2,TER3,HF,INTE,INST
      REAL*8 NX(27),NY(27),NZ(27),JACO(27),CHX(27),CHY(27),CHZ(27)
      REAL*8 DSG11(27),DSG22(27),DSG33(27),DSG12(27),DSG13(27),DSG23(27)
      REAL*8 SIG11(27),SIG22(27),SIG12(27),SIG33(27),SIG13(27),SIG23(27)

      CHARACTER*2 CODRET(1)
      CHARACTER*8 TYPMAV,ELREFE,ELREF2
      CHARACTER*16 PHENOM

C --- INITIALISATION DU TABLEAU DES NUMEROS DE NOEUDS FACE PAR FACE ----
C ----- HEXAEDRES A 8,20 ET 27 NOEUDS ----------------------------------
C ----- PENTAEDRES A 6 ET 15 NOEUDS ------------------------------------
C ----- TETRAEDRES A 4 ET 10 NOEUDS ------------------------------------
C
      DATA NOE/1,2,3,4,9,10,11,12,21,3,7,8,4,15,19,16,11,24,5,8,7,6,20,
     &     19,18,17,26,1,5,6,2,13,17,14,9,22,2,6,7,3,14,18,15,10,23,4,8,
     &     5,1,16,20,13,12,25,1,2,3,7,8,9,3*0,4,6,5,15,14,13,3*0,1,4,5,
     &     2,10,13,11,7,0,2,5,6,3,11,14,12,8,0,1,3,6,4,9,12,15,10,0,9*0,
     &     1,2,3,5,6,7,3*0,2,4,3,9,10,6,3*0,3,4,1,10,8,7,3*0,1,4,2,8,9,
     &     5,3*0,9*0,9*0/
C
C ----------------------------------------------------------------------
      CALL JEMARQ()

      CALL ELREF1(ELREFE)

      IFM=IUNIFI('MESSAGE')

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL TECACH('OOO','PCONTNO',3,ITAB,IRET)
      CALL JEVECH('PFRVOLU','L',IFOR)
      CALL JEVECH('PERREUR','E',IERR)
      CALL JEVECH('PTEMPSR','L',JTIME)
      INST=ZR(JTIME-1+1)
      IAD=ITAB(1)
      NBCMP=ITAB(2)/NNO

C ----------------------------------------------------------------------
C ---------------- CALCUL DU PREMIER TERME DE L'ERREUR -----------------
C ----------------------------------------------------------------------
C
C ----- CALCUL DU DIAMETRE HE DE LA SPHERE CIRCONSCRITE A L'ELEMENT ----
C
      NIV=1
      CALL UTHK(NOMTE,IGEOM,HE,NDIM,IBID,IBID,IBID,IBID,NIV,IBID)
C
C ----- CALCUL DES FORCES DE PESANTEUR ET DE ROTATION ---------------
C ------- INITIALISATION DES FORCES ------------------------------------
C
      FPX=0.D0
      FPY=0.D0
      FPZ=0.D0
      CALL R8INIR(27,0.D0,FRX,1)
      CALL R8INIR(27,0.D0,FRY,1)
      CALL R8INIR(27,0.D0,FRZ,1)
C
C ------ TEST D'EXISTENCE DES CARTES DE PESA ET ROTA -----------------
C
      CALL TECACH('ONN','PPESANR',1,IP,IRET)
      CALL TECACH('ONN','PROTATR',1,IR,IRET)
      IF (IP.NE.0.OR.IR.NE.0) THEN
        CALL JEVECH('PMATERC','L',IMATE)
        CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
        CALL RCVALA(ZI(IMATE),' ',PHENOM,1,' ',R8BID,1,'RHO',
     &               RHO,CODRET,'FM')
C
C ------ CALCUL DE LA FORCE DE PESANTEUR -------------------------------
C
        IF (IP.NE.0) THEN
          CALL JEVECH('PPESANR','L',IPES)
          FPX=RHO*ZR(IPES)*ZR(IPES+1)
          FPY=RHO*ZR(IPES)*ZR(IPES+2)
          FPZ=RHO*ZR(IPES)*ZR(IPES+3)
        END IF
C
C ------ CALCUL DE LA FORCE DE ROTATION AUX POINTS DE GAUSS ------------
C
        IF (IR.NE.0) THEN
          CALL JEVECH('PROTATR','L',IROT)
          CALL RESR3D(ZR(IROT),ZR(IGEOM),ZR(IVF),RHO,NNO,NPG,
     &                FRX,FRY,FRZ)
        END IF
      END IF
C
C ----- CALCUL DU TERME D'ERREUR AVEC INTEGRATION DE GAUSS -------------
C
      TER1=0.D0
      NORSIG=0.D0
C
      DO 10 IPG=1,NPG
C
C ------- CALCUL DES DERIVEES DES FONCTIONS DE FORMES /X, /Y ET /Z -----
C
        CALL DFDM3D(NNO,IPG,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS)
C
C ------- CALCUL DE LA DIVERGENCE ET DE LA NORME DE SIGMA --------------
C
        CALL ERMEV3(NNO,IPG,IVF,IAD,NBCMP,DFDX,DFDY,DFDZ,
     &              DSX,DSY,DSZ,NOR)
C
C ------- CALCUL DE L'EFFORT VOLUMIQUE ---------------------------------
C
        FORX=ZR(IFOR+3*IPG-3)+FPX+FRX(IPG)
        FORY=ZR(IFOR+3*IPG-2)+FPY+FRY(IPG)
        FORZ=ZR(IFOR+3*IPG-1)+FPZ+FRZ(IPG)
C
C ------- CALCUL DU TERME D'ERREUR AVEC INTEGRATION DE GAUSS -----------
C
        TER1=TER1+((FORX+DSX)**2+(FORY+DSY)**2+(FORZ+DSZ)**2)*POIDS
C
C ------- CALCUL DE LA NORME DE SIGMA SUR L'ELEMENT --------------------
C
        NORSIG=NORSIG+NOR*POIDS
C
   10 CONTINUE
C
      TER1=HE*SQRT(TER1)
C
C ----------------------------------------------------------------------
C ------------ FIN DU CALCUL DU PREMIER TERME DE L'ERREUR --------------
C ----------------------------------------------------------------------
C
C ----------------------------------------------------------------------
C ----------- CALCUL DU DEUXIEME ET TROISIEME TERME DE L'ERREUR --------
C ----------------------------------------------------------------------
C
      CALL JEVECH('PFORCE','L',IREF1)
      CALL JEVECH('PPRESS','L',IREF2)
      CALL JEVECH('PVOISIN','L',IVOIS)
C
C --------- TEST SUR LE TYPE DE LA MAILLE COURANTE --------------------
C
      ELREF2=' '
      IF (ELREFE.EQ.'HE8') THEN
        NBF=6
        NSOMM=4
        ITYP=1
        ELREFE='QU4'
        NBNV=8
        NDEGRE=1
        DO 40 I=1,4
          POIDS1(I)=1.D0
   40   CONTINUE
C
      ELSE IF (ELREFE.EQ.'H20') THEN
        NBF=6
        NSOMM=4
        ITYP=1
        ELREFE='QU8'
        NBNV=20
        NDEGRE=2
        DO 50 I=1,4
          POIDS1(I)=1.D0/9.D0
   50   CONTINUE
        DO 60 I=5,8
          POIDS1(I)=4.D0/9.D0
   60   CONTINUE
        POIDS1(9)=16.D0/9.D0
C
      ELSE IF (ELREFE.EQ.'H27') THEN
        NBF=6
        ITYP=1
        ELREFE='QU9'
        NBNV=27
        NDEGRE=2
        DO 70 I=1,4
          POIDS1(I)=1.D0/9.D0
   70   CONTINUE
        DO 80 I=5,8
          POIDS1(I)=4.D0/9.D0
   80   CONTINUE
        POIDS1(9)=16.D0/9.D0
C
      ELSE IF (ELREFE.EQ.'PE6') THEN
        NBF=5
        ITYP=2
        ELREFE='TR3'
        ELREF2='QU4'
        NBNV=6
        NDEGRE=1
        DO 90 I=1,3
          POIDS1(I)=1.D0/6.D0
   90   CONTINUE
        DO 100 I=1,4
          POIDS2(I)=1.D0
  100   CONTINUE
C
      ELSE IF (ELREFE.EQ.'P15') THEN
        NBF=5
        ITYP=2
        ELREFE='TR6'
        ELREF2='QU8'
        NBNV=15
        NDEGRE=2
        DO 110 I=1,3
          POIDS1(I)=0.D0
  110   CONTINUE
        DO 120 I=4,6
          POIDS1(I)=1.D0/6.D0
  120   CONTINUE
        DO 130 I=1,4
          POIDS2(I)=1.D0/9.D0
  130   CONTINUE
        DO 140 I=5,8
          POIDS2(I)=4.D0/9.D0
  140   CONTINUE
        POIDS2(9)=16.D0/9.D0
C
      ELSE IF (ELREFE.EQ.'TE4') THEN
        NSOMM=3
        NBF=4
        ITYP=3
        ELREFE='TR3'
        NBNV=4
        NDEGRE=1
        DO 150 I=1,3
          POIDS1(I)=1.D0/6.D0
  150   CONTINUE
C
      ELSE IF (ELREFE.EQ.'T10') THEN
        NSOMM=3
        NBF=4
        ITYP=3
        ELREFE='TR6'
        NBNV=10
        NDEGRE=2
        DO 160 I=1,3
          POIDS1(I)=0.D0
  160   CONTINUE
        DO 170 I=4,6
          POIDS1(I)=1.D0/6.D0
  170   CONTINUE
C
      ELSE
        CALL U2MESS('F','ELEMENTS3_84')
      END IF

      CALL ELREF4 (ELREFE,'NOEU',NDIM,NNO,NNOS,NPGB,IPOIDS,IVF,
     &             IDFDE,JGANO)
C
C ----- CAS DES PENTAEDRES ---------------------------------------------
C
      IF (NBF.EQ.5) THEN
         CALL ELREF4 (ELREF2,'NOEU',NDIM,NNO2,NNOS,NPG2,IPOIDS,IFF2,
     &                IDFDE2,JGANO)
      END IF
C
C ----- BOUCLE SUR LES FACES -------------------------------------------
C
      TER2=0.D0
      TER3=0.D0
      DO 300 IFA=1,NBF
C
C ------TEST DU TYPE DE VOISIN -----------------------------------------
C
        IATYMA=ZI(IREF1+3)
        TYV=ZI(IVOIS+7+IFA)
        IF (TYV.NE.0) THEN
          TYPMAV=ZK8(IATYMA-1+TYV)
C	
C ------- CAS DES PENTAEDRES -------------------------------------------
C
          IF (NBF.EQ.5.AND.IFA.GE.3) THEN
            NNO=NNO2
            NPGB=NPG2
            IDFDE=IDFDE2
            DO 180 IPGF=1,NPGB
              POIDSF(IPGF)=POIDS2(IPGF)
  180       CONTINUE
            NSOMM=4
C
          ELSE
C
            DO 190 IPGF=1,NPGB
              POIDSF(IPGF)=POIDS1(IPGF)
  190       CONTINUE
            NSOMM=3
C
          END IF
C
C ----- CALCUL DU DIAMETRE HF DU CERCLE CIRCONSCRIT A LA FACE ----------
C
          NDI=0
          NIV=1
          CALL UTHK(NOMTE,IGEOM,HF,NDI,NOE,NSOMM,ITYP,IFA,NIV,IFM)
C
C ----------------------------------------------------------------------
C --------------- CALCUL DU TROISIEME TERME DE L'ERREUR ----------------
C ----------------------------------------------------------------------
C
          IF (TYPMAV(1:4).EQ.'QUAD'.OR.
     &        TYPMAV(1:4).EQ.'TRIA') THEN
C
C ------- CALCUL DE NORMALES ET JACOBIENS AUX POINTS DE GAUSS ----------
C
            CALL CALNOR('3D',IBID,NNO,NPGB,IBID,IBID,NOE,IGEOM,IDFDE,
     &                   IFA,ITYP,R8BID,HF,
     &                   JACO,NX,NY,NZ,R8BID,R8BID)
C
C ------- CALCUL EFFORTS SURFACIQUES ET DES CONTRAINTES ----------------
C
            CALL ERMEB3(NOE,IFA,ITYP,IREF1,IREF2,IVOIS,IGEOM,IAD,
     &                  NBCMP,INST,NPGB,NX,NY,NZ,
     &                  SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,CHX,CHY,CHZ)
C
C ------- CALCUL DE L'INTEGRALE SUR LA FACE ----------------------------
C
            CALL INTEGA(NPGB,JACO,POIDSF,CHX,CHY,CHZ,
     &                SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,NX,NY,NZ,INTE)
C
C ------- CALCUL DU TERME D'ERREUR -------------------------------------
C
            IF (INTE.LT.0.D0) INTE=-INTE
C
            TER3=TER3+SQRT(HF)*SQRT(INTE)
C
C ----------------------------------------------------------------------
C --------------- CALCUL DU DEUXIEME TERME DE L'ERREUR -----------------
C ----------------------------------------------------------------------
C
          ELSE IF (TYPMAV(1:4).EQ.'HEXA'.OR.
     &             TYPMAV(1:4).EQ.'PENT'.OR.
     &             TYPMAV(1:4).EQ.'TETR') THEN
C
C ------- CAS DES PENTAEDRES -------------------------------------------
C
            IF (NBF.EQ.5) THEN
              IF (TYPMAV(1:4).EQ.'HEXA'.OR.
     &           (TYPMAV(1:4).EQ.'PENT'.AND.IFA.GE.3)) THEN
                NNO=NNO2
                NPGB=NPG2
                IDFDE=IDFDE2
              END IF
            END IF
C
C ------- CALCUL DE NORMALES ET JACOBIENS AUX POINTS DE GAUSS ----------
C
          CALL CALNOR('3D',IBID,NNO,NPGB,IBID,IBID,NOE,IGEOM,IDFDE,
     &                 IFA,ITYP,R8BID,HF,
     &                 JACO,NX,NY,NZ,R8BID,R8BID)
C
C ------- CALCUL DU SAUT DE CONTRAINTE ENTRE ELEMENTS ------------------
C
          CALL ERMES3(NOE,IFA,ITYP,NBNV,NPGB,IREF1,IVOIS,IAD,NBCMP,
     &                DSG11,DSG22,DSG33,DSG12,DSG13,DSG23)
C
C ------- CALCUL DE L'INTEGRALE SUR LA FACE ----------------------------
C
          CALL R8INIR(27,0.D0,CHX,1)
          CALL R8INIR(27,0.D0,CHY,1)
          CALL R8INIR(27,0.D0,CHZ,1)
C
          CALL INTEGA(NPGB,JACO,POIDSF,CHX,CHY,CHZ,
     &                DSG11,DSG22,DSG33,DSG12,DSG13,DSG23,NX,NY,NZ,INTE)
C
C ------- CALCUL DU TERME D'ERREUR -------------------------------------
C
            IF (INTE.LT.0.D0) INTE=-INTE
C
            TER2=TER2+0.5D0*SQRT(HF)*SQRT(INTE)
C
          ELSE
C
            CALL U2MESS('F','ELEMENTS3_84')
C
          END IF
C
        END IF
C
  300 CONTINUE
C
C------------MISE EN MEMOIRE DES DIFFERENTS TERMES DE L'ERREUR ---------
C
      IF (NDEGRE.EQ.2) THEN
        COEFF=SQRT(96.D0)
      ELSE IF (NDEGRE.EQ.1) THEN
        COEFF=SQRT(24.D0)
      END IF
C
      ERREST=(TER1+TER2+TER3)/COEFF
      SIGCAL=SQRT(NORSIG)
      NUEST=100.D0*SQRT(ERREST**2/(ERREST**2+NORSIG))
C
      ZR(IERR)=ERREST
      ZR(IERR+1)=NUEST
      ZR(IERR+2)=SIGCAL
C
      ERREST=TER1/COEFF
      NUEST=100.D0*SQRT(ERREST**2/(ERREST**2+NORSIG))
C
      ZR(IERR+3)=ERREST
      ZR(IERR+4)=NUEST
C
      ERREST=TER3/COEFF
      NUEST=100.D0*SQRT(ERREST**2/(ERREST**2+NORSIG))
C
      ZR(IERR+5)=ERREST
      ZR(IERR+6)=NUEST
C
      ERREST=TER2/COEFF
      NUEST=100.D0*SQRT(ERREST**2/(ERREST**2+NORSIG))
C
      ZR(IERR+7)=ERREST
      ZR(IERR+8)=NUEST
C
      ZR(IERR+9)=HE
C      
      CALL JEDEMA()
      END
