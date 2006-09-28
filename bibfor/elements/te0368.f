      SUBROUTINE TE0368(OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C ----------------------------------------------------------------------
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
C TOLE CRP_20
C
C     BUT:
C         CALCUL DE L'INDICATEUR D'ERREUR EN QUANTITE D'INTERET
C         SUR UN ELEMENT 3D AVEC LA METHODE DES RESIDUS EXPLICITES.
C         OPTION : 'QIRE_ELEM_SIGM'
C
C ----------------------------------------------------------------------
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
      INTEGER IADP,IADD,IFORP,IFORD,IERR,IPESP,IPESD,IROTP,IROTD,IMATE
      INTEGER IVOIS,NBNV
      INTEGER NBCMP,ITABP(7),ITABD(7),IBID,IPP,IRP,IPD,IRD
      INTEGER NOE(9,6,3),IPGF,NDI,IS
      INTEGER NDIM,NNO,NNOS,NNO2,JGANO,IRET,JTIME,NIV
      INTEGER IREFP1,IREFD1,IREFP2,IREFD2
      INTEGER NBF,NSOMM,ITYP,NDEGRE,NPGF,NPG2,IFF2,IDFDE2,IFA,IATYMA

      REAL*8 DFDX(27),DFDY(27),DFDZ(27),HE,POIDS
      REAL*8 FORPX,FORPY,FORPZ,FORDX,FORDY,FORDZ
      REAL*8 FPPX,FPPY,FPPZ,FPDX,FPDY,FPDZ
      REAL*8 FRPX(27),FRPY(27),FRPZ(27),FRDX(27),FRDY(27),FRDZ(27)
      REAL*8 POIDS1(9),POIDS2(9),POIDSF(9),RHO
      REAL*8 DSPX,DSPY,DSPZ,DSDX,DSDY,DSDZ,R8BID
      REAL*8 ERREST,COEFF
      REAL*8 TERPL1,TERMO1,TERPL2,TERMO2,TERPL3,TERMO3
      REAL*8 HF,INTPL,INTMO,INST,S,NX(27),NY(27),NZ(27),JACO(27)
      REAL*8 CHPX(27),CHPY(27),CHPZ(27),CHDX(27),CHDY(27),CHDZ(27)
      REAL*8 SGP11(27),SGP22(27),SGP33(27),SGP12(27),SGP13(27),SGP23(27)
      REAL*8 SGD11(27),SGD22(27),SGD33(27),SGD12(27),SGD13(27),SGD23(27)
      REAL*8 SIGP11(27),SIGP22(27),SIGP33(27),SIGP12(27),SIGP13(27)
      REAL*8 SIGP23(27),SIGD11(27),SIGD22(27),SIGD33(27),SIGD12(27)
      REAL*8 SIGD13(27),SIGD23(27)
      REAL*8 CHPLX(3),CHPLY(3),CHPLZ(3),CHMOX(3),CHMOY(3),CHMOZ(3)
      REAL*8 SOPL11(3),SOPL22(3),SOPL33(3),SOPL12(3),SOPL13(3),SOPL23(3)
      REAL*8 SOMO11(3),SOMO22(3),SOMO33(3),SOMO12(3),SOMO13(3),SOMO23(3)
      REAL*8 SIPL11(3),SIPL22(3),SIPL33(3),SIPL12(3),SIPL13(3),SIPL23(3)
      REAL*8 SIMO11(3),SIMO22(3),SIMO33(3),SIMO12(3),SIMO13(3),SIMO23(3)
      REAL*8 NUPLUS,NUMOIN

      CHARACTER*2 CODRET,CODMES(1)
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
      CALL TECACH('OOO','PCONTNOP',3,ITABP,IRET)
      CALL TECACH('OOO','PCONTNOD',3,ITABD,IRET)
      CALL JEVECH('PFRVOLUP','L',IFORP)
      CALL JEVECH('PFRVOLUD','L',IFORD)
      CALL JEVECH('PERREUR','E',IERR)
      CALL JEVECH('PTEMPSR','L',JTIME)
C
C ----- CALCUL DU COEFFICIENT S ----------------------------------------
C
      CALL JEVECH('PCONSTR','L',IS)
      S=ZR(IS-1+1)
C
      INST=ZR(JTIME-1+1)
      IADP=ITABP(1)
      IADD=ITABD(1)
      NBCMP=ITABP(2)/NNO

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
      FPPX=0.D0
      FPPY=0.D0
      FPPZ=0.D0
      FPDX=0.D0
      FPDY=0.D0
      FPDZ=0.D0
      CALL R8INIR(27,0.D0,FRPX,1)
      CALL R8INIR(27,0.D0,FRPY,1)
      CALL R8INIR(27,0.D0,FRPZ,1)
      CALL R8INIR(27,0.D0,FRDX,1)
      CALL R8INIR(27,0.D0,FRDY,1)
      CALL R8INIR(27,0.D0,FRDZ,1)
C
C ------ TEST D'EXISTENCE DES CARTES DE PESA ET ROTA -----------------
C
      CALL TECACH('ONN','PPESANRP',1,IPP,IRET)
      CALL TECACH('ONN','PROTATRP',1,IRP,IRET)
      CALL TECACH('ONN','PPESANRD',1,IPD,IRET)
      CALL TECACH('ONN','PROTATRD',1,IRD,IRET)
      IF (IPP.NE.0.OR.IRP.NE.0.OR.IPD.NE.0.OR.IRD.NE.0) THEN
        CALL JEVECH('PMATERC','L',IMATE)
        CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
        CALL RCVALA(ZI(IMATE),' ',PHENOM,1,' ',R8BID,1,'RHO',
     &               RHO,CODMES,'FM')
C
C ------ CALCUL DE LA FORCE DE PESANTEUR PB. PRIMAL --------------------
C
        IF (IPP.NE.0) THEN
          CALL JEVECH('PPESANRP','L',IPESP)
          FPPX=RHO*ZR(IPESP)*ZR(IPESP+1)
          FPPY=RHO*ZR(IPESP)*ZR(IPESP+2)
          FPPZ=RHO*ZR(IPESP)*ZR(IPESP+3)
        END IF
C
C ------ CALCUL DE LA FORCE DE PESANTEUR PB. DUAL ----------------------
C
        IF (IPD.NE.0) THEN
          CALL JEVECH('PPESANRD','L',IPESD)
          FPDX=RHO*ZR(IPESD)*ZR(IPESD+1)
          FPDY=RHO*ZR(IPESD)*ZR(IPESD+2)
          FPDZ=RHO*ZR(IPESD)*ZR(IPESD+3)
        END IF
C
C ------ CALCUL DE LA FORCE DE ROTATION AUX POINTS DE GAUSS PB. PRIMAL -
C
        IF (IRP.NE.0) THEN
          CALL JEVECH('PROTATRP','L',IROTP)
          CALL RESR3D(ZR(IROTP),ZR(IGEOM),ZR(IVF),RHO,NNO,NPG,
     &                FRPX,FRPY,FRPZ)
        END IF
C
C ------ CALCUL DE LA FORCE DE ROTATION AUX POINTS DE GAUSS PB. DUAL ---
C
        IF (IRD.NE.0) THEN
          CALL JEVECH('PROTATRD','L',IROTD)
          CALL RESR3D(ZR(IROTD),ZR(IGEOM),ZR(IVF),RHO,NNO,NPG,
     &                FRDX,FRDY,FRDZ)
        END IF
      END IF
C
C ----- CALCUL DU TERME D'ERREUR AVEC INTEGRATION DE GAUSS -------------
C
      TERPL1=0.D0
      TERMO1=0.D0
C
      DO 10 IPG=1,NPG
C
C ------- CALCUL DES DERIVEES DES FONCTIONS DE FORMES /X, /Y ET /Z -----
C
        CALL DFDM3D(NNO,IPG,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS)
C
C ------- CALCUL DE LA DIVERGENCE ET DE LA NORME DE SIGMA PB. PRIMAL ---
C
        CALL ERMEV3(NNO,IPG,IVF,IADP,NBCMP,DFDX,DFDY,DFDZ,
     &              DSPX,DSPY,DSPZ,R8BID)
C
C ------- CALCUL DE LA DIVERGENCE ET DE LA NORME DE SIGMA PB. DUAL -----
C
        CALL ERMEV3(NNO,IPG,IVF,IADD,NBCMP,DFDX,DFDY,DFDZ,
     &              DSDX,DSDY,DSDZ,R8BID)
C
C ------- CALCUL DE L'EFFORT VOLUMIQUE PB. PRIMAL ----------------------
C
        FORPX=ZR(IFORP+3*IPG-3)+FPPX+FRPX(IPG)
        FORPY=ZR(IFORP+3*IPG-2)+FPPY+FRPY(IPG)
        FORPZ=ZR(IFORP+3*IPG-1)+FPPZ+FRPZ(IPG)
C
C ------- CALCUL DE L'EFFORT VOLUMIQUE PB. DUAL ------------------------
C
        FORDX=ZR(IFORD+3*IPG-3)+FPDX+FRDX(IPG)
        FORDY=ZR(IFORD+3*IPG-2)+FPDY+FRDY(IPG)
        FORDZ=ZR(IFORD+3*IPG-1)+FPDZ+FRDZ(IPG)
C
C ------- CALCUL DU TERME D'ERREUR AVEC INTEGRATION DE GAUSS -----------
C
        TERPL1=TERPL1+((S*(FORPX+DSPX)+(1.D0/S)*(FORDX+DSDX))**2
     &                +(S*(FORPY+DSPY)+(1.D0/S)*(FORDY+DSDY))**2
     &                +(S*(FORPZ+DSPZ)+(1.D0/S)*(FORDZ+DSDZ))**2)*POIDS
C
        TERMO1=TERMO1+((S*(FORPX+DSPX)-(1.D0/S)*(FORDX+DSDX))**2
     &                +(S*(FORPY+DSPY)-(1.D0/S)*(FORDY+DSDY))**2
     &                +(S*(FORPZ+DSPZ)-(1.D0/S)*(FORDZ+DSDZ))**2)*POIDS
C
   10 CONTINUE
C
      TERPL1=(HE**2)*(SQRT(TERPL1))**2
      TERMO1=(HE**2)*(SQRT(TERMO1))**2
C
C ----------------------------------------------------------------------
C ------------ FIN DU CALCUL DU PREMIER TERME DE L'ERREUR --------------
C ----------------------------------------------------------------------
C
C ----------------------------------------------------------------------
C ----------- CALCUL DU DEUXIEME ET TROISIEME TERME DE L'ERREUR --------
C ----------------------------------------------------------------------
C
      CALL JEVECH('PFORCEP','L',IREFP1)
      CALL JEVECH('PPRESSP','L',IREFP2)
      CALL JEVECH('PFORCED','L',IREFD1)
      CALL JEVECH('PPRESSD','L',IREFD2)
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

      CALL ELREF4 (ELREFE,'NOEU',NDIM,NNO,NNOS,NPGF,IPOIDS,IVF,
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
      TERPL2=0.D0
      TERMO2=0.D0
      TERPL3=0.D0
      TERMO3=0.D0
      DO 300 IFA=1,NBF
C
C ------TEST DU TYPE DE VOISIN -----------------------------------------
C
        IATYMA=ZI(IREFP1+3)
        TYV=ZI(IVOIS+7+IFA)
        IF (TYV.NE.0) THEN
          TYPMAV=ZK8(IATYMA-1+TYV)
C	
C ------- CAS DES PENTAEDRES -------------------------------------------
C
          IF (NBF.EQ.5.AND.IFA.GE.3) THEN
            NNO=NNO2
            NPGF=NPG2
            IDFDE=IDFDE2
            DO 180 IPGF=1,NPGF
              POIDSF(IPGF)=POIDS2(IPGF)
  180       CONTINUE
            NSOMM=4
C
          ELSE
C
            DO 190 IPGF=1,NPGF
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
            CALL CALNOR('3D',IBID,NNO,NPGF,IBID,IBID,NOE,IGEOM,IDFDE,
     &                   IFA,ITYP,R8BID,HF,
     &                   JACO,NX,NY,NZ,R8BID,R8BID)
C
C ------- CALCUL EFFORTS SURFACIQUES ET DES CONTRAINTES PB. PRIMAL -----
C
            CALL ERMEB3(NOE,IFA,ITYP,IREFP1,IREFP2,IVOIS,IGEOM,IADP,
     &                  NBCMP,INST,NPGF,NX,NY,NZ,SIGP11,SIGP22,SIGP33,
     &                  SIGP12,SIGP13,SIGP23,CHPX,CHPY,CHPZ)
C
C ------- CALCUL EFFORTS SURFACIQUES ET DES CONTRAINTES PB. DUAL -------
C
            CALL ERMEB3(NOE,IFA,ITYP,IREFD1,IREFD2,IVOIS,IGEOM,IADD,
     &                  NBCMP,INST,NPGF,NX,NY,NZ,SIGD11,SIGD22,SIGD33,
     &                  SIGD12,SIGD13,SIGD23,CHDX,CHDY,CHDZ)
C
C ------- CALCUL EFFORTS SURFACIQUES ET DES CONTRAINTES PB. GLOBAL -----
C
            DO 200 IPGF=1,NPGF
              CHPLX(IPGF)=S*CHPX(IPGF)+(1.D0/S)*CHDX(IPGF)
              CHPLY(IPGF)=S*CHPY(IPGF)+(1.D0/S)*CHDY(IPGF)
              CHPLZ(IPGF)=S*CHPZ(IPGF)+(1.D0/S)*CHDZ(IPGF)
              SIPL11(IPGF)=S*SIGP11(IPGF)+(1.D0/S)*SIGD11(IPGF)
              SIPL22(IPGF)=S*SIGP22(IPGF)+(1.D0/S)*SIGD22(IPGF)
              SIPL33(IPGF)=S*SIGP33(IPGF)+(1.D0/S)*SIGD33(IPGF)
              SIPL12(IPGF)=S*SIGP12(IPGF)+(1.D0/S)*SIGD12(IPGF)
              SIPL13(IPGF)=S*SIGP13(IPGF)+(1.D0/S)*SIGD13(IPGF)
              SIPL23(IPGF)=S*SIGP23(IPGF)+(1.D0/S)*SIGD23(IPGF)
C
              CHMOX(IPGF)=S*CHPX(IPGF)-(1.D0/S)*CHDX(IPGF)
              CHMOY(IPGF)=S*CHPY(IPGF)-(1.D0/S)*CHDY(IPGF)
              CHMOZ(IPGF)=S*CHPZ(IPGF)-(1.D0/S)*CHDZ(IPGF)
              SIMO11(IPGF)=S*SIGP11(IPGF)-(1.D0/S)*SIGD11(IPGF)
              SIMO22(IPGF)=S*SIGP22(IPGF)-(1.D0/S)*SIGD22(IPGF)
              SIMO33(IPGF)=S*SIGP33(IPGF)-(1.D0/S)*SIGD33(IPGF)
              SIMO12(IPGF)=S*SIGP12(IPGF)-(1.D0/S)*SIGD12(IPGF)
              SIMO13(IPGF)=S*SIGP13(IPGF)-(1.D0/S)*SIGD13(IPGF)
              SIMO23(IPGF)=S*SIGP23(IPGF)-(1.D0/S)*SIGD23(IPGF)
C
  200       CONTINUE
C
C ------- CALCUL DE L'INTEGRALE SUR LA FACE ----------------------------
C
            CALL INTEGA(NPGF,JACO,POIDSF,CHPLX,CHPLY,CHPLZ,
     &                  SIPL11,SIPL22,SIPL33,SIPL12,SIPL13,SIPL23,
     &                  NX,NY,NZ,INTPL)
C
            CALL INTEGA(NPGF,JACO,POIDSF,CHMOX,CHMOY,CHMOZ,
     &                  SIMO11,SIMO22,SIMO33,SIMO12,SIMO13,SIMO23,
     &                  NX,NY,NZ,INTMO)
C
C ------- CALCUL DU TERME D'ERREUR -------------------------------------
C
            IF (INTPL.LT.0.D0) INTPL=-INTPL
            IF (INTMO.LT.0.D0) INTMO=-INTMO
C
            TERPL3=TERPL3+HF*(SQRT(INTPL))**2
            TERMO3=TERMO3+HF*(SQRT(INTMO))**2
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
                NPGF=NPG2
                IDFDE=IDFDE2
              END IF
            END IF
C
C ------- CALCUL DE NORMALES ET JACOBIENS AUX POINTS DE GAUSS ----------
C
          CALL CALNOR('3D',IBID,NNO,NPGF,IBID,IBID,NOE,IGEOM,IDFDE,
     &                 IFA,ITYP,R8BID,HF,
     &                 JACO,NX,NY,NZ,R8BID,R8BID)
C
C ------- CALCUL DU SAUT DE CONTRAINTE ENTRE ELEMENTS PB. PRIMAL -------
C
          CALL ERMES3(NOE,IFA,ITYP,NBNV,NPGF,IREFP1,IVOIS,IADP,NBCMP,
     &                SGP11,SGP22,SGP33,SGP12,SGP13,SGP23)
C
C ------- CALCUL DU SAUT DE CONTRAINTE ENTRE ELEMENTS PB. DUAL ---------
C
          CALL ERMES3(NOE,IFA,ITYP,NBNV,NPGF,IREFD1,IVOIS,IADD,NBCMP,
     &                SGD11,SGD22,SGD33,SGD12,SGD13,SGD23)
C
C ------- CALCUL DU SAUT DE CONTRAINTE ENTRE ELEMENTS PB. GLOBAL -------
C
          DO 210 IPGF=1,NPGF
            SOPL11(IPGF)=S*SGP11(IPGF)+(1.D0/S)*SGD11(IPGF)
            SOPL22(IPGF)=S*SGP22(IPGF)+(1.D0/S)*SGD22(IPGF)
            SOPL33(IPGF)=S*SGP33(IPGF)+(1.D0/S)*SGD33(IPGF)
            SOPL12(IPGF)=S*SGP12(IPGF)+(1.D0/S)*SGD12(IPGF)
            SOPL13(IPGF)=S*SGP13(IPGF)+(1.D0/S)*SGD13(IPGF)
            SOPL23(IPGF)=S*SGP23(IPGF)+(1.D0/S)*SGD23(IPGF)
C
            SOMO11(IPGF)=S*SGP11(IPGF)-(1.D0/S)*SGD11(IPGF)
            SOMO22(IPGF)=S*SGP22(IPGF)-(1.D0/S)*SGD22(IPGF)
            SOMO33(IPGF)=S*SGP33(IPGF)-(1.D0/S)*SGD33(IPGF)
            SOMO12(IPGF)=S*SGP12(IPGF)-(1.D0/S)*SGD12(IPGF)
            SOMO13(IPGF)=S*SGP13(IPGF)-(1.D0/S)*SGD13(IPGF)
            SOMO23(IPGF)=S*SGP23(IPGF)-(1.D0/S)*SGD23(IPGF)
  210     CONTINUE
C
C ------- CALCUL DE L'INTEGRALE SUR LA FACE ----------------------------
C
          CALL R8INIR(27,0.D0,CHPLX,1)
          CALL R8INIR(27,0.D0,CHPLY,1)
          CALL R8INIR(27,0.D0,CHPLZ,1)
          CALL R8INIR(27,0.D0,CHMOX,1)
          CALL R8INIR(27,0.D0,CHMOY,1)
          CALL R8INIR(27,0.D0,CHMOZ,1)
C
          CALL INTEGA(NPGF,JACO,POIDSF,CHPLX,CHPLY,CHPLZ,
     &                SOPL11,SOPL22,SOPL33,SOPL12,SOPL13,SOPL23,
     &                NX,NY,NZ,INTPL)
C
          CALL INTEGA(NPGF,JACO,POIDSF,CHMOX,CHMOY,CHMOZ,
     &                SOMO11,SOMO22,SOMO33,SOMO12,SOMO13,SOMO23,
     &                NX,NY,NZ,INTMO)
C
C ------- CALCUL DU TERME D'ERREUR -------------------------------------
C
            IF (INTPL.LT.0.D0) INTPL=-INTPL
            IF (INTMO.LT.0.D0) INTMO=-INTMO
C
            TERPL2=TERPL2+0.5D0*HF*(SQRT(INTPL))**2
            TERMO2=TERMO2+0.5D0*HF*(SQRT(INTMO))**2
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
C ----------------------------------------------------------------------
C ------- FIN DU CALCUL DU DEUXIEME ET TROISIEME TERME DE L'ERREUR -----
C ----------------------------------------------------------------------
C
C ----------------------------------------------------------------------
C ------------ MISE EN MEMOIRE DES DIFFERENTS TERMES DE IERR -----------
C ----------------------------------------------------------------------
C
      IF (NDEGRE.EQ.2) THEN
        COEFF=SQRT(96.D0)
      ELSE IF (NDEGRE.EQ.1) THEN
        COEFF=SQRT(24.D0)
      END IF
C
      NUPLUS=SQRT(TERPL1+TERPL2+TERPL3)
      NUMOIN=SQRT(TERMO1+TERMO2+TERMO3)
      ERREST=(1.D0/4.D0)*(NUPLUS-NUMOIN)/COEFF
C
      ZR(IERR)=ERREST
C
      ERREST=(1.D0/4.D0)*(SQRT(TERPL1)-SQRT(TERMO1))/COEFF
C
      ZR(IERR+3)=ERREST
C
      ERREST=(1.D0/4.D0)*(SQRT(TERPL3)-SQRT(TERMO3))/COEFF
C
      ZR(IERR+5)=ERREST
C
      ERREST=(1.D0/4.D0)*(SQRT(TERPL2)-SQRT(TERMO2))/COEFF
C
      ZR(IERR+7)=ERREST
C
      CALL JEDEMA()
      END
