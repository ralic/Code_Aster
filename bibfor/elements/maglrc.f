      SUBROUTINE MAGLRC (NNO,COMPOR,PGL,MATR,MATI,DELAS,ECR)

      IMPLICIT NONE

C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C     CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL,VECTEU,MATRIC,TEMPNO
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C

      REAL*8    MATR(*)
      INTEGER   MATI(*)


      REAL*8   MG(3), ROT(9), DH(9), R, R8B
      INTEGER  NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,IVF,IDFDX,IDFD2,JGANO

      INTEGER  IMATE,  IRET,   ICONTM, IVARIM
      INTEGER  ICOMPO, ICACOQ, ICONTP, IVARIP, INO,   NBCON
      INTEGER  NBVAR,  NDIMV,  IVARIX, L,      IPG,    JVARI, IVARI
      INTEGER  K
      REAL*8 DELAS(6,6)
      REAL*8 ZERO
      REAL*8 RBIB8,    RBIB1(4),  RBIB2(4),  RBIB3(6), RBIB4(6)
      REAL*8 XAB(3,3), DEFO(2,2) ,DSIDEP(6,6)
      INTEGER I,J

      REAL*8 DF(9),DM(9),DMF(9), ECR(*), ALPH, BETA, VEL
      INTEGER  JTAB(7), COD, ITABP(8),ITABM(8)
      REAL*8   DEUX, CTOR, LC
      REAL*8   T2EV(4),T2VE(4),T1VE(9),CARAT3(21),JACOB(5),CARAQ4(25)

      CHARACTER*2    CODRES(7)
      CHARACTER*8    NOMRES(7), NFX1
      CHARACTER*16   COMPOR(*)
      REAL*8         VALRES(7), PGL(3,3),VGLOB(3)
      REAL*8         MP1(3), MP2(3), C1(3,3), C2(3,3)
      REAL*8         MF1, MF2, QP1,  QP2, GAMMA, E, NU
      REAL*8         DC(2,2), DCI(2,2), DMC(3,2), DFC(3,2), EPAIS
      INTEGER        JCOQU, NBVAL, RPI, IER, JNOMA
      REAL*8         PAR1, PAR2, AX, AY, EA, HZA, NUEQ

      INTEGER       JMVK, IBID,FL,KL,RL,CL
      INTEGER       NF0, NNM0
      CHARACTER*8   NOMCHM, MAT, NMAT(100),  K8BID, NFON

      INTEGER         IFEPX,IPHEN
      CHARACTER*16    PHENOM
      CHARACTER*24    CGLR,FEPX(12)

C----------------------------------

      CALL JEMARQ()

      CALL JEVECH('PMATERC','L',IMATE)


      PHENOM = 'GLRC_DAMAGE'
C      CALL RCCOMA ( ZI(IMATE), 'GLRC' , PHENOM, CODRES)
      IF(PHENOM . NE . 'GLRC_DAMAGE' ) THEN
C      IF(PHENOM . NE . 'GLRC' ) THEN
        CALL U2MESK('F','ELEMENTS2_32',1,PHENOM)
      ENDIF

C      IF ( COMPOR(1)(1:11).EQ. 'GLRC_DAMAGE') THEN
        MATI(1) = 1
C      ELSE
C        MATI(1) = 0
C      ENDIF

      CALL JEVECH('PCACOQU','L',JCOQU)
      EPAIS = ZR(JCOQU)

C--------ELAS----------------------

      NOMRES(1)  = 'E'
      NOMRES(2)  = 'NU'

      CALL RCVALA(ZI(IMATE),' ','ELAS            ',0,' ',R8B,2,NOMRES,
     &              VALRES,CODRES,'FM')
      E     = VALRES(1)
      NU    = VALRES(2)



C--------GLRC_ACIER----------------------en attente de validation

      NOMRES(1)  = 'AX'

      CALL RCVALA(ZI(IMATE),' ','GLRC_ACIER        ',0,' ',R8B,1
     &              ,NOMRES,VALRES,CODRES,' ')
      AX     = VALRES(1)

      IF (CODRES(1) .EQ. 'OK') THEN
        NOMRES(1)  = 'AY'
        NOMRES(2)  = 'E'
        NOMRES(3)  = 'ENROB'
        CALL RCVALA(ZI(IMATE),' ','GLRC_ACIER        ',0,' ',R8B,3
     &                ,NOMRES,VALRES,CODRES,'FM')
        AY     = VALRES(1)
        EA     = VALRES(2)
        HZA    = VALRES(3)
        HZA = EPAIS*0.5D0 - HZA

C       DELAS(1,1) = DELAS(1,1) + 2.0D0*EA*AX
C       DELAS(2,2) = DELAS(2,2) + 2.0D0*EA*AY
C
C       DELAS(4,4) = DELAS(4,4) + 2.0D0*EA*HZA*HZA *AX
C       DELAS(5,5) = DELAS(5,5) + 2.0D0*EA*HZA*HZA *AY

        PAR1 = E*EPAIS**3 / (1.0D0 - NU*NU) / 12.0D0
        PAR2 = 2.0D0*EA*HZA*HZA*(AX + AY) / 2.0D0

        NUEQ = NU * PAR1 / (PAR1 + PAR2)

        E    = E * (PAR1 + PAR2) / PAR1
     &           * (1.0D0 - NU*NU) / (1.0D0 - NUEQ*NUEQ)
        NU   = NUEQ

      ELSE
        AX  = 0.0D0
        AY  = 0.0D0
        EA  = 0.0D0
        HZA = 0.0D0
      ENDIF

      PAR1 = E*EPAIS / (1.0D0 - NU*NU)
      PAR2 = PAR1*EPAIS*EPAIS/12
      CALL R8INIR(6*6,0.D0,DELAS,1)

      DELAS(1,1) = PAR1
      DELAS(1,2) = PAR1*NU
      DELAS(2,1) = DELAS(1,2)
      DELAS(2,2) = PAR1
      DELAS(3,3) = PAR1*(1.0D0 - NU)/2.0D0
      DELAS(4,4) = PAR2
      DELAS(4,5) = PAR2*NU
      DELAS(5,4) = DELAS(4,5)
      DELAS(5,5) = PAR2
      DELAS(6,6) = PAR2*(1.0D0 - NU)/2.0D0

      MATR(1) = 1.0D0
      MATR(2) = DELAS(1,1)
      MATR(3) = DELAS(1,2)
      MATR(4) = DELAS(2,2)
      MATR(5) = DELAS(3,3)
      MATR(14) = 0.0D0
C       MATR(15) = 0.0D0

C--------GLRC_DAMAGE----------------------



      NOMRES(1)  = 'CX1'
      NOMRES(2)  = 'CX2'

      CALL RCVALA( ZI(IMATE),' ',PHENOM,0,' ',R8B,2,NOMRES,
     &                 VALRES,CODRES,'FM')

      C1(1,1)  = VALRES(1)
      C2(1,1)  = VALRES(2)

      NOMRES(1)  = 'CY1'
      NOMRES(2)  = 'CY2'

      CALL RCVALA( ZI(IMATE),' ',PHENOM,0,' ',R8B,2,NOMRES,
     &                 VALRES,CODRES,'FM')

      C1(2,2) = VALRES(1)
      C2(2,2) = VALRES(2)

      NOMRES(1)  = 'CXY1'
      NOMRES(2)  = 'CXY2'

      CALL RCVALA( ZI(IMATE),' ',PHENOM,0,' ',R8B,2,NOMRES,
     &                 VALRES,CODRES,'FM')

      C1(3,3) = VALRES(1)
      C2(3,3) = VALRES(2)

      MATR(16) = C1(1,1)
      MATR(17) = C1(1,1)
      MATR(18) = C1(1,1)
      MATR(19) = C2(1,1)
      MATR(20) = C2(1,1)
      MATR(21) = C2(1,1)
      MATR(22) = C1(1,1)
      MATR(23) = C1(1,1)
      MATR(24) = C1(1,1)
      MATR(25) = C2(1,1)
      MATR(26) = C2(1,1)
      MATR(27) = C2(1,1)

C      MATR(28) = MP1(1)
      MATR(28) = 0.0D0
C      MATR(29) = MP1(2)
      MATR(29) = 0.0D0
C      MATR(30) = MP2(1)
      MATR(30) = 0.0D0
C      MATR(31) = MP2(2)
      MATR(31) = 0.0D0

      IF (MATI(1) .EQ. 1) THEN

        NOMRES(1)  = 'MF1'
        NOMRES(2)  = 'MF2'
        NOMRES(3)  = 'QP1'
        NOMRES(4)  = 'QP2'
        NOMRES(5)  = 'GAMMA'

        CALL RCVALA(ZI(IMATE),' ',PHENOM,0,' ',R8B,5,NOMRES,
     &              VALRES,CODRES,'FM')

        MF1   = VALRES(1)
        MF2   = VALRES(2)
        QP1   = VALRES(3)
        QP2   = VALRES(4)
        GAMMA = VALRES(5)

        MATR(6) = E
        MATR(7) = NU
        MATR(8) = MF1
        MATR(9) = MF2
        MATR(10) = QP1
        MATR(11) = QP2
        MATR(12) = GAMMA
      ELSE
        MATR(6) = DELAS(4,4)
        MATR(7) = DELAS(4,5)
        MATR(8) = DELAS(5,5)
        MATR(9) = DELAS(6,6)

        MATR(10) = DELAS(1,4)
        MATR(11) = DELAS(1,5)
        MATR(12) = DELAS(2,5)
        MATR(13) = DELAS(3,6)
      ENDIF

      MATI(2) = 1
      MATI(3) = 3
      MATI(4) = 2
      MATI(5) = 4

      MATI(6) = 5
      MATI(7) = 7
      MATI(8) = 6
      MATI(9) = 8
      MATI(10) = 9
      MATI(11) = 11
      MATI(12) = 10
      MATI(13) = 12

      CALL GETVID(' ','CHAM_MATER',0,1,1,NOMCHM,NBVAL)

      CALL JEVEUO ( NOMCHM//'.CHAMP_MAT .VALE' , 'L', JNOMA )
      MAT = ZK8(JNOMA)

      CALL JELIRA ( MAT//'.GLRC_DAMAG.VALK' ,'LONUTI',KL, K8BID )
      CALL JELIRA ( MAT//'.GLRC_DAMAG.VALR' ,'LONUTI',RL, K8BID )
      CALL JELIRA ( MAT//'.GLRC_DAMAG.VALC' ,'LONUTI',CL, K8BID )
      CALL JEVEUO ( MAT//'.GLRC_DAMAG.VALK' , 'L', JMVK )

      FL = INT( (KL - RL - CL)/2 )
      NF0  = RL + CL + FL
      NNM0 = RL + CL

      DO 70, I = 1,FL
        NFON = ZK8(JMVK-1 + NNM0 + I)
        IF(NFON .EQ. 'FMEX1')   THEN
          FEPX(1) = ZK8(JMVK-1 + NF0 + I)
        ELSEIF(NFON .EQ. 'FMEX2')   THEN
          FEPX(2) = ZK8(JMVK-1 + NF0 + I)
        ELSEIF(NFON .EQ. 'FMEY1')   THEN
          FEPX(3) = ZK8(JMVK-1 + NF0 + I)
        ELSEIF(NFON .EQ. 'FMEY2')   THEN
          FEPX(4) = ZK8(JMVK-1 + NF0 + I)

        ELSEIF(NFON .EQ. 'DFMEX1')   THEN
          FEPX(5) = ZK8(JMVK-1 + NF0 + I)
        ELSEIF(NFON .EQ. 'DFMEX2')   THEN
          FEPX(6) = ZK8(JMVK-1 + NF0 + I)
        ELSEIF(NFON .EQ. 'DFMEY1')   THEN
          FEPX(7) = ZK8(JMVK-1 + NF0 + I)
        ELSEIF(NFON .EQ. 'DFMEY2')   THEN
          FEPX(8) = ZK8(JMVK-1 + NF0 + I)

        ELSEIF(NFON .EQ. 'DDFMEX1')   THEN
          FEPX(9) = ZK8(JMVK-1 + NF0 + I)
        ELSEIF(NFON .EQ. 'DDFMEX2')   THEN
          FEPX(10) = ZK8(JMVK-1 + NF0 + I)
        ELSEIF(NFON .EQ. 'DDFMEY1')   THEN
          FEPX(11) = ZK8(JMVK-1 + NF0 + I)
        ELSEIF(NFON .EQ. 'DDFMEY2')   THEN
          FEPX(12) = ZK8(JMVK-1 + NF0 + I)
        ENDIF

  70  CONTINUE

      CGLR = '&&GLRC.FEPX'
      CALL JEEXIN(CGLR,IFEPX)

      IF (IFEPX .EQ. 0) THEN
        CALL WKVECT(CGLR,'V V K24',12,IFEPX)
      ELSE
        CALL JEVEUO(CGLR,'E',IFEPX)
      ENDIF

      DO 72, I = 1,12
        ZK24(IFEPX - 1 + I) = FEPX(I)
  72  CONTINUE


      CALL JEVECH('PCACOQU','L',ICACOQ)
      ALPH = ZR(ICACOQ + 1)
      BETA = ZR(ICACOQ + 2)
      VGLOB(1) = COS(ALPH)
      VGLOB(2) = SIN(ALPH)
      VGLOB(3) = COS(BETA)
      VEL = VGLOB(1)*VGLOB(1) + VGLOB(2)*VGLOB(2)
      VEL = VEL + VGLOB(3)*VGLOB(3)
      VEL = SQRT(VEL)
      DO 80, I = 1,3
        ECR(10 + I)  = VGLOB(I)/VEL
  80  CONTINUE

      CALL JEDEMA()
      END
