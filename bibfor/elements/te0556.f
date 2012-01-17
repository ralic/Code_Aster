      SUBROUTINE TE0556 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 16/01/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C
C     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
C          CORRESPONDANT A UNE IMPEDANCE ANECHOIQUE
C          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
C
C          OPTION : 'IMPE_ABSO'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER            IPOIDS,IVF,IDFDX,IDFDY,IGEOM,I,J
      INTEGER            NDIM,NNO,IPG,NPG1,INO,JNO
      INTEGER            IDEC,JDEC,KDEC,LDEC,IRES,IMATE,IVITE
      INTEGER            II,MATER,NNOS,JGANO,KPG,SPT
      REAL*8             JAC,NX,NY,NZ,SX(9,9),SY(9,9),SZ(9,9)
      REAL*8             VALRES(3),E,NU,LAMBDA,MU,RHO
      REAL*8             RHOCP,RHOCS
      REAL*8             TAUX,TAUY,TAUZ
      REAL*8             NUX,NUY,NUZ,SCAL,VNX,VNY,VNZ
      REAL*8             VTX,VTY,VTZ,R8B
      INTEGER ICODRE(3)
      CHARACTER*8        NOMRES(3),FAMI,POUM
C     ------------------------------------------------------------------
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDX,JGANO)
      IDFDY  = IDFDX  + 1
C
      CALL JEVECH ( 'PGEOMER', 'L', IGEOM )
      CALL JEVECH ( 'PMATERC', 'L', IMATE )
      CALL JEVECH ( 'PVITPLU', 'L', IVITE )
      CALL JEVECH ( 'PVECTUR', 'E', IRES  )
C
      DO 20 I = 1 , 3*NNO
         ZR(IRES+I-1) = 0.0D0
 20   CONTINUE
C
      MATER=ZI(IMATE)
      NOMRES(1)='E'
      NOMRES(2)='NU'
      NOMRES(3)='RHO'
      FAMI='FPG1'
      KPG=1
      SPT=1
      POUM='+'
      CALL RCVALB(FAMI,KPG,SPT,POUM,MATER,' ','ELAS',0,' ',R8B,3,
     &            NOMRES,VALRES,ICODRE,1)
      E = VALRES(1)
      IF (E.LT.1.D-1) GOTO 999
      NU = VALRES(2)
      RHO = VALRES(3)

      LAMBDA = E*NU/(1.D0+NU)/(1.D0-2.D0*NU)
      MU = E/2.D0/(1.D0+NU)

      RHOCP = SQRT((LAMBDA+2.D0*MU)*RHO)
      RHOCS = SQRT(MU*RHO)
C
C     --- CALCUL DES PRODUITS VECTORIELS OMI X OMJ ---
C
      DO 30 INO = 1 , NNO
         I = IGEOM + 3*(INO-1) -1
         DO 32 JNO = 1,NNO
            J = IGEOM + 3*(JNO-1) -1
            SX(INO,JNO) = ZR(I+2) * ZR(J+3) - ZR(I+3) * ZR(J+2)
            SY(INO,JNO) = ZR(I+3) * ZR(J+1) - ZR(I+1) * ZR(J+3)
            SZ(INO,JNO) = ZR(I+1) * ZR(J+2) - ZR(I+2) * ZR(J+1)
 32      CONTINUE
 30   CONTINUE
C
C     --- BOUCLE SUR LES POINTS DE GAUSS ---
C
      DO 100 IPG = 1 , NPG1
         KDEC = (IPG-1)*NNO*NDIM
         LDEC = (IPG-1)*NNO
C
         NX = 0.0D0
         NY = 0.0D0
         NZ = 0.0D0
C
C        --- CALCUL DE LA NORMALE AU POINT DE GAUSS IPG ---
C
         DO 102 I = 1 , NNO
            IDEC = (I-1)*NDIM
            DO 102 J = 1 , NNO
               JDEC = (J-1)*NDIM
           NX = NX + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SX(I,J)
           NY = NY + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SY(I,J)
           NZ = NZ + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SZ(I,J)
 102     CONTINUE
C
C        --- LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE ---
C
         JAC = SQRT (NX*NX + NY*NY + NZ*NZ)
C
C        --- CALCUL DE LA NORMALE UNITAIRE ---
C
          NUX = NX / JAC
          NUY = NY / JAC
          NUZ = NZ / JAC
C
C        --- CALCUL DE V.N ---
C
          SCAL = 0.D0
          DO 110 I = 1,NNO
             II = 3*I-2
             SCAL = SCAL+NUX*ZR(IVF+LDEC+I-1)*ZR(IVITE+II-1)
             SCAL = SCAL+NUY*ZR(IVF+LDEC+I-1)*ZR(IVITE+II+1-1)
             SCAL = SCAL+NUZ*ZR(IVF+LDEC+I-1)*ZR(IVITE+II+2-1)
110       CONTINUE
C
C        --- CALCUL DE LA VITESSE NORMALE ET DE LA VITESSE TANGENCIELLE
C
          VNX = NUX*SCAL
          VNY = NUY*SCAL
          VNZ = NUZ*SCAL

          VTX = 0.D0
          VTY = 0.D0
          VTZ = 0.D0

          DO 120 I = 1,NNO
             II = 3*I-2
             VTX = VTX+ZR(IVF+LDEC+I-1)*ZR(IVITE+II-1)
             VTY = VTY+ZR(IVF+LDEC+I-1)*ZR(IVITE+II+1-1)
             VTZ = VTZ+ZR(IVF+LDEC+I-1)*ZR(IVITE+II+2-1)
120       CONTINUE

          VTX = VTX - VNX
          VTY = VTY - VNY
          VTZ = VTZ - VNZ
C
C        --- CALCUL DU VECTEUR CONTRAINTE
C
          TAUX = RHOCP*VNX + RHOCS*VTX
          TAUY = RHOCP*VNY + RHOCS*VTY
          TAUZ = RHOCP*VNZ + RHOCS*VTZ
C
C        --- CALCUL DU VECTEUR ELEMENTAIRE
C
          DO 130 I = 1,NNO
             II = 3*I-2
             ZR(IRES+II-1) = ZR(IRES+II-1) +
     &        TAUX*ZR(IVF+LDEC+I-1)*JAC*ZR(IPOIDS+IPG-1)
             ZR(IRES+II+1-1) = ZR(IRES+II+1-1) +
     &        TAUY*ZR(IVF+LDEC+I-1)*JAC*ZR(IPOIDS+IPG-1)
             ZR(IRES+II+2-1) = ZR(IRES+II+2-1) +
     &        TAUZ*ZR(IVF+LDEC+I-1)*JAC*ZR(IPOIDS+IPG-1)
130       CONTINUE

100    CONTINUE

999    CONTINUE

       END
