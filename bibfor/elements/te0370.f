      SUBROUTINE TE0370(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C.......................................................................
C
C     BUT: CALCUL DES MATRICES DE RIGIDITE  ELEMENTAIRES EN MECANIQUE
C          ELEMENTS 2D DE COUPLAGE PESANTEUR-SURFACE LIBRE D'UN FLUIDE
C
C          OPTION : 'RIGI_MECA'
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER            ICODRE
      CHARACTER*8        FAMI,POUM
      CHARACTER*16       NOMTE,OPTION
      REAL*8             A(2,2,27,27)
      REAL*8             RHO
      INTEGER            IGEOM,IMATE
      INTEGER            I,J,K,L,IK,IJKL,LDEC,KDEC,INO,JNO
      INTEGER            NDIM,NNO,IPG,NNOS,JGANO
      INTEGER            IPOIDS,IVF,IDFRDE,IMATUU
      REAL*8                  POIDS
      REAL*8             PESA,JAC,ZERO
      REAL*8             DXDE,DXDK,DYDE,DYDK
      REAL*8             B(54,54),UL(54),C(1485)
      INTEGER            IVECTU,JCRET,NNO2,NT2,N1,N2,NN,KPG,SPT
C
C
C
C-----------------------------------------------------------------------
      INTEGER IDEPLM ,IDEPLP ,LPESA ,NPG2 
      REAL*8 R8B 
C-----------------------------------------------------------------------
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG2,IPOIDS,IVF,IDFRDE,JGANO)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PPESANR','L', LPESA)
      PESA = ZR(LPESA)
      ZERO = 0.D0
      FAMI='FPG1'
      KPG=1
      SPT=1
      POUM='+'
C
      CALL RCVALB(FAMI,KPG,SPT,POUM,ZI(IMATE),' ','FLUIDE',0,' ',R8B,1,
     &          'RHO',RHO, ICODRE,1)
C
C     INITIALISATION DE LA MATRICE
C
      DO 112 K=1,2
         DO 112 L=1,2
            DO 112 I=1,NNO
            DO 112 J=1,I
                A(K,L,I,J) = 0.D0
112   CONTINUE
C
C     BOUCLE SUR LES POINTS DE GAUSS
C
      DO 113 IPG=1,NPG2
C
         KDEC = (IPG-1)*NNO*NDIM
         LDEC = (IPG-1)*NNO
C
         DXDE=0.D0
         DXDK=0.D0
         DYDE=0.D0
         DYDK=0.D0
         DO 100 I=1,NNO
           DXDE=DXDE+ZR(IGEOM+3*(I-1))*ZR(IDFRDE+KDEC+(I-1)*NDIM)
           DXDK=DXDK+ZR(IGEOM+3*(I-1))*ZR(IDFRDE+KDEC+(I-1)*NDIM+1)
           DYDE=DYDE+ZR(IGEOM+3*(I-1)+1)*ZR(IDFRDE+KDEC+(I-1)*NDIM)
           DYDK=DYDK+ZR(IGEOM+3*(I-1)+1)*ZR(IDFRDE+KDEC+(I-1)*NDIM+1)
C
100      CONTINUE
         JAC   = DXDE*DYDK - DXDK*DYDE
         POIDS = ABS(JAC)*ZR(IPOIDS+IPG-1)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C        CALCUL DU TERME RHO * G * Z * Z   C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
         DO 130 INO=1,NNO
            DO 140 JNO=1,INO
               A(2,2,INO,JNO) = A(2,2,INO,JNO) +
     &                          POIDS * RHO * PESA *
     &                          ZR(IVF+LDEC+INO-1) * ZR(IVF+LDEC+JNO-1)
C
140         CONTINUE
130      CONTINUE
113   CONTINUE
C
C PASSAGE DU STOCKAGE RECTANGULAIRE AU STOCKAGE TRIANGULAIRE
C
      IJKL = 0
      IK = 0
      DO 160 K=1,2
         DO 160 L=1,2
            DO 162 I=1,NNO
                IK = ((2*I+K-3) * (2*I+K-2)) / 2
                DO 164 J=1,I
                   IJKL = IK + 2 * (J-1) + L
                   C(IJKL) = A(K,L,I,J)
164             CONTINUE
162         CONTINUE
160   CONTINUE
C
      NNO2 = NNO*2
      NT2 = NNO* (NNO2+1)
      IF (OPTION(1:9).NE.'FULL_MECA'.AND.OPTION(1:9).NE.'RIGI_MECA')
     &  GO TO 9998
      IF (OPTION.EQ.'RIGI_MECA_HYST') THEN
        CALL JEVECH('PMATUUC','E',IMATUU)
        DO 115 I = 1,NT2
          ZC(IMATUU+I-1) = DCMPLX(C(I),ZERO)
115     CONTINUE
      ELSE
        CALL JEVECH('PMATUUR','E',IMATUU)
        DO 114 I = 1,NT2
          ZR(IMATUU+I-1) = C(I)
114     CONTINUE
      END IF
 9998 CONTINUE
C
      IF (OPTION.NE.'FULL_MECA'.AND.OPTION.NE.'RAPH_MECA'
     &     .AND.OPTION.NE.'FORC_NODA') GO TO 9999
      CALL JEVECH('PVECTUR','E',IVECTU)
      CALL JEVECH('PDEPLMR','L',IDEPLM)
      CALL JEVECH('PDEPLPR','L',IDEPLP)
      DO 111 I = 1,NNO2
        ZR(IVECTU+I-1) = 0.D0
        UL(I) = ZR(IDEPLM+I-1) + ZR(IDEPLP+I-1)
111   CONTINUE
C
      NN = 0
      DO 120 N1 = 1,NNO2
        DO 121 N2 = 1,N1
          NN = NN + 1
          B(N1,N2) = C(NN)
          B(N2,N1) = C(NN)
121     CONTINUE
120   CONTINUE
C
      DO 131 N1 = 1,NNO2
        DO 132 N2 = 1,NNO2
          ZR(IVECTU+N1-1) = ZR(IVECTU+N1-1) + B(N1,N2)*UL(N2)
132     CONTINUE
131   CONTINUE
C
 9999 CONTINUE
      IF (OPTION(1:9).EQ.'FULL_MECA' .OR.
     &    OPTION.EQ.'RAPH_MECA') THEN
        CALL JEVECH('PCODRET','E',JCRET)
        ZI(JCRET) = 0
      END IF
C
      END
