      SUBROUTINE TE0374(OPTION,NOMTE)
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
      IMPLICIT NONE
C
C     BUT: CALCUL DES MATRICES ELEMENTAIRES EN MECANIQUE
C          CORRESPONDANT A UN TERME D'IMPEDANCE EN ONDE INCIDENTE
C           IMPOSEE SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
C
C!          OPTION : 'ONDE_FLUI'
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C
C!
      INCLUDE 'jeveux.h'
      INTEGER ICODRE(2)
C!
      CHARACTER*8        NOMRES(2),FAMI,POUM
      CHARACTER*16       NOMTE,OPTION
      REAL*8             NX,NY,NZ,SX(9,9),SY(9,9),SZ(9,9),JAC
      REAL*8             VALRES(2),RHO,CELER
      INTEGER            IPOIDS,IVF,IDFDX,IDFDY,IGEOM,IMATE
      INTEGER            NDIM,NNO,NDI,IPG,NPG2,IMATUU,KPG,SPT
      INTEGER            IDEC,JDEC,KDEC,LDEC,NNOS,JGANO
C
C
C-----------------------------------------------------------------------
      INTEGER I ,II ,IJ ,INO ,IONDE ,J ,JJ 
      INTEGER JNO 
      REAL*8 RBID 
C-----------------------------------------------------------------------
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG2,IPOIDS,IVF,IDFDX,JGANO)
C
      IDFDY  = IDFDX  + 1
      NDI = NNO*(2*NNO+1)
C

      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PONDECR','L',IONDE)
      CALL JEVECH('PMATUUR','E',IMATUU)
      FAMI='FPG1'
      KPG=1
      SPT=1
      POUM='+'
C
      NOMRES(1) = 'RHO'
      NOMRES(2) = 'CELE_R'
      CALL RCVALB (FAMI,KPG,SPT,POUM, ZI(IMATE),' ','FLUIDE',0,' ',RBID,
     &             2,NOMRES,VALRES, ICODRE,1)
      RHO   = VALRES(1)
      CELER = VALRES(2)
C
C --- INITIALISATION DE LA MATRICE D'IMPEDANCE
      DO 10 I = 1,NDI
         ZR(IMATUU+I-1) = 0.D0
10    CONTINUE
C!
      IF (ZR(IONDE).EQ.0.D0) THEN
         GOTO 999
      ELSE
C
C        CALCUL DES PRODUITS VECTORIELS OMI X OMJ
C
         DO 1 INO = 1,NNO
            I = IGEOM + 3*(INO-1) -1
            DO 2 JNO = 1,NNO
               J = IGEOM + 3*(JNO-1) -1
               SX(INO,JNO) = ZR(I+2) * ZR(J+3) - ZR(I+3) * ZR(J+2)
               SY(INO,JNO) = ZR(I+3) * ZR(J+1) - ZR(I+1) * ZR(J+3)
               SZ(INO,JNO) = ZR(I+1) * ZR(J+2) - ZR(I+2) * ZR(J+1)
2           CONTINUE
1        CONTINUE
C
C        BOUCLE SUR LES POINTS DE GAUSS
C
         DO 101 IPG=1,NPG2
            KDEC = (IPG-1)*NNO*NDIM
            LDEC = (IPG-1)*NNO
C
            NX = 0.0D0
            NY = 0.0D0
            NZ = 0.0D0
C
C           CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
C
            DO 102 I=1,NNO
               IDEC = (I-1)*NDIM
            DO 102 J=1,NNO
               JDEC = (J-1)*NDIM
C
           NX = NX + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SX(I,J)
           NY = NY + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SY(I,J)
           NZ = NZ + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SZ(I,J)
C
102         CONTINUE
C
C           CALCUL DU JACOBIEN AU POINT DE GAUSS IPG
C
            JAC = SQRT(NX*NX + NY*NY + NZ*NZ)
C
            DO 103 I=1,NNO
C
               DO 104 J=1,I
                 II = 2*I
                 JJ = 2*J
                 IJ = (II-1)*II/2 + JJ
                 ZR(IMATUU+IJ-1) = ZR(IMATUU+IJ-1) - JAC*
     &           ZR(IPOIDS+IPG-1) * ZR(IVF+LDEC+I-1) * ZR(IVF+LDEC+J-1)
     &           *RHO / CELER
104            CONTINUE
103         CONTINUE
101      CONTINUE
      END IF
999   CONTINUE
      END
