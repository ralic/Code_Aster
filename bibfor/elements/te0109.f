      SUBROUTINE TE0109(OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 20/11/2012   AUTEUR DELMAS J.DELMAS 
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
C
C     BUT:
C       CALCUL DES FLUX DE TEMPERATURE AUX POINTS DE GAUSS
C       ELEMENTS COQUE
C       OPTION : 'FLUX_ELGA'
C
C ---------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'

C
      INTEGER NBRES
      PARAMETER (NBRES=3)

      INTEGER ICODRE(NBRES)
      INTEGER I,KP,ITEMPE,ICACOQ,IMATE,IFLUPG,INBSPI
      INTEGER IVF,IGEOM,IDFDE,IPOIDS,NDIM
      INTEGER NNO,NNOS,NPG,JGANO,KPG,SPT
      INTEGER ITEMPS,K,MATER,NBCMP,CDEC,NBCOU,NIVC

      REAL*8 VALRES(NBRES),CONDUC,H,ORD
      REAL*8 COOR2D(14),DFDX(7),DFDY(7),POIDS,DTDX,DTDY,DTDZ
      REAL*8 TS,TM,TI,DTSDX,DTMDX,DTIDX,DTSDY,DTMDY,DTIDY,PX3
      REAL*8 VA1A2(3),NA1A2,X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3
      REAL*8 PVEC1(3),PVEC2(3),NPVEC1,FX,FY,FZ
      REAL*8 EP,FAC1,FAC2,FAC3

      CHARACTER*8 NOMRES(NBRES),FAMI,POUM
      CHARACTER*16 PHENOM
C
C-----------------------------------------------------------------------
C
      VALRES(1)=0.D0
      VALRES(2)=0.D0
      VALRES(3)=0.D0
      FAMI='FPG1'
      KPG=1
      SPT=1
      POUM='+'
      NBCMP=9
C
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PNBSP_I','L',INBSPI)
      CALL JEVECH('PCACOQU','L',ICACOQ)
      CALL JEVECH('PTEMPER','L',ITEMPE)
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PFLUXPG','E',IFLUPG)

      NBCOU = ZI(INBSPI)
      CALL ASSERT(NBCOU.EQ.1)
C
C --- RECUPERATION DE LA NATURE DU MATERIAU DANS PHENOM
C     -------------------------------------------------
      MATER = ZI(IMATE)
      CALL RCCOMA ( MATER, 'THER', PHENOM, ICODRE )
C
C       --------------------------
C ----- CAS DES COQUES ISOTROPES :
C       --------------------------
      IF ( PHENOM .EQ. 'THER' ) THEN
C
        NOMRES(1) = 'LAMBDA'
        CALL RCVALB(FAMI,KPG,SPT,POUM,MATER,' ','THER',1,'INST',
     &              ZR(ITEMPS),1,NOMRES,VALRES,  ICODRE, 1)
        CONDUC = VALRES(1)
        H = ZR(ICACOQ)/2.D0
        ORD = 0.D0
        EP = 2.D0*H
      ELSE
        CALL U2MESK('F','ELEMENTS3_18',1,PHENOM)
      END IF
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,
     &                IDFDE,JGANO)
C
      DO 10 I = 1,3
        VA1A2(I) = ZR(IGEOM+I+2) - ZR(IGEOM+I-1)
   10 CONTINUE
      NA1A2 = SQRT(VA1A2(1)**2+VA1A2(2)**2+VA1A2(3)**2)
      DO 15 I = 1,3
        VA1A2(I) = VA1A2(I)/NA1A2
   15 CONTINUE
C
      X1 = ZR(IGEOM)
      Y1 = ZR(IGEOM+1)
      Z1 = ZR(IGEOM+2)
      X2 = ZR(IGEOM+3)
      Y2 = ZR(IGEOM+4)
      Z2 = ZR(IGEOM+5)
      X3 = ZR(IGEOM+6)
      Y3 = ZR(IGEOM+7)
      Z3 = ZR(IGEOM+8)
      PVEC1(1) = (Y2-Y1)* (Z3-Z1) - (Z2-Z1)* (Y3-Y1)
      PVEC1(2) = (Z2-Z1)* (X3-X1) - (Z3-Z1)* (X2-X1)
      PVEC1(3) = (X2-X1)* (Y3-Y1) - (X3-X1)* (Y2-Y1)
      NPVEC1 = SQRT(PVEC1(1)**2+PVEC1(2)**2+PVEC1(3)**2)
      DO 20 I = 1,3
        PVEC1(I) = PVEC1(I)/NPVEC1
   20 CONTINUE
C
      PVEC2(1) = (PVEC1(2)*VA1A2(3)-PVEC1(3)*VA1A2(2))
      PVEC2(2) = (PVEC1(3)*VA1A2(1)-PVEC1(1)*VA1A2(3))
      PVEC2(3) = (PVEC1(1)*VA1A2(2)-PVEC1(2)*VA1A2(1))
C
      CALL CQ3D2D(NNO,ZR(IGEOM),1.D0,0.D0,COOR2D)
C
      DO 25 NIVC = -1, 1
C
        IF (NIVC.LT.0) THEN
          PX3 = ORD - EP/2.D0
          CDEC = 3
        ELSE IF (NIVC.GT.0) THEN
          PX3 = ORD + EP/2.D0
          CDEC = 6
        ELSE
          PX3 = ORD
          CDEC = 0
        END IF
C
        DO 30 KP = 1,NPG
          K = (KP-1)*NNO
          CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,COOR2D,DFDX,DFDY,POIDS)
          DTMDX = 0.D0
          DTIDX = 0.D0
          DTSDX = 0.D0
          DTMDY = 0.D0
          DTIDY = 0.D0
          DTSDY = 0.D0
          TM = 0.D0
          TI = 0.D0
          TS = 0.D0
C
          DO 35 I = 1,NNO
            DTMDX = DTMDX + ZR(ITEMPE+3*I-3)*DFDX(I)
            DTMDY = DTMDY + ZR(ITEMPE+3*I-3)*DFDY(I)
            DTIDX = DTIDX + ZR(ITEMPE+3*I-2)*DFDX(I)
            DTIDY = DTIDY + ZR(ITEMPE+3*I-2)*DFDY(I)
            DTSDX = DTSDX + ZR(ITEMPE+3*I-1)*DFDX(I)
            DTSDY = DTSDY + ZR(ITEMPE+3*I-1)*DFDY(I)
            TM = TM + ZR(ITEMPE+3*I-3)*ZR(IVF+K+I-1)
            TI = TI + ZR(ITEMPE+3*I-2)*ZR(IVF+K+I-1)
            TS = TS + ZR(ITEMPE+3*I-1)*ZR(IVF+K+I-1)
   35     CONTINUE
          FAC1 = (1.D0- (PX3/H)**2)
          FAC2 = -PX3* (1.D0-PX3/H)/ (2.D0*H)
          FAC3 = PX3* (1.D0+PX3/H)/ (2.D0*H)
          DTDX = DTMDX*FAC1 + DTIDX*FAC2 + DTSDX*FAC3
          DTDY = DTMDY*FAC1 + DTIDY*FAC2 + DTSDY*FAC3
          DTDZ = TS* (.5D0+PX3/H)/H-2.D0*TM*PX3/H**2-TI*(.5D0-PX3/H)/H
C
          FX = -CONDUC*DTDX
          FY = -CONDUC*DTDY
          FZ = -CONDUC*DTDZ
C
          ZR(IFLUPG+(KP-1)*NBCMP-1+CDEC+1) = FX*VA1A2(1)
     &                                     + FY*PVEC2(1) + FZ*PVEC1(1)
          ZR(IFLUPG+(KP-1)*NBCMP-1+CDEC+2) = FX*VA1A2(2)
     &                                     + FY*PVEC2(2) + FZ*PVEC1(2)
          ZR(IFLUPG+(KP-1)*NBCMP-1+CDEC+3) = FX*VA1A2(3)
     &                                     + FY*PVEC2(3) + FZ*PVEC1(3)
C
   30   CONTINUE
C
   25 CONTINUE
C
      END
