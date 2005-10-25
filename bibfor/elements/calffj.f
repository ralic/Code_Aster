      SUBROUTINE CALFFJ(ALIAS,XI,YI,IGEOM,TN,JAC,IAXIS,NDIM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 24/10/2005   AUTEUR KHAM M.KHAM 
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

      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER IAXIS
      REAL*8 JAC
C.......................................................................

C BUT:   CALCUL DES FONCTIONS DE FORMES ET DE LEURS DERIVEES
C        AU POINT DE COORDONNEES XI,YI,ZI

C ENTREES  --->  ALIAS       : NOM D'ALIAS DE L'ELEMENT
C          --->  XI,YI       : POINT DE CALCUL DES F FORMES ET JACOBIEN
C          --->  IGEOM       : ADRESSE DE LA GEOMETRIE DE L'ELEMENT
C          --->  IAXIS       : 1 SI AXIS 0 SINON

C SORTIES  <---  TN   : FONCTIONS DE FORMES EN XI,YI,ZI
C          <---  JAC  : LE JACOBIEN EN XI,YI,ZI


C   ON CALCULE LES NORMALES POUR LE MAITRE COMME POUR L'ESLAVE
C.......................................................................
C-----------------------------------------------------------------------
C     COMMUNS   JEVEUX
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
      INTEGER ZI,IOC,I,NDIM
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C-----------------------------------------------------------------------

      CHARACTER*8 ALIAS,K8B
      REAL*8 TN(9),AJ(2,9)

      AL31(X) = 0.5D0*X* (X-1.D0)
      AL32(X) = - (X+1.D0)* (X-1.D0)
      AL33(X) = 0.5D0*X* (X+1.D0)
      DAL31(U) = 0.5D0* (2.D0*U-1.D0)
      DAL32(U) = -2.D0*U
      DAL33(U) = 0.5D0* (2.D0*U+1.D0)
      UNS4 = 0.25D0
    
C  ----------------------------------------------------------

      IF (ALIAS(1:5).EQ.'SG2') THEN
C   LES FF EN XI,YI,ZI ET LEURS D

        TN(1) = 0.5D0* (1-XI)
        TN(2) = 0.5D0* (1+XI)
        AJ(1,1) = -0.5D0
        AJ(1,2) = 0.5D0
C   CALCUL DE JACOBIEN  L'ESCLAVE OU DU MAITRE )

        DXDS = 0
        DYDS = 0
        DZDS = 0
        DO 10 I = 1,2
          IF(NDIM.EQ.2) THEN
            DXDS = DXDS + ZR(IGEOM-1+2*(I-1)+1)*AJ(1,I)
            DYDS = DYDS + ZR(IGEOM-1+2*(I-1)+2)*AJ(1,I)
          ELSEIF(NDIM.EQ.3) THEN
            DXDS = DXDS + ZR(IGEOM-1+3*(I-1)+1)*AJ(1,I)
            DYDS = DYDS + ZR(IGEOM-1+3*(I-1)+2)*AJ(1,I)
            DZDS = DZDS + ZR(IGEOM-1+3*(I-1)+3)*AJ(1,I)
          ENDIF
   10   CONTINUE
         IF (IAXIS.EQ.1) THEN
          XX =0.D0
          DO 7 I=1,2
          XX=XX+ ZR(IGEOM-1+2*(I-1)+1)*TN(I)
7         CONTINUE
          IF (XX.EQ.0.D0) XX=0.01D-5
          JAC = SQRT(DXDS**2+DYDS**2)*ABS(XX)
         ELSE
          JAC = SQRT(DXDS**2+DYDS**2+DZDS**2)
         END IF

      ELSE IF (ALIAS(1:5).EQ.'SG3') THEN

C   LES FF EN XI,YI,ZI

        TN(1) = -0.5D0* (1-XI)*XI
        TN(2) = 0.5D0* (1+XI)*XI
        TN(3) = 1.D0* (1+XI)* (1-XI)

C   LES DERIVEES PREMIERES

        AJ(1,1) = -0.5D0* (1-2*XI)
        AJ(1,2) = 0.5D0* (1+2*XI)
        AJ(1,3) = -2.D0*XI

C   CALCUL DE JACOBIEN

        DXDS = 0
        DYDS = 0
        DO 20 I = 1,3
          DXDS = DXDS + ZR(IGEOM-1+2*(I-1)+1)*AJ(1,I)
          DYDS = DYDS + ZR(IGEOM-1+2*(I-1)+2)*AJ(1,I)
   20   CONTINUE
         IF (IAXIS.EQ.1) THEN
          XX =0.D0
          DO 3 I=1,3
          XX = XX+ ZR(IGEOM-1+2*(I-1)+1)*TN(I)
3         CONTINUE
          IF (XX.EQ.0.D0) XX=0.01D-5
          JAC = SQRT(DXDS**2+DYDS**2)*ABS(XX)
         ELSE
          JAC = SQRT(DXDS**2+DYDS**2)
         END IF

      ELSE IF (ALIAS(1:5).EQ.'TR3') THEN

        TN(1) = 0.5D0* (1+YI)
        TN(2) = -0.5D0* (XI+YI)
        TN(3) = 0.5D0* (1+XI)
        AJ(1,1) = 0.D+00
        AJ(1,2) = -0.5D0
        AJ(1,3) = 0.5D0
        AJ(2,1) = 0.5D0
        AJ(2,2) = -0.5D0
        AJ(2,3) = 0.D+00

C   CALCUL DE JACOBIEN

        DXDE = 0.D0
        DXDK = 0.D0
        DYDE = 0.D0
        DYDK = 0.D0
        DZDE = 0.D0
        DZDK = 0.D0

        DO 30 I = 1,3
          DXDE = DXDE + ZR(IGEOM+3*I-3)*AJ(1,I)
          DXDK = DXDK + ZR(IGEOM+3*I-3)*AJ(2,I)
          DYDE = DYDE + ZR(IGEOM+3*I-2)*AJ(1,I)
          DYDK = DYDK + ZR(IGEOM+3*I-2)*AJ(2,I)
          DZDE = DZDE + ZR(IGEOM+3*I-1)*AJ(1,I)
          DZDK = DZDK + ZR(IGEOM+3*I-1)*AJ(2,I)
   30   CONTINUE

        JAC = SQRT((DYDE*DZDK-DZDE*DYDK)**2+ (DZDE*DXDK-DXDE*DZDK)**2+
     &        (DXDE*DYDK-DYDE*DXDK)**2)

C--------------------------------------------------------------------

      ELSE IF (ALIAS.EQ.'TR6') THEN

        TN(1) = 0.5D0* (1.D0+YI)*YI
        TN(2) = 0.5D0* (XI+YI)* (XI+YI+1)
        TN(3) = 0.5D0* (1.D0+XI)*XI
        TN(4) = - (1.D0+YI)* (XI+YI)
        TN(5) = - (1.D0+XI)* (XI+YI)
        TN(6) = (1.D0+XI)* (1.D0+YI)

        AJ(1,1) = 0.D+00
        AJ(1,2) = 0.5D0* (2.D0*XI+2.D0*YI+1.D0)
        AJ(1,3) = 0.5D0* (2.D0*XI+1.D0)
        AJ(1,4) = - (1.D0+YI)
        AJ(1,5) = - (1.D0+YI+2.D0*XI)
        AJ(1,6) = (1.D0+YI)

        AJ(2,1) = 0.5D0* (2.D0*YI+1.D0)
        AJ(2,2) = 0.5D0* (2.D0*XI+2.D0*YI+1.D0)
        AJ(2,3) = 0.D+00
        AJ(2,4) = - (1.D0+XI+2.D0*YI)
        AJ(2,5) = - (1.D0+XI)
        AJ(2,6) = (1.D0+XI)
        
        DXDE = 0.D0
        DXDK = 0.D0
        DYDE = 0.D0
        DYDK = 0.D0
        DZDE = 0.D0
        DZDK = 0.D0


        DO 40 I = 1,6
          DXDE = DXDE + ZR(IGEOM+3*I-3)*AJ(1,I)
          DXDK = DXDK + ZR(IGEOM+3*I-3)*AJ(2,I)
          DYDE = DYDE + ZR(IGEOM+3*I-2)*AJ(1,I)
          DYDK = DYDK + ZR(IGEOM+3*I-2)*AJ(2,I)
          DZDE = DZDE + ZR(IGEOM+3*I-1)*AJ(1,I)
          DZDK = DZDK + ZR(IGEOM+3*I-1)*AJ(2,I)
   40   CONTINUE

        JAC = SQRT((DYDE*DZDK-DZDE*DYDK)**2+ (DZDE*DXDK-DXDE*DZDK)**2+
     &        (DXDE*DYDK-DYDE*DXDK)**2)


      ELSE IF (ALIAS(1:5).EQ.'QU4') THEN

        UNS4 = 0.25D0
        A = 1.D0 + XI
        B = 1.D0 + YI
        C = 1.D0 - XI
        D = 1.D0 - YI
        TN(1) = C*B*UNS4
        TN(2) = C*D*UNS4
        TN(3) = A*D*UNS4
        TN(4) = A*B*UNS4

        AJ(1,1) = -B*UNS4
        AJ(1,2) = -D*UNS4
        AJ(1,3) = D*UNS4
        AJ(1,4) = B*UNS4
        AJ(2,1) = C*UNS4
        AJ(2,2) = -C*UNS4
        AJ(2,3) = -A*UNS4
        AJ(2,4) = A*UNS4
        DXDE=0.D00
        DXDK=0.D00
        DYDE=0.D00
        DYDK=0.D00
        DZDE=0.D00
        DZDK=0.D00
        DO 50 I = 1,4
          DXDE = DXDE + ZR(IGEOM+3*I-3)*AJ(1,I)
          DXDK = DXDK + ZR(IGEOM+3*I-3)*AJ(2,I)
          DYDE = DYDE + ZR(IGEOM+3*I-2)*AJ(1,I)
          DYDK = DYDK + ZR(IGEOM+3*I-2)*AJ(2,I)
          DZDE = DZDE + ZR(IGEOM+3*I-1)*AJ(1,I)
          DZDK = DZDK + ZR(IGEOM+3*I-1)*AJ(2,I)
   50   CONTINUE

        JAC = SQRT((DYDE*DZDK-DZDE*DYDK)**2+ (DZDE*DXDK-DXDE*DZDK)**2+
     &        (DXDE*DYDK-DYDE*DXDK)**2)
C        TAU1(1)= DXDE
C        TAU1(2)= DYDE
C        TAU1(3)= DZDE
C        TAU2(1)= DXDK
C        TAU2(2)= DYDK
C        TAU2(3)= DZDK
C_______________________________________________________________________

      ELSE IF (ALIAS.EQ.'QU8') THEN

        TN(1) = (1.D0+YI)* (1.D0-XI)* (-1.D0-XI+YI)*0.25D0
        TN(2) = (1.D0-YI)* (1.D0-XI)* (-1.D0-XI-YI)*0.25D0
        TN(3) = (1.D0-YI)* (1.D0+XI)* (-1.D0+XI-YI)*0.25D0
        TN(4) = (1.D0+YI)* (1.D0+XI)* (-1.D0+XI+YI)*0.25D0
        TN(5) = (1.D0-YI)* (1.D0-XI)* (1.D0+YI)*0.5D0
        TN(6) = (1.D0-YI)* (1.D0-XI)* (1.D0+XI)*0.5D0
        TN(7) = (1.D0-YI)* (1.D0+XI)* (1.D0+YI)*0.5D0
        TN(8) = (1.D0+YI)* (1.D0-XI)* (1.D0+XI)*0.5D0
C    OK VERIFIEE
C     LES DD 1ER / XI
        AJ(1,1) = 0.25D0* (1.D0+YI)* (2.D0*XI-YI)
        AJ(1,2) = 0.25D0* (1.D0-YI)* (2.D0*XI+YI)
        AJ(1,3) = 0.25D0* (1.D0-YI)* (2.D0*XI-YI)
        AJ(1,4) = 0.25D0* (1.D0+YI)* (2.D0*XI+YI)
        AJ(1,5) = -0.5D0* (1.D0-YI)* (1.D0+YI)
        AJ(1,6) = - (1.D0-YI)*XI
        AJ(1,7) = 0.5D0* (1.D0-YI)* (1.D0+YI)
        AJ(1,8) = - (1.D0+YI)* (XI)
C     LES DD 1ER / YI
        AJ(2,1) = 0.25D0* (1.D0-XI)* (2.D0*YI-XI)
        AJ(2,2) = 0.25D0* (1.D0-XI)* (2.D0*YI+XI)
        AJ(2,3) = 0.25D0* (1.D0+XI)* (2.D0*YI-XI)
        AJ(2,4) = 0.25D0* (1.D0+XI)* (2.D0*YI+XI)
        AJ(2,5) = - (1.D0-XI)*YI
        AJ(2,6) = -0.5D0* (1.D0-XI)* (1.D0+XI)
        AJ(2,7) = - (1.D0+XI)*YI
        AJ(2,8) = (1.D0+XI)* (1.D0-XI)*0.5D0
        DXDE=0.D00
        DXDK=0.D00
        DYDE=0.D00
        DYDK=0.D00
        DZDE=0.D00
        DZDK=0.D00

        DO 60 I = 1,8
          DXDE = DXDE + ZR(IGEOM+3*I-3)*AJ(1,I)
          DXDK = DXDK + ZR(IGEOM+3*I-3)*AJ(2,I)
          DYDE = DYDE + ZR(IGEOM+3*I-2)*AJ(1,I)
          DYDK = DYDK + ZR(IGEOM+3*I-2)*AJ(2,I)
          DZDE = DZDE + ZR(IGEOM+3*I-1)*AJ(1,I)
          DZDK = DZDK + ZR(IGEOM+3*I-1)*AJ(2,I)
   60   CONTINUE

        JAC = SQRT((DYDE*DZDK-DZDE*DYDK)**2+ (DZDE*DXDK-DXDE*DZDK)**2+
     &        (DXDE*DYDK-DYDE*DXDK)**2)


      ELSE IF (ALIAS.EQ.'QU9') THEN


        TN(1) = AL31(XI)*AL33(YI)
        TN(2) = AL31(XI)*AL31(YI)
        TN(3) = AL33(XI)*AL31(YI)
        TN(4) = AL33(XI)*AL33(YI)
        TN(5) = AL31(XI)*AL32(YI)
        TN(6) = AL32(XI)*AL31(YI)
        TN(7) = AL33(XI)*AL32(YI)
        TN(8) = AL32(XI)*AL33(YI)
        TN(9) = AL32(XI)*AL32(YI)

        AJ(1,1) = DAL31(XI)*AL33(YI)
        AJ(2,1) = AL31(XI)*DAL33(YI)
        AJ(1,2) = DAL31(XI)*AL31(YI)
        AJ(2,2) = AL31(XI)*DAL31(YI)
        AJ(1,3) = DAL33(XI)*AL31(YI)
        AJ(2,3) = AL33(XI)*DAL31(YI)
        AJ(1,4) = DAL33(XI)*AL33(YI)
        AJ(2,4) = AL33(XI)*DAL33(YI)
        AJ(1,5) = DAL31(XI)*AL32(YI)
        AJ(2,5) = AL31(XI)*DAL32(YI)
        AJ(1,6) = DAL32(XI)*AL31(YI)
        AJ(2,6) = AL32(XI)*DAL31(YI)
        AJ(1,7) = DAL33(XI)*AL32(YI)
        AJ(2,7) = AL33(XI)*DAL32(YI)
        AJ(1,8) = DAL32(XI)*AL33(YI)
        AJ(2,8) = AL32(XI)*DAL33(YI)
        AJ(1,9) = DAL32(XI)*AL32(YI)
        AJ(2,9) = AL32(XI)*DAL32(YI)
        DXDE=0.D00
        DXDK=0.D00
        DYDE=0.D00
        DYDK=0.D00
        DZDE=0.D00
        DZDK=0.D00

        DO 70 I = 1,9
          DXDE = DXDE + ZR(IGEOM+3*I-3)*AJ(1,I)
          DXDK = DXDK + ZR(IGEOM+3*I-3)*AJ(2,I)
          DYDE = DYDE + ZR(IGEOM+3*I-2)*AJ(1,I)
          DYDK = DYDK + ZR(IGEOM+3*I-2)*AJ(2,I)
          DZDE = DZDE + ZR(IGEOM+3*I-1)*AJ(1,I)
          DZDK = DZDK + ZR(IGEOM+3*I-1)*AJ(2,I)
   70   CONTINUE

        JAC = SQRT((DYDE*DZDK-DZDE*DYDK)**2+ (DZDE*DXDK-DXDE*DZDK)**2+
     &        (DXDE*DYDK-DYDE*DXDK)**2)

      ELSE
        CALL JXABOR()
      END IF

      END
