       SUBROUTINE HUJOPT (MOD,ANGMAS,IMAT,MATER,NVI,VINF,NR,DRDY,
     &                    SIGF,DSDE,IRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/04/2013   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE FOUCAULT A.FOUCAULT
C TOLE CRS_1404
C     ----------------------------------------------------------------
C     CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY(DY) 
C     POUR LE MODELE HUJEUX
C     IN  MOD    :  TYPE DE MODELISATIONS
C         ANGMAS :  ANGLE NAUTIQUE (AFFE_CARA_ELEM)
C         NVI    :  NOMBRE DE VARIABLES INTERNES
C         MATER  :  COEFFICIENTS MATERIAU
C         VINF   :  VARIABLES INTERNES A T+DT
C         NR     :  DIMENSION MATRICE JACOBIENNE
C         DRDY   :  MATRICE JACOBIENNE
C     OUT DSDE   :  MATRICE TANGENTE EN VITESSE
C     ----------------------------------------------------------------
      IMPLICIT NONE
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT  , NDI
C     ----------------------------------------------------------------
      INTEGER      NMAT,NR,NVI,IRET,IMAT
      REAL*8       DRDY(NR,NR),DSDE(6,6),MATER(22,2),VINF(NVI),SIGF(6)
      REAL*8       ANGMAS(3)
      CHARACTER*8  MOD
C
      INTEGER NBMECA, NORM,I,J,NDT,NDI,NZ
      REAL*8  HOOK(6,6),UN,ZERO,DEUX,TROIS
      REAL*8  E,NU,AL,DEMU,LA,E1,E2,E3,NU12,NU13,NU23,G1,G2,G3
      REAL*8  NU21,NU31,NU32,DENOM,I1F,HOOKNL(6,6),PREF,NE
      REAL*8  COEF0,PISO,TRACE
      REAL*8  Y0(NDT,NDT),Y1(NDT,9),Y2(9,NDT),Y3(9,9)
      REAL*8  Y4(NDT,NDT),Y5(NDT,NDT),DET,R8PREM,MAXI,MINI
      REAL*8  DSDEB(NDT,NDT),R8VIDE,BID16(6),BID66(6,6)
      REAL*8  CCOND
      CHARACTER*4 CARGAU
      LOGICAL REORIE
C
      PARAMETER (ZERO  = 0.D0)
      PARAMETER (UN    = 1.D0)
      PARAMETER (DEUX  = 2.D0)
      PARAMETER (TROIS = 3.D0)
C === =================================================================
C --- RECHERCHE DU MAXIMUM DE DRDY
C === =================================================================

      NORM=0
      IF (NORM.EQ.0) GOTO 5
      
      MAXI = 0.D0
      DO 1 I = 1, NR
        DO 2 J = 1, NR
          IF(ABS(DRDY(I,J)).GT.MAXI)MAXI = ABS(DRDY(I,J))
 2    CONTINUE
 1    CONTINUE
 
C === =================================================================
C --- DIMENSIONNEMENT A R8PREM
C === =================================================================
      MINI = R8PREM()*MAXI
      DO 3 I = 1, NR
        DO 4 J = 1, NR
          IF(ABS(DRDY(I,J)).LT.MINI)DRDY(I,J) = 0.D0
 4    CONTINUE
 3    CONTINUE

 5    CONTINUE

C === =================================================================
C --- SEPARATION DES TERMES DU JACOBIEN
C === =================================================================
C --- DETERMINATION DU NOMBRE DE MECANISMES ACTIFS - NBMECA
      NBMECA = 0
      DO 10 I = 1, 8
        IF (VINF(23+I) .EQ. UN) NBMECA = NBMECA + 1
 10     CONTINUE

      NZ = 1+2*NBMECA

C === ==============================================================
C --- REDIMENSIONNEMENT DU JACOBIEN
C === ==============================================================
      CCOND = MATER(1,1)
      PREF  = MATER(8,2)

C --- DLEDR
      DO 20 I = 1, NBMECA
        DO 30 J = 1, NDT
          DRDY(J,NDT+1+I) = DRDY(J,NDT+1+I)*PREF 
 30     CONTINUE        
 20   CONTINUE     

C --- DLEDLA
      DO 40 I = 1, NBMECA
        DO 50 J = 1, NDT
          DRDY(J,NDT+1+NBMECA+I) = DRDY(J,NDT+1+NBMECA+I)*CCOND 
 50     CONTINUE        
 40   CONTINUE     
      
C --- DLRDLA
      DO 60 I = 1, NBMECA
        DRDY(NDT+1+I,NDT+1+NBMECA+I) = DRDY(NDT+1+I,NDT+1+NBMECA+I)
     &                                 *CCOND/ABS(PREF) 
 60   CONTINUE     

C --- DLRDEVP
      DO 70 I = 1, NBMECA
        DRDY(NDT+1+I,NDT+1) = DRDY(NDT+1+I,NDT+1)*CCOND/ABS(PREF) 
 70   CONTINUE     

C --- DLEVPDS
      DO 80 I = 1, NDT
        DRDY(NDT+1,I) = DRDY(NDT+1,I)*CCOND
 80   CONTINUE           

C --- DLEVPDR
      DO 90 I = 1, NBMECA
        DRDY(NDT+1,NDT+1+I) = DRDY(NDT+1,NDT+1+I)/CCOND*ABS(PREF)
 90   CONTINUE           

C --- DLFDR
      DO 100 I = 1, NBMECA
        DRDY(NDT+1+NBMECA+I,NDT+1+I) = DRDY(NDT+1+NBMECA+I,NDT+1+I)
     &                                 *ABS(PREF)
 100  CONTINUE     

C --- DLFDEVP
      DO 110 I = 1, NBMECA
        DRDY(NDT+1+NBMECA+I,NDT+1) = DRDY(NDT+1+NBMECA+I,NDT+1)
     &                               *CCOND 
 110  CONTINUE     

C ----------------------------------------------
C --- CONSTRUCTION DE L'OPERATEUR CONSISTANT ---
C ----------------------------------------------
      CALL LCINMA(ZERO,Y0)
      DO 310 I = 1, 9
        DO 320 J = 1, NDT
          Y1(J,I) = ZERO 
          Y2(I,J) = ZERO
 320    CONTINUE
 310  CONTINUE      
      
      DO 330 I = 1, 9
        Y3(I,J) = ZERO
 330  CONTINUE    

      DO 120 I = 1, NDT
        DO 130 J = 1, NDT
          Y0(I,J) = DRDY(I,J)
 130    CONTINUE
 120  CONTINUE  
      DO 140 I = 1, NDT
        DO 150 J = 1, NZ
          Y1(I,J) = DRDY(I,J+NDT)
 150     CONTINUE
 140   CONTINUE  
      DO 160 I = 1, NZ
        DO 170 J = 1, NDT
          Y2(I,J) = DRDY(I+NDT,J)
 170     CONTINUE
 160   CONTINUE  
      DO 180 I = 1, NZ
        DO 190 J = 1, NZ
          Y3(I,J) = DRDY(I+NDT,J+NDT)
 190     CONTINUE
 180   CONTINUE  

C === =================================================================
C --- CONSTRUCTION TENSEUR RIGIDITE ELASTIQUE A T+DT
C === =================================================================
C ====================================================================
C --- OPERATEURS ELASTICITE LINEAIRES---------------------------------
C ====================================================================
      CALL LCINMA (ZERO, HOOK)

      IF ((MOD(1:2) .EQ. '3D')     .OR.
     &    (MOD(1:6) .EQ. 'D_PLAN') )  THEN

        IF (MATER(17,1).EQ.UN) THEN

          E    = MATER(1,1)
          NU   = MATER(2,1)
          AL   = E*(UN-NU) /(UN+NU) /(UN-DEUX*NU)
          DEMU = E     /(UN+NU)
          LA   = E*NU/(UN+NU)/(UN-DEUX*NU)

          DO 200 I = 1, NDI
            DO 200 J = 1, NDI
              IF (I.EQ.J) HOOK(I,J) = AL
              IF (I.NE.J) HOOK(I,J) = LA
 200      CONTINUE
          DO 210 I = NDI+1, NDT
            HOOK(I,I) = DEMU
 210      CONTINUE

        ELSEIF (MATER(17,1).EQ.DEUX) THEN

          E1   = MATER(1,1)
          E2   = MATER(2,1)
          E3   = MATER(3,1)
          NU12 = MATER(4,1)
          NU13 = MATER(5,1)
          NU23 = MATER(6,1)
          G1   = MATER(7,1)
          G2   = MATER(8,1)
          G3   = MATER(9,1)
          NU21 = MATER(13,1)
          NU31 = MATER(14,1)
          NU32 = MATER(15,1)
          DENOM= MATER(16,1)

          HOOK(1,1) = (UN - NU23*NU32)*E1/DENOM
          HOOK(1,2) = (NU21 + NU31*NU23)*E1/DENOM
          HOOK(1,3) = (NU31 + NU21*NU32)*E1/DENOM
          HOOK(2,2) = (UN - NU13*NU31)*E2/DENOM
          HOOK(2,3) = (NU32 + NU31*NU12)*E2/DENOM
          HOOK(3,3) = (UN - NU21*NU12)*E3/DENOM
          HOOK(2,1) = HOOK(1,2)
          HOOK(3,1) = HOOK(1,3)
          HOOK(3,2) = HOOK(2,3)
          HOOK(4,4) = G1
          HOOK(5,5) = G2
          HOOK(6,6) = G3

        ELSE
          CALL U2MESS('F', 'COMPOR1_38')
        ENDIF
      ELSEIF (MOD(1:6) .EQ. 'C_PLAN' .OR.
     &        MOD(1:2) .EQ. '1D')   THEN
        CALL U2MESS('F', 'COMPOR1_4')
      ENDIF
C ====================================================================
C --- OPERATEUR ELASTICITE NON LINEAIRE ------------------------------
C ====================================================================
      I1F  = TRACE(NDI,SIGF)/TROIS
      NE   = MATER(1,2)
      IF ( (I1F/PREF) .LT. 1.D-6 ) I1F = 1.D-6*PREF

      COEF0 = (I1F/PREF) ** NE
      DO 220 I = 1, NDT
        DO 220 J = 1, NDT
          HOOKNL(I,J) = COEF0*HOOK(I,J)
 220  CONTINUE

C     CHOIX DES PARAMETRES DE LANCEMENT DE MGAUSS
C     METHODE 'S' : SURE
      CARGAU = 'NCSP'
C     METHODE 'W' : RATEAU
C      CARGAU = 'NCWP'      
C === =================================================================
C --- CONSTRUCTION TENSEUR CONSTITUTIF TANGENT DSDE 
C === =================================================================
C     Y2=INVERSE(Y3)*Y2
      CALL MGAUSS (CARGAU,Y3, Y2, 9, NZ, NDT, DET, IRET )
      IF(IRET.GT.1) THEN
         CALL LCEQMA(HOOK,DSDE)
         GOTO 9999
      ENDIF
C --- PRODUIT DU TERME (Y3)^-1 * Y2 = Y4
      CALL PROMAT(Y1,NDT,NDT,9,Y2,9,9,NDT,Y4)

C --- DIFFERENCE DE MATRICE (DR1DY1 - Y4) = Y5
      DO 230 I=1,NDT
         DO 240 J=1,NDT
            Y5(I,J)=Y0(I,J)-Y4(I,J)
 240     CONTINUE
 230  CONTINUE

C --- INVERSION DU TERME Y5
      CALL R8INIR(NDT*NDT,0.D0,DSDEB,1)
      DO 250 I=1,NDT
        DSDEB(I,I) = 1.D0
 250  CONTINUE
      CALL MGAUSS(CARGAU,Y5, DSDEB, NDT, NDT, NDT, DET, IRET)
      
      IF(IRET.GT.1) THEN
         CALL LCEQMA(HOOK,DSDE)
      ELSE
         CALL LCINMA(ZERO,DSDE)
         CALL LCPRMM(DSDEB,HOOKNL,DSDE)         
      ENDIF

 9999 CONTINUE
      IF (ANGMAS(1).EQ.R8VIDE()) CALL U2MESS('F','ALGORITH8_20')
      REORIE =(ANGMAS(1).NE.ZERO) .OR. (ANGMAS(2).NE.ZERO)
     &         .OR. (ANGMAS(3).NE.ZERO)
      IF(IRET.NE.0)THEN
        IRET = 0
        CALL HUJORI('LOCAL',1, REORIE, ANGMAS, SIGF, BID66)
        CALL HUJTID(MOD, IMAT, SIGF, VINF, DSDE, IRET)
        CALL HUJORI('GLOBA',1, REORIE, ANGMAS, SIGF, BID66)
        IF(IRET.NE.0)THEN
          IRET = 0
          CALL HUJTEL (MOD, MATER, SIGF, DSDE)
        ENDIF
      ENDIF
      CALL HUJORI ('GLOBA', 2, REORIE, ANGMAS, BID16, DSDE)

      END
