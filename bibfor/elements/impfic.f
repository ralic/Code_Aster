      SUBROUTINE IMPFIC ( VALE, CHAMEL, NOMNOE, RCMP, UNIT )
      IMPLICIT  NONE
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 16/06/2004   AUTEUR DURAND C.DURAND 
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
C
C     - FONCTION REALISEE:  IMPRESSION DES FACTEURS D'INTENSITE
C                           DE CONTRAINTES K1 ET K2, DU TAUX DE
C                           RESTITUTION D'ENERGIE G (VALEUR CLASSIQUE G
C                           ET PAR LA FORMULE D'IRWIN )
C
C  ENTREE
C
C    VALE       --> ADRESSE DES VALEURS DES FIC
C    CHAMEL     --> NOM DU CONCEPT RESULTAT
C    NOMNOE     --> ADRESSE DU NOM DU NOEUD DE FOND DE FISSURE
C    RCMP       --> COORDONNEES DU NOEUD DE FOND DE FISSURE
C                     ET DE LA NORMALE A LA FISSURE
C
C ......................................................................
C
      INTEGER       I,K1PHI,K2PHI,GPHI, UNIT
      REAL*8        G,FIC1,FIC2,K1,K2,GIRWIN,RCMP(4),VALE(*)
      REAL*8        K11(10),K21(10),K12(10),K22(10),K1MAX,K1MIN
      REAL*8        K2MIN,K2SUP,GMAX,K1DEV,K2DEV,GDEV,FIC1D,FIC2D
      CHARACTER*8   NOMNOE
      CHARACTER*24  CHAMEL, OBVALE
C ......................................................................
C
      G      = VALE(1)
      FIC1   = VALE(2)
      FIC2   = VALE(3)
      K1     = VALE(4)
      K2     = VALE(5)
      GIRWIN = FIC1*FIC1 + FIC2*FIC2
C
C CALCUL DE L'ANGLE DE PROPAGATION DE LA FISSURE SELON 3 CRITERES:
C  K1MAX, K2MIN, GMAX (FORMULES ISSUES DE AMNESTOY-BUI)
C
      K11(1) = 1.D0
      K11(2) = 0.9886D0
      K11(3) = 0.9552D0
      K11(4) = 0.9018D0
      K11(5) = 0.8314D0
      K11(6) = 0.7479D0
      K11(7) = 0.6559D0
      K11(8) = 0.5598D0
      K11(9) = 0.4640D0
      K11(10)= 0.3722D0
C
      K21(1) = 0
      K21(2) = 0.0864D0
      K21(3) = 0.1680D0
      K21(4) = 0.2403D0
      K21(5) = 0.2995D0
      K21(6) = 0.3431D0
      K21(7) = 0.3696D0
      K21(8) = 0.3788D0
      K21(9) = 0.3718D0
      K21(10)= 0.3507D0
C
      K12(1) = 0.D0
      K12(2) = -0.2597D0
      K12(3) = -0.5068D0
      K12(4) = -0.7298D0
      K12(5) = -0.9189D0
      K12(6) = -1.0665D0
      K12(7) = -1.1681D0
      K12(8) = -1.2220D0
      K12(9) = -1.2293D0
      K12(10)= -1.1936D0
C
      K22(1) = 1.D0
      K22(2) = 0.9764D0
      K22(3) = 0.9071D0
      K22(4) = 0.7972D0
      K22(5) = 0.6540D0
      K22(6) = 0.4872D0
      K22(7) = 0.3077D0
      K22(8) = 0.1266D0
      K22(9) = -0.0453D0
      K22(10)= -0.1988D0
C
      K1MAX = 0.D0
      K1MIN = 0.D0
      K2MIN = -ABS(K2)
      K2SUP = +ABS(K2)
      GMAX  = 0.D0
      K1PHI= 0
      K2PHI= 0
      GPHI= 0
C
      DO 100 I=1,10
        K1DEV= K11(I)*K1+K12(I)*K2
        K2DEV= K21(I)*K1+K22(I)*K2
        FIC1D= K11(I)*FIC1+K12(I)*FIC2
        FIC2D= K21(I)*FIC1+K22(I)*FIC2
        GDEV = FIC1D*FIC1D+FIC2D*FIC2D
        IF ((K1DEV.GT.K1MAX).OR.(K1DEV.LT.K1MIN)) THEN
          K1PHI =10*(I-1)
          K1MIN = -ABS(K1DEV)
          K1MAX = ABS(K1DEV)
        ENDIF
        IF ((K2DEV.LE.K2SUP).AND.(K2DEV.GE.K2MIN)) THEN
          K2PHI=10*(I-1)
          K2MIN = -ABS(K2DEV)
          K2SUP = ABS(K2DEV)
        ENDIF
        IF (GDEV.GT.GMAX) THEN
          GPHI =10*(I-1)
          GMAX  = GDEV
        ENDIF
100   CONTINUE
      DO 200 I=2,10
        K1DEV= K11(I)*K1-K12(I)*K2
        K2DEV= -K21(I)*K1+K22(I)*K2
        FIC1D= K11(I)*FIC1-K12(I)*FIC2
        FIC2D= -K21(I)*FIC1+K22(I)*FIC2
        GDEV = FIC1D*FIC1D+FIC2D*FIC2D
        IF ((K1DEV.GT.K1MAX).OR.(K1DEV.LT.K1MIN)) THEN
          K1PHI =-10*(I-1)
          K1MIN = -ABS(K1DEV)
          K1MAX = ABS(K1DEV)
        ENDIF
        IF ((K2DEV.LE.K2SUP).AND.(K2DEV.GE.K2MIN)) THEN
          K2PHI=-10*(I-1)
          K2MIN = -ABS(K2DEV)
          K2SUP = ABS(K2DEV)
        ENDIF
        IF (GDEV.GT.GMAX) THEN
          GPHI=-10*(I-1)
          GMAX  = GDEV
        ENDIF
200   CONTINUE
C
      WRITE(UNIT,*)
      WRITE(UNIT,555)
      WRITE(UNIT,*)
C
      WRITE(UNIT,*) 'NOEUD DE FOND DE FISSURE : ',NOMNOE
      WRITE(UNIT,*)
C
      WRITE(UNIT,*) 'COORDONNEES DU NOEUD DE FOND DE FISSURE : ',
     &              RCMP(1),' ',RCMP(2)
      WRITE(UNIT,*)
      WRITE(UNIT,*) 'COORDONNEES DE LA NORMALE A LA FISSURE :  ',
     &              RCMP(3),' ',RCMP(4)
      WRITE(UNIT,*)
C
      WRITE(UNIT,*) '       K1                K2         '//
     &               '     G (IRWIN)'
      WRITE(UNIT,*)
      WRITE(UNIT,999) K1,K2,GIRWIN
      WRITE(UNIT,*)
      WRITE(UNIT,777) 'TAUX DE RESTITUTION D''ENERGIE G : ', G
      WRITE(UNIT,*)
      WRITE(UNIT,*) 'DIRECTION DE LA DEVIATION DE LA FISSURE '
     &             //'(EN DEGRES): '
      WRITE(UNIT,*)
      WRITE(UNIT,666) 'SELON LE CRITERE K1 MAXIMUM : ', K1PHI,
     &              ' AVEC K1MAX = ',K1MAX
      WRITE(UNIT,666) 'SELON LE CRITERE K2 NUL     : ', K2PHI,
     &              ' AVEC K2NUL = ',K2SUP
      WRITE(UNIT,666) 'SELON LE CRITERE G MAXIMUM  : ', GPHI,
     &              ' AVEC GMAX  = ',GMAX
      WRITE(UNIT,555)
      WRITE(UNIT,*)
C
      OBVALE = CHAMEL(1:8)//'           .VALE'
      CALL JEIMPO(UNIT,OBVALE,' ',
     &        'OBJET CONTENANT LA VALEUR DES FIC SUR CHAQUE ELEMENT')
C
555   FORMAT(60('*'))
666   FORMAT(A,I3,A,1PD12.5)
777   FORMAT(A,1PD12.5)
999   FORMAT(3(1PD12.5,8X))
C
      END
