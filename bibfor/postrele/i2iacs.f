      SUBROUTINE I2IACS (EPSI,XC,YC,R,ALFINF,ALFSUP,X1,Y1,X2,Y2,
     +                   NPI,A1,A2,R1,R2,ELI)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 05/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C**********************************************************************
C
C          CALCUL DE L' INTERSECTION DU CERCLE DE CENTRE
C          (XC,YC), DE RAYON R ET DE SECTEUR ANGULAIRE
C          (ALFINF,ALFSUP) AVEC LE SEGMENT DEFINI PAR
C          LES POINTS (X1,Y1) ET (X2,Y2)
C
C*********************************************************************
C
      INTEGER NPI
      REAL*8  EPSI,XC,YC,X1,Y1,X2,Y2,R,ALFINF,ALFSUP,A1,A2,R1,R2
      LOGICAL ELI
C
      INTEGER NBRAC,ORD1,ORD2
      REAL*8  COEF2,COEF1,COEF0,RAC1,RAC2,DELTAX,DELTAY
      REAL*8  INVR,XS,YS,AUX
C
      LOGICAL LSR10,LS1R1,LSR20,LS1R2,LE0R11,LE0R21
      LOGICAL LSA1I,LSSA1,LSA2I,LSSA2,LEIA1S,LEIA2S
C
      LSA1I  = .FALSE.
      LSSA1  = .FALSE.
      LSA2I  = .FALSE.
      LSSA2  = .FALSE.
      LEIA1S = .FALSE.
      LEIA2S = .FALSE.
C
      LSR10  = .FALSE.
      LS1R1  = .FALSE.
      LSR20  = .FALSE.
      LS1R2  = .FALSE.
      LE0R11 = .FALSE.
      LE0R21 = .FALSE.
C
      NPI = 0
      A1  = 0.0D0
      A2  = 0.0D0
      R1  = 0.0D0
      R2  = 0.0D0
      ELI = .FALSE.
C
      XS   = 0.0D0
      YS   = 0.0D0
      INVR = 1.0D0/R
      AUX  = 0.0D0
C
      DELTAX = X2-X1
      DELTAY = Y2-Y1
      COEF2  = DELTAX*DELTAX + DELTAY*DELTAY
      COEF1  = 2*(DELTAX*(X1-XC) + DELTAY*(Y1-YC))
      COEF0  = (X1-XC)*(X1-XC) + (Y1-YC)*(Y1-YC) - R*R
C
      RAC1   = 0.0D0
      RAC2   = 0.0D0
      NBRAC  = 0
      ORD1   = 0
      ORD2   = 0
C
C----------CALCUL DES PARAMETRES DE REPERAGE SUR LE SEGMENT---------
C
      CALL I2REQ2(EPSI,COEF2,COEF1,COEF0,NBRAC,RAC1,RAC2,ORD1,ORD2)
C
C-------------REAJUSTEMENT DES RACINES--------------------------------
C
      IF ( ABS(RAC1) .LT. EPSI ) THEN
C
         RAC1 = 0.0D0
C
      ENDIF
C
      IF ( ABS(RAC2) .LT. EPSI ) THEN
C
         RAC2 = 0.0D0
C
      ENDIF
C
      IF ( ABS(RAC1-1.0D0) .LT. EPSI ) THEN
C
         RAC1 = 1.0D0
C
      ENDIF
C
      IF ( ABS(RAC2-1.0D0) .LT. EPSI ) THEN
C
         RAC2 = 1.0D0
C
      ENDIF
C
C---------------DEPOUILLEMENT DES RESULTATS------------------------
C
      IF ( NBRAC .EQ. 1 ) THEN
C
         IF ( (RAC1 .LE. 1.0D0) .AND. (RAC1 .GE. 0.0D0) ) THEN
C
            XS = X1 + RAC1*DELTAX
            YS = Y1 + RAC1*DELTAY
C
            AUX = INVR*(XS-XC)
C
            IF ( ABS(AUX - 1.0D0) .LT. EPSI ) THEN
C
               AUX = 1.0D0
C
            ENDIF
C
            IF ( ABS(AUX + 1.0D0) .LT. EPSI ) THEN
C
               AUX = -1.0D0
C
            ENDIF
C
            A1 = ACOS (AUX)
C
            IF ( ABS(YS-YC) .LT. EPSI ) THEN
C
               YS = YC
C
            ENDIF
C
            IF ((YS-YC) .LT. 0.0D0) THEN
C
               A1 = -A1
C
            ENDIF
C
            IF ( ABS(A1-ALFINF) .LT. EPSI ) THEN
C
               A1 = ALFINF
C
            ENDIF
C
            IF ( ABS(A1-ALFSUP) .LT. EPSI ) THEN
C
               A1 = ALFSUP
C
            ENDIF
C
            IF ( (ALFINF .LE. A1) .AND. (A1 .LE. ALFSUP))THEN
C
               NPI = 1
               R1  = RAC1
C
               IF ( (ORD1 .GE. 2 )                  .AND.
     +              ( (ABS(RAC1)         .GT. EPSI) .AND.
     +                (ABS(RAC1- 1.0D0)    .GT. EPSI) .AND.
     +                (ABS(A1  - ALFINF) .GT. EPSI) .AND.
     +                (ABS(A1  - ALFSUP) .GT. EPSI)
     +              )
     +            ) THEN
C
                  ELI = .TRUE.
C
               ENDIF
C
            ENDIF
C
         ENDIF
C
      ENDIF
C
      IF ( NBRAC .EQ. 2 ) THEN
C
         LSR10  = (RAC1 .LT. 0.0D0)
         LS1R1  = (RAC1 .GT. 1.0D0)
         LE0R11 = ( (.NOT. LSR10) .AND. (.NOT. LS1R1) )
         LSR20  = (RAC2 .LT. 0.0D0)
         LS1R2  = (RAC2 .GT. 1.0D0)
         LE0R21 = ( (.NOT. LSR20) .AND. (.NOT. LS1R2) )
C
         IF ( (LSR10 .OR. LS1R1) .AND. (LE0R21) ) THEN
C
            XS = X1 + RAC2*DELTAX
            YS = Y1 + RAC2*DELTAY
C
            AUX = INVR*(XS-XC)
C
            IF ( ABS(AUX - 1.0D0) .LT. EPSI ) THEN
C
               AUX = 1.0D0
C
            ENDIF
C
            IF ( ABS(AUX + 1.0D0) .LT. EPSI ) THEN
C
               AUX = -1.0D0
C
            ENDIF
C
            A1 = ACOS (AUX)
C
            IF ( ABS(YS-YC) .LT. EPSI ) THEN
C
               YS = YC
C
            ENDIF
C
            IF ((YS-YC) .LT. 0.0D0) THEN
C
               A1 = -A1
C
            ENDIF
C
            IF ( ABS(A1-ALFINF) .LT. EPSI ) THEN
C
                 A1 = ALFINF
C
            ENDIF
C
            IF ( ABS(A1-ALFSUP) .LT. EPSI ) THEN
C
                 A1 = ALFSUP
C
            ENDIF
C
            IF ( (A1 .LE. ALFSUP) .AND. (A1 .GE. ALFINF) ) THEN
C
               NPI = 1
               R1  = RAC2
C
            ENDIF
C
         ENDIF
C
         IF ( (LE0R11) .AND. (LSR20 .OR. LS1R2) ) THEN
C
            XS = X1 + RAC1*DELTAX
            YS = Y1 + RAC1*DELTAY
C
            AUX = INVR*(XS-XC)
C
            IF ( ABS(AUX - 1.0D0) .LT. EPSI ) THEN
C
               AUX = 1.0D0
C
            ENDIF
C
            IF ( ABS(AUX + 1.0D0) .LT. EPSI ) THEN
C
               AUX = -1.0D0
C
            ENDIF
C
            A1 = ACOS (AUX)
C
            IF ( ABS(YS-YC) .LT. EPSI ) THEN
C
               YS = YC
C
            ENDIF
C
            IF ((YS-YC) .LT. 0.0D0) THEN
C
               A1 = -A1
C
            ENDIF
C
            IF ( ABS(A1-ALFINF) .LT. EPSI ) THEN
C
                 A1 = ALFINF
C
            ENDIF
C
            IF ( ABS(A1-ALFSUP) .LT. EPSI ) THEN
C
                 A1 = ALFSUP
C
            ENDIF
C
            IF ( (A1 .LE. ALFSUP) .AND. (A1 .GE. ALFINF) ) THEN
C
               NPI = 1
               R1  = RAC1
C
            ENDIF
C
         ENDIF
C
         IF (LE0R11 .AND. LE0R21) THEN
C
            XS = X1 + RAC1*DELTAX
            YS = Y1 + RAC1*DELTAY
C
            AUX = INVR*(XS-XC)
C
            IF ( ABS(AUX - 1.0D0) .LT. EPSI ) THEN
C
               AUX = 1.0D0
C
            ENDIF
C
            IF ( ABS(AUX + 1.0D0) .LT. EPSI ) THEN
C
               AUX = -1.0D0
C
            ENDIF
C
            A1 = ACOS (AUX)
C
            IF ( ABS(YS-YC) .LT. EPSI ) THEN
C
               YS = YC
C
            ENDIF
C
            IF ((YS-YC) .LT. 0.0D0) THEN
C
               A1 = -A1
C
            ENDIF
C
            XS = X1 + RAC2*DELTAX
            YS = Y1 + RAC2*DELTAY
C
            AUX = INVR*(XS-XC)
C
            IF ( ABS(AUX - 1.0D0) .LT. EPSI ) THEN
C
               AUX = 1.0D0
C
            ENDIF
C
            IF ( ABS(AUX + 1.0D0) .LT. EPSI ) THEN
C
               AUX = -1.0D0
C
            ENDIF
C
            A2 = ACOS (AUX)
C
            IF ( ABS(YS-YC) .LT. EPSI ) THEN
C
               YS = YC
C
            ENDIF
C
            IF ((YS-YC) .LT. 0.0D0) THEN
C
               A2 = -A2
C
            ENDIF
C
            IF ( ABS(A1-ALFINF) .LT. EPSI ) THEN
C
                 A1 = ALFINF
C
            ENDIF
C
            IF ( ABS(A1-ALFSUP) .LT. EPSI ) THEN
C
                 A1 = ALFSUP
C
            ENDIF
C
            IF ( ABS(A2-ALFINF) .LT. EPSI ) THEN
C
                 A2 = ALFINF
C
            ENDIF
C
            IF ( ABS(A2-ALFSUP) .LT. EPSI ) THEN
C
                 A2 = ALFSUP
C
            ENDIF
C
            LSA1I  = (A1 .LT. ALFINF)
            LSSA1  = (A1 .GT. ALFSUP)
            LEIA1S = ( (.NOT. LSA1I) .AND. (.NOT. LSSA1) )
            LSA2I  = (A2 .LT. ALFINF)
            LSSA2  = (A2 .GT. ALFSUP)
            LEIA2S = ( (.NOT. LSA2I) .AND. (.NOT. LSSA2) )
C
            IF ( (LSA1I .OR. LSSA1) .AND. (LEIA2S) ) THEN
C
               NPI = 1
               A1  = A2
               R1  = RAC2
C
            ENDIF
C
            IF ( (LSA2I .OR. LSSA2) .AND. (LEIA1S) ) THEN
C
               NPI = 1
               R1  = RAC1
C
            ENDIF
C
            IF ( LEIA1S .AND. LEIA2S ) THEN
C
               NPI  = 2
               R1   = RAC1
               R2   = RAC2
C
               IF ( A1 .GT. A2 ) THEN
C
                  AUX = A1
                  A1  = A2
                  A2  = AUX
                  AUX = R1
                  R1  = R2
                  R2  = AUX
C
               ENDIF
C
            ENDIF
C
         ENDIF
C
      ENDIF
C
      END
