      SUBROUTINE SMCOMO ( COEF, FMOD, TEMPE, NBHIST, FTRC, TRC )
      IMPLICIT   NONE
      INTEGER             NBHIST
      REAL*8              COEF(*), FMOD(*), TEMPE
      REAL*8              FTRC((3*NBHIST),3),TRC((3*NBHIST),5)
C.......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 08/09/98   AUTEUR CIBHHLV L.VIVAN 
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
C     CONSTRUCTION DES POINTS EXPERIMENTAUX DE MEME TEMPERATURE        .
C
C   CONSTRUCTION DES TPOINT ET DES T POUR T ET POUR TOUTES LES HISTOIRES
C   AVEC COURBE PILOTE DE TRC -FONC(T(TPS))=LOG(TPS(T))- INTERPOLEE
C   POLYNOMIALE D ORDRE 5 ENTRE 199 ET 829  C ET DERIVEE DE FONC(T)
C
C.......................................................................
C
      INTEGER  I, J, K, LG, NLEXP
      REAL*8   TEMPS, COEFFZ, FP7, F7, FT, FPT, A, B, C, D, E, F, T
      REAL*8   UN, ZERO, CINQ
C     ------------------------------------------------------------------
C
      ZERO = 0.D0
      UN   = 1.D0
      CINQ = 5.D0
      T    = 700.D0
C
      DO 10 I = 1 , NBHIST
         A = COEF(3+9*(I-1))
         B = COEF(4+9*(I-1))
         C = COEF(5+9*(I-1))
         D = COEF(6+9*(I-1))
         E = COEF(7+9*(I-1))
         F = COEF(8+9*(I-1))
         IF ( (A.NE.ZERO) .AND. (B.NE.ZERO) .AND. (C.NE.ZERO) .AND.
     &        (D.NE.ZERO) .AND. (E.NE.ZERO) .AND. (F.NE.ZERO) ) THEN
            F7  = A + B*T + C*T**2 + D*T**3 + E*T**4 + F*T**5
            FT  = A+B*TEMPE+C*TEMPE**2+D*TEMPE**3+E*TEMPE**4+F*TEMPE**5
            FPT = B+2*C*TEMPE+3*D*TEMPE**2+4*E*TEMPE**3+5*F*TEMPE**4
            FP7 = B+2*C*T+3*D*T**2+4*E*T**3+5*F*T**4
            TEMPS = FT - F7 - LOG(FP7*COEF(1+9*(I-1)))
            TRC(I,4) = UN / ( FPT*EXP(TEMPS) )
         ELSE
            TRC(I,4) = COEF(1+9*(I-1))
         ENDIF
         TRC(I,5) = TEMPE
 10   CONTINUE
C
C CONSTRUCTION DES Z1(H) Z2(H) Z3(H) ET DZT1(H) DZT2(H) DZT3(H) ASSOCIES
C
      LG = 0
      DO 30 I = 1 , NBHIST
         NLEXP = NINT( COEF(9+9*(I-1)) )
         DO 20 J = 1 , NLEXP-1
            IF ( (TEMPE .LE. FMOD(4*(LG+J))) .AND. 
     &           (TEMPE .GE. (FMOD(4*(LG+J+1))-1.D-9)) )THEN
               COEFFZ = (TEMPE-FMOD(4*(LG+J))) /
     &                  (FMOD(4*(LG+J+1))-FMOD(4*(LG+J)))
               TRC(I,1) = FMOD(4*(LG+J)-3) + 
     &                   (FMOD(4*(LG+J+1)-3)-FMOD(4*(LG+J)-3))*COEFFZ
               TRC(I,2) = FMOD(4*(LG+J)-2) + 
     &                   (FMOD(4*(LG+J+1)-2)-FMOD(4*(LG+J)-2))*COEFFZ
               TRC(I,3) = FMOD(4*(LG+J)-1) + 
     &                   (FMOD(4*(LG+J+1)-1)-FMOD(4*(LG+J)-1))*COEFFZ
               FTRC(I,1) = (FMOD(4*(LG+J)-3)-FMOD(4*(LG+J+1)-3))  / 
     &                     (FMOD(4*(LG+J))-FMOD(4*(LG+J+1)))
               FTRC(I,2) = (FMOD(4*(LG+J)-2)-FMOD(4*(LG+J+1)-2))  / 
     &                     (FMOD(4*(LG+J))-FMOD(4*(LG+J+1)))
               FTRC(I,3) = (FMOD(4*(LG+J)-1)-FMOD(4*(LG+J+1)-1))  / 
     &                     (FMOD(4*(LG+J))-FMOD(4*(LG+J+1)))
            ELSE
               IF ( TEMPE .LT. FMOD(4*(NLEXP+LG)) ) THEN
                  TRC(I,1) = FMOD(4*(NLEXP+LG)-3)
                  TRC(I,2) = FMOD(4*(NLEXP+LG)-2)
                  TRC(I,3) = FMOD(4*(NLEXP+LG)-1)
                  FTRC(I,1) = ZERO
                  FTRC(I,2) = ZERO
                  FTRC(I,3) = ZERO
               ENDIF
            ENDIF
 20      CONTINUE
         LG = LG + NLEXP
 30   CONTINUE
C
C CONSTRUCTION DES TPOINT POUR T+5 C ET POUR TOUTES LES HISTOIRES
C
      TEMPE = TEMPE + CINQ
      DO 11 I = NBHIST+1 , (2*NBHIST)
         K = I - NBHIST
         NLEXP = NINT( COEF(9+9*(K-1)) )
         A = COEF(3+9*(K-1))
         B = COEF(4+9*(K-1))
         C = COEF(5+9*(K-1))
         D = COEF(6+9*(K-1))
         E = COEF(7+9*(K-1))
         F = COEF(8+9*(K-1))
         IF ( (A.NE.ZERO) .AND. (B.NE.ZERO) .AND. (C.NE.ZERO) .AND.
     &        (D.NE.ZERO) .AND. (E.NE.ZERO) .AND. (F.NE.ZERO) ) THEN
            F7 = A + B*T + C*T**2 + D*T**3 + E*T**4 + F*T**5
            FT = A+B*TEMPE+C*TEMPE**2+D*TEMPE**3+E*TEMPE**4+F*TEMPE**5
            FPT = B+2*C*TEMPE+3*D*TEMPE**2+4*E*TEMPE**3+5*F*TEMPE**4
            FP7 = B+2*C*T+3*D*T**2+4*E*T**3+5*F*T**4
            TEMPS = FT - F7 - LOG( FP7*COEF(1+9*(K-1)) )
            TRC(I,4) = UN / (FPT*EXP(TEMPS))
            TRC(I,5) = TEMPE
         ELSE
            TRC(I,4) = COEF(1+9*(K-1))
         ENDIF
         TRC(I,5) = TEMPE
 11   CONTINUE
      TEMPE = TEMPE - CINQ
C
C CONSTRUCTION DES Z1(H) Z2(H) Z3(H) ET DZT1(H) DZT2(H) DZT3(H) ASSOCIES
C
      LG = 0
      DO 31 I = NBHIST+1 , (2*NBHIST)
         K = I - NBHIST
         NLEXP = NINT( COEF(9+9*(K-1)) )
         DO 21 J = 1 , NLEXP - 1
            IF ( (TEMPE+CINQ .LE. FMOD(4*(LG+J))) .AND.
     &           (TEMPE+CINQ .GE. (FMOD(4*(LG+J+1))-1.D-9)) ) THEN
               COEFFZ = (TEMPE+CINQ-FMOD(4*(LG+J))) / 
     &                  (FMOD(4*(LG+J+1))-FMOD(4*(LG+J)))
               TRC(I,1) = FMOD(4*(LG+J)-3) + 
     &                   (FMOD(4*(LG+J+1)-3)-FMOD(4*(LG+J)-3))*COEFFZ
               TRC(I,2) = FMOD(4*(LG+J)-2) + 
     &                   (FMOD(4*(LG+J+1)-2)-FMOD(4*(LG+J)-2))*COEFFZ
               TRC(I,3) = FMOD(4*(LG+J)-1) + 
     &                   (FMOD(4*(LG+J+1)-1)-FMOD(4*(LG+J)-1))*COEFFZ
               FTRC(I,1) = (FMOD(4*(LG+J)-3)-FMOD(4*(LG+J+1)-3)) / 
     &                     (FMOD(4*(LG+J))-FMOD(4*(LG+J+1)))
               FTRC(I,2) = (FMOD(4*(LG+J)-2)-FMOD(4*(LG+J+1)-2)) / 
     &                     (FMOD(4*(LG+J))-FMOD(4*(LG+J+1)))
               FTRC(I,3) = (FMOD(4*(LG+J)-1)-FMOD(4*(LG+J+1)-1)) / 
     &                     (FMOD(4*(LG+J))-FMOD(4*(LG+J+1)))
            ELSE
               IF ( TEMPE+CINQ .LT. FMOD(4*(NLEXP+LG)) ) THEN
                  TRC(I,1)= FMOD(4*(NLEXP+LG)-3)
                  TRC(I,2)= FMOD(4*(NLEXP+LG)-2)
                  TRC(I,3)= FMOD(4*(NLEXP+LG)-1)
                  FTRC(I,1)=ZERO
                  FTRC(I,2)=ZERO
                  FTRC(I,3)=ZERO
               ENDIF
            ENDIF
 21      CONTINUE
         LG = LG + NLEXP
 31   CONTINUE
C
C CONSTRUCTION DES TPOINT POUR T-5 C ET POUR TOUTES LES HISTOIRES
C
      TEMPE = TEMPE - CINQ
      DO 12 I = (2*NBHIST)+1 , (3*NBHIST)
         K = I - 2*NBHIST
         A = COEF(3+9*(K-1))
         B = COEF(4+9*(K-1))
         C = COEF(5+9*(K-1))
         D = COEF(6+9*(K-1))
         E = COEF(7+9*(K-1))
         F = COEF(8+9*(K-1))
         IF ( (A.NE.ZERO) .AND. (B.NE.ZERO) .AND. (C.NE.ZERO) .AND.
     &        (D.NE.ZERO) .AND. (E.NE.ZERO) .AND. (F.NE.ZERO) ) THEN
            F7 = A + B*T + C*T**2 + D*T**3 + E*T**4 + F*T**5
            FT = A+B*TEMPE+C*TEMPE**2+D*TEMPE**3+E*TEMPE**4+F*TEMPE**5
            FPT = B+2*C*TEMPE+3*D*TEMPE**2+4*E*TEMPE**3+5*F*TEMPE**4
            FP7 = B+2*C*T+3*D*T**2+4*E*T**3+5*F*T**4
            TEMPS = FT - F7 - LOG( FP7*COEF(1+9*(K-1)) )
            TRC(I,4) = UN/(FPT*EXP(TEMPS))
            TRC(I,5) = TEMPE
         ELSE
            TRC(I,4) = COEF(1+9*(K-1))
         ENDIF
         TRC(I,5) = TEMPE
 12   CONTINUE
      TEMPE = TEMPE + CINQ
C
C CONSTRUCTION DES Z1(H) Z2(H) Z3(H) ET DZT1(H) DZT2(H) DZT3(H) ASSOCIES
C
      LG = 0
      DO 32 I = (2*NBHIST+1) , (3*NBHIST)
         K = I - 2*NBHIST
         NLEXP = NINT( COEF(9+9*(K-1)) )
         DO 22 J = 1 , NLEXP-1
            IF ( (TEMPE-CINQ .LE. FMOD(4*(LG+J))) .AND.
     &           (TEMPE-CINQ .GE. (FMOD(4*(LG+J+1))-1.D-9)) ) THEN
               COEFFZ = (TEMPE-CINQ-FMOD(4*(LG+J))) / 
     &                  ( FMOD(4*(LG+J+1)) - FMOD(4*(LG+J)) )
               TRC(I,1) = FMOD(4*(LG+J)-3) + 
     &              (FMOD(4*(LG+J+1)-3)-FMOD(4*(LG+J)-3))*COEFFZ
               TRC(I,2) = FMOD(4*(LG+J)-2) + 
     &                   (FMOD(4*(LG+J+1)-2)-FMOD(4*(LG+J)-2))*COEFFZ
               TRC(I,3) = FMOD(4*(LG+J)-1) + 
     &                   (FMOD(4*(LG+J+1)-1)-FMOD(4*(LG+J)-1))*COEFFZ
               FTRC(I,1) = (FMOD(4*(LG+J)-3)-FMOD(4*(LG+J+1)-3)) / 
     &                     ( FMOD(4*(LG+J)) - FMOD(4*(LG+J+1)) )
               FTRC(I,2) = (FMOD(4*(LG+J)-2)-FMOD(4*(LG+J+1)-2)) / 
     &                     ( FMOD(4*(LG+J)) - FMOD(4*(LG+J+1)) )
               FTRC(I,3) = (FMOD(4*(LG+J)-1)-FMOD(4*(LG+J+1)-1)) / 
     &                     ( FMOD(4*(LG+J)) - FMOD(4*(LG+J+1)) )
            ELSE
               IF ( TEMPE-CINQ .LT. FMOD(4*(NLEXP+LG)) )THEN
                  TRC(I,1) = FMOD(4*(NLEXP+LG)-3)
                  TRC(I,2) = FMOD(4*(NLEXP+LG)-2)
                  TRC(I,3) = FMOD(4*(NLEXP+LG)-1)
                  FTRC(I,1) = ZERO
                  FTRC(I,2) = ZERO
                  FTRC(I,3) = ZERO
               ENDIF
            ENDIF
 22      CONTINUE
         LG = LG + NLEXP
 32   CONTINUE
C
      END
