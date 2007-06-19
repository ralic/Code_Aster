      SUBROUTINE I3SL3R ( A, B, EZ, CS )
      IMPLICIT NONE
      REAL*8     A(*), B(*), EZ(*), CS(3,*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 17/12/2002   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_6
C     CALCUL D'UNE MATRICE DE PASSAGE EN 3D PLUS TRANSLATION
C     ------------------------------------------------------------------
      INTEGER   I, J
      REAL*8    CSTMP(3,4), DIF(3,4), NORMA, NORME, ANGLE
      REAL*8    P(3,3), P1(3,3), R1T(3,3), R1(3,3), R2(3,3)
      REAL*8    EX1(3), EY1(3), EZ1(3), AB, DPROJ
C     ------------------------------------------------------------------
C
      AB = 0.0D0
      NORMA = SQRT( A(1)**2 + A(2)**2 + A(3)**2 )
      DO 10, I = 1, 3, 1
         EX1(I) = A(I) / NORMA
         AB = AB + (A(I)*B(I))
 10   CONTINUE
      ANGLE = ACOS(AB)
C
      CALL PROVEC ( A, B, EZ1 )
      NORME = SQRT( EZ1(1)**2 + EZ1(2)**2 + EZ1(3)**2 )
      DO 12, I = 1, 3, 1
         EZ1(I) = EZ1(I) / NORME
 12   CONTINUE
C
      CALL PROVEC ( EZ1, EX1, EY1 )
      NORME = SQRT( EY1(1)**2 + EY1(2)**2 + EY1(3)**2 )
      DO 14, I = 1, 3, 1
         EY1(I) = EY1(I) / NORME
 14   CONTINUE
C
      DO 20, I = 1, 3, 1
          R1(1,I) = EX1(I)
          R1(2,I) = EY1(I)
          R1(3,I) = EZ1(I) 
          R1T(I,1) = EX1(I)
          R1T(I,2) = EY1(I)
          R1T(I,3) = EZ1(I) 
 20   CONTINUE
C
      R2(1,1) =  COS(ANGLE)
      R2(1,2) = -SIN(ANGLE)
      R2(1,3) = 0
      R2(2,1) = SIN(ANGLE)
      R2(2,2) = COS(ANGLE)
      R2(2,3) = 0
      R2(3,1) = 0
      R2(3,2) = 0
      R2(3,3) = 1
C
      DO 30, I = 1, 3, 1
         DO 32, J = 1, 3, 1
            P1(I,J) = (R1T(I,1)*R2(1,J)) + (R1T(I,2)*R2(2,J))
     +                                   + (R1T(I,3)*R2(3,J))
 32      CONTINUE
 30   CONTINUE
C
      DPROJ = 0.0D0
      DO 40, I = 1, 3, 1
         DO 42, J = 1, 3, 1
            P(I,J) = (P1(I,1)*R1(1,J)) + (P1(I,2)*R1(2,J))
     +                                 + (P1(I,3)*R1(3,J))
 42      CONTINUE
         DPROJ = DPROJ - (CS(I,1)*EZ(I))
 40   CONTINUE
      DO 50, I = 2, 4, 1
         DO 52, J = 1, 3, 1
            DIF(J,I) = CS(J,I)-CS(J,1)
 52      CONTINUE
 50   CONTINUE
C
      DO 60, I = 2, 4, 1
         DO 62, J = 1, 3, 1
            CSTMP(J,I) = (P(J,1)*DIF(1,I)) + (P(J,2)*DIF(2,I))
     +                                     + (P(J,3)*DIF(3,I))
            CS(J,I) = CS(J,I) - DIF(J,I) + CSTMP(J,I)
 62      CONTINUE
 60   CONTINUE 
C
      DO 70, I = 1, 4, 1
         DO 72, J = 1, 3, 1
            CS(J,I) = CS(J,I) + ( DPROJ * EZ(J) )
 72      CONTINUE
 70   CONTINUE
C
      END
