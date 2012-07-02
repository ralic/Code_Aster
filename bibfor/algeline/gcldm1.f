      SUBROUTINE GCLDM1(M,IN,IP,PREC,X,Y)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C TOLE CRP_4
      IMPLICIT NONE
      REAL*8 PREC(*),X(*),Y(*)
      INTEGER*4 IP(*)
      INTEGER IN(*)
      REAL*8 FAC
C-----------------------------------------------------------------------
C  FONCTION  :  INVERSION D'UNE MATRICE DE PRECONDITIONNEMENT LDLT_INC
C                        -1                   T
C               Y = (MAT)  *X   OU MAT = L*D*L
C          LA MATRICE MAT EST STOCKEE SOUS FORME MORSE
C                                                      -1
C          ET A LA PLACE DE 1. DANS MAT(I,I) ON A (D(I))
C               RESOLUTION DU SYSTEME :
C                                    T -1
C                          Y = (L D L ) * X
C-----------------------------------------------------------------------
C     RESOLUTION DU PREMIER SYSTEME L.W = X
C-------------------------------------------
C-----------------------------------------------------------------------
      INTEGER I ,KDEB ,KFIN ,KI ,M 
      REAL*8 SOM 
C-----------------------------------------------------------------------
      Y(1) = X(1)
C
      DO 20 I = 2 , M
         SOM = 0.D0
         KDEB = IN(I-1)+1
         KFIN = IN(I)-1
         DO 10 KI = KDEB , KFIN
            SOM = SOM + PREC(KI)*Y(IP(KI))
   10    CONTINUE
         Y(I) = (X(I)-SOM)
   20 CONTINUE
C-------------------------------------------
C     RESOLUTION DE D.Y = W
C-------------------------------------------
      DO 50 I = 1,M
         Y(I) = Y(I)*PREC(IN(I))
50    CONTINUE
C-------------------------------------------
C     RESOLUTION DU SECOND SYSTEME LT.Y = W
C-------------------------------------------
      DO 40 I = M , 2 , -1
         KDEB = IN(I-1)+1
         KFIN = IN(I)-1
         FAC = Y(I)
C
C        ---- PROCEDURE A LA MAIN
CDIR$ IVDEP
CDIR$ NOPREFETCH Y
         DO 30 KI = KDEB , KFIN
            Y(IP(KI)) = Y(IP(KI))-PREC(KI)*FAC
   30    CONTINUE
   40 CONTINUE
      END
