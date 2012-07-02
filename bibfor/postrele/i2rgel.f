      SUBROUTINE I2RGEL (EPSI, S, R, F, SL, R1L, R2L, F1L, F2L, ADR)
      IMPLICIT NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C********************************************************************
C
C       RANGEMENT DU REEL S DANS LE TABLEAU SL
C       ET DE R ET F DANS LES AUTRES TABLEAUX SUIVANT LES CAS DE
C       FIGURE (I.E. SUIVANT QUE S EST OU N' EST PAS DEJA LA)
C
C       ADR : INDICE DU PREMIER ELEMENT LIBRE DANS SL
C
C       INVARIANT DE LA ROUTINE :
C
C           SL(1)<SL(2)< ... <SL(ADR-1)
C           SL(I) LIBRE (EN FAIT -1.0) POUR I = ADR, ... ,DIM_SL
C           SL(I) <-- ABSCISSE SUIVANT AB DU IEME POINT,LOCAL,
C                     D' INTERSECTION POUR I = 1, ..., ADR
C
C*******************************************************************
C
      INTEGER ADR
      REAL*8  S, R, SL(*), R1L(*), R2L(*), EPSI
      INTEGER F, F1L(*), F2L(*)
C
      LOGICAL TROUVE, DEJALA
      INTEGER I,J
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      TROUVE = .FALSE.
      DEJALA = .FALSE.
      I      = 1
      J      = 1
C
10    CONTINUE
      IF ( (.NOT. TROUVE) .AND. (I .LT. ADR) ) THEN
C
         IF ( ABS(SL(I)-S) .LT. EPSI ) THEN
C
              TROUVE = .TRUE.
              DEJALA = .TRUE.
C
         ELSE
C
              IF ( SL(I) .LT. S ) THEN
C
                 I = I + 1
C
              ELSE
C
                 TROUVE = .TRUE.
C
              ENDIF
C
         ENDIF
C
         GOTO 10
C
      ENDIF
C
      IF (DEJALA) THEN
C
         R2L(I) = R
         F2L(I) = F
C
      ELSE
C
         DO 20, J = ADR, I+1, -1
C
            SL (J) = SL(J-1)
            R1L(J) = R1L(J-1)
            R2L(J) = R2L(J-1)
            F1L(J) = F1L(J-1)
            F2L(J) = F2L(J-1)
C
20       CONTINUE
C
         SL (I) = S
         R1L(I) = R
         R2L(I) = -1.0D0
         F1L(I) = F
         F2L(I) = 0
C
         ADR = ADR + 1
C
      ENDIF
C
      END
