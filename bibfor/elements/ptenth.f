      SUBROUTINE PTENTH ( UL, XL, F, N, MAT, ITYPE, ENERTH )
      IMPLICIT  NONE
      INCLUDE 'jeveux.h'
      INTEGER   ITYPE, N
      REAL*8    UL(12), F, MAT(N,N), ENERTH
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C TOLE CRP_6
C     CALCUL DE L'ENERGIE DE DEFORMATION THERMIQUE POUR LES
C          ELEMENTS POUTRE (POU_D_T, POU_D_E, POU_C_T)
C     ------------------------------------------------------------------
C IN  UL     :  DEPLACEMENTS DES NOEUDS DE LA POUTRE
C IN  XL     :  LONGUEUR DE LA POUTRE
C IN  F      :  DILATATION DU MATERIAU
C IN  N      :  DIMENSION DE LA MATRICE MAT
C IN  MAT    :  MATRICE DE RAIDEUR
C IN  ITYPE  :  TYPE DE LA SECTION
C OUT ENERTH :  ENERGIE DE DEFORMATION THERMIQUE
C     ------------------------------------------------------------------
      INTEGER  I, J, LRCOU
      REAL*8   UGT(12), FLT(12), ALONG, ANGS2, DEUX,  RAD, XL,
     +         ZERO, FLM(12)
C     ------------------------------------------------------------------
C
C --- INITIALISATIONS :
C     ---------------
      ZERO  = 0.D0
      DEUX  = 2.D0
C
      ENERTH = ZERO
      DO 10 I = 1, 12
        UGT(I) = ZERO
        FLT(I) = ZERO
        FLM(I) = ZERO
  10  CONTINUE
C
C --- CARACTERISTIQUES DE POUTRES COURBES :
C     -----------------------------------
      IF  ( ITYPE .EQ. 10 ) THEN
         CALL JEVECH ('PCAARPO', 'L',LRCOU)
         RAD    = ZR(LRCOU)
         ANGS2  = ASIN( XL / ( DEUX * RAD ) )
         XL     = RAD * ANGS2 * DEUX
      ENDIF
C
C
      IF (F.NE.ZERO) THEN
        IF (ITYPE.NE.10) THEN
          UGT(1) = -F*XL
          UGT(7) = -UGT(1)
        ELSE
          ALONG = DEUX*RAD*F*SIN(ANGS2)
          UGT(1) = -ALONG*COS(ANGS2)
          UGT(2) =  ALONG*SIN(ANGS2)
          UGT(7) = -UGT(1)
          UGT(8) =  UGT(2)
        ENDIF
C
C ---   CALCUL DES FORCES INDUITES PAR LES DEFORMATIONS THERMIQUES :
C       ----------------------------------------------------------
        DO 20 I = 1, 6
          DO 30 J = 1, 6
            FLT(I)   = FLT(I)   - MAT(I,J)    *UGT(J)
            FLT(I+6) = FLT(I+6) - MAT(I+6,J+6)*UGT(J+6)
            FLM(I)   = FLM(I)   - MAT(I,J)    *UL(J)
            FLM(I+6) = FLM(I+6) - MAT(I+6,J+6)*UL(J+6)
  30      CONTINUE
  20    CONTINUE
C
C ---   ENERGIE DE DEFORMATION INDUITE PAR LES DEFORMATIONS THERMIQUES :
C       --------------------------------------------------------------
        DO 40 I = 1, 12
            ENERTH = ENERTH + (0.5D0*UGT(I)*FLT(I)-UGT(I)*FLM(I))
  40    CONTINUE

      ENDIF
C
C
      END
