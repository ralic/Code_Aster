      SUBROUTINE VPZECH (D,Z,LOW,HIGH,MM,NEQ,IZ)
      IMPLICIT NONE
      INTEGER                LOW,HIGH,MM,NEQ,IZ
      REAL*8             D(1),Z(IZ,1)
C     ------------------------------------------------------------------
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
C     MISE A L'ECHELLE (NORMALISATION) DE LA COLONNE Z PAR LA BONNE
C     VALEUR DE "D"
C     ------------------------------------------------------------------
C     SERT A L'EQUILIBRAGE D'UNE MATRICE POUR LE CALCUL DE SES VALEURS
C     ET VECTEURS PROPRES.
C     ------------------------------------------------------------------
C     REFERENCE: F.L. BAUER - J.H. WILKINSON - C. REINSCH
C        HANDBOOK FOR AUTOMATIC COMPUTATION - LINEAR ALGEBRA - VOL.2
C        PAGE 321 (ROUTINE BALBAK)
C     ------------------------------------------------------------------
      INTEGER            I,J,II,JJ
      REAL*8             S
C     ------------------------------------------------------------------
      DO 10 I=LOW,HIGH
         S = D(I)
         DO 5 J=1,MM
            Z(I,J) = Z(I,J)*S
    5    CONTINUE
   10 CONTINUE
C
C     --- REPERMUTER LES LIGNES SI CA A ETE FAIT DANS VPZBAL ---
      DO 20 II = LOW-1,1,-1
         JJ = NINT(D(II))
         IF (II.NE.JJ) THEN
            DO 25 J=1,MM
               S       = Z(II,J)
               Z(II,J) = Z(JJ,J)
               Z(JJ,J) = S
   25       CONTINUE
         ENDIF
   20 CONTINUE
C
      DO 30 II=HIGH+1,NEQ
         JJ = NINT(D(II))
         IF (II.NE.JJ) THEN
            DO 35 J=1,MM
               S = Z(II,J)
               Z(II,J) = Z(JJ,J)
               Z(JJ,J) = S
   35       CONTINUE
         ENDIF
   30 CONTINUE
      END
