      SUBROUTINE BURDFI(BFI,CFI,NR,YD,DY)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE FOUCAULT A.FOUCAULT
C
C=====================================================================
C ROUTINE QUI CALCUL L INCREMENT DE DEFORMATIONS DE FLUAGE PROPRE
C
C IN  BFI      : MATRICE FLUAGE PROPRE IRREVERSIBLE
C     CFI      : MATRICE FLUAGE PROPRE IRREVERSIBLE
C     NR       : DIMENSION VECTEUR YD ET DY
C     YD       : VECTEUR DES INCONNUES INITIALES
C     DY       : INCREMENT DU VECTEUR DES INCONNUES AVEC SIGMA MAJ
C OUT DY       : INCREMENT DU VECTEUR DES INCONNUES AVEC VARI MAJ
C_______________________________________________________________________
C
      IMPLICIT NONE
      INTEGER  NDT,NDI,I,NR
      REAL*8   BFI(6,6),CFI(6,6)
      REAL*8   YD(NR),DY(NR)
      REAL*8   BNSIGD(6),SIGF(6),CNSIGF(6)
      REAL*8   DEPSF(6)

C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT ,NDI
C     ----------------------------------------------------------------

C === =================================================================
C --- CONSTRUCTION DES PARTIES PRENANTES POUR DEFORMATION IRREVERSIBLE
C === =================================================================
C === =================================================================
C --- CONSTRUCTION TENSEUR BFI*SIGD
C === =================================================================
      CALL LCPRMV(BFI,YD,BNSIGD)
C === =================================================================
C --- CONSTRUCTION TENSEUR CFI*SIGF
C === =================================================================
      CALL LCSOVE(YD,DY,SIGF)
      CALL LCPRMV(CFI,SIGF,CNSIGF)
C === =================================================================
C --- CONSTRUCTION TENSEUR CNSIGF+BNSIGD EQUIVAUT A BFI*SIGD+CFI*SIGF
C === =================================================================
      CALL LCSOVE(BNSIGD,CNSIGF,DEPSF)
C === =================================================================
C --- AFFECTATION DES VALEURS A DY + MISE A L'ECHELLE
C === =================================================================
      DO 2 I=1,NDT
        DY(NDT+I) = DEPSF(I)
 2    CONTINUE

      END
