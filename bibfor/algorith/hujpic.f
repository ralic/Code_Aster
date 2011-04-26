        SUBROUTINE HUJPIC (KK, K, TIN, VIN, MATER, YF, PC)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C  -----------------------------------------------------------
C  LOI DE HUJEUX: CALCUL DE LA PRESSION ISOTROPE CYCLIQUE
C  IN  KK       :  NUMERO D'ORDRE DU MECANISME (1 A 4)
C      K        :  MECANISME K = 8
C      TIN( )   :  CHAMPS DE CONTRAINTES
C      VIN      :  VARIABLES INTERNES ASSOCIEES
C      MATER    :  COEFFICIENT MATERIAU
C      YF       :  VECTEUR SOLUTION DU SYSTEME DE NEWTON LOCAL
C
C  OUT
C      PC     :  PRESSION ISOTROPE CYCLIQUE
C  -----------------------------------------------------------
        INTEGER   NDT, NDI, I, K, KK, NMOD
        PARAMETER     (NMOD = 15)

        REAL*8      VIN(*), D, X4
        REAL*8    TIN(NDT), PC, D13, ZERO
        REAL*8    EPSVP, BETA, PCREF, PCR
        REAL*8    I1, MATER(22,2), YF(NMOD)

        COMMON /TDIM/ NDT  , NDI

        DATA   D13, ZERO /0.333333333334D0, 0.D0/

C ==================================================================
C --- VARIABLES INTERNES -------------------------------------------
C ==================================================================

        EPSVP = YF(7)
        X4    = VIN(21)

C ==================================================================
C --- CARACTERISTIQUES MATERIAU ------------------------------------
C ==================================================================

        BETA  = MATER(2, 2)
        D     = MATER(3, 2)
        PCREF = MATER(7, 2)
        PCR   = PCREF*EXP(-BETA*EPSVP)

C ======================================================================
C ----------------- CONSTRUCTION PRESSION ISOTROPE ---------------------
C ======================================================================

        I1 = ZERO
        DO 10 I = 1, NDI
          I1=I1+D13*TIN(I)
  10    CONTINUE

C ======================================================================
C ------------ CONSTRUCTION PRESSION ISOTROPE CYCLIQUE -----------------
C ======================================================================

        PC = ABS(I1)+D*PCR*X4

        END
