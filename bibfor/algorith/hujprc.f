        SUBROUTINE HUJPRC (KK, K, TIN, VIN, MATER, YF, P, Q, TOUD)
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
C  --------------------------------------------------------
C  LOI DE HUJEUX: PROJECTION DANS LE PLAN DEVIATEUR K
C  POUR UN MECANISME CYCLIQUE
C  IN  KK       :  NUMERO D'ORDRE DU MECANISME
C      K        :  MECANISME K = 1 A 3
C      TIN( )   :  TENSEUR DES CONTRAINTES DE CAUCHY
C      VIN      :  VARIABLES INTERNES ASSOCIEES
C      MATER    :  COEFFICIENTS MATERIAU
C      YF       :  VECTEUR SOLUTION DONNANT LA VALEUR DE R ET EPSVP
C
C  OUT
C      P     :  PRESSION ISOTROPE 2D DANS LE PLAN K
C      Q     :  NORME DEVIATEUR CYCLIQUE K
C      TOUD  :  TENSEUR DEVIATOIRE CYCLIQUE DES CONTRAINTES
C  --------------------------------------------------------
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
      INTEGER   NDT, NDI, I, J, K, KK, NMOD
      PARAMETER (NMOD = 18)
      INTEGER   IFM, NIV, IADZI, IAZK24
      REAL*8    YF(NMOD), D12, DD, DEUX, VIN(*)
      REAL*8    R, X(2), TH(2), PA, PTRAC
      REAL*8    TIN(6), TOU(3), TOUD(3), P, PP, Q
      REAL*8    EPSVP, BETA, B, PHI, PCREF, PCR
      REAL*8    M, UN, DEGR, MATER(22,2), TOLE
      LOGICAL   DEBUG
      CHARACTER*8 NOMAIL

      PARAMETER (DEGR = 0.0174532925199D0)
      PARAMETER (TOLE = 1.D-7)

      COMMON /TDIM/ NDT  , NDI
      COMMON /MESHUJ/ DEBUG

      DATA   D12, DEUX, UN /0.5D0, 2.D0, 1.D0/

      CALL INFNIV (IFM, NIV)


C ==================================================================
C --- VARIABLES INTERNES -------------------------------------------
C ==================================================================
      EPSVP = YF(7)
      R     = YF(KK+7)
      X(1)  = VIN(4*K+5)
      X(2)  = VIN(4*K+6)
      TH(1) = VIN(4*K+7)
      TH(2) = VIN(4*K+8)

C ==================================================================
C --- CARACTERISTIQUES MATERIAU ------------------------------------
C ==================================================================

      BETA  = MATER(2, 2)
      B     = MATER(4, 2)
      PHI   = MATER(5, 2)
      PCREF = MATER(7, 2)
      PA    = MATER(8, 2)
      PCR   = PCREF*EXP(-BETA*EPSVP)
      PTRAC = MATER(21,2)
      M     = SIN(DEGR*PHI)


C ==================================================================
C ----------------- CONSTRUCTION DU DEVIATEUR DES CONTRAINTES ------
C ==================================================================
      J = 1
      DO 10 I = 1, NDI
        IF (I .NE. K) THEN
          TOU(J) = TIN(I)
          J = J+1
        ENDIF
  10    CONTINUE

      TOU(3)  = TIN(NDT+1-K)
      DD      = D12*(TOU(1)-TOU(2))


C ==================================================================
C ----------------- CONSTRUCTION DU DEVIATEUR CYCLIQUE -------------
C ==================================================================
      PP = D12*(TOU(1)+TOU(2)) -PTRAC

      TOU(1)=DD
      TOU(2)=-DD

      IF ((PP/PA).LE.TOLE) THEN

        IF (DEBUG) THEN
          CALL TECAEL(IADZI,IAZK24)
          NOMAIL = ZK24(IAZK24-1+3) (1:8)
          WRITE (IFM,'(10(A))')
     &    'HUJPRC :: LOG(P/PA) NON DEFINI DANS LA MAILLE ',NOMAIL
        ENDIF

        Q = 0.D0
        TOUD(1) = 0.D0
        TOUD(2) = 0.D0
        TOUD(3) = 0.D0
        GOTO 999
      ENDIF

      TOUD(1) = TOU(1)-(X(1)-R*TH(1))*PP*(UN-B*LOG(PP/PCR))*M
      TOUD(2) = - TOUD(1)
      TOUD(3) = TOU(3)-(X(2)-R*TH(2))*PP*(UN-B*LOG(PP/PCR))*M
      Q = TOUD(1)**DEUX + (TOUD(3)**DEUX)/DEUX
      Q = SQRT(Q)
      P = PP +PTRAC

 999  CONTINUE
      END
