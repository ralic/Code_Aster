      SUBROUTINE DPVPDB( NBMAT,MATER, CRIT,DT, VINM,VINP,
     &                   NVI,SEQE, I1E, SEQM, I1M,
     &                   DP, NBRE, RETCOM)
C =====================================================================
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
      IMPLICIT      NONE
      INTEGER       NBMAT, NVI, NBRE, RETCOM
      REAL*8        MATER(NBMAT,2)
      REAL*8        CRIT(3), DT
      REAL*8        VINM(NVI), VINP(NVI)
      REAL*8        SEQE, I1E, SEQM, I1M, DP
C =====================================================================
C --- IN --- : NBMAT   NOMBRE DE PARAMETRES DU MODELE -----------------
C ---------- : MATER   COEFFICIENTS MATERIAU --------------------------
C ---------- : CRIT    TABLEAU DES PARAMETRES DE CONVERGENCE ----------
C ---------- : DT      PAS DE TEMPS -----------------------------------
C ---------- : VINM    VARIABLES INTERNES AU TEMPS MOINS --------------
C ---------- : VINP    VARIABLES INTERNES AU TEMPS PLUS ---------------
C ---------- : NVI     NOMBRE DE VI -----------------------------------
C ---------- : SEQE   CONTRAINTE EQUIVALENTE DE LA PREDICTION ELASTIQUE
C ---------- : I1E    TRACE DE LA PREDICTION ELASTIQUE ----------------
C ---------- : SEQM   CONTRAINTE EQUIVALENTE A l INSTANT MOINS --------
C ---------- : I1M    TRACE DE LA CONTRAINTE A L INSTANT MOINS---------
C ----OUT -- : DP     INCONNUE - DEFORMATION VISCOPLASTIQUE CUMULEE ---
C ---------  : NBRE   NOMBRE D ITERATIONS POUR LA CONVERGENCE LOCALE --
C ---------  : RETCOM  CODE RETOUR 0 OU 1 SI REDECOUPAGE NECESSAIRE  --
C =====================================================================
C =====================================================================
C --- LOI DE COMPORTEMENT DE TYPE DRUCKER PRAGER VISCOPLASTIQUE -------
C --- VISC_DRUC_PRAG --------------------------------------------------
C --- RESOLUTION NUMERIQUE DE L EQ NON LINEAIRE AVEC BRACKETING ET ----
C --------------LA METHODE DES CORDES (APPEL A ZEROCO)-----------------
C =====================================================================
      INTEGER  NITER, I
      INTEGER  SIGNF, SIGNFI
      REAL*8   MU, K
      REAL*8   TROIS, NEUF, ZERO
      REAL*8   PREF, A, N, CONST
      REAL*8   FONC1, FONC2, FONC3, FONC4
      REAL*8   F, FP, SEUIL, XINF, XSUP, FINF, FSUP
      REAL*8   FONECP(3), FONECM(3), FONDER(3)
      REAL*8   DPVPEQ
      REAL*8   ALPHAM, RM, BETAM
      REAL*8   DALPDP, DRDP, DBETDP
      REAL*8     DP0
      REAL*8   DPVPDF, FI
C =====================================================================
      PARAMETER ( TROIS  =  3.0D0 )
      PARAMETER ( NEUF   =  9.0D0 )
      PARAMETER ( ZERO   =  0.0D0 )
C =====================================================================
C --- AFFECTATION DES VARIABLES ---------------------------------------
C =====================================================================
      MU          = MATER(4,1)
      K           = MATER(5,1)
      PREF        = MATER(1,2)
      A           = MATER(2,2)
      N           = MATER(3,2)
C =====================================================================

       CONST = A*DT/(PREF)**N
       RETCOM = 0

C =====================================================================
C --- CALCUL DE DP ----------------------------------------------------
C =====================================================================
       CALL DPVPVA(VINM, NBMAT, MATER, FONECM)
       CALL DPVPVA(VINP, NBMAT, MATER, FONECP)
       CALL DPVPDV(VINP, NBMAT, MATER, FONDER)


       ALPHAM = FONECM(1)
       RM     = FONECM(2)
       BETAM  = FONECM(3)

       DALPDP = FONDER(1)
       DRDP   = FONDER(2)
       DBETDP = FONDER(3)

       FONC1 = SEQE + ALPHAM*I1E - RM
C
       FONC2 = TROIS*MU + DRDP  - DALPDP*I1E
     &         +NEUF*K *ALPHAM*BETAM
C
       FONC3 = NEUF*K*(ALPHAM*DBETDP+BETAM*DALPDP)
C
       FONC4 = NEUF*K*DALPDP*DBETDP

C
       IF (FONC1 .GT. ZERO) THEN
           FONC1 = FONC1
         ELSE
           FONC1 = ZERO
       ENDIF
C
       XINF = ZERO

       XSUP = A * (ABS(FONC1)/PREF)**N * DT

       FINF   = DPVPEQ(XINF,N,CONST,FONC1,FONC2,FONC3,FONC4)

       FSUP   = DPVPEQ(XSUP,N,CONST,FONC1,FONC2,FONC3,FONC4)


       NITER  = INT(CRIT(1))


       DP0 = XINF


       F    = DPVPEQ(DP0,N,CONST,FONC1,FONC2,FONC3,FONC4)
       FP   = DPVPDF(DP0,N,CONST,FONC1,FONC2,FONC3,FONC4)

       SEUIL = DPVPEQ(XINF,N,CONST,FONC1,FONC2,FONC3,FONC4)

       IF (ABS(FINF/SEUIL) .LE. CRIT(3)) THEN
           DP0 = XINF
           NBRE = 1
           GOTO 50
       ELSEIF (ABS(FSUP/SEUIL) .LE. CRIT(3)) THEN
           DP0 = XSUP
           NBRE = 1
           GOTO 50
       ENDIF

       DO 40 I = 1, NITER

        IF ((ABS(F/SEUIL)).LT.CRIT(3)) THEN
         NBRE = I
         GOTO 50
        ENDIF

        DP0 = DP0 - F/FP

        IF (DP0.GE.XSUP.OR.DP0.LE.XINF)  DP0 = (XINF+XSUP)/2

        F    = DPVPEQ(DP0,N,CONST,FONC1,FONC2,FONC3,FONC4)
        FP   = DPVPDF(DP0,N,CONST,FONC1,FONC2,FONC3,FONC4)


        IF (F.GT.ZERO) THEN
         SIGNF =  1
        ELSE
         SIGNF = -1
        ENDIF

        FI    = DPVPEQ(XINF,N,CONST,FONC1,FONC2,FONC3,FONC4)
        IF (FI.GT.ZERO) THEN
         SIGNFI =  1
        ELSE
         SIGNFI = -1
        ENDIF

        IF ((SIGNF*SIGNFI).LT.ZERO) XSUP = DP0
        IF ((SIGNF*SIGNFI).GT.ZERO) XINF = DP0

        IF (ABS(FINF/SEUIL) .LE. CRIT(3)) THEN
           DP0 = XINF
           NBRE = 1
           GOTO 50
        ELSEIF (ABS(FSUP/SEUIL) .LE. CRIT(3)) THEN
           DP0 = XSUP
           NBRE = 1
           GOTO 50
        ENDIF

  40  CONTINUE
      RETCOM = 1
      GOTO 30
C =====================================================================
 50   CONTINUE
      DP=DP0
 30   CONTINUE
C =====================================================================
      END
