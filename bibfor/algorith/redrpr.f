      SUBROUTINE REDRPR( MOD, IMATE, SIGP, VIP, DSDE, ICODE )
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
C =====================================================================
      IMPLICIT      NONE
      INTEGER       IMATE, ICODE
      REAL*8        VIP(*), SIGP(*), DSDE(6,6)
      CHARACTER*8   MOD
C =====================================================================
C --- ROUTINE POUR LES RECUPERATIONS DES DONNES -----------------------
C --- POUR LE CALCUL DU TENSEUR TANGENT -------------------------------
C --- ICODE = 0 CORRESPONDT AU CAS ELASTIQUE --------------------------
C --- ICODE = 1 SINON -------------------------------------------------
C =====================================================================
      INTEGER       NDT, NDI, NVI, TYPEDP
      REAL*8       PPLUS, MATERF(5,2), HOOKF(6,6), DPDENO, DP
      REAL*8       SE(6), SEQ, PLAS, ALPHA, DPLITG, DPPATG, PHI
      REAL*8       SIIE, DEUX, TROIS,DDOT
C ======================================================================
      COMMON /TDIM/   NDT, NDI
C =====================================================================
C --- INITIALISATION --------------------------------------------------
C =====================================================================
      PARAMETER  ( DEUX  = 2.0D0 )
      PARAMETER  ( TROIS = 3.0D0 )
C =====================================================================
      CALL LCINVE ( 0.0D0, SE   )
      CALL LCINMA ( 0.0D0, DSDE )
      CALL LCINMA ( 0.0D0, HOOKF)
C =====================================================================
C --- RECUPERATION DES DONNEES MATERIAUX ------------------------------
C =====================================================================
      CALL DPMATE(MOD, IMATE, MATERF, NDT, NDI, NVI, TYPEDP)
      PPLUS  = VIP(1)
      DP     = 0.0D0
      PLAS   = VIP(NVI)
      ICODE  = 1
      SEQ    = 0.0D0
      IF (TYPEDP.EQ.1) THEN
C =====================================================================
C --- RECUPERATION DE R' POUR UNE LOI DP DE TYPE LINEAIRE -------------
C =====================================================================
         ALPHA  = MATERF(3,2)
         DPDENO = DPLITG( MATERF, PPLUS, PLAS )
      ELSE IF (TYPEDP.EQ.2) THEN
C =====================================================================
C --- RECUPERATION DE R' POUR UNE LOI DP DE TYPE PARABOLIQUE ----------
C =====================================================================
         PHI    = MATERF(2,2)
         ALPHA  = DEUX * SIN(PHI) / (TROIS - SIN(PHI))
         DPDENO = DPPATG( MATERF, PPLUS, PLAS )
      ENDIF
      IF (PLAS.EQ.0.0D0) THEN
         ICODE = 0
      ELSE
         IF (PLAS.EQ.1.0D0) THEN
C =====================================================================
C --- INTEGRATION ELASTIQUE : SIGF = HOOKF EPSP -----------------------
C =====================================================================
            CALL     LCDEVI(SIGP,SE)
            SIIE=DDOT(NDT,SE,1,SE,1)
            SEQ     = SQRT (TROIS*SIIE/DEUX)
         ENDIF
         CALL DPMATA( MOD, MATERF, ALPHA, DP, DPDENO, PPLUS,
     +                                             SE, SEQ, PLAS, DSDE)
      ENDIF
C =====================================================================
C =====================================================================
      END
