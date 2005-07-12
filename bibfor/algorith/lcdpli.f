      SUBROUTINE LCDPLI(MOD,NVI,OPTION,MATERF,SIGM,
     &                                    DEPS,VIM,VIP,SIG,DSIDEP,IRET)
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/07/2005   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER       IRET,NVI
      REAL*8        DEPS(6),VIM(NVI),VIP(NVI),SIG(6),DDOT
      REAL*8        SIGM(6),MATERF(4,2),DSIDEP(6,6)
      CHARACTER*8   MOD
      CHARACTER*16  OPTION
C =====================================================================
C --- LOI DE COMPORTEMENT DRUCKER PRAGER DE TYPE LINEAIRE -------------
C =====================================================================
C IN  OPTION  OPTION DE CALCUL (RAPH_MECA, RIGI_MECA_TANG OU FULL_MECA)
C IN  EPSM    CHAMP DE DEFORMATION EN T-
C IN  DEPS    INCREMENT DU CHAMP DE DEFORMATION
C IN  VIM     VARIABLES INTERNES EN T-
C               1   : ENDOMMAGEMENT (D)
C               2-7 : DEFORMATION ELASTIQUE (EPE AVEC RAC2 HD)
C               8   : INDICATEUR DISSIPATIF (1) OU ELASTIQUE (0)
C VAR VIP     VARIABLES INTERNES EN T+
C              IN  ESTIMATION (ITERATION PRECEDENTE)
C              OUT CALCULEES
C OUT SIGP    CONTRAINTES EN T+
C OUT DSIDEP  MATRICE TANGENTE
C OUT IRET    CODE RETOUR (0 = OK)
C =====================================================================
      LOGICAL     RIGI,RESI
      INTEGER     NDT,NDI,II,JJ
      REAL*8      TROIS,DEUX,DP,DPDENO,ALPHA,PMOINS,PPLUS
      REAL*8      HOOKF(6,6),DKOOH(6,6),PLAS,DPLITG
      REAL*8      EPSP(6),EPSM2(6),SIGE(6),SE(6),SIIE,SEQ,I1E,TRACE
C =====================================================================
      PARAMETER ( DEUX  = 2.0D0 )
      PARAMETER ( TROIS = 3.0D0 )
      COMMON /TDIM/   NDT  , NDI
C =====================================================================
C --- INITIALISATION --------------------------------------------------
C =====================================================================
      PMOINS = VIM(1)
      IRET   = 0
      RESI   = OPTION(1:9).EQ.'FULL_MECA' .OR.
     +         OPTION     .EQ.'RAPH_MECA'
      RIGI   = OPTION(1:9).EQ.'FULL_MECA' .OR.
     +         OPTION(1:9).EQ.'RIGI_MECA'
      IF ( (OPTION(1:9).NE.'RIGI_MECA') .AND.
     +     (OPTION(1:9).NE.'FULL_MECA') .AND.
     +     (OPTION     .NE.'RAPH_MECA') )  THEN
         CALL UTMESS('F','LCDPPA','PROBLEME SUR LE TYPE D OPTION')
      ENDIF
C =====================================================================
C --- AFFECTATION DES VARIABLES ---------------------------------------
C =====================================================================
      ALPHA  = MATERF(3,2)
C =====================================================================
C --- OPERATEUR ELASTIQUE LINEAIRE ISOTROPE ---------------------------
C =====================================================================
      CALL LCOPLI ( 'ISOTROPE', MOD, MATERF(1,1), HOOKF )
      CALL LCOPIL ( 'ISOTROPE', MOD, MATERF(1,1), DKOOH )
      CALL LCPRMV ( DKOOH,SIGM, EPSM2 )
      CALL LCSOVE ( EPSM2, DEPS, EPSP )
C =====================================================================
C --- INTEGRATION ELASTIQUE : SIGF = HOOKF EPSP -----------------------
C =====================================================================
      CALL     LCPRMV(HOOKF,EPSP,SIGE)
      CALL     LCDEVI(SIGE,SE)
      SIIE=DDOT(NDT,SE,1,SE,1)
      SEQ    = SQRT  (TROIS*SIIE/DEUX)
      I1E    = TRACE (NDI,SIGE)
C =====================================================================
C --- CALCUL DES CONTRAINTES ------------------------------------------
C =====================================================================
      IF (RESI) THEN
C =====================================================================
C --- RESOLUTION DU SYSTEME -------------------------------------------
C =====================================================================
         CALL RESDP1( MATERF, SEQ, I1E, PMOINS, DP, PLAS)
         IF (PLAS.EQ.0.0D0) THEN
            DO 10 II=1,NDT
               SIG(II) = SIGE(II)
 10         CONTINUE
            VIP(2) = 0.0D0
         ELSE
            CALL MAJSIG ( MATERF, SE, SEQ, I1E, ALPHA, DP, SIG)
         ENDIF
C =====================================================================
C --- STOCKAGE DES VARIABLES INTERNES ---------------------------------
C =====================================================================
         VIP(1) = VIM(1) + DP
         VIP(2) = VIM(2) + TROIS*ALPHA*DP
         VIP(NVI) = PLAS
C =====================================================================
C --- PREPARATION AU CALCUL DE LA MATRICE TANGENTE --------------------
C =====================================================================
         DPDENO = DPLITG( MATERF, VIP(1), PLAS )
         PPLUS  = VIP(1)
      ELSE
         PLAS   = VIM(NVI)
         DP     = 0.0D0
         PPLUS  = 0.0D0
         DPDENO = DPLITG( MATERF, PMOINS, PLAS )
      ENDIF
C =====================================================================
C --- CALCUL DE LA MATRICE TANGENTE -----------------------------------
C =====================================================================
      IF (RIGI) THEN
         IF (OPTION(10:14).EQ.'_ELAS') THEN
            CALL LCEQMA(HOOKF, DSIDEP)
         ELSE
            CALL DPMATA( MOD, MATERF, ALPHA, DP, DPDENO, PPLUS,
     +                                           SE, SEQ, PLAS, DSIDEP)
         ENDIF
      ENDIF
C =====================================================================
      END
