      SUBROUTINE DPVPRE(MOD,NVI,OPTION,CRIT,INSTAM,INSTAP,
     &          NBMAT,MATERF,SIGM,DEPS,VIM,VIP,SIG,NBRE,DSIDEP,IRET)
C =====================================================================
      IMPLICIT      NONE
      INTEGER       IRET,NVI,NBMAT
      REAL*8        DEPS(6),VIM(NVI),VIP(NVI),SIG(6)
      REAL*8        SIGM(6),MATERF(NBMAT,2),DSIDEP(6,6),CRIT(3)
      REAL*8        INSTAM, INSTAP
      CHARACTER*8   MOD
      CHARACTER*16  OPTION
C =====================================================================
C --- LOI DE COMPORTEMENT DRUCKER PRAGER VISCOPLASTIQUE ---------------
C --- VISC_DRUC_PRAG --------------------------------------------------
C ----RESOLUTION -----------------------------------------------------
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
      LOGICAL     RESI
      INTEGER     NDT,NDI,II, POS, NBRE
      REAL*8      DEUX,TROIS
      REAL*8      PPIC, PULT
      REAL*8      DP
      REAL*8      DT,PLAS
      REAL*8      FCRIT, DPVPCR
      REAL*8      SIIM, SIIE,SEQM,SEQE,I1M,I1E,TRACE
      REAL*8      FONECM(3), FONECP(3)
      REAL*8      HOOKF(6,6),DKOOH(6,6)
      REAL*8      SIGE(6),SE(6), SM(6)
      REAL*8       INTE(6)
C =====================================================================
      PARAMETER ( DEUX    = 2.0D0 )
      PARAMETER ( TROIS   = 3.0D0 )
      COMMON /TDIM/   NDT  , NDI
C =====================================================================
C --- RECUPERATION DES MATERIAUX --------------------------------------
C =====================================================================
      PPIC      = MATERF(4,2)
      PULT      = MATERF(5,2)
C =====================================================================
C --- INITIALISATION --------------------------------------------------
C =====================================================================
      CALL     LCINMA ( 0.0D0, HOOKF  )
      CALL     LCINMA ( 0.0D0, DKOOH  )
      CALL     LCINVE ( 0.0D0, SIG    )
      CALL     LCINVE ( 0.0D0, SIGE   )

      IRET     = 0
      DT       = INSTAP - INSTAM
      PLAS     = 0.D0
      DP       = 0.D0

C =====================================================================
C =====================================================================
      RESI   = OPTION(1:9).EQ.'FULL_MECA' .OR.
     &         OPTION     .EQ.'RAPH_MECA'
      CALL ASSERT ( (OPTION(1:9).EQ.'RIGI_MECA') .OR.
     &     (OPTION(1:9).EQ.'FULL_MECA') .OR.
     &     (OPTION     .EQ.'RAPH_MECA') )
C =====================================================================
C --- OPERATEUR ELASTIQUE LINEAIRE ISOTROPE ---------------------------
C =====================================================================
      CALL LCOPLI ( 'ISOTROPE', MOD, MATERF(1,1), HOOKF )
      CALL LCOPIL ( 'ISOTROPE', MOD, MATERF(1,1), DKOOH )

      CALL  LCDEVI(SIGM,SM)
      CALL  LCPRSC(SM,SM,SIIM)
      SEQM  = SQRT (TROIS*SIIM/DEUX)
      I1M       = TRACE(NDI,SIGM)

C =====================================================================
C --- PREDICTION ELASTIQUE : SIGF = HOOKF EPSP -----------------------
C =====================================================================


      CALL LCPRMV (HOOKF, DEPS, INTE )
      CALL LCSOVE (SIGM , INTE, SIGE)

      CALL  LCDEVI(SIGE,SE)
      CALL  LCPRSC(SE,SE,SIIE)
      SEQE  = SQRT (TROIS*SIIE/DEUX)
      I1E   = TRACE(NDI,SIGE)
C =====================================================================
C --- CALCUL DU CRITERE -----------------------------------------------
C =====================================================================
      IF (RESI) THEN
C =====================================================================
C --- SIGNE DU CRITERE ------------------------------------------------
C =====================================================================
          CALL DPVPVA(VIM(1), NBMAT, MATERF, FONECM)

          FCRIT  = DPVPCR(FONECM,  SEQE, I1E)

              IF ( FCRIT.GT.0.0D0 ) THEN
                PLAS = 1.0D0
C =====================================================================
C ---------------------RESOLUTION EQ NON LINEAIRE EN DP----------------
C =====================================================================
                CALL DPVPDB(NBMAT,MATERF,CRIT,DT,VIM,VIP,NVI,
     &                 SEQE,I1E,SEQM,I1M,DP,NBRE,IRET)
              ELSE
                    PLAS  = 0.0D0
                    DP      = 0.0D0
                    NBRE = 0
              ENDIF
C =====================================================================
C --- MISE A JOUR DES CONTRAINTES TENANT COMPTE DE DP SI VISCOPLASTICIT
C =====================================================================
          IF (PLAS.EQ.0.0D0) THEN
           DO 10 II=1,NDT
               SIG(II) = SIGE(II)
 10        CONTINUE

               VIP(1) = VIM(1)
               VIP(3) = VIM(3)
               VIP(4) = VIM(4)
          ELSE
               VIP(1)   = VIM(1) + DP

               CALL DPVPVA (VIP(1), NBMAT, MATERF, FONECP)
               CALL DPVPSI (NBMAT,MATERF,SE,SEQE,I1E,FONECP,DP,SIG)
          ENDIF
C =====================================================================
C --- STOCKAGE DES VARIABLES INTERNES ---------------------------------
C =====================================================================
          VIP(2)   = PLAS

          VIP(NVI) = NBRE

          IF (VIP(1) .LT. PPIC) THEN
                  POS = 1
          ELSEIF (VIP(1) .LT. PULT) THEN
                  POS = 2
          ELSE
                  POS = 3
          ENDIF

                VIP(3)   = POS
      ENDIF
C =====================================================================
C --- TERMES DE L OPERATEUR TANGENT -----------------------------------
C =====================================================================
      IF (OPTION(10:14).EQ.'_ELAS') THEN
            CALL LCEQMA(HOOKF, DSIDEP)
      ENDIF

      IF ( OPTION(1:14) .EQ. 'RIGI_MECA_TANG') THEN
            CALL LCEQMA(HOOKF, DSIDEP)
      ENDIF
      IF ( OPTION(1:9) .EQ. 'FULL_MECA') THEN
        IF (VIP(2) .EQ. 1.D0) THEN
             CALL DPVPOT(MOD,VIM(1),VIP(1),NBMAT,MATERF,SIGE,
     &                   DT,DP,PLAS,DSIDEP)
        ELSEIF (VIP(2).EQ.0.D0) THEN
             CALL LCEQMA(HOOKF, DSIDEP)
        ENDIF
      ENDIF
C =====================================================================
      END
