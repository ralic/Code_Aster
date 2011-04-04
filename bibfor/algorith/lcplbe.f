        SUBROUTINE LCPLBE ( TOLER, ITMAX, NMAT, MATERF, NVI, VIND,
     &                      SIGF, VINF, ELGEOM, NSEUIL, IRTETI )
         IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_20
C TOLE CRP_21
C       ----------------------------------------------------------------
C       INTEGRATION ELASTO-PLASTIQUE DE LOIS DE COMPORTEMENT BETON SUR
C       DT  ( Y = ( DPC , DPT ))
C       BETON_DOUBLE_DP: LOI ELASTO PLASTIQUE AVEC DOUBLE CRITERE DE
C       PLASTICITE AVEC UN SEUIL EN COMPRESSION ET UN SEUIL EN TRACTION
C       LE SYSTEME A RESOUDRE SE REDUIT A UN SYSTEME NON LINEAIRE D'UNE
C       OU DEUX EQUATIONS A DEUX INCONNUES (LES DEUX MULTIPLICATEURS
C       PLASTIQUES, EN COMPRESSION ET EN TRACTION)
C       ON RESOUD : Y = ( DPC , DPT )
C
C       ON RESOUD DONC                  R(DY) = 0
C       PAR UNE METHODE DE NEWTON       DRDY(DYI) DDYI = - R(DYI)
C                                       DYI+1 = DYI + DDYI  (DYO DEBUT)
C       ET ON REACTUALISE               YF = YD + DY
C       ----------------------------------------------------------------
C
C       IN  TOLER  :  TOLERANCE DE CONVERGENCE LOCALE
C           ITMAX  :  NOMBRE MAXI D'ITERATIONS LOCALES
C           NMAT   :  DIMENSION MATER
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           TEMPD  :  TEMPERATURE A T
C           TEMPF  :  TEMPERATURE A T+DT
C           TIMED  :  INSTANT  T
C           TIMEF  :  INSTANT T+DT
C           EPSD   :  DEFORMATION A T
C           VIND   :  VARIABLES INTERNES A T
C           NVI    :  NB VARIABLES INTERNES
C           ELGEOM :  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
C                     AUX LOIS DE COMPORTEMENT
C       VAR NSEUIL :  INDICE DE CRITERE ACTIVE
C       VAR DEPS   :  INCREMENT DE DEFORMATION
C       VAR SIGF   :  IN  : PREDICTION ELASTIQUE DE LA CONTRAINTE A T+DT
C                  :  OUT : CONTRAINTE ELASTOPLASTIQUE A T+DT
C       OUT VINF   :  VARIABLES INTERNES A T+DT
C           IRTETI = 1:  CONTROLE DU REDECOUPAGE DU PAS DE TEMPS
C       ----------------------------------------------------------------
        INTEGER         NMAT, NSEUIL
C
        INTEGER         ITMAX, NPROJS, NESSAI, OSCI
        INTEGER         NDT, NDI, NVI, ITER1, ITER2, ITER3, ITER4
C
        REAL*8          TOLER, ZERO, R8PREM, PRECM
        PARAMETER       ( ZERO =  0.D0   )
        REAL*8          SIGF(6)
        REAL*8          VIND(*),        VINF(*)
        REAL*8          MATERF(NMAT,2), ELGEOM(*)
C
C
        REAL*8          FCOMP, FCOMP3, FTRAC, FTRAC3, SIGE(6)
        REAL*8          SIGEQ, SIGH, DFCDLC, DFTDLT, CSEC
        REAL*8          COEFA(2,2), COEFB(2), COEFAR(2,2), COEFBR(2)
        REAL*8          DPC, DPT, JAC(2,2), EPSI, EPSI2, DELTA, MDELTA
        REAL*8          DDPC, DDPT, ERR, ERR2, PC, PT, FC, FT, E
        REAL*8          KUC, KUT, KE, CONECO, CONETR, PERMUT, FTRAEL
        REAL*8          VERIFC, VERIFT, FC0, FT0, MEPSI, FCP, FTP
        REAL*8          DDPT0, DDPC0, FTRAC2, FTRAC1, FCOMP1, FCOMP2
        CHARACTER*10    CTOL,  CITER, CERR
        CHARACTER*24 VALK(3)
        INTEGER         IRTETI
        LOGICAL         CONVER
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C       ----------------------------------------------------------------
C
      PRECM = 100.D0 * R8PREM()
      CONVER = .FALSE.
      FCP = MATERF(1,2)
      FTP = MATERF(2,2)
      DDPC = 0.D0
      DDPT = 0.D0
C
C --  CONTRAINTE ISSUE DE LA PREDICTION ELASTIQUE (DEJA CALCULEE)
C
      CALL LCEQVN ( NDT  ,  SIGF , SIGE )
C
C --  CALCUL DES COEFFICIENTS CONSTANTS DU SYSTEME NON LINEAIRE
C
      CALL BETINI ( MATERF, NMAT, SIGE, SIGEQ, SIGH,
     &              COEFA, COEFB, COEFAR, COEFBR, CONECO, CONETR)
C
      EPSI =  1.D-6
      EPSI2 = TOLER * SIGEQ / FCP
      IF(EPSI2.LT.TOLER) EPSI2 = TOLER
      MEPSI = -1.D-6
C --  COEFFICIENT DE SECURITE POUR LA CONVERGENCE
      CSEC = 1.D-1
C
C --  CALCUL DES ECROUISSAGES ET DERIVES DES COURBES D'ADOUCISSEMENT
C     A L'INSTANT MOINS
C
      PC  = VIND(1)
      PT  = VIND(2)
      CALL BETFPP ( MATERF, NMAT, ELGEOM, PC, PT, 3, FC0, FT0,
     &              DFCDLC, DFTDLT, KUC, KUT, KE)
C
C --  TEST DE PERMUTATION DES SOMMETS DES CONES DE TRACTION ET
C     COMPRESSION - ACTIVATION DU CRITERE DE TRACTION
C
      PERMUT = (COEFBR(1) * FT0) / (COEFBR(2) * FC0) - 1.D0
      FTRAEL = COEFB(2) - FT0
      NPROJS = 0
C
      IF ((PERMUT .GT. MEPSI .AND. CONECO .GT. FC0) .OR.
     &    (CONETR .GT. FT0) .OR. (NSEUIL.GT.4)) THEN
C
         IF ((((PERMUT.GT.MEPSI .AND. CONECO.GT.FC0 .AND.
     &      FTRAEL.GT.ZERO) .OR. (CONETR.GT.FT0)) .AND. NSEUIL.NE.11
     &      .AND. NSEUIL.NE.33) .OR. (NSEUIL.EQ.22)) THEN
C-----------------------------------------------------------------------
C --        RESOLUTION AVEC PROJECTION AU SOMMET DU CONE DE TRACTION
C-----------------------------------------------------------------------
C --        ON EFFECTUE LA RESOLUTION AVEC PROJECTION AU SOMMET DU
C --        CONE DE TRACTION SI ELLE EST DEMANDEE (NSEUIL=22) OU SI
C --        LA CONDITION SUR FT0 EST REALISEE
C
            OSCI = 0
            FTRAC1 = ZERO
            CONVER = .FALSE.
            NESSAI = 22
C
C --        CALCUL DES ECROUISSAGES ET DERIVES DES COURBES
C           D'ADOUCISSEMENT
C
            PC  = VIND(1)
            PT  = VIND(2)
            CALL BETFPP ( MATERF, NMAT, ELGEOM, PC, PT, NESSAI, FC,
     &                    FT, DFCDLC, DFTDLT, KUC, KUT, KE)
C
C --        CALCUL DES VALEURS DES CRITERES
C
            DPC = ZERO
            DPT = ZERO
            FTRAC = COEFBR(2) + COEFAR(2,2) * DPT - FT
            FCOMP = ZERO
C
C --        DEBUT DES ITERATIONS DE L'ALGORITHME DE NEWTON
C
            ITER1 = 0
 1          CONTINUE
            ITER1 = ITER1 + 1
            DDPT0 = DDPT
            IF (OSCI.EQ.0) THEN
C
C --           CALCUL DU JACOBIEN
C
               JAC(2,2) = COEFAR(2,2) - DFTDLT
C
C --           MISE A JOUR DES MULTIPLICATEURS PLASTIQUES
C --           (RESOLUTION DU SYSTEME LINEAIRE DRDY(DY).DDY = -R(DY) )
C
               E = MATERF(1,1)
               DELTA = JAC(2,2)
               IF(ABS(DELTA/E).LT.EPSI) THEN
                   CALL U2MESS('F','ALGORITH4_72')
               ELSE
                  MDELTA = -1.D0 / DELTA
               ENDIF
               DDPT = MDELTA * FTRAC
C
C --           EN CAS D'OSCILLATIONS : RESO PAR DICHOTOMIE
               IF(ITER1.GT.7) THEN
                  IF (ABS(DDPT/DPT).LT.1.D-3 .AND. ABS(DDPT).GT.PRECM)
     &               THEN
                     IF((FTRAC2*FTRAC1).LT.ZERO) THEN
                     OSCI = 20
                     DDPT = -0.5D0 *  DDPT0
                     ENDIF
                  ENDIF
               ENDIF
C
            ELSE
               IF(FTRAC*FTRAC1 .LT. ZERO) THEN
                  DDPT = -0.5D0 * DDPT0
               ELSE
                  FTRAC1 = FTRAC2
                  DDPT = 0.5D0 * DDPT0
               ENDIF
            ENDIF
C
            DPT  = DPT + DDPT
C
C --        CALCUL DES ECROUISSAGES ET DERIVES DES COURBES
C --        D'ADOUCISSEMENT
C
            PC  = VIND(1) + DPC
            PT  = VIND(2) + DPT
            CALL BETFPP ( MATERF, NMAT, ELGEOM, PC, PT, NESSAI, FC,
     &                    FT, DFCDLC, DFTDLT, KUC, KUT, KE)
C
C --        CALCUL DES VALEURS DES CRITERES
C
            FTRAC2 = FTRAC1
            FTRAC1 = FTRAC
            FTRAC = COEFBR(2) + COEFAR(2,2) * DPT - FT
C
C --        TEST DE CONVERGENCE
C
            ERR = ABS(FTRAC/FTP)
            IF(ABS(DDPT/DPT).LT.PRECM) ITER1 = ITMAX+OSCI
C
            IF (ERR .LE. (TOLER * CSEC)) THEN
C              -->>  CONVERGENCE   -->> FIN
               CONVER = .TRUE.
            ELSE
               IF (ITER1 .GE. ITMAX+OSCI) THEN
                  ERR2 = ABS(DDPT/DPT)
                  IF (ERR2 .LE. TOLER) THEN
C     -->>           NB MAX D'ITERATIONS DEPASSE MAIS TRES FAIBLES
C                    INCREMENTS DE NEWTON - CONVERGENCE   -->> FIN
C                    MESSAGE D'ALARME SI ERR2 EST INSUFFISANT
                     CONVER = .TRUE.
                     IF (ERR2 .GT. (TOLER*100)) THEN
                        CALL CODENT(ITER1,'G',CITER)
                        CALL CODREE(TOLER,'E',CTOL)
                        CALL CODREE((ERR/CSEC),'E',CERR)
                         VALK(1) = CITER
                         VALK(2) = CERR
                         VALK(3) = CTOL
                         CALL U2MESK('A','ALGORITH4_73', 3 ,VALK)
                     ENDIF
                  ELSE
C     -->>           NB MAX D'ITERATIONS DEPASSE  -->> FIN
C                    MESSAGE D'ALARME - ON POURSUIT AVEC RESO STANDARD
                     CALL CODENT(ITER1,'G',CITER)
                     CALL CODREE(TOLER,'E',CTOL)
                     CALL CODREE((ERR/CSEC),'E',CERR)
                      VALK(1) = CITER
                      VALK(2) = CERR
                      VALK(3) = CTOL
                      CALL U2MESK('A','ALGORITH4_74', 3 ,VALK)
                  ENDIF
               ELSE
C     -->>        NOUVELLE ITERATION -->> RETOUR
                  GOTO 1
               ENDIF
            ENDIF
C
C --        VERIFICATION DE L'ETAT FINAL OBTENU
C
            FCOMP3 = COEFBR(1) + COEFAR(1,2) * DPT - FC0
            FTRAC3 = COEFBR(2) + COEFAR(2,2) * DPT - FT
C
C --        VERIFICATION DE LA SOLUTION
C
            IF((FTRAC3/FTP).GT.EPSI2 .AND. DPT .GT. ZERO) THEN
               CALL U2MESS('A','ALGORITH4_75')
               CONVER = .FALSE.
            ENDIF
            IF((FCOMP3/FCP).GT.EPSI2) THEN
               CALL U2MESS('A','ALGORITH4_76')
               CONVER = .FALSE.
            ENDIF
C
C --        CONVERGENCE - MISE A JOUR DES DEFORMATIONS CUMULEES ET
C           CONTRAINTES
C
            IF (CONVER .AND. DPT .GT. ZERO) THEN
               VINF(1) = VIND(1) + DPC
               VINF(2) = VIND(2) + DPT
               VINF (NVI) = 1.D0 * NESSAI
               CALL BETINC ( MATERF, NMAT, SIGE, NESSAI, DPC,
     &                       DPT, SIGF, VERIFC, VERIFT )
               IF (VERIFT .GT. ZERO .OR. NSEUIL .GT. 4) THEN
                  NPROJS = NESSAI
               ENDIF
            ENDIF
C
         ENDIF
C
C
C
         IF (NPROJS .EQ. 0 .AND. ((((PERMUT .GT. MEPSI .AND.
     &      CONECO .GT. FC0 .AND. FTRAEL .GT. ZERO))
     &     .AND. NSEUIL .NE. 11) .OR. NSEUIL.EQ.22 .OR.
     &      NSEUIL.EQ.33)) THEN
C-----------------------------------------------------------------------
C --        RESOLUTION AVEC PROJECTION AU SOMMET DES CONES DE
C --        COMPRESSION ET DE TRACTION
C-----------------------------------------------------------------------
C --        ON EFFECTUE LA RESOLUTION AVEC PROJECTION AU SOMMET DES
C --        CONES DE COMPRESSION ET DE TRACTION SI ELLE EST DEMANDEE
C --        (NSEUIL=33) OU SI LA CONDITION SUR FT0 ET FC0 EST REALISEE
C
            OSCI = 0
            CONVER = .FALSE.
            NESSAI = 33
C
C --        CALCUL DES ECROUISSAGES ET DERIVES DES COURBES
C           D'ADOUCISSEMENT
C
            PC  = VIND(1)
            PT  = VIND(2)
            CALL BETFPP ( MATERF, NMAT, ELGEOM, PC, PT, NESSAI, FC,
     &                    FT, DFCDLC, DFTDLT, KUC, KUT, KE)
C
C --        CALCUL DES VALEURS DES CRITERES
C
            DPC = ZERO
            DPT = ZERO
            FCOMP = COEFBR(1) + COEFAR(1,1)*DPC + COEFAR(1,2)*DPT - FC
            FTRAC = COEFBR(2) + COEFAR(2,1)*DPC + COEFAR(2,2)*DPT - FT
C
C --        DEBUT DES ITERATIONS DE L'ALGORITHME DE NEWTON
C
            ITER2 = 0
 2          CONTINUE
            ITER2 = ITER2 + 1
C
C --        CALCUL DU JACOBIEN
C
            JAC(1,1) = COEFAR(1,1) - DFCDLC
            JAC(1,2) = COEFAR(1,2)
            JAC(2,1) = COEFAR(2,1)
            JAC(2,2) = COEFAR(2,2) - DFTDLT
C
C --        MISE A JOUR DES MULTIPLICATEURS PLASTIQUES
C --        (RESOLUTION DU SYSTEME LINEAIRE DRDY(DY).DDY = -R(DY) )
C
            E = MATERF(1,1)
            DELTA = (JAC(1,1) * JAC(2,2) - JAC(1,2) * JAC(2,1))
            IF(ABS(DELTA/E).LT.EPSI) THEN
C               SI DFCDLC=DFTDLT=0 LES DEUX EQUATIONS DU SYSTEME A
C               RESOUDRE SONT IDENTIQUES --> ON PASSE DIRECTEMENT A LA
C               PROJECTION AU SOMMET DU CONE DE COMPRESSION SEUL.
                IF((VIND(1) + DPC).LT.KUC
     &            .OR. (VIND(2) + DPT).LT.KUT) THEN
                  CALL U2MESS('F','ALGORITH4_77')
               ELSE
                  DDPC = ZERO
                  DDPT = ZERO
                  ITER2 = ITMAX
               ENDIF
            ELSE
               MDELTA = -1.D0 / DELTA
               DDPC = (JAC(2,2) * FCOMP - JAC(1,2) * FTRAC) * MDELTA
               DDPT = (JAC(1,1) * FTRAC - JAC(2,1) * FCOMP) * MDELTA
            ENDIF
C
            DPC  = DPC + DDPC
            DPT  = DPT + DDPT
C
C
C --        CALCUL DES ECROUISSAGES ET DERIVES DES COURBES
C --        D'ADOUCISSEMENT
C
            PC  = VIND(1) + DPC
            PT  = VIND(2) + DPT
            CALL BETFPP ( MATERF, NMAT, ELGEOM, PC, PT, NESSAI, FC,
     &                    FT, DFCDLC, DFTDLT, KUC, KUT, KE)
C
C --        CALCUL DES VALEURS DES CRITERES
C
            FCOMP = COEFBR(1) + COEFAR(1,1)*DPC + COEFAR(1,2)*DPT - FC
            FTRAC = COEFBR(2) + COEFAR(2,1)*DPC + COEFAR(2,2)*DPT - FT
C
C --        TEST DE CONVERGENCE
C
            ERR = ABS(FTRAC/FTP) + ABS(FCOMP/FCP)
            IF(ABS(DDPC/DPC).LT.PRECM.AND.ABS(DDPT/DPT).LT.PRECM)
     &         ITER2 = ITMAX
C
            IF (ERR .LE. (TOLER * CSEC)) THEN
C              -->>  CONVERGENCE   -->> FIN
               CONVER = .TRUE.
            ELSE
               IF (ITER2 .GE. ITMAX) THEN
                  ERR2 = ABS(DDPC/DPC) + ABS(DDPT/DPT)
                  IF (ERR2 .LE. TOLER) THEN
C     -->>           NB MAX D'ITERATIONS DEPASSE MAIS TRES FAIBLES
C                    INCREMENTS DE NEWTON - CONVERGENCE   -->> FIN
C                    MESSAGE D'ALARME SI ERR2 EST INSUFFISANT
                     CONVER = .TRUE.
                     IF (ERR2 .GT. (TOLER*100)) THEN
                        CALL CODENT(ITER2,'G',CITER)
                        CALL CODREE(TOLER,'E',CTOL)
                        CALL CODREE((ERR/CSEC),'E',CERR)
                         VALK(1) = CITER
                         VALK(2) = CERR
                         VALK(3) = CTOL
                         CALL U2MESK('A','ALGORITH4_73', 3 ,VALK)
                     ENDIF
                  ELSE
C     -->>           NB MAX D'ITERATIONS DEPASSE  -->> FIN
C                    MESSAGE D'ALARME - ON POURSUIT AVEC RESO STANDARD
                     CALL CODENT(ITER2,'G',CITER)
                     CALL CODREE(TOLER,'E',CTOL)
                     CALL CODREE((ERR/CSEC),'E',CERR)
                      VALK(1) = CITER
                      VALK(2) = CERR
                      VALK(3) = CTOL
                      CALL U2MESK('A','ALGORITH4_74', 3 ,VALK)
                  ENDIF
               ELSE
C     -->>        NOUVELLE ITERATION -->> RETOUR
                  GOTO 2
               ENDIF
            ENDIF
C
C --        VERIFICATION DE L'ETAT FINAL OBTENU
C
            FCOMP3 = COEFBR(1) + COEFAR(1,1)*DPC + COEFAR(1,2)*DPT - FC
            FTRAC3 = COEFBR(2) + COEFAR(2,1)*DPC + COEFAR(2,2)*DPT - FT
C
C --        VERIFICATION DE LA SOLUTION
C
            IF((FTRAC3/FTP).GT.EPSI2 .AND. DPT .GT. ZERO .AND.
     &         CONVER) THEN
               CALL U2MESS('A','ALGORITH4_78')
               CONVER = .FALSE.
            ENDIF
            IF((FCOMP3/FCP).GT.EPSI2 .AND. DPC .GT. ZERO .AND.
     &         CONVER) THEN
               CALL U2MESS('A','ALGORITH4_79')
               CONVER = .FALSE.
            ENDIF
C
C --        CONVERGENCE - MISE A JOUR DES DEFORMATIONS CUMULEES ET
C           CONTRAINTES
C
            IF (CONVER .AND. DPC .GT. ZERO .AND. DPT .GT. ZERO) THEN
               VINF(1) = VIND(1) + DPC
               VINF(2) = VIND(2) + DPT
               VINF (NVI) = 1.D0 * NESSAI
               CALL BETINC ( MATERF, NMAT, SIGE, NESSAI, DPC,
     &                       DPT, SIGF, VERIFC, VERIFT )
               IF (VERIFC .GT. ZERO .OR. NSEUIL .GT. 4) THEN
                  NPROJS = NESSAI
               ENDIF
            ENDIF
C
         ENDIF
C
C
C
         IF (NPROJS .EQ. 0 .AND. ((PERMUT .GT. MEPSI .AND.
     &      CONECO .GT. FC0) .OR.
     &      NSEUIL.EQ.11 .OR. NSEUIL.EQ.22 .OR. NSEUIL.EQ.33)) THEN
C-----------------------------------------------------------------------
C --        RESOLUTION AVEC PROJECTION AU SOMMET DU CONE DE COMPRESSION
C-----------------------------------------------------------------------
C
C --        ON EFFECTUE LA RESOLUTION AVEC PROJECTION AU SOMMET DU
C --        CONE DE COMPRESSION SI ELLE EST DEMANDEE (NSEUIL=11) OU SI
C --        LA CONDITION SUR FC0 EST REALISEE
C
            OSCI = 0
            FCOMP1 = ZERO
            CONVER = .FALSE.
            NESSAI = 11
C
C --        CALCUL DES ECROUISSAGES ET DERIVES DES COURBES
C           D'ADOUCISSEMENT
C
            PC  = VIND(1)
            PT  = VIND(2)
            CALL BETFPP ( MATERF, NMAT, ELGEOM, PC, PT, NESSAI, FC,
     &                    FT, DFCDLC, DFTDLT, KUC, KUT, KE)
C
C --        CALCUL DES VALEURS DES CRITERES
C
            DPC = ZERO
            DPT = ZERO
            FTRAC = ZERO
            FCOMP = COEFBR(1) + COEFAR(1,1) * DPC - FC
C
C --        DEBUT DES ITERATIONS DE L'ALGORITHME DE NEWTON
C
            ITER3 = 0
 3          CONTINUE
            ITER3 = ITER3 + 1
            DDPC0 = DDPC
            IF (OSCI.EQ.0) THEN
C
C --           CALCUL DU JACOBIEN
C
               JAC(1,1) = COEFAR(1,1) - DFCDLC
C
C --           MISE A JOUR DES MULTIPLICATEURS PLASTIQUES
C --           (RESOLUTION DU SYSTEME LINEAIRE DRDY(DY).DDY = -R(DY) )
C
               E = MATERF(1,1)
               DELTA = JAC(1,1)
               IF(ABS(DELTA/E).LT.EPSI) THEN
                   CALL U2MESS('F','ALGORITH4_80')
               ELSE
                  MDELTA = -1.D0 / DELTA
               ENDIF
               DDPC = MDELTA * FCOMP
C
C --           EN CAS D'OSCILLATIONS : RESO PAR DICHOTOMIE
               IF(ITER3.GT.7) THEN
                  IF (ABS(DDPC/DPC).LT.1.D-3 .AND. ABS(DDPC).GT.PRECM)
     &               THEN
                     IF((FCOMP2*FCOMP1).LT.ZERO) THEN
                     OSCI = 20
                     DDPC = -0.5D0 *  DDPC0
                     ENDIF
                  ENDIF
               ENDIF
C
            ELSE
               IF(FCOMP*FCOMP1 .LT. ZERO) THEN
                  DDPC = -0.5D0 * DDPC0
               ELSE
                  FCOMP1 = FCOMP2
                  DDPC = 0.5D0 * DDPC0
               ENDIF
            ENDIF
C
            DPC  = DPC + DDPC
C
C
C --        CALCUL DES ECROUISSAGES ET DERIVES DES COURBES
C --        D'ADOUCISSEMENT
C
            PC  = VIND(1) + DPC
            PT  = VIND(2) + DPT
            CALL BETFPP ( MATERF, NMAT, ELGEOM, PC, PT, NESSAI, FC,
     &                    FT, DFCDLC, DFTDLT, KUC, KUT, KE)
C
C --        CALCUL DES VALEURS DES CRITERES
C
            FCOMP2 = FCOMP1
            FCOMP1 = FCOMP
            FCOMP = COEFBR(1) + COEFAR(1,1) * DPC - FC
C
C --        TEST DE CONVERGENCE
C
            ERR = ABS(FCOMP/FCP)
            IF(ABS(DDPC/DPC).LT.PRECM) ITER3 = ITMAX+OSCI
C
            IF (ERR .LE. (TOLER * CSEC)) THEN
C              -->>  CONVERGENCE   -->> FIN
               CONVER = .TRUE.
            ELSE
               IF (ITER3 .GE. ITMAX+OSCI) THEN
                  ERR2 = ABS(DDPC/DPC)
                  IF (ERR2 .LE. TOLER) THEN
C     -->>           NB MAX D'ITERATIONS DEPASSE MAIS TRES FAIBLES
C                    INCREMENTS DE NEWTON - CONVERGENCE   -->> FIN
C                    MESSAGE D'ALARME SI ERR2 EST INSUFFISANT
                     CONVER = .TRUE.
                     IF (ERR2 .GT. (TOLER*100)) THEN
                        CALL CODENT(ITER3,'G',CITER)
                        CALL CODREE(TOLER,'E',CTOL)
                        CALL CODREE((ERR/CSEC),'E',CERR)
                         VALK(1) = CITER
                         VALK(2) = CERR
                         VALK(3) = CTOL
                         CALL U2MESK('A','ALGORITH4_73', 3 ,VALK)
                     ENDIF
                  ELSE
C     -->>           NB MAX D'ITERATIONS DEPASSE  -->> FIN
C                    MESSAGE D'ALARME - ON POURSUIT AVEC RESO STANDARD
                     CALL CODENT(ITER3,'G',CITER)
                     CALL CODREE(TOLER,'E',CTOL)
                     CALL CODREE((ERR/CSEC),'E',CERR)
                      VALK(1) = CITER
                      VALK(2) = CERR
                      VALK(3) = CTOL
                      CALL U2MESK('A','ALGORITH4_74', 3 ,VALK)
                  ENDIF
               ELSE
C     -->>        NOUVELLE ITERATION -->> RETOUR
                  GOTO 3
               ENDIF
            ENDIF
C
C --        VERIFICATION DE L'ETAT FINAL OBTENU
C
            FCOMP3 = COEFBR(1) + COEFAR(1,1) * DPC - FC
            FTRAC3 = COEFBR(2) + COEFAR(2,1) * DPC  - FT0
C
C --        VERIFICATION DE LA SOLUTION
C
            IF((FCOMP3/FCP).GT.EPSI2 .AND. DPC .GT. ZERO) THEN
               CALL U2MESS('A','ALGORITH4_81')
               CONVER = .FALSE.
            ENDIF
C
            IF((FTRAC3/FTP).GT.EPSI2) THEN
               CALL U2MESS('A','ALGORITH4_82')
               CONVER = .FALSE.
            ENDIF
C
C --        CONVERGENCE - MISE A JOUR DES DEFORMATIONS CUMULEES ET
C           CONTRAINTES
C
            IF (CONVER .AND. DPC .GT. ZERO) THEN
               VINF(1) = VIND(1) + DPC
               VINF(2) = VIND(2) + DPT
               VINF (NVI) = 1.D0 * NESSAI
               CALL BETINC ( MATERF, NMAT, SIGE, NESSAI, DPC,
     &                       DPT, SIGF, VERIFC, VERIFT )
               IF (VERIFC .GT. ZERO .OR. NSEUIL .GT. 4) THEN
                  NPROJS = NESSAI
               ENDIF
            ENDIF
C
         ENDIF
C
         IF (NPROJS .GT. 0) THEN
            NSEUIL = NPROJS
         ELSE
            NSEUIL = 44
         ENDIF
      ELSE
C-----------------------------------------------------------------------
C --     RESOLUTION STANDARD : INTEGRATION ELASTO-PLASTIQUE DE LA LOI
C --     DE COMPORTEMENT BETON DANS LES CAS OU NSEUIL = 1, 2 OU 3
C-----------------------------------------------------------------------
C
C --     CALCUL DES ECROUISSAGES ET DERIVES DES COURBES D'ADOUCISSEMENT
C
         OSCI = 0
         FCOMP1 = ZERO
         FTRAC1 = ZERO
         PC  = VIND(1)
         PT  = VIND(2)
         CALL BETFPP ( MATERF, NMAT, ELGEOM, PC, PT, NSEUIL, FC, FT,
     &                 DFCDLC, DFTDLT, KUC, KUT, KE)
C
C --     CALCUL DES VALEURS DES CRITERES
C
         DPC = ZERO
         DPT = ZERO
         FCOMP = COEFB(1) + COEFA(1,1) * DPC + COEFA(1,2) * DPT - FC
         FTRAC = COEFB(2) + COEFA(2,1) * DPC + COEFA(2,2) * DPT - FT
C
C --     DEBUT DES ITERATIONS DE L'ALGORITHME DE NEWTON
C
         ITER4 = 0
 4       CONTINUE
         ITER4 = ITER4 + 1
         DDPC0 = DDPC
         DDPT0 = DDPT
C
         IF (OSCI.EQ.0) THEN
C
C --        CALCUL DU JACOBIEN
C
            JAC(1,1) = COEFA(1,1) - DFCDLC
            JAC(1,2) = COEFA(1,2)
            JAC(2,1) = COEFA(2,1)
            JAC(2,2) = COEFA(2,2) - DFTDLT
C
C --        MISE A JOUR DES MULTIPLICATEURS PLASTIQUES
C --        (RESOLUTION DU SYSTEME LINEAIRE DRDY(DY).DDY = -R(DY) )
C
            E = MATERF(1,1)
            IF(NSEUIL.EQ.1) THEN
               DELTA = JAC(1,1)
               IF(ABS(DELTA/E).LT.EPSI) THEN
                  CALL U2MESS('F','ALGORITH4_83')
               ELSE
                  MDELTA = -1.D0 / DELTA
               ENDIF
               DDPC = MDELTA * FCOMP
               DDPT = ZERO
            ELSE IF(NSEUIL.EQ.2) THEN
               DELTA = JAC(2,2)
               IF(ABS(DELTA/E).LT.EPSI) THEN
                  CALL U2MESS('F','ALGORITH4_83')
               ELSE
                  MDELTA = -1.D0 / DELTA
               ENDIF
               DDPC = ZERO
               DDPT = MDELTA * FTRAC
            ELSE IF(NSEUIL.EQ.3) THEN
               DELTA = (JAC(1,1) * JAC(2,2) - JAC(1,2) * JAC(2,1))
               IF(ABS(DELTA/E).LT.EPSI) THEN
                  CALL U2MESS('F','ALGORITH4_83')
               ELSE
                  MDELTA = -1.D0 / DELTA
               ENDIF
               DDPC = (JAC(2,2) * FCOMP - JAC(1,2) * FTRAC) * MDELTA
               DDPT = (JAC(1,1) * FTRAC - JAC(2,1) * FCOMP) * MDELTA
            ELSE
               CALL U2MESS('A','ALGORITH4_84')
               GOTO 5
            ENDIF
C
C --        EN CAS D'OSCILLATIONS : RESO PAR DICHOTOMIE
            IF(ITER4.GT.7 .AND. NSEUIL.EQ.2) THEN
               IF (ABS(DDPT/DPT).LT.1.D-3 .AND. ABS(DDPT).GT.PRECM)
     &            THEN
                  IF((FTRAC2*FTRAC1).LT.ZERO) THEN
                  OSCI = 20
                  DDPT = -0.5D0 *  DDPT0
                  ENDIF
               ENDIF
            ENDIF
            IF(ITER4.GT.7 .AND. NSEUIL.EQ.1) THEN
               IF (ABS(DDPC/DPC).LT.1.D-3 .AND. ABS(DDPC).GT.PRECM)
     &            THEN
                  IF((FCOMP2*FCOMP1).LT.ZERO) THEN
                  OSCI = 20
                  DDPC = -0.5D0 *  DDPC0
                  ENDIF
               ENDIF
            ENDIF
C
         ELSE
            IF(NSEUIL.EQ.2) THEN
               IF(FTRAC*FTRAC1 .LT. ZERO) THEN
                  DDPT = -0.5D0 * DDPT0
               ELSE
                  FTRAC1 = FTRAC2
                  DDPT = 0.5D0 * DDPT0
               ENDIF
            ENDIF
            IF(NSEUIL.EQ.1) THEN
               IF(FCOMP*FCOMP1 .LT. ZERO) THEN
                  DDPC = -0.5D0 * DDPC0
               ELSE
                  FCOMP1 = FCOMP2
                  DDPC = 0.5D0 * DDPC0
               ENDIF
            ENDIF
         ENDIF
C
C
         DPC  = DPC + DDPC
         DPT  = DPT + DDPT
C
C --     CALCUL DES ECROUISSAGES ET DERIVES DES COURBES D'ADOUCISSEMENT
C
         PC  = VIND(1) + DPC
         PT  = VIND(2) + DPT
         CALL BETFPP ( MATERF, NMAT, ELGEOM, PC, PT, NSEUIL, FC, FT,
     &                 DFCDLC, DFTDLT, KUC, KUT, KE)
C
C --     CALCUL DES VALEURS DES CRITERES
C
         FCOMP2 = FCOMP1
         FCOMP1 = FCOMP
         FTRAC2 = FTRAC1
         FTRAC1 = FTRAC
         FCOMP = COEFB(1) + COEFA(1,1) * DPC + COEFA(1,2) * DPT - FC
         FTRAC = COEFB(2) + COEFA(2,1) * DPC + COEFA(2,2) * DPT - FT
C
C --     TEST DE CONVERGENCE
C
         IF(NSEUIL.EQ.3) THEN
            ERR = ABS(FCOMP/FCP) + ABS(FTRAC/FTP)
            IF(ABS(DDPC/DPC).LT.PRECM.AND.ABS(DDPT/DPT).LT.PRECM)
     &         ITER4 = ITMAX+OSCI
         ELSE IF(NSEUIL.EQ.2) THEN
            ERR = ABS(FTRAC/FTP)
            IF(ABS(DDPT/DPT).LT.PRECM) ITER4 = ITMAX+OSCI
         ELSE IF(NSEUIL.EQ.1) THEN
            ERR = ABS(FCOMP/FCP)
            IF(ABS(DDPC/DPC).LT.PRECM) ITER4 = ITMAX+OSCI
         ELSE
            CALL U2MESS('F','ALGORITH4_85')
         ENDIF
C
C
         IF (ERR .LE. (TOLER * CSEC)) THEN
C     -->>     CONVERGENCE   -->> FIN
         ELSE
            IF (ITER4 .GE. ITMAX+OSCI) THEN
               IF(NSEUIL.EQ.3) THEN
                  ERR2 = ABS(DDPC/DPC) + ABS(DDPT/DPT)
               ELSE IF(NSEUIL.EQ.2) THEN
                  ERR2 = ABS(DDPT/DPT)
               ELSE IF(NSEUIL.EQ.1) THEN
                  ERR2 = ABS(DDPC/DPC)
               ENDIF
C              SI L'UN DES INCREMENTS DE DEFORMATION EST NEGATIF, ON
C              FAIT UN NOUVEL ESSAI AVEC UNE AUTRE VALEUR DE NSEUIL,
C              APRES PASSAGE DANS BETCVC. IL N'EST DONC PAS NECESSAIRE
C              DE CONVERGER ! ON FORCE LA CONVERGENCE.
C              DANS LE CAS CONTRAIRE :
               IF (DPT .GE. ZERO .AND. DPC .GE. ZERO) THEN
                 IF (ERR2 .LE. TOLER) THEN
C        -->>      NB MAX D'ITERATIONS DEPASSE MAIS TRES FAIBLES
C                  INCREMENTS DE NEWTON - CONVERGENCE   -->> FIN
C                  MESSAGE D'ALARME SI ERR2 EST INSUFFISANT
                   IF (ERR2 .GT. (TOLER*100)) THEN
                     CALL CODENT(ITER4,'G',CITER)
                     CALL CODREE(TOLER,'E',CTOL)
                     CALL CODREE((ERR/CSEC),'E',CERR)
                         VALK(1) = CITER
                         VALK(2) = CERR
                         VALK(3) = CTOL
                         CALL U2MESK('A','ALGORITH4_73', 3 ,VALK)
                   ENDIF
                 ELSE
C     -->>         NB MAX D'ITERATIONS DEPASSE  -->> FIN
                   CALL CODENT(ITER4,'G',CITER)
                   CALL CODREE(TOLER,'E',CTOL)
                   CALL CODREE((ERR/CSEC),'E',CERR)
                    VALK(1) = CITER
                    VALK(2) = CERR
                    VALK(3) = CTOL
                    CALL U2MESK('A','ALGORITH4_74', 3 ,VALK)
                   NSEUIL = 4
                   GOTO 5
                ENDIF
              ENDIF
            ELSE
C     -->>     NOUVELLE ITERATION -->> RETOUR
               GOTO 4
            ENDIF
         ENDIF
C
C --     VERIFICATION DE L'ETAT FINAL OBTENU
C
         FCOMP3 = COEFB(1) + COEFA(1,1) * DPC + COEFA(1,2) * DPT - FC
         FTRAC3 = COEFB(2) + COEFA(2,1) * DPC + COEFA(2,2) * DPT - FT
C
C --     VERIFICATION DE LA SOLUTION
C
         IF(NSEUIL.EQ.3) THEN
            IF((FCOMP3/FCP).GT.EPSI2.AND.(FTRAC3/FTP).GT.EPSI2) THEN
              IF (DPT .GT. ZERO .AND. DPC .GT. ZERO) THEN
                CALL U2MESS('A','ALGORITH4_86')
            GOTO 5
              ENDIF
            ELSE
              IF((FCOMP3/FCP).GT.EPSI2) THEN
                IF (DPT .GT. ZERO .AND. DPC .GT. ZERO) THEN
                  CALL U2MESS('A','ALGORITH4_87')
            GOTO 5
                ENDIF
              ELSEIF((FTRAC3/FTP).GT.EPSI2) THEN
                IF (DPT .GT. ZERO .AND. DPC .GT. ZERO) THEN
                  CALL U2MESS('A','ALGORITH4_88')
            GOTO 5
                ENDIF
              ENDIF
            ENDIF
C
         ELSE IF(NSEUIL.EQ.2) THEN
            IF((FTRAC3/FTP).GT.EPSI2 .AND. DPT .GT. ZERO) THEN
              CALL U2MESS('A','ALGORITH4_89')
            GOTO 5
             ENDIF
         ELSE IF(NSEUIL.EQ.1) THEN
            IF((FCOMP3/FCP).GT.EPSI2 .AND. DPC .GT. ZERO) THEN
              CALL U2MESS('A','ALGORITH4_90')
            GOTO 5
            ENDIF
         ENDIF
C
C --     CONVERGENCE - MISE A JOUR DES DEFORMATIONS CUMULEES ET
C        CONTRAINTES
C
         IF(NSEUIL.NE.4.AND.NSEUIL.NE.44) THEN
            VINF(1) = VIND(1) + DPC
            VINF(2) = VIND(2) + DPT
            VINF (NVI) = 1.D0 * NSEUIL
            CALL BETINC ( MATERF, NMAT, SIGE, NSEUIL, DPC, DPT,
     &                    SIGF, VERIFC, VERIFT )
         ENDIF
C
      ENDIF
C
C
C
      IRTETI = 0
      GOTO 9999
 5    CONTINUE
      IRTETI = 1
      GOTO 9999
C
 9999 CONTINUE
      END
