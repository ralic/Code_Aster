      SUBROUTINE LCPLLG( TOLER, ITMAX, MOD, NBMAT, MATER, NR, NVI,
     +                   DEPS, EPSD, SIGD, VIND, SEUIL, ICOMP, SIGF,
     +                   VINF, DEVG, DEVGII, IRTET)
C
      IMPLICIT      NONE
      INTEGER       ITMAX, NBMAT, NR, NVI, ICOMP, IRTET
      REAL*8        TOLER, MATER(NBMAT, 2), DEPS(*), EPSD(*), SIGD(*)
      REAL*8        VIND(*), SIGF(*), VINF(*), SEUIL, DEVG(*),DEVGII
      CHARACTER*8   MOD
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 27/03/2002   AUTEUR CIBHHBC R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C ======================================================================
C --- BUT : LOI DE COMPORTEMENT PLASTIQUE POUR LA MECANIQUE DES ROCHES -
C ------- : D'APRES LA LOI DE LAIGLE -----------------------------------
C ======================================================================
C IN  : TOLER  : VALEUR DE LA TOLERANCE DE CONVERGENCE -----------------
C --- :        : (RESI_INTE_RELA) --------------------------------------
C --- : ITMAX  : NOMBRE D'ITERATIONS MAXIMUM A CONVERGENCE -------------
C --- :        : (ITER_INTE_MAXI) --------------------------------------
C --- : MOD    : TYPE DE MODELISATION ----------------------------------
C --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
C --- : MATER  : PARAMETRES MATERIAU -----------------------------------
C --- : NR     : NOMBRE DE RELATIONS NON LINEAIRES ---------------------
C --- : NVI    : NOMBRE DE VARIABLES INTERNES --------------------------
C --- : DEPS   : ACCROISSEMENTS DE DEFORMATIONS A L'ITERATION COURANTE -
C --- : EPSD   : DEFORMATIONS A L'INSTANT PRECEDENT --------------------
C --- : SIGD   : CONTRAINTES A L'INSTANT PRECEDENT ---------------------
C --- : VIND   : VARIABLES INTERNES A L'INSTANT PRECEDENT --------------
C --- : SEUIL  : VARIABLE SEUIL ELASTIQUE ------------------------------
C --- : ICOMP  : COMPTEUR POUR LE REDECOUPAGE DU PAS DE TEMPS ----------
C OUT : SIGF   : CONTRAINTES A L'INSTANT COURANT -----------------------
C --- : VINF   : VARIABLES INTERNES A L'INSTANT COURANT ----------------
C --- : DEVG   : DEVIATEUR DU TENSEUR G, DIRECTION D'ECOULEMENT --------
C --- : DEVGII : NORME DU DEVIATEUR DE G -------------------------------
C --- : IRTET  : CONTROLE DU REDECOUPAGE DU PAS DE TEMPS ---------------
C ======================================================================
      LOGICAL       PRJSOM, LGLCOV
      INTEGER       II, NDT, NDI, ITER, IRTETI
      REAL*8        SIGE(6), LGLEPS, GAMP, SE(6), SIIE, INVARE, YD(9)
      REAL*8        GAMPS, INVARS, B, S(6), DELTA, TRACE, DY(9), YF(9)
      REAL*8        FITER, DKOOH(6,6), EPSF(6), SIGC, I1, TRACEG, TROIS
      CHARACTER*10  CTOL, CITER
C ======================================================================
C --- INITIALISATION DE PARAMETRE --------------------------------------
C ======================================================================
      PARAMETER       ( TROIS   =  3.0D0   )
      PARAMETER       ( LGLEPS  =  1.0D-8  )
C ======================================================================
      COMMON /TDIM/   NDT , NDI
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- INITIALISATION DES VARIABLES -------------------------------------
C ======================================================================
      IRTETI = 0
      DELTA  = 0.0D0
      GAMP   = VIND  ( 1 )
      SIGC   = MATER (9,2)
      CALL     LCEQVN(NDT,SIGF,SIGE)
      CALL     LCDEVI(SIGE,SE)
      CALL     PSCAL (NDT,SE,SE,SIIE)
      SIIE   = SQRT  (SIIE)
      INVARE = TRACE (NDI,SIGE)
C ======================================================================
C --- INITIALISATION YD = (SIG, GAMP, EPSD(3)) -------------------------
C ======================================================================
      CALL LCEQVN (NDT, SE    , YD       )
      CALL LCEQVN (  1, INVARE, YD(NDT+1))
      CALL LCEQVN (  1, GAMP  , YD(NDT+2))
      CALL LCEQVN (  1, DELTA , YD(NDT+3))
C ======================================================================
C --- CALCUL A PRIORI DE LA PROJECTION AU SOMMET -----------------------
C ======================================================================
      CALL CALCPJ(NDT, NDI, NBMAT, MATER, GAMP, SIGD, SIGE, LGLEPS,
     +            GAMPS, INVARS, B)
C ======================================================================
C --- FAUT-IL FAIRE UNE PROJECTION AU SOMMET DU DOMAINE ? --------------
C ======================================================================
      IF (PRJSOM(NBMAT, MATER, INVARE, INVARS, B, SIIE,
     +                                                'SUPERIEUR')) THEN
C ======================================================================
C --- LA PROJECTION AU SOMMET DU DOMAINE EST RETENUE -------------------
C ======================================================================
         DO 10 II=1,NDT
            SIGF(II) = 0.0D0
 10      CONTINUE
         DO 20 II=1,NDI
            SIGF(II) = INVARS / TROIS
 20      CONTINUE
         CALL LCOPIL ( 'ISOTROPE' , MOD , MATER(1,1) , DKOOH )
         CALL LCPRMV ( DKOOH, SIGF, EPSF    )
         IF (MOD.EQ.'C_PLAN') THEN
            SIGF(3) = 0.0D0
            EPSF(3) = DKOOH(3,1) * SIGF(1) +
     +                DKOOH(3,2) * SIGF(2) +
     +                DKOOH(3,4) * SIGF(4)
         ENDIF
         CALL LCEQVN (  1, GAMPS  , VINF(1) )
         CALL LCEQVN (NDT, EPSF(1), VINF(2) )
         VINF(NVI) = 1.0D0
         IRTETI = 0
      ELSE
C ======================================================================
C --- LA PROJECTION AU SOMMET DU DOMAINE N'EST PAS RETENUE -------------
C ======================================================================
C --- CALCUL INITIAL (ITERATION 0) -------------------------------------
C ======================================================================
         CALL LGLINI(NDT, NDI, YD, NR, NBMAT, MATER, SEUIL, SIGD, DEPS,
     +               DEVG, DEVGII, TRACEG, DY)
         ITER = 0
 1       CONTINUE
C ======================================================================
C --- ITERATION ITER ---------------------------------------------------
C ======================================================================
C --- INCREMENTATION DES VARIABLES -------------------------------------
C ======================================================================
         CALL LCSOVN(NR-1, YD, DY, YF)
C ======================================================================
C --- VERIFICATION DE LA COHERENCE DE GAMP -----------------------------
C ======================================================================
         IF (YF(NR-1).LT.0.0D0) THEN
C ======================================================================
C --- GAMP < 0 ---------------------------------------------------------
C --- PEUT-ON FAIRE UN DECOUPAGE DE L'INCREMENT DE DEPLACEMENT ? -------
C ======================================================================
            IF ( ICOMP .EQ. 0 .OR. ICOMP .EQ. 1 ) THEN
               CALL CODENT(ITER,'G',CITER)
               CALL CODREE(TOLER,'E',CTOL)
               CALL UTMESS ('I','LAIGLE',' ERREUR'//
     +         ' - NON CONVERGENCE A ITERATION MAXI '//CITER//
     +         ' - CONVERGENCE IRREGULIERE & ERREUR > '//CTOL//
     +         ' - REDECOUPAGE DU PAS DE TEMPS')
               IRTETI = 3
               GOTO 100
            ELSE
               CALL CODENT(ITER,'G',CITER)
               CALL CODREE(TOLER,'E',CTOL)
               CALL UTMESS ('S','LAIGLE',' ERREUR'//
     +         ' - NON CONVERGENCE A ITERATION MAXI '//CITER//
     +         ' - CONVERGENCE IRREGULIERE & ERREUR > '//CTOL//
     +         ' - DIMINUER LA TAILLE D INCREMENT')
            ENDIF
         ENDIF
C ======================================================================
         DELTA = DELTA + DY(NR)
         CALL LCEQVN (  1, DELTA , YF(NR))
C ======================================================================
C --- CALCUL DE F A L'ITERATION ITER + 1 -------------------------------
C ======================================================================
         CALL LGLDOM(NBMAT, MATER, NR, YF, FITER)
C ======================================================================
C --- A-T-ON CONVERGE ? ------------------------------------------------
C ======================================================================
         IF (LGLCOV(FITER,SIGC,TOLER)) THEN
C ======================================================================
C --- IL Y A CONVERGENCE -----------------------------------------------
C ======================================================================
C --- MISE A JOUR DES VARIABLES INTERNES -------------------------------
C ======================================================================
            CALL LCEQVN (NDT, YF(1)    , S(1) )
            CALL LCEQVN (  1, YF(NDT+1), I1   )
            CALL LCEQVN (  1, YF(NDT+2), GAMP )
            DO 30 II=1,NDT
               SIGF(II) = S(II)
 30         CONTINUE
            DO 40 II=1,NDI
               SIGF(II) = SIGF(II) + I1/TROIS
 40         CONTINUE
            CALL LCOPIL ( 'ISOTROPE' , MOD , MATER(1,1) , DKOOH )
            CALL LCPRMV ( DKOOH, SIGF, EPSF    )
            IF (MOD.EQ.'C_PLAN') THEN
               SIGF(3) = 0.0D0
               EPSF(3) = DKOOH(3,1) * SIGF(1) +
     +                   DKOOH(3,2) * SIGF(2) +
     +                   DKOOH(3,4) * SIGF(4)
            ENDIF
            CALL LCEQVN (  1, GAMP   , VINF(1) )
            CALL LCEQVN (NDT, EPSF(1), VINF(2) )
            VINF(NVI) = 1.0D0
            IRTETI = 0
         ELSE
C ======================================================================
C --- IL N'Y A PAS CONVERGENCE -----------------------------------------
C ======================================================================
            IF (ITER.LT.ITMAX) THEN
C ======================================================================
C --- LE NOMBRE D'ITERATION MAXIMAL N'A PAS ETE ATTEINT ----------------
C ======================================================================
               ITER = ITER + 1
C ======================================================================
C --- NOUVEAU CALCUL PLASTIQUE -----------------------------------------
C ======================================================================
               CALL LGLITE(NDT, NDI, YF, NR, NBMAT, MATER, FITER,
     +                     DEVG, DEVGII, TRACEG, DY)
               IRTETI = 1
            ELSE
C ======================================================================
C --- ON NE CONVERGE VRAIMENT PAS ! ------------------------------------
C ======================================================================
C --- FAUT-IL PROJETER AU SOMMET DU DOMAINE ? --------------------------
C ======================================================================
               IF (PRJSOM(NBMAT, MATER, INVARE, INVARS, B, SIIE,
     +                                               'INFERIEUR'))  THEN
C ======================================================================
C --- DECOUPAGE
C ======================================================================
                  IF ( ICOMP .EQ. 0 .OR. ICOMP .EQ. 1 ) THEN
                     CALL CODENT(ITER,'G',CITER)
                     CALL CODREE(TOLER,'E',CTOL)
                     CALL UTMESS ('I','LAIGLE',' ERREUR'//
     +               ' - NON CONVERGENCE A ITERATION MAXI '//CITER//
     +               ' - CONVERGENCE IRREGULIERE & ERREUR > '//CTOL//
     +               ' - REDECOUPAGE DU PAS DE TEMPS')
                     IRTETI = 3
                     GOTO 100
                  ELSE
                     CALL CODENT(ITER,'G',CITER)
                     CALL CODREE(TOLER,'E',CTOL)
                     CALL UTMESS ('S','LAIGLE',' ERREUR'//
     +               ' - NON CONVERGENCE A ITERATION MAXI '//CITER//
     +               ' - CONVERGENCE IRREGULIERE & ERREUR > '//CTOL//
     +               ' - DIMINUER LA TAILLE D INCREMENT')
                  ENDIF
C ======================================================================
C --- ON PROJETE AU SOMMET DU DOMAINE ----------------------------------
C ======================================================================
C --- MISE A JOUR DES VARIABLES INTERNES -------------------------------
C ======================================================================
                  DO 50 II=1,NDT
                     SIGF(II) = S(II)
 50               CONTINUE
                  DO 60 II=1,NDI
                     SIGF(II) = INVARS/TROIS
 60               CONTINUE
                  CALL LCOPIL ( 'ISOTROPE' , MOD , MATER(1,1) , DKOOH )
                  CALL LCPRMV ( DKOOH, SIGF, EPSF    )
                  IF (MOD.EQ.'C_PLAN') THEN
                     SIGF(3) = 0.0D0
                     EPSF(3) = DKOOH(3,1) * SIGF(1) +
     +                         DKOOH(3,2) * SIGF(2) +
     +                         DKOOH(3,4) * SIGF(4)
                  ENDIF
                  CALL LCEQVN (  1, GAMPS  , VINF(1) )
                  CALL LCEQVN (NDT, EPSF(1), VINF(2) )
                  VINF(NVI) = 1.0D0
                  IRTETI = 0
               ELSE
C ======================================================================
C --- IL N'Y A PAS CONVERGENCE -----------------------------------------
C --- PEUT-ON FAIRE UN DECOUPAGE DE L'INCREMENT DE DEPLACEMENT ? -------
C ======================================================================
                  IF ( ICOMP .EQ. 0 .OR. ICOMP .EQ. 1 ) THEN
                     CALL CODENT(ITER,'G',CITER)
                     CALL CODREE(TOLER,'E',CTOL)
                     CALL UTMESS ('I','LAIGLE',' ERREUR'//
     +               ' - NON CONVERGENCE A ITERATION MAXI '//CITER//
     +               ' - CONVERGENCE IRREGULIERE & ERREUR > '//CTOL//
     +               ' - REDECOUPAGE DU PAS DE TEMPS')
                     IRTETI = 3
                     GOTO 100
                  ELSE
                     CALL CODENT(ITER,'G',CITER)
                     CALL CODREE(TOLER,'E',CTOL)
                     CALL UTMESS ('S','LAIGLE',' ERREUR'//
     +               ' - NON CONVERGENCE A ITERATION MAXI '//CITER//
     +               ' - CONVERGENCE IRREGULIERE & ERREUR > '//CTOL//
     +               ' - DIMINUER LA TAILLE D INCREMENT')
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         IF (IRTETI.EQ.1) GOTO 1
      ENDIF
 100  CONTINUE
      IF (IRTETI.EQ.3) THEN
         IRTET = 1
      ELSE
         IRTET = 0
      ENDIF
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
