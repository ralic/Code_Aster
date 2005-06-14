      SUBROUTINE LCHOBR(TOLER, ITMAX, MOD, NBMAT, MATERF, NR, NVI,
     1                 DEPSM, SIGM, VIM, SEUIL, VP, VECP, ICOMP, SIGP,
     2                 VIP, IRTET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/06/2005   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
      IMPLICIT      NONE
      INTEGER       ITMAX, NBMAT, NR, NVI, ICOMP, IRTET
      REAL*8        TOLER, MATERF(NBMAT, 2), DEPSM(6), SIGM(6)
      REAL*8        VIM(*), SIGP(6), VIP(*), SEUIL, VP(3), VECP(3,3)
      CHARACTER*8   MOD
C ======================================================================
C --- LOI DE COMPORTEMENT DE TYPE HOEK BROWN MODIFIE -------------------
C --- *ELASTICITE ISOTROPE ---------------------------------------------
C --- *CRITERE DE PLASTICITE DE HEOK BROWN -----------------------------
C --- *ECOULEMENT PLASTIQUE DE DRUCKER PRAGER --------------------------
C --- CALCUL DU TENSEUR DES CONTRAINTES DANS LE CAS PLASTIQUE ----------
C ======================================================================
C IN  : TOLER  : VALEUR DE LA TOLERANCE DE CONVERGENCE -----------------
C --- :        : (RESI_INTE_RELA) --------------------------------------
C --- : ITMAX  : NOMBRE D'ITERATIONS MAXIMUM A CONVERGENCE -------------
C --- :        : (ITER_INTE_MAXI) --------------------------------------
C --- : MOD    : TYPE DE MODELISATION ----------------------------------
C --- : NBMAT  : NOMBRE DE DONNEES MATERIAU ----------------------------
C --- : MATERF : DONNEES MATERIAU --------------------------------------
C --- : NR     : NOMBRE DE RELATIONS NON LINEAIRES ---------------------
C --- : NVI    : NOMBRE DE VARIABLES INTERNES --------------------------
C --- : DEPSM  : ACCROISSEMENTS DE DEFORMATIONS A L'ITERATION COURANTE -
C --- : SIGM   : CONTRAINTES A L'INSTANT PRECEDENT ---------------------
C --- : VIM    : VARIABLES INTERNES A L'INSTANT PRECEDENT --------------
C --- : SEUIL  : VARIABLE SEUIL ELASTIQUE ------------------------------
C --- : VP     : VALEURS PROPRES DU DEVIATEUR ELASTIQUE ----------------
C --- : VECP   : VECTEURS PROPRES DU DEVIATEUR ELASTIQUE ---------------
C --- : ICOMP  : COMPTEUR POUR LE REDECOUPAGE DU PAS DE TEMPS ----------
C OUT : SIGP   : CONTRAINTES A L'INSTANT COURANT -----------------------
C --- : VIP    : VARIABLES INTERNES A L'INSTANT COURANT ----------------
C --- : IRTET  : CONTROLE DU REDECOUPAGE DU PAS DE TEMPS ---------------
C ======================================================================
C     INFO   MATERF   COEFFICIENTS MATERIAUX A T+DT
C                     MATERF(*,1) = CARACTERISTIQUES ELASTIQUES
C                     MATERF(*,2) = CARACTERISTIQUES PLASTIQUES
C            NDT      NOMBRE DE COMPOSANTES TOTALES DES TENSEURS
C            NDI      NOMBRE DE COMPOSANTES DIRECTES DES TENSEURS
C            NVI      NOMBRE DE VARIABLES INTERNES 
C ======================================================================
      INTEGER      NDT,NDI,II,ITERI,ITER,JJ
      REAL*8       GM,ETAM,ETAP,AUX,SIG3,MU,K,AUX2,AUX3
      REAL*8       EPSM(6),EPSP(6),SIGE(6),SE(6),SEB(6)
      REAL*8       TOLER2,SEQ,I1E,SEUIL2,PLAS,DG,SIGEQE
      REAL*8       HOOKF(6,6),DKOOH(6,6),DEUX,TROIS,TRACE
      REAL*8       INCRG,GNP,DGNP,ETANP,VH,VG
      REAL*8       PARAME(4),DERIVE(5),PI,R8PI,FMOINS
      CHARACTER*10 CVP1,CVP2,CVP3
C ======================================================================
      PARAMETER       ( DEUX   =  2.0D0  )
      PARAMETER       ( TROIS  =  3.0D0  )
C ======================================================================
      COMMON /TDIM/   NDT, NDI
C ======================================================================
C --- INITIALISATION DES PARAMETRES DE CONVERGENCE ---------------------
C ======================================================================
      TOLER2 = 1.0D-6
      PI     = R8PI()
      PI     = PI/180.D0
C ======================================================================
C --- INITIALISATION ---------------------------------------------------
C ======================================================================
      GM = VIM(1)
      IF (GM.LT.0.0D0) THEN
         CALL UTMESS('F','LCHOBR','GM NEGATIF')
      ENDIF
      IRTET = 0
      ITERI = 0
C =====================================================================
C --- CALCUL DES PARAMETRES D ECROUISSAGE -----------------------------
C =====================================================================
      CALL HBVAEC(GM,NBMAT,MATERF,PARAME)
      ETAM = DEUX*SIN(PARAME(4)*PI)/(TROIS+SIN(PARAME(4)*PI))
C =====================================================================
      CALL LCDEVI(SIGP,SE)
      CALL PSCAL(NDT,SE,SE,SEQ)
      SIGEQE = SQRT(TROIS*SEQ/DEUX) 
      I1E    = TRACE(NDI,SIGP)
C ======================================================================
      DG = 0.0D0
C ======================================================================
C --- CALCUL DE DELTA GAMMA --------------------------------------------
C ======================================================================
      PLAS = 1.0D0
      CALL HBCREL(VP,GM,DG,NBMAT,MATERF,SIGEQE,I1E,ETAM,PARAME,SEUIL2)
      FMOINS = SEUIL2    
C ======================================================================
C --------- CALCUL DE L INCREMENT DE GAMMA PAR METHODE DE NEWTON -------
C ======================================================================
C --------- INITIALISATION DES VARIABLES -------------------------------
C ======================================================================
      ITER   = 0
      INCRG  = 0.D0
      DGNP   = DG
      GNP    = GM
      ETANP  = ETAM
      CALL CALCVH(NBMAT,MATERF,ETANP,VP,SIGEQE,VH,VG)
       CALL HBDERI(GNP,NBMAT,MATERF,VG,ETANP,PARAME,DERIVE)
C ======================================================================
C --------- PREMIERE ITERATION -----------------------------------------
C ======================================================================
         CALL HBCALC(SEUIL2,GNP,DGNP,NBMAT,MATERF,I1E,SIGEQE,VP,ETANP,
     &           VH,VG,PARAME,DERIVE,INCRG)
 2       CONTINUE 
         GNP    = GNP + INCRG
         DGNP   = DGNP + INCRG
C ======================================================================
C -- ON OBTIENT DGAMMA_P NEGATIF : ON ESSAIE DE DECOUPER LE PAS DE TEMPS
C ======================================================================
         IF (DGNP.LT.0.D0) THEN
            IF ((ICOMP.EQ.0) .OR. (ICOMP.EQ.1)) THEN
               CALL UTMESS('I','LCHOBR','ERREUR: 
     &               PB DE CONVERGENCE (DGP NEG)')
               ITERI = 1
               GOTO 100
            ELSE
               CALL UTMESS('I','LCHOBR','ERREUR: 
     &                 PB DE CONVERGENCE 2 (DGP NEG)')
               GOTO 100
            ENDIF
         ENDIF
         CALL HBVAEC(GNP,NBMAT,MATERF,PARAME)
         ETANP = DEUX*SIN(PARAME(4)*PI)/(TROIS+SIN(PARAME(4)*PI))
         CALL HBCREL(VP,GNP,DGNP,NBMAT,MATERF,SIGEQE,I1E,ETANP,
     &           PARAME,SEUIL2)
C ======================================================================
C ---------- IL Y A CONVERGENCE ----------------------------------------
C ======================================================================
         IF ((ABS(SEUIL2).LT.TOLER2).OR.
     &             (ABS(SEUIL2/FMOINS).LT.TOLER2)) THEN
C ======================================================================
C --------- ON DETECTE LES SOLUTIONS NON ADMISSIBLES -------------------
C ======================================================================
            AUX = SIGEQE*(ETANP+1.0D0)/(TROIS*MATERF(4,1))
            IF (DGNP.GT.AUX) THEN
              CALL UTMESS('I','LCHOBR','ERREUR: 
     &                 PAS DE SOLUTION')
              ITERI = 1
              GOTO 100
            ENDIF
            DG    = DGNP
            ITERI = 0
C ======================================================================
C --------- LE NOMBRE MAX D ITERATIONS N A PAS ETE ATTEINT -------------
C ======================================================================
         ELSE IF (ITER.LT.ITMAX) THEN
            ITER  = ITER + 1
            ITERI = 0
            CALL CALCVH(NBMAT,MATERF,ETANP,VP,SIGEQE,VH,VG)
            CALL HBDERI(GNP,NBMAT,MATERF,VG,ETANP,PARAME,DERIVE)
           CALL HBCALC(SEUIL2,GNP,DGNP,NBMAT,MATERF,I1E,SIGEQE,VP,ETANP,
     &           VH,VG,PARAME,DERIVE,INCRG)
            GOTO 2
C ======================================================================
C --------- LE NOMBRE MAX D ITERATIONS A ETE ATTEINT -------------------
C ======================================================================
         ELSE
C ======================================================================
C --------- ON ESSAIE DE DECOUPER LE PAS DE TEMPS ----------------------
C ======================================================================
            IF ((ICOMP.EQ.0) .OR. (ICOMP.EQ.1)) THEN
               CALL UTMESS('I','LCHOBR','ERREUR: PB DE CONVERGENCE')
               ITERI = 1
               GOTO 100
            ELSE
               CALL UTMESS('F','LBHOBR','ERREUR: PB DE CONV 2')
            ENDIF        
         ENDIF
 100     CONTINUE   
         IF (ITERI.GE.1) GOTO (1),ITERI
C ======================================================================
         ETAP = ETANP
         CALL HBMAJS(DG,NBMAT,MATERF,SE,I1E,SIGEQE,ETAP,SIGP)
         VIP(1) = VIM(1) + DG
         VIP(2) = VIM(2) + TROIS*ETAP*DG/(ETAP+1.0D0)
         VIP(3) = PLAS
C ======================================================================
      IRTET = 0
      GOTO 9999
 1    CONTINUE
      IRTET = 1
 9999 CONTINUE            
C ======================================================================
      END
