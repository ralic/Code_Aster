      SUBROUTINE LGLJPL(MOD,NBMAT,MATER,SIG,DEVG,DEVGII,VIN,DSDE,CODRET)
C
      IMPLICIT      NONE
      INTEGER       NBMAT,CODRET
      REAL*8        MATER(NBMAT,2), SIG(6), VIN(*), DSDE(6,6)
      REAL*8        DEVG(6), DEVGII
      CHARACTER*8   MOD
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 27/10/2009   AUTEUR FERNANDES R.FERNANDES 
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
C --- BUT : CALCUL DE DSIG/DEPS ----------------------------------------
C ======================================================================
C IN  : MOD    : TYPE DE MODELISATION ----------------------------------
C --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
C --- : MATER  : PARAMETRES MATERIAU -----------------------------------
C --- : SIG    : TENSEUR DES CONTRAINTES -------------------------------
C --- : DEVG   : DEVIATEUR DU TENSEUR G --------------------------------
C --- : DEVGII : NORME DU DEVIATEUR DE G -------------------------------
C --- : VIN    : VARIABLES INTERNES ------------------------------------
C OUT : DSDE   : DSIG/DEPS ---------------------------------------------
C ======================================================================
C --------------- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C ======================================================================
      INTEGER       JPARA, JDERIV, NDT, NDI
      REAL*8        EPSSIG, SIGC, GAMCJS, PREF, SN(6), SNII, INVN, H0
      REAL*8        MUN, HLODE, GAMPN, RCOS3T, COS3T, RN, GN, GDEV
      REAL*8        UCRITP, DUDS(6), DUDG, DFDS(6), DFDG, TRACE
      REAL*8        Q(6), HOOK(6,6),DDOT
      CHARACTER*16  PARECR, DERIVE
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARAMETER       ( EPSSIG  =  1.0D-8 )
      PARAMETER       ( MUN     = -1.0D0  )
C ======================================================================
      COMMON /TDIM/   NDT , NDI
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- DEFINITIONS ------------------------------------------------------
C ======================================================================
      PARECR = '&&LGLJPL.PARECR'
      DERIVE = '&&LGLJPL.DERIVE'
      CALL     WKVECT(PARECR,'V V R',5,JPARA )
      CALL     WKVECT(DERIVE,'V V R',4,JDERIV)
      CALL     LCINMA ( 0.0D0, HOOK )
      CALL     LCINMA ( 0.0D0, DSDE )
C ======================================================================
C --- RECUPERATION DE PARAMETRES MATERIAU ------------------------------
C ======================================================================
      SIGC   = MATER ( 9,2)
      GAMCJS = MATER (12,2)
      PREF   = MATER (15,2)
      CALL     LCEQVN(1, VIN(1) , GAMPN)
C ======================================================================
C --- RECUPERATION DE LA MATRICE DE HOOK -------------------------------
C ======================================================================
      CALL LCOPLI ( 'ISOTROPE' , MOD , MATER(1,1) , HOOK )
C ======================================================================
C --- CALCULS INITIAUX DE VARIABLES INTERMEDIAIRES ---------------------
C ======================================================================
      CALL     LCDEVI(SIG, SN)
      SNII=DDOT(NDT,SN,1,SN,1)
      SNII   = SQRT  (SNII)
      INVN   = TRACE (NDI, SIG)
      H0     = HLODE (GAMCJS, MUN)
C ======================================================================
C --- CALCULS DES VARIABLES D'ECROUISSAGES ET DE SES DERIVEES ----------
C ======================================================================
      CALL     VARECR(GAMPN, NBMAT, MATER, ZR(JPARA))
      CALL     DERVAR(GAMPN, NBMAT, MATER, ZR(JPARA), ZR(JDERIV))
C ======================================================================
C --- CALCUL DES VARIABLES INITIALES -----------------------------------
C ======================================================================
      RCOS3T = COS3T (SN, PREF, EPSSIG)
      RN     = HLODE (GAMCJS, RCOS3T)
      GN     = GDEV  (SNII, RN)
C ======================================================================
C --- CALCUL DE Q A L'ITERATION COURANTE -------------------------------
C ======================================================================
      CALL SOLREN(SN, NBMAT, MATER, Q, CODRET)
      IF (CODRET.NE.0) GOTO 100
C ======================================================================
C --- CALCUL DES DIFFERENTES DERIVEES ----------------------------------
C ======================================================================
C **********************************************************************
C --- CALCUL DE DUDS ---------------------------------------------------
C **********************************************************************
      CALL DRUDRS(ZR(JPARA), Q, H0, SIGC, DUDS)
C **********************************************************************
C --- CALCUL DE DUDG ---------------------------------------------------
C **********************************************************************
      CALL DRUDRG(ZR(JPARA), ZR(JDERIV), H0, SIGC, GN, INVN, DUDG)
C **********************************************************************
C --- CALCUL DE DFDS ---------------------------------------------------
C **********************************************************************
      CALL DRFDRS(Q, ZR(JPARA), H0, SIGC, GN, DUDS, DFDS)
C **********************************************************************
C --- CALCUL DE DFDG ---------------------------------------------------
C **********************************************************************
      CALL DRFDRG(ZR(JPARA), ZR(JDERIV), H0, SIGC, GN, DUDG, DFDG)
C **********************************************************************
C ======================================================================
C --- CALCUL DE DSIG/DEPS ----------------------------------------------
C ======================================================================
      CALL CALCDS(HOOK, DEVG, DEVGII, DFDS, DFDG, DSDE)
C ======================================================================
C --- DESTRUCTION DES VECTEURS INUTILES --------------------------------
C ======================================================================
 100  CONTINUE
      CALL JEDETR(PARECR)
      CALL JEDETR(DERIVE)
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
