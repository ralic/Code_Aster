      SUBROUTINE LGLINI(YD, NBMAT, MATER, F0, SIGD, DEPS,
     +                  DEVG, DEVGII, TRACEG, DY)
C
      IMPLICIT      NONE
      INTEGER       NBMAT
      REAL*8        YD(10), MATER(NBMAT,2), F0, SIGD(6), DEPS(6)
      REAL*8        DEVG(6), DEVGII, TRACEG, DY(10)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 11/07/2005   AUTEUR VABHHTS J.PELLET 
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
C --- BUT : CALCUL INITIAL POUR LE PREMIER MULTIPLICATEUR PLASTIQUE ----
C ======================================================================
C IN  : YD     : (DEVIATEUR,1ER INVAVRIANT,GAMP,EVP,DELTA) ITERATION 0 -
C --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
C --- : MATER  : PARAMETRES MATERIAU -----------------------------------
C --- : F0     : VALEUR SEUIL A L'ITERATION 0 --------------------------
C --- : SIGD   : TENSEUR DES CONTRAINTES A L'INSTANT MOINS -------------
C --- : DEPS   : TENSEUR D'ACCROISSEMENT DES DEFORMATIONS --------------
C OUT : DEVG   : DEVIATEUR DU TENSEUR G --------------------------------
C --- : DEVGII : NORME DU DEVIATEUR DE G -------------------------------
C --- : TRACEG : TRACE DE G --------------------------------------------
C --- : DY     : INCREMENTS (DEVIATEUR, 1ER INVARIANT, GAMP, EVP, DELTA)
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
      LOGICAL       TESTE
      INTEGER       JPARA, JDERIV, II, NDT, NDI
      REAL*8        SE(6), GAMP, DELTA, SIIE, PREF, EPSSIG
      REAL*8        GAMCJS, RCOS3T, COS3T, RE, HLODE, GDEV, GE
      REAL*8        UCRITP, Q(6), VECN(6), IE, R8PREM
      REAL*8        SI(6), INVN, TRACE,DDOT
      CHARACTER*16  PARECR, DERIVE
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARAMETER       ( EPSSIG  = 1.0D-8 )
C ======================================================================
      COMMON /TDIM/   NDT , NDI
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- DEFINITIONS ------------------------------------------------------
C ======================================================================
      PARECR = '&&LGLINI.PARECR'
      DERIVE = '&&LGLINI.DERIVE'
      CALL     WKVECT (PARECR,'V V R',5,JPARA )
      CALL     WKVECT (DERIVE,'V V R',4,JDERIV)
C ======================================================================
C --- INITIALISATION DE DONNEES ----------------------------------------
C ======================================================================
      GAMCJS = MATER(12,2)
      PREF   = MATER(15,2)
      CALL     LCEQVN (NDT, YD(1)    , SE(1) )
      CALL     LCEQVN (  1, YD(NDT+1), IE    )
      CALL     LCEQVN (  1, YD(NDT+2), GAMP  )
      CALL     LCEQVN (  1, YD(NDT+4), DELTA )
C ======================================================================
C --- CALCUL DES VARIABLES D'ECROUISSAGES ------------------------------
C ======================================================================
      CALL     VARECR (GAMP, NBMAT, MATER, ZR(JPARA))
C ======================================================================
C --- CALCUL DES VARIABLES ELASTIQUES INITIALES ------------------------
C ======================================================================
      SIIE=DDOT(NDT,SE,1,SE,1)
      SIIE   = SQRT  (SIIE)
      RCOS3T = COS3T (SE, PREF, EPSSIG)
      RE     = HLODE (GAMCJS, RCOS3T)
      GE     = GDEV  (SIIE, RE)
C ======================================================================
C --- CALCUL DE Q ET DE N ----------------------------------------------
C ======================================================================
      TESTE = .FALSE.
      DO 10 II=1,NDT
         IF (ABS(SIGD(II)).GT.EPSSIG) TESTE = .TRUE.
 10   CONTINUE
      IF (TESTE) THEN
         CALL   LCDEVI(SIGD,SI)
         INVN = TRACE (NDI,SIGD)
         CALL   SOLREI(GAMP, SI, INVN, ZR(JPARA), NBMAT,
     +                 MATER, Q, VECN)
      ELSE
         CALL   SOLREI(GAMP, SE, IE,   ZR(JPARA), NBMAT,
     +                 MATER, Q, VECN)
      ENDIF
C ======================================================================
C --- INITIALISATION ---------------------------------------------------
C ======================================================================
      IF (GAMP.LT.R8PREM()) THEN
C ======================================================================
C --- PREMIERE INITIALISATION POUR GAMP = 0 ----------------------------
C ======================================================================
         CALL LGLIND(NBMAT, MATER, ZR(JPARA), GE, Q,
     +               VECN, DEPS, DEVG, DEVGII, TRACEG, DY)
      ELSE
C ======================================================================
C --- INITIALISATION DE NEWTON -----------------------------------------
C ======================================================================
         CALL DERVAR(GAMP, NBMAT, MATER, ZR(JPARA), ZR(JDERIV))
         CALL LGLINN(NBMAT, MATER, ZR(JPARA),
     +               ZR(JDERIV), GE, IE, Q, VECN, F0, DELTA, DEVG,
     +               DEVGII, TRACEG, DY)
      ENDIF
C ======================================================================
C --- DESTRUCTION DES VECTEURS INUTILES --------------------------------
C ======================================================================
      CALL JEDETR(PARECR)
      CALL JEDETR(DERIVE)
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
