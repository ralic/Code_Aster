      SUBROUTINE LGLDOM (NBMAT, MATER, YF, FITER)
C
      IMPLICIT    NONE
      INTEGER     NBMAT
      REAL*8      MATER(NBMAT,2), YF(10), FITER
C =================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/07/2005   AUTEUR VABHHTS J.PELLET 
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
C =================================================================
C --- BUT : VALEUR DE F POUR LE CONVEXE ELASTO-PLASTIQUE ----------
C =================================================================
C IN  : NBMAT : NOMBRE DE PARAMETRES MATERIAU ---------------------
C --- : MATER : PARAMETRES MATERIAU -------------------------------
C --- : NR    : NOMBRE DE CONDITIONS NON LINEAIRES ----------------
C --- : YF    : INCREMENTS A L'INSTANT COURANT --------------------
C OUT : FITER : VALEUR DE F(S) A L'INSTANT COURANT ----------------
C =================================================================
C --------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------
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
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX -----------
C =================================================================
      INTEGER       NDT, NDI, JPARA
      REAL*8        SN(6), I1N, GAMPN, SNII, LGLEPS, GAMCJS, PREF
      REAL*8        RCOS3T, COS3T, RHLODE, HLODE, RGDEV, GDEV, SIGC
      REAL*8        RUCPLA, UCRITP, DOMREV,DDOT
      CHARACTER*16  PARECR
C =================================================================
C --- INITIALISATION DE PARAMETRES --------------------------------
C =================================================================
      PARAMETER       ( LGLEPS  = 1.0D-8 )
C =================================================================
      COMMON /TDIM/   NDT , NDI
C =================================================================
      CALL JEMARQ ()
C =================================================================
C --- DEFINITIONS -------------------------------------------------
C =================================================================
      PARECR = '&&LGLDOM.PARECR'
      CALL     WKVECT (PARECR,'V V R',5,JPARA)
C =================================================================
C --- RECUPERATION DE DONNEES -------------------------------------
C =================================================================
      SIGC   = MATER( 9,2)
      GAMCJS = MATER(12,2)
      PREF   = MATER(15,2)
      CALL     LCEQVN (NDT, YF(1)    , SN(1) )
      CALL     LCEQVN (  1, YF(NDT+1), I1N   )
      CALL     LCEQVN (  1, YF(NDT+2), GAMPN )
C =================================================================
C --- CALCUL DE G(S) ----------------------------------------------
C =================================================================
      SNII=DDOT(NDT,SN,1,SN,1)
      SNII   = SQRT  (SNII)
      RCOS3T = COS3T (SN, PREF, LGLEPS)
      RHLODE = HLODE (GAMCJS, RCOS3T)
      RGDEV  = GDEV  (SNII  , RHLODE)
C =================================================================
C --- CALCUL DE U(SIG, GAMP) --------------------------------------
C =================================================================
      CALL     VARECR(GAMPN, NBMAT, MATER, ZR(JPARA))
C =================================================================
C --- SI LE CRITERE PLASTIQUE EST NEGATIF ON REDECOUPE ------------
C =================================================================
      RUCPLA = UCRITP(NBMAT, MATER, ZR(JPARA), RGDEV, I1N)
      FITER  = DOMREV(GAMCJS, SIGC, ZR(JPARA), RGDEV, RUCPLA)
C =================================================================
C --- DESTRUCTION DES VECTEURS INUTILES ---------------------------
C =================================================================
      CALL     JEDETR(PARECR)
C =================================================================
      CALL JEDEMA ()
C =================================================================
      END
