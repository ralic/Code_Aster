      SUBROUTINE LGLCVX (SIG, VIN, NBMAT, MATER, SEUIL)
C
      IMPLICIT    NONE
      INTEGER     NBMAT
      REAL*8      SIG(*), MATER(NBMAT,2), VIN(*), SEUIL
C =================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/03/2002   AUTEUR CIBHHBC R.FERNANDES 
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
C --- BUT : VALEUR SEUIL POUR LE CONVEXE ELASTO-PLASTIQUE ---------
C =================================================================
C IN  : SIG   :  TENSEUR DES CONTRAINTES (ELASTIQUE) A T+DT -------
C --- : VIN   :  VARIABLES INTERNES -------------------------------
C --- : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
C --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
C ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
C ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
C OUT : SEUIL :  VALEUR DE F(S) -----------------------------------
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
      INTEGER       NDT , NDI, JPARA
      REAL*8        DEV(6), INVAR1, SII, PREF, LGLEPS, GAMCJS, SIGC
      REAL*8        GAMP, RCOS3T, RHLODE, RGDEV, RUCPLA
      REAL*8        COS3T, HLODE, GDEV, UCRITP, DOMREV, TRACE
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
      PARECR = '&&LGLCVX.PARECR'
      CALL     WKVECT (PARECR,'V V R',5,JPARA)
C =================================================================
C --- RECUPERATION DES DONNEES MATERIAU ---------------------------
C =================================================================
      SIGC   = MATER( 9,2)
      GAMCJS = MATER(12,2)
      PREF   = MATER(15,2)
C =================================================================
C --- CALCUL DU DEVIATEUR ET DU PREMIER INVARIANT DES CONTRAINTES -
C =================================================================
      CALL     LCDEVI(SIG, DEV)
      INVAR1 = TRACE (NDI, SIG)
C =================================================================
C --- CALCUL DE G(S) ----------------------------------------------
C =================================================================
      CALL     LCPRSC(DEV, DEV, SII)
      SII    = SQRT  (SII)
      RCOS3T = COS3T (NDT, DEV, PREF, LGLEPS)
      RHLODE = HLODE (GAMCJS, RCOS3T)
      RGDEV  = GDEV  (SII   , RHLODE)
C =================================================================
C --- CALCUL DE U(SIG, GAMP) --------------------------------------
C =================================================================
      GAMP   = VIN(1)
      CALL     VARECR(GAMP, NBMAT, MATER, ZR(JPARA))
C =================================================================
C --- SI LE CRITERE PLASTIQUE EST NEGATIF ON REDECOUPE ------------
C =================================================================
      RUCPLA = UCRITP(NBMAT, MATER, ZR(JPARA), RGDEV, INVAR1)
      SEUIL  = DOMREV(GAMCJS, SIGC, ZR(JPARA), RGDEV, RUCPLA)
C =================================================================
C --- DESTRUCTION DES VECTEURS INUTILES ---------------------------
C =================================================================
      CALL     JEDETR(PARECR)
C =================================================================
      CALL JEDEMA ()
C =================================================================
      END
