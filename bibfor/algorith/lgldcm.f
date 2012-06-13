      SUBROUTINE LGLDCM ( NBMAT, MATER, SIG, VIN)
C
      IMPLICIT    NONE
      INCLUDE 'jeveux.h'
      INTEGER     NBMAT
      REAL*8      SIG(6), MATER(NBMAT,2), VIN(*)
C =================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C =================================================================
C --- BUT : CALCUL DU DOMAINE DE COMPORTEMENT DU MATERIAU ---------
C =================================================================
C IN  : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
C --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
C ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
C ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
C --- : SIG   :  TENSEUR DES CONTRAINTES (ELASTIQUE) A T+DT -------
C OUT : VIN   :  VARIABLES INTERNES -------------------------------
C =================================================================
C =================================================================
      INTEGER       NDT, NDI, JPARA, POSDOM
      REAL*8        GAMP, MUN, ZERO,UN, DEUX, TROIS, QUATRE
      REAL*8        GAMMAE, GAMULT, SIGC, GAMCJS, PREF, LGLEPS
      REAL*8        DEV(6), INVAR1, TRACE, SII, COS3T, RCOS3T
      REAL*8        HLODE, RHLODE, GDEV, RGDEV, RUCPLA, UCRITP, H0
      REAL*8        AGAMP, INDIDC
      CHARACTER*16  PARECR
C =================================================================
C --- INITIALISATION DE PARAMETRES --------------------------------
C =================================================================
      PARAMETER  ( MUN    = -1.0D0  )
      PARAMETER  ( ZERO   =  0.0D0  )
      PARAMETER  ( UN     =  1.0D0  )
      PARAMETER  ( DEUX   =  2.0D0  )
      PARAMETER  ( TROIS  =  3.0D0  )
      PARAMETER  ( QUATRE =  4.0D0  )
      PARAMETER  ( LGLEPS =  1.0D-8 )
C =================================================================
      COMMON /TDIM/   NDT , NDI
C =================================================================
      CALL JEMARQ ()
C =================================================================
C --- INITIALISATION DE PARAMETRES --------------------------------
C --- POSDOM DESIGNE LA POSITION DU DOMAINE DANS LES VARIABLES ----
C --- INTERNES ----------------------------------------------------
C =================================================================
      GAMP   =    VIN(1)
      POSDOM =    3
      GAMULT =    MATER( 1,2)
      GAMMAE =    MATER( 2,2)
      SIGC   =    MATER( 9,2)
      GAMCJS =    MATER(12,2)
      PREF   =    MATER(15,2)
      PARECR = '&&LGLDCM.PARECR'
      CALL        WKVECT (PARECR,'V V R',5,JPARA)
C =================================================================
      IF (GAMP.EQ.ZERO) THEN
C =================================================================
C --- DOMAINES PRE-PIC --------------------------------------------
C =================================================================
C --- CALCUL DE H0 = (1-GAMMA_CJS)**(1/6) -------------------------
C =================================================================
         H0 = HLODE(GAMCJS, MUN)
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
         RCOS3T = COS3T (DEV, PREF, LGLEPS)
         RHLODE = HLODE (GAMCJS, RCOS3T)
         RGDEV  = GDEV  (SII   , RHLODE)
C =================================================================
C --- CALCUL DE U(SIG, GAMP) --------------------------------------
C =================================================================
         CALL     VARECR(GAMP, NBMAT, MATER, ZR(JPARA))
         AGAMP  = ZR(JPARA-1+2)
C =================================================================
C --- SI LE CRITERE PLASTIQUE EST NEGATIF ON REDECOUPE ------------
C =================================================================
         RUCPLA = UCRITP(NBMAT, MATER, ZR(JPARA), RGDEV, INVAR1)
         INDIDC = RGDEV/(SIGC*H0*RUCPLA**AGAMP)
         IF (INDIDC.LT.0.7D0) THEN
            VIN(POSDOM) = ZERO
         ELSE
            VIN(POSDOM) = UN
         ENDIF
      ELSE IF (GAMP.LT.GAMMAE) THEN
         VIN(POSDOM) = DEUX
      ELSE IF (GAMP.LT.GAMULT) THEN
         VIN(POSDOM) = TROIS
      ELSE
         VIN(POSDOM) = QUATRE
      ENDIF
C =================================================================
C --- DESTRUCTION DES VECTEURS INUTILES ---------------------------
C =================================================================
      CALL     JEDETR(PARECR)
C =================================================================
      CALL JEDEMA ()
C =================================================================
      END
