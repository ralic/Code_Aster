      SUBROUTINE LGLITE(YF, NBMAT, MATER, F0, DEVG, DEVGII, TRACEG, DY)
C
      IMPLICIT      NONE
      INTEGER       NBMAT
      REAL*8        YF(10), MATER(NBMAT,2), F0
      REAL*8        DEVG(6), DEVGII, TRACEG, DY(10)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 17/06/2003   AUTEUR CIBHHBC R.FERNANDES 
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
C --- BUT : CALCUL DES DIFFERENTS INCREMENTS ---------------------------
C ======================================================================
C IN  : YF     : (SIG, I1, GAMP, EVP, DELTA) A L'INSTANT COURANT -------
C --- : NR     : DIMENSION DE YD ---------------------------------------
C --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
C --- : MATER  : PARAMETRES MATERIAU -----------------------------------
C --- : F0     : VALEUR SEUIL A L'INSTANT 0 ----------------------------
C --- : DEVG   : DEVIATEUR DU TENSEUR G --------------------------------
C --- : DEVGII : NORME DU DEVIATEUR DU TENSEUR G -----------------------
C --- : TRACEG : TRACE DU TENSEUR G ------------------------------------
C OUT : DY     : INCREMENTS (DSIG, DI1, DGAMP, DEVP, DDELTA) -----------
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
      REAL*8        PREF, EPSSIG, GAMCJS, MU, K, SNII, RN, GN, UCN
      REAL*8        RCOS3T, COS3T, HLODE, GDEV, UCRITP
      REAL*8        DFDL, SN(6), INVN, GAMPN, EVPN, DELTAN, Q(6)
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
      PARECR = '&&LGLITE.PARECR'
      DERIVE = '&&LGLITE.DERIVE'
      CALL     WKVECT (PARECR,'V V R',5,JPARA  )
      CALL     WKVECT (DERIVE,'V V R',4,JDERIV )
C ======================================================================
C --- RECUPERATION DE DONNEES ------------------------------------------
C ======================================================================
      MU     = MATER ( 4,1)
      K      = MATER ( 5,1)
      GAMCJS = MATER (12,2)
      PREF   = MATER (15,2)
      CALL     LCEQVN(NDT, YF(1)    , SN(1)  )
      CALL     LCEQVN(  1, YF(NDT+1), INVN   )
      CALL     LCEQVN(  1, YF(NDT+2), GAMPN  )
      CALL     LCEQVN(  1, YF(NDT+3), EVPN   )
      CALL     LCEQVN(  1, YF(NDT+4), DELTAN )
C ======================================================================
C --- CALCUL DES VARIABLES D'ECROUISSAGES ET DE SES DERIVEES -----------
C ======================================================================
      CALL     VARECR (GAMPN, NBMAT, MATER, ZR(JPARA))
      CALL     DERVAR (GAMPN, NBMAT, MATER, ZR(JPARA), ZR(JDERIV))
C ======================================================================
C --- CALCUL DES VARIABLES ELASTIQUES INITIALES ------------------------
C ======================================================================
      CALL     PSCAL (NDT, SN, SN, SNII)
      SNII   = SQRT  (SNII)
      RCOS3T = COS3T (SN, PREF, EPSSIG)
      RN     = HLODE (GAMCJS, RCOS3T)
      GN     = GDEV  (SNII, RN)
      UCN    = UCRITP(NBMAT, MATER, ZR(JPARA), GN, INVN)
C ======================================================================
C --- CALCUL DE Q ------------------------------------------------------
C ======================================================================
      CALL SOLREN(SN, NBMAT, MATER, Q)
C ======================================================================
C --- CALCUL DES DIFFERENTES DERIVEES ----------------------------------
C ======================================================================
      CALL CALCDR(NBMAT, MATER, ZR(JPARA), ZR(JDERIV), GN,
     +            INVN, UCN, Q, DEVG, DEVGII, TRACEG, DFDL)
C ======================================================================
C --- CALCUL DES DIFFERENTS INCREMENTS ---------------------------------
C ======================================================================
      CALL CALCDY(MU, K, F0, DEVG, DEVGII, TRACEG, DFDL, 
     +            DELTAN, DY)
C ======================================================================
C --- DESTRUCTION DES VECTEURS INUTILES --------------------------------
C ======================================================================
      CALL JEDETR(PARECR)
      CALL JEDETR(DERIVE)
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
