      SUBROUTINE CALCPJ(NBMAT, MATER, GAMP, EVP, SIGD, SIGE, 
     +                  EPSSIG, INVARE, GAMPS, INVARS, EVPS, B)
C
      IMPLICIT      NONE
      INTEGER       NBMAT
      REAL*8        MATER(NBMAT,2), GAMP, EVP, SIGD(6), SIGE(6), EPSSIG
      REAL*8        INVARE, GAMPS, INVARS, EVPS, B
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
C --- BUT : CALCUL DE LA PROJECTION AU SOMMET --------------------------
C ======================================================================
C IN  : NDT    : NOMBRE DE COMPOSANTES TOTALES DU TENSEUR --------------
C --- : NDI    : NOMBRE DE COMPOSANTES DIAGONALES DU TENSEUR -----------
C --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
C --- : MATER  : PARAMETRES MATERIAU -----------------------------------
C --- : GAMP   : DEFORMATION DEVIATOIRE PLASTIQUE CUMULEE --------------
C --- : EVP    : DEFORMATION VOLUMIQUE PLASTIQUE CUMULEE ---------------
C --- : SIIE   : NORME DU TENSEUR --------------------------------------
C --- : EPSSIG : EPSILON -----------------------------------------------
C --- : INVARE : PREMIER INVARIANT DU TENSEUR DES CONTRAINTES ELASTIQUE-
C OUT : GAMPS  : DEFORMATION DEVIATOIRE PLASTIQUE CUMULEE AU SOMMET ----
C --- : INVARS : PREMIER INVARIANT DU TENSEUR DES CONTRAINTES AU SOMMET-
C --- : EVPS   : DEFORMATION VOLUMIQUE PLASTIQUE CUMULEE AU SOMMET -----
C --- : B      : PARAMETRE CONTROLANT LE COMPORTEMENT VOLUMIQUE --------
C ------------ : DU MATERIAU -------------------------------------------
C ======================================================================
C --------------- DEBUT DECLARATIONS NORMALISEES JEVEUX ---------------
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
      INTEGER       JPARA, JPARA2, NDT, NDI
      REAL*8        MU, SIGC, SIG(6), SD(6), SGAMP, MGAMP, BPRIME
      REAL*8        ZERO, UN, DEUX, TROIS, SE(6), TRACE
      REAL*8        SIGII, SIIE, INVAR, EPSULT, GAMULT, K
      CHARACTER*16  PARECR, PAREC2
C ======================================================================
C --- INITIALISATION DE PARAMETRE --------------------------------------
C ======================================================================
      PARAMETER       ( ZERO     =  0.0D0   )
      PARAMETER       ( UN       =  1.0D0   )
      PARAMETER       ( DEUX     =  2.0D0   )
      PARAMETER       ( TROIS    =  3.0D0   )
      PARAMETER       ( EPSULT   =  1.0D-03 )
C ======================================================================
      COMMON /TDIM/   NDT , NDI
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- RECUPERATION DE PARAMETRES DU MODELE -----------------------------
C ======================================================================
      MU     = MATER(4,1)
      K      = MATER(5,1)
      GAMULT = MATER(1,2)
      SIGC   = MATER(9,2)
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARECR = '&&CALCPJ.PARECR'
      PAREC2 = '&&CALCPJ.PAREC2'
      CALL     WKVECT(PARECR,'V V R',5,JPARA )
      CALL     WKVECT(PAREC2,'V V R',5,JPARA2)
C ======================================================================
C --- CALCUL DES PROJECTIONS AU SOMMET ---------------------------------
C ======================================================================
      CALL     LCDEVI(SIGE,SE)
      CALL     PSCAL (NDT,SE,SE,SIIE)
      SIIE   = SQRT  (SIIE)
      GAMPS  = GAMP + SQRT(DEUX/TROIS)*SIIE/(DEUX*MU)
      CALL     VARECR(GAMPS, NBMAT, MATER, ZR(JPARA))
      SGAMP  = ZR(JPARA-1+1)
      MGAMP  = ZR(JPARA-1+4)
      INVARS = TROIS*SIGC*SGAMP/MGAMP
      EVPS   = EVP + (INVARE - INVARS)/(TROIS * K)
C ======================================================================
C --- CALCUL DU PARAMETRE DE DILATANCE B A L'INSTANT MOINS -------------
C ======================================================================
C --- CAS OU GAMP > GAMULT(1-EPS) --------------------------------------
C ======================================================================
      IF (GAMP.GT.(GAMULT*(UN-EPSULT))) THEN
         B = ZERO
      ELSE
C ======================================================================
C --- CAS OU GAMP <= GAMULT(1-EPS) -------------------------------------
C ======================================================================
         CALL     PSCAL (NDT,SIGD,SIGD,SIGII)
         IF (SIGII.LT.EPSSIG) THEN
            CALL  LCEQVN(NDT,SIGE,SIG)
         ELSE
            CALL  LCEQVN(NDT,SIGD,SIG)
         ENDIF
         CALL     LCDEVI(SIG,SD)
         INVAR  = TRACE (NDI,SIG)
         CALL     VARECR(GAMP, NBMAT, MATER, ZR(JPARA2))
         B      = BPRIME(NBMAT,MATER,ZR(JPARA2),INVAR,SD,EPSSIG)
      ENDIF
C ======================================================================
C --- DESTRUCTION DES VECTEURS INUTILES --------------------------------
C ======================================================================
      CALL JEDETR(PARECR)
      CALL JEDETR(PAREC2)
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
