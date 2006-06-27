      SUBROUTINE EPTHMC (MODELI,NNO,NDIM,NBSIG,NPG,NI,TEMPE,TREF,
     +                   HYDR,SECH,SREF,INSTAN,MATER,OPTION,EPSITH)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 27/06/2006   AUTEUR CIBHHPD L.SALMONA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      EPTHMC   -- CALCUL DES  DEFORMATIONS THERMIQUES+RETRAIT 
C                  AUX POINTS D'INTEGRATION
C                  POUR LES ELEMENTS ISOPARAMETRIQUES
C
C   ARGUMENT        E/S  TYPE         ROLE
C    MODELI         IN     K8       MODELISATION (AXI, FOURIER,...)
C    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
C    NDIM           IN     I        DIMENSION DE L'ELEMENT (2 OU 3)
C    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE
C                                   A L'ELEMENT
C    NPG            IN     I        NOMBRE DE POINTS D'INTEGRATION
C                                   DE L'ELEMENT
C    NI(1)          IN     R        FONCTIONS DE FORME
C    TEMPE(1)       IN     R        TEMPERATURES AUX NOEUDS DE
C                                   L'ELEMENT
C    TREF           IN     R        TEMPERATURE DE REFERENCE
C    HYDR(1)        IN     R        HYDRATATION AUX POINTS DE GAUSS
C    SECH(1)        IN     R        SECHAGE AUX NOEUDS DE L'ELEMENT
C    SREF           IN     R        SECHAGE DE REFERENCE
C    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
C    MATER          IN     I        MATERIAU
C    OPTION         IN     K16      OPTION DE CALCUL
C    EPSITH(1)      OUT    R        DEFORMATIONS THERMIQUES
C                                   AUX POINTS D'INTEGRATION
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           CHARACTER*8  MODELI
           CHARACTER*16 K16BID, OPTION
           REAL*8       NI(1), TEMPE(1), EPSITH(1), HYDR(1), SECH(1)
           REAL*8       INSTAN, SREF
C -----  VARIABLES LOCALES
           REAL*8       EPSTH(6),EPSHY(6),EPSSE(6)
           CHARACTER*16 OPTIO2, OPTIO3
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     -----------------
      ZERO   = 0.0D0
      K16BID = ' '
C
      DO 10 I = 1, NBSIG*NPG
         EPSITH(I) = ZERO
 10   CONTINUE
C
C --- CALCUL DES CONTRAINTES D'ORIGINE THERMIQUE :
C ---  BOUCLE SUR LES POINTS D'INTEGRATION
C      -----------------------------------
      DO 20 IGAU = 1, NPG
C
C  --      TEMPERATURE AU POINT D'INTEGRATION COURANT
C          ------------------------------------------
          TEMPG     = ZERO
          SECHG     = ZERO
          HYDRG     = HYDR(IGAU)
C
          DO 30 I = 1, NNO
             TEMPG     = TEMPG     + NI(I+NNO*(IGAU-1))*TEMPE(I)
             SECHG     = SECHG     + NI(I+NNO*(IGAU-1))*SECH(I)
  30      CONTINUE
C
C  --      CALCUL DES DEFORMATIONS THERMIQUES  AU POINT D'INTEGRATION
C  --      COURANT
C          -------
          CALL EPSTMC(MODELI, TEMPG, TREF, HYDRG, SECHG, SREF, INSTAN,
     &                MATER, K16BID, EPSTH)
C
C  --      DEFORMATIONS THERMIQUES SUR L'ELEMENT
C          -------------------------------------
          DO 40 I = 1, NBSIG
                EPSITH(I+NBSIG*(IGAU-1)) = EPSITH(I+NBSIG*(IGAU-1)) +
     +                                     EPSTH(I)
  40      CONTINUE
C
          OPTIO2 = OPTION(1:9) // '_HYDR'
          CALL EPSTMC(MODELI, TEMPG, TREF, HYDRG, SECHG, SREF,
     &                INSTAN,MATER, OPTIO2, EPSHY)
          OPTIO3 = OPTION(1:9) // '_SECH'
          CALL EPSTMC(MODELI, TEMPG, TREF, HYDRG, SECHG, SREF, 
     &                INSTAN,MATER, OPTIO3, EPSSE)
C
C  --     DEFORMATIONS DE RETRAIT SUR L'ELEMENT
C         -------------------------------------
          DO 50 I = 1, NBSIG
            EPSITH(I+NBSIG*(IGAU-1)) = EPSITH(I+NBSIG*(IGAU-1))
     +                                + EPSHY(I) + EPSSE(I)
  50      CONTINUE

  20  CONTINUE
C
C.============================ FIN DE LA ROUTINE ======================
      END
