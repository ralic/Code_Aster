      SUBROUTINE  D1MAMC(MODELI,MATER,TEMPE,INSTAN,REPERE,XYZGAU,
     +                   NBSIG,D1)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 07/07/98   AUTEUR CIBHHGB G.BERTRAND 
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
C      D1MAMC :   CALCUL DE L'INVERSE DE LA MATRICE DE HOOKE 
C                 POUR LES ELEMENTS ISOPARAMETRIQUES POUR DES 
C                 MATERIAUX ISOTROPE, ORTHOTROPE ET ISOTROPE TRANSVERSE
C
C   ARGUMENT        E/S  TYPE         ROLE
C    MODELI         IN     K8       MODELISATION (AXI,FOURIER,...)
C    MATER          IN     I        MATERIAU
C    TEMPE          IN     R        TEMPERATURE AU POINT D'INTEGRATION
C    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
C    REPERE(7)      IN     R        VALEURS DEFINISSANT LE REPERE
C                                   D'ORTHOTROPIE
C    XYZGAU(3)      IN     R        COORDONNEES DU POINT D'INTEGRATION
C    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE A
C                                   L'ELEMENT
C    D1(NBSIG,1)    OUT    R        MATRICE DE HOOKE
C
C
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           CHARACTER*8  MODELI
           REAL*8       REPERE(7), XYZGAU(1), D1(NBSIG,1), INSTAN
C
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C       ------------------------
C ----  CAS MASSIF 3D ET FOURIER
C       ------------------------
      IF (MODELI(1:2).EQ.'CA'.OR.MODELI(1:2).EQ.'FO') THEN
C
          CALL D1MA3D(MATER,TEMPE,INSTAN,REPERE,XYZGAU,D1)
C
C       ----------------------------------------
C ----  CAS DEFORMATIONS PLANES ET AXISYMETRIQUE
C       ----------------------------------------
      ELSEIF (MODELI(1:2).EQ.'DP'.OR.MODELI(1:2).EQ.'AX') THEN
C
          CALL D1MADP(MATER,TEMPE,INSTAN,REPERE,D1)
C
C       ----------------------
C ----  CAS CONTRAINTES PLANES
C       ----------------------
      ELSEIF (MODELI(1:2).EQ.'CP') THEN
C
          CALL D1MACP(MATER,TEMPE,INSTAN,REPERE,D1)
C
      ELSE
         CALL UTMESS('F','D1MAMC','LA MODELISATION : '//MODELI//
     +               'N''EST PAS TRAITEE.')
      ENDIF
C.============================ FIN DE LA ROUTINE ======================
      END
