      SUBROUTINE VFRTW(NPT,E,K,VFTW)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER NPT
      REAL*8 E,K
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 06/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C .  - FONCTION REALISEE:  CALCUL DES FONCTIONS DE FORME POUR LA FLECHE.
C .     POUR L ELEMENT DE MORLEY AUX POINTS DE GAUSS D UNE FAMILLE     .
C .     (APPELEE DANS INI098 )                                         .
C .                                                                    .
C .  - ARGUMENTS:                                                      .
C .      DONNEES:          NPT      -->  NOMBRE DE POINTS DU TRIANGLE  .
C .                        E,K      -->  COORDONNEES DU POINT DE GAUSS .
C .                                                                    .
C .      RESULTATS:                                                   .
C .                        VFTW     <--  VALEUR DES FONCTIONS DE FORME .
C .                                      AU POINT DE GAUSS CONSIDERE   .
C ......................................................................
C
C     VALEUR SUIVANT ETA ET KSI DES FONCTIONS DE
C   FORME DE W(ELEMENT DE MORLEY)
C
C     DESCRIPTION DE LA NUMEROTATION DU TRIANGLE
C
C                    ! KSI
C                    !
C                    +1
C                    +  +
C                    +    +
C                    +4     +6
C                    +        +
C                    +2    5    +3
C                    +++++++++++++----> ETA
C
      REAL*8 VFTW(6)
C
      VFTW(1) = (1.D0-E+K-((E**2 - K**2)/2.D0)-E*K)/4.D0
      VFTW(2) = (1.D0+E*K)/2.D0
      VFTW(3) = (1.D0+E-K+((E**2 - K**2)/2.D0)-E*K)/4.D0
      VFTW(4) = (E**2 -1.D0)/2.D0
      VFTW(5) = (K**2 -1.D0)/2.D0
      VFTW(6) = SQRT(2.D0)*((E+K+E*K+((E**2 +K**2)/2.D0))/2.D0)
C
      END
