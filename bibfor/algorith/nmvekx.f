      SUBROUTINE NMVEKX(IMATE, TP, XHI, NB,KXHI, DKXIDX) 
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/09/98   AUTEUR SABMTEC P.LACLERGUE 
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
C ----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER IMATE, NB
      REAL*8  TP, XHI, KXHI, DKXIDX

C ----------------------------------------------------------------------
C     INTEGRATION DE LA LOI DE COMPORTEMENT VISCO PLASTIQUE DE
C     CHABOCHE AVEC ENDOMAGEMENT
C     METHODE ITERATIVE D'EULER IMPLICITE
C
C     CALCUL DE LA CARACTERISTIQUE K DU MATERIAU FONCTION DE TEMPE+ ET
C     DE SIGMA
C     DERIVEE DK/DSIGMA
C ----------------------------------------------------------------------
C IN  IMATE  : ADRESSE DU MATERIAU CODE
C     TP     : TEMPERATURE EN T+
C     NB     : NOMBRE DE COMPOSANTES DE SIGMA
C     XHI    : CRITERE D'ENDOMMAGEMENT VISCOPLASTIQUE
C 
C OUT KXHI   : CARACTERISTIQUE VISQUEUSE EN T+
C     DKXIDX : DERIVEE DK/DXHI
C
C INFO      KXHI = MATE(9,2) = K_D          (ENDOMMAGEMENT)
C ----------------------------------------------------------------------
C
      REAL*8      VPAR(2)
      REAL*8      ZERO
      PARAMETER    (ZERO = 0.D0)
      CHARACTER*2 BL2,FB2,OK
      CHARACTER*8 NOMPAR(2), NOM
C 
      DATA NOM / 'K_D' /
      DATA FB2 / 'F '/
C
C ----------------------------------------------------------------------
C-- 1. INITIALISATION
C--------------------
      KXHI = ZERO
      DKXIDX = ZERO
C
C-- 2. CALCUL DE K_D(XHI,TEMP) A PARTIR DU MATERIAU CODE
C------------------------------------------------------ 
      NOMPAR(1) = 'TEMP    '
      NOMPAR(2) = 'X       '
C
      VPAR(1) = TP
      VPAR(2) = XHI
C
      CALL RCVALA (IMATE, 'VENDOCHAB', 2,  NOMPAR, VPAR, 1,
     1              NOM,  KXHI, OK, FB2 )
C
C-- 3. CREATION D'UNE FONCTION KT(XHI) EXTRAITE DE LA NAPPE 
C          A LA TEMPERATURE COURANTE
C-----------------------------------------------------------
CCCCC      CALL RCNAPP (IMATE,TP,IFON)
C
C-- 4.  CALCUL DE LA DERIVEE DK(XHI)/DXHI
C--------------------------------------------
CCCCC      CALL RCFODE (IFON,XHI,KXHI,DKHIDX)
C
      END
