      SUBROUTINE BURAFR(VIN,NVI,MATERD,MATERF,
     &                  NMAT,TIMED,TIMEF,AFR,BFR,CFR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/12/2011   AUTEUR FOUCAULT A.FOUCAULT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE FOUCAULT A.FOUCAULT
C=======================================================================
C
C ROUTINE QUI CALCULE LES MATRICES DE DEFORMATION 
C  DE FLUAGE PROPRE SPHERIQUE ET DEVIATORIQUE REVERSIBLE
C   D APRES LE MODELE BETON_BURGER_FP
C
C IN  VIN      : VARIABLES INTERNES INITIALES
C     NVI      : DIMENSION DES VECTEURS VARIABLES INTERNES
C     MATERD   : VECTEUR DE PARAMETRES MATERIAU A T
C     MATERF   : VECTEUR DE PARAMETRES MATERIAU A T+DT
C     NMAT     : DIMENSION DE CMAT
C     TIMED    : INSTANT T
C     TIMEF    : INSTANT T+DT
C OUT AFR      : VECTEUR LIE A LA DEFOR. REV. DE FLUAGE PROPRE 
C     BFR      : MATRICE LIEE A LA DEFOR. REV. DE FLUAGE PROPRE 
C     CFR      : MATRICE LIEE A LA DEFOR. REV. DE FLUAGE PROPRE 
C=======================================================================
      IMPLICIT NONE
      INTEGER         NVI,NMAT,NDT,NDI,I,J
      REAL*8          VIN(*)
      REAL*8          MATERD(NMAT,2),MATERF(NMAT,2)
      REAL*8          TIMED,TIMEF
      REAL*8          AFR(6),BFR(6,6),CFR(6,6)
      REAL*8          AFRS,BFRS,CFRS
      REAL*8          AFRD(6),BFRD,CFRD

      COMMON /TDIM/   NDT,NDI

C === =================================================================
C INITIALISATION DES VARIABLES
C === =================================================================
      DO 1 I=1,NDT
        AFRD(I) = 0.D0
        AFR(I)  = 0.D0
        DO 2 J = 1, NDT
          BFR(I,J) = 0.D0
          CFR(I,J) = 0.D0
 2      CONTINUE
 1    CONTINUE
C === =================================================================
C CALCUL DE LA MATRICE DES DEFORMATIONS REVERSIBLES 
C          DE FLUAGE PROPRE SPHERIQUE INCREMENTALES
C === =================================================================
      CALL BURARS(VIN,NVI,MATERD,MATERF,NMAT,
     &            TIMED,TIMEF,AFRS,BFRS,CFRS)
C === =================================================================
C CALCUL DE LA MATRICE DES DEFORMATIONS REVERSIBLES 
C          DE FLUAGE PROPRE DEVIATORIQUE INCREMENTALES
C === =================================================================
      CALL BURARD(VIN,NVI,MATERD,MATERF,NMAT,
     &            TIMED,TIMEF,AFRD,BFRD,CFRD)
C === =================================================================
C CONSTRUCTION DE LA MATRICE DES DEFORMATIONS REVERSIBLES 
C          DE FLUAGE PROPRE INCREMENTALES
C === =================================================================
      DO 3 I = 1, NDI 
        AFR(I)   = AFRS + AFRD(I)
        AFR(I+3) = AFRD(I+3)
        BFR(I,I) = (BFRS+2.D0*BFRD)/3.D0
        CFR(I,I) = (CFRS+2.D0*CFRD)/3.D0
        BFR(I+NDI,I+NDI) = BFRD
        CFR(I+NDI,I+NDI) = CFRD
 3    CONTINUE 
      BFR(1,2) = (BFRS-BFRD)/3
      CFR(1,2) = (CFRS-CFRD)/3
      BFR(2,1) = BFR(1,2)
      CFR(2,1) = CFR(1,2)
      BFR(3,1) = BFR(1,2)
      CFR(3,1) = CFR(1,2)
      BFR(1,3) = BFR(1,2)
      CFR(1,3) = CFR(1,2)
      BFR(2,3) = BFR(1,2)
      CFR(2,3) = CFR(1,2)
      BFR(3,2) = BFR(1,2)
      CFR(3,2) = CFR(1,2)

      END
