      SUBROUTINE BURAFD(MATERD,MATERF,NMAT,AFD,BFD,CFD)
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
C=====================================================================
C
C SOUS PROGRAMME QUI CALCUL LES MATRICE DE DEFORMATION DE FLUAGE
C   DE DESSICCATION D APRES LE MODELE DE BAZANT
C
C     - MODELE DE BAZANT : => EQUATION (3.3-2)
C
C   DFDES(N+1) = AFD + BFD * SIGMA(N) + CFD * SIGMA(N+1)
C
C    => EQUATION (3.3-1)
C     ----------------------------------------------------------------
C     IN
C          MATERD :  COEFFICIENTS MATERIAU A T
C          MATERF :  COEFFICIENTS MATERIAU A T+DT
C          NMAT   :  DIMENSION TABLEAU MATER
C     OUT  
C          AFD     :  VECTEUR LIE AU FLUAGE DE DESSICCATION
C          BFD     :  TENSEUR LIE AU FLUAGE DE DESSICCATION
C          CFD     :  TENSEUR LIE AU FLUAGE DE DESSICCATION
C     ----------------------------------------------------------------
C=====================================================================
      IMPLICIT NONE
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT ,NDI
C     ----------------------------------------------------------------
      INTEGER   NMAT,NDT,NDI
      INTEGER   I,J
      REAL*8    MATERD(NMAT,2),MATERF(NMAT,2)
      REAL*8    AFD(6),BFD(6,6),CFD(6,6)
      REAL*8    B,C,ZERO
      REAL*8    ETAFD,DH
      DATA      ZERO/0.D0/

C === =================================================================
C RECUPERATION DES VALEURS DES PARAMETRES MATERIAU
C LE MODELE DE BAZANT NE COMPREND QU'UN PARAMETRE
C === =================================================================
      ETAFD = MATERD(8,2)
C === =================================================================
C --- INITIALISATION DES VECTEURS ET MATRICES
C === =================================================================
      DO 1 I=1,NDT
        AFD(I) = ZERO
        DO 2 J=1,NDT
          BFD(I,J) = ZERO
          CFD(I,J) = ZERO
 2      CONTINUE
 1    CONTINUE

C === =================================================================
C INITIALISATION DES VARIABLES
C === =================================================================

      DH = ABS(MATERF(6,1)-MATERD(6,1))

C === =================================================================
C AIGUILLAGE SUIVANT LA VALEUR DE ETAFD -> MODELE DE BAZANT
C === =================================================================
      IF (ETAFD.GT.ZERO) THEN
        B = DH / (2.D0*ETAFD)
        C = B
C === =================================================================
C CONSTRUCTION DE LA MATRICE DE FLUAGE DE DESSICCATION
C === =================================================================
        DO 10 I=1,NDT
          BFD(I,I) = B
          CFD(I,I) = C
  10    CONTINUE
      ENDIF

      END
