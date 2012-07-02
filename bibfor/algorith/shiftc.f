      SUBROUTINE SHIFTC(CRAID,CMASS,NDIM,VALSHI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C***********************************************************************
C    P. RICHARD     DATE 20/03/91
C-----------------------------------------------------------------------
C  BUT:  PROCEDER A UN SHIFT DES MATRICES COMPLEXES POUR CALCUL AUX
C   VALEURS PROPRES
      IMPLICIT NONE
C
C-----------------------------------------------------------------------
C
C CRAID    /M/: MATRICE COMPLEXE DE RAIDEUR
C CMASS    /I/: MATRICE COMPLEXE DE MASSE
C NDIM     /I/: DIMENSION DES MATRICES CARREES COMPLEXES
C VALSHI   /I/: VALEUR COMPLEXE DU SHIFT
C
C-----------------------------------------------------------------------
      COMPLEX*16 CRAID(*),CMASS(*),CZERO,VALSHI
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,NDIM 
C-----------------------------------------------------------------------
      CZERO=DCMPLX(0.D0,0.D0)
C
      IF(VALSHI.EQ.CZERO) GOTO 9999
C
      DO 10 I=1,NDIM*(NDIM+1)/2
          CRAID(I)=CRAID(I)+VALSHI*CMASS(I)
 10   CONTINUE
C
 9999 CONTINUE
      END
