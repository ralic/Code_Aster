      SUBROUTINE CMATVE(MAT,VECTIN,VECTOU,NDIM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/01/98   AUTEUR CIBHHLB L.BOURHRARA 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
C***********************************************************************
C    B. GUIGON    P. RICHARD                   DATE 06/04/92
C-----------------------------------------------------------------------
C  BUT:  < PRODUIT MATRICE VECTEUR COMPLEXE >
C
C   CETTE ROUTINE CALCULE LE PRODUIT D'UNE MATRICE COMPLEXE PAR UNE
C VECTEUR COMPLEXE
C
C-----------------------------------------------------------------------
C
C MAT      /I/: MATRICE COMPLEXE
C VECTIN   /I/: VECTEUR COMPLEXE D'ENTREE
C VECTOUT  /I/: VECTEUR COMPLXE DE SORTIE
C NDIM     /I/: DIMENSION DU VECTEUR ET DE LA MATRICE
C
C-----------------------------------------------------------------------
C
      INTEGER NDIM
      INTEGER I,J
      COMPLEX*16 MAT(NDIM,NDIM),VECTIN(NDIM),VECTOU(NDIM)
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
      DO 10 I = 1,NDIM
        VECTOU(I) = DCMPLX(0.D0,0.D0)
        DO 20 J = 1,NDIM
          VECTOU(I) = VECTOU(I) + MAT(I,J)*VECTIN(J)
   20   CONTINUE
   10 CONTINUE
      GOTO 9999

 9999 CONTINUE
      END
