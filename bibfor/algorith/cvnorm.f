      SUBROUTINE CVNORM(MAT,VECT,NDIM,IRETOU)
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
      IMPLICIT NONE
C
C***********************************************************************
C    B. GUIGON    P. RICHARD                   DATE 06/04/92
C-----------------------------------------------------------------------
C  BUT:  < NORME VECTEUR >
C
C   CETTE ROUTINE NORME UN VECTEUR COMPLEXE PAR RAPPORT A UNE MATRICE
C   COMPLEXE
C
C-----------------------------------------------------------------------
C
C MAT      /I/: MATRICE COMPLEXE DEFINISSANT LE PRODUIT SCALAIRE
C VECT     /M/: VECTEUR COMPLEXE A NORMER
C NDIM     /I/: DIMENSION DU VECTEUR ET DE LA MATRICE
C IRETOU
C-----------------------------------------------------------------------
C
      INTEGER    NDIM
      COMPLEX*16 MAT(*),VECT(NDIM)
      REAL*8     ZERO
      INTEGER    I,IRETOU
      COMPLEX*16 NORMEC
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DATA ZERO /0.D0/

C-----------------------------------------------------------------------
C
      CALL SESQUI(MAT,VECT,NDIM,NORMEC)
      IRETOU = 0
      IF(ABS(NORMEC).EQ.ZERO) THEN
         IRETOU = 1
         GOTO 9999
      ENDIF
      NORMEC=DCMPLX(SQRT(ABS(DBLE(NORMEC))),0.D0)
      DO 10 I=1, NDIM
        VECT(I)=VECT(I)/NORMEC
 10   CONTINUE
 9999 CONTINUE
      END
