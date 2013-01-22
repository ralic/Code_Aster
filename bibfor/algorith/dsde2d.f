      SUBROUTINE DSDE2D(NDIM,F,DSDE,D)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/01/2013   AUTEUR SFAYOLLE S.FAYOLLE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE SFAYOLLE S.FAYOLLE

C ----------------------------------------------------------------------
C     CALCUL DU TERME D=2FF(dS/dE)F^TF^T POUR LES FORMULATIONS INCO_LOG
C ----------------------------------------------------------------------
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  F       : GRADIENT TRANSFORMATION EN T
C IN  DSDE    : 2dS/dC = dS/dE_GL AVEC RACINE DE 2 ET 2
C OUT D       : 2FF(dS/dE)F^TF^T

      IMPLICIT NONE
      INTEGER NDIM, INDI(6), INDJ(6), MN, OP, IJ, KL, NMAX
      REAL*8  F(3,3), D(6,6), DSDE(6,6), F1, F2, F3, F4
      REAL*8  RIND(6)
      DATA    INDI / 1, 2, 3, 1, 1, 2 /
      DATA    INDJ / 1, 2, 3, 2, 3, 3 /
      DATA    RIND / 0.5D0, 0.5D0, 0.5D0, 0.70710678118655D0,
     &               0.70710678118655D0, 0.70710678118655D0 /

      NMAX=2*NDIM

      DO 100 MN = 1,NMAX
        DO 200 OP = 1,NMAX
          D(MN,OP) = 0.D0
            DO 300 IJ = 1,NMAX
              DO 400 KL = 1,NMAX
                F1 = F(INDI(MN),INDI(IJ))*F(INDJ(MN),INDJ(IJ))
                F2 = F(INDI(OP),INDI(KL))*F(INDJ(OP),INDJ(KL))
                F3 = F(INDI(MN),INDJ(IJ))*F(INDJ(MN),INDI(IJ))
                F4 = F(INDI(OP),INDJ(KL))*F(INDJ(OP),INDI(KL))
                D(MN,OP) = D(MN,OP)
     &                   +(F1+F3)*(F2+F4)*DSDE(IJ,KL)*RIND(IJ)*RIND(KL)
 400         CONTINUE
 300      CONTINUE
 200    CONTINUE
 100  CONTINUE
      END
