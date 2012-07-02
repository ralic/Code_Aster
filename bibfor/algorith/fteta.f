      SUBROUTINE FTETA( THETA, NEQ , F0 , F1 )
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
      REAL*8            THETA,       F0(*),F1(*)
C**********************************************************************
C
C      BUT :   POUR LA METHODE DE WILSON  , CALCUL DU SECOND MEMBRE
C      ====    DANS VEC(NEQ+1:2*NEQ)
C
C     INPUT:
C           THETA  : PARAMETRE THETA
C           NEQ    : DIMENSION DES VECTEURS FA ET VEC
C           F0     : VECTEUR CHARGEMENT AU TEMPS T
C           F1     : VECTEUR CHARGEMENT AU TEMPS T+DT
C     OUTPUT:
C           F1     : VECTEUR CHARGEMENT THETA METHODE
C
C
C----------------------------------------------------------------------
C   E.D.F DER   JACQUART G. 47-65-49-41      LE 19 JUILLET 1990
C**********************************************************************
C
      REAL *8  COEF
C-----------------------------------------------------------------------
      INTEGER NEQ 
C-----------------------------------------------------------------------
      COEF = 1.0D0 - THETA
      CALL DSCAL ( NEQ , THETA , F1 , 1 )
      CALL DAXPY ( NEQ, COEF , F0 , 1 , F1 , 1 )
      END
