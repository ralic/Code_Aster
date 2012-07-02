      SUBROUTINE NEWDEP( NEQ , C , DT , D0 , V0 , A0 , D1 , A1 )
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
C**********************************************************************
      IMPLICIT NONE
C
C
C   INPUT:
C        NEQ   : NOMBRE D'EQUATIONS (D.D.L. ACTIFS)
C        C     : CONSTANTE DE CALCUL
C        DT    : PAS DE TEMPS DE CALCUL
C        D0    : VECTEUR DEPLACEMENT  INITIAL  (NEQ)
C        V0    : VECTEUR VITESSE      INITIALE (NEQ)
C        A0    : VECTEUR ACCELERATION INITIALE (NEQ)
C        A1    : VECTEUR ACCELERATION SOLUTION (NEQ)
C
C   OUTPUT:
C        D1    : VECTEUR DEPLACEMENT  SOLUTION (NEQ)
C
C  CALCUL DU DEPLACEMENT (WILSON)
C  ============================= DEPSOL = D0 + DT*V0 + C*(ACCSOL+2.A0)
C
C
C----------------------------------------------------------------------
C   E.D.F DER   JACQUART G. 47-65-49-41      LE 19 JUILLET 1990
C**********************************************************************
C
      REAL *8         D0(*) , D1(*) , V0(*) , A0(*) ,A1(*) , C , DT
C-----------------------------------------------------------------------
      INTEGER NEQ 
C-----------------------------------------------------------------------
      CALL DCOPY( NEQ , D0 , 1 , D1 , 1 )
      CALL DAXPY( NEQ , DT , V0, 1 , D1 , 1 )
      CALL DAXPY( NEQ , C  , A1, 1 , D1 , 1 )
      CALL DAXPY( NEQ , 2*C, A0, 1 , D1 , 1 )
      END
