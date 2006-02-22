      SUBROUTINE THH218 ( TYPELE, TYPEMO )
      IMPLICIT NONE
      CHARACTER*16        TYPELE, TYPEMO
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/02/2006   AUTEUR GRANET S.GRANET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
      IF ( TYPELE.EQ.'THH2_AXIS_QU8' .OR.
     +     TYPELE.EQ.'THH2_AXIS_TR6' .OR.
     +     TYPELE.EQ.'THH2_AXIS_SE3' ) THEN
         TYPEMO = 'AXIS_THH2'

      ELSEIF ( TYPELE.EQ.'THH2_AXIS_QU8D' .OR.
     +         TYPELE.EQ.'THH2_AXIS_TR6D' .OR.
     +         TYPELE.EQ.'THH2_AXIS_QU4D' .OR.
     +         TYPELE.EQ.'THH2_AXIS_TR3D' .OR.
     +         TYPELE.EQ.'THH2_AXIS_SE2' .OR.
     +         TYPELE.EQ.'THH2_AXIS_SE3' ) THEN
         TYPEMO = 'AXIS_THH2D'

      ELSEIF ( TYPELE.EQ.'THH_HEXA20' .OR. TYPELE.EQ.'THH_FACE8' ) THEN
         TYPEMO = '3D_THH2'

      ELSEIF ( TYPELE.EQ.'THH2_HEXA20D'  .OR.
     +         TYPELE.EQ.'THH2_TETRA10D' .OR.
     +         TYPELE.EQ.'THH2_PYRAM13D' .OR.
     +         TYPELE.EQ.'THH2_PENTA15D' .OR.
     +         TYPELE.EQ.'THH2_HEXA8D'   .OR.
     +         TYPELE.EQ.'THH2_TETRA4D'  .OR.
     +         TYPELE.EQ.'THH2_PENTA6D'  .OR.
     +         TYPELE.EQ.'THH2_FACE6' .OR. TYPELE.EQ.'THH_FACE8' .OR.
     +         TYPELE.EQ.'THH2_FACE3' .OR. TYPELE.EQ.'THH_FACE4' ) THEN
         TYPEMO = '3D_THH2D'

      ELSEIF ( TYPELE.EQ.'THH2_DPQ8'  .OR.
     +         TYPELE.EQ.'THH2_DPQ4'  .OR.
     +         TYPELE.EQ.'THH2_DPTR3' .OR.
     +         TYPELE.EQ.'THH2_DPTR6' .OR.
     +         TYPELE.EQ.'THH2_D_PLAN_SE2' .OR.
     +         TYPELE.EQ.'THH2_D_PLAN_SE3' ) THEN
         TYPEMO = 'D_PLAN_THH2'

      ELSEIF ( TYPELE.EQ.'THH2_DPQ8D'  .OR.
     +         TYPELE.EQ.'THH2_DPQ4D'  .OR.
     +         TYPELE.EQ.'THH2_DPTR3D' .OR.
     +         TYPELE.EQ.'THH2_DPTR6D' .OR.
     +         TYPELE.EQ.'THH2_D_PLAN_SE2' .OR.
     +         TYPELE.EQ.'THH2_D_PLAN_SE3' ) THEN
         TYPEMO = 'D_PLAN_THH2D'

      ENDIF
C
      END
