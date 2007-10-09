      SUBROUTINE THHM18 ( TYPELE, TYPEMO )
      IMPLICIT NONE
      CHARACTER*16        TYPELE, TYPEMO
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/10/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C
      IF ( TYPELE.EQ.'THHM_AXIS_QU8' .OR. 
     +     TYPELE.EQ.'THHM_AXIS_TR6' .OR.
     +     TYPELE.EQ.'THHM_AXIS_SE3' ) THEN
         TYPEMO = 'AXIS_THHM'

      ELSEIF ( TYPELE.EQ.'THHM_AXIS_QU8D' .OR. 
     +         TYPELE.EQ.'THHM_AXIS_TR6D' .OR.
     +         TYPELE.EQ.'THHM_AXIS_SE3' ) THEN
         TYPEMO = 'AXIS_THHMD'

      ELSEIF ( TYPELE.EQ.'THHM_DPQ8'  .OR. 
     +         TYPELE.EQ.'THHM_DPTR6' .OR.
     +         TYPELE.EQ.'THHM_D_PLAN_SE3' ) THEN
         TYPEMO = 'D_PLAN_THHM'

      ELSEIF ( TYPELE.EQ.'THHM_DPQ8D'  .OR. 
     +         TYPELE.EQ.'THHM_DPTR6D' .OR.
     +         TYPELE.EQ.'THHM_D_PLAN_SE3' ) THEN
         TYPEMO = 'D_PLAN_THHMD'

      ELSEIF ( TYPELE.EQ.'THHM_HEXA20'  .OR. 
     +         TYPELE.EQ.'THHM_TETRA10' .OR.
     +         TYPELE.EQ.'THHM_PYRAM13' .OR.
     +         TYPELE.EQ.'THHM_PENTA15' .OR.
     +         TYPELE.EQ.'THHM_FACE6'   .OR.
     +         TYPELE.EQ.'THHM_FACE8'   ) THEN
         TYPEMO = '3D_THHM'

      ELSEIF ( TYPELE.EQ.'THHM_HEXA20D'  .OR.
     +         TYPELE.EQ.'THHM_TETRA10D' .OR.
     +         TYPELE.EQ.'THHM_PYRAM13D' .OR.
     +         TYPELE.EQ.'THHM_PENTA15D' .OR.
     +         TYPELE.EQ.'THHM_FACE6'    .OR. 
     +         TYPELE.EQ.'THHM_FACE8'    ) THEN
         TYPEMO = '3D_THHMD'

      ENDIF
C
      END
