      SUBROUTINE HM0018 ( TYPELE, TYPEMO )
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
      IF ( TYPELE.EQ.'HM_HEXA20' .OR. TYPELE.EQ.'HM_FACE8' ) THEN
         TYPEMO = '3D_HM'

      ELSEIF ( TYPELE.EQ.'HM_HEXA20D' .OR. TYPELE.EQ.'HM_FACE8' ) THEN
         TYPEMO = '3D_HMD'

      ELSEIF ( TYPELE.EQ.'HM_AXIS_QU8' .OR. 
     +         TYPELE.EQ.'HM_AXIS_TR6' .OR.
     +         TYPELE.EQ.'HM_AXIS_SE3' ) THEN
         TYPEMO = 'AXIS_HM'

      ELSEIF ( TYPELE.EQ.'HM_AXIS_QU8D' .OR. 
     +         TYPELE.EQ.'HM_AXIS_TR6D' .OR.
     +         TYPELE.EQ.'HM_AXIS_SE3' ) THEN
         TYPEMO = 'AXIS_HMD'

      ELSEIF ( TYPELE.EQ.'HM_DPQ8'  .OR. 
     +         TYPELE.EQ.'HM_DPTR6' .OR.
     +         TYPELE.EQ.'HM_D_PLAN_SE3' .OR.
     +         TYPELE.EQ.'HM_DPQ8_P'  .OR. 
     +         TYPELE.EQ.'HM_DPTR6_P' .OR.
     +         TYPELE.EQ.'HM_D_PLAN_SE3_P'
     +          ) THEN
         TYPEMO = 'D_PLAN_HM'

      ELSEIF ( TYPELE.EQ.'HM_DPQ8D'  .OR. 
     +         TYPELE.EQ.'HM_DPTR6D' .OR.
     +         TYPELE.EQ.'HM_D_PLAN_SE3' ) THEN
         TYPEMO = 'D_PLAN_HMD'

      ENDIF
C
      END
