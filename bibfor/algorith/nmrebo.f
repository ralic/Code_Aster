      SUBROUTINE NMREBO(F     ,MEM   ,SENS  ,RHO   ,RHOOPT,
     &                  LICOPT,LICCVG,FOPT  ,FCVG  ,OPT   ,
     &                  ACT   ,OPTI  ,STITE)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/04/2007   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_21
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      REAL*8       MEM(2,10),SENS,RHO,F,RHOOPT
      LOGICAL      OPTI,STITE
      INTEGER      LICOPT,LICCVG
      REAL*8       FOPT,FCVG 
      INTEGER      OPT,ACT           
C      
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (RECH. LINE. - METHODE MIXTE)
C
C RECHERCHE LINEAIRE AVEC LA METHODE MIXTE: BORNES + UNIDIRECTIONNEL
C      
C ----------------------------------------------------------------------
C
C
C      
C ----------------------------------------------------------------------
C
      REAL*8       RHONEW
C      
C-----------------------------------------------------------------------
C
      CALL ZBITER(SENS*RHO,SENS*F,MEM,OPTI,RHONEW)     
      RHO    = RHONEW * SENS

C      
C --- PRISE EN COMPTE D'UN RESIDU OPTIMAL SI NECESSAIRE
C
      IF (OPTI) THEN
        RHOOPT = RHO
        LICOPT = LICCVG
        FOPT   = ABS(F)
        OPT    = ACT
        ACT    = 3 - ACT
        IF (ABS(F) .LT. FCVG) THEN
          STITE = .TRUE.
        ENDIF 
      END IF      
      
      END
