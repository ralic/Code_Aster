      SUBROUTINE CFZONE(DEFICO,IZONE ,TYPSUR,ISURF )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      CHARACTER*24 DEFICO
      CHARACTER*4  TYPSUR
      INTEGER      IZONE,ISURF
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (TOUTES METHODES - UTILITAIRE)
C
C NUMERO DE LA ZONE
C
C ----------------------------------------------------------------------
C
C
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  IZONE  : NUMERO DE LA ZONE DE CONTACT
C IN  TYPSUR : TYPE DE SURFACE
C               'MAIT'
C               'ESCL'
C OUT ISURF  : NUMERO DANS LA SURFACE 
C                 POUR ACCES PSUNOCO/PSUMACO/PNOEUQU
C
C
C
C
      CHARACTER*24 PZONE
      INTEGER      JZONE
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ ()
C      
C --- RECUPERATION DE QUELQUES DONNEES      
C
      PZONE  = DEFICO(1:16)//'.PZONECO'
      CALL JEVEUO(PZONE, 'L',JZONE )    
C
C --- INITIALISATIONS
C
      IF (TYPSUR.EQ.'ESCL') THEN
        ISURF  = ZI(JZONE+IZONE)
      ELSEIF (TYPSUR.EQ.'MAIT') THEN
        ISURF  = ZI(JZONE+IZONE-1) + 1         
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF   
C
      CALL JEDEMA()
C
      END
