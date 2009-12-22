      INTEGER FUNCTION MMINFI(DEFICO,QUESTZ,IZONE )
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      CHARACTER*24  DEFICO
      INTEGER       IZONE
      CHARACTER*(*) QUESTZ
C      
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE POUR LE CONTACT (TOUTES METHODES)
C
C REPOND A UNE QUESTION SUR UNE OPTION/CARACTERISTIQUE DU CONTACT
C VARIABLE SUIVANT LA ZONE
C REPONSE INTEGER
C
C ----------------------------------------------------------------------
C
C
C IN  DEFICO : SD POUR LA DEFINITION DU CONTACT
C IN  IZONE  : NUMERO DE LA ZONE DE CONTACT QU'ON INTERROGE
C IN  QUESTI : QUESTION POSEE
C
C ----------------------------------------------------------------------
C
      INTEGER      IREP
      REAL*8       R8BID
      LOGICAL      LBID
C
C ----------------------------------------------------------------------
C
      CALL MMINFP(IZONE ,DEFICO,QUESTZ,IREP  ,R8BID ,
     &            LBID  )
      MMINFI = IREP
      END
