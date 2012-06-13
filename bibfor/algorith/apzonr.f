      SUBROUTINE APZONR(SDAPPA,IZONE ,QUESTZ,VALR  )
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
      CHARACTER*19  SDAPPA
      INTEGER       IZONE 
      REAL*8        VALR
      CHARACTER*(*) QUESTZ
C      
C ----------------------------------------------------------------------
C
C ROUTINE APPARIEMENT (UTILITAIRE)
C
C INFO. DE TYPE REEL SUR LA ZONE COURANTE
C
C ----------------------------------------------------------------------
C
C
C IN  SDAPPA : NOM DE LA SD APPARIEMENT
C IN  IZONE  : NUMERO DE LA ZONE
C IN  QUESTI : QUESTION
C OUT VALR   : REPONSE A LA QUESTION 
C
C
C
C
      INTEGER      IFM,NIV
      CHARACTER*24 APINZR
      INTEGER      JPINZR
      CHARACTER*24 QUESTI
      INTEGER      APMMVD,ZINZR
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('APPARIEMENT',IFM,NIV)     
C
C --- ACCES SDAPPA
C
      APINZR = SDAPPA(1:19)//'.INZR'
      CALL JEVEUO(APINZR,'L',JPINZR)
C
C --- INITIALISATIONS
C
      QUESTI = QUESTZ   
      VALR   = 0.D0
      ZINZR  = APMMVD('ZINZR')
C      
C --- REPONSE
C
      IF (QUESTI.EQ.'TOLE_APPA') THEN   
        VALR   = ZR(JPINZR+ZINZR*(IZONE-1)+4 -1)
      ELSEIF (QUESTI.EQ.'TOLE_PROJ_EXT') THEN   
        VALR   = ZR(JPINZR+ZINZR*(IZONE-1)+5 -1)    
      ELSE
        CALL ASSERT(.FALSE.)  
      ENDIF
C
      CALL JEDEMA()
C 
      END
