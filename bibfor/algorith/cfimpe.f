      SUBROUTINE CFIMPE(IFM,NIV,SUBAPP,IMESG )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      CHARACTER*6   SUBAPP
      INTEGER       IFM
      INTEGER       NIV
      INTEGER       IMESG  
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES)
C
C CENTRALISATION DES ROUTINES D'AFFICHAGE EN MODE DEBUG
C AFFICHAGE SIMPLE NON FORMATE
C
C ----------------------------------------------------------------------
C
C
C IN  IFM    : UNITE D'IMPRESSION (EN GENERAL 6: FICHIER MESSAGE)
C IN  NIV    : NIVEAU D'AFFICHAGE PROVENANT MOT-CLEF INFO
C IN  SUBAPP : SOUS-ROUTINE D'APPEL
C IN  IMESG  : NUMERO DE MESSAGE DANS LA SOUS-ROUTINE
C
C ----------------------------------------------------------------------
C
      IF (NIV.LT.2) THEN
        GOTO 999
      ENDIF
C
      IF (SUBAPP.EQ.'CFGEOM') THEN
        IF (IMESG.EQ.1) THEN
          CALL U2MESS('I','CONTACTDEBG_1')
        ELSEIF (IMESG.EQ.2) THEN
          CALL U2MESS('I','CONTACTDEBG_2')
        ELSEIF (IMESG.EQ.3) THEN
          CALL U2MESS('I','CONTACTDEBG_3')   
        ELSEIF (IMESG.EQ.4) THEN
          CALL U2MESS('I','CONTACTDEBG_4')               
        ELSE
          CALL U2MESS('I','CONTACTDEBG_99')
        ENDIF 
      ELSEIF (SUBAPP.EQ.'CFNORM') THEN
        IF (IMESG.EQ.1) THEN
          CALL U2MESS('I','CONTACTDEBG_6')              
        ELSE
          CALL U2MESS('I','CONTACTDEBG_99')
        ENDIF         
      ELSEIF (SUBAPP.EQ.'SAUVCO') THEN
        IF (IMESG.EQ.1) THEN
          CALL U2MESS('I','CONTACTDEBG_5')   
        ELSE
          CALL U2MESS('I','CONTACTDEBG_99')
        ENDIF 
      ELSEIF (SUBAPP.EQ.'RESTCO') THEN
        IF (IMESG.EQ.1) THEN
          CALL U2MESS('I','CONTACTDEBG_9')   
        ELSE
          CALL U2MESS('I','CONTACTDEBG_99')
        ENDIF 
      ELSEIF (SUBAPP.EQ.'RECHMN') THEN
        IF (IMESG.EQ.1) THEN
          CALL U2MESS('I','CONTACTDEBG_15')   
        ELSE
          CALL U2MESS('I','CONTACTDEBG_99')
        ENDIF 
      ELSEIF (SUBAPP.EQ.'CHMANO') THEN
        IF (IMESG.EQ.1) THEN
          CALL U2MESS('I','CONTACTDEBG_16')   
        ELSEIF (IMESG.EQ.2) THEN
          CALL U2MESS('I','CONTACTDEBG_17')           
        ELSE
          CALL U2MESS('I','CONTACTDEBG_99')
        ENDIF     
        
      ELSEIF (SUBAPP.EQ.'NMCOFR') THEN
        IF (IMESG.EQ.1) THEN
          CALL U2MESS('I','CONTACTDEBG_18')   
        ELSEIF (IMESG.EQ.4) THEN
          CALL U2MESS('I','CONTACTDEBG_19')           
        ELSE
          CALL U2MESS('I','CONTACTDEBG_99')
        ENDIF         

      ELSEIF (SUBAPP.EQ.'CFCRSD') THEN
        IF (IMESG.EQ.1) THEN
          CALL U2MESS('I','CONTACTDEBG_22')         
        ELSE
          CALL U2MESS('I','CONTACTDEBG_99')
        ENDIF 

      ELSEIF (SUBAPP.EQ.'CFSYME') THEN
        IF (IMESG.EQ.1) THEN
          CALL U2MESS('I','CONTACTDEBG_23')         
        ELSE
          CALL U2MESS('I','CONTACTDEBG_99')
        ENDIF
                                                
      ELSE
        CALL U2MESS('I','CONTACTDEBG_99')
      ENDIF
C
 999  CONTINUE
C
      END
