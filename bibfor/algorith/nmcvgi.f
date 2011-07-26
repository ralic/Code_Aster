      SUBROUTINE NMCVGI(SDIMPR,TYPAFF)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/07/2011   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*24 SDIMPR
      CHARACTER*6  TYPAFF           
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (AFFICHAGE)
C
C ENCAPSULATION DES AFFICHAGES DU TABLEAU DE CONVERGENCE ET INFOS
C
C ----------------------------------------------------------------------
C
C
C IN  SDIMPR : SD AFFICHAGE
C IN  TYPAFF : TYPE AFFICHAGE
C               'CVG_OK'  MESSAGE DE CONVERGENCE NORMALE 
C               'CVG_MX'  MESSAGE DE CONVERGENCE SI CRITERE 
C                             RESI_GLOB_RELA ET CHARGEMENT = 0,
C                             ON UTILISE RESI_GLOB_MAXI (MAXREL)
C               'CVG_FO'  MESSAGE DE CONVERGENCE SI CONVERGENCE
C                             FORCEE (ARRET=NON)
C               'CVG_NO'  MESSAGE DE CONVERGENCE SI PAS DE CRITERES DE 
C                             CONVERGENCE 
C
C ----------------------------------------------------------------------
C
      IF (TYPAFF.EQ.'CVG_OK') THEN
        CALL NMIMPR(SDIMPR,'TABL',' ',' ',0.D0,0) 
        CALL NMIMPR(SDIMPR,'IMPR','LIGNE',' ',0.D0,0)
        CALL NMIMPR(SDIMPR,'IMPR','CONV_OK',' ',0.D0,0)
        CALL NMIMPR(SDIMPR,'IMPR','CONV_RECA',' ',0.D0,0)        
      ELSEIF (TYPAFF.EQ.'CVG_MX') THEN
        CALL NMIMPR(SDIMPR,'TABL',' ',' ',0.D0,0) 
        CALL NMIMPR(SDIMPR,'IMPR','LIGNE',' ',0.D0,0)
        CALL NMIMPR(SDIMPR,'IMPR','CONV_MAXI',' ',0.D0,0)
        CALL NMIMPR(SDIMPR,'IMPR','CONV_RECA',' ',0.D0,0) 
      ELSEIF (TYPAFF.EQ.'CVG_NO') THEN
        CALL NMIMPR(SDIMPR,'TABL',' ',' ',0.D0,0) 
        CALL NMIMPR(SDIMPR,'IMPR','LIGNE',' ',0.D0,0)
        CALL NMIMPR(SDIMPR,'IMPR','CONV_NONE',' ',0.D0,0)
      ELSEIF (TYPAFF.EQ.'CVG_FO') THEN
        CALL NMIMPR(SDIMPR,'TABL',' ',' ',0.D0,0) 
        CALL NMIMPR(SDIMPR,'IMPR','LIGNE',' ',0.D0,0)
        CALL NMIMPR(SDIMPR,'IMPR','CONV_FORC',' ',0.D0,0)
        CALL NMIMPR(SDIMPR,'IMPR','CONV_RECA',' ',0.D0,0)
      ELSE
        CALL ASSERT(.FALSE.)   
      ENDIF     
C
      END
