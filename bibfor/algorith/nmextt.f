      SUBROUTINE NMEXTT(NOMCHA,TYPCHA)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/01/2011   AUTEUR ABBAS M.ABBAS 
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
      IMPLICIT      NONE
      CHARACTER*16  NOMCHA
      CHARACTER*4   TYPCHA
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (EXTRACTION - UTILITAIRE)
C
C TYPE DU CHAMP (NOEU OU ELGA)
C
C ----------------------------------------------------------------------
C
C
C IN  NOMCHA : NOM DU CHAMP
C OUT TYPCHA : TYPE DU CHAMP
C             'NOEU'
C             'ELGA'
C
C ----------------------------------------------------------------------
C

C
C --- INITIALISATIONS
C
      TYPCHA = 'XXXX'
C
C --- TYPE DE CHAMP
C
      IF ( NOMCHA .EQ. 'TEMP' ) THEN
        TYPCHA = 'NOEU'        
      ELSEIF ( NOMCHA .EQ. 'DEPL' ) THEN
        TYPCHA = 'NOEU'
      ELSEIF ( NOMCHA .EQ. 'VITE' ) THEN
        TYPCHA = 'NOEU'
      ELSEIF ( NOMCHA .EQ. 'ACCE' ) THEN
        TYPCHA = 'NOEU'
      ELSEIF ( NOMCHA .EQ. 'FORC_NODA' ) THEN
        TYPCHA = 'NOEU'        
      ELSEIF ( NOMCHA .EQ. 'DEPL_ABSOLU' ) THEN
        TYPCHA = 'NOEU'
      ELSEIF ( NOMCHA .EQ. 'VITE_ABSOLU' ) THEN
        TYPCHA = 'NOEU'
      ELSEIF ( NOMCHA .EQ. 'ACCE_ABSOLU' ) THEN
        TYPCHA = 'NOEU'
      ELSEIF ( NOMCHA .EQ. 'VALE_CONT' ) THEN
        TYPCHA = 'NOEU'
      ELSEIF ( NOMCHA .EQ. 'SIEF_ELGA' ) THEN
        TYPCHA = 'ELGA'
      ELSEIF ( NOMCHA .EQ. 'VARI_ELGA' ) THEN
        TYPCHA = 'ELGA'
      ELSE
        CALL ASSERT(.FALSE.)    
      ENDIF  

      END
