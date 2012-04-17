      CHARACTER*8 FUNCTION DINOGD(NOCHAM)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/07/2009   AUTEUR GENIAUT S.GENIAUT 
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
C
      IMPLICIT NONE
      CHARACTER*16 NOCHAM
C
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE 
C
C CONNAISSANT LE NOM DU CHAMP, ON RENVOIT LE NOM DE LA GRANDEUR
C
C ----------------------------------------------------------------------
C
C
C IN  NOCHAM : NOM DU CHAMP
C OUT DINOGD : NOM DE LA GRANDEUR
C
      IF ( NOCHAM .EQ. 'DEPL' )      DINOGD = 'DEPL_R'
      IF ( NOCHAM .EQ. 'VITE' )      DINOGD = 'DEPL_R'
      IF ( NOCHAM .EQ. 'ACCE' )      DINOGD = 'DEPL_R'
      IF ( NOCHAM .EQ. 'VALE_CONT' ) DINOGD = 'INFC_R'
      IF ( NOCHAM .EQ. 'SIEF_ELGA' ) DINOGD = 'SIEF_R'
      IF ( NOCHAM .EQ. 'VARI_ELGA' ) DINOGD = 'VARI_R'
      IF ( NOCHAM .EQ. 'TEMP' )      DINOGD = 'TEMP_R'                
      IF ( NOCHAM .EQ. 'FORC_NODA' ) DINOGD = 'FORC_R'
C      
      END
