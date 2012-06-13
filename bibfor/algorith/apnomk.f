      SUBROUTINE APNOMK(SDAPPA,QUESTI,RNOMSD)
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
      CHARACTER*(*) QUESTI
      CHARACTER*24  RNOMSD
C      
C ----------------------------------------------------------------------
C
C ROUTINE APPARIEMENT (UTILITAIRE)
C
C ROUTINE D'INTERROGATION DE LA SD NOMSD
C
C ----------------------------------------------------------------------
C
C
C IN  SDAPPA : NOM DE LA SD APPARIEMENT
C IN  QUISTI : QUEL NOM DE SD
C               'NEWGEO' - OBJET GEOMETRIE ACTUALISEE
C               'NOMA'   - OBJET MAILLAGE
C               'DEFICO' - OBJET DEFINTION DU CONTACT               
C OUT RNOMSD : NOM DE LA SD INTERROGEE
C
C
C
C
      CHARACTER*24 NOMSD
      INTEGER      JNOMSD

C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      RNOMSD = ' '         
C
C --- ACCES SDAPPA
C  
      NOMSD  = SDAPPA(1:19)//'.NOSD'
      CALL JEVEUO(NOMSD ,'L',JNOMSD)
C
C --- REPONSE
C
      IF (QUESTI(1:4).EQ.'NOMA') THEN
        RNOMSD = ZK24(JNOMSD+1 -1)
      
      ELSEIF (QUESTI(1:6).EQ.'NEWGEO') THEN
        RNOMSD = ZK24(JNOMSD+2 -1)
      
      ELSEIF(QUESTI(1:6).EQ.'DEFICO') THEN
        RNOMSD = ZK24(JNOMSD+3 -1)
      
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF     
C
      CALL JEDEMA()
C 
      END
