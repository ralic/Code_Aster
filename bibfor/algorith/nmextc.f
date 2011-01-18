      SUBROUTINE NMEXTC(PHENOZ,MOTFAC,IOCC  ,NOMCHA,LEXTR )
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
      CHARACTER*16  MOTFAC
      INTEGER       IOCC
      CHARACTER*(*) PHENOZ
      CHARACTER*16  NOMCHA
      LOGICAL       LEXTR 
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (EXTRACTION - LECTURE)
C
C LECTURE DU NOM DU CHAMP
C VERIFICATION CHAMP OK POUR PHENOMENE
C
C ----------------------------------------------------------------------
C
C
C IN  PHENOM : TYPE DE PHENOMENE A EXTRAIRE
C               TH             - THERMIQUE
C               ME             - MECANIQUE
C               MEST           - MECANIQUE - STATIQUE
C               MEDY           - MECANIQUE - DYNAMIQUE
C               MEDY_MUAP      - MECANIQUE - DYNAMIQUE - MULTI-APPUI
C               ME**_****_CONT - MECANIQUE - CONTACT
C IN  MOTFAC : MOT-FACTEUR POUR LIRE 
C IN  IOCC   : OCCURRENCE DU MOT-CLEF FACTEUR MOTFAC
C OUT NOMCHA : NOM DU CHAMP
C OUT LEXTR  : .TRUE. SI LE CHAMP EST EXTRACTABLE (COMPATIBLE AVEC
C               PHENOMENE)
C  
C
C ----------------------------------------------------------------------
C
      CHARACTER*16 PHENOM
      CHARACTER*8  K8BID
      INTEGER      NCHP,N1
C
C ----------------------------------------------------------------------
C
C
C --- INITIALISATIONS
C
      PHENOM = PHENOZ
      LEXTR  = .TRUE.
C
C --- LECTURE: IL FAUT UN CHAMP ET UN SEUL
C      
      CALL GETVTX(MOTFAC,'NOM_CHAM',IOCC  ,1     ,0     ,
     &            K8BID ,N1    )
      NCHP   = -N1
      CALL ASSERT(NCHP.EQ.1)
C
C --- NOM DU CHAMP
C
      CALL GETVTX(MOTFAC,'NOM_CHAM',IOCC,1,1,NOMCHA,N1)
C
C --- CONFORMITE DU CHAMP SUIVANT PHENOMENE
C
      IF ( NOMCHA .EQ. 'TEMP' ) THEN
        IF (PHENOM(1:2).EQ.'ME') LEXTR  = .FALSE.
        
      ELSEIF ( NOMCHA .EQ. 'DEPL' ) THEN
        IF (PHENOM(1:2).EQ.'TH') LEXTR  = .FALSE.

      ELSEIF ( NOMCHA .EQ. 'FORC_NODA' ) THEN
        IF (PHENOM(1:2).EQ.'TH') LEXTR  = .FALSE.
        
      ELSEIF ( NOMCHA .EQ. 'VITE' ) THEN
        IF (PHENOM(3:4).NE.'DY') LEXTR  = .FALSE.
        
      ELSEIF ( NOMCHA .EQ. 'ACCE' ) THEN
        IF (PHENOM(3:4).NE.'DY') LEXTR  = .FALSE.
        
      ELSEIF ( NOMCHA .EQ. 'DEPL_ABSOLU' ) THEN
        IF (PHENOM(6:9).NE.'MUAP') LEXTR  = .FALSE.
               
      ELSEIF ( NOMCHA .EQ. 'VITE_ABSOLU' ) THEN
        IF (PHENOM(6:9).NE.'MUAP') LEXTR  = .FALSE.
        
      ELSEIF ( NOMCHA .EQ. 'ACCE_ABSOLU' ) THEN
        IF (PHENOM(6:9).NE.'MUAP') LEXTR  = .FALSE.
        
      ELSEIF ( NOMCHA .EQ. 'VALE_CONT' ) THEN
        IF (PHENOM(11:14).NE.'CONT') LEXTR  = .FALSE.
        
      ELSEIF ( NOMCHA .EQ. 'SIEF_ELGA' ) THEN
        IF (PHENOM(1:2).EQ.'TH') LEXTR  = .FALSE.
        
      ELSEIF ( NOMCHA .EQ. 'VARI_ELGA' ) THEN
        IF (PHENOM(1:2).EQ.'TH') LEXTR  = .FALSE.
      
      ELSEIF ( NOMCHA .EQ. 'SIEF_ELEM' ) THEN
        IF (PHENOM(1:2).EQ.'TH') LEXTR  = .FALSE.
        
      ELSEIF ( NOMCHA .EQ. 'VARI_ELEM' ) THEN
        IF (PHENOM(1:2).EQ.'TH') LEXTR  = .FALSE.
            
      ELSE
        LEXTR  = .FALSE.
        
      ENDIF
C
      END
