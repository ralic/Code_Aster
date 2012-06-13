      SUBROUTINE NMCROI(SDOBSE,MOTFAC,NBOCC )
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
      CHARACTER*19 SDOBSE
      INTEGER      NBOCC
      CHARACTER*16 MOTFAC
C
C --------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (STRUCTURES DE DONNES - OBSERVATION)
C
C LECTURE LISTE DES INSTANTS
C
C ----------------------------------------------------------------------
C
C
C IN  SDOBSE : NOM DE LA SD POUR OBSERVATION
C IN  MOTFAC : MOT-FACTEUR POUR LIRE 
C IN  MOTPAS : MOT-FACTEUR POUR LIRE PAS
C
C
C
C
      INTEGER      IOCC
      CHARACTER*1  BASE
      CHARACTER*19 LISTLI
      CHARACTER*2  CHAINE
      CHARACTER*16 MOTPAS
C      
C ----------------------------------------------------------------------
C      
      CALL JEMARQ()                       
C
C --- INITIALISATIONS
C
      BASE   = 'V'
      MOTPAS = 'PAS_OBSE'
C
C --- LECTURE LISTE INSTANTS
C 
      DO 10 IOCC = 1 , NBOCC        
        CALL IMPFOI(0,2,IOCC,CHAINE)        
        LISTLI = SDOBSE(1:14)//CHAINE(1:2)//'.LI'
        CALL NMCRPX(MOTFAC,MOTPAS,IOCC  ,LISTLI,BASE  )
  10  CONTINUE
C
      CALL JEDEMA()
      END
