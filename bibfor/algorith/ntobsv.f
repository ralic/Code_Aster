      SUBROUTINE NTOBSV(NOMA  ,SDSENS,SDIETO,SDOBSE,NUMINS,
     &                  INST  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/06/2011   AUTEUR ABBAS M.ABBAS 
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
      INTEGER       NUMINS
      CHARACTER*8   NOMA
      CHARACTER*19  SDOBSE
      CHARACTER*24  SDIETO,SDSENS
      REAL*8        INST  
C 
C ----------------------------------------------------------------------
C
C ROUTINE THER_NON_LINE (ALGORITHME)
C
C REALISER UNE OBSERVATION 
C      
C ----------------------------------------------------------------------
C
C
C IN  NOMA   : NOM DU MAILLAGE
C IN  SDEXTR : NOM DE LA SD POUR EXTRACTION
C IN  SDIETO : SD GESTION IN ET OUT
C IN  SDOBSE : SD OBSERVATION
C IN  INST   : INSTANT COURANT
C      
C ----------------------------------------------------------------------
C
      LOGICAL      LOBSV
C      
C ----------------------------------------------------------------------
C 
C
C --- INITIALISATIONS
C
      LOBSV  = .FALSE.
C
C --- DOIT-ON FAIRE UNE OBSERVATION  ?
C
      CALL LOBS  (SDOBSE,NUMINS,INST  ,LOBSV )
C
C --- AU MOINS UNE OBSERVATION
C
      IF (LOBSV) THEN
        CALL NMOBSE(NOMA  ,SDSENS,SDIETO,SDOBSE,NUMINS,
     &              INST  )
      ENDIF
C
      END
