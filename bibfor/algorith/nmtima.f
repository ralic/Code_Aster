      SUBROUTINE NMTIMA(SDTIME,TIMER ,VALL)
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
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*24 SDTIME
      CHARACTER*3  TIMER
      LOGICAL      VALL
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (UTILITAIRE)
C
C GESTION DES TIMERS - MESURE DU TEMPS RESTANT
C      
C ----------------------------------------------------------------------
C
C
C IN  TIMER  : NOM DU TIMER
C                'PAS'   TIMER POUR UN PAS DE TEMPS
C                'ITE'   TIMER POUR UNE ITERATION DE NEWTON
C IN  SDTIME : SD TIMER
C OUT VALL   : RESULTAT DE L'ACTION
C
C
C
C
      CHARACTER*24 TIMPAS,TIMITE,TIMARC
      INTEGER      JTPAS,JTITE,JTARC
      REAL*8       TPSRST,MOYARC,MOYITE,MOYPAS
C      
C ----------------------------------------------------------------------
C      
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      VALL = .FALSE. 
C
C --- ACCES SD TIMER
C
      TIMPAS = SDTIME(1:19)//'.TPAS'
      TIMITE = SDTIME(1:19)//'.TITE'
      TIMARC = SDTIME(1:19)//'.TARC'
      CALL JEVEUO(TIMPAS,'E',JTPAS)
      CALL JEVEUO(TIMITE,'E',JTITE)
      CALL JEVEUO(TIMARC,'E',JTARC)
C
C --- TEMPS MOYENS
C
      MOYARC = ZR(JTARC+4-1)
      MOYITE = ZR(JTITE+4-1)
      MOYPAS = ZR(JTPAS+4-1)
C
C --- MESURE DES TEMPS RESTANTS
C
      IF (TIMER.EQ.'ITE') THEN
        TPSRST = ZR(JTITE+1-1)
        IF ((2.D0*MOYITE).LE.
     &      (0.95D0*TPSRST-MOYARC)) THEN
          VALL = .FALSE.
        ELSE
          VALL = .TRUE.
        ENDIF
      ELSEIF (TIMER.EQ.'PAS') THEN
        TPSRST = ZR(JTPAS+1-1)
        IF (MOYPAS .GT. 0.90D0*TPSRST) THEN
          VALL = .TRUE.
        ELSE
          VALL = .FALSE.
        ENDIF
      ELSE
        CALL ASSERT(.FALSE.)  
      ENDIF  
C
      CALL JEDEMA()
      END
