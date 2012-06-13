      SUBROUTINE NMTIME(SDTIME,PHASEZ,TIMERZ)
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
      CHARACTER*24  SDTIME
      CHARACTER*(*) PHASEZ
      CHARACTER*(*) TIMERZ
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (UTILITAIRE)
C
C GESTION DES TIMERS
C      
C ----------------------------------------------------------------------
C
C
C IN  SDTIME : SD TIMER
C IN  PHASE  : TYPE D'ACTION
C              'INI'               INITIALISATION DU TIMER
C              'RUN'               LANCEMENT DU TIMER
C              'END'               ARRET DU TIMER
C IN  TIMER  : NOM DU TIMER
C              'PAS'               TIMER PAS DE TEMPS
C              'ITE'               TIMER ITERATION DE NEWTON
C              'ARC'               TIMER ARCHIVAGE
C              'POST_TRAITEMENT'   TIMER POST_PROCESSING
C              'FACTOR'            TIMER FACTORISATION
C              'SOLVE'             TIMER RESOLUTION
C              'INTEGRATION'       TIMER INTEG. LDC
C              'ASSE_MATR'         TIMER ASSEMBLAGE MATRICES
C              'CONT_GEOM'         TIMER APPARIEMENT CONTACT
C              'CTCD_ALGO'         TIMER RESOLUTION CONTACT DISCRET
C              'CTCC_CONT'         TIMER RESOLUTION CONTACT CONTINU
C              'CTCC_FROT'         TIMER RESOLUTION FROTTEMENT CONTINU
C              'CTCC_MATR'         TIMER CALCUL MATRICE CONTINU
C              'SECO_MEMB'         TIMER CALCUL SECOND MEMBRE
C
C
C
C
      CHARACTER*24 TIMPAS,TIMITE,TIMARC,TIMPST
      INTEGER      JTPAS,JTITE,JTARC,JTPST
      CHARACTER*24 TIMDEB
      INTEGER      JTDEB
      CHARACTER*24 TIMTM1,TIMTM2 
      INTEGER      JTMP1,JTMP2
      CHARACTER*24 PHASE,TIMER
      REAL*8       TIME
C      
C ----------------------------------------------------------------------
C      
      CALL JEMARQ()
C    
C --- INITIALISATIONS
C                
      PHASE  = PHASEZ
      TIMER  = TIMERZ
C
C --- ACCES SD TIMER
C
      TIMTM1 = SDTIME(1:19)//'.TMP1'
      TIMTM2 = SDTIME(1:19)//'.TMP2'
      TIMPAS = SDTIME(1:19)//'.TPAS'
      TIMITE = SDTIME(1:19)//'.TITE'
      TIMPST = SDTIME(1:19)//'.TPST'
      TIMARC = SDTIME(1:19)//'.TARC'
      CALL JEVEUO(TIMTM1,'E',JTMP1)
      CALL JEVEUO(TIMTM2,'E',JTMP2)
      CALL JEVEUO(TIMPAS,'E',JTPAS)
      CALL JEVEUO(TIMITE,'E',JTITE)
      CALL JEVEUO(TIMPST,'E',JTPST)
      CALL JEVEUO(TIMARC,'E',JTARC)     
C
      TIMDEB = SDTIME(1:19)//'.TDEB'
      CALL JEVEUO(TIMDEB,'E',JTDEB)    
C
C --- INTERROGATION SD
C         
      IF (PHASE.EQ.'INI') THEN
C
C ----- INITIALISATIONS DES TIMERS
C
        IF (TIMER.EQ.'ALL') THEN
          CALL UTTCPU('CPU.NMTIME.PAS','INIT',' ')
          CALL UTTCPU('CPU.NMTIME.ITE','INIT',' ')
          CALL UTTCPU('CPU.NMTIME.ARC','INIT',' ')
          CALL UTTCPU('CPU.NMTIME.PST','INIT',' ')
          CALL UTTCPU('CPU.NMTIME.TM1','INIT',' ')
          CALL UTTCPU('CPU.NMTIME.TM2','INIT',' ') 
          ZR(JTDEB+1-1) = 0.D0
          ZR(JTDEB+2-1) = 0.D0
          ZR(JTDEB+3-1) = 0.D0
          ZR(JTDEB+4-1) = 0.D0
          ZR(JTDEB+5-1) = 0.D0
          ZR(JTDEB+6-1) = 0.D0
        ELSEIF (TIMER.EQ.'POST_TRAITEMENT') THEN
          CALL UTTCPU('CPU.NMTIME.PST','INIT',' ')
          ZR(JTDEB+4-1) = 0.D0 
        ELSEIF ((TIMER.EQ.'FACTOR').OR.
     &          (TIMER.EQ.'CONT_GEOM').OR.
     &          (TIMER.EQ.'CTCD_ALGO').OR.
     &          (TIMER.EQ.'CTCC_MATR').OR.
     &          (TIMER.EQ.'CTCC_CONT').OR.
     &          (TIMER.EQ.'CTCC_FROT').OR. 
     &          (TIMER.EQ.'CTCC_PREP').OR.
     &          (TIMER.EQ.'SECO_MEMB').OR.          
     &          (TIMER.EQ.'INTEGRATION').OR.
     &          (TIMER.EQ.'SOLVE')) THEN
          CALL UTTCPU('CPU.NMTIME.TM1','INIT',' ')
          ZR(JTDEB+5-1) = 0.D0
        ELSEIF (TIMER.EQ.'ASSE_MATR') THEN
          CALL UTTCPU('CPU.NMTIME.TM2','INIT',' ')
          ZR(JTDEB+6-1) = 0.D0                 
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF 
C 
      ELSEIF (PHASE.EQ.'RUN') THEN
C
C ----- LANCEMENT DES TIMERS
C
        IF (TIMER.EQ.'PAS') THEN
          CALL UTTCPU('CPU.NMTIME.PAS','DEBUT',' ')
        ELSEIF (TIMER.EQ.'ITE') THEN
          CALL UTTCPU('CPU.NMTIME.ITE','DEBUT',' ')
        ELSEIF (TIMER.EQ.'ARC') THEN
          CALL UTTCPU('CPU.NMTIME.ARC','DEBUT',' ')
        ELSEIF (TIMER.EQ.'POST_TRAITEMENT') THEN
          CALL UTTCPU('CPU.NMTIME.PST','DEBUT',' ')          
        ELSEIF ((TIMER.EQ.'FACTOR').OR.
     &          (TIMER.EQ.'CONT_GEOM').OR.
     &          (TIMER.EQ.'CTCD_ALGO').OR.
     &          (TIMER.EQ.'CTCC_MATR').OR.
     &          (TIMER.EQ.'CTCC_CONT').OR.
     &          (TIMER.EQ.'CTCC_FROT').OR. 
     &          (TIMER.EQ.'CTCC_PREP').OR.
     &          (TIMER.EQ.'SECO_MEMB').OR.          
     &          (TIMER.EQ.'INTEGRATION').OR.
     &          (TIMER.EQ.'SOLVE')) THEN
          CALL UTTCPU('CPU.NMTIME.TM1','DEBUT',' ')
        ELSEIF (TIMER.EQ.'ASSE_MATR') THEN
          CALL UTTCPU('CPU.NMTIME.TM2','DEBUT',' ')          
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
C               
      ELSEIF (PHASE.EQ.'END') THEN
C
C ----- ARRET DES TIMERS
C
        IF (TIMER.EQ.'PAS') THEN
          CALL UTTCPU('CPU.NMTIME.PAS','FIN',' ')
          CALL UTTCPR('CPU.NMTIME.PAS',4,ZR(JTPAS))
          TIME          = ZR(JTPAS+3-1) - ZR(JTDEB+1-1)
          ZR(JTDEB+1-1) = ZR(JTPAS+3-1)
          CALL NMRTIM(SDTIME,TIMER ,TIME  )
        ELSEIF (TIMER.EQ.'ITE') THEN
          CALL UTTCPU('CPU.NMTIME.ITE','FIN',' ')
          CALL UTTCPR('CPU.NMTIME.ITE',4,ZR(JTITE))
          TIME          = ZR(JTITE+3-1) - ZR(JTDEB+2-1)
          ZR(JTDEB+2-1) = ZR(JTITE+3-1)
          CALL NMRTIM(SDTIME,TIMER ,TIME  )
        ELSEIF (TIMER.EQ.'ARC') THEN
          CALL UTTCPU('CPU.NMTIME.ARC','FIN',' ')
          CALL UTTCPR('CPU.NMTIME.ARC',4,ZR(JTARC))
          TIME          = ZR(JTARC+3-1) - ZR(JTDEB+3-1)
          ZR(JTDEB+3-1) = ZR(JTARC+3-1)
          CALL NMRTIM(SDTIME,TIMER ,TIME)
        ELSEIF (TIMER.EQ.'POST_TRAITEMENT') THEN
          CALL UTTCPU('CPU.NMTIME.PST','FIN',' ')
          CALL UTTCPR('CPU.NMTIME.PST',4,ZR(JTPST))
          TIME          = ZR(JTPST+3-1) - ZR(JTDEB+4-1)
          ZR(JTDEB+4-1) = ZR(JTPST+3-1)
          CALL NMRTIM(SDTIME,TIMER ,TIME)          
        ELSEIF ((TIMER.EQ.'FACTOR').OR.
     &          (TIMER.EQ.'CONT_GEOM').OR.
     &          (TIMER.EQ.'CTCD_ALGO').OR.
     &          (TIMER.EQ.'CTCC_MATR').OR.
     &          (TIMER.EQ.'CTCC_CONT').OR.
     &          (TIMER.EQ.'CTCC_FROT').OR. 
     &          (TIMER.EQ.'CTCC_PREP').OR.
     &          (TIMER.EQ.'SECO_MEMB').OR.          
     &          (TIMER.EQ.'INTEGRATION').OR.
     &          (TIMER.EQ.'SOLVE')) THEN
          CALL UTTCPU('CPU.NMTIME.TM1','FIN',' ')
          CALL UTTCPR('CPU.NMTIME.TM1',4,ZR(JTMP1))
          TIME          = ZR(JTMP1+3-1) - ZR(JTDEB+5-1)
          ZR(JTDEB+5-1) = 0.D0
          CALL NMRTIM(SDTIME,TIMER,TIME  )
        ELSEIF (TIMER.EQ.'ASSE_MATR') THEN
          CALL UTTCPU('CPU.NMTIME.TM2','FIN',' ')
          CALL UTTCPR('CPU.NMTIME.TM2',4,ZR(JTMP2))
          TIME          = ZR(JTMP2+3-1) - ZR(JTDEB+6-1)
          ZR(JTDEB+6-1) = 0.D0
          CALL NMRTIM(SDTIME,TIMER,TIME  )
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF  
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF    
C
      CALL JEDEMA()
      END
