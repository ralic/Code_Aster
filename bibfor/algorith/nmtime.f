      SUBROUTINE NMTIME(PHASEZ,TIMEZ ,SDTIME,VALL  ,VALR  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/01/2008   AUTEUR ABBAS M.ABBAS 
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
      IMPLICIT NONE
      CHARACTER*(*) PHASEZ
      CHARACTER*(*) TIMEZ
      CHARACTER*24  SDTIME
      LOGICAL       VALL
      REAL*8        VALR
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
C IN  PHASE  : TYPE D'ACTION
C                'CREA'  CREATION DE LA SD TIMER
C                'INIT'  INITIALISATION DU TIMER
C                'DEBUT' LANCEMENT DU TIMER
C                'FIN'   ARRET DU TIMER
C                'MES'   CALCUL DES CRITERES D'ARRET SUR TEMPS
C                'VAL'   RECUPERE VALEUR DU TEMPS
C IN  TIMER  : NOM DU TIMER
C                'PAS'   TIMER POUR UN PAS DE TEMPS
C                'ITE'   TIMER POUR UNE ITERATION DE NEWTON
C                'ARC'   TIMER POUR L'ARCHIVAGE
C                'RES'   TIMER POUR JOB TOTAL
C OUT VALL   : RESULTAT DE L'ACTION MES
C OUT VALR   : RESULTAT DE L'ACTION VAL
C IN  SDTIME : SD TIMER
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER      ITPAS,ITITE,ITARC
      PARAMETER    (ITPAS=1,ITITE=2,ITARC=3)
C
      CHARACTER*24 TIMPAS,TIMITE,TIMARC,TIMINF
      INTEGER      JTPAS,JTITE,JTARC,JTINF  
      CHARACTER*3  TIMER 
      CHARACTER*5  PHASE
      INTEGER      IFM,NIV 
C      
C ----------------------------------------------------------------------
C      
      CALL JEMARQ()
      CALL INFDBG('MECA_NON_LINE',IFM,NIV)
C    
C --- INITIALISATIONS
C
      TIMPAS = SDTIME(1:19)//'.TPAS'
      TIMITE = SDTIME(1:19)//'.TITE'
      TIMARC = SDTIME(1:19)//'.TARC'
      TIMINF = SDTIME(1:19)//'.INFO'
      TIMER  = TIMEZ  
      PHASE  = PHASEZ   
      VALL   = .FALSE.
      VALR   = 0.D0
C
      IF (PHASE.EQ.'CREA') THEN
        IF (NIV.GE.2) THEN
          WRITE (IFM,*) '<MECANONLINE> ... CREATION SD TIMER'
        ENDIF
        CALL WKVECT(TIMPAS,'V V R',4,JTPAS)
        CALL WKVECT(TIMITE,'V V R',4,JTITE)
        CALL WKVECT(TIMARC,'V V R',4,JTARC)
        CALL WKVECT(TIMINF,'V V R',4,JTINF)
        GOTO 999
      ELSE
        CALL JEVEUO(TIMPAS,'E',JTPAS)
        CALL JEVEUO(TIMITE,'E',JTITE)
        CALL JEVEUO(TIMARC,'E',JTARC)
        CALL JEVEUO(TIMINF,'E',JTINF)      
      ENDIF  
C            
      IF (PHASE.EQ.'INIT') THEN
        CALL UTTCPU(ITPAS,'INIT',4,ZR(JTPAS))
        CALL UTTCPU(ITITE,'INIT',4,ZR(JTITE))
        CALL UTTCPU(ITARC,'INIT',4,ZR(JTARC))  
            
      ELSEIF (PHASE.EQ.'DEBUT') THEN
        IF (TIMER.EQ.'PAS') THEN
          CALL UTTCPU(ITPAS,'DEBUT',4,ZR(JTPAS)) 
          ZR(JTINF+1-1) = ZR(JTPAS+3-1)
        ELSEIF (TIMER.EQ.'ITE') THEN
          CALL UTTCPU(ITITE,'DEBUT',4,ZR(JTITE))
        ELSEIF (TIMER.EQ.'ARC') THEN
          CALL UTTCPU(ITARC,'DEBUT',4,ZR(JTARC))
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF             

      ELSEIF (PHASE.EQ.'FIN') THEN
        IF (TIMER.EQ.'PAS') THEN
          CALL UTTCPU(ITPAS,'FIN',4,ZR(JTPAS))
          VALR = ZR(JTPAS+3-1) - ZR(JTINF+1-1)
        ELSEIF (TIMER.EQ.'ITE') THEN
          CALL UTTCPU(ITITE,'FIN',4,ZR(JTITE))
        ELSEIF (TIMER.EQ.'ARC') THEN
          CALL UTTCPU(ITARC,'FIN',4,ZR(JTARC))
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF 
        
      ELSEIF (PHASE.EQ.'MES') THEN  
        IF (TIMER.EQ.'ITE') THEN
          IF ((2.D0*ZR(JTITE+4-1)).LE.
     &         (0.95D0*ZR(JTITE+1-1)-ZR(JTARC+4-1))) THEN
            VALL = .FALSE.
          ELSE
            VALL = .TRUE.
          ENDIF
        ELSEIF (TIMER.EQ.'PAS') THEN
          IF (ZR(JTPAS+4-1) .GT. 0.90D0*ZR(JTPAS+1-1)) THEN
            VALL = .TRUE.
          ELSE
            VALL = .FALSE.
          ENDIF  
        ELSE
          CALL ASSERT(.FALSE.)  
        ENDIF     
      ELSEIF (PHASE.EQ.'VAL') THEN
        IF (TIMER.EQ.'ITE') THEN
          VALR = ZR(JTITE+4-1)
        ELSEIF (TIMER.EQ.'PAS') THEN
          VALR = ZR(JTPAS+4-1)  
        ELSEIF (TIMER.EQ.'RES') THEN
          VALR = ZR(JTITE+1-1)          
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF                
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
  999 CONTINUE      
C
      CALL JEDEMA()
      END
