      SUBROUTINE EXSTAT( ISTAT , ICOND , XTT )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            ISTAT , ICOND
      REAL*8                             XTT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 21/02/2002   AUTEUR D6BHHJP J.P.LEFEBVRE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C     IMPRESSIONS DE STATISTIQUES  (TEMPS, ... )
C     ------------------------------------------------------------------
C IN  ISTAT : IS :
C             = 1  TEMPS ORIGINE
C             = 2  TEMPS FINAL AVEC IMPRESSION DU DELTA
C             = 3  TEMPS FINAL AVEC IMPRESSION DU DELTA ET RESUME
C FIN EXSTAT
C     ------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     --- VARIABLES GLOBALES -------------------------------------------
      CHARACTER*24    KINFO , KRESU, KSTAT
      COMMON /GCUCC1/ KINFO , KRESU, KSTAT
C
      INTEGER      LONUTI, LONMAX, INDIC
      CHARACTER*8  NOMRES
      CHARACTER*16 CONCEP, NOMCMD
      REAL*8       TPSD(6),TPSF(6),TP
      CHARACTER*1 K1BID
      SAVE         TPSD,TPSF,TP
      DATA         TP/0.D0/
C
      CALL JEMARQ()
      INDIC = 0
      IF(ISTAT .EQ. 3) THEN
         ISTAT = 2
         INDIC = 1
      ENDIF
      IF ( ICOND .EQ. 0 ) THEN
        IF(ISTAT.EQ.1) THEN
          CALL UTTCPU(-1,'INIT',6,TPSD)
          CALL UTTCPU(-1,'DEBUT',6,TPSD)
          XTT = TPSD(1)
        ELSE IF(ISTAT.EQ.2) THEN
          CALL UTTCPU(-1,'FIN',6,TPSF)
          XTT = TPSD(1) - TPSF(1)
        ENDIF
      ELSE
        CALL UTTCPU(0,'   ',1,TPSR)
        XTT= ABS(TP-TPSR)
        TP = TPSR
      ENDIF
      IF ( ICOND.EQ.0 .AND. ISTAT .EQ. 2 ) THEN
         IFM = IUNIFI('MESSAGE')
         IF (IFM.NE.0) THEN
            CALL GETRES ( NOMRES, CONCEP, NOMCMD)
            WRITE(IFM,'(1X,3A,F10.2,A,F8.2,A)')
     +           ' %  FIN COMMANDE: ',NOMCMD,
     +           ' DUREE TOTALE: ',TPSF(3),' s (SYST:',TPSF(6),' s)'
         ENDIF
         CALL JEEXIN ( KSTAT, IST )
         IF (IST.EQ.0 .AND. INDIC.EQ.1) THEN
C        --- IMPRESSION DANS LE FICHIER MESSAGE DES ---
C        --- STATISTIQUES POUR LA COMMANDE FIN ---
C        --- CETTE ROUTINE DOIT ETRE APPELEE PAR OP9999 ---
            IFR = IUNIFI('RESULTAT')
            WRITE(IFR,'(1X,''*'',1X,A,3(1X,'':'',1X,F10.2),1X,''*'')')
     +           'FIN             ',
     +           TPSF(5), TPSF(6), TPSF(3)
            WRITE(IFR,'(1X,59(''*''))')
         ELSEIF (IST .NE. 0) THEN
            CALL GETRES ( NOMRES, CONCEP, NOMCMD)
            CALL JELIRA ( KSTAT , 'LONUTI', LONUTI , K1BID )
            CALL JELIRA ( KSTAT , 'LONMAX', LONMAX , K1BID )
            CALL JEVEUO(KSTAT,'E',LSTAT)
            IF (LONUTI .GE. LONMAX) THEN
               LONMAX = LONMAX + 100
               CALL JUVECA(KSTAT,LONMAX)
               CALL JEVEUO(KSTAT,'E',LSTAT)
            ENDIF
            LONUTI = LONUTI + 1
            CALL JEECRA ( KSTAT , 'LONUTI',  LONUTI , ' ' )
            WRITE(ZK80(LSTAT+LONUTI-1),'(1X,''*'',1X,A,1X
     +           ,3('':'',1X,F10.2,1X),''*'')') NOMCMD, TPSF(5)
     +           ,TPSF(6), TPSF(3)
        ENDIF
      ENDIF
C
      CALL JEDEMA()
      END
