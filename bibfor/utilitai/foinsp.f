      SUBROUTINE FOINSP ( METHOD,NOMFON,AMOR,CRIT,EPSI,FINI,
     &                                            FFIN,ISPEC )
      IMPLICIT NONE
      CHARACTER*(*)       METHOD, NOMFON, CRIT
      REAL*8              EPSI, FINI, FFIN, ISPEC, AMOR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 07/03/2001   AUTEUR CIBHHLV L.VIVAN 
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
C     ------------------------------------------------------------------
C
C     CALCUL DE L INTENSITE SPECTRALE (IH) D UN SPECTRE DE REPONSE 
C     D'OSCILLATEUR EN PSEUDO VITESSE (SROV)
C
C IN      METHOD : K : METHODE D'INTEGRATION (TRAPEZE OU SIMPSON)
C IN      NOMFON : K : NOM DE LA FONCTION NAPPE REPRESENTANT LE SROV
C IN      AMOR   : R : COEFFICIENT D'AMORTISSEMENT DU SROV
C IN      CRIT   : K : CRITERE D'ERREUR (ABSOLUE OU RELATIVE)
C IN      EPSI   : R : TOLERENCE SUR LA PRECISION DES REELS 
C IN_OUT  FINI   : R : BORNE INFERIEURE DE L'INTEGRATION (DEF=0.4HZ)
C IN_OUT  FFIN   : R : BORNE SUPERIEURE DE L'INTEGRATION (DEF=10 HZ)
C OUT     ISPEC  : R : INTENSITE SPECTRALE (IH) DU SROV
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNUM,JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER        NBVAL, LVAR, LFON, NBPTS, IER
      REAL*8         F0, F1, FF0, FF1, SIGN, AUX, CSTE, VALPU(2)
      INTEGER        INTRP0, INTRP1, IDEB, IFIN, IABSS
      INTEGER        NBVALU, LABSS, LORDO , LSPEC, LFINI, LFFIN  
      CHARACTER*1    K1BID
      CHARACTER*8    NOMPU(2)
      CHARACTER*19   NOMFI
      CHARACTER*24   VALE
C     ------------------------------------------------------------------
      DATA  NOMPU / 'AMOR' , 'FREQ'    /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      NOMFI = NOMFON
      VALPU(1) = AMOR
C
C     ---  NOMBRE DE POINTS ----
      VALE = NOMFI//'.VALE'
      CALL JELIRA(VALE,'LONT',NBVAL,K1BID)
      CALL JEVEUO(JEXNUM(VALE,1),'L',LVAR)
      NBPTS = NBVAL/2
      LFON  = LVAR + NBPTS
C
C     ----------------------------------------------------------------
      LFINI = 1
      F0 = FINI
      LFFIN = 1
      F1 = FFIN     
C
      IF (METHOD.EQ.'SIMPSON' .OR. METHOD.EQ.'TRAPEZE'
     &                        .OR. METHOD.EQ.'  ')      THEN
C
         SIGN = 1.D0
         IF ( F0 .GT. F1 ) THEN
            AUX = F1
            F1  = F0
            F0  = AUX
            SIGN = - SIGN 
         ENDIF
C
         CALL FONOC0 ( ZR(LVAR), FINI, LFINI, FFIN, LFFIN, CRIT, EPSI,
     &                     NBPTS, F0, IDEB, INTRP0, F1, IFIN, INTRP1 )
C
C        --- ALLOCATION DES TABLEAUX AUXILIARES ---
C
         NBVALU = IFIN - IDEB + 1
         IF( INTRP0 .EQ. 1)  NBVALU = NBVALU + 1
         IF( INTRP1 .EQ. 1)  NBVALU = NBVALU + 1
         IF(IDEB .EQ. 0 .AND. IFIN .EQ. 0)  NBVALU = 2
C
         CALL WKVECT('&&FOINSP.ABSS', 'V V R',NBVALU,LABSS)
         CALL WKVECT('&&FOINSP.ORDO', 'V V R',NBVALU,LORDO)
         CALL WKVECT('&&FOINSP.ISPEC',  'V V R',NBVALU,LSPEC)
C
C        --- STOKAGE DES FREQUENCES COMPRISES ENTRE F0 ET F1 ---
C
         ZR(LABSS) = F0
         DO 55 IABSS = 2, NBVALU-1
            ZR(LABSS+IABSS-1) = ZR(LVAR+IDEB+IABSS-2) 
  55     CONTINUE        
         ZR(LABSS+NBVALU-1) = F1
C
C        --- STOKAGE DES VALEURS DU SROV DIVISE PAR LE CARRE DE ---
C        --- LA FREQUENCE POUR LES FREQUENCES COMPRISES ENTRE   ---
C        --- F0 ET F1                                           ---
C
         DO 66 IABSS = 2, NBVALU-1
          ZR(LORDO+IABSS-1)=ZR(LFON+IDEB+IABSS-2)/(ZR(LABSS+IABSS-1))**2
  66     CONTINUE
         IF(INTRP0 .EQ. 0) THEN
             FF0 =  ZR(LFON+IDEB-1)    
         ELSE
             VALPU(2) = F0
             CALL FOINTE('F ',NOMFON, 2, NOMPU, VALPU, FF0, IER )
         ENDIF
         ZR(LORDO) = FF0 / F0**2
C
         IF(INTRP1 .EQ. 0) THEN
             FF1 =  ZR(LFON+IFIN-1)   
         ELSE
             VALPU(2) = F1
             CALL FOINTE('F ',NOMFON, 2, NOMPU, VALPU, FF1, IER )
         ENDIF
         ZR(LORDO+NBVALU-1) = FF1 / F1**2
C
C        -- CALCUL DE L INTENSITE SPECTRALE --
         CSTE = 0.D0
         CALL FOC2IN(METHOD,NBVALU,ZR(LABSS),ZR(LORDO),CSTE,ZR(LSPEC)) 
         ISPEC = ZR(LSPEC+NBVALU-1)
C
         CALL JEDETR('&&FOINSP.ABSS')
         CALL JEDETR('&&FOINSP.ORDO')
         CALL JEDETR('&&FOINSP.ISPEC')
      ENDIF
C
C     ----------------------------------------------------------------
C
C     --- INTEGRATION ---
      IF (METHOD.EQ.'SIMPSON') THEN
         WRITE(6,'(1X,A)') 'INTEGRATION D"ORDRE 2 (METHODE SIMPSON)'
         CALL UTMESS('I',METHOD,'METHODE D''INTEGRATION DE SIMPSON'
     &   //' PEUT PROVOQUER DES OSCILLATIONS SI LA COURBE A '
     &   //'INTEGRER N''EST PAS ASSEZ DISCRETISEE OU REGULIERE. '
     &   //' FAIRE ATTENTION AVEC LES ACCELEROGRAMMES.')
      ELSE IF ( METHOD .EQ. 'TRAPEZE' .OR. METHOD .EQ. '  ' ) THEN
         WRITE(6,'(1X,A)') 'INTEGRATION D"ORDRE 1 (METHODE TRAPEZE)'
      ELSE
         CALL UTMESS('F',METHOD,'METHODE D''INTEGRATION INEXISTANTE.')
      ENDIF
C
      CALL JEDEMA()
      END
