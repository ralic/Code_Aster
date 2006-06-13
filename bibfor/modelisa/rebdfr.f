      SUBROUTINE REBDFR(FREQ,NFI,NFF,FREQI,FREQF,NMODI,NMODF,NBM,NPV)
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 28/09/98   AUTEUR KXBADNG F.BEAUD 
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
C-----------------------------------------------------------------------
C     BUT : DEFINIR LA BANDE DE FREQUENCE POUR LE CALCUL DU SPECTRE 
C     ---   D EXCITATION (ROUTINE APPELEE PAR OP0146).
C
C     - SI LA FREQUENCE INITIALE ET LA FREQUENCE FINALE NE SONT PAS
C     DEFINIES PAR L UTILISATEUR, ELLES SONT CALCULEES A PARTIR DES
C     FREQUENCES MODALES.
C     FREQI = FMIN/2
C     FREQF = FMAX/2
C     (FMIN ET FMAX SONT RESPECTIVEMENT LA PREMIERE ET LA DERNIERE FREQ.
C      MODALES PRISES EN COMPTE).
C     - EN FONCTION DE FREQI ET FREQF DEFINIES PAR L UTILISATEUR,
C     ON RECHERCHE LES MODES PRIS EN COMPTE.    
C-----------------------------------------------------------------------
C     IN  : FREQ  : CARACTERIST. MODALES DE LA BASE DE CONCEPT MELASFLU
C     IN  : NFI   : SI NFI .EQ. 0 , FREQI EST CALCULEE
C     IN  : NFF   : SI NFF .EQ. 0 , FREQF EST CALCULEE
C     I/O : FREQI : FREQUENCE INITIALE DE LA BANDE DE FREQUENCE
C     I/O : FREQF : FREQUENCE FINALE   DE LA BANDE DE FREQUENCE
C     OUT : NMODI : NUMERO D ORDRE DU PREMIER MODE PRIS EN COMPTE
C     OUT : NMODF : NUMERO D ORDRE DU DERNIER MODE PRIS EN COMPTE
C     IN  : NBM   : NBR. DE MODES DE LA BASE MODALE
C     IN  : NPV   : NOMBRE DE VITESSES ETUDIEES
C-----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER      NFI, NFF, NMODI, NMODF
      REAL*8       FREQI, FREQF, FREQ(2,NBM,NPV)
C
C-----------------------------------------------------------------------
      NMODI = 0
      NMODF = 0
      FRQMIN = R8MAEM()
      FRQMMA = 0.D0
      FRQMAX = 0.D0
C
C 1. --- DECOUPAGE DE LA BANDE DE FREQUENCE ---
C
C ---- RECHERCHE DE LA FREQUENCE INITIALE  ET
C                DE LA FREQUENCE FINALE
C
      IF (NFI .EQ. 0) THEN
        DO 10 I = 1,NPV
          IF (FREQ(1,1,I) .GT. 0.D0) THEN
            FRQMIN = MIN(FRQMIN,FREQ(1,1,I))
            FRQMMA = MAX(FRQMMA,FREQ(1,1,I))
          ENDIF
 10     CONTINUE
        NMODI = 1
        FREQI = FRQMIN/2.D0
      ENDIF
C
      IF (NFF .EQ. 0) THEN
        DO 20 I = 1,NPV
          IF (FREQ(1,NBM,I) .GT. 0.D0) THEN
            FRQMAX = MAX(FRQMAX,FREQ(1,NBM,I))
          ENDIF
 20     CONTINUE
        NMODF = NBM
        FREQF = FRQMAX + ( FRQMMA/2.D0)
      ENDIF
C   
C 2. ---- ON SECTIONNE LES MODES COMPRIS ENTRE FREQI ET FREQF
C
      IF (NMODI .EQ. 0) THEN
        DO 30 I=1,NBM
          IND = 1
          DO 40 J=1,NPV
            IF (FREQ(1,I,J) .LE. FREQI) THEN
              IND = 0
            ENDIF
 40       CONTINUE
          IF (IND .EQ. 1) THEN
            NMODI = I
            GOTO 900
          ENDIF
 30     CONTINUE
      ENDIF
 900  CONTINUE
C
      IF (NMODF .EQ. 0) THEN
        DO 50 I=NBM,1,-1
          IND = 1
          DO 60 J=1,NPV
            IF (FREQ(1,I,J) .GT. FREQF) THEN
              IND = 0
            ENDIF
 60       CONTINUE
          IF (IND .EQ. 1) THEN
            NMODF = I
            GOTO 901
          ENDIF
 50     CONTINUE
      ENDIF
901   CONTINUE
C
      IF (NMODF .LT. NMODI) THEN 
        CALL UTMESS('F','REBDFR','MAUVAISE DEFINITION DE LA PLAGE'//
     +              ' DE FREQUENCE, AUCUN MODE PRIS EN COMPTE')
      ENDIF
C
      END
