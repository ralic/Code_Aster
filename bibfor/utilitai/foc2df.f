      SUBROUTINE FOC2DF ( NOM, NOMLIS, IMPR, NBVAL )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       NOM, NOMLIS
      INTEGER                          IMPR, NBVAL
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 21/02/96   AUTEUR VABHHTS J.PELLET 
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
C     CREATION DE LISTE PAR DEFAUT SUR LA BASE VOLATILE
C     ------------------------------------------------------------------
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
      PARAMETER     (NBFREQ=150,   NBAMOR = 3)
      REAL*8    FREQ(NBFREQ), AMOR(NBAMOR)
C     ------------------------------------------------------------------
      DATA  AMOR/0.02D0,0.05D0,0.10D0/
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      IF ( NOM .EQ. 'FREQ' ) THEN
C
C         --- GENERATION DES FREQUENCES PAR DEFAUT --
C         150 FREQUENCES SONT GENEREES SELON LE MODE SUIVANT.
C             LA PREMIERE VAUT  0.2
C          ET DE LA   2 A  57: PAR PAS DE 0.05
C             DE LA  58 A  65: PAR PAS DE 0.075
C             DE LA  66 A  79: PAR PAS DE 0.10
C             DE LA  80 A 103: PAR PAS DE 0.125
C             DE LA 104 A 131: PAR PAS DE 0.25
C             DE LA 132 A 137: PAR PAS DE 0.50
C             DE LA 138 A 141: PAR PAS DE 1.
C             DE LA 142 A 150: PAR PAS DE 1.5
          FREQ(1)=0.2D0
          DO 1 I= 2, 57
             FREQ(I)=FREQ(I-1) + 0.05D0
    1     CONTINUE
          DO 2 I =  58,  65
             FREQ(I)=FREQ(I-1) + 0.075D0
    2     CONTINUE
          DO 3 I =  66,  79
             FREQ(I)=FREQ(I-1) + 0.10D0
    3     CONTINUE
          DO 4 I =  80, 103
             FREQ(I)=FREQ(I-1) + 0.125D0
    4     CONTINUE
          DO 5 I = 104, 131
             FREQ(I)=FREQ(I-1) + 0.25D0
    5     CONTINUE
          DO 6 I = 132, 137
             FREQ(I)=FREQ(I-1) + 0.50D0
    6     CONTINUE
          DO 7 I = 138, 141
             FREQ(I)=FREQ(I-1) + 1.D0
    7     CONTINUE
          DO 8 I = 142, 150
             FREQ(I)=FREQ(I-1) + 1.5D0
    8     CONTINUE
C
         NBVAL = NBFREQ
         CALL WKVECT(NOMLIS,'V V R',NBVAL,LDEC)
         DO 10 I=1,NBFREQ
            ZR(LDEC+I-1) = FREQ(I)
   10    CONTINUE
C
         IF (IMPR .GT. 0 ) THEN
            CALL UTDEBM('I','VALEURS PAR DEFAUT',' - ')
            CALL UTIMPI('S','GENERATION DE ',1,NBVAL)
            CALL UTIMPK('S',' FREQUENCES:',0,' ')
            CALL UTIMPR('L',' ',NBVAL,ZR(LDEC))
            CALL UTFINM()
         ENDIF
      ELSEIF ( NOM .EQ. 'AMOR' ) THEN
         NBVAL = NBAMOR
         CALL WKVECT(NOMLIS,'V V R',NBVAL,LDEC)
         DO 20 I=1,NBAMOR
            ZR(LDEC+I-1) = AMOR(I)
   20    CONTINUE
         IF (IMPR .GT. 0 ) THEN
            CALL UTDEBM('I','VALEURS PAR DEFAUT',' - ')
            CALL UTIMPI('S','GENERATION DE ',1,NBVAL)
            CALL UTIMPK('S',' AMORTISSEMENTS:',0,' ')
            CALL UTIMPR('L',' ',NBVAL,ZR(LDEC))
            CALL UTFINM()
         ENDIF
      ELSE
         CALL UTDEBM('F','FOC2DF','PARAMETRE D''ENTREE INCONNU')
         CALL UTIMPK('S',' ',1, NOM)
         CALL UTFINM()
      ENDIF
      CALL JEDEMA()
      END
