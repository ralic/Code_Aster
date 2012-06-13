      SUBROUTINE DLTP0 (T0,NUME)
      IMPLICIT  REAL*8  (A-H,O-Z)
      INCLUDE 'jeveux.h'
      REAL*8    T0
      INTEGER   NUME
C     ------------------------------------------------------------------
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ------------------------------------------------------------------
C OUT : T0   : INSTANT INITIAL
C OUT : NUME : NUMERO D'ORDRE DE REPRISE
C     ------------------------------------------------------------------
      INTEGER       VALI
      REAL*8        VALR
      CHARACTER*8   K8B, NOMRES, DYNA, LI, CRIT, CTYPE
      CHARACTER*16  TYPRES, NOMCMD
      CHARACTER*24  VALK            
      COMPLEX*16    C16B
      INTEGER      IARG
C     -----------------------------------------------------------------
      CALL JEMARQ()
      CALL GETRES(NOMRES,TYPRES,NOMCMD)
C
C     --- EST-ON EN REPRISE ? ---
C
C     INITIALISATION DE T0 PAR DEFAUT

      CALL GETVID('ETAT_INIT','RESULTAT',1,IARG,1,DYNA,NDY)
      IF ( NDY .NE. 0 ) THEN
         CALL GETVIS('ETAT_INIT','NUME_ORDRE' ,1,IARG,1,NUME,NNI)
         IF ( NNI .EQ. 0 ) THEN
            CALL GETVR8('ETAT_INIT','INST_INIT',1,IARG,1,TEMPS,NT)
            IF ( NT .EQ. 0 ) THEN
               CALL RSORAC(DYNA,'DERNIER',IBID,TEMPS,K8B,C16B,
     &                                         PREC,CRIT,NUME,1,NBTROU)
               IF (NBTROU.NE.1) THEN
                CALL U2MESS('F','ALGORITH3_24')
               ENDIF
            ELSE
               CALL GETVR8('ETAT_INIT','PRECISION',1,IARG,1,PREC ,NP)
               CALL GETVTX('ETAT_INIT','CRITERE'  ,1,IARG,1,CRIT ,NC)
               CALL RSORAC(DYNA,'INST',IBID,TEMPS,K8B,C16B,
     &                                        PREC,CRIT,NUME,1,NBTROU)
               IF (NBTROU.LT.0) THEN
                  VALK = DYNA
                  VALR = TEMPS
                  VALI = -NBTROU
      CALL U2MESG('F', 'ALGORITH12_83',1,VALK,1,VALI,1,VALR)
               ELSEIF (NBTROU.EQ.0) THEN
                  VALK = DYNA
                  VALR = TEMPS
                  CALL U2MESG('F', 'ALGORITH12_84',1,VALK,0,0,1,VALR)
               ENDIF
            ENDIF
         ELSE
C           --- VERIFICATION QUE NUME EXISTE ---
            CALL RSORAC(DYNA,'LONUTI',IBID,R8B,K8B,C16B,R8B,K8B,
     &                                                 NBORDR,1,IBID)
            CALL WKVECT('&&OP0048.NUME_ORDRE','V V I',NBORDR,JORDR)
            CALL RSORAC(DYNA,'TOUT_ORDRE',IBID,R8B,K8B,C16B,R8B,K8B,
     &                                        ZI(JORDR),NBORDR,IBID)
            DO 10 I = 1,NBORDR
               IF (ZI(JORDR+I-1).EQ.NUME) GOTO 12
 10         CONTINUE
            CALL U2MESK('F','ALGORITH3_36',1,DYNA)
 12         CONTINUE
         ENDIF
C
C        --- RECUPERATION DE L'INSTANT ---
         CALL RSADPA(DYNA,'L',1,'INST',NUME,1,JADR,CTYPE)
         T0 = ZR(JADR)
      ELSE
C
C     --- DEFINITION DES INSTANTS DE CALCUL A PARTIR DE "LIST_INST" ---
C
         CALL GETVID('INCREMENT','LIST_INST',1,IARG,1,LI,N1)
         IF (N1.NE.0) THEN
            CALL JEVEUO(LI//'           .BINT','L',JBINT)
            T0 = ZR (JBINT)
         ELSE
C
C
C     --- DEFINITION DE L'INSTANT INITIAL AVEC "INST_INIT" ---
C
            T0 = 0.D0
            CALL GETVR8('INCREMENT','INST_INIT',1,IARG,1,T0,NP)
            IF (NP.EQ.0) CALL U2MESS('I','ALGORITH5_62')
         ENDIF
      ENDIF
C
      CALL JEDEMA()
      END
