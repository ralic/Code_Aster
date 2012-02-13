      SUBROUTINE RFBEFL ( BASE )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       BASE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 14/02/2012   AUTEUR COURTOIS M.COURTOIS 
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
C
C     OPERATEUR "RECU_FONCTION"  MOT CLE "BASE_ELAS_FLUI"
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      IFM,NIV, LXLGUT
      CHARACTER*4  INTERP(2)
      CHARACTER*8  K8B, BASEFL, PARAY, TTORDR
      CHARACTER*16 NOMCMD, TYPCON
      CHARACTER*19 NOMFON
      CHARACTER*24 VITE , NUMEO , NUMO , FREQ
      INTEGER      IARG
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL INFMAJ
      CALL INFNIV(IFM,NIV)
C
      CALL GETRES ( NOMFON , TYPCON , NOMCMD )
      BASEFL = BASE
      NBV = 0
      INTERP(1) = 'NON '
      INTERP(2) = 'NON '
C
C     --- RECUPERATION DES ENTREES ---
C
      CALL GETVTX(' ','PARA_Y'    ,0,IARG,1,PARAY ,N2)
      CALL GETVTX(' ','TOUT_ORDRE',0,IARG,1,TTORDR,N3)
      CALL GETVIS(' ','NUME_MODE' ,0,IARG,1,NUMMOD,N4)
      CALL GETVTX(' ','INTERPOL'  ,0,IARG,2,INTERP,N5)
      IF ( N5 .EQ. 1 ) INTERP(2) = INTERP(1)
C
C     --- REMPLISSAGE DU .PROL ---
C
      CALL ASSERT(LXLGUT(NOMFON).LE.24)
      CALL WKVECT(NOMFON//'.PROL','G V K24',6,LPRO)
      ZK24(LPRO)   = 'FONCTION'
      ZK24(LPRO+1) = INTERP(1)//INTERP(2)
      ZK24(LPRO+2) = 'VITE_FLU'
      ZK24(LPRO+3) = PARAY(1:4)
      ZK24(LPRO+4) = 'EE      '
      ZK24(LPRO+5) = NOMFON
C
C     --- RECUPERATION DES OJB ---
      VITE = BASEFL//'           .VITE'
      CALL JELIRA(VITE,'LONUTI',NPV,K8B)
      CALL JEVEUO(VITE,'L',LVITE)
      FREQ = BASEFL//'           .FREQ'
      CALL JEVEUO(FREQ,'L',LFREQ)
      NUMO = BASEFL//'           .NUMO'
      CALL JELIRA(NUMO,'LONUTI',NBM,K8B)
      CALL JEVEUO(NUMO,'L',LNUMO)
C
C   --- VERIFICATION DE LA VALIDITE DES NUMEROS D'ORDRE DES VITESSES -
C
      IF ( TTORDR .NE. 'OUI' ) THEN
         NUMEO = '&&RFBEFL.NUME_ORDRE'
         CALL GETVIS(' ','NUME_ORDRE',0,IARG,0,IBID,NBNO)
         NBNO = -NBNO
         CALL WKVECT(NUMEO,'V V I',NBNO,INUMEO)
         CALL GETVIS(' ','NUME_ORDRE',0,IARG,NBNO,ZI(INUMEO),N1)
         MIN = ZI(INUMEO)
         DO 10 I = 1,NBNO
            ID = MIN - ZI(INUMEO + I - 1)
            IF ( ID .GT. 0 ) MIN = ZI(INUMEO + I - 1)
 10      CONTINUE
         IF ( MIN .GT. NPV ) THEN
            CALL U2MESS('F','UTILITAI4_9')
         ENDIF
      ENDIF
C
C     --- DETERMINATION DU NUMERO D'ORDRE DU MODE VOULU ---
C
      DO 20 IMOD = 1 , NBM
         ID = NUMMOD - ZI(LNUMO + IMOD - 1)
         IF ( ID .EQ. 0 ) GOTO 30
 20   CONTINUE
      CALL U2MESS('F','UTILITAI4_10')
 30   CONTINUE
C
C     --- CAS 1 : REMPLISSAGE POUR TOUS LES NUMEROS D'ORDRE ---
C
      IF ( TTORDR .EQ. 'OUI' ) THEN
         CALL WKVECT(NOMFON//'.VALE','G V R',2*NPV,LVAR)
         LFON = LVAR + NPV
         DO 40 I = 1,NPV
            ZR(LVAR + I - 1) = ZR(LVITE + I - 1)
            IF (PARAY(1:4).EQ.'FREQ') THEN
               IND = 2*NBM*(I - 1) + 2*(IMOD - 1)
               ZR(LFON + I - 1) = ZR(LFREQ + IND)
            ELSE
               IND = 2*NBM*(I - 1) + 2*(IMOD - 1) + 1
               ZR(LFON + I - 1) = ZR(LFREQ + IND)
            ENDIF
 40      CONTINUE
      ELSE
C
C     --- CAS 2 : REMPLISSAGE POUR UNE LISTE DE NUMEROS D'ORDRE ---
C
C---------2.1 ON ORDONNE LA LISTE DES NUMEROS D'ORDRE
         IF (NBNO.GT.1) THEN
            DO 41 I = 1,NBNO
               IND = I
               MIN = ZI(INUMEO + I - 1)
               DO 42 J = I+1,NBNO
                  ID = MIN - ZI(INUMEO + J - 1)
                  IF (ID.GT.0) THEN
                     IND = J
                     MIN = ZI(INUMEO + J - 1)
                  ENDIF
 42            CONTINUE
               ZI(INUMEO + IND - 1) = ZI(INUMEO + I - 1)
               ZI(INUMEO + I - 1) = MIN
 41         CONTINUE
         ENDIF
C
C---------2.2 DETERMINATION DU NOMBRE DE NUMEROS D'ORDRE VALIDES
         IF (NBNO.GT.1) THEN
            DO 43 I = 1,NBNO
              IF (ZI(INUMEO + I - 1).GT.NPV) GOTO 44
              NBV = NBV + 1
 43         CONTINUE
 44         CONTINUE
         ELSE
            NBV = 1
         ENDIF
C
C---------2.3 REMPLISSAGE
         CALL WKVECT(NOMFON//'.VALE','G V R',2*NBV,LVAR)
         LFON = LVAR + NBV
         DO 45 I = 1,NBV
            IND1 = ZI(INUMEO + I - 1)
            ZR(LVAR + I - 1) = ZR(LVITE + IND1 - 1)
            IF (PARAY(1:4).EQ.'FREQ') THEN
               IND2 = 2*NBM*(IND1 - 1) + 2*(IMOD - 1)
               ZR(LFON + I - 1) = ZR(LFREQ + IND2)
            ELSE
               IND2 = 2*NBM*(IND1 - 1) + 2*(IMOD - 1) + 1
               ZR(LFON + I - 1) = ZR(LFREQ + IND2)
            ENDIF
 45      CONTINUE
         CALL JEDETR(NUMEO)
C
      ENDIF
C
      CALL FOATTR(' ',1,NOMFON)
C
C     --- VERIFICATION QU'ON A BIEN CREER UNE FONCTION ---
C         ET REMISE DES ABSCISSES EN ORDRE CROISSANT
      CALL ORDONN(NOMFON,0)
C
      CALL TITRE
      IF (NIV.GT.1) CALL FOIMPR(NOMFON,NIV,IFM,0,K8B)

      CALL JEDEMA()
      END
