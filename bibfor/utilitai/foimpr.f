      SUBROUTINE FOIMPR(NOMF,IMPR,IUL,IND,FONINS)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     NOMF,             FONINS
      INTEGER                IMPR,IUL,IND
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 16/06/2004   AUTEUR DURAND C.DURAND 
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
C     ROUTINE D'IMPRESSION D'UNE FONCTION SUR UN FICHIER
C     ----------------------------------------------------------------
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
      CHARACTER*8   K8B
      CHARACTER*16  NOMCMD
      CHARACTER*19  NOMFON, NOMF1, LISTR
      CHARACTER*24  PROL, VALE, PARA
      CHARACTER*24  NOMPAR, NOMRES, TITR
      COMPLEX*16    RESUC
      INTEGER       NBPU
      CHARACTER*8   NOMPU
C     ------------------------------------------------------------------
      CALL JEMARQ()
      IF ( IMPR .LE. 0) GOTO 9999
      IF ( IUL .LE. 0 ) THEN
         CALL GETRES(K8B,K8B,NOMCMD)
         CALL UTMESS('A',NOMCMD,'UNITE LOGIQUE INEXISTANTE')
         GOTO 9999
      ENDIF
      LISTR = FONINS
      NOMF1 = '&&FOIMPR'
C
C     --- NOM DE LA FONCTION A EDITER ---
      NOMFON = NOMF
      PROL   = NOMFON//'.PROL'
      VALE   = NOMFON//'.VALE'
      PARA   = NOMFON//'.PARA'
      TITR   = NOMFON//'.TITR'
C
C     --- IMPRESSION DU TITRE ---
      WRITE(IUL,'(/,80(''-''))')
      CALL JEEXIN(TITR,IRET)
      IF (IRET .NE. 0 ) THEN
         CALL JEVEUO(TITR,'L',LTITR)
         CALL JELIRA(TITR,'LONMAX',NBTITR,K8B)
         DO 10 I= 1, NBTITR
            WRITE(IUL,*) ZK80(LTITR+I-1)
  10     CONTINUE
      ENDIF
C
C     --- CAS D'UNE FONCTION "FORMULE" ---
      CALL JEEXIN(NOMFON//'.NOVA',IRET)
      IF ( IRET.NE.0 .AND. IND.NE.0 ) THEN
         CALL JEVEUO(NOMFON//'.NOVA','L',LNOVA)
         CALL JELIRA(NOMFON//'.NOVA','LONUTI',NBNOVA,K8B)
         IF ( NBNOVA .NE. 1 ) THEN
         CALL UTMESS('A','FOIMPR','FONCTION A UNE SEULE VARIABLE ADMIS')
            GOTO 9999
         ENDIF
         CALL JEVEUO(LISTR//'.VALE','L',JVAL)
         CALL JELIRA(LISTR//'.VALE','LONUTI',NBVAL,K8B)
         NBV = 2 * NBVAL
         CALL WKVECT(NOMF1//'.VALE','V V R8',NBV,LVAL)
         LFON = LVAL + NBVAL
         DO 100 IVAL = 0, NBVAL-1
            ZR(LVAL+IVAL) = ZR(JVAL+IVAL)
            CALL FOINTE('F ',NOMFON,NBNOVA,ZK8(LNOVA),ZR(LVAL+IVAL),
     +                                           ZR(LFON+IVAL),IRET)
 100     CONTINUE
C
         CALL WKVECT(NOMF1//'.PROL','V V K16',5,LPROL)
         ZK16(LPROL)   = 'FONCTION'
         ZK16(LPROL+1) = 'LIN LIN '
         ZK16(LPROL+2) = ZK8(LNOVA)
         ZK16(LPROL+3) = 'TOUTRESU'
         ZK16(LPROL+4) = 'EE'
C
         CALL FOEC1F(IUL,NOMFON,ZK16(LPROL),NBVAL,'RIEN')
         IF (IMPR.GE.2) THEN
            IDEB = 1
            IFIN = MIN( 10 ,NBVAL )
            IF (IMPR.GE.3)  IFIN = NBVAL
            NOMPAR = ZK16(LPROL+2)
            NOMRES = ZK16(LPROL+3)
            CALL FOEC2F(IUL,ZR(LVAL),NBVAL,IDEB,IFIN,NOMPAR,NOMRES)
         ENDIF
         CALL JEDETR(NOMF1//'.PROL')
         CALL JEDETR(NOMF1//'.VALE')
         GOTO 9999
      ENDIF
C
C     --- INFORMATIONS COMPLEMENTAIRES POUR L'EDITION ---
      CALL JEVEUO(PROL,'L',LPROL)
      NOMPAR = ZK16(LPROL+2)
      NOMRES = ZK16(LPROL+3)
C
      IF (ZK16(LPROL).EQ.'CONSTANT'.OR.ZK16(LPROL).EQ.'FONCTION') THEN
C
C        --- NOMBRE DE VALEURS DE LA FONCTION ---
         IF (IND.NE.0) THEN
            CALL JELIRA(LISTR//'.VALE','LONUTI',NBVAL,K8B)
         ELSE
            CALL JELIRA(VALE,'LONUTI',NBVAL,K8B)
            NBVAL= NBVAL/2
         ENDIF
C
         CALL FOEC1F(IUL,NOMFON,ZK16(LPROL),NBVAL,'RIEN')
         IF (IMPR.GE.2) THEN
            CALL JEVEUO(VALE,'L',LVAL)
            IF (IND.NE.0) THEN
               CALL JEVEUO(LISTR//'.VALE','L',JVAL)
               NBV2 = 2 * NBVAL
               CALL WKVECT(NOMF1//'.VALE','V V R8',NBV2,LVAL)
               LFON = LVAL + NBVAL
               DO 200 IVAL = 0, NBVAL-1
                  ZR(LVAL+IVAL) = ZR(JVAL+IVAL)
                  CALL FOINTE('F ',NOMFON,1,NOMPAR,ZR(LVAL+IVAL),
     +                                        ZR(LFON+IVAL),IRET)
 200           CONTINUE
            ENDIF
            IDEB = 1
            IFIN = MIN( 10 ,NBVAL )
            IF (IMPR.GE.3)  IFIN = NBVAL
            CALL FOEC2F(IUL,ZR(LVAL),NBVAL,IDEB,IFIN,NOMPAR,NOMRES)
            IF (IND.NE.0) THEN
               CALL JEDETR(NOMF1//'.PROL')
               CALL JEDETR(NOMF1//'.VALE')
            ENDIF
         ENDIF
C
      ELSEIF ( ZK16(LPROL) .EQ. 'NAPPE   ' ) THEN
C
         PARA = NOMFON//'.PARA'
         CALL JELIRA(PARA,'LONMAX',NBFONC,K8B)
         CALL FOEC1N(IUL,NOMFON,ZK16(LPROL),NBFONC,'RIEN')
         IF (IMPR.GE.2) THEN
            CALL JEVEUO(PARA,'L',LVAL)
            IF (IND.NE.0) THEN
               CALL UTMESS('E','FOIMPR','DEVELOPPEMENT NON IMPLANTE.')
            ELSE
               CALL FOEC2N(IUL,ZK16(LPROL),ZR(LVAL),VALE,NBFONC,IMPR)
            ENDIF
         ENDIF
C
      ELSEIF (ZK16(LPROL).EQ.'FONCT_C ' ) THEN
C
         CALL JELIRA(VALE,'LONUTI',NBVAL,K8B)
         NBVAL= NBVAL/3
         CALL FOEC1C(IUL,NOMFON,ZK16(LPROL),NBVAL,'RIEN')
         IF (IMPR.GE.2) THEN
            CALL JEVEUO(VALE,'L',LVAL)
            IF (IND.NE.0) THEN
               CALL JEVEUO(LISTR//'.VALE','L',JVAL)
               CALL JELIRA(LISTR//'.VALE','LONUTI',NBVAL,K8B)
               NBV2 = 3 * NBVAL
               CALL WKVECT(NOMF1//'.VALE','V V R8',NBV2,LVAL)
               LFON = LVAL + NBVAL
               II = 0
               DO 300 IVAL = 0, NBVAL-1
                  ZR(LVAL+IVAL) = ZR(JVAL+IVAL)
                  CALL FOINRI(NOMFON,NBPU,NOMPU,ZR(LVAL+IVAL),RESURE,
     &                        RESUIM,IRET)
                  ZR(LFON+II) = RESURE
                  II = II + 1
                  ZR(LFON+II) = RESUIM
                  II = II + 1
 300           CONTINUE
            ENDIF
            IDEB = 1
            IFIN = MIN( 10 ,NBVAL )
            IF (IMPR.GE.3)  IFIN = NBVAL
            CALL FOEC2C(IUL,ZR(LVAL),NBVAL,IDEB,IFIN,NOMPAR,NOMRES)
            IF (IND.NE.0) THEN
               CALL JEDETR(NOMF1//'.PROL')
               CALL JEDETR(NOMF1//'.VALE')
            ENDIF
          ENDIF
C
      ELSEIF (ZK16(LPROL).EQ.'INTERPRE' ) THEN
         CALL GETRES(K8B,K8B,NOMCMD)
         CALL UTMESS('A',NOMCMD//' (ALARME 01)',
     +               'ON N''IMPRIME PAS ENCORE DE FONCTION DE TYPE "'//
     +               ZK16(LPROL)//'"      DESOLE. ')
C
      ELSE
         CALL GETRES(K8B,K8B,NOMCMD)
         CALL UTMESS('A',NOMCMD//' (ALARME 01)',
     +               'ON NE SAIT PAS IMPRIMER UNE FONCTION DE TYPE "'//
     +               ZK16(LPROL)//'"      DESOLE. ')
C
      ENDIF
 9999 CONTINUE
      CALL JEDEMA()
      END
