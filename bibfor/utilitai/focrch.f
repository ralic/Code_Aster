      SUBROUTINE FOCRCH ( NOMFON, RESU, NOEUD, PARAX, PARAY, BASE,
     +                                  INT, INTITU, IND, LISTR,
     +                    SST,    NSST,                          IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                     NSST, INT,         IND,        IER
      CHARACTER*1                                            BASE
      CHARACTER*16                      PARAX, PARAY
      CHARACTER*8         SST,          NOEUD, INTITU
      CHARACTER*19        NOMFON, RESU,                   LISTR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 17/12/2002   AUTEUR CIBHHGB G.BERTRAND 
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
C     RECUPERATION D'UNE FONCTION DANS UNE STRUCTURE "TRAN_GENE"
C     POUR UN NOEUD DE CHOC
C     ------------------------------------------------------------------
C IN  : NOMFON : NOM DE LA FONCTION
C IN  : RESU   : NOM DE LA STRUCTURE RESULTAT
C IN  : NOEUD  : NOEUD DE CHOC
C IN  : PARAX  : PARAMETRE DE LA FONCTION EN X
C IN  : PARAY  : PARAMETRE DE LA FONCTION EN Y
C IN  : BASE   : 'GLOBALE'
C IN  : INT    : PRISE EN COMPTE D'UN NOM DE LIAISON
C IN  : INTITU : NOM D'UNE LIAISON
C IN  : IND    : PRISE EN COMPTE D'UNE LISTE DE PARAMETRES
C IN  : LISTR  : LISTE DE PARAMETRES
C IN  : SST    : NOM DE LA SOUS-STRUCTURE
C IN  : NSST   : PRISE EN COMPTE DU NOM D'UNE SOUS-STRUCTURE
C OUT : IER    : CODE RETOUR, = 0 : OK
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      JSST
      CHARACTER*8  K8B
      CHARACTER*16 NOMCMD
      CHARACTER*19 FONCT1, FONCT2
C     ----------------------------------------------------------------
      CALL JEMARQ()
      IER = 9999
      CALL GETRES(K8B,K8B,NOMCMD)
C
      CALL JEVEUO(RESU//'.DESC','L',JDESC)
      NBCHOC = ZI(JDESC+2)
      CALL JELIRA(RESU//'.INST','LONUTI',NBINST,K8B)
      CALL JEVEUO(RESU//'.INST','L',JINST)
      CALL JEVEUO(RESU//'.NCHO','L',JNCHO)
      CALL JEVEUO(RESU//'.INTI','L',JINTI)
      IF (NSST.NE.0) CALL JEVEUO(RESU//'.SST','L',JSST)
      IC = 1
      IF (INT.NE.0) THEN
         DO 2 ICHOC = 1,NBCHOC
            IF (ZK8(JINTI+ICHOC-1).EQ.INTITU) GOTO 4
 2       CONTINUE
         CALL UTMESS('A',NOMCMD,'L''INTITULE "'//INTITU//
     +                       '" N''EST PAS CORRECT.')
         GOTO 9999
 4       CONTINUE
         IF (NSST.EQ.0) THEN
           IF (ZK8(JNCHO+ICHOC-1).EQ.NOEUD) GOTO 16
           IC = 2
           IF (ZK8(JNCHO+NBCHOC+ICHOC-1).EQ.NOEUD) GOTO 16
           LG = MAX(1,LXLGUT(NOEUD))
           CALL UTMESS('A',NOMCMD,'LE NOEUD "'//NOEUD(1:LG)//
     +                         '" N''EST PAS UN NOEUD DE CHOC.')
           GOTO 9999
         ELSE
           IF (ZK8(JSST+ICHOC-1).EQ.SST) GOTO 116
           IF (ZK8(JSST+NBCHOC+ICHOC-1).EQ.SST) GOTO 116
           CALL UTMESS('A',NOMCMD,'NOM DE SOUS-STRUCTURE ET '//
     +                         'D''INTITULE INCOMPATIBLE')
           GOTO 9999
116        CONTINUE
           IF (ZK8(JNCHO+ICHOC-1).NE.NOEUD.AND.
     +         ZK8(JNCHO+NBCHOC+ICHOC-1).NE.NOEUD) THEN
              LG = MAX(1,LXLGUT(NOEUD))
              CALL UTMESS('A',NOMCMD,'LE NOEUD "'//NOEUD(1:LG)//
     +            '" N''EST PAS UN NOEUD DE CHOC DE L''INTITULE.')
              GOTO 9999
           ENDIF
           IF (ZK8(JNCHO+ICHOC-1).EQ.NOEUD.AND.
     +         ZK8(JSST+ICHOC-1).EQ.SST) GOTO 16
           IC = 2
           IF (ZK8(JNCHO+NBCHOC+ICHOC-1).EQ.NOEUD.AND.
     +         ZK8(JSST+NBCHOC+ICHOC-1).EQ.SST) GOTO 16
           LG = MAX(1,LXLGUT(NOEUD))
           CALL UTMESS('A',NOMCMD,'LE NOEUD "'//NOEUD(1:LG)//
     +  '" N''EST PAS COMPATIBLE AVEC LE NOM DE LA SOUS-STRUCTURE.')
           GOTO 9999
        ENDIF
      ENDIF
C     --- RECHERCHE DU NOEUD_1 DE CHOC ---
      DO 10 ICHOC = 1,NBCHOC
         IF (ZK8(JNCHO+ICHOC-1).EQ.NOEUD) GOTO 16
 10   CONTINUE
C     --- RECHERCHE DU NOEUD_2 DE CHOC ---
      IC = 2
      DO 12 ICHOC = 1,NBCHOC
         IF (ZK8(JNCHO+NBCHOC+ICHOC-1).EQ.NOEUD) GOTO 16
 12   CONTINUE
      LG = MAX(1,LXLGUT(NOEUD))
      CALL UTMESS('A',NOMCMD,'LE NOEUD "'//NOEUD(1:LG)//
     +                       '" N''EST PAS UN NOEUD DE CHOC.')
      GOTO 9999
 16   CONTINUE
C
      IF (PARAX(1:4).EQ.'INST') THEN
         JVALX = JINST
         GOTO 20
      ELSEIF (PARAX(1:2).EQ.'FN') THEN
         CALL JEVEUO(RESU//'.FCHO','L',JPARX)
         IDEC = 1 + 3*(ICHOC-1)
      ELSEIF (PARAX(1:3).EQ.'FT1') THEN
         CALL JEVEUO(RESU//'.FCHO','L',JPARX)
         IDEC = 2 + 3*(ICHOC-1)
      ELSEIF (PARAX(1:3).EQ.'FT2') THEN
         CALL JEVEUO(RESU//'.FCHO','L',JPARX)
         IDEC = 3 + 3*(ICHOC-1)
      ELSEIF (PARAX(1:2).EQ.'VN') THEN
         CALL JEVEUO(RESU//'.VCHO','L',JPARX)
         IDEC = 1 + 3*(ICHOC-1)
      ELSEIF (PARAX(1:3).EQ.'VT1') THEN
         CALL JEVEUO(RESU//'.VCHO','L',JPARX)
         IDEC = 2 + 3*(ICHOC-1)
      ELSEIF (PARAX(1:3).EQ.'VT2') THEN
         CALL JEVEUO(RESU//'.VCHO','L',JPARX)
         IDEC = 3 + 3*(ICHOC-1)
      ELSEIF (PARAX(1:5).EQ.'DXLOC') THEN
         CALL JEVEUO(RESU//'.DLOC','L',JPARX)
         IF (IC.EQ.1) THEN
            IDEC = 1 + 3*(ICHOC-1)
         ELSE
            IDEC = 3*NBCHOC*NBINST + 1 + 3*(ICHOC-1)
         ENDIF
      ELSEIF (PARAX(1:5).EQ.'DYLOC') THEN
         CALL JEVEUO(RESU//'.DLOC','L',JPARX)
         IF (IC.EQ.1) THEN
            IDEC = 2 + 3*(ICHOC-1)
         ELSE
            IDEC = 3*NBCHOC*NBINST + 2 + 3*(ICHOC-1)
         ENDIF
      ELSEIF (PARAX(1:5).EQ.'DZLOC') THEN
         CALL JEVEUO(RESU//'.DLOC','L',JPARX)
         IF (IC.EQ.1) THEN
            IDEC = 3 + 3*(ICHOC-1)
         ELSE
            IDEC = 3*NBCHOC*NBINST + 3 + 3*(ICHOC-1)
         ENDIF
      ELSE
         LG = MAX(1,LXLGUT(PARAX(1:8)))
         CALL UTMESS('A',NOMCMD,'LE PARAMETRE "'//PARAX(1:LG)//
     +                          '" N''EST PAS UN PARAMETRE DE CHOC.')
         GOTO 9999
      ENDIF
      CALL WKVECT('&&FOCRCH.PARAX','V V R',NBINST,JVALX)
      CALL R8COPY(NBINST,ZR(JPARX+IDEC-1),3*NBCHOC,ZR(JVALX),1)
 20   CONTINUE
C
      IF (PARAY(1:4).EQ.'INST') THEN
         JVALY = JINST
         GOTO 22
      ELSEIF (PARAY(1:2).EQ.'FN') THEN
         CALL JEVEUO(RESU//'.FCHO','L',JPARY)
         IDEC = 1 + 3*(ICHOC-1)
      ELSEIF (PARAY(1:3).EQ.'FT1') THEN
         CALL JEVEUO(RESU//'.FCHO','L',JPARY)
         IDEC = 2 + 3*(ICHOC-1)
      ELSEIF (PARAY(1:3).EQ.'FT2') THEN
         CALL JEVEUO(RESU//'.FCHO','L',JPARY)
         IDEC = 3 + 3*(ICHOC-1)
      ELSEIF (PARAY(1:2).EQ.'VN') THEN
         CALL JEVEUO(RESU//'.VCHO','L',JPARY)
         IDEC = 1 + 3*(ICHOC-1)
      ELSEIF (PARAY(1:3).EQ.'VT1') THEN
         CALL JEVEUO(RESU//'.VCHO','L',JPARY)
         IDEC = 2 + 3*(ICHOC-1)
      ELSEIF (PARAY(1:3).EQ.'VT2') THEN
         CALL JEVEUO(RESU//'.VCHO','L',JPARY)
         IDEC = 3 + 3*(ICHOC-1)
      ELSEIF (PARAY(1:5).EQ.'DXLOC') THEN
         CALL JEVEUO(RESU//'.DLOC','L',JPARY)
         IF (IC.EQ.1) THEN
            IDEC = 1 + 3*(ICHOC-1)
         ELSE
            IDEC = 3*NBCHOC*NBINST + 1 + 3*(ICHOC-1)
         ENDIF
      ELSEIF (PARAY(1:5).EQ.'DYLOC') THEN
         CALL JEVEUO(RESU//'.DLOC','L',JPARY)
         IF (IC.EQ.1) THEN
            IDEC = 2 + 3*(ICHOC-1)
         ELSE
            IDEC = 3*NBCHOC*NBINST + 2 + 3*(ICHOC-1)
         ENDIF
      ELSEIF (PARAY(1:5).EQ.'DZLOC') THEN
         CALL JEVEUO(RESU//'.DLOC','L',JPARY)
         IF (IC.EQ.1) THEN
            IDEC = 3 + 3*(ICHOC-1)
         ELSE
            IDEC = 3*NBCHOC*NBINST + 3 + 3*(ICHOC-1)
         ENDIF
      ELSE
         LG = MAX(1,LXLGUT(PARAY(1:8)))
         CALL UTMESS('A',NOMCMD,'LE PARAMETRE "'//PARAY(1:LG)//
     +                          '" N''EST PAS UN PARAMETRE DE CHOC.')
         GOTO 9999
      ENDIF
      CALL WKVECT('&&FOCRCH.PARAY','V V R',NBINST,JVALY)
      CALL R8COPY(NBINST,ZR(JPARY+IDEC-1),3*NBCHOC,ZR(JVALY),1)
 22   CONTINUE
C
      IF (IND.EQ.0) THEN
         CALL WKVECT(NOMFON//'.PROL',BASE//' V K16',5,LPRO)
         ZK16(LPRO) = 'FONCTION'
         ZK16(LPRO+1) = 'LIN LIN '
         ZK16(LPRO+2) = PARAX
         ZK16(LPRO+3) = PARAY
         ZK16(LPRO+4) = 'EE'
C
         NBVAL = NBINST * 2
         CALL WKVECT(NOMFON//'.VALE',BASE//' V R',NBVAL,LVAL)
         LFON = LVAL + NBINST
         DO 30 IVAL = 0,NBINST-1
            ZR(LVAL+IVAL) = ZR(JVALX+IVAL)
            ZR(LFON+IVAL) = ZR(JVALY+IVAL)
 30      CONTINUE
         IER = 0
C
      ELSE
         FONCT1 = '&&FOCRCH.FONCT1'
         CALL WKVECT(FONCT1//'.PROL','V V K16',5,LPRO)
         ZK16(LPRO) = 'FONCTION'
         ZK16(LPRO+1) = 'LIN LIN '
         ZK16(LPRO+2) = 'INST'
         ZK16(LPRO+3) = PARAX
         ZK16(LPRO+4) = 'EE'
         NBVAL = NBINST * 2
         CALL WKVECT(FONCT1//'.VALE','V V R',NBVAL,LVAL)
         LFON = LVAL + NBINST
         DO 100 IVAL = 0,NBINST-1
            ZR(LVAL+IVAL) = ZR(JINST+IVAL)
            ZR(LFON+IVAL) = ZR(JVALX+IVAL)
 100     CONTINUE
C
         FONCT2 = '&&FOCRCH.FONCT2'
         CALL WKVECT(FONCT2//'.PROL','V V K16',5,LPRO)
         ZK16(LPRO) = 'FONCTION'
         ZK16(LPRO+1) = 'LIN LIN '
         ZK16(LPRO+2) = 'INST'
         ZK16(LPRO+3) = PARAY
         ZK16(LPRO+4) = 'EE'
         NBVAL = NBINST * 2
         CALL WKVECT(FONCT2//'.VALE','V V R',NBVAL,LVAL)
         LFON = LVAL + NBINST
         DO 110 IVAL = 0,NBINST-1
            ZR(LVAL+IVAL) = ZR(JINST+IVAL)
            ZR(LFON+IVAL) = ZR(JVALY+IVAL)
 110     CONTINUE
C
         CALL JEVEUO(LISTR//'.VALE','L',JVAL)
         CALL JELIRA(LISTR//'.VALE','LONUTI',NBPARA,K8B)
C
         CALL WKVECT(NOMFON//'.PROL',BASE//' V K16',5,LPRO)
         ZK16(LPRO) = 'FONCTION'
         ZK16(LPRO+1) = 'LIN LIN '
         ZK16(LPRO+2) = PARAX
         ZK16(LPRO+3) = PARAY
         ZK16(LPRO+4) = 'EE'
C
         NBVAL = NBPARA * 2
         CALL WKVECT(NOMFON//'.VALE',BASE//' V R',NBVAL,LVAL)
         LFON = LVAL + NBPARA
         DO 120 IVAL = 0, NBPARA-1
            CALL FOINTE('F ',FONCT1,1,'INST',ZR(JVAL+IVAL),
     +                      ZR(LVAL+IVAL),IE)
            CALL FOINTE('F ',FONCT2,1,'INST',ZR(JVAL+IVAL),
     +                      ZR(LFON+IVAL),IE)
 120     CONTINUE
C
         CALL JEDETR(FONCT1//'.PROL')
         CALL JEDETR(FONCT1//'.VALE')
         CALL JEDETR(FONCT2//'.PROL')
         CALL JEDETR(FONCT2//'.VALE')
         IER = 0
      ENDIF
      IF (PARAX(1:4).NE.'INST') CALL JEDETR('&&FOCRCH.PARAX')
      IF (PARAY(1:4).NE.'INST') CALL JEDETR('&&FOCRCH.PARAY')
C
 9999 CONTINUE
      CALL JEDEMA()
      END
