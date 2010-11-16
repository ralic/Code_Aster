      SUBROUTINE VPRECU ( MODES, NOMSY, NBVECT, LPOSI, NOMVEC,
     &                    NBPARA, NOPARA, NOMVAI, NOMVAR, NOMVAK,
     &                    NEQ, NBMODE, TYPMOD, NBPARI, NBPARR, NBPARK )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     MODES, NOMSY, NOMVEC, TYPMOD, NOPARA
      CHARACTER*(*)     NOMVAI, NOMVAR, NOMVAK
      INTEGER           NBVECT, LPOSI(*), NEQ, NBMODE, NBPARA
      INTEGER           NBPARI, NBPARR, NBPARK
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 16/11/2010   AUTEUR BODEL C.BODEL 
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
C     RECUPERATION DES VALEURS ET VECTEURS PROPRES
C     ------------------------------------------------------------------
C IN  MODES  : K8 : NOM DE LA STRUCTURE MODE_MECA
C IN  NOMSY  : K16: NOM DU CHAMP A RECUPERER
C IN  NBVECT : IS : NOMBRE DE MODE A RECUPERER
C                   =0   AUCUN CHAMP A RECUPERER
C                   >0   RECUPERATION DE LA LISTE PAR NUMERO D'ORDRE
C                   <0   RECUPERATION DE TOUS LES MODES
C IN  LPOSI  : IS : LISTE DES NUMEROS D'ORDRE
C IN  NOMVEC : K24: NOM DU TABLEAU CONTENANT LES VECTEURS PROPRES
C IN  NBPARA : IS : NOMBRE DE PARAMETRES A RECUPERER
C                   =0   AUCUN PARAMETRE A RECUPERER
C                   >0   RECUPERATION A PARTIR D'UNE LISTE
C                   <0   RECUPERATION DE TOUS LES PARAMETRES
C VAR NOPARA : K16: LISTE DES NOMS DES PARAMETRES
C IN  NOMVAI : K24: NOM DU TABLEAU CONTENANT LES PARAMETRES ENTIERS
C IN  NOMVAR : K24: NOM DU TABLEAU CONTENANT LES PARAMETRES REELS
C IN  NOMVAK : K24: NOM DU TABLEAU CONTENANT LES PARAMETRES CHARACTERS
C OUT NEQ    : IS : NOMBRE D'EQUATIONS
C OUT NBMODE : IS : NOMBRE DE MODE(S) RECUPERE(S)
C OUT NBPARI : IS : NOMBRE DE PARAMETRE(S) ENTIER(S) RECUPERE(S)
C OUT NBPARR : IS : NOMBRE DE PARAMETRE(S) REEL(S) RECUPERE(S)
C OUT NBPARK : IS : NOMBRE DE PARAMETRE(S) CHARACTER(S) RECUPERE(S)
C OUT TYPMOD : K  : TYPE DU CHAMP RECUPERE
C     ------------------------------------------------------------------
C
C     *** ATTENTION ***
C
C     LES VECTEURS PROPRES SONT LES UNS DERRIERE LES AUTRES
C         ACCES PAR   ZR(IDDL,IMODE)
C
C     LES PARAMETRES MODAUX SONT PAR CATEGORIES
C         ACCES PAR   ZR(IMODE,IPARA)
C
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
C
C     ------------------------------------------------------------------
      REAL*8        RBID(2)
      CHARACTER*4   TYPE
      CHARACTER*8   K8B
      CHARACTER*16  NOMCMD
      CHARACTER*24  VALE, NOMJV
      CHARACTER*24 VALK(2)
      COMPLEX*16    C16B
      LOGICAL       RECUNP
C     ------------------------------------------------------------------
      DATA  VALE  /'                   .VALE'/
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      IF ( NBVECT .LT. 0 ) THEN
C        --- TOUS LES MODES ---
         CALL RSORAC(MODES,'LONUTI',IBID,RBID,K8B,C16B,0.0D0,
     &                                           K8B,NBMODE,1,NBTROU)
         CALL WKVECT('&&VPRECU.NUMERO.ORDRE','V V I',NBMODE,LNUMOR)
         CALL RSORAC(MODES,'TOUT_ORDRE',IBID,RBID,K8B,C16B,0.0D0,
     &                                   K8B,ZI(LNUMOR),NBMODE,NBTROU)
      ELSEIF ( NBVECT .GT. 0 ) THEN
C        --- A PARTIR D'UNE LISTE DE NUMEROS D'ORDRE ---
         NBMODE = NBVECT
         CALL WKVECT('&&VPRECU.NUMERO.ORDRE','V V I',NBMODE,LNUMOR)
         DO 10 I = 1,NBVECT
            ZI(LNUMOR+I-1) = LPOSI(I)
 10      CONTINUE
C
      ELSE
C        --- RIEN ---
         NBMODE  = 0
         TYPMOD = '?'
         CALL RSORAC(MODES,'LONUTI',IBID,RBID,K8B,C16B,0.0D0,
     &                                           K8B,NBMODT,1,NBTROU)
         CALL WKVECT('&&VPRECU.NUMERO.ORDRE','V V I',NBMODT,LNUMOR)
         CALL RSORAC(MODES,'TOUT_ORDRE',IBID,RBID,K8B,C16B,0.0D0,
     &                                   K8B,ZI(LNUMOR),NBMODT,NBTROU)
         GOTO 100
      ENDIF
C     ------------------------------------------------------------------
C
C          *************** ON RECUPERE LES CHAMPS ***************
C
C     --- RECUPERATION DE NEQ ---
      CALL RSEXCH(MODES,NOMSY,ZI(LNUMOR),VALE(1:19),IRET)
      IF (IRET.NE.0) THEN
         CALL GETRES(K8B,K8B,NOMCMD)
            VALK (1) = VALE(1:19)
            VALK (2) = ' '
         CALL U2MESG('F', 'ALGELINE4_79',2,VALK,0,0,0,0.D0)
       ELSE
         CALL JEEXIN(VALE(1:19)//'.VALE',IBID)
         IF (IBID.GT.0) THEN
           VALE(20:24)='.VALE'
         ELSE
           VALE(20:24)='.CELV'
         END IF

         CALL JELIRA(VALE,'LONMAX',NEQ,K8B)
         CALL JELIRA(VALE,'TYPE',IBID,TYPMOD)
       ENDIF
C
C     --- CREATION DES OBJETS DE NOM NOMVEC ET NOMVAL ---
      IF ( TYPMOD(1:1) .EQ. 'R' ) THEN
         CALL WKVECT(NOMVEC,'V V R',NEQ*NBMODE,LMODE)
      ELSEIF ( TYPMOD(1:1) .EQ. 'C' ) THEN
         CALL WKVECT(NOMVEC,'V V C',NEQ*NBMODE,LMODE)
      ELSE
            VALK (1) = TYPMOD(1:1)
         CALL U2MESG('F', 'ALGELINE4_80',1,VALK,0,0,0,0.D0)
      ENDIF
C
C        --- VECTEUR PROPRE ---
      DO 20 IMODE = 1,NBMODE
         NORDR =  ZI(LNUMOR-1+IMODE)
         CALL RSEXCH ( MODES, NOMSY, NORDR, VALE(1:19), IRET)
         IF (IRET.NE.0) THEN
            CALL GETRES(K8B,K8B,NOMCMD)
            VALK (1) = VALE(1:19)
            CALL U2MESG('E', 'ALGELINE4_81',1,VALK,0,0,0,0.D0)
         ELSE
            CALL JEEXIN(VALE(1:19)//'.VALE',IBID)
            IF (IBID.GT.0) THEN
              VALE(20:24)='.VALE'
            ELSE
              VALE(20:24)='.CELV'
            END IF

            CALL JEVEUO(VALE,'L',LVALE)
            CALL JELIRA(VALE,'LONMAX',NEQ1,K8B)
            CALL JELIRA(VALE,'TYPE',IBID,K8B)
            IF ( TYPMOD(1:1) .NE. K8B(1:1) ) THEN
             CALL U2MESS('F','ALGELINE3_70')
            ELSEIF ( NEQ .EQ. NEQ1 ) THEN
               IF ( TYPMOD(1:1) .EQ. 'R' ) THEN
                  DO 22 IEQ = 0, NEQ-1
                     ZR(LMODE+NEQ*(IMODE-1)+IEQ) = ZR(LVALE+IEQ)
 22               CONTINUE
               ELSEIF ( TYPMOD(1:1) .EQ. 'C' ) THEN
                  DO 24 IEQ = 0, NEQ-1
                     ZC(LMODE+NEQ*(IMODE-1)+IEQ) = ZC(LVALE+IEQ)
 24               CONTINUE
               ENDIF
               CALL JELIBE(VALE)
            ELSE
             CALL U2MESS('F','ALGELINE3_71')
            ENDIF
         ENDIF
 20   CONTINUE
 100  CONTINUE
C     ------------------------------------------------------------------
C
C        *************** ON RECUPERE LES PARAMETRES ***************
C
      RECUNP = .FALSE.
      IF ( NBPARA .LT. 0 ) THEN
C        --- TOUS LES PARAMETRES ---
         NOMJV = '&&VPRECU.NOM_PARA'
         CALL IRPARB ( MODES, NBPARA, NOPARA, NOMJV, NBOUT )
         CALL JEVEUO ( NOMJV , 'L', JPARA )
         RECUNP = .TRUE.
      ELSEIF ( NBPARA .GT. 0 ) THEN
C        --- A PARTIR D'UNE LISTE DE PARAMETRES ---
         NOMJV = '&&VPRECU.NOM_PARA'
         CALL IRPARB (MODES, NBPARA, NOPARA, NOMJV, NBOUT )
         CALL JEVEUO ( NOMJV , 'L', JPARA )
C
      ELSE
C        --- RIEN ---
         NBPARI = 0
         NBPARR = 0
         NBPARK = 0
         GOTO 200
      ENDIF
C
      NBPARI = 0
      NBPARR = 0
      NBPARK = 0
      DO 40 I = 1,NBOUT
        CALL RSADPA(MODES,'L',1,ZK16(JPARA+I-1),ZI(LNUMOR),I,LNUME,TYPE)
         IF (TYPE(1:1).EQ.'I') THEN
            NBPARI = NBPARI + 1
         ELSEIF (TYPE(1:1).EQ.'R') THEN
            NBPARR = NBPARR + 1
         ELSEIF (TYPE(1:1).EQ.'K') THEN
            NBPARK = NBPARK + 1
         ELSE
         ENDIF
 40   CONTINUE
      IF ( RECUNP ) THEN
         NBTPAR = NBPARI + NBPARR + NBPARK
         CALL WKVECT ( NOPARA, 'V V K16', NBTPAR, LNOPAR )
      ENDIF
C
      IF(NBPARI.NE.0) CALL WKVECT(NOMVAI,'V V I'  ,NBPARI*NBMODE,LRESUI)
      IF(NBPARR.NE.0) CALL WKVECT(NOMVAR,'V V R'  ,NBPARR*NBMODE,LRESUR)
      IF(NBPARK.NE.0) CALL WKVECT(NOMVAK,'V V K24',NBPARK*NBMODE,LRESUK)
C
      II = 0
      IR = 0
      IK = 0
      DO 50 I = 1,NBOUT
        DO 52 J = 1,NBMODE
          NORDR   =  ZI(LNUMOR-1+J)
          CALL RSADPA(MODES,'L',1,ZK16(JPARA+I-1),NORDR,I,LNUME,TYPE)
          IF (TYPE(1:1).EQ.'I') THEN
            II = II + 1
            ZI(LRESUI+II-1) = ZI(LNUME)
            IF ( RECUNP .AND. J.EQ.1 ) THEN
               ZK16(LNOPAR+II-1) = ZK16(JPARA+I-1)
            ENDIF
          ELSEIF (TYPE(1:1).EQ.'R') THEN
            IR = IR + 1
            ZR(LRESUR+IR-1) = ZR(LNUME)
            IF ( RECUNP .AND. J.EQ.1 ) THEN
               ZK16(LNOPAR+NBPARI+IR-1) = ZK16(JPARA+I-1)
            ENDIF
          ELSEIF (TYPE(1:1).EQ.'K') THEN
            IK = IK + 1
            IF (TYPE(2:3).EQ.'24') THEN
              ZK24(LRESUK+IK-1) = ZK24(LNUME)
            ELSE
              ZK24(LRESUK+IK-1) = ZK16(LNUME)//'        '
            ENDIF
            IF ( RECUNP .AND. J.EQ.1 ) THEN
               ZK16(LNOPAR+NBPARI+NBPARR+IK-1) = ZK16(JPARA+I-1)
            ENDIF
          ENDIF
 52     CONTINUE
 50   CONTINUE
 200  CONTINUE
C
C     --- DESTRUCTION DES OBJET DE TRAVAIL ---
      CALL JEEXIN ( '&&VPRECU.NOM_PARA' , IRET )
      IF (IRET.NE.0 ) CALL JEDETR ( '&&VPRECU.NOM_PARA' )
      CALL JEEXIN ( '&&VPRECU.NUMERO.ORDRE' , IRET )
      IF (IRET.NE.0 ) CALL JEDETR ( '&&VPRECU.NUMERO.ORDRE' )
C
      CALL JEDEMA()
      END
