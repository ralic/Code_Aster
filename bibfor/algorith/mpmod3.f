      SUBROUTINE MPMOD3(BASEMO,NOMMES,NBMESU,NBMTOT,VCHAM,
     &                  VNOEUD,VRANGE,VORIEN,NNOEMA,NCMPMA)
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C
C     PROJ_MESU_MODAL : ESTIMATION DE NBMESU ET VERIFICATION EXISTENCE
C                       DES VECTEURS DE BASE
C
C     IN  : BASEMO : NOM DE LA BASE DE PROJECTION
C     IN  : NOMMES : NOM DE LA MESURE
C
C     OUT  : NBMESU : NOMBRE DE MESURE (DATASET 58)
C     OUT  : NBMTOT : NOMBRE DE VECTEURS DE BASE
C     OUT  : VNOEUD : NOM RANGEMENT NOEUD MESURE
C     OUT  : VRANGE : NOM CORRESPONDANCE CMP SUIVANT VNOEUD
C     OUT  : VORIEN : NOM CORRESPONDANCE ORIENTATION SUIVANT VNOEUD
C     OUT  : VCHAM : NOM CORRESPONDANCE CHAMP SUIVANT VNOEUD
C     OUT  : NNOEMA : NOMBRE DE NOEUDS MAXI
C     OUT  : NCMPMA : NOMBRE DE CMP MAXI
C
      IMPLICIT NONE
C     ------------------------------------------------------------------
C-------- DEBUT COMMUNS NORMALISES  JEVEUX  ----------------------------
C
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
C
C----------  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*8   BASEMO, NOMMES
      CHARACTER*24  VNOEUD,VRANGE,VCHAM,VORIEN
      CHARACTER*24 VALK(2)
      INTEGER       NBMESU, NBMTOT,NNOEMA,NCMPMA
C
      CHARACTER*1  TYPVAL
      CHARACTER*8  NOMRES,K8BID,SCAL,SCALAI,COMPMS,COMCAP
      CHARACTER*8  NOMGD,LICMP(30),MODMES
      CHARACTER*16 NOMCHA, TYPRES, K16BID
      CHARACTER*19 CHAMNO, CH1S, CH2S, CHS,CHAMES,CHACAP
      CHARACTER*24 VREF

      INTEGER      LORD, LORI,LRANGE,LREF
      INTEGER      IMESU, II, IMODE, IRET,NBORD
      INTEGER      ICMP, INO,INOMES,INOCAP
      INTEGER      LNOEUD,IDESC,GD,NBNOEU,NBCMP
      INTEGER      JCNSD,JCNSC,JCNSV,JCNSL,JCNSK
      INTEGER      IBID,INDICE,NBCHAM,LCH,ICH,LCHAM

      LOGICAL      ZCMPLX,ORIEN,DCAPT

      REAL*8       VAL,RBID,R8PREM

      COMPLEX*16   CBID
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
C RECUPERATION DU NOM DU CONCEPT RESULTAT
      CALL GETRES (NOMRES, TYPRES, K16BID)
C
C RECUPERATION DU CHAMP MESURE

      CALL GETVTX ('MODELE_MESURE','NOM_CHAM',1,1,0,NOMCHA,NBCHAM)
      IF (NBCHAM .NE. 0) THEN
        NBCHAM = -NBCHAM
      ELSE
        CALL U2MESS('A','ALGORITH10_93')
      ENDIF
      CALL WKVECT ('&&LISTE_CHAM', 'V V K16', NBCHAM, LCH)
      CALL GETVTX ('MODELE_MESURE','NOM_CHAM',1,1,NBCHAM,ZK16(LCH),IBID)
C
C RECUPERATION DU NB DE VECTEURS DE BASE : NBMTOT

      CALL RSORAC(BASEMO,'LONUTI',IBID,RBID,K8BID,CBID,RBID,'ABSOLU',
     &            NBMTOT,1,IBID)
C
C RECUPERATION DES OBJETS LIES A LA MESURE
C
      CALL JEVEUO ( NOMMES//'           .ORDR' , 'L' , LORD )

C RECUPERATION DU NB DE NUMERO D ORDRE : NBORD

      CALL RSORAC(NOMMES,'LONUTI',IBID,RBID,K8BID,CBID,RBID,'ABSOLU',
     &            NBORD,1,IBID)
C
      CHS = '&&MESURE.CHS'
      NBMESU = 0

      NNOEMA = 0
      NCMPMA = 0

C     CALCUL DE NNOEMA ET NCMPMA

      DO 150 ICH = 1,NBCHAM
        NOMCHA = ZK16(LCH-1 +ICH)
        CALL RSEXCH (NOMMES,NOMCHA,ZI(LORD),CHAMNO,IRET)
        IF (IRET .NE. 0) THEN
          VALK (1) = NOMMES
          VALK (2) = NOMCHA
          CALL U2MESG('F', 'ALGORITH13_53',2,VALK,0,0,0,0.D0)
        END IF
C
C TRANSFORMATION DE CHAMNO EN CHAM_NO_S : CHS
        CALL CNOCNS(CHAMNO,'V',CHS)
        CALL JEVEUO(CHS//'.CNSD','L',JCNSD)

        NBNOEU = ZI(JCNSD-1 + 1)
        NBCMP = ZI(JCNSD-1 + 2)
        NNOEMA = NNOEMA + NBNOEU
        NCMPMA = NCMPMA + NBCMP

150   CONTINUE

C ORDRE DE RANGEMENT MESURE SELON VRANGE ET VNOEUD
      VNOEUD = NOMRES//'.PROJM    .PJMNO'
      VRANGE = NOMRES//'.PROJM    .PJMRG'
      VCHAM = NOMRES//'.PROJM    .PJMCH'
      VORIEN = NOMRES//'.PROJM    .PJMOR'
      VREF = NOMRES//'.PROJM    .PJMRF'

      CALL WKVECT(VNOEUD,'G V I',NNOEMA*NCMPMA,LNOEUD)
      CALL WKVECT(VRANGE,'G V K8',NNOEMA*NCMPMA,LRANGE)
      CALL WKVECT(VCHAM,'V V K16',NNOEMA*NCMPMA,LCHAM)
      CALL WKVECT(VORIEN,'G V R',NNOEMA*NCMPMA*3,LORI)
      CALL WKVECT(VREF,'G V K16',5,LREF)

C BOUCLE SUR LES CHAMPS MESURES

      DO 151 ICH = 1,NBCHAM
        NOMCHA = ZK16(LCH-1 +ICH)
        CALL RSEXCH (NOMMES,NOMCHA,ZI(LORD),CHAMNO,IRET)
C
        CALL JEVEUO(CHAMNO//'.DESC','L',IDESC)
        GD = ZI(IDESC-1 +1)
        SCAL = SCALAI(GD)
        TYPVAL = SCAL(1:1)
        IF (TYPVAL.EQ.'C') THEN
          ZCMPLX = .TRUE.
        ELSE
          ZCMPLX = .FALSE.
        ENDIF

C TRANSFORMATION DE CHAMNO EN CHAM_NO_S : CHS
        CALL CNOCNS(CHAMNO,'V',CHS)
        CALL JEVEUO(CHS//'.CNSK','L',JCNSK)
        CALL JEVEUO(CHS//'.CNSD','L',JCNSD)
        CALL JEVEUO(CHS//'.CNSC','L',JCNSC)
        CALL JEVEUO(CHS//'.CNSV','L',JCNSV)
        CALL JEVEUO(CHS//'.CNSL','L',JCNSL)

        NBNOEU = ZI(JCNSD-1 + 1)
        NBCMP = ZI(JCNSD-1 + 2)
        NOMGD = ZK8(JCNSK-1 +2)

        IF (NOMGD(1:4) .EQ. 'DEPL') THEN
C RECUPERATION DE L ORIENTATION
          IF ((TYPRES(1:9).EQ.'HARM_GENE').OR.
     &      (TYPRES(1:9).EQ.'TRAN_GENE')) THEN
C  SI RESULTAT DE TYPE *_GENE ON SUPPOSE QUE L'ORIENTATION
C  EST DEFINI PAR LIRE_RESU AU FORMAT DATASET 58
            LICMP(1) = 'D1X'
            LICMP(2) = 'D1Y'
            LICMP(3) = 'D1Z'
            LICMP(4) = 'D2X'
            LICMP(5) = 'D2Y'
            LICMP(6) = 'D2Z'
            LICMP(7) = 'D3X'
            LICMP(8) = 'D3Y'
            LICMP(9) = 'D3Z'
            DO 120 INO = 1,NBNOEU
              DO 130 ICMP = 1,NBCMP
                INDICE = (INO-1)*NBCMP+ICMP
                ORIEN = .FALSE.
                IF(ZL(JCNSL-1 + INDICE)) THEN
                  DO 140 II = 1,9
                    IF (ZK8(JCNSC-1 +ICMP) .EQ. LICMP(II)) THEN
                      ORIEN = .TRUE.
                    ENDIF
 140              CONTINUE
C ON NE TRAITE PAS NON PLUS LES DRX DRY DRZ
                  IF (ZK8(JCNSC-1 +ICMP)(1:2) .EQ. 'DR') THEN
                    ORIEN = .TRUE.
                  ENDIF
                  IF(.NOT. ORIEN) THEN
                    NBMESU = NBMESU+1
                    ZI(LNOEUD-1 +NBMESU) = INO
                    ZK16(LCHAM-1 +NBMESU) = NOMCHA
                    IF(ZK8(JCNSC-1 +ICMP) .EQ. 'D1') THEN
                      ZK8(LRANGE-1 +NBMESU) = 'D1'
                      DO 141 II = 1,NBCMP
                        IF (ZCMPLX) THEN
                          VAL = DBLE(ZC(JCNSV-1 +(INO-1)*NBCMP+II))
                        ELSE
                          VAL = ZR(JCNSV-1 +(INO-1)*NBCMP+II)
                        ENDIF
                        IF(ZK8(JCNSC-1 +II) .EQ. LICMP(1)) THEN
                          ZR(LORI-1 +(NBMESU-1)*3+1) = VAL
                        ENDIF
                        IF(ZK8(JCNSC-1 +II) .EQ. LICMP(2)) THEN
                          ZR(LORI-1 +(NBMESU-1)*3+2) = VAL
                        ENDIF
                        IF(ZK8(JCNSC-1 +II) .EQ. LICMP(3)) THEN
                          ZR(LORI-1 +(NBMESU-1)*3+3) = VAL
                        ENDIF
 141                  CONTINUE
                    ENDIF
                    IF(ZK8(JCNSC-1 +ICMP) .EQ. 'D2') THEN
                      ZK8(LRANGE-1 +NBMESU) = 'D2'
                      DO 142 II = 1,NBCMP
                        IF (ZCMPLX) THEN
                          VAL = DBLE(ZC(JCNSV-1 +(INO-1)*NBCMP+II))
                        ELSE
                          VAL = ZR(JCNSV-1 +(INO-1)*NBCMP+II)
                        ENDIF
                        IF(ZK8(JCNSC-1 +II) .EQ. LICMP(4)) THEN
                          ZR(LORI-1 +(NBMESU-1)*3+1) = VAL
                        ENDIF
                        IF(ZK8(JCNSC-1 +II) .EQ. LICMP(5)) THEN
                          ZR(LORI-1 +(NBMESU-1)*3+2) = VAL
                        ENDIF
                        IF(ZK8(JCNSC-1 +II) .EQ. LICMP(6)) THEN
                          ZR(LORI-1 +(NBMESU-1)*3+3) = VAL
                        ENDIF
 142                  CONTINUE
                    ENDIF
                    IF(ZK8(JCNSC-1 +ICMP) .EQ. 'D3') THEN
                      ZK8(LRANGE-1 +NBMESU) = 'D3'
                      DO 143 II = 1,NBCMP
                        IF (ZCMPLX) THEN
                          VAL = DBLE(ZC(JCNSV-1 +(INO-1)*NBCMP+II))
                        ELSE
                          VAL = ZR(JCNSV-1 +(INO-1)*NBCMP+II)
                        ENDIF
                        IF(ZK8(JCNSC-1 +II) .EQ. LICMP(7)) THEN
                          ZR(LORI-1 +(NBMESU-1)*3+1) = VAL
                        ENDIF
                        IF(ZK8(JCNSC-1 +II) .EQ. LICMP(8)) THEN
                          ZR(LORI-1 +(NBMESU-1)*3+2) = VAL
                        ENDIF
                        IF(ZK8(JCNSC-1 +II) .EQ. LICMP(9)) THEN
                          ZR(LORI-1 +(NBMESU-1)*3+3) = VAL
                        ENDIF
 143                  CONTINUE
                    ENDIF

                    IF(ZK8(JCNSC-1 +ICMP)(1:2) .EQ. 'DX') THEN
                      ZK8(LRANGE-1 +NBMESU) = 'DX'
                      ZR(LORI-1 +(NBMESU-1)*3+1) = 1.0D0
                      ZR(LORI-1 +(NBMESU-1)*3+2) = 0.0D0
                      ZR(LORI-1 +(NBMESU-1)*3+3) = 0.0D0
                    ENDIF
                    IF(ZK8(JCNSC-1 +ICMP)(1:2) .EQ. 'DY') THEN
                      ZK8(LRANGE-1 +NBMESU) = 'DY'
                      ZR(LORI-1 +(NBMESU-1)*3+1) = 0.0D0
                      ZR(LORI-1 +(NBMESU-1)*3+2) = 1.0D0
                      ZR(LORI-1 +(NBMESU-1)*3+3) = 0.0D0
                    ENDIF
                    IF(ZK8(JCNSC-1 +ICMP)(1:2) .EQ. 'DZ') THEN
                      ZK8(LRANGE-1 +NBMESU) = 'DZ'
                      ZR(LORI-1 +(NBMESU-1)*3+1) = 0.0D0
                      ZR(LORI-1 +(NBMESU-1)*3+2) = 0.0D0
                      ZR(LORI-1 +(NBMESU-1)*3+3) = 1.0D0
                    ENDIF
                  ENDIF
                ENDIF
 130          CONTINUE
 120        CONTINUE
          ELSEIF (TYPRES(1:9).EQ.'MODE_GENE') THEN
C  SI RESULTAT DE TYPE MODE_GENE ON SUPPOSE QUE L'ORIENTATION
C  EST DEFINI PAR LIRE_RESU AU FORMAT DATASET 55
C  BOUCLE SUR LES NUMEROS D ORDRE POUR RECUPERATION DES DDL MESURE
            DO 200 IMODE = 1,NBORD
              CALL RSEXCH (NOMMES,NOMCHA,ZI(LORD-1+IMODE),CHAMNO,IRET)
              CALL CNOCNS(CHAMNO,'V',CHS)
              CALL JEVEUO(CHS//'.CNSC','L',JCNSC)
              CALL JEVEUO(CHS//'.CNSV','L',JCNSV)
              CALL JEVEUO(CHS//'.CNSL','L',JCNSL)
              DO 221 INO = 1,NBNOEU
                DO 231 ICMP = 1,NBCMP
                  INDICE = (INO-1)*NBCMP+ICMP
                  IF(ZL(JCNSL-1 + INDICE)) THEN
                    DCAPT = .FALSE.
                    IF (ZCMPLX) THEN
                      VAL = DBLE(ZC(JCNSV-1 +(INO-1)*NBCMP+ICMP))
                    ELSE
                      VAL = ZR(JCNSV-1 +(INO-1)*NBCMP+ICMP)
                    ENDIF
                    IF (ABS(VAL).GT.(100*R8PREM())) THEN
                      IF((ZK8(JCNSC-1 +ICMP).EQ.'DX') .OR.
     &                   (ZK8(JCNSC-1 +ICMP).EQ.'DY') .OR.
     &                   (ZK8(JCNSC-1 +ICMP).EQ.'DZ')) THEN
                        INOMES = INO
                        CHAMES = NOMCHA
                        COMPMS = ZK8(JCNSC-1 +ICMP)
                        DCAPT = .TRUE.
                      ENDIF
                      DO 210 IMESU = 1,NBMESU
                        INOCAP = ZI(LNOEUD-1 +IMESU)
                        CHACAP = ZK16(LCHAM-1 +IMESU)
                        COMCAP = ZK8(LRANGE-1 +IMESU)
                        IF((COMCAP .EQ. COMPMS) .AND.
     &                     (CHACAP .EQ. CHAMES) .AND.
     &                     (INOCAP .EQ. INOMES)) THEN
                          DCAPT = .FALSE.
                        ENDIF
210                   CONTINUE
                      IF (DCAPT) THEN
                        NBMESU = NBMESU+1
                        ZI(LNOEUD-1 +NBMESU) = INO
                        ZK16(LCHAM-1 +NBMESU) = NOMCHA
                        ZK8(LRANGE-1 +NBMESU) = ZK8(JCNSC-1 +ICMP)
                      ENDIF
                    ENDIF
                  ENDIF
231             CONTINUE
221           CONTINUE
200         CONTINUE

            DO 121 IMESU = 1,NBMESU
              IF(ZK8(LRANGE-1 +IMESU) .EQ. 'DX') THEN
                ZR(LORI-1 +(IMESU-1)*3+1) = 1.0D0
                ZR(LORI-1 +(IMESU-1)*3+2) = 0.0D0
                ZR(LORI-1 +(IMESU-1)*3+3) = 0.0D0
              ENDIF
              IF(ZK8(LRANGE-1 +IMESU) .EQ. 'DY') THEN
                ZR(LORI-1 +(IMESU-1)*3+1) = 0.0D0
                ZR(LORI-1 +(IMESU-1)*3+2) = 1.0D0
                ZR(LORI-1 +(IMESU-1)*3+3) = 0.0D0
              ENDIF
              IF(ZK8(LRANGE-1 +IMESU) .EQ. 'DZ') THEN
                ZR(LORI-1 +(IMESU-1)*3+1) = 0.0D0
                ZR(LORI-1 +(IMESU-1)*3+2) = 0.0D0
                ZR(LORI-1 +(IMESU-1)*3+3) = 1.0D0
              ENDIF
121         CONTINUE
          ENDIF
        ENDIF

        IF (NOMGD(1:4) .EQ. 'SIEF' .OR. NOMGD(1:4) .EQ. 'EPSI') THEN
          DO 220 INO = 1,NBNOEU
            DO 230 ICMP = 1,NBCMP
              INDICE = (INO-1)*NBCMP+ICMP
              IF(ZL(JCNSL-1 + INDICE)) THEN
                NBMESU = NBMESU+1
                ZI(LNOEUD-1 +NBMESU) = INO
                ZK16(LCHAM-1 +NBMESU) = NOMCHA
                ZK8(LRANGE-1 +NBMESU) = ZK8(JCNSC-1 +ICMP)
              ENDIF
 230        CONTINUE
 220      CONTINUE
        ENDIF

        CALL JEVEUO ( BASEMO//'           .ORDR' , 'L' , LORD )
C
        CH1S='&&PJEFPR.CH1S'
        CH2S='&&PJEFPR.CH2S'
C
C FIN BOUCLE SUR LES NOMCHA
 151  CONTINUE

      CALL GETVID ('MODELE_MESURE','MODELE',1,1,1,MODMES,IBID)

      CALL JEECRA (VNOEUD,'LONUTI',NBMESU,K8BID )
      CALL JEECRA (VRANGE,'LONUTI',NBMESU,K8BID )

      ZK16(LREF-1 +1) = MODMES
C PAS DE CALCUL DE MODIF STRUCTURALE POUR LES SDMIXTES
      IF (NBCHAM .GT. 1) CALL U2MESS('A','SOUSTRUC2_11')
      ZK16(LREF-1 +2) = NOMCHA
      ZK16(LREF-1 +3) = BASEMO
C
C DESTRUCTION DES VECTEURS DE TRAVAIL
C
      CALL DETRSD('CHAM_NO_S',CHS)
      CALL DETRSD('CHAM_NO_S',CH1S)
      CALL DETRSD('CHAM_NO_S',CH2S)

      CALL JEDEMA ( )
C
      END
