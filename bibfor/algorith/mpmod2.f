      SUBROUTINE MPMOD2(BASEMO,NOMMES,NBMESU,NBMTOT,BASEPR,
     &                  VNOEUD,VRANGE)
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/11/2004   AUTEUR VABHHTS J.PELLET 
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
C
C     PROJ_MESU_MODAL : PROJECTION DE LA MATRICE MODALE SUR LES CMP
C                       DES NOEUDS MESURE
C
C     IN  : BASEMO : NOM DE LA BASE DE PROJECTION
C     IN  : NOMMES : NOM DE LA MESURE
C
C     OUT  : NBMESU : NOMBRE DE MESURE (DATASET 58)
C     OUT  : NBMTOT : NOMBRE DE VECTEURS DE BASE
C     OUT  : BASEPR : NOM BASE PROJETEE SUIVANT DIRECTION MESURE
C     OUT  : VNOEUD : NOM RANGEMENT NOEUD MESURE
C     OUT  : VRANGE : NOM CORRESPONDANCE CMP SUIVANT VNOEUD
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
      CHARACTER*24  VNOEUD,VRANGE,BASEPR
      INTEGER       NBMESU, NBMTOT
C
      CHARACTER*1  TYPVAL
      CHARACTER*8  K8BID,SCAL,SCALAI
      CHARACTER*8  NOMGD,LICMP(30)
      CHARACTER*16 NOMROU, NOMCHA, CORRES,NOMCHP
      CHARACTER*19 CHAMNO, CH1S, CH2S, CHS
      CHARACTER*24 VORIEN

      INTEGER      LORD, LRED, LORI,LRANGE
      INTEGER      IMESU, II, IMODE, IRET
      INTEGER      IPOSD, ICMP, INO
      INTEGER      LNOEUD,IDESC,GD,NBNOEU,NBCMP
      INTEGER      JCNSD,JCNSC,JCNSV,JCNSL,JCNSK
      INTEGER      IBID,INDICE,NBCMPI

      LOGICAL      ZCMPLX,ORIEN

      REAL*8       VORI(3)
      REAL*8       VAL,RBID,VECT(3),R8PREM

      COMPLEX*16   CBID
C
C ----------------------------------------------------------------------
C
      DATA NOMROU/'MPMOD2'/
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
      CALL GETVTX ('MODELE_MESURE','NOM_CHAM',1,1,1,NOMCHA,IBID)
C
C RECUPERATION DU NB DE VECTEURS DE BASE : NBMTOT
      CALL RSORAC(BASEMO,'LONUTI',IBID,RBID,K8BID,CBID,RBID,'ABSOLU',
     &            NBMTOT,1,IBID)
C
C =============================================
C RECUPERATION DES OBJETS LIES A LA MESURE
C =============================================
C
C RECUPERATION ADRESSE DES NUMEROS D'ORDRE POUR CHAQUE MESURE
C
      CALL JEVEUO ( NOMMES//'           .ORDR' , 'L' , LORD )

      CHS = '&&MESURE.CHS'
      NBMESU = 0
C        -> EXISTENCE DU CHAMP DANS LA STRUCTURE DE DONNEES MESURE
      CALL RSEXCH (NOMMES,NOMCHA,ZI(LORD),CHAMNO,IRET)
      IF (IRET .NE. 0) THEN
        CALL UTDEBM('F', NOMROU , ' CHAMP INEXISTANT ' )
        CALL UTIMPK('L', ' MESURE '  , 1 , NOMMES )
        CALL UTIMPK('L', ' NOM_CHAM '  , 1 , NOMCHA )
        CALL UTFINM
      END IF
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

C ORDRE DE RANGEMENT MESURE SELON VRANGE ET VNOEUD
      VNOEUD = '&&RANGENOEUD'
      VORIEN = '&&ORIENTATION'
      VRANGE = '&&RANGECMP'
      BASEPR = '&&BASEPROJ'
      CALL WKVECT (VNOEUD, 'V V I', NBNOEU*NBCMP, LNOEUD)
      CALL WKVECT (VRANGE, 'V V K8', NBNOEU*NBCMP, LRANGE)
      CALL WKVECT (VORIEN, 'V V R', NBNOEU*NBCMP*3, LORI)

      IF (NOMGD(1:4) .EQ. 'DEPL') THEN
C RECUPERATION DE L ORIENTATION
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
 140          CONTINUE
              IF(.NOT. ORIEN) THEN
                NBMESU = NBMESU+1
                ZI(LNOEUD-1 +NBMESU) = INO
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
 141              CONTINUE
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
 142              CONTINUE
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
 143              CONTINUE
                ENDIF
              ENDIF
            ENDIF
 130      CONTINUE
 120    CONTINUE
      ENDIF

      IF (NOMGD(1:4) .EQ. 'SIEF' .OR. NOMGD(1:4) .EQ. 'EPSI') THEN
        DO 220 INO = 1,NBNOEU
          DO 230 ICMP = 1,NBCMP
            INDICE = (INO-1)*NBCMP+ICMP
            IF(ZL(JCNSL-1 + INDICE)) THEN
              NBMESU = NBMESU+1
              ZI(LNOEUD-1 +NBMESU) = INO
              ZK8(LRANGE-1 +NBMESU) = ZK8(JCNSC-1 +ICMP)
            ENDIF
 230      CONTINUE
 220    CONTINUE
      ENDIF

C     -> OBJET MATRICE MODALE REDUITE SUIVANT DIRECTION DE MESURE
      CALL WKVECT ( BASEPR,'V V R', NBMESU*NBMTOT , LRED)
C
C RECUPERATION SD CORRESPONDANCE ENTRE MAILLAGE MODELE/MESURE
C
       CORRES = '&&PJEFTE.CORRESP'
       CALL MPJEFT (CORRES)
C
C =========================================
C RECUPERATION ADRESSE DES NUMEROS D'ORDRE DES MODES
C =========================================
C
      CALL JEVEUO ( BASEMO//'           .ORDR' , 'L' , LORD )
C
      CH1S='&&PJEFPR.CH1S'
      CH2S='&&PJEFPR.CH2S'
C
C BOUCLE SUR TOUS LES MODES
C **************************
      DO 10 IMODE = 1,NBMTOT
C
        NOMCHP = NOMCHA
C MEMES VECTEURS DE BASE POUR : DEPL, VITE ET ACCE
        IF (NOMCHP .EQ. 'VITE' .OR. NOMCHP .EQ. 'ACCE')
     &        NOMCHP = 'DEPL'
C MEMES VECTEURS DE BASE POUR LES CONTRAINTES
        IF (NOMCHP(1:4) .EQ. 'SIEF')
     &        NOMCHP = 'SIGM_NOEU_DEPL'
C MEMES VECTEURS DE BASE POUR LES DEFORMATIONS
        IF (NOMCHP(1:4) .EQ. 'EPSI')
     &        NOMCHP = 'EPSI_NOEU_DEPL'
C      -> EXISTENCE DES CHAMPS DANS LA STRUCTURE DE DONNEES BASEMO
        CALL RSEXCH (BASEMO,NOMCHP,ZI(LORD-1+IMODE),CHAMNO,IRET)
C
        IF (IRET .NE. 0) THEN
          CALL UTDEBM('F', NOMROU , ' CHAMP INEXISTANT ' )
          CALL UTIMPK('L', ' BASE '  , 1 , BASEMO )
          CALL UTIMPK('L', ' NOM_CHAM '  , 1 , NOMCHP )
          CALL UTIMPI('S', ' NUME_ORDRE ',1, ZI (LORD-1+IMODE) )
          CALL UTFINM
        END IF
C
C       2-1 : TRANSFORMATION DE CHAMNO EN CHAM_NO_S : CH1S
        CALL DETRSD('CHAM_NO_S',CH1S)
        CALL CNOCNS(CHAMNO,'V',CH1S)

C       2-2 : PROJECTION DU CHAM_NO_S : CH1S -> CH2S
        CALL DETRSD('CHAM_NO_S',CH2S)
        CALL CNSPRJ(CH1S,CORRES,'V',CH2S,IRET)
        IF (IRET.GT.0) CALL UTMESS('F','MPMOD2','ECHEC PROJECTION')

C RECUPERATION DU CHAMP AU NOEUD INO

      CALL JEVEUO(CH2S//'.CNSK','L',JCNSK)
      CALL JEVEUO(CH2S//'.CNSD','L',JCNSD)
      CALL JEVEUO(CH2S//'.CNSC','L',JCNSC)
      CALL JEVEUO(CH2S//'.CNSV','L',JCNSV)
      CALL JEVEUO(CH2S//'.CNSL','L',JCNSL)

      NBCMPI = ZI(JCNSD-1 + 2)

C BOUCLE SUR LES POINTS DE MESURE

        DO 20 IMESU = 1,NBMESU

C NUMERO DU NOEUD ASSOCIE A IMESU : INO
          INO = ZI(LNOEUD-1 + IMESU)
          IF (NOMCHP(1:4) .EQ. 'DEPL') THEN
C
C CAS DES MESURES DE TYPE 'DEPL'
C ******************************
C
C DIRECTION DE MESURE (VECTEUR DIRECTEUR)
            DO 21 II = 1 , 3
              VORI(II) = ZR(LORI-1 + (IMESU-1)*3 +II)
 21         CONTINUE

C NORMALISATION DU VECTEUR DIRECTEUR
            VAL = 0.D0
            DO 22 II = 1,3
              VAL = VAL + VORI(II)*VORI(II)
 22         CONTINUE
            VAL = SQRT(VAL)
            IF (VAL.LT.R8PREM()) THEN
              CALL UTMESS('F',NOMROU,'NORME VECTEUR DIR. NULLE')
            ENDIF
            DO 23 II = 1,3
              VORI(II) = VORI(II)/VAL
 23         CONTINUE
C
C RECUPERATION DU CHAMP AU NOEUD (BASE)
C **************************

            DO 101 ICMP = 1,NBCMPI
              IF (ZK8(JCNSC-1 +ICMP) .EQ. 'DX')
     &          VECT(1) = ZR(JCNSV-1 +(INO-1)*NBCMPI+ICMP)
              IF (ZK8(JCNSC-1 +ICMP) .EQ. 'DY')
     &          VECT(2) = ZR(JCNSV-1 +(INO-1)*NBCMPI+ICMP)
              IF (ZK8(JCNSC-1 +ICMP) .EQ. 'DZ')
     &          VECT(3) = ZR(JCNSV-1 +(INO-1)*NBCMPI+ICMP)
 101        CONTINUE

C DETERMINATION DE LA BASE RESTREINTE
C *********************************************
C
            IPOSD = (IMODE-1)*NBMESU + IMESU
            ZR(LRED-1 + IPOSD) = 0.D0

            DO 300 II = 1 , 3
              ZR(LRED-1 + IPOSD) = ZR(LRED-1 + IPOSD)
     &               + VECT(II) * VORI(II)
 300        CONTINUE
C
          ELSE IF ( (NOMCHP(1:14) .EQ. 'EPSI_NOEU_DEPL') .OR.
     &              (NOMCHP(1:14) .EQ. 'SIGM_NOEU_DEPL') ) THEN
C
C CAS DES MESURES DE TYPE 'EPSI_NOEU_DEPL' OU 'SIEF_NOEU'
C ************************************************************
C ON NE TRAITE PAS LES ORIENTATIONS POUR LES DEFORMATIONS ET
C   CONTRAINTES POUR L INSTANT
C
C DETERMINATION DE LA BASE RESTREINTE
C **************************

            IPOSD = (IMODE-1)*NBMESU + IMESU
            DO 401 ICMP = 1,NBCMPI
              IF (ZK8(JCNSC-1 +ICMP) .EQ. ZK8(LRANGE-1 +IMESU))
     &          ZR(LRED-1 +IPOSD) = ZR(JCNSV-1 +(INO-1)*NBCMPI+ICMP)
 401        CONTINUE
C
          ENDIF
C
C =========================================
C FIN DE LA BOUCLE SUR LES POINTS DE MESURE
C =========================================
C
 20     CONTINUE

C FIN DE LA BOUCLE SUR LES MODES
C ******************************
 10   CONTINUE
C
C DESTRUCTION DES VECTEURS DE TRAVAIL
C
      CALL JEDETR (VORIEN)
      CALL DETRSD('CHAM_NO_S',CHS)
      CALL DETRSD('CHAM_NO_S',CH1S)
      CALL DETRSD('CHAM_NO_S',CH2S)
      CALL DETRSD('CORRESP_2_MAILLA',CORRES)

      CALL JEDEMA ( )
C
      END
