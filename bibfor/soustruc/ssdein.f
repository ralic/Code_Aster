      SUBROUTINE SSDEIN(UL,UG,MAIL,NOCAS)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
      IMPLICIT NONE
C     ARGUMENTS:
C     ----------
C ----------------------------------------------------------------------
C     BUT:
C      - CALCULER LE CHAMP DE DEPLACEMENT INTERNE A UNE SOUS-STRUCTURE
C        A PARTIR DU CHAMP DE DEPLACEMENT CONNU SUR SES NOEUDS EXTERNES
C
C IN_F,OU_J: UL : NOM DU CHAMP LOCAL A LA SOUS-STRUCTURE
C IN_F,IN_J: UG : NOM DU CHAMP GLOBAL (MODELE DE NIVEAU SUPERIEUR)
C IN_F     : MAIL : NOM DE LA (SUPER)MAILLE SUR LAQUELLE ON VEUT UL
C IN_F     : NOCAS: NOM DU CHARGEMENT CORRESPONDANT (EN PRINCIPE) A UG.
C                   (EVENTUELLEMENT : ' ')
C
      INCLUDE 'jeveux.h'
      CHARACTER*8  UL,UG,MAIL,NOCAS,MAG,MAL,NOMGD,KBID,NOMACR
      CHARACTER*14 NUL
      CHARACTER*19 NUG2,NUL2
      REAL*8 LAMBDA(6,6),ANGL(3), PGL(3,3)
      LOGICAL EXISDG,EXIL,EXIG
      CHARACTER*8 ROTA,CH8(2)
      CHARACTER*19 UG2,UL2
      CHARACTER*24 VALK(2)
C ----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,IABIDO ,IACONX ,IADESC ,IADGG ,IADGL ,IALICA 
      INTEGER IALICH ,IAMACR ,IANUEG ,IANUEL ,IAPARR ,IAPHI0 ,IAPHIE 
      INTEGER IAPRNG ,IAPRNL ,IAREFE ,IASUPM ,IAVALG ,IAVALL ,IAVALP 
      INTEGER IAVALT ,IBI ,IBID ,IBLPH ,ICMP ,ICOG ,ICOL 
      INTEGER IEQG ,IEQL ,IER ,IIBLPH ,ILI ,INOE ,INOG 
      INTEGER INOL ,IRET ,ISMA ,J ,JDESM ,LGBLPH ,NBLPH 
      INTEGER NBNOET ,NCMPMX ,NDDLE ,NDDLI ,NDDLT ,NEC ,NLBLPH 
      INTEGER NUEQG ,NUEQL 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      UG2= UG
      UL2= UL
C
      CALL DISMOI('F','NOM_MAILLA',UG,'CHAM_NO',IBI,MAG,IER)
      CALL JEVEUO(UG2//'.REFE','L',IAVALT)
      NUG2=ZK24(IAVALT+1)(1:19)
      CALL DISMOI('F','NOM_GD',UG,'CHAM_NO',IBI,NOMGD,IER)
      IF (NOMGD(1:6).NE.'DEPL_R') CALL U2MESK('F','SOUSTRUC_43',1,NOMGD)
C
C
C     1- RECUPERATION DU NOM DU MACR_ELEM:
C     ------------------------------------
      CALL JEVEUO(MAG//'.NOMACR','L',IAMACR)
      CALL JENONU(JEXNOM(MAG//'.SUPMAIL',MAIL),ISMA)
      IF (ISMA.LE.0) THEN
        CH8(1)=MAIL
        CH8(2)=MAG
        CALL U2MESK('F','SOUSTRUC_44',2,CH8)
      ENDIF
      CALL JEVEUO(JEXNOM(MAG//'.SUPMAIL',MAIL),'L',IASUPM)
      NOMACR= ZK8(IAMACR-1+ISMA)
      NUL= NOMACR
      NUL2=NUL//'.NUME'
C
      CALL DISMOI('F','NOM_MAILLA',NOMACR,'MACR_ELEM_STAT',IBI,MAL,IER)
      CALL JEVEUO(NOMACR//'.CONX','L',IACONX)
      CALL JEVEUO(NOMACR//'.DESM','L',JDESM)
      NBNOET= ZI(JDESM-1+2)+ZI(JDESM-1+8)+ZI(JDESM-1+9)
      NDDLE= ZI(JDESM-1+4)
      NDDLI= ZI(JDESM-1+5)
      NDDLT= NDDLE+NDDLI
C                 '&&SSDEIN.VALP' EST UN VECTEUR DE TRAVAIL :
      CALL WKVECT('&&SSDEIN.VALP','V V R',NDDLT,IAVALP)
C
      CALL JEVEUO(UG2//'.VALE','L',IAVALG)
      CALL JEVEUO(JEXNUM(NUG2//'.PRNO',1),'L',IAPRNG)
      CALL JEVEUO(NUG2//'.NUEQ','L',IANUEG)
      CALL JEVEUO(NUL2//'.NUEQ','L',IANUEL)
      CALL DISMOI('F','NB_EC',NOMGD,'GRANDEUR',NEC,KBID,IER)
      CALL DISMOI('F','NB_CMP_MAX',NOMGD,'GRANDEUR',NCMPMX,KBID,IER)
C
C
C     2- ALLOCATION DU CHAM_NO RESULTAT : UL
C     --------------------------------------
C     .DESC:
      CALL WKVECT(UL2//'.DESC','G V I',2,IADESC)
      CALL JEVEUO(UG2//'.DESC','L',IABIDO)
      ZI(IADESC-1+1)=ZI(IABIDO-1+1)
      ZI(IADESC-1+2)=1
      CALL JEECRA(UL2//'.DESC','DOCU',IBID,'CHNO')
C     .REFE:
      CALL WKVECT(UL2//'.REFE','G V K24',4,IAREFE)
      ZK24(IAREFE-1+1)=MAL
      ZK24(IAREFE-1+2)=NUL//'.NUME'
C     .VALE:
      CALL WKVECT(UL2//'.VALE','G V R',NDDLT,IAVALL)
C
C
C     4- CALCUL DES VALEURS DE UL.VALE:
C     ---------------------------------
C
C     4-1- ON RECOPIE UG.VALE DANS Q_E:
C     ---------------------------------
      DO 1, INOE=1,NBNOET
        INOG=ZI(IASUPM-1+INOE)
        INOL= ZI(IACONX-1+3*(INOE-1)+2)
        ILI= ZI(IACONX-1+3*(INOE-1)+1)
C
        CALL JEVEUO(JEXNUM(NUL2//'.PRNO',ILI),'L',IAPRNL)
C
        NUEQL = ZI(IAPRNL-1+ (INOL-1)* (NEC+2)+1)
        IADGL = IAPRNL - 1 + (INOL-1)* (NEC+2)+3
        IEQL=ZI(IANUEL-1+NUEQL)
        IF (IEQL.LE.NDDLI)
     &  CALL U2MESS('F','SOUSTRUC_45')
C
        NUEQG = ZI(IAPRNG-1+ (INOG-1)* (NEC+2)+1)
        IADGG = IAPRNG - 1 + (INOG-1)* (NEC+2)+3
C
        ICOL = 0
        ICOG = 0
        DO 2 ,ICMP = 1,NCMPMX
          EXIL= EXISDG(ZI(IADGL),ICMP)
          EXIG= EXISDG(ZI(IADGG),ICMP)
          IF (EXIL) ICOL=ICOL+1
          IF (EXIG) ICOG=ICOG+1
          IF (EXIG.AND.EXIL) THEN
            IEQL= ZI(IANUEL-1+NUEQL-1+ICOL)
            IEQG= ZI(IANUEG-1+NUEQG-1+ICOG)
            ZR(IAVALL-1+IEQL) = ZR(IAVALG-1+IEQG)
          END IF
 2      CONTINUE
 1    CONTINUE
C
C
C     4-2- ON CHANGE LE REPERE (ROTATION G->L ) : Q_E  --> Q_E :
C     ----------------------------------------------------------
C
        CALL SSRONE(MAG,ISMA,ROTA)
C
        IF (ROTA(1:3).EQ.'OUI') THEN
          CALL JEVEUO(MAG//'.PARA_R','L',IAPARR)
          ANGL(1) = ZR(IAPARR-1+14*(ISMA-1)+4)
          ANGL(2) = ZR(IAPARR-1+14*(ISMA-1)+5)
          ANGL(3) = ZR(IAPARR-1+14*(ISMA-1)+6)
          CALL MATROT ( ANGL , PGL )
          DO 710 I = 1,3
            DO 712 J = 1,3
              LAMBDA(I,J) = PGL(I,J)
              LAMBDA(I,J+3) = 0.D0
              LAMBDA(I+3,J) = 0.D0
              LAMBDA(I+3,J+3) = PGL(I,J)
 712        CONTINUE
 710      CONTINUE
          CALL SSVARO(LAMBDA,'GL',.FALSE.,'EXTE',NOMACR,IAVALL,IAVALP)
          DO 4, I=1,NDDLE
            ZR(IAVALL-1+NDDLI+I)= ZR(IAVALP-1+NDDLI+I)
 4        CONTINUE
          CALL JEDETR('&&SSVARO.IINO')
        END IF
C
C
C     4-3  Q_I= (K_II**-1)*F_I :
C     -------------------------
      IF (NOCAS(1:1).NE.' ') THEN
        CALL JEEXIN(JEXNOM(NOMACR//'.LICA',NOCAS),IRET)
        IF (IRET.EQ.0) THEN
           VALK(1) = NOCAS
           VALK(2) = NOMACR
           CALL U2MESK('A','SOUSTRUC_46', 2 ,VALK)
        ELSE
          CALL JEVEUO(JEXNOM(NOMACR//'.LICA',NOCAS),'L',IALICA)
          CALL JEVEUO(JEXNOM(NOMACR//'.LICH',NOCAS),'L',IALICH)
C
          IF (ZK8(IALICH-1+1)(1:3).EQ.'NON') THEN
C
C           -- LE CHARGEMENT N'EST PAS "SUIVEUR" :
            IF (ROTA(1:3).EQ.'OUI') THEN
              CALL SSVARO(LAMBDA,'GL',.FALSE.,'TOUS',NOMACR,
     &                   IALICA,IAVALP)
              CALL SSVAU1(NOMACR,IAVALP,IAVALP)
              DO 5, I=1,NDDLI
                ZR(IAVALL-1+I)=ZR(IAVALP-1+I)
 5            CONTINUE
            ELSE
              DO 6, I=1,NDDLI
                ZR(IAVALL-1+I)=ZR(IALICA-1+NDDLT+I)
 6            CONTINUE
            END IF
C
          ELSE IF (ZK8(IALICH-1+1)(1:3).EQ.'OUI') THEN
C
C           -- LE CHARGEMENT EST "SUIVEUR" :
            DO 7, I=1,NDDLI
              ZR(IAVALL-1+I)=ZR(IALICA-1+NDDLT+I)
 7          CONTINUE
          ELSE
            CALL U2MESS('F','SOUSTRUC_47')
          END IF
        END IF
      ENDIF
C
C
C
C     4-4  Q_I= Q_I + PHI_IE * Q_E :
C     ------------------------------
      CALL JELIRA(NOMACR//'.PHI_IE','LONMAX',LGBLPH,KBID)
      CALL JELIRA(NOMACR//'.PHI_IE','NMAXOC',NBLPH,KBID)
      NLBLPH=LGBLPH/NDDLI

      J=0
      DO 10, IBLPH=1,NBLPH
        CALL JEVEUO(JEXNUM(NOMACR//'.PHI_IE',IBLPH),'L',IAPHI0)
        DO 11, IIBLPH=1,NLBLPH
          J=J+1
          IF (J.GT.NDDLE) GO TO 13
          IAPHIE=IAPHI0+ (IIBLPH-1)*NDDLI
          DO 12, I=1,NDDLI
            ZR(IAVALL-1+I)=ZR(IAVALL-1+I)
     &       - ZR(IAPHIE-1+I)* ZR(IAVALL-1+NDDLI+J)
 12       CONTINUE
 11     CONTINUE
 13     CONTINUE
        CALL JELIBE(JEXNUM(NOMACR//'.PHI_IE',IBLPH))
 10   CONTINUE
C
      CALL JEDETR('&&SSDEIN.VALP')
C
      CALL JEDEMA()
      END
