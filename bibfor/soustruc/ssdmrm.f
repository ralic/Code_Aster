      SUBROUTINE SSDMRM ( MAG )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C     ARGUMENTS:
C     ----------
      CHARACTER*8 MAG
C ----------------------------------------------------------------------
C     BUT:
C        - TRAITER LE MOT CLEF "RECO_SUPER_MAILLE"
C          DE LA COMMANDE DEFI_MAILLAGE.
C
C     IN:
C        MAG : NOM DU MAILLAGE QUE L'ON DEFINIT.
C     VAR:
C     -- MODIFICATION DE L'OBJET .NOEUD_CONF CREE DANS SSDMRC
C
C ----------------------------------------------------------------------
C INSPI SSDMRM  SSDMRG
C ---------------- COMMUNS NORMALISES  JEVEUX  -------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*8  KBID,CRIT,NOMACR,MAL,NOSMA,NOGNOI,NOGNOJ
      INTEGER ZI
      REAL*8 ZR,PREC,DI,DJ
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16,OPTION
      CHARACTER*24 ZK24,VALK(2)
      CHARACTER*32 ZK32,JEXNOM
      CHARACTER*80 ZK80
      INTEGER      IARG
C ----------------------------------------------------------------------
      CALL JEMARQ()
      CALL GETFAC('RECO_SUPER_MAILLE',NOCC)
      IF (NOCC.EQ.0) GO TO 9999
C
C     -- ON RECUPERE CERTAINES DIMENSIONS:
C     ------------------------------------
      CALL JEVEUO(MAG//'.DIME','L',IADIME)
      NBSMA=ZI(IADIME-1+4)
C
      CALL JEVEUO(MAG//'.NOMACR','L',IAMACR)
      CALL JEVEUO(MAG//'.NOEUD_CONF','E',IANCNF)
C
      CALL JEVEUO(MAG//'.COORDO_2','L',IACOO2)
      CALL JEVEUO(MAG//'.DIME_2','L',IADIM2)
      CALL JEVEUO(MAG//'.PARA_R','L',IAPARR)
      CALL WKVECT('&&SSDMRM.LIKM','V V K8',NBSMA,IALIKM)
      CALL WKVECT('&&SSDMRM.LIKG','V V K8',NBSMA,IALIKG)
C
C
C     -- BOUCLE SUR LES OCCURENCES DU MOT-CLEF:
C     -----------------------------------------
      LONGI=0
      LONGJ=0
      DO 2, IOCC=1,NOCC
C
C     -- ON RECUPERE LA LISTE DES MAILLES ET LA LISTE DES GROUP_NO:
C     -------------------------------------------------------------
        CALL GETVTX('RECO_SUPER_MAILLE','SUPER_MAILLE',
     &               IOCC,IARG,NBSMA,ZK8(IALIKM),N1)
        CALL GETVTX('RECO_SUPER_MAILLE','GROUP_NO',IOCC,IARG,NBSMA,
     &               ZK8(IALIKG),N2)
        IF (N1.LT.0) CALL U2MESS('F','SOUSTRUC_64')
        IF (N1.NE.N2) CALL U2MESS('F','SOUSTRUC_65')
        IF (N1.LT.2) CALL U2MESS('F','SOUSTRUC_66')
C
        NBSMAR=N1
C
        CALL GETVTX('RECO_SUPER_MAILLE','OPTION',IOCC,IARG,1,OPTION,N1)
        IF (OPTION(1:11).EQ.'GEOMETRIQUE') THEN
          CALL GETVR8('RECO_SUPER_MAILLE','PRECISION',IOCC,IARG,1,
     &                PREC,N1)
          CALL GETVTX('RECO_SUPER_MAILLE','CRITERE',IOCC,IARG,1,CRIT,N1)
        END IF
C
        DO 5, I=1,NBSMAR
          NOSMA= ZK8(IALIKM-1+I)
          NOGNOI= ZK8(IALIKG-1+I)
          CALL JENONU(JEXNOM(MAG//'.SUPMAIL',NOSMA),ISMAI)
          DI=ZR(IAPARR-1+14*(ISMAI-1)+13)
          NOMACR= ZK8(IAMACR-1+ISMAI)
          CALL DISMOI('F','NOM_MAILLA',NOMACR,'MACR_ELEM_STAT',
     &                 IBI,MAL,IER)
          CALL JEVEUO(NOMACR//'.LINO','L',IALINI)
          CALL JELIRA(NOMACR//'.LINO','LONUTI',NBEXTI,KBID)
          CALL JEVEUO(JEXNOM(MAL//'.GROUPENO',NOGNOI),'L',IAGNO)
          CALL JELIRA(JEXNOM(MAL//'.GROUPENO',NOGNOI),'LONUTI',
     &                 NBNGNO,KBID)
          CALL UTLISI('INTER',ZI(IAGNO),NBNGNO,ZI(IALINI),NBEXTI,
     &                 IBID,0,N3)
          NBNORI=-N3
          IF (NBNORI.GT.LONGI) THEN
            IF (LONGI.NE.0) CALL JEDETR('&&SSDMRM.LIII')
C           POUR NE PAS LE DETRUIRE A CHAQUE FOIS, ON ALLOUE PLUS GRAND
            LONGI=(NBNORI+10)*2
            CALL WKVECT('&&SSDMRM.LIII','V V I',LONGI,IALIII)
          END IF
          CALL UTLISI('INTER',ZI(IAGNO),NBNGNO,ZI(IALINI),NBEXTI,
     &                 ZI(IALIII),NBNORI,NBID)
          DO 6, J=I+1,NBSMAR
            NOSMA= ZK8(IALIKM-1+J)
            NOGNOJ= ZK8(IALIKG-1+J)
            CALL JENONU(JEXNOM(MAG//'.SUPMAIL',NOSMA),ISMAJ)
            DJ=ZR(IAPARR-1+14*(ISMAJ-1)+13)
            NOMACR= ZK8(IAMACR-1+ISMAJ)
            CALL DISMOI('F','NOM_MAILLA',NOMACR,'MACR_ELEM_STAT',
     &                   IBI,MAL,IER)
            CALL JEVEUO(NOMACR//'.LINO','L',IALINJ)
            CALL JELIRA(NOMACR//'.LINO','LONUTI',NBEXTJ,KBID)
            CALL JEVEUO(JEXNOM(MAL//'.GROUPENO',NOGNOJ),'L',IAGNO)
            CALL JELIRA(JEXNOM(MAL//'.GROUPENO',NOGNOJ),'LONUTI',
     &                   NBNGNO,KBID)
            CALL UTLISI('INTER',ZI(IAGNO),NBNGNO,ZI(IALINJ),NBEXTJ,
     &                   IBID,0,N3)
            NBNORJ=-N3
            IF (NBNORJ.GT.LONGJ) THEN
              IF (LONGJ.NE.0) CALL JEDETR('&&SSDMRM.LIIJ')
              LONGJ=(NBNORJ+10)*2
              CALL WKVECT('&&SSDMRM.LIIJ','V V I',LONGJ,IALIIJ)
            END IF
            CALL UTLISI('INTER',ZI(IAGNO),NBNGNO,ZI(IALINJ),NBEXTJ,
     &                   ZI(IALIIJ),NBNORJ,NBID)
C
            DJ=MIN(DI,DJ)
C
            IF (NBNORI.NE.NBNORJ) THEN
              VALK(1) = NOGNOI
              VALK(2) = NOGNOJ
              CALL U2MESK('A','SOUSTRUC_67', 2 ,VALK)
            ENDIF
            NBNORE= MIN(NBNORI,NBNORJ)
C
C
            IF (OPTION(1:13).EQ.'NOEUD_A_NOEUD') THEN
C           ---------------------------------
            DO 61, II=1,NBNORE
              INOII=INDIIS(ZI(IALINI),ZI(IALIII-1+II),1,NBEXTI)
              INOJJ=INDIIS(ZI(IALINJ),ZI(IALIIJ-1+II),1,NBEXTJ)
              INOI= ZI(IADIM2-1+4*(ISMAI-1)+3)+INOII
              INOJ= ZI(IADIM2-1+4*(ISMAJ-1)+3)+INOJJ
              IF (INOI.LT.INOJ) THEN
                ZI(IANCNF-1+INOJ)=INOI
              ELSE
                ZI(IANCNF-1+INOI)=INOJ
              END IF
 61         CONTINUE
C
C
            ELSE IF (OPTION(1:7).EQ.'INVERSE') THEN
C           -----------------------------------
            DO 62, II=1,NBNORE
              IF (I.EQ.1) THEN
                KK=NBNORE+1-II
                INOII=INDIIS(ZI(IALINI),ZI(IALIII-1+KK),1,NBEXTI)
              ELSE
                INOII=INDIIS(ZI(IALINI),ZI(IALIII-1+II),1,NBEXTI)
              END IF
              INOJJ=INDIIS(ZI(IALINJ),ZI(IALIIJ-1+II),1,NBEXTJ)
              INOI= ZI(IADIM2-1+4*(ISMAI-1)+3)+INOII
              INOJ= ZI(IADIM2-1+4*(ISMAJ-1)+3)+INOJJ
              IF (INOI.LT.INOJ) THEN
                ZI(IANCNF-1+INOJ)=INOI
              ELSE
                ZI(IANCNF-1+INOI)=INOJ
              END IF
 62         CONTINUE
C
C
            ELSE IF (OPTION(1:11).EQ.'GEOMETRIQUE') THEN
C           --------------------------------------
              ICO=0
              DO 63, II=1,NBNORE
                INOII=INDIIS(ZI(IALINI),ZI(IALIII-1+II),1,NBEXTI)
                INOI= ZI(IADIM2-1+4*(ISMAI-1)+3)+INOII
                DO 631, JJ=1,NBNORE
                  INOJJ=INDIIS(ZI(IALINJ),ZI(IALIIJ-1+JJ),1,NBEXTJ)
                  INOJ= ZI(IADIM2-1+4*(ISMAJ-1)+3)+INOJJ
                  CALL SSDMU1(DJ,CRIT,PREC,ZR(IACOO2+3*(INOI-1)),
     &                          ZR(IACOO2+3*(INOJ-1)),ICONF)
                  IF (ICONF.EQ.0) THEN
                    IF (INOI.LT.INOJ) THEN
                      ZI(IANCNF-1+INOJ)=INOI
                    ELSE
                      ZI(IANCNF-1+INOI)=INOJ
                    END IF
                    ICO=ICO+1
                    GO TO 63
                  END IF
 631            CONTINUE
 63           CONTINUE
C
              IF (NBNORE.NE.ICO) THEN
                 VALK(1) = NOGNOI
                 VALK(2) = NOGNOJ
                 CALL U2MESK('A','SOUSTRUC_68', 2 ,VALK)
              ENDIF
C
            END IF
C
 6        CONTINUE
 5      CONTINUE
C
 2    CONTINUE
C
C
 9999 CONTINUE
      CALL JEDETC('V','&&SSDMRM',1)
      CALL JEDEMA()
      END
