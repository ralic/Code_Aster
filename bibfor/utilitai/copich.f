      SUBROUTINE COPICH ( BASE, CH1Z, CH2Z )

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C RESPONSABLE VABHHTS J.PELLET

      IMPLICIT NONE
      CHARACTER*1   BASE
      CHARACTER*(*)  CH1Z, CH2Z
C ----------------------------------------------------------------------
C
C   BUT:
C   DUPLIQUER UN CHAMP_GD SOUS UN AUTRE NOM.
C    L'EXISTENCE DE CH1Z EST OBLIGATOIRE
C   (SI CH2Z EXISTE DEJA, ON L'ECRASE)
C
C     IN       BASE        'G' , 'V' , ... : BASE DE CREATION DE CH2
C     IN       CH1Z    K19  NOM DU CHAMP_GD A DUPLIQUER
C     IN/JXOUT CH2Z    K19  NOM DU CHAMP_GD A CREER
C
C-----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------

      CHARACTER*4  DOCU
      CHARACTER*8  NOMU,K8BID
      CHARACTER*16 CONCEP,CMD
      CHARACTER*19 PRNO,PRNO2,CH1ESC,CH2ESC,CH1,CH2
      CHARACTER*24  NOOJB
      INTEGER      IBID,IRET,IRET1,IRET2,JAD,IER,NBSD,ILIMPI,IFETI,
     &             IFETC1,IFETC2,IDD
C-----------------------------------------------------------------------
      CALL JEMARQ()

      CH1 = CH1Z
      CH2 = CH2Z

      CALL JEEXIN(CH1 // '.DESC',IRET1)
      CALL JEEXIN(CH1 // '.CELD',IRET2)
      IF (MAX(IRET1,IRET2).EQ.0) GO TO 9999

      IF (IRET1.GT.0) THEN
        CALL JELIRA(CH1//'.DESC','DOCU',IBID,DOCU)
      ELSE
        CALL JELIRA(CH1//'.CELD','DOCU',IBID,DOCU)
      END IF

C     -- CAS DES CHAM_NO :
C     ----------------------
      IF  (DOCU.EQ.'CHNO') THEN

C --- ON DUPLIQUE LE CHAM_NO MAITRE DANS TOUS LES CAS

        CALL JEDUP1(CH1//'.DESC',BASE,CH2//'.DESC')
        CALL JEDUP1(CH1//'.REFE',BASE,CH2//'.REFE')
        CALL JEDUP1(CH1//'.VALE',BASE,CH2//'.VALE')


C       SI LE NOUVEAU CHAM_NO DOIT ETRE CREE SUR 'G', ON IMPOSE
C       QUE LE NOM DU PROF_CHNO DE CE CHAMP  COMMENCE PAR LE NOM
C       UTILISATEUR DU RESULTAT DE LA COMMANDE EN COURS :
C       --------------------------------------------------------------
        IF (BASE.EQ.'G') THEN
          CALL GETRES(NOMU,CONCEP,CMD)
          CALL DISMOI('F','PROF_CHNO',CH2,'CHAM_NO',IBID,PRNO,IER)
C         -- REMARQUE : UN CHAM_NO PEUT NE PAS AVOIR DE PROF_CHNO (' '):
          IF (PRNO.NE.' ') THEN
            IF (PRNO(1:8).NE.NOMU) THEN
              NOOJB='12345678.PRCHN00000.PRNO'
              CALL GNOMSD ( NOOJB,15,19 )
              PRNO2=NOOJB(1:19)
              CALL COPISD('PROF_CHNO',BASE,PRNO,PRNO2)
              CALL JEVEUO(CH2//'.REFE','E',JAD)
              ZK24(JAD-1+2)=PRNO2
            END IF
          END IF
        END IF

C --- SI FETI, ON DUPLIQUE AUSSI LES CHAM_NO ESCLAVES
        CALL JEEXIN(CH1//'.FETC',IFETI)
        IF (IFETI.NE.0) THEN
          CALL JELIRA(CH1//'.FETC','LONMAX',NBSD,K8BID)
          CALL JEVEUO('&FETI.LISTE.SD.MPI','L',ILIMPI)
          CALL JEVEUO(CH1//'.FETC','L',IFETC1)
          CALL JEEXIN(CH2//'.FETC',IRET)
C --- SI LE CHAM_NO CH2 N'EST PAS FETI, ON CREE LES CHAM_NOS FILS
C     SINON ON LES REUTILISE
          IF (IRET.EQ.0) THEN
            CALL WKVECT(CH2//'.FETC',BASE//' V K24',NBSD,IFETC2)
          ELSE
            CALL JEVEUO(CH2//'.FETC','L',IFETC2)
          ENDIF
C --- BOUCLE SUR LES SOUS-DOMAINES CF VTCREB OU VTCMBL PAR EXEMPLE
          DO 20 IDD=1,NBSD
            IF (ZI(ILIMPI+IDD).EQ.1)  THEN
              CH1ESC=ZK24(IFETC1+IDD-1)(1:19)
              IF (IRET.EQ.0) THEN
                CALL GCNCON('.',K8BID)
                K8BID(1:1)='F'
                CH2ESC=CH2(1:11)//K8BID
                ZK24(IFETC2+IDD-1)(1:19)=CH2ESC
              ELSE
                CH2ESC=ZK24(IFETC2+IDD-1)(1:19)
              ENDIF
              CALL JEDUP1(CH1ESC//'.DESC',BASE,CH2ESC//'.DESC')
              CALL JEDUP1(CH1ESC//'.REFE',BASE,CH2ESC//'.REFE')
              CALL JEDUP1(CH1ESC//'.VALE',BASE,CH2ESC//'.VALE')
            ENDIF
   20     CONTINUE
        ENDIF

C     -- CAS DES CARTES :
C     ----------------------
      ELSE IF  (DOCU.EQ.'CART') THEN
        CALL JEDUP1(CH1//'.DESC',BASE,CH2//'.DESC')
        CALL JEDUP1(CH1//'.LIMA',BASE,CH2//'.LIMA')
        CALL JEDUP1(CH1//'.NOLI',BASE,CH2//'.NOLI')
        CALL JEDUP1(CH1//'.NOMA',BASE,CH2//'.NOMA')
        CALL JEDUP1(CH1//'.VALE',BASE,CH2//'.VALE')


C     -- CAS DES CHAM_ELEM :
C     ----------------------
      ELSE IF  (DOCU.EQ.'CHML') THEN
        CALL JEDUP1(CH1//'.CELD',BASE,CH2//'.CELD')
        CALL JEDUP1(CH1//'.CELK',BASE,CH2//'.CELK')
        CALL JEDUP1(CH1//'.CELV',BASE,CH2//'.CELV')


C     -- CAS DES RESUELEM :
C     ----------------------
      ELSE IF  (DOCU.EQ.'RESL') THEN
        CALL JEDUP1(CH1//'.DESC',BASE,CH2//'.DESC')
        CALL JEDUP1(CH1//'.NOLI',BASE,CH2//'.NOLI')
        CALL JEDUP1(CH1//'.RESL',BASE,CH2//'.RESL')
        CALL JEDUP1(CH1//'.RSVI',BASE,CH2//'.RSVI')


      ELSE
         CALL U2MESS('F','CALCULEL_17')
      END IF

9999  CONTINUE
      CALL JEDEMA()
      END
