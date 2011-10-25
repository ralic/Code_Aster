      SUBROUTINE LRCOMM(RESU,TYPRES,NBORDR,CHMAT,CARAEL,MODELE)
      IMPLICIT  NONE
      INTEGER NBORDR
      CHARACTER*8 RESU,CHMAT,CARAEL,MODELE
      CHARACTER*16 TYPRES
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 24/10/2011   AUTEUR DELMAS J.DELMAS 
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
C
C     BUT:
C       STOCKER EVENTUELLEMENT : MODELE, CHAM_MATER, CARA_ELEM, EXCIT
C
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN   RESU     : NOM DE LA SD_RESULTAT
C IN   TYPRES   : TYPE DE LA SD_RESULTAT
C IN   CHMAT    : NOM DU CHAM_MATER
C IN   CARAEL   : NOM DU CARA_ELEM
C IN   MODELE   : NOM DU MODELE
C
C ......................................................................
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)

      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*6 NOMPRO
      PARAMETER (NOMPRO='LRCOMM')

      INTEGER IORDR,LORDR,NEXCI,JPARA
      INTEGER I,IRET,IBID,IAUX,NBTROU,NBPASE

      REAL*8 EPSI,RBID

      CHARACTER*8 CRIT,BASENO,K8BID,BLAN8
      CHARACTER*13 INPSCO
      CHARACTER*19 INFCHA,LISCHA,LISCH2
      CHARACTER*24 CHAMP,NOOBJ,FOMULT,K24B,COMPOR,CARCRI,BLAN24

      COMPLEX*16 CBID

      LOGICAL MATCST,COECST
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()

C               12345678
      BLAN8  = '        '

C               123456789012345678901234
      BLAN24 = '                        '
      COMPOR = BLAN24
C
      LISCHA = '&&'//NOMPRO//'.LISCHA    '

      CALL RSORAC(RESU,'LONUTI',IBID,RBID,K8BID,CBID,EPSI,CRIT,NBORDR,1,
     &            NBTROU)
      IF (NBORDR.LE.0)  CALL U2MESS('F','UTILITAI2_97')
      CALL WKVECT('&&'//NOMPRO//'.NUME_ORDR','V V I',NBORDR,LORDR)
      CALL RSORAC(RESU,'TOUT_ORDRE',IBID,RBID,K8BID,CBID,EPSI,CRIT,
     &            ZI(LORDR),NBORDR,NBTROU)

      IF(CHMAT.NE.BLAN8)THEN
        DO 10 I=1,NBORDR
          IORDR=ZI(LORDR+I-1)
          CALL RSADPA(RESU,'E',1,'CHAMPMAT',IORDR,0,JPARA,K8BID)
          ZK8(JPARA)=CHMAT
 10     CONTINUE
      ENDIF
      IF(CARAEL.NE.BLAN8)THEN
        DO 20 I=1,NBORDR
          IORDR=ZI(LORDR+I-1)
          CALL RSADPA(RESU,'E',1,'CARAELEM',IORDR,0,JPARA,K8BID)
          ZK8(JPARA)=CARAEL
 20     CONTINUE
      ENDIF
      IF(MODELE.NE.BLAN8)THEN
        IF(TYPRES(1:9).EQ.'EVOL_NOLI')THEN
          CALL NMDORC(MODELE,COMPOR,CARCRI)
          IF (COMPOR.NE.BLAN24) THEN
            DO 30 I=1,NBORDR
              IORDR=ZI(LORDR+I-1)
              CALL RSEXCH(RESU,'COMPORTEMENT',IORDR,CHAMP,IRET)
              IF (IRET.LE.100) THEN
                CALL COPISD('CHAMP_GD','G',COMPOR(1:19),CHAMP(1:19))
                CALL RSNOCH(RESU,'COMPORTEMENT',IORDR,' ')
              END IF
 30         CONTINUE
          ENDIF
        ENDIF
        DO 40 I=1,NBORDR
          IORDR=ZI(LORDR+I-1)
          CALL RSADPA(RESU,'E',1,'MODELE',IORDR,0,JPARA,K8BID)
          ZK8(JPARA)=MODELE
 40     CONTINUE
      ENDIF
      CALL GETFAC('EXCIT',NEXCI)
      IF(NEXCI.GT.0)THEN
        IF(TYPRES(1:4).EQ.'DYNA'.OR.TYPRES(1:4).EQ.'MODE')THEN
          CALL U2MESK('A','UTILITAI5_94',1,TYPRES)
         GOTO 60
        ENDIF
        NOOBJ ='12345678'//'.1234'//'.EXCIT.INFC'
        CALL GNOMSD(NOOBJ,10,13)
        LISCH2 = NOOBJ(1:19)
        IAUX = 1
        BASENO='&&'//NOMPRO
        INPSCO='&&'//NOMPRO//'_PSCO'
        IBID=0
        CALL PSLECT ( ' ', IBID, BASENO, RESU, IAUX,
     &                NBPASE, INPSCO, IRET )
        IF(TYPRES.EQ.'EVOL_ELAS'.OR.
     &     TYPRES.EQ.'EVOL_NOLI')THEN
          CALL NMDOME (K24B,K24B,K24B,LISCHA,NBPASE,INPSCO,BLAN8,IBID)
        ELSEIF(TYPRES.EQ.'EVOL_THER')THEN
          INFCHA = '&&'//NOMPRO//'_INFCHA    '
          CALL NTDOTH (K24B,K24B,K24B,FOMULT,MATCST,
     &                 COECST,INFCHA,NBPASE,INPSCO,BLAN8,IBID)
        ENDIF
        DO 50 I=1,NBORDR
          IORDR=ZI(LORDR+I-1)
          CALL RSADPA(RESU,'E',1,'EXCIT',IORDR,0,JPARA,K8BID)
          ZK24(JPARA)=LISCH2
 50     CONTINUE
        CALL COPISD(' ','G',LISCHA,LISCH2)
      ENDIF
 60   CONTINUE

      CALL JEDETR('&&'//NOMPRO//'.NUME_ORDR')
C
      CALL JEDEMA()
C
      END
