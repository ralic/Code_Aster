      SUBROUTINE IRDRCA (IFI,NBNO,DESC,NEC,DG,NCMPMX,VALE,NOMGD,
     +               NCMPGD,NOMSYM,NUMNOE,LRESU,NBCPUT,NCMPUT,
     +               NIVE )
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER        IFI, NBNO, DESC(*), NEC, DG(*), NCMPMX,
     +               NUMNOE(*), NBCPUT, NIVE
      REAL*8         VALE(*)
      CHARACTER*(*)  NOMGD, NCMPGD(*), NCMPUT(*)
      CHARACTER*(*)  NOMSYM
      LOGICAL        LRESU
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C--------------------------------------------------------------------
C        ECRITURE D'UN CHAM_NO SUR FICHIER CASTEM, DATASET TYPE 55
C        A VALEURS REELLES
C      ENTREE:
C         IFI   : UNITE LOGIQUE DU FICHIER CASTEM
C         NBNO  : NOMBRE DE NOEUDS DU MAILLAGE
C         DESC  :
C         NEC   : NOMBRE D'ENTIERS-CODES
C         DG    : ENTIERS CODES
C         NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR NOMGD
C         VALE  : VALEURS DU CHAM_NO
C         NOMGD : NOM DE LA GRANDEUR  DEPL_R, TEMP_R, SIEF_R, EPSI_R,...
C         NCMPGD: NOMS DES CMP
C         NOMSYM: NOM SYMBOLIQUE
C         NUMNOE: NUMERO DES NOEUDS
C         LRESU : =.TRUE. IMPRESSION D'UN CONCEPT RESULTAT
C         NBCPUT: NOMBRE DE CMP DEMANDE PAR L'UTILISATEUR
C         NCMPUT: NOMS DES CMP DEMANDE PAR L'UTILISATEUR
C         NIVE  : NIVEAU IMPRESSION CASTEM 3 OU 10
C
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8  ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*24 VALK(2)
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C     ------------------------------------------------------------------
      CHARACTER*8   NOMVAR(30)
      CHARACTER*8   BLANC,NOMCO
      INTEGER       NBCMP,IUTIL
      LOGICAL       EXISDG
C
C  --- INITIALISATIONS ----
C
      CALL JEMARQ()
      BLANC  = '      '
      CALL WKVECT('&&IRDRCA.POS','V V I',NCMPMX,IPOS)
      IF(NEC.GT.0) THEN
        DO 16 IEC=1,NEC
          DG(IEC)=DESC(3+IEC-1)
 16     CONTINUE
      ENDIF
C
      IF (.NOT.LRESU) THEN
         CALL JEVEUO ( '&&OP0039.LAST', 'E', JLAST )
         INUM = ZI(JLAST-1+4) + 1
      ELSE
         INUM = 0
      ENDIF
C
      NCMP = -DESC(2)
      IF ( NBCPUT .NE. 0 ) THEN
        DO 30 ICM = 1,NBCPUT
          DO 32 ICMP = 1,NCMPMX
             IF ( NCMPUT(ICM) .EQ. NCMPGD(ICMP) ) THEN
                ZI(IPOS-1+ICMP) = ICMP
                GO TO 30
             ENDIF
  32      CONTINUE
          VALK (1) = NCMPUT(ICM)
          VALK (2) = NOMGD
          CALL U2MESG('A', 'PREPOST5_25',2,VALK,0,0,0,0.D0)
 30     CONTINUE
      ELSE
        DO 2 ICMP = 1,NCMPMX
          IF ( EXISDG(DG,ICMP) ) ZI(IPOS-1+ICMP) = ICMP
 2     CONTINUE
      ENDIF
C
      IAD = 1
      NBCMP = 0
      DO 5 I = 1,NCMPMX
        IF ( ZI(IPOS-1+I) .NE. 0 ) THEN
          IF(NCMPGD(I).EQ.'DX') THEN
           NBCMP = NBCMP+1
           NOMVAR(IAD)='UX'
           IAD = IAD+1
          ELSEIF(NCMPGD(I).EQ.'DY') THEN
           NBCMP= NBCMP+1
           NOMVAR(IAD)='UY'
           IAD = IAD+1
          ELSEIF(NCMPGD(I).EQ.'DZ') THEN
           NBCMP= NBCMP+1
           NOMVAR(IAD)='UZ'
           IAD = IAD+1
          ELSEIF(NCMPGD(I).EQ.'DRX') THEN
           NBCMP= NBCMP+1
           NOMVAR(IAD)='RX'
           IAD = IAD+1
          ELSEIF(NCMPGD(I).EQ.'DRY') THEN
           NBCMP= NBCMP+1
           NOMVAR(IAD)='RY'
           IAD = IAD+1
          ELSEIF(NCMPGD(I).EQ.'DRZ') THEN
           NBCMP= NBCMP+1
           NOMVAR(IAD)='RZ'
           IAD = IAD+1
          ELSE
           NBCMP= NBCMP+1
           NOMCO = NCMPGD(I)
           IUTIL = LXLGUT (NOMCO)
           IF(IUTIL.LE.4) THEN
             NOMVAR(IAD) = NCMPGD(I)
           ELSE
             NOMVAR(IAD) = NOMCO (1:2)//
     +                       NOMCO((IUTIL-1):IUTIL)
           ENDIF
           IAD = IAD+1
          ENDIF
        ENDIF
  5   CONTINUE
C
C ---- ECRITURE DE L'EN-TETE -----
C
      IBID  = 2
      IUN   = 1
      IZERO = 0
      WRITE (IFI,'(A,I4)')   ' ENREGISTREMENT DE TYPE',IBID
      IF ( LRESU ) THEN
       IF(NIVE.EQ.3) THEN
        WRITE (IFI,'(A,I4,A,I5,A,I5)')  ' PILE NUMERO',IBID,
     +   'NBRE OBJETS NOMMES',IZERO,'NBRE OBJETS',IUN
       ELSEIF(NIVE.EQ.10) THEN
        WRITE (IFI,'(A,I4,A,I8,A,I8)')  ' PILE NUMERO',IBID,
     +   'NBRE OBJETS NOMMES',IZERO,'NBRE OBJETS',IUN
       ENDIF
      ELSE
       IF(NIVE.EQ.3) THEN
        WRITE (IFI,'(A,I4,A,I5,A,I5)')  ' PILE NUMERO',IBID,
     +   'NBRE OBJETS NOMMES',IUN,'NBRE OBJETS',IUN
C ECRITURE DES OBJETS NOMMES
        WRITE(IFI,'(1X,A8)') NOMSYM
        WRITE(IFI,'(I5)') INUM
       ELSE IF (NIVE.EQ.10) THEN
        WRITE (IFI,'(A,I4,A,I8,A,I8)')  ' PILE NUMERO',IBID,
     +   'NBRE OBJETS NOMMES',IUN,'NBRE OBJETS',IUN
C ECRITURE DES OBJETS NOMMES
        WRITE(IFI,'(1X,A8)') NOMSYM
        WRITE(IFI,'(I8)') INUM
       ENDIF
      ENDIF
C
      IF (NIVE.EQ.3) THEN
        WRITE(IFI,'(I5,I5,I5)') IUN,NBCMP,IBID
        WRITE(IFI,'(16(I5))') IUN,NBNO,NBCMP
      ELSE IF (NIVE.EQ.10) THEN
        WRITE(IFI,'(I8,I8,I8,I8)') IUN,NBCMP,IBID,IUN
        WRITE(IFI,'(10(I8))') IUN,NBNO,NBCMP
      ENDIF
C
      CALL WKVECT('&&IRDRCA.BID','V V I' ,NCMPMX,IBI )
      CALL WKVECT('&&IRDRCA.NOM','V V K8',NBCMP ,INOM)
C
      IZERO = 0
      DO 10 I=1,NBCMP
       ZI(IBI-1+I) = IZERO
       ZK8(INOM-1+I) = NOMVAR(I)
 10   CONTINUE
      WRITE(IFI,'(16(1X,A4))') (ZK8(INOM-1+I)(1:4),I=1,NBCMP)
      IF(NIVE.EQ.3) WRITE(IFI,'(16(I5))') (ZI(IBI-1+I),I=1,NBCMP)
      IF(NIVE.EQ.10) WRITE(IFI,'(10(I8))') (ZI(IBI-1+I),I=1,NBCMP)
      IF (LRESU) THEN
        WRITE(IFI,'(1X,A71)') NOMSYM
      ELSE
        WRITE(IFI,'(1X,A71)') NOMGD
      ENDIF
      WRITE(IFI,'(1X,A)') BLANC
      IF (NIVE.EQ.10)  WRITE (IFI,'(I8)') IZERO

C --- ALLOCATION DES TABLEAUX DE TRAVAIL ---
C
      CALL WKVECT('&&IRDRCA.VALE','V V R',NCMPMX*NBNO,IRVAL)
C
C ---- BOUCLE SUR LES DIVERSES GRANDEURS CASTEM ----
      DO 22 IVA = 1,NCMP
        IC = ZI(IPOS-1+IVA)
        IADR = IRVAL-1+(IVA-1)*NBNO
        DO 12 INNO = 1,NBNO
          INO  = NUMNOE(INNO)
          IVAL = (INO-1)*NCMP
          ZR(IADR+INNO) = VALE(IVAL+IC)
 12     CONTINUE
 22   CONTINUE
      WRITE(IFI,'(3(1X,E21.13E3))') (ZR(IRVAL-1+I),I=1,NBCMP*NBNO)
      IF(.NOT.LRESU) ZI(JLAST-1+4)=INUM
      CALL JEDETR('&&IRDRCA.VALE')
      CALL JEDETR('&&IRDRCA.BID')
      CALL JEDETR('&&IRDRCA.NOM')
      CALL JEDETR('&&IRDRCA.POS')
      CALL JEDEMA()
      END
