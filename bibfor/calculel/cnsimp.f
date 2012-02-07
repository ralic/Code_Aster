      SUBROUTINE CNSIMP(CNSZ,UNITE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 07/02/2012   AUTEUR PELLET J.PELLET 
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
C RESPONSABLE VABHHTS J.PELLET
      IMPLICIT NONE
      CHARACTER*(*) CNSZ
      INTEGER UNITE
C ---------------------------------------------------------------------
C BUT: IMPRIMER UN CHAM_NO_S
C ---------------------------------------------------------------------
C     ARGUMENTS:
C CNSZ   IN/JXIN  K19 : SD CHAM_NO_S A IMPRIMER
C UNITE  IN       I   : NUMERO DE L'UNITE LOGIQUE D'IMPRESSION

C REMARQUE :
C  - POUR L'INSTANT ON IMPRIME AU FORMAT "RESULTAT" LES CHAMPS DE R8
C-----------------------------------------------------------------------

C---- COMMUNS NORMALISES  JEVEUX
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32,JEXNUM
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ------------------------------------------------------------------
      INTEGER JCNSK,JCNSD,JCNSV,JCNSL,JCNSC
      INTEGER NBNO,IBID,K,INO,NCMP,NCMPU,JLVAL,IK,LICMPU(997)
      CHARACTER*8 MA,NOMGD,NOMNO
      CHARACTER*3 TSCA
      CHARACTER*19 CNS
      CHARACTER*40 FMT1,FMT2
      LOGICAL EXICMP
C     ------------------------------------------------------------------
      CALL JEMARQ()


      CNS = CNSZ

      CALL JEVEUO(CNS//'.CNSK','L',JCNSK)
      CALL JEVEUO(CNS//'.CNSD','L',JCNSD)
      CALL JEVEUO(CNS//'.CNSC','L',JCNSC)
      CALL JEVEUO(CNS//'.CNSV','L',JCNSV)
      CALL JEVEUO(CNS//'.CNSL','L',JCNSL)

      MA = ZK8(JCNSK-1+1)
      NOMGD = ZK8(JCNSK-1+2)
      NBNO = ZI(JCNSD-1+1)
      NCMP = ZI(JCNSD-1+2)


C     1- CALCUL DE NCMPU  : NB CMPS UTILISEES DANS LE CHAMP
C            ET DE LICMPU : NUMEROS DES CMPS UTILISEES
C     ------------------------------------------------------------
      NCMPU = 0
      DO 30,K = 1,NCMP
        DO 10,INO = 1,NBNO
          IF (ZL(JCNSL-1+ (INO-1)*NCMP+K)) GO TO 20
   10   CONTINUE
        GO TO 30
   20   CONTINUE
        NCMPU = NCMPU + 1
        CALL ASSERT(NCMPU.LE.997)
        LICMPU(NCMPU) = K
   30 CONTINUE

C     -- LE CHAMP EST VIDE : ON SORT
      IF (NCMPU.EQ.0) GOTO 9999

      CALL DISMOI('F','TYPE_SCA',NOMGD,'GRANDEUR',IBID,TSCA,IBID)
      CALL ASSERT((TSCA.EQ.'R').OR.(TSCA.EQ.'K8')
     &            .OR.(TSCA.EQ.'I').OR.(TSCA.EQ.'C'))


C     1- ALLOCATION D'UN TABLEAU DE K16 QUI CONTIENDRA LES VALEURS
C         D'UNE LIGNE A ECRIRE
C     ------------------------------------------------------------
      IF (TSCA.NE.'C') THEN
        CALL WKVECT('&&CNSIMP.LVALEURS','V V K16',NCMPU,JLVAL)
      ELSE
        CALL WKVECT('&&CNSIMP.LVALEURS','V V K16',2*NCMPU,JLVAL)
      ENDIF


C     2- FORMAT DES LIGNES :
C     ----------------------
      IF (TSCA.NE.'C') THEN
        FMT1 = '(A12,XXX(''|'',A12))'
        FMT2 = '(A12,XXX(''|'',A12))'
      ELSE
        FMT1 = '(A12,XXX(''|'',A25))'
        FMT2 = '(A12,XXX(''|'',A12,'' '',A12))'
      ENDIF
      CALL CODENT(NCMPU,'D',FMT1(6:8))
      CALL CODENT(NCMPU,'D',FMT2(6:8))


C     3- ECRITURE DE L'ENTETE DU CHAMP :
C     ---------------------------------------
      WRITE (UNITE,*) ' '
      WRITE (UNITE,*) ' GRANDEUR: ',NOMGD
      WRITE (UNITE,*) ' '
      WRITE (UNITE,FMT1) 'NOEUD', (ZK8(JCNSC-1+LICMPU(IK)),IK=1,NCMPU)


C     4- ECRITURE DES VALEURS :
C     ---------------------------------------
      DO 70,INO = 1,NBNO
        CALL JENUNO(JEXNUM(MA//'.NOMNOE',INO),NOMNO)

C       -- ON N'ECRIT UN NOEUD QUE S'IL EXISTE AU MOINS 1 CMP :
        EXICMP = .FALSE.
        DO 40,IK = 1,NCMPU
          K = LICMPU(IK)
          IF (ZL(JCNSL-1+ (INO-1)*NCMP+K)) THEN
            EXICMP = .TRUE.
            GO TO 50
          END IF
   40   CONTINUE
   50   CONTINUE
        IF (.NOT.EXICMP) GO TO 70



C       -- ON MET LES VALEURS NON AFFECTEES A " " :
        DO 60,IK = 1,NCMPU
          K = LICMPU(IK)
          IF (ZL(JCNSL-1+ (INO-1)*NCMP+K)) THEN
            IF (TSCA.EQ.'R') THEN
              WRITE (ZK16(JLVAL-1+IK),'(E12.5,A4)') ZR(JCNSV-1+
     &          (INO-1)*NCMP+K),' '
            ELSE IF (TSCA.EQ.'K8') THEN
              WRITE (ZK16(JLVAL-1+IK),'(A8,A8)') ZK8(JCNSV-1+
     &          (INO-1)*NCMP+K),' '
            ELSE IF (TSCA.EQ.'C') THEN
              WRITE (ZK16(JLVAL-1+2*(IK-1)+1),'(E12.5,A4)')
     &          DBLE(ZC(JCNSV-1+(INO-1)*NCMP+K)),' '
              WRITE (ZK16(JLVAL-1+2*(IK-1)+2),'(E12.5,A4)')
     &          DIMAG(ZC(JCNSV-1+(INO-1)*NCMP+K)),' '
            ELSE IF (TSCA.EQ.'I') THEN
              WRITE (ZK16(JLVAL-1+IK),'(I12,A4)') ZI(JCNSV-1+
     &          (INO-1)*NCMP+K),' '
            END IF
          ELSE
            IF(TSCA.NE.'C') THEN
              WRITE (ZK16(JLVAL-1+IK),'(A16)') ' '
            ELSE
              WRITE (ZK16(JLVAL-1+2*(IK-1)+1),'(A16)') ' '
              WRITE (ZK16(JLVAL-1+2*(IK-1)+2),'(A16)') ' '
            ENDIF
          END IF
   60   CONTINUE
        IF(TSCA.NE.'C') THEN
          WRITE (UNITE,FMT2) NOMNO,
     &            (ZK16(JLVAL-1+IK),IK=1,NCMPU)
        ELSE
          WRITE (UNITE,FMT2) NOMNO,
     &            (ZK16(JLVAL-1+2*(IK-1)+1),
     &             ZK16(JLVAL-1+2*(IK-1)+2),IK=1,NCMPU)
        ENDIF

   70 CONTINUE

 9999 CONTINUE

      CALL JEDETR('&&CNSIMP.LVALEURS')
      CALL JEDEMA()
      END
