      SUBROUTINE EDCHNO(CHAMNO,IFIC)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) CHAMNO
      INTEGER IFIC
C     ------------------------------------------------------------------
C        IMPRESSION D'UN CHAM_NO A COMPOSANTES REELLES OU COMPLEXES
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C     ENTREES:
C        CHAMNO : NOM DU CHAM_NO A ECRIRE PROPREMENT SUR IFIC
C        IFIC   : NUMERO LOGIQUE DU FICHIER DE SORTIE
C     SORTIES:
C
C     ------------------------------------------------------------------
C
C     FONCTIONS EXTERNES:
C     -------------------
      LOGICAL EXISDG
      INTEGER NBEC
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C
C     VARIABLES LOCALES:
C     ------------------
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*1 TYPE
      INTEGER ZI,GD,IADG
      REAL*8 ZR,TAMPON(18,2)
      COMPLEX*16 ZC
      LOGICAL ZL,LMAILA
      CHARACTER*8 ZK8,NOMMA,NOMGD,NOMNO,NOMCMP(18),BLANC
      CHARACTER*16 ZK16
      CHARACTER*19 CHAMN2
      CHARACTER*24 ZK24,NOMNU,NOLILI
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      CHARACTER*1 K1BID
C
      CALL JEMARQ()
      BLANC = '  '
C
      CHAMN2 = CHAMNO
C
      CALL JEVEUO(CHAMN2//'.REFE','L',IAREFE)
      NOMMA = ZK24(IAREFE-1+1) (1:8)
      NOMNU = ZK24(IAREFE-1+2)
C
      CALL JELIRA(CHAMN2//'.VALE','TYPE',IBID,TYPE)
      IF (TYPE(1:1).EQ.'R') THEN
          ITYPE = 1
      ELSE IF (TYPE(1:1).EQ.'C') THEN
          ITYPE = 2
      ELSE
          WRITE (IFIC,*) 'ERREUR EDCHNO '
          WRITE (IFIC,*) 'ON NE SAIT PAS IMPRIMER LES CHAMPS DE TYPE "',
     &      TYPE(1:1),'"   ON EST VRAIMENT DESOLE.'
          GO TO 9999
      END IF
      CALL JEVEUO(CHAMN2//'.VALE','L',IAVALE)
C
      CALL JEVEUO(CHAMN2//'.DESC','L',IADESC)
      GD = ZI(IADESC-1+1)
      NUM = ZI(IADESC-1+2)
      CALL JENUNO(JEXNUM('&CATA.GD.NOMGD',GD),NOMGD)
      NEC = NBEC(GD)
      CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',GD),'LONMAX',NCMPMX,K1BID)
      IF (NCMPMX.GT.18) THEN
CC       CALL UTMESS('F',' EDCHNO ','1')
          NCMPMX = 18
      END IF
C
C     -- ON STOCKE LE NOM DES CMPS EVENTUELLEMENT PRESENTES:
      CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',GD),'L',IAD)
      DO 1,I = 1,NCMPMX
          NOMCMP(I) = ZK8(IAD-1+I)
    1 CONTINUE
C
C     --SI LE CHAMP EST A REPRESENTATION CONSTANTE:
C
      IF (NUM.LT.0) THEN
C         A FAIRE ???
          CALL U2MESS('F','CALCULEL2_44')
      ELSE
C        --SI LE CHAMP EST DECRIT PAR 1 "PRNO":
C
          WRITE (IFIC,*) '---- ECRITURE DU CHAMP AUX NOEUDS: ',
     &      CHAMNO(1:19)
          CALL JEVEUO(NOMNU(1:19)//'.NUEQ','L',IANUEQ)
          CALL JELIRA(NOMNU(1:19)//'.PRNO','NMAXOC',NLILI,K1BID)
          DO 10,I = 1,NLILI
              CALL JENUNO(JEXNUM(NOMNU(1:19)//'.LILI',I),NOLILI)
              CALL JELIRA(JEXNUM(NOMNU(1:19)//'.PRNO',I),'LONMAX',IBID,
     &                    K1BID)
              IF (IBID.EQ.0) GO TO 10
              CALL JEVEUO(JEXNUM(NOMNU(1:19)//'.PRNO',I),'L',IAPRNO)
              IF (I.EQ.1) THEN
                  WRITE (IFIC,*) '----  NOEUDS DU MAILLAGE:'
              ELSE
                  WRITE (IFIC,*) '----  NOEUDS SUPPLEMENTAIRES DE:',
     &              NOLILI(1:19)
              END IF
              IF (NCMPMX.LE.9) THEN
                  WRITE (IFIC,8001) ' NOEUD ', (NOMCMP(II),II=1,NCMPMX)
              ELSE IF (NCMPMX.LE.18) THEN
                  WRITE (IFIC,8001) ' NOEUD ', (NOMCMP(II),II=1,9)
                  WRITE (IFIC,8001) '       ', (NOMCMP(II),II=10,NCMPMX)
              END IF
C
C           --RECHERCHE DU NOMBRE DE NOEUDS : NBNO
              IF (NOLILI(1:19).EQ.'&MAILLA            ') THEN
                  CALL JELIRA(NOMMA//'.NOMNOE','NOMMAX',NBNO,K1BID)
                  LMAILA = .TRUE.
              ELSE
                  CALL JEVEUO(NOLILI(1:19)//'.NBNO','L',IALILI)
                  NBNO = ZI(IALILI-1+1)
                  LMAILA = .FALSE.
              END IF
              DO 11,INO = 1,NBNO
C
C              --RECHERCHE DU NOM DU NOEUD:
                  IF (LMAILA) THEN
                      CALL JENUNO(JEXNUM(NOMMA//'.NOMNOE',INO),NOMNO)
                  ELSE
                      CALL CODENT(INO,'D',NOMNO)
                  END IF
C
C              NCMP : NOMBRE DE CMPS SUR LE NOEUD INO
C              IVAL : ADRESSE DU DEBUT DU NOEUD INO DANS .NUEQ
C              IADG : DEBUT DU DESCRIPTEUR GRANDEUR DU NOEUD INO
                  IVAL = ZI(IAPRNO-1+ (INO-1)* (NEC+2)+1)
                  NCMP = ZI(IAPRNO-1+ (INO-1)* (NEC+2)+2)
                  IADG = IAPRNO - 1 + (INO-1)* (NEC+2) + 3
                  IF (NCMP.EQ.0) GO TO 11
C
                  ICOMPT = 0
                  DO 12,ICMP = 1,NCMPMX
                      IF (EXISDG(ZI(IADG),ICMP)) THEN
                          ICOMPT = ICOMPT + 1
                          IEQ = ZI(IANUEQ-1+IVAL-1+ICOMPT)
                          IF (ITYPE.EQ.1) THEN
                              TAMPON(ICMP,1) = ZR(IAVALE-1+IEQ)
                          ELSE IF (ITYPE.EQ.2) THEN
                              TAMPON(ICMP,1) = DBLE(ZC(IAVALE-1+IEQ))
                              TAMPON(ICMP,2) = DIMAG(ZC(IAVALE-1+IEQ))
                          END IF
                      ELSE
                          TAMPON(ICMP,1) = 0.0D0
                          TAMPON(ICMP,2) = 0.0D0
                      END IF
   12             CONTINUE
                  IF (NCMPMX.LE.9) THEN
                      IF (ITYPE.EQ.1) THEN
                          WRITE (IFIC,8000) NOMNO,
     &                      (TAMPON(II,1),II=1,NCMPMX)
                      ELSE IF (ITYPE.EQ.2) THEN
                          WRITE (IFIC,8000) NOMNO,
     &                      (TAMPON(II,1),II=1,NCMPMX)
                          WRITE (IFIC,8000) BLANC,
     &                      (TAMPON(II,2),II=1,NCMPMX)
                      END IF
                  ELSE IF (NCMPMX.LE.18) THEN
                      IF (ITYPE.EQ.1) THEN
                          WRITE (IFIC,8000) NOMNO, (TAMPON(II,1),II=1,9)
                          WRITE (IFIC,8000) BLANC,
     &                      (TAMPON(II,1),II=10,NCMPMX)
                      ELSE IF (ITYPE.EQ.2) THEN
                          WRITE (IFIC,8000) NOMNO, (TAMPON(II,1),II=1,9)
                          WRITE (IFIC,8000) BLANC,
     &                      (TAMPON(II,1),II=10,NCMPMX)
                          WRITE (IFIC,8000) BLANC, (TAMPON(II,2),II=1,9)
                          WRITE (IFIC,8000) BLANC,
     &                      (TAMPON(II,2),II=10,NCMPMX)
                      END IF
                  END IF
   11         CONTINUE
   10     CONTINUE
      END IF
 9999 CONTINUE
 8000 FORMAT (1X,A8,9 (1X,1PD10.3))
 8001 FORMAT (1X,A8,9 (1X,A10))
      CALL JEDEMA()
      END
