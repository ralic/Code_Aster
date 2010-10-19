      SUBROUTINE CHCSUR ( CHCINE, CNSZ, TYPE, MO, NOMGD )
      IMPLICIT NONE
      CHARACTER*1         TYPE
      CHARACTER*8         NOMGD
      CHARACTER*(*)       CHCINE, CNSZ, MO
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 19/10/2010   AUTEUR DELMAS J.DELMAS 
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
C-----------------------------------------------------------------------
C OBJET : CREATION D"UNE CHARGE CINEMATIQUE.
C        1) LE .REFE DE LA CHARGE DOIT DEJA EXISTER
C        2) MISE A JOUR DE : .AFCI ET .AFCV
C-----------------------------------------------------------------------
C OUT  CHCINE  K*19    : NOM DE LA CHARGE CINEMATIQUE
C IN   CNS     K*19    : NOM D'UN CHAM_NO_S CONTENANT LES DEGRES IMPOSES
C IN   TYPE    K*1     : 'R','C' OU 'F' TYPE DE LA CHARGE
C IN   MO      K*      : NOM DU MODELE
C IN   NOMGD   K*      : NOM DE LA GRANDEUR
C-----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
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
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER       NBLOC, IBLOC, ICMP, NCMP, INO, NBNO, NBEC, IER, II,
     +              JCNSD, JCNSV, JCNSL, JAFCI, JAFCV, IAPRNM
      LOGICAL       EXISDG
      CHARACTER*8   K8B, NOMO
      CHARACTER*19  CHCI, CNS
      CHARACTER*24  CAFCI, CAFCV
C
      DATA CAFCI /'                   .AFCI'/
      DATA CAFCV /'                   .AFCV'/
C
C --- DEBUT -----------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMO = MO
      CALL DISMOI ( 'F', 'NB_EC', NOMGD, 'GRANDEUR', NBEC, K8B, IER )
      CALL JEVEUO ( NOMO//'.MODELE    .PRNM', 'L', IAPRNM )
C
      CHCI = CHCINE
      CNS  = CNSZ
      CAFCI(1:19) = CHCI
      CAFCV(1:19) = CHCI
C
      CALL JEVEUO ( CNS//'.CNSD', 'L', JCNSD )
      CALL JEVEUO ( CNS//'.CNSV', 'L', JCNSV )
      CALL JEVEUO ( CNS//'.CNSL', 'L', JCNSL )
C
      NBNO = ZI(JCNSD)
      NCMP = ZI(JCNSD+1)
C
      NBLOC = 0
      DO 100 ICMP = 1, NCMP
         DO 110 INO = 1 , NBNO
            IF ( ZL(JCNSL+(INO-1)*NCMP+ICMP-1) ) NBLOC = NBLOC + 1
 110     CONTINUE
 100  CONTINUE
C
C --- CREATION DE LA SD
C
      CALL WKVECT ( CAFCI, 'G V I', (3*NBLOC+1), JAFCI )
      IF (TYPE.EQ.'R') THEN
         CALL WKVECT ( CAFCV, 'G V R' , MAX(NBLOC,1), JAFCV )
      ELSE IF (TYPE.EQ.'C') THEN
         CALL WKVECT ( CAFCV, 'G V C' , MAX(NBLOC,1), JAFCV )
      ELSE IF (TYPE.EQ.'F') THEN
         CALL WKVECT ( CAFCV, 'G V K8', MAX(NBLOC,1), JAFCV )
      ENDIF
C
C --- ON REMPLIT LES .AFCI .AFCV
C
C
      IBLOC = 0
      IF (TYPE.EQ.'R') THEN
         DO 120 INO = 1, NBNO
            II = 0
            DO 122 ICMP = 1, NCMP
               IF (EXISDG(ZI(IAPRNM-1+NBEC*(INO-1)+1),ICMP)) THEN
                  II = II + 1
                  IF ( ZL(JCNSL+(INO-1)*NCMP+ICMP-1) ) THEN
                     IBLOC = IBLOC + 1
                     ZI(JAFCI+3*(IBLOC-1)+1) = INO
                     ZI(JAFCI+3*(IBLOC-1)+2) = II
                     ZR(JAFCV-1+IBLOC) = ZR(JCNSV+(INO-1)*NCMP+ICMP-1)
                  ENDIF
               ENDIF
 122        CONTINUE
 120     CONTINUE
C
      ELSE IF (TYPE.EQ.'C') THEN
         DO 130 INO = 1, NBNO
            II = 0
            DO 132 ICMP = 1, NCMP
               IF (EXISDG(ZI(IAPRNM-1+NBEC*(INO-1)+1),ICMP)) THEN
                  II = II + 1
                  IF ( ZL(JCNSL+(INO-1)*NCMP+ICMP-1) ) THEN
                     IBLOC = IBLOC + 1
                     ZI(JAFCI+3*(IBLOC-1)+1) = INO
                     ZI(JAFCI+3*(IBLOC-1)+2) = II
                     ZC(JAFCV-1+IBLOC) = ZC(JCNSV+(INO-1)*NCMP+ICMP-1)
                  ENDIF
               ENDIF
 132        CONTINUE
 130     CONTINUE
C
      ELSE IF (TYPE.EQ.'F') THEN
         DO 140 INO = 1, NBNO
            II = 0
            DO 142 ICMP = 1, NCMP
               IF (EXISDG(ZI(IAPRNM-1+NBEC*(INO-1)+1),ICMP)) THEN
                  II = II + 1
                  IF ( ZL(JCNSL+(INO-1)*NCMP+ICMP-1) ) THEN
                     IBLOC = IBLOC + 1
                     ZI(JAFCI+3*(IBLOC-1)+1) = INO
                     ZI(JAFCI+3*(IBLOC-1)+2) = II
                     ZK8(JAFCV-1+IBLOC) = ZK8(JCNSV+(INO-1)*NCMP+ICMP-1)
                  ENDIF
               ENDIF
 142        CONTINUE
 140     CONTINUE
      ENDIF

      IF (IBLOC.EQ.0) CALL U2MESS('A','CALCULEL_9')
      ZI(JAFCI) = IBLOC
C
      CALL JEDEMA()
      END
