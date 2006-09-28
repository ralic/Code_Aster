      SUBROUTINE CNTRAN ( LINOEU, NBNO, CHS1, CHS2 )
      IMPLICIT   NONE
      INTEGER             LINOEU(*), NBNO
      CHARACTER*(*)       CHS1, CHS2
C ----------------------------------------------------------------------
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
C
C     COMMANDE:  CREA_RESU
C     TRAITEMENT DU MOT CLE FACTEUR "PERM_CHAMP"
C
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER        IBID, INDIK8, NCMP1, NCMP2, INO1, INO2,
     &               JCN1K, JCN1D, JCN1C, JCN1V, JCN1L, ICMP1,
     &               JCN2K, JCN2D, JCN2C, JCN2V, JCN2L, ICMP2
      CHARACTER*3    TSCA
      CHARACTER*8    NOMGD, NOMGD2, NOCMP
      CHARACTER*19   CNS1, CNS2
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      CNS1 = CHS1
      CNS2 = CHS2
C
      CALL JEVEUO ( CNS1//'.CNSK', 'L', JCN1K )
      CALL JEVEUO ( CNS1//'.CNSD', 'L', JCN1D )
      CALL JEVEUO ( CNS1//'.CNSC', 'L', JCN1C )
      CALL JEVEUO ( CNS1//'.CNSV', 'L', JCN1V )
      CALL JEVEUO ( CNS1//'.CNSL', 'E', JCN1L )
C
      CALL JEVEUO ( CNS2//'.CNSK', 'L', JCN2K )
      CALL JEVEUO ( CNS2//'.CNSD', 'L', JCN2D )
      CALL JEVEUO ( CNS2//'.CNSC', 'L', JCN2C )
      CALL JEVEUO ( CNS2//'.CNSV', 'E', JCN2V )
      CALL JEVEUO ( CNS2//'.CNSL', 'E', JCN2L )
C
      NOMGD = ZK8(JCN1K-1+2)
      NCMP1 =  ZI(JCN1D-1+2)
C
      NOMGD2 = ZK8(JCN2K-1+2)
      NCMP2  =  ZI(JCN2D-1+2)
C
      IF (NOMGD2.NE.NOMGD) CALL U2MESS('F','CALCULEL_84')
C
      CALL DISMOI ( 'F', 'TYPE_SCA', NOMGD, 'GRANDEUR', IBID,TSCA,IBID)
C
      DO 10 INO2 = 1 , NBNO
C
         INO1 = LINOEU(INO2)
         IF ( INO1 .EQ. 0 ) GOTO 10
C
         DO 20 ICMP2 = 1,NCMP2
C
            NOCMP = ZK8(JCN2C-1+ICMP2)
C
            ICMP1 = INDIK8( ZK8(JCN1C), NOCMP, 1, NCMP1 )
            IF ( ICMP1 .EQ. 0 ) GOTO 20
            IF ( .NOT. ZL(JCN1L-1+(INO1-1)*NCMP1+ICMP1) ) GOTO 20
C
            ZL(JCN2L-1+(INO2-1)*NCMP2+ICMP2) = .TRUE.
C
            IF (TSCA.EQ.'R') THEN
               ZR(JCN2V-1+(INO2-1)*NCMP2+ICMP2) =
     &                                 ZR(JCN1V-1+(INO1-1)*NCMP1+ICMP1)
            ELSE IF (TSCA.EQ.'C') THEN
               ZC(JCN2V-1+(INO2-1)*NCMP2+ICMP2) =
     &                                 ZC(JCN1V-1+(INO1-1)*NCMP1+ICMP1)
            ELSE IF (TSCA.EQ.'I') THEN
               ZI(JCN2V-1+(INO2-1)*NCMP2+ICMP2) =
     &                                 ZI(JCN1V-1+(INO1-1)*NCMP1+ICMP1)
            ELSE IF (TSCA.EQ.'L') THEN
               ZL(JCN2V-1+(INO2-1)*NCMP2+ICMP2) =
     &                                 ZL(JCN1V-1+(INO1-1)*NCMP1+ICMP1)
            ELSE IF (TSCA.EQ.'K8') THEN
               ZK8(JCN2V-1+(INO2-1)*NCMP2+ICMP2) =
     &                                ZK8(JCN1V-1+(INO1-1)*NCMP1+ICMP1)
            ELSE
               CALL U2MESS('F','CALCULEL_39')
            END IF
C
 20      CONTINUE
C
 10   CONTINUE
C
      CALL JEDEMA()
      END
