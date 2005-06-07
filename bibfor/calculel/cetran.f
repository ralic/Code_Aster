      SUBROUTINE CETRAN ( LIMA1, LIMA2, NBMA, CHS1, CHS2 )
      IMPLICIT   NONE
      INTEGER             LIMA1(*), LIMA2(*), NBMA
      CHARACTER*(*)       CHS1, CHS2
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
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
      INTEGER        IBID, INDIK8, NBPT, NBPT2, NBSP, NBSP2,
     +               NCMP1, NCMP2, IPT, ISP, IAD1, IAD2,
     +               JCE1K, JCE1D, JCE1C, JCE1V, JCE1L, ICMP1,
     +               JCE2K, JCE2D, JCE2C, JCE2V, JCE2L, ICMP2,
     +               IMA, IMA1, IMA2
      CHARACTER*3    TSCA
      CHARACTER*8    NOMGD, NOMGD2, NOCMP
      CHARACTER*19   CES1, CES2
C
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      CES1 = CHS1
      CES2 = CHS2
C
      CALL JEVEUO ( CES1//'.CESK', 'L', JCE1K )
      CALL JEVEUO ( CES1//'.CESD', 'L', JCE1D )
      CALL JEVEUO ( CES1//'.CESC', 'L', JCE1C )
      CALL JEVEUO ( CES1//'.CESV', 'L', JCE1V )
      CALL JEVEUO ( CES1//'.CESL', 'L', JCE1L )
C
      CALL JEVEUO ( CES2//'.CESK', 'L', JCE2K )
      CALL JEVEUO ( CES2//'.CESD', 'L', JCE2D )
      CALL JEVEUO ( CES2//'.CESC', 'L', JCE2C )
      CALL JEVEUO ( CES2//'.CESV', 'E', JCE2V )
      CALL JEVEUO ( CES2//'.CESL', 'E', JCE2L )
C
      NOMGD = ZK8(JCE1K-1+2)
      NCMP1 =  ZI(JCE1D-1+2)
C
      NOMGD2 = ZK8(JCE2K-1+2)
      NCMP2  =  ZI(JCE2D-1+2)
C
      IF (NOMGD2.NE.NOMGD) CALL UTMESS('F','CETRAN','STOP NOMGD')
C
      CALL DISMOI ( 'F', 'TYPE_SCA', NOMGD, 'GRANDEUR', IBID,TSCA,IBID)
C
      DO 10  IMA = 1 , NBMA
C
         IMA1 = LIMA1(IMA)
         IMA2 = LIMA2(IMA)
C
         NBPT = ZI(JCE1D-1+5+4*(IMA1-1)+1)
         NBSP = ZI(JCE1D-1+5+4*(IMA1-1)+2)
C
         NBPT2 = ZI(JCE2D-1+5+4*(IMA2-1)+1)
         NBSP2 = ZI(JCE2D-1+5+4*(IMA2-1)+2)
         IF (NBPT2.NE.NBPT) CALL UTMESS('F','CETRAN','STOP NBPT')
         IF (NBSP2.NE.NBSP) CALL UTMESS('F','CETRAN','STOP NBSP')
C
         DO 20 ICMP2 = 1 , NCMP2
C
            NOCMP = ZK8(JCE2C-1+ICMP2)
C
            ICMP1 = INDIK8( ZK8(JCE1C), NOCMP, 1, NCMP1 )
            IF ( ICMP1 .EQ. 0 ) GOTO 20
C
            DO 30 IPT = 1 , NBPT
C
               DO 40 ISP = 1 , NBSP
C
                  CALL CESEXI('C',JCE1D,JCE1L,IMA1,IPT,ISP,ICMP1,IAD1)
                  IF (IAD1.LE.0) GOTO 40
                  IF ( .NOT. ZL(JCE1L-1+IAD1) ) GOTO 40
                  CALL CESEXI('C',JCE2D,JCE2L,IMA2,IPT,ISP,ICMP2,IAD2)
                  IF (IAD2.LE.0) CALL UTMESS('F','CETRAN','STOP IAD2')
C
                  ZL(JCE2L-1+IAD2) = .TRUE.
C
                  IF (TSCA.EQ.'R') THEN
                     ZR(JCE2V-1+IAD2) = ZR(JCE1V-1+IAD1)
                  ELSE IF (TSCA.EQ.'C') THEN
                     ZC(JCE2V-1+IAD2) = ZC(JCE1V-1+IAD1)
                  ELSE IF (TSCA.EQ.'I') THEN
                     ZI(JCE2V-1+IAD2) = ZI(JCE1V-1+IAD1)
                  ELSE IF (TSCA.EQ.'L') THEN
                     ZL(JCE2V-1+IAD2) = ZL(JCE1V-1+IAD1)
                  ELSE IF (TSCA.EQ.'K8') THEN
                     ZK8(JCE2V-1+IAD2) = ZK8(JCE1V-1+IAD1)
                  ELSE
                     CALL UTMESS('F','CETRAN','TYPE SCALAIRE INCONNU')
                  END IF
C
 40            CONTINUE
C
 30         CONTINUE
C
 20      CONTINUE
C
 10   CONTINUE
C
      CALL JEDEMA()
      END
