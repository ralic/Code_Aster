      SUBROUTINE RCMATN ( CHMAT, NOMAIL)
      IMPLICIT   NONE
      CHARACTER*8         CHMAT, NOMAIL
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 29/10/2007   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C BUT : CREER LE CHAM_NO DE MATERIAU (AFFE_MATERIAU/AFFE_NOEUD)
C ======================================================================
C
C  IN : CHMAT  : CHAMP MATERIAU PRODUIT
C  IN : NOMAIL : NOM DU MAILLAGE
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C ----------------------------------------------------------------------
C
      INTEGER  NOCC, I, NM, NT, NBNO,JNOEU,JCNSL,JCNSV,JCNSD
      INTEGER INO, NBNOT,KNO,IBID
      CHARACTER*4   KBID
      CHARACTER*8   NOMMAT, TYPMCL(2)
      CHARACTER*16  MOTCLE(2)
      CHARACTER*19  CNS
      CHARACTER*24  MESNOE
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL GETFAC ( 'AFFE_NOEUD' , NOCC )
      IF (NOCC.EQ.0) GOTO 9999

      CNS='&&RCMATN.CNS'
      CALL CNSCRE(NOMAIL,'NEUT_F',1,'X1','V',CNS)
      CALL JEVEUO(CNS//'.CNSD','L',JCNSD)
      CALL JEVEUO(CNS//'.CNSV','E',JCNSV)
      CALL JEVEUO(CNS//'.CNSL','E',JCNSL)
      NBNOT = ZI(JCNSD-1+1)

      MOTCLE(1) = 'GROUP_MA'
      MOTCLE(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
C
      MESNOE = '&&RCMATN.MES_NOEUDS'
C
      DO 10 I = 1 , NOCC
         CALL GETVID ( 'AFFE_NOEUD', 'MATER' , I,1,1,  NOMMAT, NM )
         CALL ASSERT(NM.EQ.1)
         CALL GETVTX ( 'AFFE_NOEUD', 'TOUT'  , I,1,1, KBID, NT )
         IF ( NT .NE. 0 ) THEN
            DO 11, INO=1,NBNOT
              ZL(JCNSL-1+ (INO-1)*1+1) = .TRUE.
              ZK8(JCNSV-1+ (INO-1)*1+1) = NOMMAT
 11         CONTINUE
         ELSE
            CALL RELIEM(' ',NOMAIL,'NU_NOEUD','AFFE_NOEUD',I,2,MOTCLE,
     &                                      TYPMCL, MESNOE, NBNO )
            IF ( NBNO .NE. 0 ) THEN
              CALL JEVEUO ( MESNOE, 'L', JNOEU )
              DO 12, KNO=1,NBNO
                INO= ZI(JNOEU-1+KNO)
                ZL(JCNSL-1+ (INO-1)*1+1) = .TRUE.
                ZK8(JCNSV-1+ (INO-1)*1+1) = NOMMAT
 12           CONTINUE
            ENDIF
         ENDIF
 10   CONTINUE

      CALL JEDETR (MESNOE)
      CALL CNSCNO(CNS,' ',' ','G',CHMAT//'.CHAMP_MATN','F',IBID)
      CALL DETRSD('CHAM_NO_S',CNS)

9999  CONTINUE
      CALL JEDEMA()
      END
