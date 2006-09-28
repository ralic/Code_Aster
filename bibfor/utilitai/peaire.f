      SUBROUTINE PEAIRE(RESU,MODELE,NBOCC)
      IMPLICIT   NONE
      INTEGER           NBOCC
      CHARACTER*(*)     RESU, MODELE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C     OPERATEUR   POST_ELEM
C     TRAITEMENT DU MOT CLE-FACTEUR "AIRE_INTERNE"
C     ------------------------------------------------------------------
C
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNOM, JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      NBPARR, IBID, IRET, IOCC, NG,JNOMA,NGB,JGB,IGB,NBB
      INTEGER      IE, IFM ,NIV,JMA,NBMA,IADGMA,J,IMA
      PARAMETER    ( NBPARR = 3 )
      REAL*8        VALPAR(NBPARR), AIRE, LONG
      CHARACTER*3   TYPARR(NBPARR)
      CHARACTER*8   K8B, NOMA, GROUPE
      CHARACTER*16  NOPARR(NBPARR)
      CHARACTER*24  GRPMA
      COMPLEX*16    C16B
C     ------------------------------------------------------------------
      DATA NOPARR / 'GROUP_MA' , 'AIRE' , 'LONGUEUR' /
      DATA TYPARR / 'K8' , 'R' , 'R' /
C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
C --- RECUPERATION DU NIVEAU D'IMPRESSION
      CALL INFNIV(IFM,NIV)
C
      CALL JEVEUO(MODELE(1:8)//'.MODELE    .NOMA','L',JNOMA)
      NOMA = ZK8(JNOMA)
      GRPMA = NOMA//'.GROUPEMA'
C
C     --- CREATION DE LA TABLE ---
C
      CALL TBCRSD ( RESU, 'G' )
      CALL TBAJPA ( RESU, NBPARR, NOPARR, TYPARR )
C
      DO 10 IOCC = 1 , NBOCC
         CALL GETVEM(NOMA,'GROUP_MA','AIRE_INTERNE','GROUP_MA_BORD',
     &                                IOCC,1,0,K8B,NGB)
         IF ( NGB .NE. 0 ) THEN
            NGB = -NGB
            CALL WKVECT ( '&&PEAIRE.GROUP_NO', 'V V K8', NGB, JGB )
            CALL GETVEM(NOMA,'GROUP_MA','AIRE_INTERNE','GROUP_MA_BORD',
     &                                   IOCC,1,NGB,ZK8(JGB),NG)
            DO 40 IGB = 1 , NGB
               CALL JEEXIN ( JEXNOM(GRPMA,ZK8(JGB+IGB-1)), IRET )
               IF ( IRET .EQ. 0 ) THEN
                  CALL U2MESK('A','UTILITAI3_46',1,ZK8(JGB+IGB-1))
                  GOTO 40
               ENDIF
               CALL JELIRA ( JEXNOM(GRPMA,ZK8(JGB+IGB-1)),
     &                                             'LONMAX', NBB, K8B )
               IF ( NBB .EQ. 0 ) THEN
                  CALL U2MESK('A','UTILITAI3_47',1,ZK8(JGB+IGB-1))
                  GOTO 40
               ENDIF
               CALL JEVEUO(JEXNOM(GRPMA,ZK8(JGB+IGB-1)),'L',IADGMA)
C
C              BORD DU TROU : CALCUL DE L'AIRE
C
               CALL PEAIR1 ( MODELE, NBB, ZI(IADGMA), AIRE, LONG )
               VALPAR(1) = AIRE
               VALPAR(2) = LONG
               CALL TBAJLI ( RESU, NBPARR, NOPARR, IBID,
     &                             VALPAR, C16B, ZK8(JGB+IGB-1),0 )
 40         CONTINUE
            CALL JEDETR ( '&&PEAIRE.GROUP_NO' )
         ENDIF
 10   CONTINUE
C
 9999 CONTINUE
      CALL JEDEMA ( )
      END
