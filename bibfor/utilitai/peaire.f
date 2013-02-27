      SUBROUTINE PEAIRE(RESU,MODELE,NBOCC)
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNOM
      INTEGER           NBOCC
      CHARACTER*(*)     RESU, MODELE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 25/02/2013   AUTEUR SELLENET N.SELLENET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER      NBPARR, IBID, IRET, IOCC, NG,JNOMA,NGB,JGB,IGB,NBB
      INTEGER       IFM ,NIV,IADGMA
      PARAMETER    ( NBPARR = 3 )
      REAL*8        VALPAR(NBPARR), AIRE, LONG
      CHARACTER*3   TYPARR(NBPARR)
      CHARACTER*8   K8B, NOMA
      CHARACTER*16  NOPARR(NBPARR)
      CHARACTER*24  GRPMA
      COMPLEX*16    C16B
      INTEGER      IARG
C     ------------------------------------------------------------------
      DATA NOPARR / 'GROUP_MA' , 'AIRE' , 'LONGUEUR' /
      DATA TYPARR / 'K24' , 'R' , 'R' /
C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
C --- RECUPERATION DU NIVEAU D'IMPRESSION
      CALL INFNIV(IFM,NIV)
C
      CALL JEVEUO(MODELE(1:8)//'.MODELE    .LGRF','L',JNOMA)
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
     &                                IOCC,IARG,0,K8B,NGB)
         IF ( NGB .NE. 0 ) THEN
            NGB = -NGB
            CALL WKVECT ( '&&PEAIRE.GROUP_NO', 'V V K24', NGB, JGB )
            CALL GETVEM(NOMA,'GROUP_MA','AIRE_INTERNE','GROUP_MA_BORD',
     &                                   IOCC,IARG,NGB,ZK24(JGB),NG)
            DO 40 IGB = 1 , NGB
               CALL JEEXIN ( JEXNOM(GRPMA,ZK24(JGB+IGB-1)), IRET )
               IF ( IRET .EQ. 0 ) THEN
                  CALL U2MESK('A','UTILITAI3_46',1,ZK24(JGB+IGB-1))
                  GOTO 40
               ENDIF
               CALL JELIRA ( JEXNOM(GRPMA,ZK24(JGB+IGB-1)),
     &                                             'LONMAX', NBB, K8B )
               IF ( NBB .EQ. 0 ) THEN
                  CALL U2MESK('A','UTILITAI3_47',1,ZK24(JGB+IGB-1))
                  GOTO 40
               ENDIF
               CALL JEVEUO(JEXNOM(GRPMA,ZK24(JGB+IGB-1)),'L',IADGMA)
C
C              BORD DU TROU : CALCUL DE L'AIRE
C
               CALL PEAIR1 ( MODELE, NBB, ZI(IADGMA), AIRE, LONG )
               VALPAR(1) = AIRE
               VALPAR(2) = LONG
               CALL TBAJLI ( RESU, NBPARR, NOPARR, IBID,
     &                             VALPAR, C16B, ZK24(JGB+IGB-1),0 )
 40         CONTINUE
            CALL JEDETR ( '&&PEAIRE.GROUP_NO' )
         ENDIF
 10   CONTINUE
C
      CALL JEDEMA ( )
      END
