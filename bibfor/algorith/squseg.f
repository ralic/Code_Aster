      SUBROUTINE SQUSEG ( NOMA, NOMVE1, NOMVE2, NOMVE3 )
      IMPLICIT NONE
      CHARACTER*8         NOMA
      CHARACTER*24              NOMVE1, NOMVE2, NOMVE3
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/02/2008   AUTEUR MACOCCO K.MACOCCO 
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
C     CHANGE LES MAILLES DU MAILLAGE "NOMA" EN DES MAILLES "SEG2"
C-----------------------------------------------------------------------
C-------- DEBUT COMMUNS NORMALISES  JEVEUX  ----------------------------
C
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
      CHARACTER*32 JEXNOM,JEXNUM
C
C----------  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER       IER, NBMA, NBNO
      INTEGER       JNOE1, JNOE2, JNOEU, JTYP, JPT,IATYMA
      INTEGER       NBMAIL, NBNOEU, IMA, NUMN1, NUMN2, NUMN3, NUMN4
      CHARACTER*8   K8B, TYPE
      CHARACTER*24  TYPMAI, CONNEX
C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,K8B,IER)
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,K8B,IER)
      TYPMAI = NOMA//'.TYPMAIL        '
      CONNEX = NOMA//'.CONNEX         '
C
      CALL WKVECT ( NOMVE1 , 'V V I', NBMA, JNOE1 )
      CALL WKVECT ( NOMVE2 , 'V V I', NBMA, JNOE2 )
      CALL WKVECT ( NOMVE3 , 'V V I', NBNO, JNOEU  )
C
      NBMAIL = 0
      NBNOEU = 0
      DO 10 IMA = 1 , NBMA
         CALL JEVEUO ( TYPMAI, 'L',IATYMA)
         JTYP=IATYMA-1+IMA
         CALL JEVEUO ( JEXNUM(CONNEX,IMA), 'L', JPT )
         CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(JTYP)),TYPE)
C
         IF ( TYPE .EQ. 'POI1' ) THEN
            CALL U2MESS('A','ALGORITH10_74')
C
         ELSEIF ( TYPE .EQ. 'SEG2'  .OR.
     &            TYPE .EQ. 'SEG3'  ) THEN
            NUMN1 = ZI(JPT  )
            NUMN2 = ZI(JPT+1)
            CALL SQULIS ( ZI(JNOE1), ZI(JNOE2), ZI(JNOEU),
     &                    NUMN1, NUMN2, NBMAIL, NBNOEU )
C
         ELSEIF ( TYPE .EQ. 'TRIA3'  .OR.
     &            TYPE .EQ. 'TRIA6'  .OR.
     &            TYPE .EQ. 'TRIA7'  ) THEN
            NUMN1 = ZI(JPT  )
            NUMN2 = ZI(JPT+1)
            NUMN3 = ZI(JPT+2)
            CALL SQULIS ( ZI(JNOE1), ZI(JNOE2), ZI(JNOEU),
     &                    NUMN1, NUMN2, NBMAIL, NBNOEU )
            CALL SQULIS ( ZI(JNOE1), ZI(JNOE2), ZI(JNOEU),
     &                    NUMN2, NUMN3, NBMAIL, NBNOEU )
            CALL SQULIS ( ZI(JNOE1), ZI(JNOE2), ZI(JNOEU),
     &                    NUMN3, NUMN1, NBMAIL, NBNOEU )
C
         ELSEIF ( TYPE .EQ. 'QUAD4'  .OR.
     &            TYPE .EQ. 'QUAD8'  .OR.
     &            TYPE .EQ. 'QUAD9'  ) THEN
            NUMN1 = ZI(JPT  )
            NUMN2 = ZI(JPT+1)
            NUMN3 = ZI(JPT+2)
            NUMN4 = ZI(JPT+3)
            CALL SQULIS ( ZI(JNOE1), ZI(JNOE2), ZI(JNOEU),
     &                    NUMN1, NUMN2, NBMAIL, NBNOEU )
            CALL SQULIS ( ZI(JNOE1), ZI(JNOE2), ZI(JNOEU),
     &                    NUMN2, NUMN3, NBMAIL, NBNOEU )
            CALL SQULIS ( ZI(JNOE1), ZI(JNOE2), ZI(JNOEU),
     &                    NUMN3, NUMN4, NBMAIL, NBNOEU )
            CALL SQULIS ( ZI(JNOE1), ZI(JNOE2), ZI(JNOEU),
     &                    NUMN4, NUMN1, NBMAIL, NBNOEU )
C
         ELSE
            CALL U2MESS('F','ALGORITH10_75')
         ENDIF
 10   CONTINUE
C
      CALL JEECRA ( NOMVE1 , 'LONUTI' , NBMAIL , K8B )
      CALL JEECRA ( NOMVE2 , 'LONUTI' , NBMAIL , K8B )
      CALL JEECRA ( NOMVE3 , 'LONUTI' , NBNOEU , K8B )
C
      CALL JEDEMA ( )
      END
