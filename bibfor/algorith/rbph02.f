      SUBROUTINE RBPH02 ( MAILLA, NUMDDL, NBNOEU, OBJVE1, OBJVE2 )
      IMPLICIT   NONE
      INTEGER             NBNOEU
      CHARACTER*8         MAILLA
      CHARACTER*14        NUMDDL
      CHARACTER*24        OBJVE1 , OBJVE2
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C     ------------------------------------------------------------------
C     OPERATEUR REST_BASE_PHYS
C               TRAITEMENT DES MOTS CLES "NOEUD" ET "GROUP_NO"
C ----------------------------------------------------------------------
C     ------- DEBUT DES COMMUNS JEVEUX ---------------------------------
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
      CHARACTER*32     JEXNOM, JEXNUM
C     ------- FIN DES COMMUNS JEVEUX -----------------------------------
C     ------------------------------------------------------------------
      INTEGER       ICMP, N1, NBGRNO, INOMGR, INBNO, I, IRET, NB, NEQ,
     &              INOMNO, INUMNO, IDEC, J, IINO, NUNO, NUMIN, IMIN,
     &              INUDDL, NUNOE
      CHARACTER*8   K8B, NNOE, NOMCMP(6), GRNO
      CHARACTER*24  GRPNOE, NOMNOE
C     ------------------------------------------------------------------
      DATA NOMCMP   /'DX      ','DY      ','DZ      ',
     &               'DRX     ','DRY     ','DRZ     '/
C     ------------------------------------------------------------------
C
C
      NOMNOE = MAILLA//'.NOMNOE'
      GRPNOE = MAILLA//'.GROUPENO'
C
C
      CALL GETVEM(MAILLA,'GROUP_NO', ' ','GROUP_NO',
     &     1,1,0,K8B,N1)
      IF ( N1 .NE. 0 ) THEN
         NBGRNO = -N1
         CALL WKVECT ('&&RBPH02.LISTE_GROUPE','V V K8', NBGRNO, INOMGR )
         CALL GETVEM(MAILLA,'GROUP_NO',' ','GROUP_NO',
     &       1,1,NBGRNO,ZK8(INOMGR),N1)
C
         CALL WKVECT ( '&&RBPH02.NBNO_GROUPE', 'V V I', NBGRNO, INBNO )
         NBNOEU = 0
         DO 20 I = 1 , NBGRNO
            GRNO = ZK8(INOMGR+I-1)
            CALL JEEXIN(JEXNOM(GRPNOE,GRNO),IRET)
            IF ( IRET.EQ.0 ) CALL UTMESS('F','RBPH02','LE GROUPE DE '//
     &            'NOEUDS '//GRNO//' NE FAIT PAS PARTIE DU MAILLAGE '//
     &             MAILLA)
C        CALL U2MESK('F','ALGORITH10_18', 2 ,VALK)
            CALL JELIRA(JEXNOM(GRPNOE,GRNO),'LONMAX',NB,K8B)
            ZI(INBNO+I-1) = NB
            NBNOEU = NBNOEU + NB
  20     CONTINUE
         CALL WKVECT ('&&RBPH02.NOMS_NOEUDS', 'V V K8', NBNOEU, INOMNO)
         CALL WKVECT ( OBJVE2 , 'V V I' , NBNOEU, INUMNO )
         IDEC = 0
         DO 30 J = 1 , NBGRNO
            GRNO = ZK8(INOMGR+J-1)
            CALL JEVEUO(JEXNOM(GRPNOE,GRNO),'L',IINO)
            NB = ZI(INBNO+J-1)
            DO 40 I = 1,NB
               NUNO = ZI(IINO+I-1)
               ZI(INUMNO+IDEC+I-1) = NUNO
               CALL JENUNO(JEXNUM(NOMNOE,NUNO),ZK8(INOMNO+IDEC+I-1))
  40        CONTINUE
            CALL JELIBE(JEXNOM(GRPNOE,GRNO))
            IDEC = IDEC + NB
  30     CONTINUE
         CALL JEDETR ( '&&RBPH02.LISTE_GROUPE' )
         CALL JEDETR ( '&&RBPH02.NBNO_GROUPE'  )
      ENDIF
C
C
      CALL GETVEM(MAILLA,'NOEUD', ' ','NOEUD',
     &  1,1,0,K8B,N1)
      IF ( N1 .NE. 0 ) THEN
         NBNOEU = -N1
         CALL WKVECT ('&&RBPH02.NOMS_NOEUDS','V V K8', NBNOEU, INOMNO)
         CALL GETVEM(MAILLA,'NOEUD',' ','NOEUD',
     &   1,1,NBNOEU,ZK8(INOMNO),N1)
C
         CALL WKVECT ( OBJVE2 , 'V V I', NBNOEU, INUMNO )
         DO 50 I = 1,NBNOEU
            NNOE = ZK8(INOMNO+I-1)
            CALL JEEXIN ( JEXNOM(NOMNOE,NNOE) , IRET )
            IF ( IRET.EQ.0 ) CALL UTMESS('F','RBPH02','LE NOEUD '//
     &             NNOE//' NE FAIT PAS PARTIE DU MAILLAGE '//MAILLA)
C        CALL U2MESK('F','ALGORITH10_19', 2 ,VALK)
            CALL JENONU ( JEXNOM(NOMNOE,NNOE), ZI(INUMNO+I-1) )
  50     CONTINUE
      ENDIF
C
C
      DO 60 I = 1 , NBNOEU-1
         NUMIN = ZI(INUMNO+I-1)
         IMIN = I
         DO 70 J = I+1,NBNOEU
            IF ( ZI(INUMNO+J-1) .LT. NUMIN ) THEN
               NUMIN = ZI(INUMNO+J-1)
               IMIN = J
            ENDIF
  70     CONTINUE
         IF ( IMIN .NE. I ) THEN
            ZI(INUMNO+IMIN-1) = ZI(INUMNO+I-1)
            ZI(INUMNO+I-1) = NUMIN
            K8B = ZK8(INOMNO+IMIN-1)
            ZK8(INOMNO+IMIN-1) = ZK8(INOMNO+I-1)
            ZK8(INOMNO+I-1) = K8B
         ENDIF
  60  CONTINUE
C
      NEQ = 6 * NBNOEU
      CALL WKVECT ( OBJVE1, 'V V I', NEQ, INUDDL )
      DO 80 I = 1 , NBNOEU
         IDEC = 6 * ( I - 1 )
         NNOE = ZK8(INOMNO+I-1)
         DO 90 ICMP = 1,6
            CALL POSDDL('NUME_DDL',NUMDDL,NNOE,NOMCMP(ICMP),NUNOE,
     &                    ZI(INUDDL+IDEC+ICMP-1))
  90     CONTINUE
  80  CONTINUE
C
      CALL JEDETR ( '&&RBPH02.NOMS_NOEUDS')
C
      END
