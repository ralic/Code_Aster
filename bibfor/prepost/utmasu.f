      SUBROUTINE UTMASU ( MAIL, KDIM, NLIMA, LIMA, NOMOB1, PREC,
     &                    COOR, NBMAVO, MAILVO )
      IMPLICIT NONE
      INTEGER             LIMA(*), NLIMA, NBMAVO, MAILVO(*)
      REAL*8              PREC, COOR(*)
      CHARACTER*2         KDIM
      CHARACTER*8         MAIL
      CHARACTER*(*)       NOMOB1
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     DETERMINE LES MAILLES SUPPORT D'UNE LISTE DE MAILLES DE PEAU
C     MAILLE PEAU 2D => MAILLE SUPPORT 3D
C     MAILLE PEAU 1D => MAILLE SUPPORT 2D
C
C   ARGUMENT EN ENTREE
C   ------------------
C     MAIL   : NOM DE L'OJB REPRESENTANT LE MAILLAGE
C     KDIM   : '3D' RECHERCHE LES MAILLES 3D VOISINES
C              '2D' RECHERCHE LES MAILLES 2D VOISINES
C              '  ' RECHERCHE TOUTES LES MAILLES VOISINES
C     LIMA   : LISTE DES NUMEROS DE MAILLES
C     NLIMA  : NOMBRE DE MAILLES
C     BASE   : BASE DE CREATION
C     NOMOB1 : NOM DE L' OJB A CREER
C     MAILVO : SI ORIE_PEAU_3D ("GROUP_MA_VOLU"):
C                  = LISTE DES MAILLES VOLUMIQUES
C                    UTILES A LA REORIENTATION
C              SI ORIE_PEAU_2D ("GROUP_MA_SURF"):
C                  = LISTE DES MAILLES SURFACIQUES
C                    UTILES A LA REORIENTATION
C              SINON: MAILVO N'EST PAS UTILISE
C     NBMAVO : NB DE MAILLES DE MAILVO

C-----------------------------------------------------------------------
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
      CHARACTER*32      JEXNUM, JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       P1,P2,P3,P4, JM3D, INDIIS, NBMAT, IRET, IM1, IM2
      INTEGER       IMA, NUMA, NNOE, INO, NBM, I, K, INDI, NNOEM, NNOE1
      INTEGER       IFM , NIV, IPOS, ITYPMA, NUTYMA
      INTEGER       LISNOE(27),INDMAI
      LOGICAL       FIRST,LVNOR,OKCOIN
      CHARACTER*8   K8B, NOMAIL, TYPE, VNOR
      CHARACTER*16  OPER,K16B
      CHARACTER*24  NOMAVO,VALK(4)
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL INFNIV ( IFM , NIV )
      FIRST = .FALSE.
      OKCOIN= .FALSE.
      CALL GETRES(K8B,K16B,OPER)
C
C --- VERIFIERA T'ON LES NORMALES ?
C     -----------------------------
      LVNOR=.TRUE.
      IF(OPER(1:14).EQ.'AFFE_CHAR_MECA')THEN
         CALL GETVTX(' ','VERI_NORM',0,1,1,VNOR,IRET)
         IF(IRET.NE.0)THEN
            IF(VNOR(1:3).EQ.'NON')LVNOR=.FALSE.
         ENDIF
      ENDIF
C
C --- AUTORISE T'ON LES MAILLES DE PEAU COINCEES ENTRE
C     DEUX MAILLES VOLUMIQUES ?
C     -------------------------
      IF(OPER(1:14).EQ.'CALC_ELEM')THEN
         LVNOR=.FALSE.
         OKCOIN=.TRUE.
      ENDIF
C
C --- APPEL A LA CONNECTIVITE :
C     -----------------------
      CALL JEVEUO ( JEXATR(MAIL//'.CONNEX','LONCUM'), 'L', P2 )
      CALL JEVEUO ( MAIL//'.CONNEX', 'L', P1 )
      CALL JEVEUO ( MAIL//'.TYPMAIL','L',ITYPMA)
C
C --- CREATION DE LA SD :
C     -----------------
      CALL WKVECT ( NOMOB1, 'V V I' , NLIMA, JM3D )
C
C --- RECUPERATION DES MAILLES VOISINES DU GROUP_MA :
C     ---------------------------------------------
      NOMAVO = '&&UTMASU.MAILLE_VOISINE '
      CALL UTMAVO ( MAIL, KDIM, LIMA, NLIMA, 'V', NOMAVO,NBMAVO,MAILVO)
      CALL JEVEUO ( JEXATR(NOMAVO,'LONCUM'), 'L', P4 )
      CALL JEVEUO ( NOMAVO, 'L', P3 )
C
C --- ON REMPLIT LA SD :
C     -----------------
      DO 100 IMA = 1, NLIMA
         NUMA = LIMA(IMA)
         NUTYMA=ZI(ITYPMA+NUMA-1)
         NNOE = ZI(P2+NUMA)-ZI(P2-1+NUMA)
         CALL ASSERT( NNOE .LE. 27 )
         DO 80 INO = 1,NNOE
            LISNOE(INO) = ZI(P1-1+ZI(P2+NUMA-1)+INO-1)
  80     CONTINUE
         NBMAT = ZI(P4+IMA+1-1) - ZI(P4+IMA-1)
         NBM = 0
         DO 10 I = 1, NBMAT
            IM2 = ZI(P3+ZI(P4+IMA-1)-1+I-1)
            IF ( IM2 .EQ. 0 ) GOTO 10
            IF ( ZI(P1+ZI(P2+IM2-1)-1) .EQ. 0 ) GOTO 10
            NNOEM = ZI(P2+IM2) - ZI(P2-1+IM2)

            DO 12 K =  1 , NNOE
               INDI = INDIIS(ZI(P1+ZI(P2+IM2-1)-1),LISNOE(K),1,NNOEM)
               IF ( INDI .EQ. 0 )   GOTO 10
 12         CONTINUE
            NBM = NBM + 1
            IF ( NBM .EQ. 1 ) THEN
               ZI(JM3D+IMA-1) = IM2
            ELSE
               IM1 = ZI(JM3D+IMA-1)
               NNOE1 = ZI(P2+IM1) - ZI(P2-1+IM1)
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYMA),TYPE)
               CALL ORIEM0 ( TYPE, MAIL, COOR, ZI(P1+ZI(P2+IM1-1)-1),
     &              NNOE1, ZI(P1+ZI(P2+IM2-1)-1), NNOEM, LISNOE, NNOE,
     &              PREC, IRET, IPOS, INDMAI )
               IF ( IPOS .NE. 0)THEN
                 IF(LVNOR) THEN
                   CALL JENUNO(JEXNUM(MAIL//'.NOMMAI',NUMA),VALK(1))
                   CALL JENUNO(JEXNUM(MAIL//'.NOMMAI',IM1),VALK(2))
                   CALL JENUNO(JEXNUM(MAIL//'.NOMMAI',IM2),VALK(3))
                   CALL U2MESK('F','PREPOST4_97',3,VALK)
                 ELSE IF(OKCOIN .AND. INDMAI.EQ.2)THEN
                   ZI(JM3D+IMA-1) = IM2
                 ENDIF
               ENDIF
            ENDIF
C
 10      CONTINUE
C
         IF ( NBM .EQ. 0 .AND. NIV.GT.1 ) THEN
            CALL JENUNO(JEXNUM(MAIL//'.NOMMAI',NUMA),NOMAIL)
            IF ( FIRST ) THEN
               VALK(1)=NOMAIL
               CALL U2MESK('A+','PREPOST6_29',1,VALK)
            ELSE
               VALK (1)= NOMAIL
               CALL U2MESG('A+','PREPOST6_30',1,VALK,0,0,0,0.D0)
            ENDIF
            FIRST = .TRUE.
         ENDIF
C
 100  CONTINUE
C
      CALL JEDETR ( NOMAVO )
C
      CALL JEDEMA()
C
      END
