      SUBROUTINE INFDIS(QUEST,IVALE,RVALE,KVALE)
      IMPLICIT       NONE
      CHARACTER*4    QUEST
      CHARACTER*(*)  KVALE
      INTEGER        IVALE
      REAL*8         RVALE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/06/2011   AUTEUR PELLET J.PELLET 
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
C RESPONSABLE FLEJOU J-L.FLEJOU
C --- ------------------------------------------------------------------
C
C              INTERROGE LA CARTE DES 'CINFDI' DES DISCRETS
C              INFORMATIONS ANNEXES SUR LES DISCRETS
C
C  IN
C     QUEST : INFORMATION QUE L'ON SOUHAITE RECUPERER
C        RECUPERE DES INFORMATIONS STOCKEES DANS LA CARTE
C           =  REP[K|M|A]  : REPERE
C           =  SYM[K|M|A]  : SYMETRIQUE
C           =  DIS[K|M|A]  : TYPE DE MATRICE AFFECTEE AU DISCRET
C           =  ETAK        : COEFFICIENT AMORTISSEMENT HYSTERETIQUE
C           =  TYDI        : TYPE DU DISCRET
C        POUR FACILITER LA VIE
C           =  SKMA        : TOUTES LES MATRICES SONT-ELLES SYMETRIQUES
C        INFORMATIONS ANNEXES SUR LES DISCRETS
C           =  DIMC        : TAILLE DE LA CARTE
C           =  DMXM        : TAILLE MAXI DES MATRICES D'UN DISCRET
C           =  CODE        : LE CODE DU DISCRET A PARTIR DE KVALE
C           =  INIT        : VALEUR INITIALE DE KVALE
C     KVALE : SI QUEST=CODE, DOIT CONTENIR LE NOM DU DISCRET
C             SI QUEST=INIT, DOIT CONTENIR LE PARAMETRE A INITIALISER
C  OUT
C     IVALE : SI REP[K|M|A] : REPERE GLOBAL(=1) OU LOCAL(=2)
C           : SI SYM[K|M|A] : MATRICE SYMETRIQUE(=1), NON-SYSMETRE(=2)
C           : SI DIS[K|M|A] : MATRICE AFFECTEE(=1), NON AFFECTEE(=0)
C           : SI SKMA : TOUTES LES MATRICES SONT SYMETRIQUE=3 SINON >3
C           : SI TYDI : LE CODE DU DISCRET STOKE DANS LA CARTE
C           : SI CODE : LE CODE ENTIER DU DISCRET
C     RVALE : SI ETAK : COEFFICIENT AMORTISSEMENT HYSTERETIQUE
C
C --- ------------------------------------------------------------------
C     ELEMENTS CONCERNES : TOUS LES DISCREST
C --- ------------------------------------------------------------------
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
      INTEGER        NBELEM,II,JDC,JJ,KK,IADZI,IAZK24,ICOORD
      PARAMETER     (NBELEM=8)
      INTEGER        LENMND(NBELEM),LENMDD(NBELEM)
      CHARACTER*13   ELEMND(NBELEM),ELEMDD(NBELEM)
      CHARACTER*20   CARACZ

      CHARACTER*8    NOMMAI,MAILLA
      INTEGER        NBNOEU
      REAL*8         R8BID

      DATA ELEMND /  '_DIS_T_N     ','_DIS_TR_N    ',
     &               '_DIS_T_L     ','_DIS_TR_L    ',
     &               '2D_DIS_T_N   ','2D_DIS_TR_N  ',
     &               '2D_DIS_T_L   ','2D_DIS_TR_L  '/
      DATA LENMND /   8, 9, 8, 9,10,11,10,11/
      DATA ELEMDD /  '_DIS_T_D_N   ','_DIS_TR_D_N  ',
     &               '_DIS_T_D_L   ','_DIS_TR_D_L  ',
     &               '2D_DIS_T_D_N ','2D_DIS_TR_D_N',
     &               '2D_DIS_T_D_L ','2D_DIS_TR_D_L'/
      DATA LENMDD /  10,11,10,11,12,13,12,13/
C
C     ORDRE DE STOCKAGE DANS LA CARTE : CINFDI
C     0     1     2     3     4     5     6     7     8     9     10
C     REPK  REPM  REPA  SYMK  SYMM  SYMA  DISK  DISM  DISA  ETAK  TYDI
C
C
      CARACZ = ' '
      IF      ( QUEST .EQ. 'DIMC' ) THEN
         IVALE = 11
         RVALE = 11.0D0
         GOTO 9999
      ELSE IF ( QUEST .EQ. 'DMXM' ) THEN
         IVALE = 144
         RVALE = 144.0D0
         GOTO 9999
      ELSE IF ( QUEST .EQ. 'DUMP' ) THEN
         CALL TECAEL(IADZI,IAZK24)
         NOMMAI = ZK24(IAZK24-1+3)
         NBNOEU = ZI(IADZI+1)
         CALL U2MESG(KVALE(1:1)//'+','DISCRETS_30',
     &      1,NOMMAI,1,NBNOEU,0,R8BID)
         MAILLA = ZK24(IAZK24)
         CALL JEVEUO(MAILLA//'.COORDO    .VALE','L',ICOORD)
         DO 10 JJ=1,NBNOEU
            II = ZI(IADZI+1+JJ)
            IF ( JJ .EQ. NBNOEU ) THEN
               CALL U2MESG(KVALE(1:1),'DISCRETS_31',
     &            1,ZK24(IAZK24-1+3+JJ),0,NBNOEU,3,ZR(ICOORD+3*(II-1)))
            ELSE
               CALL U2MESG(KVALE(1:1)//'+','DISCRETS_31',
     &            1,ZK24(IAZK24-1+3+JJ),0,NBNOEU,3,ZR(ICOORD+3*(II-1)))
            ENDIF
10       CONTINUE
         GOTO 9999
      ELSE IF ( QUEST .EQ. 'CODE' ) THEN
         CARACZ = KVALE
         KK=LEN( CARACZ )
         DO 20 II=KK,1,-1
            IF ( CARACZ(II:II).NE.' ') THEN
               KK=II
               GOTO 9995
            ENDIF
20       CONTINUE
         CALL ASSERT( .FALSE. )
9995     CONTINUE
         IVALE  = 0
         RVALE  = 0.0D0
         DO 25 II=1 , NBELEM
            JJ=LENMND(II)
            IF (KK.GE.JJ) THEN
               IF ( CARACZ(KK-JJ+1:KK).EQ.ELEMND(II) ) THEN
                  IVALE = II
                  RVALE = IVALE
                  GOTO 9999
               ENDIF
            ENDIF
25       CONTINUE
         DO 30 II=1 , NBELEM
            JJ=LENMDD(II)
            IF (KK.GE.JJ) THEN
               IF ( CARACZ(KK-JJ+1:KK).EQ.ELEMDD(II) ) THEN
                  IVALE = II
                  RVALE = IVALE
                  GOTO 9999
               ENDIF
            ENDIF
30       CONTINUE
         CALL ASSERT( IVALE.NE.0 )
      ELSE IF ( QUEST .EQ. 'INIT' ) THEN
         CARACZ = KVALE
         IF      ( CARACZ(1:3) .EQ. 'REP' ) THEN
            IVALE = 1
         ELSE IF ( CARACZ(1:3) .EQ. 'SYM' ) THEN
            IVALE = 1
         ELSE IF ( CARACZ(1:3) .EQ. 'DIS' ) THEN
            IVALE = 0
         ELSE IF ( CARACZ .EQ. 'ETAK' ) THEN
            IVALE = 0
         ELSE IF ( CARACZ .EQ. 'TYDI' ) THEN
            IVALE = 0
         ELSE
            CALL ASSERT( .FALSE. )
         ENDIF
         RVALE = IVALE
         GOTO 9999
      ENDIF
C
      RVALE = 0.0D0
      IVALE = 0
      CALL JEVECH('PCINFDI','L',JDC)
      IF      ( QUEST .EQ. 'REPK' ) THEN
         RVALE = ZR(JDC)
      ELSE IF ( QUEST .EQ. 'REPM' ) THEN
         RVALE = ZR(JDC+1)
      ELSE IF ( QUEST .EQ. 'REPA' ) THEN
         RVALE = ZR(JDC+2)
C
      ELSE IF ( QUEST .EQ. 'SYMK' ) THEN
         RVALE = ZR(JDC+3)
      ELSE IF ( QUEST .EQ. 'SYMM' ) THEN
         RVALE = ZR(JDC+4)
      ELSE IF ( QUEST .EQ. 'SYMA' ) THEN
         RVALE = ZR(JDC+5)
C
      ELSE IF ( QUEST .EQ. 'DISK' ) THEN
         RVALE = ZR(JDC+6)
      ELSE IF ( QUEST .EQ. 'DISM' ) THEN
         RVALE = ZR(JDC+7)
      ELSE IF ( QUEST .EQ. 'DISA' ) THEN
         RVALE = ZR(JDC+8)
C
      ELSE IF ( QUEST .EQ. 'ETAK' ) THEN
         RVALE = ZR(JDC+9)
         GOTO 9999
C
      ELSE IF ( QUEST .EQ. 'TYDI' ) THEN
         RVALE = ZR(JDC+10)
C
      ELSE IF ( QUEST .EQ. 'SKMA' ) THEN
         RVALE = ZR(JDC+3)+ZR(JDC+4)+ZR(JDC+5)
      ELSE
         CALL ASSERT( .FALSE. )
      ENDIF
C
      IVALE = NINT(RVALE)
9999  CONTINUE
      END
