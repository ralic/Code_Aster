      SUBROUTINE UTMAVO ( MAIL, KDIM, LIMA, NLIMA, BASE, NOMZ )
      IMPLICIT NONE
      INTEGER             LIMA(*), NLIMA
      CHARACTER*1         BASE
      CHARACTER*2         KDIM
      CHARACTER*8         MAIL
      CHARACTER*(*)       NOMZ
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 13/03/2006   AUTEUR CIBHHLV L.VIVAN 
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
C
C     DETERMINE LES MAILLES VOISINES D'UNE LISTE DE MAILLES
C     MAILLE --> LISTE DES MAILLES VOISINES
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
C     NOMZ   : NOM DE L' OJB A CREER
C
C   ORGANISATION
C   ------------
C     TYPE : XC V I ACCES(NUMEROTE) LONG(VARIABLE)
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
      CHARACTER*32     JEXNOM, JEXNUM, JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       IBID,NARE,NUMA,NBNO,NBMAT,INO,NUNO,P1,P2,IRET
      INTEGER       I,J,K,JMAIL,NBMAN,ADRMA,ADRVLC,ACNCIN,IMA
      INTEGER       ADRA,IAD,JTR1(1000), JTR2, IDTYMA, NUTYMA
      CHARACTER*8   K8B, TYPE
      CHARACTER*24  NOM, NCNINV
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      NOM = NOMZ
C
C --- APPEL A LA CONNECTIVITE INVERSE
C
      IBID = 0
      NCNINV = '&&UTMAVO.CONNEC_INV'
      CALL JEEXIN ( NCNINV, IRET )
      IF ( IRET .EQ. 0 ) CALL CNCINV ( MAIL, IBID, IBID, 'V', NCNINV )
      CALL JEVEUO ( JEXATR(NCNINV,'LONCUM'), 'L', ADRVLC )
      CALL JEVEUO ( NCNINV, 'L', ACNCIN )
C
C --- APPEL A LA CONNECTIVITE
C
      CALL JEVEUO ( JEXATR(MAIL//'.CONNEX','LONCUM'), 'L', P2 )
      CALL JEVEUO ( MAIL//'.CONNEX', 'L', P1 )
C
      CALL JEVEUO ( MAIL//'.TYPMAIL', 'L', IDTYMA )
C
C --- DIMENSIONNEMENT DE LA SD
C
CCC      CALL WKVECT ( '&&UTMAVO.TRAV1', 'V V I', 1000, JTR1 )
      CALL WKVECT ( '&&UTMAVO.TRAV2', 'V V I', NLIMA, JTR2 )
      NARE = 0
      DO 100 I = 1, NLIMA
         NUMA = LIMA(I)
         NBNO = ZI(P2+NUMA+1-1) - ZI(P2+NUMA-1)
         IAD  = ZI(P2+NUMA-1)
         NBMAT = 0
         DO 110 INO = 1, NBNO
            NUNO = ZI(P1+IAD-1+INO-1)
            NBMAN = ZI(ADRVLC+NUNO+1-1) - ZI(ADRVLC+NUNO-1)
            ADRA  = ZI(ADRVLC+NUNO-1)
            DO 120 J = 1 , NBMAN
               IMA = ZI(ACNCIN+ADRA-1+J-1)
               IF ( IMA .EQ. NUMA ) GOTO 120
               NUTYMA = ZI(IDTYMA+IMA-1)
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYMA),TYPE)
               IF (TYPE(1:4).EQ.'HEXA') THEN
                  IF ( KDIM .EQ. '2D' ) GOTO 120
               ELSEIF (TYPE(1:4).EQ.'PENT') THEN
                  IF ( KDIM .EQ. '2D' ) GOTO 120
               ELSEIF (TYPE(1:4).EQ.'PYRA') THEN
                  IF ( KDIM .EQ. '2D' ) GOTO 120
               ELSEIF (TYPE(1:4).EQ.'TETR') THEN
                  IF ( KDIM .EQ. '2D' ) GOTO 120
               ELSEIF (TYPE(1:4).EQ.'QUAD') THEN
                  IF ( KDIM .EQ. '3D' ) GOTO 120
               ELSEIF (TYPE(1:4).EQ.'TRIA') THEN
                  IF ( KDIM .EQ. '3D' ) GOTO 120
               ELSE
                  IF ( KDIM .NE. '  ' ) GOTO 120
               ENDIF
               DO 122 K = 1 , NBMAT
CCC                  IF ( ZI(JTR1-1+K) .EQ. IMA ) GOTO 120
                  IF ( JTR1(K) .EQ. IMA ) GOTO 120
 122           CONTINUE
               NBMAT = NBMAT + 1
CCC               ZI(JTR1-1+NBMAT) = IMA
               JTR1(NBMAT) = IMA
 120        CONTINUE
 110     CONTINUE
         ZI(JTR2-1+I) = NBMAT
         NARE = NARE + NBMAT
 100  CONTINUE
C
C --- CREATION DE LA SD
C
      CALL JECREC (NOM,BASE//' V I','NU','CONTIG','VARIABLE',NLIMA)
      CALL JEECRA (NOM, 'LONT', NARE, ' ' )
C
C --- ON REMPLIT LA SD
C
      DO 200 I = 1, NLIMA
         NUMA = LIMA(I)
         NBNO = ZI(P2+NUMA+1-1) - ZI(P2+NUMA-1)
         IAD  = ZI(P2+NUMA-1)

         CALL JECROC ( JEXNUM(NOM,I) )
         IF ( ZI(JTR2-1+I) .EQ. 0 ) THEN
            CALL JEECRA ( JEXNUM(NOM,I), 'LONMAX', 1, ' ' )
            CALL JEECRA ( JEXNUM(NOM,I), 'LONUTI', 0, ' ' )
            GOTO 200
         ELSE
            CALL JEECRA ( JEXNUM(NOM,I), 'LONMAX', ZI(JTR2-1+I), ' ' )
            CALL JEECRA ( JEXNUM(NOM,I), 'LONUTI', ZI(JTR2-1+I), ' ' )
            CALL JEVEUO ( JEXNUM(NOM,I), 'E', JMAIL )
         ENDIF
 
         NBMAT = 0
         DO 210 INO = 1, NBNO
            NUNO = ZI(P1+IAD-1+INO-1)
            NBMAN = ZI(ADRVLC+NUNO+1-1) - ZI(ADRVLC+NUNO-1)
            ADRA  = ZI(ADRVLC+NUNO-1)
            DO 220 J = 1 , NBMAN
               IMA = ZI(ACNCIN+ADRA-1+J-1)
               IF ( IMA .EQ. NUMA ) GOTO 220
               NUTYMA = ZI(IDTYMA+IMA-1)
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYMA),TYPE)
               IF (TYPE(1:4).EQ.'HEXA') THEN
                  IF ( KDIM .EQ. '2D' ) GOTO 220
               ELSEIF (TYPE(1:4).EQ.'PENT') THEN
                  IF ( KDIM .EQ. '2D' ) GOTO 220
               ELSEIF (TYPE(1:4).EQ.'PYRA') THEN
                  IF ( KDIM .EQ. '2D' ) GOTO 220
               ELSEIF (TYPE(1:4).EQ.'TETR') THEN
                  IF ( KDIM .EQ. '2D' ) GOTO 220
               ELSEIF (TYPE(1:4).EQ.'QUAD') THEN
                  IF ( KDIM .EQ. '3D' ) GOTO 220
               ELSEIF (TYPE(1:4).EQ.'TRIA') THEN
                  IF ( KDIM .EQ. '3D' ) GOTO 220
               ELSE
                  IF ( KDIM .NE. '  ' ) GOTO 220
               ENDIF
               DO 222 K = 1 , NBMAT
                  IF ( ZI(JMAIL-1+K) .EQ. IMA ) GOTO 220
 222           CONTINUE
               NBMAT = NBMAT + 1
               ZI(JMAIL-1+NBMAT) = IMA
 220        CONTINUE
 210     CONTINUE

 200  CONTINUE

      CALL JEDETR ( '&&UTMAVO.TRAV1' )
      CALL JEDETR ( '&&UTMAVO.TRAV2' )

      CALL JEDEMA()

      END
