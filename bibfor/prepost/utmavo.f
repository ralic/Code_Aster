      SUBROUTINE UTMAVO ( MAIL, KDIM, LIMA, NLIMA, BASE, NOMZ,
     &                    NBMAVO, MAILVO )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER             LIMA(*), NLIMA, NBMAVO, MAILVO(*)
      CHARACTER*1         BASE
      CHARACTER*2         KDIM
      CHARACTER*8         MAIL
      CHARACTER*(*)       NOMZ
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C              '1D' RECHERCHE LES MAILLES 1D VOISINES
C              '  ' RECHERCHE TOUTES LES MAILLES VOISINES
C     LIMA   : LISTE DES NUMEROS DE MAILLES
C     NLIMA  : NOMBRE DE MAILLES
C     BASE   : BASE DE CREATION
C     NOMZ   : NOM DE L' OJB A CREER
C     MAILVO : SI ORIE_PEAU_3D ("GROUP_MA_VOLU"):
C                  = LISTE DES MAILLES VOLUMIQUES
C                    UTILES A LA REORIENTATION
C              SI ORIE_PEAU_2D ("GROUP_MA_SURF"):
C                  = LISTE DES MAILLES SURFACIQUES
C                    UTILES A LA REORIENTATION
C              SINON: MAILVO N'EST PAS UTILISE
C     NBMAVO : NB DE MAILLES DE MAILVO
C
C   ORGANISATION
C   ------------
C     TYPE : XC V I ACCES(NUMEROTE) LONG(VARIABLE)
C-----------------------------------------------------------------------
C
      INTEGER       IBID,NARE,NUMA,NBNO,NBMAT,INO,NUNO,P1,P2
      INTEGER       I,J,K,JMAIL,NBMAN,ADRVLC,ACNCIN,IMA,II
      INTEGER       ADRA,IAD,JTR1(1000), JTR2, IDTYMA, NUTYMA, IEXINV
      CHARACTER*8    TYPE
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
C     EST-CE QUE LA CONNECTIVITE INVERSE A DEJA ETE CALCULEE ?
      CALL JEEXIN ( NCNINV, IEXINV )
      IF(NBMAVO.EQ.0)THEN
         IF(IEXINV.EQ.0)
     +      CALL CNCINV ( MAIL, IBID, IBID, 'V', NCNINV )
      ELSE
C        ON FORCE LE CALCUL DE LA CONNECTIVITE INVERSE
         CALL JEDETR ( NCNINV )
         CALL CNCINV ( MAIL, MAILVO, NBMAVO, 'V', NCNINV )
      ENDIF
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
               II = ZI(ACNCIN+ADRA-1+J-1)
C              -- SI UN NOEUD EST ORPHELIN : II=0
C                 (PAS D'OBJET JEVEUX DE LONG=0)
               IF (II.EQ.0) THEN
                 CALL ASSERT(NBMAN.EQ.1)
                 GOTO 120
               ENDIF
               IF(NBMAVO.EQ.0)THEN
                 IMA=II
               ELSE
                 IMA=MAILVO(II)
               ENDIF
               IF ( IMA .EQ. NUMA ) GOTO 120
               NUTYMA = ZI(IDTYMA+IMA-1)
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYMA),TYPE)
               IF (TYPE(1:4).EQ.'HEXA') THEN
                  IF ( KDIM.EQ.'2D' .OR. KDIM.EQ.'1D') GOTO 120
               ELSEIF (TYPE(1:4).EQ.'PENT') THEN
                  IF ( KDIM.EQ.'2D' .OR. KDIM.EQ.'1D') GOTO 120
               ELSEIF (TYPE(1:4).EQ.'PYRA') THEN
                  IF ( KDIM.EQ.'2D' .OR. KDIM.EQ.'1D') GOTO 120
               ELSEIF (TYPE(1:4).EQ.'TETR') THEN
                  IF ( KDIM.EQ.'2D' .OR. KDIM.EQ.'1D') GOTO 120
               ELSEIF (TYPE(1:4).EQ.'QUAD') THEN
                  IF ( KDIM.EQ.'3D' .OR. KDIM.EQ.'1D') GOTO 120
               ELSEIF (TYPE(1:4).EQ.'TRIA') THEN
                  IF ( KDIM.EQ.'3D' .OR. KDIM.EQ.'1D') GOTO 120
               ELSEIF (TYPE(1:3).EQ.'SEG') THEN
                  IF ( KDIM.EQ.'3D' .OR. KDIM.EQ.'2D') GOTO 120
               ELSEIF (TYPE(1:3).EQ.'POI') THEN
                  IF ( KDIM.NE.'  ' ) GOTO 120
               ELSE
                  CALL U2MESK('F','PREPOST4_89',1,TYPE)
               ENDIF
               DO 122 K = 1 , NBMAT
                  IF ( JTR1(K) .EQ. IMA ) GOTO 120
 122           CONTINUE
               NBMAT = NBMAT + 1
               JTR1(NBMAT) = IMA
 120        CONTINUE
 110     CONTINUE
         ZI(JTR2-1+I) = NBMAT
         NARE = NARE + MAX(NBMAT,1)
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
               II = ZI(ACNCIN+ADRA-1+J-1)
               IF (II.EQ.0)  GOTO 220

               IF(NBMAVO.EQ.0)THEN
                 IMA=II
               ELSE
                 IMA=MAILVO(II)
               ENDIF
               IF ( IMA .EQ. NUMA ) GOTO 220
               NUTYMA = ZI(IDTYMA+IMA-1)
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYMA),TYPE)
               IF (TYPE(1:4).EQ.'HEXA') THEN
                  IF ( KDIM .EQ. '2D' ) GOTO 220
                  IF ( KDIM .EQ. '1D' ) GOTO 220
               ELSEIF (TYPE(1:4).EQ.'PENT') THEN
                  IF ( KDIM .EQ. '2D' ) GOTO 220
                  IF ( KDIM .EQ. '1D' ) GOTO 220
               ELSEIF (TYPE(1:4).EQ.'PYRA') THEN
                  IF ( KDIM .EQ. '2D' ) GOTO 220
                  IF ( KDIM .EQ. '1D' ) GOTO 220
               ELSEIF (TYPE(1:4).EQ.'TETR') THEN
                  IF ( KDIM .EQ. '2D' ) GOTO 220
                  IF ( KDIM .EQ. '1D' ) GOTO 220
               ELSEIF (TYPE(1:4).EQ.'QUAD') THEN
                  IF ( KDIM .EQ. '3D' ) GOTO 220
                  IF ( KDIM .EQ. '1D' ) GOTO 220
               ELSEIF (TYPE(1:4).EQ.'TRIA') THEN
                  IF ( KDIM .EQ. '3D' ) GOTO 220
                  IF ( KDIM .EQ. '1D' ) GOTO 220
               ELSEIF (TYPE(1:3).EQ.'SEG') THEN
                  IF ( KDIM .EQ. '3D' ) GOTO 220
                  IF ( KDIM .EQ. '2D' ) GOTO 220
               ELSEIF (TYPE(1:3).EQ.'POI') THEN
                  IF ( KDIM.NE.'  ' ) GOTO 220
               ELSE
                  CALL U2MESK('F','PREPOST4_89',1,TYPE)
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
      IF (NBMAVO.NE.0)
     +   CALL JEDETR ( NCNINV )
      CALL JEDEMA()

      END
