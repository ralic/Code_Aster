      SUBROUTINE RCMA01 ( CHMATE, IMA, IPT, NBM, ADRM, VALE )
      IMPLICIT   NONE
      INTEGER             IMA, IPT, NBM, ADRM(*)
      REAL*8              VALE(*)
      CHARACTER*24        CHMATE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C
C     RECUPERATION DES CARACTERISTIQUES MATERIAU POUR UNE MAILLE DONNEE
C
C IN  : CHMATE : CHAM_ELEM MATERIAU
C IN  : IMA    : NUMERO DE LA MAILLE
C IN  : IPT    : NUMERO DU NOEUD DE LA MAILLE
C IN  : NBM    : NB DE MAILLES AU NOEUD
C IN  : ADRM   : NUMERO DES MAILLES
C OUT : VALE   : CARACTERISTIQUES MATERIAU
C                VALE(1) = E     TEMPERATURE CALCUL
C                VALE(2) = E     TEMPERATURE AMBIANTE
C                VALE(3) = NU
C                VALE(4) = ALPHA
C                VALE(5) = E        A GAUCHE DU NOEUD A TEMP AMBIANTE
C                VALE(6) = E        A DROITE DU NOEUD A TEMP AMBIANTE
C                VALE(7) = ALPHA    A GAUCHE DU NOEUD
C                VALE(8) = ALPHA    A DROITE DU NOEUD
C                VALE(9) = E        MOYEN ENTRE LES 2 ZONES
C                VALE(10) = EC
C                VALE(11) = SM
C                VALE(12) = M
C                VALE(13) = N
C     ------------------------------------------------------------------
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER    JCESV, JCESD, JCESL, NBCMP, DECMA, DECMB, ICMP, IAD,
     &           IN, IMB
      REAL*8     EC, E, NU, ALPHA, EA, ALPHAA, EB, ALPHAB
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
C --- LE CHAMP MATERIAU
C
      CALL JEVEUO(CHMATE(1:19)//'.CESV', 'L', JCESV )
      CALL JEVEUO(CHMATE(1:19)//'.CESD', 'L', JCESD )
      CALL JEVEUO(CHMATE(1:19)//'.CESL', 'L', JCESL )
      NBCMP = ZI(JCESD-1+2)
      DECMA = ZI(JCESD-1+5+4*(IMA-1)+4)
C
C --- LE MATERIAU : E   TEMPERATURE CALCUL
C
      ICMP = 1
      IAD = DECMA + (IPT-1)*NBCMP + ICMP
      IF ( .NOT. ZL(JCESL-1+IAD) ) THEN
         CALL UTDEBM('F','RCMA01','ERREUR DONNEES ')
         CALL UTIMPI('L','POUR LA MAILLE ',1,IMA)
         CALL UTIMPI('S',' ET LE NOEUD ',1,IPT)
         CALL UTIMPK('L','IL MANQUE LE ',1,'E CALCUL')
         CALL UTFINM
      ENDIF
      EC = ZR(JCESV-1+IAD)
C
C --- LE MATERIAU : E   TEMPERATURE AMBIANTE
C
      ICMP = 2
      IAD = DECMA + (IPT-1)*NBCMP + ICMP
      IF ( .NOT. ZL(JCESL-1+IAD) ) THEN
         CALL UTDEBM('F','RCMA01','ERREUR DONNEES ')
         CALL UTIMPI('L','POUR LA MAILLE ',1,IMA)
         CALL UTIMPI('S',' ET LE NOEUD ',1,IPT)
         CALL UTIMPK('L','IL MANQUE LE ',1,'E AMBIANT')
         CALL UTFINM
      ENDIF
      E = ZR(JCESV-1+IAD)
C
C --- LE MATERIAU : NU
C
      ICMP = 3
      IAD = DECMA + (IPT-1)*NBCMP + ICMP
      IF ( .NOT. ZL(JCESL-1+IAD) ) THEN
         CALL UTDEBM('F','RCMA01','ERREUR DONNEES ')
         CALL UTIMPI('L','POUR LA MAILLE ',1,IMA)
         CALL UTIMPI('S',' ET LE NOEUD ',1,IPT)
         CALL UTIMPK('L','IL MANQUE LE ',1,'NU')
         CALL UTFINM
      ENDIF
      NU = ZR(JCESV-1+IAD)
C
C --- LE MATERIAU : ALPHA
C
      ICMP = 4
      IAD = DECMA + (IPT-1)*NBCMP + ICMP
      IF ( .NOT. ZL(JCESL-1+IAD) ) THEN
         CALL UTDEBM('F','RCMA01','ERREUR DONNEES ')
         CALL UTIMPI('L','POUR LA MAILLE ',1,IMA)
         CALL UTIMPI('S',' ET LE NOEUD ',1,IPT)
         CALL UTIMPK('L','IL MANQUE LE ',1,'ALPHA')
         CALL UTFINM
      ENDIF
      ALPHA = ZR(JCESV-1+IAD)
C
C --- TRAITEMENT DE LA DISCONTINUITE, A GAUCHE ET A DROITE DU NOEUD
C
      EA = 0.D0
      ALPHAA = 0.D0
      EB = 0.D0
      ALPHAB = 0.D0
C
      IF ( NBM .EQ. 1 ) THEN
         EA = E
         EB = E
         ALPHAA = ALPHA
         ALPHAB = ALPHA
      ELSEIF ( NBM .EQ. 2 ) THEN
         EA = E
         ALPHAA = ALPHA
         DO 104 IN = 1 , NBM
            IF ( ADRM(IN) .NE. IMA ) THEN
               IMB =  ADRM(IN)
               GOTO 106
            ENDIF
 104     CONTINUE
         CALL U2MESS('F','POSTRELE_42')
 106     CONTINUE
         DECMB = ZI(JCESD-1+5+4*(IMB-1)+4)
         ICMP = 2
         IAD = DECMB + (IPT-1)*NBCMP + ICMP
         IF ( .NOT. ZL(JCESL-1+IAD) ) THEN
            CALL UTDEBM('F','RCMA01','ERREUR DONNEES ')
            CALL UTIMPI('L','POUR LA MAILLE ',1,IMB)
            CALL UTIMPI('S',' ET LE NOEUD ',1,IPT)
            CALL UTIMPK('L','IL MANQUE LE ',1,'E_B')
            CALL UTFINM
         ENDIF
         EB = ZR(JCESV-1+IAD)
         ICMP = 4
         IAD = DECMB + (IPT-1)*NBCMP + ICMP
         IF ( .NOT. ZL(JCESL-1+IAD) ) THEN
            CALL UTDEBM('F','RCMA01','ERREUR DONNEES ')
            CALL UTIMPI('L','POUR LA MAILLE ',1,IMB)
            CALL UTIMPI('S',' ET LE NOEUD ',1,IPT)
            CALL UTIMPK('L','IL MANQUE LE ',1,'ALPHA_B')
            CALL UTFINM
         ENDIF
         ALPHAB = ZR(JCESV-1+IAD)
      ENDIF
C
      VALE(1) = EC
      VALE(2) = E
      VALE(3) = NU
      VALE(4) = ALPHA
      VALE(5) = EA
      VALE(6) = EB
      VALE(7) = ALPHAA
      VALE(8) = ALPHAB
      VALE(9) = ( EA + EB ) / 2
C
C --- LE MATERIAU : E_REFE
C
      ICMP = 5
      IAD = DECMA + (IPT-1)*NBCMP + ICMP
      IF ( .NOT. ZL(JCESL-1+IAD) ) THEN
         CALL UTDEBM('F','RCMA01','ERREUR DONNEES ')
         CALL UTIMPI('L','POUR LA MAILLE ',1,IMA)
         CALL UTIMPI('S',' ET LE NOEUD ',1,IPT)
         CALL UTIMPK('L','IL MANQUE LE ',1,'E_REFE')
         CALL UTFINM
      ENDIF
      VALE(10) = ZR(JCESV-1+IAD)
C
C --- LE MATERIAU : SM
C
      ICMP = 6
      IAD = DECMA + (IPT-1)*NBCMP + ICMP
      IF ( .NOT. ZL(JCESL-1+IAD) ) THEN
         CALL UTDEBM('F','RCMA01','ERREUR DONNEES ')
         CALL UTIMPI('L','POUR LA MAILLE ',1,IMA)
         CALL UTIMPI('S',' ET LE NOEUD ',1,IPT)
         CALL UTIMPK('L','IL MANQUE LE ',1,'SM')
         CALL UTFINM
      ENDIF
      VALE(11) = ZR(JCESV-1+IAD)
C
C --- LE MATERIAU : M
C
      ICMP = 7
      IAD = DECMA + (IPT-1)*NBCMP + ICMP
      IF ( .NOT. ZL(JCESL-1+IAD) ) THEN
         CALL UTDEBM('F','RCMA01','ERREUR DONNEES ')
         CALL UTIMPI('L','POUR LA MAILLE ',1,IMA)
         CALL UTIMPI('S',' ET LE NOEUD ',1,IPT)
         CALL UTIMPK('L','IL MANQUE LE ',1,'M')
         CALL UTFINM
      ENDIF
      VALE(12) = ZR(JCESV-1+IAD)
C
C --- LE MATERIAU : N
C
      ICMP = 8
      IAD = DECMA + (IPT-1)*NBCMP + ICMP
      IF ( .NOT. ZL(JCESL-1+IAD) ) THEN
         CALL UTDEBM('F','RCMA01','ERREUR DONNEES ')
         CALL UTIMPI('L','POUR LA MAILLE ',1,IMA)
         CALL UTIMPI('S',' ET LE NOEUD ',1,IPT)
         CALL UTIMPK('L','IL MANQUE LE ',1,'N')
         CALL UTFINM
      ENDIF
      VALE(13) = ZR(JCESV-1+IAD)
C
C
C --- TYPE DE KE
C
      ICMP = 9
      IAD = DECMA + (IPT-1)*NBCMP + ICMP
      IF ( .NOT. ZL(JCESL-1+IAD) ) THEN
         CALL UTDEBM('F','RCMA01','ERREUR DONNEES ')
         CALL UTIMPI('L','POUR LA MAILLE ',1,IMA)
         CALL UTIMPI('S',' ET LE NOEUD ',1,IPT)
         CALL UTIMPK('L','IL MANQUE LE ',1,'TYPEKE')
         CALL UTFINM
      ENDIF
      VALE(14) = ZR(JCESV-1+IAD)
C
      CALL JEDEMA( )
      END
