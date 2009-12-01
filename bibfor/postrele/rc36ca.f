      SUBROUTINE RC36CA ( CARAEL, NOMA, NBMA, LISTMA, CHCARA )
      IMPLICIT   NONE
      INTEGER             NBMA, LISTMA(*)
      CHARACTER*8         CARAEL, NOMA
      CHARACTER*24        CHCARA
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 16/02/2009   AUTEUR GALENNE E.GALENNE 
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
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
C
C     TRAITEMENT DU CARA_ELEM
C     ON A BESOIN DES INERTIES (CARGENPO): IY1, IZ1, IY2, IZ2
C                 DU DIAMETRE EXTERIEUR (CARGEOPO) : R1, R2
C                 DE L'EPAISSEUR (CARGEOPO) : EP1, EP2
C
C IN  : CARAEL : CARA_ELEM UTILISATEUR
C IN  : NOMA   : MAILLAGE
C IN  : NBMA   : NOMBRE DE MAILLES D'ANALYSE
C IN  : LISTMA : LISTE DES MAILLES D'ANALYSE
C OUT : CHCARA : CHAM_ELEM DE TYPE ELNO DE CARACTERISTIQUES ELEMENTAIRES
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
      INTEGER      NBCMP, JCNSV, JCNSL, IRET, JCESD, JCESV, 
     +             IM, IMA, NBPT, DECAL, IPT, ICMP, IAD
      REAL*8       IY1, IZ1, R1, EP1, IY2, IZ2, R2, EP2 
      CHARACTER*8  NOMGD
      CHARACTER*16 NOCMP(4)
      CHARACTER*19 K19B, CES
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      NOMGD = 'RCCM_R'
      NBCMP = 4
      NOCMP(1) = 'IY'
      NOCMP(2) = 'IZ'
      NOCMP(3) = 'D'
      NOCMP(4) = 'EP'
C
      CALL RC36ZZ ( NOMA, NOMGD, NBCMP, NOCMP, NBMA, LISTMA, CHCARA )
C
      CALL JEVEUO ( CHCARA(1:19)//'.CESV', 'E', JCNSV ) 
      CALL JEVEUO ( CHCARA(1:19)//'.CESL', 'E', JCNSL )
C
      K19B = CARAEL//'.CARGENPO'
      CES  = '&&RC36CA.CARGENPO'
      CALL CARCES ( K19B, 'ELNO', ' ', 'V', CES, IRET )
C
      NBCMP = 4
      NOCMP(1) = 'IY1'
      NOCMP(2) = 'IZ1'
      NOCMP(3) = 'IY2'
      NOCMP(4) = 'IZ2'
      CALL CESRED ( CES, NBMA, LISTMA, NBCMP, NOCMP, 'V', CES )
C
      CALL JEVEUO ( CES//'.CESD', 'L', JCESD )
      CALL JEVEUO ( CES//'.CESV', 'L', JCESV )
C
      DO 100  IM = 1 , NBMA
         IMA = LISTMA(IM)
         NBPT = ZI(JCESD-1+5+4*(IMA-1)+1)
         DECAL= ZI(JCESD-1+5+4*(IMA-1)+4)
         DO 110 IPT = 1 , NBPT
            IF ( IPT .EQ. 1 ) THEN
               ICMP = 1
               IAD = DECAL + (IPT-1)*NBCMP + ICMP
               IY1 = ZR(JCESV-1+IAD)
               ICMP = 1
               IAD = DECAL + (IPT-1)*NBCMP + ICMP
               ZL(JCNSL-1+IAD) = .TRUE.
               ZR(JCNSV-1+IAD) = IY1
C
               ICMP = 2
               IAD = DECAL + (IPT-1)*NBCMP + ICMP
               IZ1 = ZR(JCESV-1+IAD)
               ICMP = 2
               IAD = DECAL + (IPT-1)*NBCMP + ICMP
               ZL(JCNSL-1+IAD) = .TRUE.
               ZR(JCNSV-1+IAD) = IZ1
            ELSEIF ( IPT .EQ. NBPT ) THEN
               ICMP = 3
               IAD = DECAL + (IPT-1)*NBCMP + ICMP
               IY2 = ZR(JCESV-1+IAD)
               ICMP = 1
               IAD = DECAL + (IPT-1)*NBCMP + ICMP
               ZL(JCNSL-1+IAD) = .TRUE.
               ZR(JCNSV-1+IAD) = IY2
C
               ICMP = 4
               IAD = DECAL + (IPT-1)*NBCMP + ICMP
               IZ2 = ZR(JCESV-1+IAD)
               ICMP = 2
               IAD = DECAL + (IPT-1)*NBCMP + ICMP
               ZL(JCNSL-1+IAD) = .TRUE.
               ZR(JCNSV-1+IAD) = IZ2
            ENDIF
 110     CONTINUE
 100  CONTINUE
C
      CALL DETRSD ( 'CHAM_ELEM_S', CES )
C
      K19B = CARAEL//'.CARGEOPO'
      CES  = '&&RC36CA.CARGEOPO'
      CALL CARCES ( K19B, 'ELNO', ' ', 'V', CES, IRET )
C
      NBCMP = 4
      NOCMP(1) = 'R1'
      NOCMP(2) = 'EP1'
      NOCMP(3) = 'R2'
      NOCMP(4) = 'EP2'
      CALL CESRED ( CES, NBMA, LISTMA, NBCMP, NOCMP, 'V', CES )
C
      CALL JEVEUO ( CES//'.CESD', 'L', JCESD )
      CALL JEVEUO ( CES//'.CESV', 'L', JCESV )
C
      DO 200  IM = 1 , NBMA
         IMA = LISTMA(IM)
         NBPT = ZI(JCESD-1+5+4*(IMA-1)+1)
         DECAL= ZI(JCESD-1+5+4*(IMA-1)+4)
         DO 210 IPT = 1,NBPT
            IF ( IPT .EQ. 1 ) THEN
               ICMP = 1
               IAD = DECAL + (IPT-1)*NBCMP + ICMP
               R1 = ZR(JCESV-1+IAD)
               ICMP = 3
               IAD = DECAL + (IPT-1)*NBCMP + ICMP
               ZL(JCNSL-1+IAD) = .TRUE.
               ZR(JCNSV-1+IAD) = 2 * R1
C
               ICMP = 2
               IAD = DECAL + (IPT-1)*NBCMP + ICMP
               EP1 = ZR(JCESV-1+IAD)
               ICMP = 4
               IAD = DECAL + (IPT-1)*NBCMP + ICMP
               ZL(JCNSL-1+IAD) = .TRUE.
               ZR(JCNSV-1+IAD) = EP1
            ELSEIF ( IPT .EQ. NBPT ) THEN
               ICMP = 3
               IAD = DECAL + (IPT-1)*NBCMP + ICMP
               R2 = ZR(JCESV-1+IAD)
               ICMP = 3
               IAD = DECAL + (IPT-1)*NBCMP + ICMP
               ZL(JCNSL-1+IAD) = .TRUE.
               ZR(JCNSV-1+IAD) = 2 *  R2
C
               ICMP = 4
               IAD = DECAL + (IPT-1)*NBCMP + ICMP
               EP2 = ZR(JCESV-1+IAD)
               ICMP = 4
               IAD = DECAL + (IPT-1)*NBCMP + ICMP
               ZL(JCNSL-1+IAD) = .TRUE.
               ZR(JCNSV-1+IAD) = EP2
            ENDIF
 210     CONTINUE
 200  CONTINUE
C
      CALL DETRSD ( 'CHAM_ELEM_S', CES )
C
      CALL JEDEMA( )
      END
