      SUBROUTINE RCMO01 ( CHMOME, IMA, IPT, VALE )
      IMPLICIT   NONE
      INTEGER             IMA, IPT
      REAL*8              VALE(*)
      CHARACTER*24        CHMOME
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 11/09/2002   AUTEUR VABHHTS J.PELLET 
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
C IN  : CHMOME : CHAM_ELEM DE MOMENT RESULTANT
C IN  : IMA    : NUMERO DE LA MAILLE
C IN  : IPT    : NUMERO DU NOEUD DE LA MAILLE
C OUT : VALE   : MOMENT RESULTANT
C                VALE(1) = MX
C                VALE(2) = MY
C                VALE(3) = MZ
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
      INTEGER    JCESV, JCESD, JCESL, NBCMP, DECAL, ICMP, IAD
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
C --- LE CHAMP MOMENT
C
      CALL JEVEUO(CHMOME(1:19)//'.CESV', 'L', JCESV )
      CALL JEVEUO(CHMOME(1:19)//'.CESD', 'L', JCESD )
      CALL JEVEUO(CHMOME(1:19)//'.CESL', 'L', JCESL )
      NBCMP = ZI(JCESD-1+2)
      DECAL = ZI(JCESD-1+5+4*(IMA-1)+4)
C
C --- LES VALEURS DES COMPOSANTES
C
      DO 10 ICMP = 1 , 3
         IAD = DECAL + (IPT-1)*NBCMP + ICMP
         IF ( .NOT. ZL(JCESL-1+IAD) ) THEN
            CALL UTDEBM('F','RCMO01','ERREUR DONNEES ')
            CALL UTIMPI('L','POUR LA MAILLE ',1,IMA)
            CALL UTIMPI('S',' ET LE NOEUD ',1,IPT)
            CALL UTIMPK('L','IL MANQUE UN ',1,'MOMENT')
            CALL UTFINM
         ENDIF
         VALE(ICMP) = ZR(JCESV-1+IAD)
 10   CONTINUE
C
      CALL JEDEMA( )
      END
