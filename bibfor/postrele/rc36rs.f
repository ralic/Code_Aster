      SUBROUTINE RC36RS ( NOMRES, NOMA, NBMA, LISTMA, CHINDI, CHRESU )
      IMPLICIT   NONE
      INTEGER             NBMA, LISTMA(*)
      CHARACTER*8         NOMRES, NOMA
      CHARACTER*24        CHINDI, CHRESU
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
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
C
C     TRANSFERT DE CHRESU DANS LA TABLE 
C
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
      CHARACTER*32     JEXNOM, JEXNUM, JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       IBID, JCESD, JCESV, IM, IMA, DECAL, NBCMP, NBPT, 
     +              IPT, INO, JCONX1, JCONX2, JCINV, JCIND, NPARA,
     +              NBCIN, DECIN, ICMP, IAD
      PARAMETER   ( NPARA = 8 )
      REAL*8        VALER(5), TYPE
      COMPLEX*16    C16B
      CHARACTER*8   VALEK(3), TYPARA(NPARA)
      CHARACTER*16  NOPARA(NPARA)
      CHARACTER*24  CONNEX, NOMMAI, NOMNOE
C     ------------------------------------------------------------------
      DATA NOPARA / 'MAILLE', 'TYPE_MAILLE', 'NOEUD' ,  'SM', 
     +              'SN_MAX', 'SN/3SM', 'SALT_MAX' , 'FACT_USAGE_CUMU' /
      DATA TYPARA / 'K8'    , 'K8'    , 'K8'       , 'R' ,
     +              'R'     , 'R'     , 'R'        , 'R' /
C DEB ------------------------------------------------------------------
C
      CALL TBCRSD ( NOMRES, 'G' )
      CALL TBAJPA ( NOMRES, NPARA, NOPARA, TYPARA )
C
      NOMMAI = NOMA//'.NOMMAI         '
      NOMNOE = NOMA//'.NOMNOE         '
      CONNEX = NOMA//'.CONNEX         '
      CALL JEVEUO ( CONNEX, 'L', JCONX1 )
      CALL JEVEUO ( JEXATR(CONNEX,'LONCUM'), 'L', JCONX2 )
C
C --- LE CHAMP INDICE DE CONTRAINTES
C
      CALL JEVEUO ( CHINDI(1:19)//'.CESV', 'L', JCINV )
      CALL JEVEUO ( CHINDI(1:19)//'.CESD', 'L', JCIND )
      NBCIN = ZI(JCIND-1+2)
C
C --- LE CHAM_ELEM RESULTAT
C
      CALL JEVEUO ( CHRESU(1:19)//'.CESD', 'L', JCESD ) 
      CALL JEVEUO ( CHRESU(1:19)//'.CESV', 'L', JCESV ) 
      NBCMP = ZI(JCESD-1+2)
C
      DO 10 IM = 1 , NBMA
C
        IMA   = LISTMA(IM)
        CALL JENUNO ( JEXNUM(NOMMAI,IMA), VALEK(1) )
C
        NBPT  = ZI(JCESD-1+5+4*(IMA-1)+1)
        DECAL = ZI(JCESD-1+5+4*(IMA-1)+4)
        DECIN = ZI(JCIND-1+5+4*(IMA-1)+4)
C
        DO 20  IPT = 1 , NBPT
C
           ICMP = 7
           IAD = DECIN + (IPT-1)*NBCIN + ICMP
           TYPE = ZR(JCINV-1+IAD)
           IF ( TYPE .EQ. 0.D0 ) THEN
              VALEK(2) = '???'
           ELSEIF ( TYPE .EQ. 10.D0 ) THEN
              VALEK(2) = 'DRO'
           ELSEIF ( TYPE .EQ. 20.D0 ) THEN
              VALEK(2) = 'COU'
           ELSEIF ( TYPE .EQ. 30.D0 ) THEN
              VALEK(2) = 'TRN'
           ELSEIF ( TYPE .EQ. 40.D0 ) THEN
              VALEK(2) = 'TEE'
           ENDIF
C
           INO = ZI(JCONX1-1+ZI(JCONX2+IMA-1)+IPT-1)
           CALL JENUNO ( JEXNUM(NOMNOE,INO), VALEK(3) )
C
           DO 30  ICMP = 1 , NBCMP

              IAD = DECAL + (IPT-1)*NBCMP + ICMP
              VALER(ICMP) = ZR(JCESV-1+IAD)
C
 30        CONTINUE
C
           CALL TBAJLI ( NOMRES, NPARA, NOPARA, IBID, VALER,
     +                                                C16B, VALEK, 0 )
C
 20     CONTINUE
C
 10   CONTINUE
C
      END
