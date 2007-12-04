      SUBROUTINE RC36IN ( NOMA, NBMA, LISTMA, CHINDI )
      IMPLICIT   NONE
      INTEGER             NBMA, LISTMA(*)
      CHARACTER*8         NOMA
      CHARACTER*24        CHINDI
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
C     RECUPERATION DES DONNEES DE "INDI_SIGM"
C
C IN  : NOMA   : MAILLAGE
C IN  : NBMA   : NOMBRE DE MAILLES D'ANALYSE
C IN  : LISTMA : LISTE DES MAILLES D'ANALYSE
C OUT : CHINDI : CHAM_ELEM DE TYPE ELNO D'INDICES DE CONTRAINTES
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
      CHARACTER*32     JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      N1, N2, NBINDI, IOCC, NBCMP, DECAL, IPT, ICMP, IAD,
     +             NBPT, JCESD, JCESV, JCONX1, JCONX2, IN, IM, IMA, 
     +             INO, NBNOEU, JNOEU, NBMAIL, JMAIL, NBTOU, IM1
      PARAMETER  ( NBCMP = 7 )
      REAL*8       VALE(NBCMP)
      CHARACTER*8  K8B,  NOMGD, TYPE
      CHARACTER*8  MOTCLS(2), TYPMCS(2), MOTCLN(2), TYPMCN(2)
      CHARACTER*16 MOTCLF, NOCMP(NBCMP)
      CHARACTER*24 MESMAI, MESNOE
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      MOTCLF = 'INDI_SIGM'
C
      MESMAI = 'RC36IN.MES_MAILLES'
      MOTCLS(1) = 'GROUP_MA'
      MOTCLS(2) = 'MAILLE'
      TYPMCS(1) = 'GROUP_MA'
      TYPMCS(2) = 'MAILLE'  
      MESNOE = 'RC36IN.MES_NOEUDS'
      MOTCLN(1) = 'GROUP_NO'
      MOTCLN(2) = 'NOEUD'
      TYPMCN(1) = 'GROUP_NO'
      TYPMCN(2) = 'NOEUD'  
C
      CALL GETFAC ( MOTCLF, NBINDI )
C
      NOMGD = 'RCCM_R'
      NOCMP(1) = 'C1'
      NOCMP(2) = 'C2'
      NOCMP(3) = 'C3'
      NOCMP(4) = 'K1'
      NOCMP(5) = 'K2'
      NOCMP(6) = 'K3'
      NOCMP(7) = 'TYPE'
C
      CALL RC36ZZ ( NOMA, NOMGD, NBCMP, NOCMP, NBMA, LISTMA, CHINDI )
C
      CALL JEVEUO ( CHINDI(1:19)//'.CESD', 'L', JCESD )
      CALL JEVEUO ( CHINDI(1:19)//'.CESV', 'E', JCESV )
C
      CALL JEVEUO ( NOMA//'.CONNEX','L', JCONX1 )
      CALL JEVEUO ( JEXATR(NOMA//'.CONNEX','LONCUM'), 'L', JCONX2 )
C
      DO 10, IOCC = 1, NBINDI, 1
C
         CALL GETVR8 ( MOTCLF, 'C1', IOCC,1,1, VALE(1), N1)
         CALL GETVR8 ( MOTCLF, 'C2', IOCC,1,1, VALE(2), N1)
         CALL GETVR8 ( MOTCLF, 'C3', IOCC,1,1, VALE(3), N1)
         CALL GETVR8 ( MOTCLF, 'K1', IOCC,1,1, VALE(4), N1)
         CALL GETVR8 ( MOTCLF, 'K2', IOCC,1,1, VALE(5), N1)
         CALL GETVR8 ( MOTCLF, 'K3', IOCC,1,1, VALE(6), N1)
C
         CALL GETVTX ( MOTCLF, 'TYPE_ELEM_STANDARD', IOCC,1,1, TYPE, N1)
         IF ( N1 .EQ. 0 ) THEN
            VALE(7) = 0.D0
         ELSE
            IF ( TYPE(1:3).EQ.'DRO' ) VALE(7) = 10.D0
            IF ( TYPE(1:3).EQ.'COU' ) VALE(7) = 20.D0
            IF ( TYPE(1:3).EQ.'TRN' ) VALE(7) = 30.D0
            IF ( TYPE(1:3).EQ.'TEE' ) VALE(7) = 40.D0
         ENDIF
C
         CALL GETVID ( MOTCLF, 'GROUP_NO', IOCC,1,0, K8B, N1)
         CALL GETVID ( MOTCLF, 'NOEUD'   , IOCC,1,0, K8B, N2)
         IF ( N1+N2 .NE. 0 ) THEN
            CALL RELIEM ( ' ', NOMA, 'NU_NOEUD', MOTCLF, IOCC, 2, 
     +                                MOTCLN, TYPMCN, MESNOE, NBNOEU )
            CALL JEVEUO ( MESNOE, 'L', JNOEU )
         ELSE
            NBNOEU = 0
         ENDIF
C
         CALL GETVTX ( MOTCLF, 'TOUT', IOCC, 1, 1, K8B, NBTOU )
         IF ( NBTOU .NE. 0 ) THEN
            DO 100  IM = 1 , NBMA
               IMA = LISTMA(IM)
               NBPT = ZI(JCESD-1+5+4*(IMA-1)+1)
               DECAL= ZI(JCESD-1+5+4*(IMA-1)+4)
               DO 110,IPT = 1,NBPT
                  DO 120,ICMP = 1,NBCMP
                     IAD = DECAL + (IPT-1)*NBCMP + ICMP
                     ZR(JCESV-1+IAD) = VALE(ICMP)
 120              CONTINUE
 110           CONTINUE
 100        CONTINUE
C
         ELSE
            CALL RELIEM ( ' ', NOMA, 'NU_MAILLE', MOTCLF, IOCC, 2, 
     +                                MOTCLS, TYPMCS, MESMAI, NBMAIL )
            CALL JEVEUO ( MESMAI, 'L', JMAIL )
C
            IF ( NBNOEU .EQ. 0 ) THEN
               DO 200  IM = 1 , NBMAIL
                  IMA = ZI(JMAIL+IM-1)
                  DO 202 IM1 = 1 , NBMA
                     IF ( LISTMA(IM1) .EQ. IMA ) GOTO 204
 202              CONTINUE
                  GOTO 200
 204              CONTINUE
                  NBPT = ZI(JCESD-1+5+4*(IMA-1)+1)
                  DECAL= ZI(JCESD-1+5+4*(IMA-1)+4)
                  DO 210,IPT = 1,NBPT
                     INO = ZI(JCONX1-1+ZI(JCONX2+IMA-1)+IPT-1)
                     DO 220,ICMP = 1,NBCMP
                        IAD = DECAL + (IPT-1)*NBCMP + ICMP
                        ZR(JCESV-1+IAD) = VALE(ICMP)
 220                 CONTINUE
 210              CONTINUE
 200           CONTINUE
            ELSE
               DO 300  IM = 1 , NBMAIL
                  IMA = ZI(JMAIL+IM-1)
                  DO 302 IM1 = 1 , NBMA
                     IF ( LISTMA(IM1) .EQ. IMA ) GOTO 304
 302              CONTINUE
                  GOTO 300
 304              CONTINUE
                  NBPT = ZI(JCESD-1+5+4*(IMA-1)+1)
                  DECAL= ZI(JCESD-1+5+4*(IMA-1)+4)
                  DO 310,IPT = 1,NBPT
                     INO = ZI(JCONX1-1+ZI(JCONX2+IMA-1)+IPT-1)
                     DO 320,IN = 1,NBNOEU
                        IF ( ZI(JNOEU+IN-1) .EQ. INO ) THEN
                           DO 330,ICMP = 1,NBCMP
                              IAD = DECAL + (IPT-1)*NBCMP + ICMP
                              ZR(JCESV-1+IAD) = VALE(ICMP)
 330                       CONTINUE
                           GOTO 310
                        ENDIF
 320                 CONTINUE
 310              CONTINUE
 300           CONTINUE
               CALL JEDETR ( MESNOE )
            ENDIF
            CALL JEDETR ( MESMAI )
         ENDIF
C
 10   CONTINUE
C
      CALL JEDEMA( )
      END
