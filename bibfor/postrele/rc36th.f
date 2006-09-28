      SUBROUTINE RC36TH ( NOMA, NBMA, LISTMA ,CHTH, IOCS, NBTHS, LITHS)
      IMPLICIT   NONE
      INTEGER             NBMA, LISTMA(*), IOCS, NBTHS, LITHS(*)
      CHARACTER*8         NOMA
      CHARACTER*24        CHTH(*)
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
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
C     RECUPERATION DES DONNEES DE "RESU_THER"
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
      INTEGER      NBRETH, NBCMP, ITHS, ITH, IRET, N1, N2, IOCC, IERD,
     &             INO, IAD, IN, IMA, IM, JMAIL, JNOEU, NBMAIL, NBMAT,
     &             NBTOU, NBNOEU, JCESD, JCESL, JCESV, NBPT, DECAL,
     &             IPT, JCONX1, JCONX2, ICMP, IT1
      PARAMETER   ( NBCMP = 2 )
      CHARACTER*8  K8B, NOMGD, LICMP(NBCMP), TBTHER, TBMOYE
      CHARACTER*16 MOTCLF, MOTCLS(2), TYPMCS(2), MOTCLN(2), TYPMCN(2)
      CHARACTER*19 CHAMS0
      CHARACTER*24 MESMAI, MESNOE
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      MOTCLF = 'RESU_THER'
      CALL GETFAC ( MOTCLF, NBRETH )
C
      NOMGD = 'RCCM_K'
      LICMP(1) = 'TB_TEMP'
      LICMP(2) = 'TB_MOYE'
C
      MESMAI = 'RC36TH.MES_MAILLES'
      MOTCLS(1) = 'GROUP_MA'
      MOTCLS(2) = 'MAILLE'
      TYPMCS(1) = 'GROUP_MA'
      TYPMCS(2) = 'MAILLE'
      MESNOE = 'RC36TH.MES_NOEUDS'
      MOTCLN(1) = 'GROUP_NO'
      MOTCLN(2) = 'NOEUD'
      TYPMCN(1) = 'GROUP_NO'
      TYPMCN(2) = 'NOEUD'
C
C --- POUR CHAQUE SITUATION, UN CHAMELEM ELNO EST CREE
C
C     PAS DE SURCHARGE AUTORISEE POUR REMPLIR CE CHAMP
C
      CALL CODENT ( IOCS , 'D0', K8B)
      CHAMS0 = 'RC36TH.CHAM'//K8B
      CALL JEEXIN ( CHAMS0, IRET )
      IF ( IRET .EQ. 0 ) THEN
         CALL CESCRE ( 'V', CHAMS0, 'ELNO', NOMA, NOMGD,NBCMP,LICMP,
     &         -1, -1, -NBCMP )
         CHTH(IOCS) = CHAMS0
      ELSE
         CALL U2MESS('F','POSTRELE_41')
      ENDIF
C
      CALL JEVEUO (CHAMS0//'.CESD', 'L', JCESD )
      CALL JEVEUO (CHAMS0//'.CESL', 'E', JCESL )
      CALL JEVEUO (CHAMS0//'.CESV', 'E', JCESV )
C
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMAT,K8B,IERD)
      CALL JEVEUO(NOMA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',JCONX2)
C
      DO 5, IT1 = 1, NBTHS, 1
C
         DO 10, IOCC = 1, NBRETH, 1
C
            CALL GETVIS ( MOTCLF, 'NUME_RESU_THER', IOCC,1,1,ITH,N1)
C
            ITHS = LITHS(IT1)
            IF (ITH.EQ.ITHS) GOTO 6
10       CONTINUE
         CALL UTDEBM('F','RC36TH','ERREUR DONNEES ')
         CALL UTIMPI('L','POUR LA SITUATION NUMERO ',1,IOCS)
         CALL UTIMPI('L','ON N''A PAS PU RECUPERER LE "RESU_THER"'//
     &                   ' CORRESPONDANT AU NUMERO ',1,ITHS)
         CALL UTFINM
6        CONTINUE

         CALL GETVID ( MOTCLF,'TABL_RESU_THER' ,IOCC,1,1,TBTHER,N1)
C
         CALL GETVID ( MOTCLF,'TABL_MOYE_THER' ,IOCC,1,1,TBMOYE,N1)
C
         CALL GETVTX ( MOTCLF, 'TOUT', IOCC, 1, 1, K8B, NBTOU )

         IF ( NBTOU .NE. 0 ) THEN
            NBMAIL = NBMAT
            CALL WKVECT ( MESMAI, 'V V I' , NBMAIL, JMAIL )
            DO 12 IMA = 1 , NBMAIL
               ZI(JMAIL+IMA-1) = IMA
 12         CONTINUE
         ELSE
            CALL RELIEM ( ' ', NOMA, 'NU_MAILLE', MOTCLF, IOCC, 2,
     &                                 MOTCLS, TYPMCS, MESMAI, NBMAIL )
            CALL JEVEUO ( MESMAI, 'L', JMAIL )
         ENDIF
C
         CALL GETVID ( MOTCLF, 'GROUP_NO', IOCC,1,0, K8B, N1)
         CALL GETVID ( MOTCLF, 'NOEUD'   , IOCC,1,0, K8B, N2)
         IF ( N1+N2 .NE. 0 ) THEN
            CALL RELIEM ( ' ', NOMA, 'NU_NOEUD', MOTCLF, IOCC, 2,
     &                                MOTCLN, TYPMCN, MESNOE, NBNOEU )
            CALL JEVEUO ( MESNOE, 'L', JNOEU )
         ELSE
            NBNOEU = 0
         ENDIF

         IF ( NBNOEU .EQ. 0 ) THEN
            DO 100  IM = 1 , NBMAIL
               IMA = ZI(JMAIL+IM-1)
               NBPT = ZI(JCESD-1+5+4*(IMA-1)+1)
               DECAL= ZI(JCESD-1+5+4*(IMA-1)+4)
               DO 110,IPT = 1,NBPT
                  ICMP = 1
                  IAD = DECAL + (IPT-1)*NBCMP + ICMP
                  IF (.NOT.ZL(JCESL-1+IAD)) THEN
                     ZL (JCESL-1+IAD) = .TRUE.
                     ZK8(JCESV-1+IAD) = TBTHER
                  ELSE
                     CALL UTDEBM('F','RC36TH','ERREUR DONNEES ')
                     CALL UTIMPI('L','POUR LA SITUATION NUMERO ',1,IOCS)
                     CALL UTIMPI('L','SUR LA MAILLE NUMERO ',1,IMA)
                     CALL UTIMPI('L','IL Y A PLUSIEURS RESU_THER',1,0)
                     CALL UTFINM
                  ENDIF
                  ICMP = 2
                  IAD = DECAL + (IPT-1)*NBCMP + ICMP
                  ZL (JCESL-1+IAD) = .TRUE.
                  ZK8(JCESV-1+IAD) = TBMOYE
 110           CONTINUE
 100        CONTINUE
         ELSE
            DO 200  IM = 1 , NBMAIL
               IMA = ZI(JMAIL+IM-1)
               NBPT = ZI(JCESD-1+5+4*(IMA-1)+1)
               DECAL= ZI(JCESD-1+5+4*(IMA-1)+4)
               DO 210,IPT = 1,NBPT
                  INO = ZI(JCONX1-1+ZI(JCONX2+IMA-1)+IPT-1)
                  DO 220,IN = 1,NBNOEU
                     IF ( ZI(JNOEU+IN-1) .EQ. INO ) THEN
                        ICMP = 1
                        IAD = DECAL + (IPT-1)*NBCMP + ICMP
                        IF (.NOT.ZL(JCESL-1+IAD)) THEN
                           ZL (JCESL-1+IAD) = .TRUE.
                           ZK8(JCESV-1+IAD) = TBTHER
                        ELSE
                           CALL UTDEBM('F','RC36TH','ERREUR DONNEES')
                           CALL UTIMPI('L','SITUATION NUMERO ',1,IOCS)
                           CALL UTIMPI('L','MAILLE NUMERO ',1,IMA)
                           CALL UTIMPI('L','NOEUD NUMERO ',1,INO)
                           CALL UTIMPI('L','PLUSIEURS RESU_THER',1,0)
                           CALL UTFINM
                        ENDIF
                        ICMP = 2
                        IAD = DECAL + (IPT-1)*NBCMP + ICMP
                        ZL (JCESL-1+IAD) = .TRUE.
                        ZK8(JCESV-1+IAD) = TBMOYE
                        GOTO 210
                     ENDIF
 220              CONTINUE
 210           CONTINUE
 200        CONTINUE
         ENDIF
C
 20      CONTINUE
C
         CALL JEDETR ( MESMAI )
         CALL JEDETR ( MESNOE )
C
 5    CONTINUE

C      CALL INFNIV ( IFM, NIV )
C      IF ( NIV .GE. 2 ) THEN
C         WRITE(IFM,*)' SITUATION NUMERO ', IOCS
C         WRITE(IFM,*)' CHAMP THERMIQUE ',CHTH(IOCS)
C         CALL CESIMP ( CHTH(IOCS), IFM, 0, IBID )
C      ENDIF

C    VERIF QUE TOUTES LES MAILLES ANALYSEES SONT AFFECTEES

      DO 7 IM = 1 , NBMA
         IMA = LISTMA(IM)
         NBPT = ZI(JCESD-1+5+4*(IMA-1)+1)
         DECAL= ZI(JCESD-1+5+4*(IMA-1)+4)
         DO 8,IPT = 1,NBPT
            DO 9 ICMP=1,2
               IAD = DECAL + (IPT-1)*NBCMP + ICMP
               IF (.NOT.ZL(JCESL-1+IAD)) THEN
                  CALL UTDEBM('F','RC36TH','ERREUR DONNEES ')
                  CALL UTIMPI('L','POUR LA SITUATION NUMERO ',1,IOCS)
                  CALL UTIMPI('L','SUR LA MAILLE NUMERO ',1,IMA)
                  CALL UTIMPI('L','AUCUN RESU_THER',1,0)
                  CALL UTFINM
               ENDIF
 9          CONTINUE
 8       CONTINUE
 7    CONTINUE
C
      CALL JEDEMA( )
      END
