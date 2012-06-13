      SUBROUTINE CRPERM()
      IMPLICIT   NONE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C     COMMANDE:  CREA_RESU
C     TRAITEMENT DU MOT CLE FACTEUR "PERM_CHAM"
C
C ----------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'
      INTEGER VALI
C
C
      INTEGER        N1, NBCHAM, IORD1,IORD2, NBPERM, JORDR, NBTROU, IP,
     &      IBID, IC, IRET, JLIM1, JLIM2, NBMA, JLINO, NBNO2,NNCP
      REAL*8         INST1, TRAN(3), PREC
      REAL*8 VALR
      COMPLEX*16     CBID
      CHARACTER*8    K8B, CRIT, RESU1, RESU2, RESU3, MA1, MA2,
     &               GMA1, GMA2
      CHARACTER*16   TYPRES, NOMCMD, CHAM(4), OPTION
      CHARACTER*24 VALK(2)
      CHARACTER*19   PRCHNO
      CHARACTER*24   CH1, CH2, CHS1, CHS2, LINOEU,
     &               LIMA1, LIMA2, LIGREL, CHSI1(4), CHSI2(4)
      INTEGER      IARG
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL GETRES ( RESU3, TYPRES, NOMCMD )
C
C --- RECUPERATION DES DONNEES UTILISATEUR :
C     ------------------------------------
C
      CALL GETVID ( ' ', 'RESU_INIT'    , 1,IARG,1, RESU1, N1 )
      CALL GETVR8 ( ' ', 'INST_INIT'    , 1,IARG,1, INST1, N1 )
      IF ( N1 .EQ. 0 ) THEN
         CALL JELIRA ( RESU1//'           .ORDR', 'LONUTI', IBID, K8B)
         CALL JEVEUO ( RESU1//'           .ORDR', 'L', JORDR )
         IORD1 = ZI(JORDR+IBID-1)
      ELSE
         CALL GETVR8 ( ' ', 'PRECISION' , 1,IARG,1, PREC, N1)
         CALL GETVTX ( ' ', 'CRITERE'   , 1,IARG,1, CRIT, N1)
         CALL RSORAC ( RESU1, 'INST', IBID, INST1, K8B, CBID, PREC,
     &                 CRIT, IORD1, 1, NBTROU )
         IF ( NBTROU .EQ. 0 ) THEN
            VALR = INST1
            VALK (1) = RESU1
            CALL U2MESG('F', 'CALCULEL5_70',1,VALK,0,0,1,VALR)
         ELSEIF ( NBTROU .NE. 1 ) THEN
            VALR = INST1
            CALL U2MESG('F', 'CALCULEL5_71',0,' ',0,0,1,VALR)
         ENDIF
      ENDIF
      CALL GETVID ( ' ', 'MAILLAGE_INIT'  , 1,IARG,1, MA1  , N1 )
      CALL GETVID ( ' ', 'RESU_FINAL'     , 1,IARG,1, RESU2, N1 )
      CALL GETVID ( ' ', 'MAILLAGE_FINAL' , 1,IARG,1, MA2  , N1 )
      CALL GETVTX ( ' ', 'NOM_CHAM'       , 1,IARG,0, K8B  , N1 )
      IF ( N1 .EQ. 0 ) THEN
         NBCHAM = 3
         CHAM(1) = 'DEPL'
         CHAM(2) = 'SIEF_ELGA'
         CHAM(3) = 'VARI_ELGA'
      ELSE
         NBCHAM = -N1
         CALL GETVTX ( ' ', 'NOM_CHAM', 1,IARG,NBCHAM, CHAM, N1 )

      ENDIF
C
      CALL DISMOI('F','NB_NO_MAILLA',MA2,'MAILLAGE',NBNO2,K8B,IBID)
      IORD2 = 1
C
C --- VERIFICATIONS SUPPLEMENTAIRES :
C     -----------------------------
C
      IF ( RESU2 .NE. RESU3 ) THEN
            VALK (1) = RESU3
            VALK (2) = RESU2
         CALL U2MESG('F', 'CALCULEL5_72',2,VALK,0,0,0,0.D0)
      ENDIF
C
      CALL JELIRA ( RESU2//'           .ORDR', 'LONUTI', IBID, K8B)
      IF ( IBID .NE. 1 ) THEN
            VALK (1) = RESU2
            VALK (2) = K8B
         CALL U2MESG('F', 'CALCULEL5_73',2,VALK,0,0,0,0.D0)
      ENDIF
C
      DO 100 IC = 1 , NBCHAM
         CALL RSEXCH ( RESU1, CHAM(IC), IORD1, CH1, IRET )
         IF ( IRET .NE. 0 ) THEN
            VALK (1) = CHAM(IC)
            VALK (2) = RESU1
            VALI = IORD1
            CALL U2MESG('F', 'CALCULEL5_74',2,VALK,1,VALI,0,0.D0)
         ENDIF
         CALL RSEXCH ( RESU2, CHAM(IC), IORD2, CH2, IRET )
         IF ( IRET .NE. 0 ) THEN
            VALK (1) = CHAM(IC)
            VALK (2) = RESU2
            VALI = IORD2
            CALL U2MESG('F', 'CALCULEL5_74',2,VALK,1,VALI,0,0.D0)
         ENDIF
C
         IF ( CHAM(IC) .EQ. 'DEPL' ) THEN
            CHS1 = '&&CRPERM.DEPL_1'
            CALL CNOCNS ( CH1, 'V', CHS1 )
            CHSI1(IC) = CHS1
            CHS2 = '&&CRPERM.DEPL_2'
            CALL CNOCNS ( CH2, 'V', CHS2 )
            CHSI2(IC) = CHS2
         ELSEIF ( CHAM(IC) .EQ. 'SIEF_ELGA' ) THEN
            CHS1 = '&&CRPERM.SIEF_1'
            CALL CELCES ( CH1, 'V', CHS1 )
            CHSI1(IC) = CHS1
            CHS2 = '&&CRPERM.SIEF_2'
            CALL CELCES ( CH2, 'V', CHS2 )
            CHSI2(IC) = CHS2
         ELSEIF ( CHAM(IC) .EQ. 'VARI_ELGA' ) THEN
            CHS1 = '&&CRPERM.VARI_1'
            CALL CELCES ( CH1, 'V', CHS1 )
            CHSI1(IC) = CHS1
            CHS2 = '&&CRPERM.VARI_2'
            CALL CELCES ( CH2, 'V', CHS2 )
            CHSI2(IC) = CHS2
         ELSEIF ( CHAM(IC) .EQ. 'STRX_ELGA' ) THEN
            CHS1 = '&&CRPERM.STRX_1'
            CALL CELCES ( CH1, 'V', CHS1 )
            CHSI1(IC) = CHS1
            CHS2 = '&&CRPERM.STRX_2'
            CALL CELCES ( CH2, 'V', CHS2 )
            CHSI2(IC) = CHS2
         ENDIF
C
 100  CONTINUE
C
C
      LINOEU = '&&CRPERM.LISTE_NOEU'
      LIMA1  = '&&CRPERM.LISTE_MA_1'
      LIMA2  = '&&CRPERM.LISTE_MA_2'
C
      CALL GETFAC ( 'PERM_CHAM' , NBPERM )
C
C --- BOUCLE SUR LES TRANSLATIONS A EFFECTUER :
C     ---------------------------------------
C
      DO 10 IP = 1 , NBPERM
C
         CALL GETVEM ( MA1, 'GROUP_MA', 'PERM_CHAM', 'GROUP_MA_INIT',
     &                 IP,IARG,1, GMA1, N1 )
         CALL GETVEM ( MA2, 'GROUP_MA', 'PERM_CHAM', 'GROUP_MA_FINAL',
     &                 IP,IARG,1, GMA2, N1 )
C
         CALL GETVR8 ( 'PERM_CHAM', 'TRAN'      , IP,IARG,3, TRAN, N1)
         CALL GETVR8 ( 'PERM_CHAM', 'PRECISION' , IP,IARG,1, PREC, N1)
C
C ------ VERIFICATION DES GROUPES DE MAILLES FOURNIES :
C        --------------------------------------------
C
         CALL WKVECT ( LINOEU, 'V V I', NBNO2, JLINO )
C
         CALL CRPCVG ( MA1, MA2, GMA1, GMA2, TRAN, PREC,
     &                                       LIMA1, LIMA2, ZI(JLINO) )
C
         CALL JELIRA ( LIMA1, 'LONMAX', NBMA, K8B )
         CALL JEVEUO ( LIMA1, 'L', JLIM1 )
         CALL JEVEUO ( LIMA2, 'L', JLIM2 )
C
         DO 20 IC = 1 , NBCHAM
C
            CHS1 = CHSI1(IC)
            CHS2 = CHSI2(IC)
C
C --------- ON TRANSFERE LES VALEURS DE 1 VERS 2 :
C           ------------------------------------
C
            IF ( CHAM(IC) .EQ. 'DEPL' ) THEN
               CALL CNTRAN ( ZI(JLINO), NBNO2, CHS1, CHS2 )
            ELSE
               CALL CETRAN ( ZI(JLIM1), ZI(JLIM2), NBMA, CHS1, CHS2 )

            ENDIF
C
 20      CONTINUE
C
         CALL JEDETR ( LIMA1 )
         CALL JEDETR ( LIMA2 )
         CALL JEDETR ( LINOEU )
C
 10   CONTINUE
C
      DO 110 IC = 1 , NBCHAM
         CALL RSEXCH ( RESU2, CHAM(IC), IORD2, CH2, IRET )
         CHS1 = CHSI1(IC)
         CHS2 = CHSI2(IC)
         IF ( CHAM(IC) .EQ. 'DEPL' ) THEN
            CALL DISMOI ( 'F', 'PROF_CHNO', CH2, 'CHAM_NO',
     &                                            IBID, PRCHNO, IBID )
            CALL CNSCNO ( CHS2, PRCHNO,'NON','G',CH2,'F',IBID)
            CALL DETRSD ( 'CHAM_NO_S', CHS1 )
            CALL DETRSD ( 'CHAM_NO_S', CHS2 )
         ELSE
            CALL DISMOI ( 'F', 'NOM_LIGREL', CH2, 'CHAM_ELEM',
     &                                             IBID, LIGREL, IBID )
            CALL DISMOI ( 'F', 'NOM_OPTION', CH2, 'CHAM_ELEM',
     &                                             IBID, OPTION, IBID )
            CALL CESCEL(CHS2,LIGREL,OPTION,' ','OUI',NNCP,'G',CH2,'F',
     &                  IBID)
            CALL DETRSD ( 'CHAM_ELEM_S', CHS1 )
            CALL DETRSD ( 'CHAM_ELEM_S', CHS2 )
         ENDIF
 110  CONTINUE
C
      CALL JEDEMA()
      END
