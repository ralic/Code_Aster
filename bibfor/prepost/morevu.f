      SUBROUTINE MOREVU ( TABPUS, DINST, NBSECT, SECT, VOLTUB, VOLOBS )
      IMPLICIT  NONE
      INTEGER             NBSECT
      REAL*8              DINST, VOLTUB(*), VOLOBS(*), SECT(*)
      CHARACTER*(*)       TABPUS
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C     OPERATEUR  "MODI_OBSTACLE"
C     RECUPERATION DES VOLUMES USES DANS LA TABLE "POST_USURE"
C
C ----------------------------------------------------------------------
C     ---- DEBUT DES COMMUNS JEVEUX ------------------------------------
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
      CHARACTER*24 VALK
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ---- FIN DES COMMUNS JEVEUX --------------------------------------

      INTEGER       IBID, NIS, NBVPU, JINST, NBSS2, JSECT, I, IRET
      INTEGER VALI
      REAL*8        PREC, VOTUB(12), VOOBS(12), SEC(12)
      REAL*8 VALR
      COMPLEX*16    C16B
      CHARACTER*8   K8B, CRIT
      CHARACTER*19  NOMTA
      CHARACTER*16  VALEK(2)
      INTEGER      IARG
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMTA = TABPUS
      PREC  = 1.D-06
      CRIT  = 'RELATIF '
C
C     VERIFICATION DES PARAMETRES DE LA TABLE
      CALL TBEXP2( NOMTA, 'INST')
      CALL TBEXP2( NOMTA, 'SECTEUR')
      CALL TBEXP2( NOMTA, 'V_USUR_TUBE_CUMU')
      CALL TBEXP2( NOMTA, 'V_USUR_OBST_CUMU')
      CALL TBEXP2( NOMTA, 'ANGLE_DEBUT')
C
      CALL GETVR8 ( ' ', 'INST', 1,IARG,1, DINST, NIS )
      IF ( NIS .EQ. 0 ) THEN
         CALL TBEXV1 ( NOMTA, 'INST', '&&MOREVU.INST','V', NBVPU, K8B )
         CALL JEVEUO ('&&MOREVU.INST', 'L', JINST )
         DINST = ZR(JINST+NBVPU-1)
         CALL JEDETR ( '&&MOREVU.INST' )
      ENDIF
C
      CALL TBEXV1 ( NOMTA, 'SECTEUR', '&&MOREVU.SECT','V', NBVPU, K8B )
      CALL JEVEUO ( '&&MOREVU.SECT', 'L', JSECT )
      NBSECT = ZI(JSECT+NBVPU-1)
      IF ( NBSECT.NE.10 .AND. NBSECT.NE.12 ) THEN
         CALL U2MESS('F','PREPOST3_63')
      ENDIF
      NBSS2 = NBSECT / 2
C
      VALEK(1) = 'INST'
      VALEK(2) = 'SECTEUR'
C
      DO 10 I = 1 , NBSECT
C
         CALL TBLIVA ( NOMTA, 2,VALEK, I,DINST,C16B,K8B, CRIT,PREC,
     &             'V_USUR_TUBE_CUMU', K8B,IBID,VOTUB(I),C16B,K8B,IRET)
C                   ----------------
         IF (IRET.NE.0) THEN
            VALR = DINST
            VALK = 'V_USUR_TUBE_CUMU'
            VALI = I
            CALL U2MESG('F', 'PREPOST5_54',1,VALK,1,VALI,1,VALR)
         ENDIF
C
         CALL TBLIVA ( NOMTA, 2,VALEK, I,DINST,C16B,K8B, CRIT,PREC,
     &             'V_USUR_OBST_CUMU', K8B,IBID,VOOBS(I),C16B,K8B,IRET)
C                   ----------------
         IF (IRET.NE.0) THEN
            VALR = DINST
            VALK = 'V_USUR_OBST_CUMU'
            VALI = I
            CALL U2MESG('F', 'PREPOST5_54',1,VALK,1,VALI,1,VALR)
         ENDIF
C
         CALL TBLIVA ( NOMTA, 2,VALEK, I,DINST,C16B,K8B, CRIT,PREC,
     &                    'ANGLE_DEBUT', K8B,IBID,SEC(I),C16B,K8B,IRET)
C                          -----------
         IF (IRET.NE.0) THEN
            VALR = DINST
            VALK = 'ANGLE_DEBUT'
            VALI = I
            CALL U2MESG('F', 'PREPOST5_54',1,VALK,1,VALI,1,VALR)
         ENDIF
C
 10   CONTINUE
C
C --- LES ANGLES ETAIENT COMPRIS ENTRE -180 ET +180 DEGREES
C     ON LES PASSE ENTRE 0 ET 360 DEGRES
C
      DO 20 I = 1 , NBSECT
         IF ( I .LE. NBSS2 ) THEN
            SECT(I)   = SEC(NBSS2+I)
            VOLTUB(I) = VOTUB(NBSS2+I)
            VOLOBS(I) = VOOBS(NBSS2+I)
         ELSE
            SECT(I)   = SEC(I-NBSS2) + 360.D0
            VOLTUB(I) = VOTUB(I-NBSS2)
            VOLOBS(I) = VOOBS(I-NBSS2)
         ENDIF
 20   CONTINUE
C
      CALL JEDETR ( '&&MOREVU.SECT' )
C
      CALL JEDEMA()
      END
