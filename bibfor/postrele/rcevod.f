      SUBROUTINE RCEVOD ( CSIGM, CINST, CNOC, SM, LFATIG, LPMPB, LSN,
     +                    CSNO, CSNE, FLEXIO, CSNEO, CSNEE, CFAO,
     +                    CFAE, CSPO, CSPE, CRESU, KINTI, IT, JT )
      IMPLICIT   NONE
      INTEGER      IT, JT
      REAL*8       SM
      LOGICAL      LFATIG, LPMPB, LSN, FLEXIO
      CHARACTER*16 KINTI
      CHARACTER*24 CSIGM, CINST, CNOC, CSNO, CSNE, CSNEO, CSNEE,
     +             CFAO, CFAE, CSPO, CSPE, CRESU
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 08/02/2005   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     OPERATEUR POST_RCCM, TYPE_RESU_MECA='EVOLUTION'
C     TYPE_RESU = 'DETAILS'
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
      CHARACTER*32     JEXNOM, JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      NCMP, JSIGM, JINST, NBINST, JSNO, JSNE, N1, 
     +             IND, I1, I2, ICMP, L1, L2, NPARA, IK, IR, I, 
     +             VAIO(5), VAIE(5), IOO1, IOO2, IOE1, IOE2, NPAR1,
     +             JSNEO, JSNEE, JSPO, JSPE, JFAO, JFAE, JNOC, JRESU
      PARAMETER  ( NCMP = 6 )
      REAL*8       TPM(NCMP), TPB(NCMP), TPMPBO(NCMP), TPMPBE(NCMP),
     +             DCO, DCE, EQUI(NCMP), VALO(36), VALE(36)
      COMPLEX*16   C16B
      CHARACTER*8  K8B, NOMRES, RESU, RESUTH, TYPARA(36)
      CHARACTER*16 NOMCMD, CONCEP, NOPARA(36), VAKO(5), VAKE(5)
C
      INTEGER      NPAREN, NPARPM, NPARSN, NPARSE, NPARF1, NPARF2
      PARAMETER  ( NPAREN=4, NPARPM=5, NPARSN=5, NPARSE=1,
     +             NPARF1=14, NPARF2=13 )
      CHARACTER*8  TYPAEN(NPAREN), TYPAPM(NPARPM), TYPASN(NPARSN),
     +             TYPASE(NPARSE), TYPAF1(NPARF1), TYPAF2(NPARF2)
      CHARACTER*16 NOPAEN(NPAREN), NOPAPM(NPARPM), NOPASN(NPARSN),
     +             NOPASE(NPARSE), NOPAF1(NPARF1), NOPAF2(NPARF2)
C
      DATA NOPAEN / 'INTITULE', 'LIEU', 'SM', '3SM' /
      DATA TYPAEN / 'K16',      'K8'  , 'R' , 'R'   / 
      DATA NOPAPM / 'TABL_RESU', 'INST', 'PM', 'PB', 'PMB' /
      DATA TYPAPM / 'K8'       , 'R'   , 'R' , 'R' , 'R'   /
      DATA NOPASN / 'TABL_RESU_1', 'INST_1', 
     +              'TABL_RESU_2', 'INST_2', 'SN' /
      DATA TYPASN / 'K8', 'R' ,'K8', 'R', 'R'  /
      DATA NOPASE / 'SN*' /
      DATA TYPASE / 'R'   /
      DATA NOPAF1 / 'TABL_RESU_1', 'INST_1', 'NB_OCCUR_1', 
     +              'TABL_RESU_2', 'INST_2', 'NB_OCCUR_2', 
     +              'SN', 'SN*', 'SP', 'KE', 'SALT', 'NADM',
     +              'DOMMAGE', 'DOMMAGE_CUMU' /
      DATA TYPAF1 / 'K8', 'R', 'I', 'K8', 'R', 'I',
     +              'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R' /
      DATA NOPAF2 / 'TABL_RESU_1', 'INST_1', 'NB_OCCUR_1', 
     +              'TABL_RESU_2', 'INST_2', 'NB_OCCUR_2', 
     +              'SN', 'SP', 'KE', 'SALT', 'NADM',
     +              'DOMMAGE', 'DOMMAGE_CUMU' /
      DATA TYPAF2 / 'K8', 'R', 'I', 'K8', 'R', 'I',
     +              'R', 'R', 'R', 'R', 'R', 'R', 'R' /
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL GETRES ( NOMRES , CONCEP , NOMCMD )
C
      CALL JEVEUO ( CRESU, 'L', JRESU )
      CALL JEVEUO ( CINST, 'L', JINST )
      CALL JELIRA ( CINST, 'LONMAX', NBINST, K8B )
C
C --- CREATION DE LA TABLE
C
      IF ( IT .EQ. 1 .AND. JT .EQ. 1 ) THEN
        NPARA = NPAREN
        DO 10 I = 1 , NPAREN
          NOPARA(I) = NOPAEN(I)
          TYPARA(I) = TYPAEN(I)
 10     CONTINUE
        IF ( LPMPB ) THEN
           DO 12 I = 1 , NPARPM
              NOPARA(NPARA+I) = NOPAPM(I)
              TYPARA(NPARA+I) = TYPAPM(I)
 12        CONTINUE
           NPARA = NPARA + NPARPM
        ENDIF
        IF ( LFATIG ) THEN
          IF ( FLEXIO ) THEN
            DO 14 I = 1 , NPARF1
              NOPARA(NPARA+I) = NOPAF1(I)
              TYPARA(NPARA+I) = TYPAF1(I)
 14         CONTINUE
            NPARA = NPARA + NPARF1
          ELSE
            DO 16 I = 1 , NPARF2
              NOPARA(NPARA+I) = NOPAF2(I)
              TYPARA(NPARA+I) = TYPAF2(I)
 16         CONTINUE
            NPARA = NPARA + NPARF2
          ENDIF
        ELSE
          IF ( LSN ) THEN
            DO 18 I = 1 , NPARSN
              NOPARA(NPARA+I) = NOPASN(I)
              TYPARA(NPARA+I) = TYPASN(I)
 18         CONTINUE
            NPARA = NPARA + NPARSN
          ENDIF
          IF ( FLEXIO ) THEN
            DO 20 I = 1 , NPARSE
              NOPARA(NPARA+I) = NOPASE(I)
              TYPARA(NPARA+I) = TYPASE(I)
 20         CONTINUE
            NPARA = NPARA + NPARSE
          ENDIF
        ENDIF
C
        CALL TBCRSD ( NOMRES, 'G' )
        CALL TBAJPA ( NOMRES, NPARA, NOPARA, TYPARA )
      ENDIF
C
C --- LES LIGNES  LIEU ET SM
C
      IK = 0
      NPARA = NPAREN
      DO 30 I = 1 , NPAREN
        NOPARA(I) = NOPAEN(I)
        TYPARA(I) = TYPAEN(I)
 30   CONTINUE
      IK = IK + 1
      VAKO(IK) = KINTI
      VAKE(IK) = KINTI
      IK = IK + 1
      VAKO(IK) = 'ORIG'
      VAKE(IK) = 'EXTR'
C
      VALO(1) = SM
      VALE(1) = SM
C
      VALO(2) = 3*SM
      VALE(2) = 3*SM
C
C --- POUR L'OPTION "PMPB"
C
      IF ( LPMPB ) THEN
         DO 102 I = 1 , NPARPM
            NOPARA(NPARA+I) = NOPAPM(I)
 102     CONTINUE
         NPAR1 = NPARA + NPARPM
C
         CALL JEVEUO ( CSIGM, 'L', JSIGM )
         DO 110 I = 1, NBINST
            VAKO(IK+1) = ZK8(JRESU+I-1)
            IR = 2 + 1
            VALO(IR) = ZR(JINST+I-1)
            DO 112 ICMP = 1, NCMP
               L1 =               NCMP*(I-1) + ICMP
               L2 = NCMP*NBINST + NCMP*(I-1) + ICMP
               TPM(ICMP) = ZR(JSIGM-1+L1)
               TPB(ICMP) = ZR(JSIGM-1+L2)
               TPMPBO(ICMP) = ZR(JSIGM-1+L1) - ZR(JSIGM-1+L2)
 112        CONTINUE
            CALL FGEQUI ( TPM, 'SIGM', 3, EQUI )
            IR = IR + 1
            VALO(IR) = EQUI(2)
            CALL FGEQUI ( TPB, 'SIGM', 3, EQUI )
            IR = IR + 1
            VALO(IR) = EQUI(2)
            CALL FGEQUI ( TPMPBO, 'SIGM', 3, EQUI )
            IR = IR + 1
            VALO(IR) = EQUI(2)
            CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIO, VALO, C16B, VAKO,0)
 110     CONTINUE
         DO 120 I = 1, NBINST
            VAKE(IK+1) = ZK8(JRESU+I-1)
            IR = 2 + 1
            VALE(IR) = ZR(JINST+I-1)
            DO 122 ICMP = 1, NCMP
               L1 =               NCMP*(I-1) + ICMP
               L2 = NCMP*NBINST + NCMP*(I-1) + ICMP
               TPM(ICMP) = ZR(JSIGM-1+L1)
               TPB(ICMP) = ZR(JSIGM-1+L2)
               TPMPBE(ICMP) = ZR(JSIGM-1+L1) + ZR(JSIGM-1+L2)
 122        CONTINUE
            CALL FGEQUI ( TPM, 'SIGM', 3, EQUI )
            IR = IR + 1
            VALE(IR) = EQUI(2)
            CALL FGEQUI ( TPB, 'SIGM', 3, EQUI )
            IR = IR + 1
            VALE(IR) = EQUI(2)
            CALL FGEQUI ( TPMPBE, 'SIGM', 3, EQUI )
            IR = IR + 1
            VALE(IR) = EQUI(2)
            CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIE, VALE, C16B, VAKE,0)
 120     CONTINUE
      ENDIF
C
C --- POUR L'OPTION "SN"
C
      IF ( LSN .AND. .NOT.LFATIG ) THEN
         DO 202 I = 1 , NPARSN
            NOPARA(NPARA+I) = NOPASN(I)
 202     CONTINUE
         NPAR1 = NPARA + NPARSN
         IF ( FLEXIO ) THEN
            NPAR1 = NPAR1 + 1
            NOPARA(NPAR1) = 'SN*'
         ENDIF
C
         CALL JEVEUO (CSNO , 'L', JSNO  )
         IF ( FLEXIO ) CALL JEVEUO (CSNEO, 'L', JSNEO )
         
         IND = 0
         DO 210 I1 = 1, NBINST
            IND = IND + 1
            VAKO(IK+1) = ZK8(JRESU+I1-1)
            VAKO(IK+2) = ZK8(JRESU+I1-1)
            IR = 2 + 1
            VALO(IR) = ZR(JINST+I1-1)
            IR = IR + 1
            VALO(IR) = ZR(JINST+I1-1)
            IR = IR + 1
            VALO(IR) = ZR(JSNO+IND-1)
            IF ( FLEXIO ) THEN
               IR = IR + 1
               VALO(IR) = ZR(JSNEO+IND-1)
            ENDIF
            CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIO,VALO,C16B,VAKO,0)
            DO 212 I2 = I1+1, NBINST
               IND = IND + 1
               VAKO(IK+1) = ZK8(JRESU+I1-1)
               VAKO(IK+2) = ZK8(JRESU+I2-1)
               IR = 2 + 1
               VALO(IR) = ZR(JINST+I1-1)
               IR = IR + 1
               VALO(IR) = ZR(JINST+I2-1)
               IR = IR + 1
               VALO(IR) = ZR(JSNO+IND-1)
               IF ( FLEXIO ) THEN
                  IR = IR + 1
                  VALO(IR) = ZR(JSNEO+IND-1)
               ENDIF
               CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIO,VALO,C16B,VAKO,0)
 212        CONTINUE
 210     CONTINUE
C
         CALL JEVEUO (CSNE , 'L', JSNE  )
         IF ( FLEXIO ) CALL JEVEUO (CSNEE, 'L', JSNEE )
         IND = 0
         DO 220 I1 = 1, NBINST
            IND = IND + 1
            VAKE(IK+1) = ZK8(JRESU+I1-1)
            VAKE(IK+2) = ZK8(JRESU+I1-1)
            IR = 2 + 1
            VALE(IR) = ZR(JINST+I1-1)
            IR = IR + 1
            VALE(IR) = ZR(JINST+I1-1)
            IR = IR + 1
            VALE(IR) = ZR(JSNE+IND-1)
            IF ( FLEXIO ) THEN
               IR = IR + 1
               VALE(IR) = ZR(JSNEE+IND-1)
            ENDIF
            CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIE,VALE,C16B,VAKE,0)
            DO 222 I2 = I1+1, NBINST
               IND = IND + 1
               VAKE(IK+1) = ZK8(JRESU+I1-1)
               VAKE(IK+2) = ZK8(JRESU+I2-1)
               IR = 2 + 1
               VALE(IR) = ZR(JINST+I1-1)
               IR = IR + 1
               VALE(IR) = ZR(JINST+I2-1)
               IR = IR + 1
               VALE(IR) = ZR(JSNE+IND-1)
               IF ( FLEXIO ) THEN
                  IR = IR + 1
                  VALE(IR) = ZR(JSNEE+IND-1)
               ENDIF
               CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIE,VALE,C16B,VAKE,0)
 222        CONTINUE
 220     CONTINUE
      ENDIF
C
C --- POUR L'OPTION "FATIGUE"
C
      IF ( LFATIG ) THEN
         IF ( FLEXIO ) THEN
            DO 302 I = 1 , NPARF1
               NOPARA(NPARA+I) = NOPAF1(I)
 302        CONTINUE
            NPAR1 = NPARA + NPARF1 - 1
         ELSE
            DO 304 I = 1 , NPARF2
               NOPARA(NPARA+I) = NOPAF2(I)
 304        CONTINUE
            NPAR1 = NPARA + NPARF2 - 1
         ENDIF
C
         CALL JEVEUO ( CNOC, 'L', JNOC )
         CALL JEVEUO (CSNO , 'L', JSNO  )
         IF ( FLEXIO ) CALL JEVEUO (CSNEO, 'L', JSNEO )
         CALL JEVEUO ( CSPO, 'L', JSPO )
         CALL JEVEUO ( CFAO, 'L', JFAO )
         IND = 0
         DO 310 I1 = 1, NBINST
            IND = IND + 1
            VAKO(IK+1) = ZK8(JRESU+I1-1)
            VAKO(IK+2) = ZK8(JRESU+I1-1)
            VAIO(1) = ZI(JNOC+I1-1)
            VAIO(2) = ZI(JNOC+I1-1)
            IR = 2 + 1
            VALO(IR) = ZR(JINST+I1-1)
            IR = IR + 1
            VALO(IR) = ZR(JINST+I1-1)
            IR = IR + 1
            VALO(IR) = ZR(JSNO+IND-1)
            IF ( FLEXIO ) THEN
               IR = IR + 1
               VALO(IR) = ZR(JSNEO+IND-1)
            ENDIF
            IR = IR + 1
            VALO(IR) = ZR(JSPO-1+IND)
            IR = IR + 1
            VALO(IR) = ZR(JFAO-1+4*(IND-1)+1)
            IR = IR + 1
            VALO(IR) = ZR(JFAO-1+4*(IND-1)+2)
            IR = IR + 1
            VALO(IR) = ZR(JFAO-1+4*(IND-1)+3)
            IR = IR + 1
            VALO(IR) = ZR(JFAO-1+4*(IND-1)+4)
            CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIO,VALO,C16B,VAKO,0)
            DO 312 I2 = I1+1, NBINST
               IND = IND + 1
               VAKO(IK+1) = ZK8(JRESU+I1-1)
               VAKO(IK+2) = ZK8(JRESU+I2-1)
               VAIO(1) = ZI(JNOC+I1-1)
               VAIO(2) = ZI(JNOC+I2-1)
               IR = 2 + 1
               VALO(IR) = ZR(JINST+I1-1)
               IR = IR + 1
               VALO(IR) = ZR(JINST+I2-1)
               IR = IR + 1
               VALO(IR) = ZR(JSNO+IND-1)
               IF ( FLEXIO ) THEN
                  IR = IR + 1
                  VALO(IR) = ZR(JSNEO+IND-1)
               ENDIF
               IR = IR + 1
               VALO(IR) = ZR(JSPO-1+IND)
               IR = IR + 1
               VALO(IR) = ZR(JFAO-1+4*(IND-1)+1)
               IR = IR + 1
               VALO(IR) = ZR(JFAO-1+4*(IND-1)+2)
               IR = IR + 1
               VALO(IR) = ZR(JFAO-1+4*(IND-1)+3)
               IR = IR + 1
               VALO(IR) = ZR(JFAO-1+4*(IND-1)+4)
               CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIO,VALO,C16B,VAKO,0)
 312        CONTINUE
 310     CONTINUE
C
         CALL JEVEUO (CSNE , 'L', JSNE  )
         IF ( FLEXIO ) CALL JEVEUO (CSNEE, 'L', JSNEE )
         CALL JEVEUO ( CSPE, 'L', JSPE )
         CALL JEVEUO ( CFAE, 'L', JFAE )
         IND = 0
         DO 320 I1 = 1, NBINST
            IND = IND + 1
            VAKE(IK+1) = ZK8(JRESU+I1-1)
            VAKE(IK+2) = ZK8(JRESU+I1-1)
            VAIE(1) = ZI(JNOC+I1-1)
            VAIE(2) = ZI(JNOC+I1-1)
            IR = 2 + 1
            VALE(IR) = ZR(JINST+I1-1)
            IR = IR + 1
            VALE(IR) = ZR(JINST+I1-1)
            IR = IR + 1
            VALE(IR) = ZR(JSNE+IND-1)
            IF ( FLEXIO ) THEN
               IR = IR + 1
               VALE(IR) = ZR(JSNEE+IND-1)
            ENDIF
            IR = IR + 1
            VALE(IR) = ZR(JSPE-1+IND)
            IR = IR + 1
            VALE(IR) = ZR(JFAE-1+4*(IND-1)+1)
            IR = IR + 1
            VALE(IR) = ZR(JFAE-1+4*(IND-1)+2)
            IR = IR + 1
            VALE(IR) = ZR(JFAE-1+4*(IND-1)+3)
            IR = IR + 1
            VALE(IR) = ZR(JFAE-1+4*(IND-1)+4)
            CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIE,VALE,C16B,VAKE,0)
            DO 322 I2 = I1+1, NBINST
               IND = IND + 1
               VAKE(IK+1) = ZK8(JRESU+I1-1)
               VAKE(IK+2) = ZK8(JRESU+I2-1)
               VAIE(1) = ZI(JNOC+I1-1)
               VAIE(2) = ZI(JNOC+I2-1)
               IR = 2 + 1
               VALE(IR) = ZR(JINST+I1-1)
               IR = IR + 1
               VALE(IR) = ZR(JINST+I2-1)
               IR = IR + 1
               VALE(IR) = ZR(JSNE+IND-1)
               IF ( FLEXIO ) THEN
                  IR = IR + 1
                  VALE(IR) = ZR(JSNEE+IND-1)
               ENDIF
               IR = IR + 1
               VALE(IR) = ZR(JSPE-1+IND)
               IR = IR + 1
               VALE(IR) = ZR(JFAE-1+4*(IND-1)+1)
               IR = IR + 1
               VALE(IR) = ZR(JFAE-1+4*(IND-1)+2)
               IR = IR + 1
               VALE(IR) = ZR(JFAE-1+4*(IND-1)+3)
               IR = IR + 1
               VALE(IR) = ZR(JFAE-1+4*(IND-1)+4)
               CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIE,VALE,C16B,VAKE,0)
 322        CONTINUE
 320     CONTINUE
C
         NOPARA(NPARA+1) = 'DOMMAGE_CUMU'
         NPAR1 = NPARA + 1
         CALL RCEVFU ( CNOC, CFAO, DCO )
         CALL RCEVFU ( CNOC, CFAE, DCE )
         VALO(3) = DCO
         VALE(3) = DCE
         CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIO,VALO,C16B,VAKO,0)
         CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIE,VALE,C16B,VAKE,0)
C
      ENDIF
C
      CALL JEDEMA( )
      END
