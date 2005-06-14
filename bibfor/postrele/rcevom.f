      SUBROUTINE RCEVOM ( CSIGM, CINST, CNOC, SM, LFATIG, LPMPB, LSN,
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
C MODIF POSTRELE  DATE 21/03/2005   AUTEUR CIBHHLV L.VIVAN 
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
C TOLE CRP_20
C     ------------------------------------------------------------------
C     OPERATEUR POST_RCCM, TYPE_RESU_MECA='EVOLUTION'
C     TYPE_RESU = 'VALE_MAX'
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
      INTEGER      NCMP, JSIGM, JINST, NBINST, NBORDR, JSNO, JSNE, N1,
     +             IND, I1, I2, ICMP, L1,L2,L3,L4, NPARA, II,IK,IR, I,
     +             VAIO(5), VAIE(5), IOO1, IOO2, IOE1, IOE2, NPAR1,
     +             JSNEO, JSNEE, JSPO, JSPE, JFAO, JFAE, JNOC, JRESU
      PARAMETER  ( NCMP = 6 )
      REAL*8       TPM(NCMP), TPB(NCMP), TPMPBO(NCMP), TPMPBE(NCMP),
     +             PM, PB, PMPBO, PMPBE, IPM, IPB, IPMPBO, IPMPBE,
     +             SNO, SNE, I1SNO, I2SNO, I1SNE, I2SNE,
     +             SPO, SPE, KEO, KEE, SAO, SAE, NAO, NAE,
     +             DOO, DOE, DCO, DCE,
     +             EQUI(NCMP), VALO(36), VALE(36)
      COMPLEX*16   C16B
      CHARACTER*8  K8B, NOMRES, TYPARA(36), RPM, RPB, RPMPBO, RPMPBE, 
     +             R1SNO, R1SNE, R2SNO, R2SNE
      CHARACTER*16 NOMCMD, CONCEP, NOPARA(36), VAKO(5), VAKE(5)
C
      INTEGER      NPAREN, NPARPM, NPARSN, NPARSE, NPARFA
      PARAMETER  ( NPAREN=4, NPARPM=7, NPARSN=5, NPARSE=3, NPARFA=10 )
      CHARACTER*8  TYPAEN(NPAREN), TYPAPM(NPARPM), TYPASN(NPARSN),
     +             TYPASE(NPARSE), TYPAFA(NPARFA)
      CHARACTER*16 NOPAEN(NPAREN), NOPAPM(NPARPM), NOPASN(NPARSN),
     +             NOPASE(NPARSE), NOPAFA(NPARFA)
C
      DATA NOPAEN / 'INTITULE', 'LIEU', 'SM', '3SM' /
      DATA TYPAEN / 'K16',      'K8'  , 'R' , 'R'   / 
      DATA NOPAPM / 'TABL_RESU', 'INST_PM', 'PM', 'INST_PB', 'PB',
     +              'INST_PMB' , 'PMB' /
      DATA TYPAPM / 'K8', 'R', 'R', 'R', 'R', 'R', 'R' /
      DATA NOPASN / 'TABL_RESU_1', 'INST_SN_1', 
     +              'TABL_RESU_2', 'INST_SN_2', 'SN' /
      DATA TYPASN / 'K8', 'R' ,'K8', 'R', 'R'  /
      DATA NOPASE / 'INST_SN*_1', 'INST_SN*_2', 'SN*' /
      DATA TYPASE / 'R' , 'R', 'R'  /
      DATA NOPAFA / 'INST_SALT_1', 'NB_OCCUR_1',
     +              'INST_SALT_2', 'NB_OCCUR_2', 
     +              'SP', 'KE', 'SALT', 'NADM', 'DOMMAGE',
     +              'DOMMAGE_CUMU' /
      DATA TYPAFA / 'R', 'I', 'R', 'I','R', 'R', 'R', 'R', 'R', 'R' /
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
        IF ( LSN ) THEN
           DO 14 I = 1 , NPARSN
              NOPARA(NPARA+I) = NOPASN(I)
              TYPARA(NPARA+I) = TYPASN(I)
 14        CONTINUE
           NPARA = NPARA + NPARSN
        ENDIF
        IF ( FLEXIO .AND. LSN ) THEN
           DO 16 I = 1 , NPARSE
              NOPARA(NPARA+I) = NOPASE(I)
              TYPARA(NPARA+I) = TYPASE(I)
 16        CONTINUE
           NPARA = NPARA + NPARSE
        ENDIF
        IF ( LFATIG ) THEN
           DO 18 I = 1 , NPARFA
              NOPARA(NPARA+I) = NOPAFA(I)
              TYPARA(NPARA+I) = TYPAFA(I)
 18        CONTINUE
           NPARA = NPARA + NPARFA
        ENDIF
C
        CALL TBCRSD ( NOMRES, 'G' )
        CALL TBAJPA ( NOMRES, NPARA, NOPARA, TYPARA )
      ENDIF
C
C --- LES LIGNES LIEU ET SM
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
C
C --- LES CRITERES DE NIVEAU 0 VISENT A PREMUNIR LE MATERIEL CONTRE LES
C     DOMMAGES DE DEFORMATION EXCESSIVE, D'INSTABILITE PLASTIQUE ET
C     D'INSTABILITE ELASTIQUE ET ELASTOPLASTIQUE.
C     ON NE PREND QUE LA PARTIE MECANIQUE
C
         CALL JEVEUO ( CSIGM, 'L', JSIGM )
         PM = 0.D0
         PB = 0.D0
         PMPBO = 0.D0
         PMPBE = 0.D0
         RPM = ZK8(JRESU)
         RPB = ZK8(JRESU)
         RPMPBO = ZK8(JRESU)
         RPMPBE = ZK8(JRESU)
         DO 100 I = 1, NBINST
            DO 102 ICMP = 1, NCMP
               L1 =                 NCMP*(I-1) + ICMP
               L2 =   NCMP*NBINST + NCMP*(I-1) + ICMP
               L3 = 2*NCMP*NBINST + NCMP*(I-1) + ICMP
               L4 = 3*NCMP*NBINST + NCMP*(I-1) + ICMP
               TPM(ICMP) = ZR(JSIGM-1+L1) - ZR(JSIGM-1+L3)
               TPB(ICMP) = ZR(JSIGM-1+L2) - ZR(JSIGM-1+L4)
               TPMPBO(ICMP) = ZR(JSIGM-1+L1) - ZR(JSIGM-1+L2)
     +                         - ( ZR(JSIGM-1+L3) - ZR(JSIGM-1+L4) )
               TPMPBE(ICMP) = ZR(JSIGM-1+L1) + ZR(JSIGM-1+L2)
     +                         - ( ZR(JSIGM-1+L3) + ZR(JSIGM-1+L4) )
 102        CONTINUE
            CALL FGEQUI ( TPM, 'SIGM', 3, EQUI )
            IF ( EQUI(2) .GT. PM ) THEN
                PM = EQUI(2)
               IPM = ZR(JINST+I-1)
               RPM = ZK8(JRESU+I-1)
            ENDIF
            CALL FGEQUI ( TPB, 'SIGM', 3, EQUI )
            IF ( EQUI(2) .GT. PB ) THEN
                PB = EQUI(2)
               IPB = ZR(JINST+I-1)
               RPB = ZK8(JRESU+I-1)
            ENDIF
            CALL FGEQUI ( TPMPBO, 'SIGM', 3, EQUI )
            IF ( EQUI(2) .GT. PMPBO ) THEN
                PMPBO = EQUI(2)
               IPMPBO = ZR(JINST+I-1)
               RPMPBO = ZK8(JRESU+I-1)
            ENDIF
            CALL FGEQUI ( TPMPBE, 'SIGM', 3, EQUI )
            IF ( EQUI(2) .GT. PMPBE ) THEN
                PMPBE = EQUI(2)
               IPMPBE = ZR(JINST+I-1)
               RPMPBE = ZK8(JRESU+I-1)
            ENDIF
 100     CONTINUE
C
         NPAR1 = NPARA + 1
         NOPARA(NPAR1) = 'TABL_RESU'
         VAKO(IK+1) = RPM
         VAKE(IK+1) = RPM
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'INST_PM'
         IR = 2 + 1
         VALO(IR) = IPM
         VALE(IR) = IPM
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'PM'
         IR = IR + 1
         VALO(IR) = PM
         VALE(IR) = PM
         CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIO, VALO, C16B, VAKO,0)
         CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIE, VALE, C16B, VAKE,0)
C
         NPAR1 = NPARA + 1
         NOPARA(NPAR1) = 'TABL_RESU'
         VAKO(IK+1) = RPB
         VAKE(IK+1) = RPB
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'INST_PB'
         IR = 2 + 1
         VALO(IR) = IPB
         VALE(IR) = IPB
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'PB'
         IR = IR + 1
         VALO(IR) = PB
         VALE(IR) = PB
         CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIO, VALO, C16B, VAKO,0)
         CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIE, VALE, C16B, VAKE,0)
C
         NPAR1 = NPARA + 1
         NOPARA(NPAR1) = 'TABL_RESU'
         VAKO(IK+1) = RPMPBO
         VAKE(IK+1) = RPMPBE
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'INST_PMB'
         IR = 2 + 1
         VALO(IR) = IPMPBO
         VALE(IR) = IPMPBE
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'PMB'
         IR = IR + 1
         VALO(IR) = PMPBO
         VALE(IR) = PMPBE
         CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIO, VALO, C16B, VAKO,0)
         CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIE, VALE, C16B, VAKE,0)
      ENDIF
C
C --- POUR L'OPTION "SN"
C
      IF ( LSN ) THEN
         CALL JEVEUO (CSNO, 'L', JSNO )
         CALL JEVEUO (CSNE, 'L', JSNE )
         SNO = 0.D0
         SNE = 0.D0
         SNO = 0.D0
         SNE = 0.D0
         IND = 0
         R1SNO = ZK8(JRESU)
         R2SNO = ZK8(JRESU)
         R1SNE = ZK8(JRESU)
         R2SNE = ZK8(JRESU)
         DO 200 I1 = 1, NBINST
            IND = IND + 1
            IF ( ZR(JSNO+IND-1) .GT. SNO ) THEN
                 SNO = ZR(JSNO+IND-1)
               I1SNO = ZR(JINST+I1-1)
               I2SNO = ZR(JINST+I1-1)
               R1SNO = ZK8(JRESU+I1-1)
               R2SNO = ZK8(JRESU+I1-1)
            ENDIF
            IF ( ZR(JSNE+IND-1) .GT. SNE ) THEN
                 SNE = ZR(JSNE+IND-1)
               I1SNE = ZR(JINST+I1-1)
               I2SNE = ZR(JINST+I1-1)
               R1SNE = ZK8(JRESU+I1-1)
               R2SNE = ZK8(JRESU+I1-1)
            ENDIF
            DO 202 I2 = I1+1, NBINST
               IND = IND + 1
               IF ( ZR(JSNO+IND-1) .GT. SNO ) THEN
                    SNO = ZR(JSNO+IND-1)
                  I1SNO = ZR(JINST+I1-1)
                  I2SNO = ZR(JINST+I2-1)
                  R1SNO = ZK8(JRESU+I1-1)
                  R2SNO = ZK8(JRESU+I2-1)
               ENDIF
               IF ( ZR(JSNE+IND-1) .GT. SNE ) THEN
                    SNE = ZR(JSNE+IND-1)
                  I1SNE = ZR(JINST+I1-1)
                  I2SNE = ZR(JINST+I2-1)
                  R1SNE = ZK8(JRESU+I1-1)
                  R2SNE = ZK8(JRESU+I2-1)
               ENDIF
 202        CONTINUE
 200     CONTINUE
C
         NPAR1 = NPARA + 1
         NOPARA(NPAR1) = 'TABL_RESU_1'
         VAKO(IK+1) = R1SNO
         VAKE(IK+1) = R1SNE
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'INST_SN_1'
         IR = 2 + 1
         VALO(IR) = I1SNO
         VALE(IR) = I1SNE
C
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'TABL_RESU_2'
         VAKO(IK+2) = R2SNO
         VAKE(IK+2) = R2SNE
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'INST_SN_2'
         IR = IR + 1
         VALO(IR) = I2SNO
         VALE(IR) = I2SNE
C
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'SN'
         IR = IR + 1
         VALO(IR) = SNO
         VALE(IR) = SNE
         CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIO, VALO, C16B, VAKO,0)
         CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIE, VALE, C16B, VAKE,0)
      ENDIF
C
      IF ( FLEXIO .AND. LSN ) THEN
         CALL JEVEUO (CSNEO, 'L', JSNO )
         CALL JEVEUO (CSNEE, 'L', JSNE )
         SNO = 0.D0
         SNE = 0.D0
         SNO = 0.D0
         SNE = 0.D0
         IND = 0
         R1SNO = ZK8(JRESU)
         R2SNO = ZK8(JRESU)
         R1SNE = ZK8(JRESU)
         R2SNE = ZK8(JRESU)
         DO 210 I1 = 1, NBINST
            IND = IND + 1
            IF ( ZR(JSNO+IND-1) .GT. SNO ) THEN
                 SNO = ZR(JSNO+IND-1)
               I1SNO = ZR(JINST+I1-1)
               I2SNO = ZR(JINST+I1-1)
               R1SNO = ZK8(JRESU+I1-1)
               R2SNO = ZK8(JRESU+I1-1)
            ENDIF
            IF ( ZR(JSNE+IND-1) .GT. SNE ) THEN
                 SNE = ZR(JSNE+IND-1)
               I1SNE = ZR(JINST+I1-1)
               I2SNE = ZR(JINST+I1-1)
               R1SNE = ZK8(JRESU+I1-1)
               R2SNE = ZK8(JRESU+I1-1)
            ENDIF
            DO 212 I2 = I1+1, NBINST
               IND = IND + 1
               IF ( ZR(JSNO+IND-1) .GT. SNO ) THEN
                    SNO = ZR(JSNO+IND-1)
                  I1SNO = ZR(JINST+I1-1)
                  I2SNO = ZR(JINST+I2-1)
                  R1SNO = ZK8(JRESU+I1-1)
                  R2SNO = ZK8(JRESU+I2-1)
               ENDIF
               IF ( ZR(JSNE+IND-1) .GT. SNE ) THEN
                    SNE = ZR(JSNE+IND-1)
                  I1SNE = ZR(JINST+I1-1)
                  I2SNE = ZR(JINST+I2-1)
                  R1SNE = ZK8(JRESU+I1-1)
                  R2SNE = ZK8(JRESU+I2-1)
               ENDIF
 212        CONTINUE
 210     CONTINUE
C
         NPAR1 = NPARA + 1
         NOPARA(NPAR1) = 'TABL_RESU_1'
         VAKO(IK+1) = R1SNO
         VAKE(IK+1) = R1SNE
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'INST_SN*_1'
         IR = 2 + 1
         VALO(IR) = I1SNO
         VALE(IR) = I1SNE
C
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'TABL_RESU_2'
         VAKO(IK+2) = R2SNO
         VAKE(IK+2) = R2SNE
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'INST_SN*_2'
         IR = IR + 1
         VALO(IR) = I2SNO
         VALE(IR) = I2SNE
C
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'SN*'
         IR = IR + 1
         VALO(IR) = SNO
         VALE(IR) = SNE
         CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIO, VALO, C16B, VAKO,0)
         CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIE, VALE, C16B, VAKE,0)
      ENDIF
C
C --- POUR L'OPTION "FATIGUE"
C
      IF ( LFATIG ) THEN
         CALL JELIRA ( CSPO, 'LONMAX', NBORDR, K8B )
         CALL JEVEUO ( CSPO, 'L', JSPO )
         CALL JEVEUO ( CSPE, 'L', JSPE )
         CALL JEVEUO ( CFAO, 'L', JFAO )
         CALL JEVEUO ( CFAE, 'L', JFAE )
         CALL JEVEUO ( CNOC, 'L', JNOC )
         SPO = 0.D0
         KEO = 0.D0
         SAO = 0.D0
         NAO = 0.D0
         DOO = 0.D0
         SPE = 0.D0
         KEE = 0.D0
         SAE = 0.D0
         NAE = 0.D0
         DOE = 0.D0
         R1SNO = ZK8(JRESU)
         R2SNO = ZK8(JRESU)
         R1SNE = ZK8(JRESU)
         R2SNE = ZK8(JRESU)
         DO 300 I = 1, NBORDR
            SPO = MAX ( SPO , ZR(JSPO-1+I) )
            KEO = MAX ( KEO , ZR(JFAO-1+4*(I-1)+1) )
            NAO = MAX ( NAO , ZR(JFAO-1+4*(I-1)+3) )
            DOO = MAX ( DOO , ZR(JFAO-1+4*(I-1)+4) )
            SPE = MAX ( SPE , ZR(JSPE-1+I) )
            KEE = MAX ( KEE , ZR(JFAE-1+4*(I-1)+1) )
            NAE = MAX ( NAE , ZR(JFAE-1+4*(I-1)+3) )
            DOE = MAX ( DOE , ZR(JFAE-1+4*(I-1)+4) )
 300     CONTINUE
         IND = 0
         DO 310 I1 = 1, NBINST
            IND = IND + 1
            IF ( ZR(JFAO-1+4*(IND-1)+2) .GT. SAO ) THEN
               SAO  = ZR(JFAO-1+4*(IND-1)+2)
               IOO1 = ZI(JNOC+I1-1)
               IOO2 = ZI(JNOC+I1-1)
               R1SNO = ZK8(JRESU+I1-1)
               R2SNO = ZK8(JRESU+I1-1)
               I1SNO = ZR(JINST+I1-1)
               I2SNO = ZR(JINST+I1-1)
            ENDIF
            IF ( ZR(JFAE-1+4*(IND-1)+2) .GT. SAE ) THEN
               SAO  = ZR(JFAE-1+4*(IND-1)+2)
               IOE1 = ZI(JNOC+I1-1)
               IOE2 = ZI(JNOC+I1-1)
               R1SNE = ZK8(JRESU+I1-1)
               R2SNE = ZK8(JRESU+I1-1)
               I1SNE = ZR(JINST+I1-1)
               I2SNE = ZR(JINST+I1-1)
            ENDIF
            DO 312 I2 = I1+1, NBINST
               IND = IND + 1
               IF ( ZR(JFAO-1+4*(IND-1)+2) .GT. SAO ) THEN
                  SAO  = ZR(JFAO-1+4*(IND-1)+2)
                  IOO1 = ZI(JNOC+I1-1)
                  IOO2 = ZI(JNOC+I2-1)
                  R1SNO = ZK8(JRESU+I1-1)
                  R2SNO = ZK8(JRESU+I2-1)
                  I1SNO = ZR(JINST+I1-1)
                  I2SNO = ZR(JINST+I2-1)
               ENDIF
               IF ( ZR(JFAE-1+4*(IND-1)+2) .GT. SAE ) THEN
                  SAE  = ZR(JFAE-1+4*(IND-1)+2)
                  IOE1 = ZI(JNOC+I1-1)
                  IOE2 = ZI(JNOC+I2-1)
                  R1SNE = ZK8(JRESU+I1-1)
                  R2SNE = ZK8(JRESU+I2-1)
                  I1SNE = ZR(JINST+I1-1)
                  I2SNE = ZR(JINST+I2-1)
               ENDIF
 312        CONTINUE
 310     CONTINUE
C
         NPAR1 = NPARA + 1
         NOPARA(NPAR1) = 'TABL_RESU_1'
         VAKO(IK+1) = R1SNO
         VAKE(IK+1) = R1SNE
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'INST_SALT_1'
         IR = 2 + 1
         VALO(IR) = I1SNO
         VALE(IR) = I1SNE
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'NB_OCCUR_1'
         VAIO(1) = IOO1
         VAIE(1) = IOE1
C
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'TABL_RESU_2'
         VAKO(IK+2) = R2SNO
         VAKE(IK+2) = R2SNE
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'INST_SALT_2'
         IR = IR + 1
         VALO(IR) = I2SNO
         VALE(IR) = I2SNE
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'NB_OCCUR_2'
         VAIO(2) = IOO2
         VAIE(2) = IOE2
C
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'SP'
         IR = IR + 1
         VALO(IR) = SPO
         VALE(IR) = SPE
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'KE'
         IR = IR + 1
         VALO(IR) = KEO
         VALE(IR) = KEE
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'SALT'
         IR = IR + 1
         VALO(IR) = SAO
         VALE(IR) = SAE
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'NADM'
         IR = IR + 1
         VALO(IR) = NAO
         VALE(IR) = NAE
         NPAR1 = NPAR1 + 1
         NOPARA(NPAR1) = 'DOMMAGE'
         IR = IR + 1
         VALO(IR) = DOO
         VALE(IR) = DOE
         CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIO, VALO, C16B, VAKO,0)
         CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIE, VALE, C16B, VAKE,0)
C
         CALL RCEVFU ( CNOC, CFAO, DCO )
         CALL RCEVFU ( CNOC, CFAE, DCE )
C
         NPAR1 = NPARA + 1
         NOPARA(NPAR1) = 'DOMMAGE_CUMU'
         CALL RCEVFU ( CNOC, CFAO, DCO )
         CALL RCEVFU ( CNOC, CFAE, DCE )
         VALO(3) = DCO
         VALE(3) = DCE
         CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIO,VALO,C16B,VAKO,0)
         CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIE,VALE,C16B,VAKE,0)
      ENDIF
C
      CALL JEDEMA( )
      END
