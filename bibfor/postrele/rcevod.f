      SUBROUTINE RCEVOD ( CSIGM, CINST, CNOC, SM, LFATIG, LPMPB, LSN,
     +                    CSNO, CSNE, FLEXIO, CSNEO, CSNEE, CFAO, CFAE,
     +                    CSPO, CSPE, CRESU, KINTI, IT, JT, LROCHT, 
     +                    SYMAX, CPRES, KEMIXT, CSPTO, CSPTE, 
     +                    CSPMO, CSPME)
      IMPLICIT   NONE
      INTEGER      IT, JT
      REAL*8       SM, SYMAX
      LOGICAL      LFATIG, LPMPB, LSN, FLEXIO, LROCHT, KEMIXT
      CHARACTER*16 KINTI
      CHARACTER*24 CSIGM, CINST, CNOC, CSNO, CSNE, CSNEO, CSNEE,
     +             CFAO, CFAE, CSPO, CSPE, CRESU, CPRES,
     +             CSPTO, CSPTE, CSPMO, CSPME
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 06/07/2009   AUTEUR GALENNE E.GALENNE 
C TOLE CRP_20 CRP_21
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      NCMP, JSIGM, JINST, NBINST, JSNO, JSNE, 
     +             IND, I1, I2, ICMP, L1,L2,L3,L4, NPARA, IK, IR, I,
     +             VAIO(5), VAIE(5), NPAR1,JRESP,
     +             JSNEO, JSNEE, JSPO, JSPE, JFAO, JFAE, JNOC, JRESU,
     +             JSPTO, JSPTE, JSPMO, JSPME
      PARAMETER  ( NCMP = 6 )
      REAL*8       TPM(NCMP), TPB(NCMP), TPMPBO(NCMP), TPMPBE(NCMP),
     +             DCO, DCE, TRESCA, VALO(39), VALE(39), STLIN,STPAR
      COMPLEX*16   C16B
      CHARACTER*8  K8B, NOMRES, TYPARA(39)
      CHARACTER*16 NOMCMD, CONCEP, NOPARA(39), VAKO(5), VAKE(5)
C
      INTEGER      NPAREN, NPARPM, NPARSN, NPARSE, NPARF1, NPARF2,
     +             NPARF3, NPARRT, IFM, NIV
      PARAMETER  ( NPAREN=4, NPARPM=5, NPARSN=5, NPARSE=1,
     +             NPARF1=14, NPARF2=13, NPARRT=6 , NPARF3=17)
      CHARACTER*8  TYPAEN(NPAREN), TYPAPM(NPARPM), TYPASN(NPARSN),
     +             TYPASE(NPARSE), TYPAF1(NPARF1), TYPAF2(NPARF2),
     +             TYPART(NPARRT), TYPAF3(NPARF3)
      CHARACTER*16 NOPAEN(NPAREN), NOPAPM(NPARPM), NOPASN(NPARSN),
     +             NOPASE(NPARSE), NOPAF1(NPARF1), NOPAF2(NPARF2),
     +             NOPART(NPARRT), NOPAF3(NPARF3)
C
      DATA NOPAEN / 'INTITULE', 'LIEU', 'SM', '3SM' /
      DATA TYPAEN / 'K16',      'K8'  , 'R' , 'R'   / 
      DATA NOPART / 'TABL_PRES', 'SY', 'INST', 'SIGM_M_PRES',
     +              'VALE_MAXI_LINE', 'VALE_MAXI_PARAB' /
      DATA TYPART / 'K8', 'R', 'R' , 'R'   , 'R'   , 'R'   / 
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
      DATA NOPAF3 / 'TABL_RESU_1', 'INST_1', 'NB_OCCUR_1', 
     +            'TABL_RESU_2', 'INST_2', 'NB_OCCUR_2', 
     +            'SN', 'SN*', 'SP','SP_MECA','SP_THER','KE_MECA', 
     +            'KE_THER', 'SALT', 'NADM', 'DOMMAGE', 'DOMMAGE_CUMU' /
      DATA TYPAF3 / 'K8', 'R', 'I', 'K8', 'R', 'I','R', 'R', 
     +              'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R' /
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL GETRES ( NOMRES , CONCEP , NOMCMD )
      CALL INFNIV ( IFM, NIV )
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
        IF ( LROCHT ) THEN
           DO 11 I = 1 , NPARRT
              NOPARA(NPARA+I) = NOPART(I)
              TYPARA(NPARA+I) = TYPART(I)
 11        CONTINUE
           NPARA = NPARA + NPARRT
        ENDIF
        IF ( LPMPB ) THEN
           DO 12 I = 1 , NPARPM
              NOPARA(NPARA+I) = NOPAPM(I)
              TYPARA(NPARA+I) = TYPAPM(I)
 12        CONTINUE
           NPARA = NPARA + NPARPM
        ENDIF
        IF ( LFATIG ) THEN
          IF (KEMIXT) THEN
            DO 14 I = 1 , NPARF3
              NOPARA(NPARA+I) = NOPAF3(I)
              TYPARA(NPARA+I) = TYPAF3(I)
 14         CONTINUE
            NPARA = NPARA + NPARF3
          ELSE
            IF ( FLEXIO ) THEN
              DO 15 I = 1 , NPARF1
                NOPARA(NPARA+I) = NOPAF1(I)
                TYPARA(NPARA+I) = TYPAF1(I)
 15           CONTINUE
              NPARA = NPARA + NPARF1
            ELSE
              DO 16 I = 1 , NPARF2
                NOPARA(NPARA+I) = NOPAF2(I)
                TYPARA(NPARA+I) = TYPAF2(I)
 16           CONTINUE
              NPARA = NPARA + NPARF2
            ENDIF
          ENDIF
        ELSE
          IF ( LSN ) THEN
            DO 18 I = 1 , NPARSN
              NOPARA(NPARA+I) = NOPASN(I)
              TYPARA(NPARA+I) = TYPASN(I)
 18         CONTINUE
            NPARA = NPARA + NPARSN
            IF ( FLEXIO ) THEN
              DO 20 I = 1 , NPARSE
                NOPARA(NPARA+I) = NOPASE(I)
                TYPARA(NPARA+I) = TYPASE(I)
 20           CONTINUE
              NPARA = NPARA + NPARSE
            ENDIF
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
C --- POUR LE ROCHET THERMIQUE
C
      IF ( LROCHT ) THEN
         CALL JEVEUO ( CSIGM, 'L', JSIGM )
         CALL JEVEUO ( CPRES, 'L', JRESP )
         DO 404 I = 1 , NPARRT
            NOPARA(NPARA+I) = NOPART(I)
 404     CONTINUE
         NPAR1 = NPARA + NPARRT
         VAKO(IK+1) = ZK8(JRESP-1+JT)
         VAKE(IK+1) = ZK8(JRESP-1+JT)
         IR = 2 + 1
         VALO(IR) = SYMAX
         VALE(IR) = SYMAX
         DO 400 I = 1, NBINST
            IR = 3 + 1
            VALO(IR) = ZR(JINST+I-1)
            VALE(IR) = ZR(JINST+I-1)
            DO 402 ICMP = 1, NCMP
               L3 = 4*NCMP*NBINST + NCMP*(I-1) + ICMP
               TPM(ICMP) = ZR(JSIGM-1+L3)
 402        CONTINUE
            CALL RCTRES ( TPM, TRESCA )
            CALL RCMCRT ( SYMAX, TRESCA, STLIN, STPAR )
C
            IR = IR + 1
            VALO(IR) = TRESCA
            VALE(IR) = TRESCA
            IR = IR + 1
            VALO(IR) = STLIN
            VALE(IR) = STLIN
            IR = IR + 1
            VALO(IR) = STPAR
            VALE(IR) = STPAR       
            CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIO, VALO, C16B, VAKO,0)
            CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIE, VALE, C16B, VAKE,0)
 400     CONTINUE
C
      ENDIF
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
               L1 =                 NCMP*(I-1) + ICMP
               L2 =   NCMP*NBINST + NCMP*(I-1) + ICMP
               L3 = 2*NCMP*NBINST + NCMP*(I-1) + ICMP
               L4 = 3*NCMP*NBINST + NCMP*(I-1) + ICMP
               TPM(ICMP) = ZR(JSIGM-1+L1) - ZR(JSIGM-1+L3)
               TPB(ICMP) = ZR(JSIGM-1+L2) - ZR(JSIGM-1+L4)
               TPMPBO(ICMP) = ZR(JSIGM-1+L1) - ZR(JSIGM-1+L2)
     +                        - (ZR(JSIGM-1+L3) - ZR(JSIGM-1+L4))
 112        CONTINUE
            CALL RCTRES ( TPM, TRESCA )
            IR = IR + 1
            VALO(IR) = TRESCA
            CALL RCTRES ( TPB, TRESCA )
            IR = IR + 1
            VALO(IR) = TRESCA
            CALL RCTRES ( TPMPBO, TRESCA )
            IR = IR + 1
            VALO(IR) = TRESCA
            CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIO, VALO, C16B, VAKO,0)
 110     CONTINUE
         DO 120 I = 1, NBINST
            VAKE(IK+1) = ZK8(JRESU+I-1)
            IR = 2 + 1
            VALE(IR) = ZR(JINST+I-1)
            DO 122 ICMP = 1, NCMP
               L1 =                 NCMP*(I-1) + ICMP
               L2 =   NCMP*NBINST + NCMP*(I-1) + ICMP
               L3 = 2*NCMP*NBINST + NCMP*(I-1) + ICMP
               L4 = 3*NCMP*NBINST + NCMP*(I-1) + ICMP
               TPM(ICMP) = ZR(JSIGM-1+L1) - ZR(JSIGM-1+L3)
               TPB(ICMP) = ZR(JSIGM-1+L2) - ZR(JSIGM-1+L4)
               TPMPBE(ICMP) = ZR(JSIGM-1+L1) + ZR(JSIGM-1+L2)
     +                        - (ZR(JSIGM-1+L3) + ZR(JSIGM-1+L4))
 122        CONTINUE
            CALL RCTRES ( TPM, TRESCA )
            IR = IR + 1
            VALE(IR) = TRESCA
            CALL RCTRES ( TPB, TRESCA )
            IR = IR + 1
            VALE(IR) = TRESCA
            CALL RCTRES ( TPMPBE, TRESCA )
            IR = IR + 1
            VALE(IR) = TRESCA
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
         IF ( KEMIXT ) THEN
           DO 301 I = 1 , NPARF3
             NOPARA(NPARA+I) = NOPAF3(I)
 301       CONTINUE
           NPAR1 = NPARA + NPARF3 - 1
         ELSEIF ( FLEXIO ) THEN
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
         IF (KEMIXT) THEN
           CALL JEVEUO ( CSPMO, 'L', JSPMO )
           CALL JEVEUO ( CSPTO, 'L', JSPTO )
         ENDIF
         IND = 0
         DO 310 I1 = 1, NBINST
            IND = IND + 1
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
               IF (KEMIXT) THEN
                 IR = IR + 1
                 VALO(IR) = ZR(JSPMO-1+IND)
                 IR = IR + 1
                 VALO(IR) = ZR(JSPTO-1+IND)
               ENDIF
               IR = IR + 1
               VALO(IR) = ZR(JFAO-1+5*(IND-1)+1)
               IF (KEMIXT) THEN
                 IR = IR + 1
                 VALO(IR) =ZR(JFAO-1+5*(IND-1)+5)
               ENDIF
               IR = IR + 1
               VALO(IR) = ZR(JFAO-1+5*(IND-1)+2)
               IR = IR + 1
               VALO(IR) = ZR(JFAO-1+5*(IND-1)+3)
               IR = IR + 1
               VALO(IR) = ZR(JFAO-1+5*(IND-1)+4)
               CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIO,VALO,C16B,VAKO,0)
 312        CONTINUE
 310     CONTINUE
C
         CALL JEVEUO (CSNE , 'L', JSNE  )
         IF ( FLEXIO ) CALL JEVEUO (CSNEE, 'L', JSNEE )
         CALL JEVEUO ( CSPE, 'L', JSPE )
         CALL JEVEUO ( CFAE, 'L', JFAE )
         IF (KEMIXT) THEN
           CALL JEVEUO ( CSPME, 'L', JSPME )
           CALL JEVEUO ( CSPTE, 'L', JSPTE )
         ENDIF
         IND = 0
         DO 320 I1 = 1, NBINST
            IND = IND + 1
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
               IF (KEMIXT) THEN
                 IR = IR + 1
                 VALE(IR) = ZR(JSPME-1+IND)
                 IR = IR + 1
                 VALE(IR) = ZR(JSPTE-1+IND)
               ENDIF
               IR = IR + 1
               VALE(IR) = ZR(JFAE-1+5*(IND-1)+1)
               IF (KEMIXT) THEN
                 IR = IR + 1
                 VALE(IR) =ZR(JFAE-1+5*(IND-1)+5)
               ENDIF
               IR = IR + 1
               VALE(IR) = ZR(JFAE-1+5*(IND-1)+2)
               IR = IR + 1
               VALE(IR) = ZR(JFAE-1+5*(IND-1)+3)
               IR = IR + 1
               VALE(IR) = ZR(JFAE-1+5*(IND-1)+4)
               CALL TBAJLI ( NOMRES,NPAR1,NOPARA, VAIE,VALE,C16B,VAKE,0)
 322        CONTINUE
 320     CONTINUE
C
         NOPARA(NPARA+1) = 'DOMMAGE_CUMU'
         NPAR1 = NPARA + 1
         IF ( NIV .EQ. 2 ) 
     &      WRITE(6,*) '******* ORIGINE DU SEGMENT *******'
         CALL RCEVFU ( CNOC, CFAO, DCO )
         IF ( NIV .EQ. 2 ) 
     &      WRITE(6,*) '******* EXTREMITE DU SEGMENT *******'
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
