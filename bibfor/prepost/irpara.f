      SUBROUTINE IRPARA(RESU,FORM,IFI,NBORDR,ORDR,NBPA,NOMPAR,CECR)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'jeveux.h'
      CHARACTER*(*)                               NOMPAR(*)
      CHARACTER*(*)                                            CECR
      CHARACTER*(*)       RESU,FORM
      INTEGER                          NBORDR,ORDR(*),NBPA
C     ------------------------------------------------------------------
C MODIF PREPOST  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C TOLE CRS_602
C     IMPRESSION DES PARAMETRES
C     ------------------------------------------------------------------
C IN  RESU   : K8  : NOM DU CONCEPT
C IN  FORM   : K8  : FORMAT D'ECRITURE
C IN  IFI    : IS  : UNITE LOGIQUE D'ECRITURE
C IN  NBORDR : I   : NOMBRE D'ORDRE
C IN  ORDR   : I   : LISTE DES NUMEROS D'ORDRE
C IN  NBPA   : I   : NOMBRE DE PARAMETRES
C IN  NOMPAR : K16 : NOM DES PARAMETRES
C IN  CECR   : K1  : CODE D'ECRITURE DES PARAMETRES 'L' LISTE
C                                                   'T' TABLEAU
C                                                   'E' EXCEL
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
      INTEGER        NECRI, NECRR, IUNDF
      REAL*8         RUNDF
      CHARACTER*4    CTYPE, CHFIN
      CHARACTER*8    FORM1
      CHARACTER*104  TOTO
      CHARACTER*2000 TITI
C     ------------------------------------------------------------------
C
C     --- INITIALISATION ---
      CALL JEMARQ()
      IUNDF  = ISNNEM()
      RUNDF  = R8VIDE()
      TOTO = ' '
C
C      --- IMPRESSION DES PARAMETRES AU FORMAT RESULTAT ---
      IF(NBPA.NE.0) THEN
        IF (FORM.EQ.'RESULTAT') THEN
C
C     --- STOCKAGE DES NOMS ET VALEURS DES PARAMETRES A IMPRIMER ---
C
          NECRI  = 0
          NECRR  = 0
          NECK8  = 0
          NECK16 = 0
          NECK24 = 0
          NECK32 = 0
          NECK80 = 0
          CALL WKVECT('&&IRPARA.NOMI_PARA'  ,'V V K16',NBPA,LNIPA)
          CALL WKVECT('&&IRPARA.NOMR_PARA'  ,'V V K16',NBPA,LNRPA)
          CALL WKVECT('&&IRPARA.NOMK8_PARA' ,'V V K16',NBPA,LK8PA)
          CALL WKVECT('&&IRPARA.NOMK16_PARA','V V K16',NBPA,LK16PA)
          CALL WKVECT('&&IRPARA.NOMK24_PARA','V V K16',NBPA,LK24PA)
          CALL WKVECT('&&IRPARA.NOMK32_PARA','V V K16',NBPA,LK32PA)
          CALL WKVECT('&&IRPARA.NOMK80_PARA','V V K16',NBPA,LK80PA)
          DO 120 IPA=1,NBPA
            CALL RSADPA(RESU,'L',1,NOMPAR(IPA),ORDR(1),1,IAD,CTYPE)
            IF (CTYPE(1:1).EQ.'I' .AND .ZI(IAD).NE.IUNDF)THEN
               ZK16(LNIPA+NECRI) = NOMPAR(IPA)
               NECRI = NECRI + 1
            ELSEIF (CTYPE(1:1).EQ.'I') THEN
            ELSEIF (CTYPE(1:1).EQ.'R' ) THEN
               IF (ZR(IAD).NE.RUNDF) THEN
                 ZK16(LNRPA+NECRR) = NOMPAR(IPA)
                 NECRR = NECRR + 1
               ENDIF
            ELSEIF (CTYPE(1:1).EQ.'R') THEN
            ELSEIF (CTYPE(1:2).EQ.'K8') THEN
               ZK16(LK8PA+NECK8) = NOMPAR(IPA)
               NECK8 = NECK8 + 1
            ELSEIF (CTYPE(1:3).EQ.'K16') THEN
               ZK16(LK16PA+NECK16) = NOMPAR(IPA)
               NECK16 = NECK16 + 1
            ELSEIF (CTYPE(1:3).EQ.'K24') THEN
               ZK16(LK24PA+NECK24) = NOMPAR(IPA)
               NECK24 = NECK24 + 1
            ELSEIF (CTYPE(1:3).EQ.'K32') THEN
               ZK16(LK32PA+NECK32) = NOMPAR(IPA)
               NECK32 = NECK32 + 1
            ELSEIF (CTYPE(1:3).EQ.'K80') THEN
               ZK16(LK80PA+NECK80) = NOMPAR(IPA)
               NECK80 = NECK80 + 1
            ELSE
               CALL U2MESK('A','PREPOST3_6',1,CTYPE)
            ENDIF
 120      CONTINUE
C
C     --- IMPRESSION DES PARAMETRES ----
C
          WRITE(IFI,'(/)')
          IF(CECR(1:1).EQ.'L') THEN
C            ----------------
            WRITE(IFI,'(1X,3A)') 'IMPRESSION DES PARAMETRES DU ',
     &        'CONCEPT ',RESU
C
            DO 200 IORD=1,NBORDR
C
            WRITE(IFI,'(1X,A,I4,/)') 'POUR LE NUMERO D''ORDRE ',
     &                                ORDR(IORD)
              IF(NECRI.NE.0) THEN
                DO 202 IEC = 1,NECRI
                   CALL RSADPA(RESU,'L',1,ZK16(LNIPA-1+IEC),ORDR(IORD),
     &                          1,IAD,CTYPE)
                   WRITE(IFI,'(14X,A,I12)') ZK16(LNIPA-1+IEC),ZI(IAD)
 202            CONTINUE
              ENDIF
              IF(NECRR.NE.0) THEN
                DO 204 IEC = 1,NECRR
                   CALL RSADPA(RESU,'L',1,ZK16(LNRPA-1+IEC),ORDR(IORD),
     &                          1,IAD,CTYPE)
                   WRITE(IFI,'(14X,A,1PE12.5)') ZK16(LNRPA-1+IEC),
     &                                         ZR(IAD)
 204            CONTINUE
              ENDIF
              IF(NECK8.NE.0) THEN
                DO 206 IEC = 1,NECK8
                   CALL RSADPA(RESU,'L',1,ZK16(LK8PA-1+IEC),ORDR(IORD),
     &                          1,IAD,CTYPE)
                   WRITE(IFI,'(14X,A,1X,A)') ZK16(LK8PA-1+IEC),ZK8(IAD)
 206            CONTINUE
              ENDIF
              IF(NECK16.NE.0) THEN
                DO 208 IEC = 1,NECK16
                   CALL RSADPA(RESU,'L',1,ZK16(LK16PA-1+IEC),ORDR(IORD),
     &                          1,IAD,CTYPE)
                  WRITE(IFI,'(14X,A,1X,A)') ZK16(LK16PA-1+IEC),ZK16(IAD)
 208            CONTINUE
              ENDIF
              IF(NECK24.NE.0) THEN
                DO 210 IEC = 1,NECK24
                   CALL RSADPA(RESU,'L',1,ZK16(LK24PA-1+IEC),ORDR(IORD),
     &                          1,IAD,CTYPE)
                  WRITE(IFI,'(14X,A,1X,A)') ZK16(LK24PA-1+IEC),ZK24(IAD)
 210            CONTINUE
              ENDIF
              IF(NECK32.NE.0) THEN
                DO 212 IEC = 1,NECK32
                   CALL RSADPA(RESU,'L',1,ZK16(LK32PA-1+IEC),ORDR(IORD),
     &                          1,IAD,CTYPE)
                  WRITE(IFI,'(14X,A,1X,A)') ZK16(LK32PA-1+IEC),ZK32(IAD)
 212            CONTINUE
              ENDIF
              IF(NECK80.NE.0) THEN
                DO 214 IEC = 1,NECK80
                   CALL RSADPA(RESU,'L',1,ZK16(LK80PA-1+IEC),ORDR(IORD),
     &                          1,IAD,CTYPE)
                   WRITE(IFI,'(14X,A)') ZK16(LK32PA-1+IEC)
                   WRITE(IFI,'(1X,A)')  ZK80(IAD)
 214            CONTINUE
              ENDIF
 200        CONTINUE
C
          ELSEIF(CECR(1:1).EQ.'T') THEN
C                ----------------
            WRITE(IFI,'(1X,A,4(1X,A),/,(13X,4(1X,A)))')
     &          'NUMERO_ORDRE',(ZK16(LNIPA-1+IECI),IECI=1,NECRI),
     &                         (ZK16(LNRPA-1+IECR),IECR=1,NECRR),
     &                         (ZK16(LK8PA-1+IK8),IK8=1,NECK8),
     &                         (ZK16(LK16PA-1+IK16),IK16=1,NECK16)
            WRITE(IFI,'(14X,2(A,9X))')
     &                         (ZK16(LK24PA-1+IK24),IK24=1,NECK24)
            WRITE(IFI,'(14X,2(A,17X))')
     &                         (ZK16(LK32PA-1+IK32),IK32=1,NECK32)
            WRITE(IFI,'(1X,A)')
     &                         (ZK16(LK80PA-1+IK80),IK80=1,NECK80)
            DO 300 IORD=1,NBORDR
             I = 1
             WRITE(TOTO(I:I+13),1000) ORDR(IORD)
             I=14
             IF (NECRI.NE.0) THEN
               DO 302 IEC = 1,NECRI
                 CALL RSADPA(RESU,'L',1,ZK16(LNIPA-1+IEC),ORDR(IORD),
     &                         1,IAD,CTYPE)
                 WRITE(TOTO(I:I+16),1001) ZI(IAD)
                 I=I+17
                 IF(I.GE.68)  THEN
                    WRITE(IFI,'(A)') TOTO
                    TOTO=' '
                    I=14
                 ENDIF
 302           CONTINUE
             ENDIF
             IF (NECRR.NE.0) THEN
               DO 304 IEC = 1,NECRR
                 CALL RSADPA(RESU,'L',1,ZK16(LNRPA-1+IEC),ORDR(IORD),
     &                         1,IAD,CTYPE)
                 WRITE(TOTO(I:I+16),1002) ZR(IAD)
                 I=I+17
                 IF(I.GE.68)  THEN
                   WRITE(IFI,'(A)') TOTO
                   TOTO=' '
                   I=14
                 ENDIF
 304           CONTINUE
             ENDIF
             IF (NECK8.NE.0) THEN
               DO 306 IEC = 1,NECK8
                 CALL RSADPA(RESU,'L',1,ZK16(LK8PA-1+IEC),ORDR(IORD),
     &                         1,IAD,CTYPE)
                 WRITE(TOTO(I:I+16),1008) ZK8(IAD)
                 I=I+17
                 IF(I.GE.68)  THEN
                    WRITE(IFI,'(A)') TOTO
                    TOTO=' '
                    I=14
                 ENDIF
 306           CONTINUE
             ENDIF
             IF (NECK16.NE.0) THEN
               DO 308 IEC = 1,NECK16
                 CALL RSADPA(RESU,'L',1,ZK16(LK16PA-1+IEC),ORDR(IORD),
     &                         1,IAD,CTYPE)
                 WRITE(TOTO(I:I+16),1016) ZK16(IAD)
                 I=I+17
                 IF(I.GE.68)  THEN
                    WRITE(IFI,'(A)') TOTO
                    TOTO=' '
                    I=14
                 ENDIF
 308           CONTINUE
             ENDIF
             IF (NECK24.NE.0) THEN
               IF (I.NE.14) THEN
                  WRITE(IFI,'(A)') TOTO
                  TOTO=' '
               ENDIF
               I=14
               DO 310 IEC = 1,NECK24
                 CALL RSADPA(RESU,'L',1,ZK16(LK24PA-1+IEC),ORDR(IORD),
     &                         1,IAD,CTYPE)
                 WRITE(TOTO(I:I+24),1024) ZK24(IAD)
                 I=I+25
                 IF(I.GE.50)  THEN
                    WRITE(IFI,'(A)') TOTO
                    TOTO=' '
                    I=14
                 ENDIF
 310           CONTINUE
             ENDIF
             IF (NECK32.NE.0) THEN
               IF (I.NE.14) THEN
                  WRITE(IFI,'(A)') TOTO
                  TOTO=' '
               ENDIF
               I=14
               DO 312 IEC = 1,NECK32
                 CALL RSADPA(RESU,'L',1,ZK16(LK32PA-1+IEC),ORDR(IORD),
     &                         1,IAD,CTYPE)
                 WRITE(TOTO(I:I+32),1032) ZK32(IAD)
                 I=I+33
                 IF(I.GE.64)  THEN
                    WRITE(IFI,'(A)') TOTO
                    TOTO=' '
                    I=14
                 ENDIF
 312           CONTINUE
             ENDIF
             IF (NECK80.NE.0) THEN
               IF (I.NE.14) THEN
                  WRITE(IFI,'(A)') TOTO
                  TOTO=' '
               ENDIF
               DO 314 IEC = 1,NECK80
                 CALL RSADPA(RESU,'L',1,ZK16(LK80PA-1+IEC),ORDR(IORD),
     &                         1,IAD,CTYPE)
                 WRITE(IFI,'(A)') ZK80(IAD)
 314           CONTINUE
             ENDIF
             IF(TOTO.NE.' ')  WRITE(IFI,'(A)') TOTO
 300        CONTINUE
C
          ELSEIF(CECR(1:1).EQ.'E') THEN
C                ----------------
            TITI = ' '
            TITI(1:12) = 'NUMERO_ORDRE'
            I = 14
            DO 402 IECI = 1,NECRI
               TITI(I:I+15) = ZK16(LNIPA-1+IECI)
               I = I + 17
 402        CONTINUE
            DO 404 IECR = 1,NECRR
               TITI(I:I+15) = ZK16(LNRPA-1+IECR)
               I = I + 17
 404        CONTINUE
            DO 406 IK8 = 1,NECK8
               TITI(I:I+15) = ZK16(LK8PA-1+IK8)
               I = I + 17
 406        CONTINUE
            DO 408 IK16 = 1,NECK16
               TITI(I:I+15) = ZK16(LK16PA-1+IK16)
               I = I + 17
 408        CONTINUE
            DO 410 IK24 = 1,NECK24
               TITI(I:I+15) = ZK16(LK24PA-1+IK24)
               I = I + 25
 410        CONTINUE
            DO 412 IK32 = 1,NECK32
               TITI(I:I+15) = ZK16(LK32PA-1+IK32)
               I = I + 33
 412        CONTINUE
            DO 414 IK80 = 1,NECK80
               TITI(I:I+15) = ZK16(LK80PA-1+IK80)
               I = I + 81
 414        CONTINUE
            CALL CODENT ( I, 'G', CHFIN )
            FORM1 = '(A'//CHFIN//')'
            WRITE(IFI,FORM1) TITI(1:I)
C
            DO 420 IORD=1,NBORDR
              TITI = ' '
              WRITE(TITI(1:12),'(I12)') ORDR(IORD)
              I = 14
              DO 422 IEC = 1 , NECRI
                CALL RSADPA(RESU,'L',1,ZK16(LNIPA-1+IEC),ORDR(IORD),
     &                         1,IAD,CTYPE)
                WRITE(TITI(I:I+15),'(I12)') ZI(IAD)
                I = I + 17
 422          CONTINUE
              DO 424 IEC = 1,NECRR
                CALL RSADPA(RESU,'L',1,ZK16(LNRPA-1+IEC),ORDR(IORD),
     &                         1,IAD,CTYPE)
                WRITE(TITI(I:I+15),'(1PD12.5)') ZR(IAD)
                I = I + 17
 424          CONTINUE
              DO 426 IEC = 1,NECK8
                CALL RSADPA(RESU,'L',1,ZK16(LK8PA-1+IEC),ORDR(IORD),
     &                         1,IAD,CTYPE)
                TITI(I:I+15) = ZK8(IAD)
                I = I + 17
 426          CONTINUE
              DO 428 IEC = 1,NECK16
                CALL RSADPA(RESU,'L',1,ZK16(LK16PA-1+IEC),ORDR(IORD),
     &                         1,IAD,CTYPE)
                TITI(I:I+15) = ZK16(IAD)
                I = I + 17
 428          CONTINUE
              DO 430 IEC = 1,NECK24
                CALL RSADPA(RESU,'L',1,ZK16(LK24PA-1+IEC),ORDR(IORD),
     &                         1,IAD,CTYPE)
                TITI(I:I+23) = ZK24(IAD)
                I = I + 25
 430          CONTINUE
              DO 432 IEC = 1,NECK32
                 CALL RSADPA(RESU,'L',1,ZK16(LK32PA-1+IEC),ORDR(IORD),
     &                         1,IAD,CTYPE)
                TITI(I:I+31) = ZK32(IAD)
                I = I + 33
 432          CONTINUE
              DO 434 IEC = 1,NECK80
                 CALL RSADPA(RESU,'L',1,ZK16(LK80PA-1+IEC),ORDR(IORD),
     &                         1,IAD,CTYPE)
                TITI(I:I+79) = ZK80(IAD)
                I = I + 81
 434          CONTINUE
              CALL CODENT ( I, 'G', CHFIN )
              FORM1 = '(A'//CHFIN//')'
              WRITE(IFI,FORM1) TITI(1:I)
 420        CONTINUE
          ENDIF
        ENDIF
      ENDIF
C
      CALL JEDETR('&&IRPARA.NOMI_PARA')
      CALL JEDETR('&&IRPARA.NOMR_PARA')
      CALL JEDETR('&&IRPARA.NOMK8_PARA')
      CALL JEDETR('&&IRPARA.NOMK16_PARA')
      CALL JEDETR('&&IRPARA.NOMK24_PARA')
      CALL JEDETR('&&IRPARA.NOMK32_PARA')
      CALL JEDETR('&&IRPARA.NOMK80_PARA')
 1000  FORMAT(I12,1X)
 1001  FORMAT(I12,5X)
 1002  FORMAT(1PD12.5,5X)
 1008  FORMAT(A,9X)
 1016  FORMAT(A,1X)
 1024  FORMAT(1X,A)
 1032  FORMAT(1X,A)
      CALL JEDEMA()
      END
