      SUBROUTINE UTEST4 ( CHAMGD, TYPTES, TYPRES, NBREF, TBTXT, REFI,
     &              REFR, REFC, EPSI, LIGN1, LIGN2,CRIT, IFIC, NBCMP,
     &              NOCMP, SSIGNE )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER              NBREF, REFI(NBREF), IFIC, NBCMP
      REAL*8               REFR(NBREF), EPSI
      CHARACTER*8          TYPTES, NOCMP(*)
      CHARACTER*16         TBTXT(2)
      CHARACTER*(*)        CHAMGD, TYPRES, CRIT, SSIGNE
      CHARACTER*200        LIGN1,LIGN2
      COMPLEX*16           REFC(NBREF)
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
C ----------------------------------------------------------------------
C IN  : CHAMGD : NOM DU CHAM_GD
C IN  : TYPTES : TYPE DE TEST A EFFECTUER SUR LE CHAMP
C IN  : REFI   : VALEUR REELLE ENTIERE ATTENDUE
C IN  : REFR   : VALEUR REELLE ATTENDUE
C IN  : REFC   : VALEUR COMPLEXE ATTENDUE
C IN  : CRIT   : 'RELATIF' OU 'ABSOLU'(PRECISION RELATIVE OU ABSOLUE).
C IN  : EPSI   : PRECISION ESPEREE
C IN  : IFIC   : NUMERO LOGIQUE DU FICHIER DE SORTIE
C IN  : TBTXT  : (1)=REFERENCE, (2)=LEGENDE
C IN  : NODDL  : NOM DU DDL A TRAITER
C IN/OUT  : LIGN1  : PREMIERE LIGNE D'IMPRESSION DU RESULTAT
C IN/OUT  : LIGN2  : DEUXIEME LIGNE D'IMPRESSION DU RESULTAT
C OUT : IMPRESSION SUR LISTING
C ----------------------------------------------------------------------
      INTEGER       VALI, IBID, NEQ, I, J, K, IRET1, VALII, ICMP
      INTEGER       NCMP, VNOCMP, JCSD, JCSC, JCSV, JCSL, JCMP, IND
      INTEGER       LXLGUT,NL1,NL11,NL2,NL22
      REAL*8        VALR, VALRR
      COMPLEX*16    VALC
      CHARACTER*1   TYPREZ
      CHARACTER*24 VALK(3)
      CHARACTER*4   TYPE
      CHARACTER*8   TYCH, NODDL
      CHARACTER*19  CHAM19, CNSINR
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CHAM19 = CHAMGD
      TYPREZ = TYPRES(1:1)
C
      CALL WKVECT ( '&&UTEST4_CMP', 'V V I', NBCMP, JCMP )
C
C     -- LE CHAMP EXISTE-T-IL ?
C     =========================
      CALL DISMOI('C','TYPE_CHAMP',CHAM19,'CHAMP',IBID,TYCH,IRET1)
C
      CALL ASSERT(NBCMP.EQ.1)
C
      IF (TYCH(1:4).EQ.'NOEU') THEN
C         -------------------
         CNSINR = '&&UTEST4.CNSINR'
         CALL CNOCNS(CHAM19,'V',CNSINR)
         CALL JEVEUO(CNSINR//'.CNSV','L',JCSV)
         CALL JEVEUO(CNSINR//'.CNSC','L',JCSC)
         CALL JEVEUO(CNSINR//'.CNSL','L',JCSL)
         CALL JEVEUO(CNSINR//'.CNSD','L',JCSD)
         NCMP = ZI(JCSD-1+2)
         DO 10 I = 1 , NBCMP
            NODDL = NOCMP(I)
            DO 12 J =  1 , NCMP
               IF ( ZK8(JCSC-1+J).EQ.NODDL ) THEN
                  ZI(JCMP-1+I) = J
                  GOTO 10
               ENDIF
 12         CONTINUE
            CALL U2MESK('F','CALCULEL6_88', 1 ,NODDL)
 10      CONTINUE
         CALL JELIRA ( CNSINR//'.CNSV','TYPE', IBID, TYPE )
         CALL JELIRA ( CNSINR//'.CNSV','LONMAX', NEQ, TYPE )
         NEQ = NEQ / NCMP
         IF ( TYPE(1:1) .NE. TYPREZ ) THEN
            WRITE(IFIC,*) 'NOOK '
        VALK(1) = CHAM19
        VALK(2) = TYPE
        VALK(3) = TYPREZ
        CALL U2MESK('A','CALCULEL5_13', 3 ,VALK)
            GOTO 9999
         ENDIF
C
      ELSE IF (TYCH(1:2).EQ.'EL') THEN
C              -----------------
         CNSINR = '&&UTEST4.CNSINR'
         CALL CELCES(CHAM19,'V',CNSINR)
         CALL JEVEUO(CNSINR//'.CESV','L',JCSV)
         CALL JEVEUO(CNSINR//'.CESC','L',JCSC)
         CALL JEVEUO(CNSINR//'.CESL','L',JCSL)
         CALL JEVEUO(CNSINR//'.CESD','L',JCSD)
         NCMP = ZI(JCSD-1+2)
         DO 20 I = 1 , NBCMP
            NODDL = NOCMP(I)
            DO 22 J =  1 , NCMP
               IF ( ZK8(JCSC-1+J).EQ.NODDL ) THEN
                  ZI(JCMP-1+I) = J
                  GOTO 20
               ENDIF
 22         CONTINUE
            CALL U2MESK('F','CALCULEL6_88', 1 ,NODDL)
 20      CONTINUE
         CALL JELIRA ( CNSINR//'.CESV','TYPE', IBID, TYPE )
         CALL JELIRA ( CNSINR//'.CESV','LONMAX', NEQ, TYPE )
         NEQ = NEQ / NCMP
         IF ( TYPE(1:1) .NE. TYPREZ ) THEN
            WRITE(IFIC,*) 'NOOK '
        VALK(1) = CHAM19
        VALK(2) = TYPE
        VALK(3) = TYPREZ
        CALL U2MESK('A','CALCULEL5_13', 3 ,VALK)
            GOTO 9999
         ENDIF
      ELSE
         WRITE(IFIC,*) 'NOOK '
         CALL U2MESK('A','CALCULEL5_14',1,CHAM19)
      ENDIF
C
      NL1 = LXLGUT(LIGN1)
      LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' NOM_CMP'
      LIGN1(NL1+17:NL1+17)='.'

      IF ( TYPE .EQ. 'I' ) THEN
         IF ( TYPTES .EQ. 'SOMM_ABS' ) THEN
            VALI = 0
            DO 102 I = 1 , NBCMP
               VNOCMP = ZI(JCMP+I-1)
               DO 100 J = 1 , NEQ
                  IND = NCMP*(J-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALI = VALI + ABS( ZI(JCSV-1+IND) )
                  ENDIF
 100           CONTINUE
 102        CONTINUE
            NL2 = LXLGUT(LIGN2)
            LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//
     &                      ZK8(JCSC-1+ZI(JCMP))
            LIGN2(NL2+17:NL2+17)='.'

         ELSEIF ( TYPTES .EQ. 'SOMM' ) THEN
            VALI = 0
            DO 112 I = 1 , NBCMP
               VNOCMP = ZI(JCMP+I-1)
               DO 110 J = 1 , NEQ
                  IND = NCMP*(J-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALI = VALI + ZI(JCSV-1+IND)
                  ENDIF
 110           CONTINUE
 112        CONTINUE
            NL2 = LXLGUT(LIGN2)
            LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//
     &                      ZK8(JCSC-1+ZI(JCMP))
            LIGN2(NL2+17:NL2+17)='.'

         ELSEIF ( TYPTES .EQ. 'MAX' ) THEN
            DO 122 I = 1 , NBCMP
               VNOCMP = ZI(JCMP+I-1)
               DO 120 J = 1,NEQ
                  IND = NCMP*(J-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALII = ZI(JCSV-1+IND)
                     GOTO 124
                  ENDIF
 120           CONTINUE
 124           CONTINUE
               DO 126 K = J+1 , NEQ
                  IND = NCMP*(K-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALII = MAX( VALII , ZI(JCSV-1+IND) )
                  ENDIF
 126           CONTINUE
               IF(I.EQ.1)THEN
                  VALI=VALII
                  ICMP=1
               ELSE
                  IF(VALII.GT.VALI)THEN
                     VALI=VALII
                     ICMP=I
                   ENDIF
               ENDIF
 122        CONTINUE
            NL2 = LXLGUT(LIGN2)
            LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//
     &                      ZK8(JCSC-1+ZI(JCMP-1+ICMP))
            LIGN2(NL2+17:NL2+17)='.'

         ELSEIF ( TYPTES .EQ. 'MIN' ) THEN
            DO 132 I = 1 , NBCMP
               VNOCMP = ZI(JCMP+I-1)
               DO 130 J = 1,NEQ
                  IND = NCMP*(J-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALII = ZI(JCSV-1+IND)
                     GOTO 134
                  ENDIF
 130           CONTINUE
 134           CONTINUE
               DO 136 K = J+1 , NEQ
                  IND = NCMP*(K-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALII = MIN( VALII , ZI(JCSV-1+IND) )
                     ICMP=VNOCMP
                  ENDIF
 136           CONTINUE
               IF(I.EQ.1)THEN
                  VALI=VALII
                  ICMP=1
               ELSE
                  IF(VALII.LT.VALI)THEN
                     VALI=VALII
                     ICMP=I
                   ENDIF
                ENDIF
 132        CONTINUE
            NL2 = LXLGUT(LIGN2)
            LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//
     &                      ZK8(JCSC-1+ZI(JCMP-1+ICMP))
            LIGN2(NL2+17:NL2+17)='.'
         ELSE
            WRITE(IFIC,*) 'NOOK '
            CALL U2MESS('A','CALCULEL5_12')
            GOTO 9999
         ENDIF
C
      ELSEIF ( TYPE .EQ. 'R' ) THEN
         IF ( TYPTES .EQ. 'SOMM_ABS' ) THEN
            VALR = 0.D0
            DO 202 I = 1 , NBCMP
               VNOCMP = ZI(JCMP+I-1)
               DO 200 J = 1 , NEQ
                  IND = NCMP*(J-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALR = VALR + ABS( ZR(JCSV-1+IND) )
                  ENDIF
 200           CONTINUE
 202        CONTINUE
            NL2 = LXLGUT(LIGN2)
            LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//
     &                      ZK8(JCSC-1+ZI(JCMP))
            LIGN2(NL2+17:NL2+17)='.'
         ELSEIF ( TYPTES .EQ. 'SOMM' ) THEN
            VALR = 0.D0
            DO 212 I = 1 , NBCMP
               VNOCMP = ZI(JCMP+I-1)
               DO 210 J = 1 , NEQ
                  IND = NCMP*(J-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALR = VALR + ZR(JCSV-1+IND)
                  ENDIF
 210           CONTINUE
 212        CONTINUE
            NL2 = LXLGUT(LIGN2)
            LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//
     &                      ZK8(JCSC-1+ZI(JCMP))
            LIGN2(NL2+17:NL2+17)='.'
         ELSEIF ( TYPTES .EQ. 'MAX' ) THEN
            DO 222 I = 1 , NBCMP
               VNOCMP = ZI(JCMP+I-1)
               DO 220 J = 1 , NEQ
                  IND = NCMP*(J-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALRR = ZR(JCSV-1+IND)
                     GOTO 224
                  ENDIF
 220           CONTINUE
 224           CONTINUE
               DO 226 K = J+1 , NEQ
                  IND = NCMP*(K-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALRR = MAX( VALRR , ZR(JCSV-1+IND) )
                  ENDIF
 226           CONTINUE
               IF(I.EQ.1)THEN
                  VALR=VALRR
                  ICMP=1
               ELSE
                  IF(VALRR.GT.VALR)THEN
                     VALR=VALRR
                     ICMP=I
                  ENDIF
               ENDIF
 222        CONTINUE 
            NL2 = LXLGUT(LIGN2)
            LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//
     &                      ZK8(JCSC-1+ZI(JCMP-1+ICMP))
            LIGN2(NL2+17:NL2+17)='.'

         ELSEIF ( TYPTES .EQ. 'MIN' ) THEN
            DO 232 I = 1 , NBCMP
               VNOCMP = ZI(JCMP+I-1)
               DO 230 J = 1 , NEQ
                  IND = NCMP*(J-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALRR = ZR(JCSV-1+IND)
                     GOTO 234
                  ENDIF
 230           CONTINUE
 234           CONTINUE
               DO 236 K = J+1 , NEQ
                  IND = NCMP*(K-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALRR = MIN( VALRR , ZR(JCSV-1+IND) )
                  ENDIF
 236           CONTINUE
               IF(I.EQ.1)THEN
                  VALR=VALRR
                  ICMP=1
               ELSE
                  IF(VALRR.LT.VALR)THEN
                     VALR=VALRR
                     ICMP=I
                  ENDIF
               ENDIF
 232        CONTINUE
            NL2 = LXLGUT(LIGN2)
            LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//
     &                      ZK8(JCSC-1+ZI(JCMP-1+ICMP))
            LIGN2(NL2+17:NL2+17)='.'
         ELSE
            WRITE(IFIC,*) 'NOOK '
            CALL U2MESS('A','CALCULEL5_12')
            GOTO 9999
         ENDIF
C
      ELSEIF ( TYPE .EQ. 'C' ) THEN
         IF ( TYPTES .EQ. 'SOMM_ABS' ) THEN
            VALR = 0.D0
            DO 302 I = 1 , NBCMP
               VNOCMP = ZI(JCMP+I-1)
               DO 300 J = 1 , NEQ
                  IND = NCMP*(J-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALR = VALR + ABS( ZC(JCSV-1+IND) )
                  ENDIF
 300           CONTINUE
 302        CONTINUE
            NL2 = LXLGUT(LIGN2)
            LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//
     &                      ZK8(JCSC-1+ZI(JCMP))
            LIGN2(NL2+17:NL2+17)='.'
         ELSEIF ( TYPTES .EQ. 'SOMM' ) THEN
            VALC = DCMPLX(0.D0,0.D0)
            DO 312 I = 1 , NBCMP
               VNOCMP = ZI(JCMP+I-1)
               DO 310 J = 1 , NEQ
                  IND = NCMP*(J-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALC = VALC + ZC(JCSV-1+IND)
                  ENDIF
 310           CONTINUE
 312        CONTINUE
            NL2 = LXLGUT(LIGN2)
            LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//
     &                      ZK8(JCSC-1+ZI(JCMP))
            LIGN2(NL2+17:NL2+17)='.'
         ELSE
            WRITE(IFIC,*) 'NOOK '
            CALL U2MESS('A','CALCULEL5_12')
            GOTO 9999
         ENDIF
      ENDIF
C
      NL1 = LXLGUT(LIGN1)
      NL11 = LXLGUT(LIGN1(1:NL1-1))
      NL2 = LXLGUT(LIGN2)
      NL22 = LXLGUT(LIGN2(1:NL2-1))

      IF(NL11.LT.80)THEN
          WRITE (IFIC,*) LIGN1(1:NL11)
      ELSEIF(NL11.LT.160)THEN
          WRITE (IFIC,1160) LIGN1(1:80),LIGN1(81:NL11)
      ELSE
          WRITE (IFIC,1200) LIGN1(1:80),LIGN1(81:160),LIGN1(161:NL11)
      ENDIF

      IF(NL22.LT.80)THEN
          WRITE (IFIC,*) LIGN2(1:NL22)
      ELSEIF(NL22.LT.160)THEN
          WRITE (IFIC,1160) LIGN2(1:80),LIGN2(81:NL22)
      ELSE
          WRITE (IFIC,1200) LIGN2(1:80),LIGN2(81:160),LIGN2(161:NL22)
      ENDIF

      CALL UTITES ( TBTXT(1), TBTXT(2), TYPRES, NBREF, REFI, REFR, REFC,
     +              VALI, VALR, VALC, EPSI, CRIT, IFIC, SSIGNE )
C
      CALL DETRSD('CHAM_NO_S',CNSINR)
 9999 CONTINUE
      CALL JEDETR ( '&&UTEST4_CMP' )
C
1160  FORMAT(1X,A80,A)
1200  FORMAT(1X,2(A80),A)

      CALL JEDEMA()
      END
