      SUBROUTINE UTEST4 ( CHAMGD, TYPTES, TYPRES, NBREF, TBTXT, REFI,
     +              REFR, REFC, EPSI, CRIT, IFIC, NBCMP, NOCMP, SSIGNE )
      IMPLICIT   NONE
      INTEGER              NBREF, REFI(NBREF), IFIC, NBCMP
      REAL*8               REFR(NBREF), EPSI
      CHARACTER*8          TYPTES, NOCMP(*)
      CHARACTER*16         TBTXT(2)
      CHARACTER*(*)        CHAMGD, TYPRES, CRIT, SSIGNE
      COMPLEX*16           REFC(NBREF)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 01/02/2010   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C OUT : IMPRESSION SUR LISTING
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER       VALI, IBID, NEQ, I, J, K, IRET1
      INTEGER       NCMP, VNOCMP, JCSD, JCSC, JCSV, JCSL, JCMP, IND
      REAL*8        VALR
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
      CALL DISMOI('A','TYPE_CHAMP',CHAM19,'CHAMP',IBID,TYCH,IRET1)
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
         ELSEIF ( TYPTES .EQ. 'MAX' ) THEN
            DO 122 I = 1 , NBCMP
               VNOCMP = ZI(JCMP+I-1)
               DO 120 J = 1,NEQ
                  IND = NCMP*(J-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALI = ZI(JCSV-1+IND)
                     GOTO 124
                  ENDIF
 120           CONTINUE
 124           CONTINUE
               DO 126 K = J+1 , NEQ
                  IND = NCMP*(K-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALI = MAX( VALI , ZI(JCSV-1+IND) )
                  ENDIF
 126           CONTINUE
 122        CONTINUE
         ELSEIF ( TYPTES .EQ. 'MIN' ) THEN
            DO 132 I = 1 , NBCMP
               VNOCMP = ZI(JCMP+I-1)
               DO 130 J = 1,NEQ
                  IND = NCMP*(J-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALI = ZI(JCSV-1+IND)
                     GOTO 134
                  ENDIF
 130           CONTINUE
 134           CONTINUE
               DO 136 K = J+1 , NEQ
                  IND = NCMP*(K-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALI = MIN( VALI , ZI(JCSV-1+IND) )
                  ENDIF
 136           CONTINUE
 132        CONTINUE
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
         ELSEIF ( TYPTES .EQ. 'MAX' ) THEN
            DO 222 I = 1 , NBCMP
               VNOCMP = ZI(JCMP+I-1)
               DO 220 J = 1 , NEQ
                  IND = NCMP*(J-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALR = ZR(JCSV-1+IND)
                     GOTO 224
                  ENDIF
 220           CONTINUE
 224           CONTINUE
               DO 226 K = J+1 , NEQ
                  IND = NCMP*(K-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALR = MAX( VALR , ZR(JCSV-1+IND) )
                  ENDIF
 226           CONTINUE
 222        CONTINUE
         ELSEIF ( TYPTES .EQ. 'MIN' ) THEN
            DO 232 I = 1 , NBCMP
               VNOCMP = ZI(JCMP+I-1)
               DO 230 J = 1 , NEQ
                  IND = NCMP*(J-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALR = ZR(JCSV-1+IND)
                     GOTO 234
                  ENDIF
 230           CONTINUE
 234           CONTINUE
               DO 236 K = J+1 , NEQ
                  IND = NCMP*(K-1)+(VNOCMP-1)+1
                  IF ( ZL(JCSL-1+IND) ) THEN
                     VALR = MIN( VALR , ZR(JCSV-1+IND) )
                  ENDIF
 236           CONTINUE
 232        CONTINUE
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
         ELSE
            WRITE(IFIC,*) 'NOOK '
            CALL U2MESS('A','CALCULEL5_12')
            GOTO 9999
         ENDIF
      ENDIF
C
      CALL UTITES ( TBTXT(1), TBTXT(2), TYPRES, NBREF, REFI, REFR, REFC,
     +              VALI, VALR, VALC, EPSI, CRIT, IFIC, SSIGNE )
C
      CALL DETRSD('CHAM_NO_S',CNSINR)
 9999 CONTINUE
      CALL JEDETR ( '&&UTEST4_CMP' )
C
      CALL JEDEMA()
      END
