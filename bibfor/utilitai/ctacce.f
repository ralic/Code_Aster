      SUBROUTINE CTACCE(NSYMB,TYPAC,NBVAL,NIVAL,NRVAL,NIORD,NKCHA,
     &                   RESU)
      IMPLICIT   NONE
      INTEGER                        NBVAL
      CHARACTER*24                         NIVAL,NRVAL,NIORD,NKCHA
      CHARACTER*8              TYPAC,RESU,CHAM
      CHARACTER*16        NSYMB
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 07/12/2010   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ----- OPERATEUR CREA_TABLE , MOT-CLE FACTEUR RESU   --------------
C
C        BUT : RECUPERER LES ACCES DE LA SD RESULTATS
C
C        IN/OUT : NIVAL (K24): OBJET DES VALEURS D'ACCES (ENTIERS)
C                 NRVAL (K24): OBJET DES VALEURS D'ACCES (REELS)
C                 NIORD (K24): OBJET DES NUMEROS D'ORDRE
C                 NKCHA (K24): OBJET DES NOMS DE CHAMP
C           OUT : RESU  (K8) : NOM DU RESULTAT (SI RESULTAT, SINON ' ')
C                 TYPAC (K8) : ACCES (ORDRE,MODE,FREQ,INST)
C                 NSYMB (K16): NOM SYMBOLIQUE DU CHAMP
C                 NBVAL (I)  : NOMBRE DE VALEURS D'ACCES
C
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
      CHARACTER*16              ZK16,CONCEP
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      CHARACTER*8 K8B
      INTEGER N0,N1,JKCHA,JRVAL,JIVAL,JNIORD,NBTO,NBNO,NBLO,NBNI,NBLI
      INTEGER NBNM,NBLM,NBNF,NBLF,NBIS,NBR8,JLIST,IBID,NBTROU,I
      INTEGER NBCMP,VALI,N2,JINST,KK,NUORD
      REAL*8  R8B,EPSI,VALR,RINST
      COMPLEX*16 CBID
      CHARACTER*4 TYCH
      CHARACTER*8 CRIT
      CHARACTER*16 VALK
      CHARACTER*24 NLIST
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL GETVID('RESU','CHAM_GD'  ,1,1,0,K8B,N0)
      CALL GETVID('RESU','RESULTAT'  ,1,1,0,K8B,N1)
C
C =============================================================
C -1- CAS: CHAM_GD
C =============================================================

      IF(N0.NE.0)THEN
C
           CALL WKVECT(NKCHA,'V V K24',1,JKCHA)
           CALL GETVID('RESU','CHAM_GD'  ,1,1,1,ZK24(JKCHA),N0)
           CALL WKVECT(NRVAL,'V V R',1,JRVAL)
           ZR(JRVAL) = 0.0D0
           CALL WKVECT(NIVAL,'V V I',1,JIVAL)
           ZI(JIVAL) = 0
           CALL WKVECT(NIORD,'V V I',1,JNIORD)
           ZI(JNIORD)=0
           NBVAL=1
           TYPAC = ' '
           NSYMB = ' '
           RESU  = ' '

C =============================================================
C -2- CAS: RESULTAT/NOM_CHAM
C =============================================================
      ELSEIF(N1.NE.0)THEN
C
C --- 2.1- ON DETERMINE :
C     ------------------
C         NBVAL = NOMBRE DE VALEURS D'ACCES
C         TYPAC = TYPE D'ACCES (ORDRE,INST,FREQ,MODE,TOUT)
C         NIVAL = TABLEAU DES VALEURS D'ACCES (ENTIERES)
C         NRVAL = TABLEAU DES VALEURS D'ACCES (REELLES)
C         NKCHA = TABLEAU DES NOMS DE CHAMP
C
        CALL GETVID('RESU','RESULTAT'  ,1,1,1,RESU,N1)
C
        CALL GETVR8('RESU','PRECISION' ,1,1,1,EPSI,N1)
        CALL GETVTX('RESU','CRITERE'   ,1,1,1,CRIT,N1)
        CALL GETVTX('RESU','TOUT_ORDRE',1,1,0,K8B, NBTO)
        CALL GETVIS('RESU','NUME_ORDRE',1,1,0,IBID,NBNO)
        CALL GETVID('RESU','LIST_ORDRE',1,1,0,K8B ,NBLO)
        CALL GETVR8('RESU','INST'      ,1,1,0,R8B ,NBNI)
        CALL GETVID('RESU','LIST_INST' ,1,1,0,K8B ,NBLI)
        CALL GETVIS('RESU','MODE'      ,1,1,0,IBID,NBNM)
        CALL GETVID('RESU','LIST_MODE' ,1,1,0,K8B ,NBLM)
        CALL GETVR8('RESU','FREQ'      ,1,1,0,R8B ,NBNF)
        CALL GETVID('RESU','LIST_FREQ' ,1,1,0,K8B ,NBLF)
C
        NBTO=-NBTO
        NBNO=-NBNO
        NBLO=-NBLO
        NBNI=-NBNI
        NBLI=-NBLI
        NBNM=-NBNM
        NBLM=-NBLM
        NBNF=-NBNF
        NBLF=-NBLF
        NBIS=NBNO+NBLO+NBNM+NBLM
        NBR8=NBNI+NBLI+NBNF+NBLF
C
C    -- ACCES PAR ORDRE, MODE, LIST_ORDRE, LIST_MODE :
        IF ( NBIS .NE. 0 ) THEN
          CALL WKVECT(NRVAL,'V V R',1,JRVAL)
          ZR(JRVAL) = 0.0D0
C        -- NUME_ORDRE
          IF (NBNO .NE. 0 ) THEN
            TYPAC = 'ORDRE'
            NBVAL =  NBNO
            CALL WKVECT(NIVAL,'V V I',NBNO,JIVAL)
            CALL GETVIS('RESU','NUME_ORDRE',1,1,NBNO,ZI(JIVAL),N1)
C        -- MODE
          ELSE IF (NBNM .NE. 0 ) THEN
            TYPAC = 'MODE'
            NBVAL =  NBNM
            CALL WKVECT(NIVAL,'V V I',NBNM,JIVAL)
            CALL GETVIS('RESU','MODE',1,1,NBNM,ZI(JIVAL),N1)
C        -- LIST_ORDRE, LIST_MODE
          ELSE IF (NBLO .NE. 0 ) THEN
            IF(NBLO.NE.0)THEN
               TYPAC = 'ORDRE'
               CALL GETVID('RESU','LIST_ORDRE',1,1,1,NLIST,N1)
            ELSE
               TYPAC = 'MODE'
               CALL GETVID('RESU','LIST_MODE',1,1,1,NLIST,N1)
            ENDIF
            NLIST(20:24) = '.VALE'
            CALL JELIRA(NLIST,'LONMAX',NBVAL,K8B)
            CALL JEVEUO(NLIST,'L',JLIST)
            CALL WKVECT(NIVAL,'V V I',NBVAL,JIVAL)
            DO 10 I = 1,NBVAL
               ZI(JIVAL+I-1) = ZI(JLIST+I-1)
10          CONTINUE
          ENDIF
C
C    -- ACCES PAR INST, FREQ, LIST_INST, LIST_FREQ :
        ELSEIF ( NBR8 .NE. 0 ) THEN
          CALL WKVECT(NIVAL,'V V I',1,JIVAL)
          ZI(JIVAL) = 0
C        -- INST
          IF (NBNI .NE. 0 ) THEN
            TYPAC = 'INST'
            NBVAL =  NBNI
            CALL WKVECT(NRVAL,'V V R',NBNI,JRVAL)
            CALL GETVR8('RESU','INST',1,1,NBNI,ZR(JRVAL),N1)
C        -- FREQ
          ELSEIF (NBNF .NE. 0 ) THEN
            TYPAC = 'FREQ'
            NBVAL =  NBNF
            CALL WKVECT(NRVAL,'V V R',NBNF,JRVAL)
            CALL GETVR8('RESU','FREQ',1,1,NBNF,ZR(JRVAL),N1)
          ELSE
            IF ( NBLI .NE. 0 ) THEN
               TYPAC = 'INST'
               CALL GETVID('RESU','LIST_INST',1,1,1,NLIST,N1)
            ELSE
               TYPAC = 'FREQ'
               CALL GETVID('RESU','LIST_FREQ',1,1,1,NLIST,N1)
            ENDIF
            NLIST(20:24) = '.VALE'
            CALL JELIRA(NLIST,'LONMAX',NBVAL,K8B)
            CALL JEVEUO(NLIST,'L',JLIST)
            CALL WKVECT(NRVAL,'V V R',NBVAL,JRVAL)
            DO 20 I = 1,NBVAL
               ZR(JRVAL+I-1)=ZR(JLIST+I-1)
20          CONTINUE
          ENDIF
C
C    -- ACCES TOUT_ORDRE :
        ELSE

          CBID = DCMPLX(0,0)
          CALL RSORAC(RESU,'LONUTI',IBID,R8B,K8B,CBID,R8B,K8B,
     &               NBVAL,1,NBTROU)
          CALL WKVECT(NIVAL,'V V I',NBVAL,JIVAL)
          CALL RSORAC(RESU,'TOUT_ORDRE',IBID,R8B,K8B,CBID,R8B,K8B,
     &               ZI(JIVAL),NBVAL,NBTROU)

C         -- SI LE RESULTAT EST UN EVOL_XXX, ON FORCE TYPAC='INST'
C            POUR QUE LES INSTANTS SOIENT IMPRIMES DANS LA TABLE :
          CALL GETTCO(RESU,CONCEP)
          IF (CONCEP(1:5).EQ.'EVOL_') THEN
            TYPAC='INST'
            CALL WKVECT(NRVAL,'V V R',NBVAL,JRVAL)
            DO 25, KK=1,NBVAL
              NUORD=ZI(JIVAL-1+KK)
              CALL RSADPA(RESU,'L',1,'INST',NUORD,0,JINST,K8B)
              RINST=ZR(JINST)
              ZR(JRVAL-1+KK) = RINST
25          CONTINUE
          ELSE
            TYPAC = 'ORDRE'
            CALL WKVECT(NRVAL,'V V R',1,JRVAL)
            ZR(JRVAL) = 0.0D0
          ENDIF

        ENDIF

C
C --- 2.2- ON DETERMINE :
C     ------------------
C         NKCHA = TABLEAU DES NOMS DE CHAMP

        CALL GETVTX('RESU','NOM_CHAM',1,1,1,NSYMB,N1)

        CALL WKVECT(NIORD,'V V I',NBVAL,JNIORD)
        ZI(JNIORD)=-1
        CALL WKVECT(NKCHA,'V V K24',NBVAL,JKCHA)

        IF ( TYPAC .EQ. 'ORDRE' ) THEN
          CALL JEVEUO(NIVAL,'L',JIVAL)
          DO 30 I = 1,NBVAL
            ZI(JNIORD+I-1)=ZI(JIVAL+I-1)
            CALL RSEXCH(RESU,NSYMB,ZI(JIVAL+I-1),ZK24(JKCHA+I-1),N1)
            IF(N1.NE.0)THEN
                VALK = NSYMB
                VALI = ZI(JIVAL+I-1)
                CALL U2MESG('I', 'TABLE0_38',1,VALK,1,VALI,0,0.D0)
                ZK24(JKCHA+I-1) = '&&CHAMP_INEXISTANT'
            ENDIF
30        CONTINUE

        ELSEIF ( TYPAC .EQ. 'MODE' ) THEN
          CALL JEVEUO(NIVAL,'L',JIVAL)
          DO 40 I = 1,NBVAL
            CALL RSORAC(RESU,'NUME_MODE',ZI(JIVAL+I-1),0.0D0,K8B,
     &                  CBID,EPSI,CRIT,IBID,0,N1)
            IF(N1.EQ.0)THEN
               ZK24(JKCHA+I-1) = '&&CHAMP_INEXISTANT'
               VALK = TYPAC
               VALI = ZI(JIVAL+I-1)
               ZI(JNIORD+I-1)=-1
               CALL U2MESG('I','TABLE0_39',1,VALK,1,VALI,0,0.D0)
               GOTO 40
            ENDIF
            N1=-N1
            CALL RSORAC(RESU,'NUME_MODE',ZI(JIVAL+I-1),0.0D0,K8B,
     &                  CBID,EPSI,CRIT,ZI(JNIORD+I-1),N1,N2)
            CALL RSEXCH(RESU,NSYMB,ZI(JNIORD+I-1),ZK24(JKCHA+I-1),N2)
            IF(N2.NE.0)THEN
                VALK = NSYMB
                VALI = ZI(JIVAL+I-1)
                ZI(JNIORD+I-1)=-1
                CALL U2MESG('I', 'TABLE0_38',1,VALK,1,VALI,0,0.D0)
                ZK24(JKCHA+I-1) = '&&CHAMP_INEXISTANT'
            ENDIF
40        CONTINUE

        ELSEIF ( TYPAC .EQ. 'INST' .OR. TYPAC .EQ. 'FREQ') THEN
          CALL JEVEUO(NRVAL,'L',JRVAL)
          DO 50 I = 1,NBVAL
            CALL RSORAC(RESU,TYPAC,0,ZR(JRVAL+I-1),K8B,
     &                  CBID,EPSI,CRIT,IBID,0,N1)
            IF(N1.EQ.0)THEN
               ZK24(JKCHA+I-1) = '&&CHAMP_INEXISTANT'
               VALK = TYPAC
               VALR = ZR(JRVAL+I-1)
               ZI(JNIORD+I-1)=-1
               CALL U2MESG('I','TABLE0_40',1,VALK,0,IBID,1,VALR)
               GOTO 50
            ENDIF
            N1=-N1
            CALL RSORAC(RESU,TYPAC,0,ZR(JRVAL+I-1),K8B,
     &                  CBID,EPSI,CRIT,ZI(JNIORD+I-1),N1,N2)
            CALL RSEXCH(RESU,NSYMB,ZI(JNIORD+I-1),ZK24(JKCHA+I-1),N2)
            IF(N2.NE.0)THEN
                VALK = NSYMB
                VALI = ZI(JNIORD+I-1)
                CALL U2MESG('I', 'TABLE0_38',1,VALK,1,VALI,0,0.D0)
                ZK24(JKCHA+I-1) = '&&CHAMP_INEXISTANT'
            ENDIF
50        CONTINUE
        ENDIF
      ENDIF

      CALL JEDEMA()

      END
