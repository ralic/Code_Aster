      SUBROUTINE SSMAU2(NOMU,OPTION)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
      IMPLICIT REAL*8 (A-H,O-Z)

C     ARGUMENTS:
C     ----------
      INCLUDE 'jeveux.h'
      CHARACTER*8 NOMU
      CHARACTER*(*) OPTION
C ----------------------------------------------------------------------
C     BUT:
C          CONDENSATION DE LA MATRICE DE MASSE (OU D'AMORTISSEMENT)
C          D'UN MACR_ELEM_STAT:
C          CALCUL DE MP_EE = M_EE + PHI_EI*M_II*PHI_IE
C                            + M_EI*PHI_IE + PHI_EI*M_IE
C   ATTENTION LE PHI_IE D'ASTER EST L'OPPOSE DE CELUI DE LA FORMULE

C     IN: NOMU   : NOM DU MACR_ELEM_STAT
C         OPTION : 'MASS_MECA' OU 'AMOR_MECA'

C     OUT:   / NOMU.MAEL_MASS_VALE  SI 'MASS_MECA'
C            / NOMU.MAEL_AMOR_VALE  SI 'AMOR_MECA'

C ----------------------------------------------------------------------


      INTEGER I,SCDI,SCHC,IBLO,IBID
      CHARACTER*8 KBID
      CHARACTER*8 PROMES
      LOGICAL MOSTRU

      CHARACTER*16 OPTIO2
      CHARACTER*19 NU,MATAS,STOCK


      CALL JEMARQ()
      OPTIO2 = OPTION
      NU = NOMU
      NU = NU(1:14)//'.NUME'
      STOCK = NU(1:14)//'.SLCS'

      IF (OPTIO2(1:9).EQ.'MASS_MECA') THEN
        MATAS = NOMU//'.MASSMECA'

      ELSEIF (OPTIO2(1:9).EQ.'AMOR_MECA') THEN
        MATAS = NOMU//'.AMORMECA'

      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

C     -- MOSTRU=.TRUE. : CAS MODIFICATION STRUCTURALE
      MOSTRU = .TRUE.
      CALL DISMOI('F','NOM_PROJ_MESU',NOMU,'MACR_ELEM_STAT',IBID,PROMES,
     &            IER)
      IF (PROMES.EQ.' ') MOSTRU = .FALSE.


      CALL JEVEUO(NOMU//'.DESM','E',JDESM)
      NDDLE = ZI(JDESM-1+4)
      NDDLI = ZI(JDESM-1+5)


C     -- ALLOCATION DE TMP_IE (TEMPORAIRE LIKE PHI_IE )
C     -------------------------------------------------------
      CALL JELIRA(NOMU//'.PHI_IE','LONMAX',LGBLPH,KBID)
      CALL JELIRA(NOMU//'.PHI_IE','NMAXOC',NBLPH,KBID)
      NLBLPH=LGBLPH/NDDLI

      CALL JECREC(NOMU//'.TMP_IE','V V R','NU','DISPERSE','CONSTANT',
     &            NBLPH)
      CALL JEECRA(NOMU//'.TMP_IE','LONMAX',LGBLPH,KBID)
      DO 10,J = 1,NBLPH
        CALL JECROC(JEXNUM(NOMU//'.TMP_IE',J))
        CALL JEVEUO(JEXNUM(NOMU//'.TMP_IE',J),'E',IATMI0)
        CALL JELIBE(JEXNUM(NOMU//'.TMP_IE',J))
   10 CONTINUE

      IF (OPTIO2(1:9).EQ.'MASS_MECA') THEN
       CALL WKVECT(NOMU//'.MAEL_MASS_VALE','G V R',(NDDLE*(NDDLE+1)/2),
     &            IAMPEE)
      ELSEIF ((OPTIO2(1:9).EQ.'AMOR_MECA').AND.(MOSTRU)) THEN
       CALL WKVECT(NOMU//'.MAEL_AMOR_VALE','G V R',(NDDLE*(NDDLE+1)/2),
     &            IAMPEE)
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      IF (MOSTRU) THEN
C       CREATION DE LA MATRICE POUR MODIFICATION STRUCTURALE
        IF (OPTIO2(1:9).EQ.'MASS_MECA') CALL CRMEMA(PROMES,IAMPEE)
        IF (OPTIO2(1:9).EQ.'AMOR_MECA') CALL CRMEAM(PROMES,IAMPEE)

      ELSE

C     -- ALLOCATION DE MP_EE  ET INITIALISATION PAR M_EE:
C     ---------------------------------------------------

        CALL MTDSCR(MATAS)
        CALL JEVEUO(MATAS(1:19)//'.&INT','E',LMAT)
        CALL MTDSC2(ZK24(ZI(LMAT+1)),'SCDI','L',IASCDI)
        CALL JEVEUO(ZK24(ZI(LMAT+1)) (1:19)//'.REFA','L',JREFA)
        CALL JEVEUO(ZK24(JREFA-1+2) (1:14)//'.SLCS.SCHC','L',IASCHC)
        CALL JEVEUO(STOCK//'.SCIB','L',IASCIB)

        IBLOLD = 0
        DO 30,J = 1,NDDLE
          IBLO = ZI(IASCIB-1+NDDLI+J)
          SCDI = ZI(IASCDI-1+NDDLI+J)
          SCHC = ZI(IASCHC-1+NDDLI+J)
          IF (IBLO.NE.IBLOLD) THEN
            IF (IBLOLD.GT.0) CALL JELIBE(JEXNUM(MATAS//'.UALF',IBLOLD))
            CALL JEVEUO(JEXNUM(MATAS//'.UALF',IBLO),'L',JUALF)
          ENDIF
          IBLOLD = IBLO
CCDIR$ IVDEP
          DO 20,I = MAX(1,J+1-SCHC),J
            II = (J-1)*J/2 + I
            ZR(IAMPEE-1+II) = ZR(JUALF-1+SCDI+I-J)
   20     CONTINUE

   30   CONTINUE
        IF (IBLOLD.GT.0) CALL JELIBE(JEXNUM(MATAS//'.UALF',IBLOLD))

C     -- CALCUL DE MP_EE = MP_EE + M_EI*PHI_IE + PHI_EI*M_IE :
C     --------------------------------------------------------
        IBLOLD = 0
        DO 120,J = 1,NDDLE
          IBLO = ZI(IASCIB-1+NDDLI+J)
          SCDI = ZI(IASCDI-1+NDDLI+J)
          SCHC = ZI(IASCHC-1+NDDLI+J)
          IF (IBLO.NE.IBLOLD) THEN
            IF (IBLOLD.GT.0) CALL JELIBE(JEXNUM(MATAS//'.UALF',IBLOLD))
            CALL JEVEUO(JEXNUM(MATAS//'.UALF',IBLO),'L',JUALF)
          ENDIF
          IBLOLD = IBLO

          I = 0
          DO 60,IBLPH = 1,NBLPH
            CALL JEVEUO(JEXNUM(NOMU//'.PHI_IE',IBLPH),'L',IAPHI0)
            DO 50,IIBLPH = 1,NLBLPH
              I = I + 1
              IF (I.GT.J) THEN
                CALL JELIBE(JEXNUM(NOMU//'.PHI_IE',IBLPH))
                GOTO 70

              ENDIF
              IAPHIE = IAPHI0 + (IIBLPH-1)*NDDLI
              II = (J-1)*J/2 + I
              KK = 0
CCDIR$ IVDEP
              DO 40,K = NDDLI + J + 1 - SCHC,NDDLI
                KK = KK + 1
                ZR(IAMPEE-1+II) = ZR(IAMPEE-1+II) -
     &                            ZR(IAPHIE-1+K)*ZR(JUALF-1+SCDI-SCHC+
     &                            KK)
   40         CONTINUE
   50       CONTINUE
            CALL JELIBE(JEXNUM(NOMU//'.PHI_IE',IBLPH))
   60     CONTINUE
   70     CONTINUE


C        SYMETRIE:
          I = 0
          DO 100,IBLPH = 1,NBLPH
            IF (I+NLBLPH.LT.J) THEN
              I = I + NLBLPH
              GOTO 100

            ENDIF
            CALL JEVEUO(JEXNUM(NOMU//'.PHI_IE',IBLPH),'L',IAPHI0)
            DO 90,IIBLPH = 1,NLBLPH
              I = I + 1
              IF (I.LT.J) GOTO 90
              IF (I.GT.NDDLE) THEN
                CALL JELIBE(JEXNUM(NOMU//'.PHI_IE',IBLPH))
                GOTO 110

              ENDIF
              IAPHIE = IAPHI0 + (IIBLPH-1)*NDDLI
              II = (I* (I-1)/2) + J
              KK = 0
CCDIR$ IVDEP
              DO 80,K = NDDLI + J + 1 - SCHC,NDDLI
                KK = KK + 1
                ZR(IAMPEE-1+II) = ZR(IAMPEE-1+II) -
     &                            ZR(IAPHIE-1+K)*ZR(JUALF-1+SCDI-SCHC+
     &                            KK)
   80         CONTINUE
   90       CONTINUE
            CALL JELIBE(JEXNUM(NOMU//'.PHI_IE',IBLPH))
  100     CONTINUE
  110     CONTINUE

  120   CONTINUE


        IF (IBLOLD.GT.0) CALL JELIBE(JEXNUM(MATAS//'.UALF',IBLOLD))


C     -- CALCUL DE TMP_IE = M_II*PHI_IE :
C     -----------------------------------
        I = 0
        DO 180,IBLPH = 1,NBLPH
          CALL JEVEUO(JEXNUM(NOMU//'.PHI_IE',IBLPH),'L',IAPHI0)
          CALL JEVEUO(JEXNUM(NOMU//'.TMP_IE',IBLPH),'E',IATMI0)
          DO 160,IIBLPH = 1,NLBLPH
            I = I + 1
            IF (I.GT.NDDLE) GOTO 170
            IAPHIE = IAPHI0 + (IIBLPH-1)*NDDLI
            IATMIE = IATMI0 + (IIBLPH-1)*NDDLI

            IBLOLD = 0
            DO 150,J = 1,NDDLI
              IBLO = ZI(IASCIB-1+J)
              SCDI = ZI(IASCDI-1+J)
              SCHC = ZI(IASCHC-1+J)
              IF (IBLO.NE.IBLOLD) THEN
                IF (IBLOLD.GT.0) CALL JELIBE(JEXNUM(MATAS//'.UALF',
     &                                IBLOLD))
                CALL JEVEUO(JEXNUM(MATAS//'.UALF',IBLO),'L',JUALF)
              ENDIF
              IBLOLD = IBLO

              KK = 0
CCDIR$ IVDEP
              DO 130,K = J + 1 - SCHC,J
                KK = KK + 1
                ZR(IATMIE-1+J) = ZR(IATMIE-1+J) -
     &                           ZR(IAPHIE-1+K)*ZR(JUALF-1+SCDI-SCHC+KK)
  130         CONTINUE
              KK = 0
CCDIR$ IVDEP
              DO 140,K = J + 1 - SCHC,J - 1
                KK = KK + 1
                ZR(IATMIE-1+K) = ZR(IATMIE-1+K) -
     &                           ZR(IAPHIE-1+J)*ZR(JUALF-1+SCDI-SCHC+KK)
  140         CONTINUE
  150       CONTINUE
            IF (IBLOLD.GT.0) CALL JELIBE(JEXNUM(MATAS//'.UALF',IBLOLD))

  160     CONTINUE
  170     CONTINUE
          CALL JELIBE(JEXNUM(NOMU//'.PHI_IE',IBLPH))
          CALL JELIBE(JEXNUM(NOMU//'.TMP_IE',IBLPH))
  180   CONTINUE


C     -- CALCUL DE MP_EE = MP_EE + PHI_EI*TMP_IE:
C     -------------------------------------------

        I = 0
        DO 250,IBLPH = 1,NBLPH
          CALL JEVEUO(JEXNUM(NOMU//'.PHI_IE',IBLPH),'L',IAPHI0)
          DO 230,IIBLPH = 1,NLBLPH
            I = I + 1
            IF (I.GT.NDDLE) GOTO 240
            IAPHIE = IAPHI0 + (IIBLPH-1)*NDDLI
            J = 0
            DO 220,JBLPH = 1,NBLPH
              CALL JEVEUO(JEXNUM(NOMU//'.TMP_IE',JBLPH),'L',IATMI0)
              DO 200,JJBLPH = 1,NLBLPH
                J = J + 1
                IF (J.GT.I) GOTO 210
                IATMIE = IATMI0 + (JJBLPH-1)*NDDLI
                II = (I-1)*I/2 + J
CCDIR$ IVDEP
                DO 190,K = 1,NDDLI
                  ZR(IAMPEE-1+II) = ZR(IAMPEE-1+II) -
     &                              ZR(IAPHIE-1+K)*ZR(IATMIE-1+K)
  190           CONTINUE
  200         CONTINUE
  210         CONTINUE
              CALL JELIBE(JEXNUM(NOMU//'.TMP_IE',IBLPH))
  220       CONTINUE
  230     CONTINUE
  240     CONTINUE
          CALL JELIBE(JEXNUM(NOMU//'.PHI_IE',IBLPH))
  250   CONTINUE

      ENDIF

      CALL JEDETR(NOMU//'.TMP_IE')
      CALL JEDEMA()
      END
