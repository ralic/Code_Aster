      SUBROUTINE SSMAU2(NOMU,OPTION)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 01/02/2000   AUTEUR VABHHTS J.PELLET 
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
      IMPLICIT REAL*8 (A-H,O-Z)

C     ARGUMENTS:
C     ----------
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

C     OUT:   / NOMU.MP_EE  SI 'MASS_MECA'
C            / NOMU.AP_EE  SI 'AMOR_MECA'

C ----------------------------------------------------------------------


      INTEGER I,ADIA,HCOL,IBLO
      CHARACTER*8 KBID
      CHARACTER*8 NOMO
      INTEGER IDBG
C --------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR,JEXR8
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR,EPSI
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16,OPTIO2
      CHARACTER*19 NU,MATAS,STOCK
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C ---------------- FIN COMMUNS NORMALISES  JEVEUX  --------------------


      CALL JEMARQ()
      OPTIO2 = OPTION
      NU = NOMU
      NU = NU(1:14)//'.NUME'
      STOCK = NU(1:14)//'.SLCS'

      IF (OPTIO2(1:9).EQ.'MASS_MECA') THEN
        MATAS = NOMU//'.MASSMECA'
      ELSE IF (OPTIO2(1:9).EQ.'AMOR_MECA') THEN
        MATAS = NOMU//'.AMORMECA'
      ELSE
        CALL JXABOR()
      END IF


      CALL JEVEUO(NOMU//'.DESM','E',IADESM)
      NDDLE = ZI(IADESM-1+4)
      NDDLI = ZI(IADESM-1+5)

C     -- ALLOCATION DE TMP_IE (TEMPORAIRE LIKE PHI_IE )
C     -------------------------------------------------------
      LGBLPH = MIN(4*1024*1024,NDDLE*NDDLI)
      NBLPH = (NDDLE*NDDLI-1)/LGBLPH + 1
      NLBLPH = LGBLPH/NDDLI
      CALL JECREC(NOMU//'.TMP_IE','G V R','NU','DISPERSE','CONSTANT',
     &            NBLPH)
      CALL JEECRA(NOMU//'.TMP_IE','LONMAX',LGBLPH,KBID)
      DO 10,J = 1,NBLPH
        CALL JECROC(JEXNUM(NOMU//'.TMP_IE',J))
        CALL JEVEUO(JEXNUM(NOMU//'.TMP_IE',J),'E',IATMI0)
        CALL JELIBE(JEXNUM(NOMU//'.TMP_IE',J))
   10 CONTINUE


C     -- ALLOCATION DE MP_EE  ET INITIALISATION PAR M_EE:
C     ---------------------------------------------------

      CALL MTDSCR(MATAS)
      CALL JEVEUO(MATAS(1:19)//'.&INT','E',LMAT)
      CALL MTDSC2(ZK24(ZI(LMAT+1)),'ADIA','L',IAADIA)
      CALL JEVEUO(ZK24(ZI(LMAT+1)) (1:19)//'.REFA','L',IDREFE)
      CALL JEVEUO(ZK24(IDREFE+2) (1:19)//'.HCOL','L',IAHCOL)
      CALL JEVEUO(STOCK//'.IABL','L',IAIABL)

      CALL WKVECT(NOMU//'.MP_EE','G V R', (NDDLE* (NDDLE+1)/2),IAMPEE)

      IBLOLD = 0
      DO 30,J = 1,NDDLE
        IBLO = ZI(IAIABL-1+NDDLI+J)
        ADIA = ZI(IAADIA-1+NDDLI+J)
        HCOL = ZI(IAHCOL-1+NDDLI+J)
        IF (IBLO.NE.IBLOLD) THEN
          IF (IBLOLD.GT.0) CALL JELIBE(JEXNUM(MATAS//'.VALE',IBLOLD))
          CALL JEVEUO(JEXNUM(MATAS//'.VALE',IBLO),'L',IAVALE)
        END IF
        IBLOLD = IBLO
CCDIR$ IVDEP
        DO 20,I = MAX(1,J+1-HCOL),J
          II = (J-1)*J/2 + I
          ZR(IAMPEE-1+II) = ZR(IAVALE-1+ADIA+I-J)
   20   CONTINUE

   30 CONTINUE
      IF (IBLOLD.GT.0) CALL JELIBE(JEXNUM(MATAS//'.VALE',IBLOLD))

C     -- CALCUL DE MP_EE = MP_EE + M_EI*PHI_IE + PHI_EI*M_IE :
C     --------------------------------------------------------
      IBLOLD = 0
      DO 120,J = 1,NDDLE
        IBLO = ZI(IAIABL-1+NDDLI+J)
        ADIA = ZI(IAADIA-1+NDDLI+J)
        HCOL = ZI(IAHCOL-1+NDDLI+J)
        IF (IBLO.NE.IBLOLD) THEN
          IF (IBLOLD.GT.0) CALL JELIBE(JEXNUM(MATAS//'.VALE',IBLOLD))
          CALL JEVEUO(JEXNUM(MATAS//'.VALE',IBLO),'L',IAVALE)
        END IF
        IBLOLD = IBLO

        I = 0
        DO 60,IBLPH = 1,NBLPH
          CALL JEVEUO(JEXNUM(NOMU//'.PHI_IE',IBLPH),'L',IAPHI0)
          DO 50,IIBLPH = 1,NLBLPH
            I = I + 1
            IF (I.GT.J) THEN
              CALL JELIBE(JEXNUM(NOMU//'.PHI_IE',IBLPH))
              GO TO 70
            END IF
            IAPHIE = IAPHI0 + (IIBLPH-1)*NDDLI
            II = (J-1)*J/2 + I
            KK = 0
CCDIR$ IVDEP
            DO 40,K = NDDLI + J + 1 - HCOL,NDDLI
              KK = KK + 1
              ZR(IAMPEE-1+II) = ZR(IAMPEE-1+II) -
     &                          ZR(IAPHIE-1+K)*ZR(IAVALE-1+ADIA-HCOL+KK)
   40       CONTINUE
   50     CONTINUE
          CALL JELIBE(JEXNUM(NOMU//'.PHI_IE',IBLPH))
   60   CONTINUE
   70   CONTINUE


C        SYMETRIE:
        I = 0
        DO 100,IBLPH = 1,NBLPH
          IF (I+NLBLPH.LT.J) THEN
            I = I + NLBLPH
            GO TO 100
          END IF
          CALL JEVEUO(JEXNUM(NOMU//'.PHI_IE',IBLPH),'L',IAPHI0)
          DO 90,IIBLPH = 1,NLBLPH
            I = I + 1
            IF (I.LT.J) GO TO 90
            IF (I.GT.NDDLE) THEN
              CALL JELIBE(JEXNUM(NOMU//'.PHI_IE',IBLPH))
              GO TO 110
            END IF
            IAPHIE = IAPHI0 + (IIBLPH-1)*NDDLI
            II = (I* (I-1)/2) + J
            KK = 0
CCDIR$ IVDEP
            DO 80,K = NDDLI + J + 1 - HCOL,NDDLI
              KK = KK + 1
              ZR(IAMPEE-1+II) = ZR(IAMPEE-1+II) -
     &                          ZR(IAPHIE-1+K)*ZR(IAVALE-1+ADIA-HCOL+KK)
   80       CONTINUE
   90     CONTINUE
          CALL JELIBE(JEXNUM(NOMU//'.PHI_IE',IBLPH))
  100   CONTINUE
  110   CONTINUE

  120 CONTINUE


      IF (IBLOLD.GT.0) CALL JELIBE(JEXNUM(MATAS//'.VALE',IBLOLD))


C     -- CALCUL DE TMP_IE = M_II*PHI_IE :
C     -----------------------------------
      I = 0
      DO 180,IBLPH = 1,NBLPH
        CALL JEVEUO(JEXNUM(NOMU//'.PHI_IE',IBLPH),'L',IAPHI0)
        CALL JEVEUO(JEXNUM(NOMU//'.TMP_IE',IBLPH),'E',IATMI0)
        DO 160,IIBLPH = 1,NLBLPH
          I = I + 1
          IF (I.GT.NDDLE) GO TO 170
          IAPHIE = IAPHI0 + (IIBLPH-1)*NDDLI
          IATMIE = IATMI0 + (IIBLPH-1)*NDDLI

          IBLOLD = 0
          DO 150,J = 1,NDDLI
            IBLO = ZI(IAIABL-1+J)
            ADIA = ZI(IAADIA-1+J)
            HCOL = ZI(IAHCOL-1+J)
            IF (IBLO.NE.IBLOLD) THEN
              IF (IBLOLD.GT.0) CALL JELIBE(JEXNUM(MATAS//'.VALE',
     &                              IBLOLD))
              CALL JEVEUO(JEXNUM(MATAS//'.VALE',IBLO),'L',IAVALE)
            END IF
            IBLOLD = IBLO

            KK = 0
CCDIR$ IVDEP
            DO 130,K = J + 1 - HCOL,J
              KK = KK + 1
              ZR(IATMIE-1+J) = ZR(IATMIE-1+J) -
     &                         ZR(IAPHIE-1+K)*ZR(IAVALE-1+ADIA-HCOL+KK)
  130       CONTINUE
            KK = 0
CCDIR$ IVDEP
            DO 140,K = J + 1 - HCOL,J - 1
              KK = KK + 1
              ZR(IATMIE-1+K) = ZR(IATMIE-1+K) -
     &                         ZR(IAPHIE-1+J)*ZR(IAVALE-1+ADIA-HCOL+KK)
  140       CONTINUE
  150     CONTINUE
          IF (IBLOLD.GT.0) CALL JELIBE(JEXNUM(MATAS//'.VALE',IBLOLD))

  160   CONTINUE
  170   CONTINUE
        CALL JELIBE(JEXNUM(NOMU//'.PHI_IE',IBLPH))
        CALL JELIBE(JEXNUM(NOMU//'.TMP_IE',IBLPH))
  180 CONTINUE


C     -- CALCUL DE MP_EE = MP_EE + PHI_EI*TMP_IE:
C     -------------------------------------------

      I = 0
      DO 250,IBLPH = 1,NBLPH
        CALL JEVEUO(JEXNUM(NOMU//'.PHI_IE',IBLPH),'L',IAPHI0)
        DO 230,IIBLPH = 1,NLBLPH
          I = I + 1
          IF (I.GT.NDDLE) GO TO 240
          IAPHIE = IAPHI0 + (IIBLPH-1)*NDDLI
          J = 0
          DO 220,JBLPH = 1,NBLPH
            CALL JEVEUO(JEXNUM(NOMU//'.TMP_IE',JBLPH),'L',IATMI0)
            DO 200,JJBLPH = 1,NLBLPH
              J = J + 1
              IF (J.GT.I) GO TO 210
              IATMIE = IATMI0 + (JJBLPH-1)*NDDLI
              II = (I-1)*I/2 + J
CCDIR$ IVDEP
              DO 190,K = 1,NDDLI
                ZR(IAMPEE-1+II) = ZR(IAMPEE-1+II) -
     &                            ZR(IAPHIE-1+K)*ZR(IATMIE-1+K)
  190         CONTINUE
  200       CONTINUE
  210       CONTINUE
            CALL JELIBE(JEXNUM(NOMU//'.TMP_IE',IBLPH))
  220     CONTINUE
  230   CONTINUE
  240   CONTINUE
        CALL JELIBE(JEXNUM(NOMU//'.PHI_IE',IBLPH))
  250 CONTINUE




  260 CONTINUE
      CALL JEDETR(NOMU//'.TMP_IE')
      CALL JELIBE(ZK24(IDREFE+2) (1:19)//'.HCOL')

      CALL JEDEMA()
      END
