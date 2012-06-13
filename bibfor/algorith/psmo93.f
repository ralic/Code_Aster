      SUBROUTINE PSMO93(SOLVEU,MASSE,RAIDE,RAIDFA,NUME,NBPSMO,NBMODA,
     &                  NBMOAD)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C     ------------------------------------------------------------------
C
C     BUT : CONSTRUIRE LES PSEUDO MODES A ACCELERATION IMPOSEE
C
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
      INCLUDE 'jeveux.h'
      INTEGER      IBID, NEQ, LMATM
      REAL*8       R8B, ZERO, UN, COEF(3),XNORM
      CHARACTER*8  K8B,MONAXE
      CHARACTER*14 NUME
      CHARACTER*16 NOMCMD
      CHARACTER*19 RAIDE,RAIDFA,MASSE,MATPRE
      CHARACTER*19 SOLVEU
      CHARACTER*24 MOAUNI,MOAIMP,DDLAC
      LOGICAL      ACCUNI
      INTEGER      IARG
C     ------------------------------------------------------------------
      CALL JEMARQ()

      NBMOAD = 0
      NBMODA = 0
      UN=1.D0
      ZERO=0.D0
      ACCUNI = .FALSE.
      MOAUNI='&&OP0093.MODE_STAT_ACCU'
      MOAIMP='&&OP0093.MODE_ACCE_IMPO'
      DDLAC='&&OP0093.DDL_ACCE_IMPO'
      MATPRE = '&&MOIN93.MATPRE'

      CALL DISMOI('F','NB_EQUA',RAIDE,'MATR_ASSE',NEQ ,K8B ,IERD)
      CALL JEVEUO(MASSE(1:19)//'.&INT','E',LMATM)

      DO 30 I = 1,NBPSMO
         CALL GETVTX('PSEUDO_MODE','AXE',I,IARG,0,K8B,NA)
         IF (NA.NE.0) NBMODA = NBMODA - NA
         CALL GETVR8('PSEUDO_MODE','DIRECTION',I,IARG,0,R8B,ND)
         IF (ND.NE.0) NBMODA = NBMODA + 1
  30    CONTINUE

      IF ( NBMODA .NE. 0 ) THEN
        CALL WKVECT('&&OP0093.COEFFICIENT','V V R',3*NBMODA,JCOEF)
      ENDIF

C----------------------------------C
C--                              --C
C-- BOUCLE SUR LES PSEUDOS MODES --C
C--                              --C
C----------------------------------C

      IMOD = 0
      NBACC = 0
      DO 32 I = 1,NBPSMO

C-- PSEUDO MODE AUTOUR D'UN AXE
        CALL GETVTX('PSEUDO_MODE','AXE',I,IARG,0,MONAXE,NA)
        IF (NA.NE.0) THEN
          NBACC = NBACC + 1
          NNAXE = -NA
          ACCUNI = .TRUE.
          CALL WKVECT('&&OP0093.AXE','V V K8',NNAXE,JAXE)
          CALL GETVTX('PSEUDO_MODE','AXE',I,IARG,NNAXE,ZK8(JAXE),NA)
          DO 34 IA = 1,NNAXE
            MONAXE = ZK8(JAXE+IA-1)
            IF (MONAXE(1:1).EQ.'X') THEN
              IMOD = IMOD + 1
              IND = 3 * ( IMOD - 1 )
              ZR(JCOEF+IND+1-1) = UN
              ZR(JCOEF+IND+2-1) = ZERO
              ZR(JCOEF+IND+3-1) = ZERO
            ELSEIF (MONAXE(1:1).EQ.'Y') THEN
              IMOD = IMOD + 1
              IND = 3 * ( IMOD - 1 )
              ZR(JCOEF+IND+1-1) = ZERO
              ZR(JCOEF+IND+2-1) = UN
              ZR(JCOEF+IND+3-1) = ZERO
            ELSEIF (MONAXE(1:1).EQ.'Z') THEN
              IMOD = IMOD + 1
              IND = 3 * ( IMOD - 1 )
              ZR(JCOEF+IND+1-1) = ZERO
              ZR(JCOEF+IND+2-1) = ZERO
              ZR(JCOEF+IND+3-1) = UN
            ENDIF
  34      CONTINUE
          CALL JEDETR('&&OP0093.AXE')
        ENDIF

C-- PSEUDO MODE DANS UNE DIRECTION
        CALL GETVR8('PSEUDO_MODE','DIRECTION',I,IARG,3,COEF,ND)
        IF (ND.NE.0) THEN
          NBACC = NBACC + 1
          ACCUNI = .TRUE.
          XNORM = ZERO
          DO 36 ID = 1,3
             XNORM = XNORM + COEF(ID)*COEF(ID)
36            CONTINUE
          IF (XNORM.LE.ZERO) THEN
             CALL U2MESS('F','ALGELINE2_78')
          ENDIF
          XNORM = UN / SQRT(XNORM)
          DO 38 ID = 1,3
             COEF(ID) = COEF(ID) * XNORM
38            CONTINUE
          IMOD = IMOD + 1
          IND = 3 * ( IMOD - 1 )
          ZR(JCOEF+IND+1-1) = COEF(1)
          ZR(JCOEF+IND+2-1) = COEF(2)
          ZR(JCOEF+IND+3-1) = COEF(3)
        ENDIF
32    CONTINUE

C--------------------------C
C--                      --C
C-- CALCUL DES DEFORMEES --C
C--                      --C
C--------------------------C

      IF ( ACCUNI ) THEN
        CALL WKVECT(MOAUNI,'V V R',NEQ*NBMODA,LMODA)
        CALL MODSTA('ACCE',RAIDFA,MATPRE,SOLVEU,LMATM,NUME,IBID,
     &              ZR(JCOEF),NEQ,NBMODA,ZR(LMODA))
      ENDIF

      IF ( NBACC .NE. NBPSMO ) THEN
        CALL WKVECT(DDLAC,'V V I',NEQ,LDDAD)
        CALL MSTGET(NOMCMD,MASSE,'PSEUDO_MODE',NBPSMO,ZI(LDDAD))
        DO 24 II = 0,NEQ-1
           NBMOAD = NBMOAD + ZI(LDDAD+II)
  24    CONTINUE
        CALL WKVECT(MOAIMP,'V V R',NEQ*NBMOAD,LMOAD)
        CALL MODSTA('ACCD',RAIDFA,MATPRE,SOLVEU,LMATM,NUME,ZI(LDDAD),
     &              R8B,NEQ,NBMOAD,ZR(LMOAD))
      ENDIF

      CALL JEDEMA()

      END
