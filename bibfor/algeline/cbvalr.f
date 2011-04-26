      SUBROUTINE CBVALR(ROUC,NEQ,SMHC,SMDI,IDLEXC,COEFR,COEFC,VALMI,
     &                  VALMR,VALMC)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_4
C-------------------------------------------------------------------
C     BUT : ACCUMULTATION DANS VALMR (OU VALMC) DE COEF*VALMI
C     ROUC=
C        /'RR' : ON UTILISE VALMR ET COEFR
C        /'RC' : ON UTILISE VALMR ET COEFC
C        /'CR' : ON UTILISE VALMC ET COEFR
C        /'CC' : ON UTILISE VALMC ET COEFC
C-------------------------------------------------------------------
      CHARACTER*2 ROUC
      INTEGER*4 SMHC(*)
      INTEGER NEQ,SMDI(*),IDLEXC(*)
      INTEGER KIN,IDEBLI,ILIG,IFINLI,IND,JCOL
      REAL*8 COEFR,VALMI(*),VALMR(*)
      COMPLEX*16 COEFC,VALMC(*)
C     ------------------------------------------------------------------
      KIN = 0
      IDEBLI = 1


      IF (ROUC.EQ.'RR') THEN
C     -------------------------------
        DO 20 ILIG = 1,NEQ
          IFINLI = SMDI(ILIG)
          DO 10 IND = IDEBLI,IFINLI
            KIN = KIN + 1
            JCOL = SMHC(IND)
            VALMR(KIN) = VALMR(KIN) + COEFR*VALMI(KIN)*
     &                   (1-IDLEXC(JCOL))* (1-IDLEXC(ILIG))
   10     CONTINUE
          IDEBLI = SMDI(ILIG) + 1
   20   CONTINUE


      ELSE IF (ROUC.EQ.'RC') THEN
C     -------------------------------
        DO 40 ILIG = 1,NEQ
          IFINLI = SMDI(ILIG)
          DO 30 IND = IDEBLI,IFINLI
            KIN = KIN + 1
            JCOL = SMHC(IND)
            VALMR(KIN) = VALMR(KIN) + DBLE(COEFC*VALMI(KIN)*
     &                   (1-IDLEXC(JCOL))* (1-IDLEXC(ILIG)))
   30     CONTINUE
          IDEBLI = SMDI(ILIG) + 1
   40   CONTINUE


      ELSE IF (ROUC.EQ.'CR') THEN
C     -------------------------------
        DO 60 ILIG = 1,NEQ
          IFINLI = SMDI(ILIG)
          DO 50 IND = IDEBLI,IFINLI
            KIN = KIN + 1
            JCOL = SMHC(IND)
            VALMC(KIN) = VALMC(KIN) + COEFR*VALMI(KIN)*
     &                   (1-IDLEXC(JCOL))* (1-IDLEXC(ILIG))
   50     CONTINUE
          IDEBLI = SMDI(ILIG) + 1
   60   CONTINUE


      ELSE IF (ROUC.EQ.'CC') THEN
C     -------------------------------
        DO 80 ILIG = 1,NEQ
          IFINLI = SMDI(ILIG)
          DO 70 IND = IDEBLI,IFINLI
            KIN = KIN + 1
            JCOL = SMHC(IND)
            VALMC(KIN) = VALMC(KIN) + COEFC*VALMI(KIN)*
     &                   (1-IDLEXC(JCOL))* (1-IDLEXC(ILIG))
   70     CONTINUE
          IDEBLI = SMDI(ILIG) + 1
   80   CONTINUE


      ELSE
         CALL ASSERT(.FALSE.)
      END IF

      END
