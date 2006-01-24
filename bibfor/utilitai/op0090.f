      SUBROUTINE OP0090(IER)
      IMPLICIT NONE
C     ------------------------------------------------------------------
C MODIF UTILITAI  DATE 23/01/2006   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C RESPONSABLE MCOURTOI M.COURTOIS
C     OPERATEUR "RECU_FONCTION"
C     ------------------------------------------------------------------
      INTEGER NREG,NRB,NCH,NOBS,NG,IER
      INTEGER NGN,IBID,NTA,NRES,NRGEN,NC
      CHARACTER*8 K8B
      CHARACTER*19 NOMFON,CHAM19,RESU,TABRES
C     ------------------------------------------------------------------
C
C     -----------------------------------------------------------------
C                      --- CAS D'UN CHAM_GD ---
C     -----------------------------------------------------------------
      CALL GETVID(' ','CHAM_GD',0,1,1,CHAM19,NCH)
      IF (NCH.NE.0) THEN
        CALL RFRCHA(IER)
        GO TO 10
      END IF

C     -----------------------------------------------------------------
C                       --- CAS D'UN RESULTAT ---
C     -----------------------------------------------------------------
      CALL GETVID(' ','RESULTAT ',0,1,1,RESU,NRES)
      IF (NRES.NE.0) THEN
        CALL RFRESU(IER)
        GO TO 10
      ENDIF
C

C     -----------------------------------------------------------------
C                   --- CAS D'UN NOEUD DE CHOC ---
C     -----------------------------------------------------------------
      CALL GETVID(' ','NOEUD_CHOC',0,1,1,K8B,NC)
      CALL GETVID(' ','GROUP_NO_CHOC',0,1,1,K8B,NG)
      IF (NC+NG.NE.0) THEN
        CALL RFNOCH()
        GO TO 10
      END IF

C     -----------------------------------------------------------------
C                    --- CAS D'UN RESU_GENE ---
C     -----------------------------------------------------------------
      CALL GETVID(' ','RESU_GENE',0,1,1,RESU,NREG)
      IF (NREG.NE.0) THEN
        CALL RFRGEN(RESU)
        GO TO 10
      END IF

C     -----------------------------------------------------------------
C                       --- CAS D'UNE TABLE ---
C     -----------------------------------------------------------------
      CALL GETVID(' ','TABLE',0,1,1,TABRES,NTA)
      IF (NTA.NE.0) THEN
        CALL RFTABL(TABRES)
        GO TO 10
      END IF

C     -----------------------------------------------------------------
C                    --- CAS D'UN OBSTACLE ---
C     -----------------------------------------------------------------
      CALL GETVID(' ','OBSTACLE',0,1,1,RESU,NOBS)
      IF (NOBS.NE.0) THEN
        CALL RFOBST(RESU)
        GO TO 10
      END IF

C     -----------------------------------------------------------------
C                 --- CAS D'UNE BASE_ELAS_FLUI ---
C     -----------------------------------------------------------------
      CALL GETVID(' ','BASE_ELAS_FLUI',0,1,1,RESU,NRB)
      IF (NRB.NE.0) THEN
        CALL RFBEFL(RESU)
        GO TO 10
      END IF

C     -----------------------------------------------------------------
   10 CONTINUE

      END
