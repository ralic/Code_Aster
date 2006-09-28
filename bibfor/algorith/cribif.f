      SUBROUTINE CRIBIF( MOD, DSIDEP, VBIFUR , NBRAC4, RACINE )
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C =====================================================================
      IMPLICIT      NONE
      INTEGER       NBRAC4
      REAL*8        DSIDEP(6,6),RACINE(4),VBIFUR
      CHARACTER*8   MOD
C =====================================================================
C --- RECHERCHE DE ZONES DE LOCALISATION PAR LE CRITERE DE RICE -------
C =====================================================================
      INTEGER     TESTIV,II,DEGRE,COMPT,NBRAC3
      REAL*8      ZERO, UN, DEUX, TROIS, QUATRE, R8PREM, R8NNEM, R8PI
      REAL*8      A0, A1, A2, A3, A4, LAMBA, LAMBB, LAMBC, LAMBP, LAMBQ
      REAL*8      VALEUR, FBIFUR, DELTA, LAMBR, LAMBT, TRIGOM
      REAL*8      AI(4),RAC4(8),SC,SIGNE, RAC3(3), ANGLE, R8RDDG,R8DGRD
C =====================================================================
      PARAMETER  ( ZERO   = 0.0D0 )
      PARAMETER  ( UN     = 1.0D0 )
      PARAMETER  ( DEUX   = 2.0D0 )
      PARAMETER  ( TROIS  = 3.0D0 )
      PARAMETER  ( QUATRE = 4.0D0 )
C =====================================================================
C --- INITIALISATIONS ET COHERENCES -----------------------------------
C =====================================================================
      VBIFUR    = ZERO
      NBRAC4    = 0
      SIGNE     = 1.0D0
      VALEUR    = R8NNEM()
      RACINE(1) = 0.0D0
      RACINE(2) = 0.0D0
      RACINE(3) = 0.0D0
      RACINE(4) = 0.0D0
      IF ( (MOD(1:6).NE.'D_PLAN') .AND.
     &     (MOD(1:6).NE.'C_PLAN') .AND.
     &     (MOD(1:4).NE.'AXIS') )    THEN
         CALL U2MESS('F','ALGORITH2_43')
      ENDIF
C =====================================================================
C --- AFFECTATION DES VARIABLES ---------------------------------------
C =====================================================================
       A0 = DSIDEP(1,1)*DSIDEP(4,4) - DSIDEP(1,4)*DSIDEP(4,1)
       A1 = DSIDEP(1,1)*(DSIDEP(4,2)+DSIDEP(2,4)) -
     &                DSIDEP(1,4)*DSIDEP(2,1) - DSIDEP(1,2)*DSIDEP(4,1)
       A2 = DSIDEP(1,1)*DSIDEP(2,2) + DSIDEP(1,4)*DSIDEP(4,2) +
     &     DSIDEP(4,1)*DSIDEP(2,4) - DSIDEP(1,2)*DSIDEP(4,4) -
     &     DSIDEP(1,2)*DSIDEP(2,1) - DSIDEP(4,4)*DSIDEP(2,1)
       A3 = DSIDEP(2,2)*(DSIDEP(1,4) + DSIDEP(4,1)) -
     &     DSIDEP(1,2)*DSIDEP(2,4) - DSIDEP(4,2)*DSIDEP(2,1)
       A4 = DSIDEP(4,4)*DSIDEP(2,2) - DSIDEP(4,2)*DSIDEP(2,4)
       IF (A4.LT.-R8PREM()) THEN
          SIGNE = -1.0D0
       ENDIF
C =====================================================================
C --- CAS OU A4 = 0 ---------------------------------------------------
C =====================================================================
       IF (ABS(A4).LT.R8PREM()) THEN
C =====================================================================
C --- ON LOCALISE POUR A4 = 0 -----------------------------------------
C =====================================================================
          VBIFUR    = UN
          NBRAC4    = 1
          RACINE(1) = 90.0D0
          RACINE(2) = 0.0D0
          RACINE(3) = 0.0D0
          RACINE(4) = 0.0D0
          GOTO 9998
       ENDIF
       LAMBA = TROIS*A3/QUATRE/A4
       LAMBB = A2/DEUX/A4
       LAMBC = A1/QUATRE/A4
C =====================================================================
C --- RESOLUTION DU POLYNOME DE DEGRE 3 -------------------------------
C =====================================================================
       CALL ZEROP3(LAMBA, LAMBB, LAMBC, RAC3, NBRAC3)
       DO 10 II=1, NBRAC3
          VALEUR = SIGNE * FBIFUR(A0,A1,A2,A3,A4,RAC3(II))
          IF (VALEUR.LT.-R8PREM()) VBIFUR = UN
 10    CONTINUE
C =====================================================================
C --- RECHERCHE DES RACINES DU POLYNOME -------------------------------
C =====================================================================
       IF (VBIFUR.EQ.UN) THEN
          DEGRE = 4
          AI(1) = A0/A4
          AI(2) = A1/A4
          AI(3) = A2/A4
          AI(4) = A3/A4
          CALL ZEROPN(DEGRE, AI(1), RAC4)
C =====================================================================
C --- ON RECUPERE LES RACINES REELLES ---------------------------------
C =====================================================================
          COMPT = 0
          DO 20 II=1,4
             IF (ABS(RAC4((II-1)*2+2)).LT.R8PREM()) THEN
                COMPT = COMPT + 1
                RACINE(COMPT) = ATAN2(RAC4((II-1)*2+1),1.0D0)
                RACINE(COMPT) = RACINE(COMPT)*R8RDDG()
             ENDIF
 20       CONTINUE
          NBRAC4 = COMPT
       ENDIF
C =====================================================================
 9998     CONTINUE
C =====================================================================
       END
