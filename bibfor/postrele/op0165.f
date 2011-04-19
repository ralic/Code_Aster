      SUBROUTINE OP0165()
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT   NONE
C     ------------------------------------------------------------------
C MODIF POSTRELE  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
C     ------------------------------------------------------------------
C
C     OPERATEUR POST_RCCM
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER        N1, NBOPT, IOPT, NBTHER
      REAL*8        SYMAX, R8VIDE
      LOGICAL       PMPB, SN, SNET, FATIGU, LROCHT
      INTEGER   ICODRE
      CHARACTER*8   K8B, NOMMAT
      CHARACTER*16  TYPTAB, TYPMEC, KOPT(4), PHENOM
C DEB ------------------------------------------------------------------
C
      CALL INFMAJ
C
      SYMAX = R8VIDE()
C
      CALL GETVTX ( ' ', 'TYPE_RESU', 1,1,1, TYPTAB , N1 )
C
      CALL GETVTX ( ' ', 'TYPE_RESU_MECA',1,1,1, TYPMEC, N1 )
C
C     ------------------------------------------------------------------
C
C     ------------------- TYPE_RESU_MECA = EVOLUTION -------------------
C
C     ------------------------------------------------------------------
C
      IF ( TYPMEC .EQ. 'EVOLUTION' ) THEN
C
         CALL GETVTX ( ' ', 'OPTION',1,1,0, K8B, N1 )
         NBOPT = -N1
         CALL GETVTX ( ' ', 'OPTION',1,1,NBOPT, KOPT, N1 )
C
         CALL GETVID ( ' ', 'MATER',  1, 1,1,NOMMAT, N1 )
         CALL GETVR8 ( ' ', 'SY_MAX', 1, 1,1,SYMAX,  N1 )
C
         CALL RCCOME ( NOMMAT, 'RCCM', PHENOM, ICODRE )
         IF (ICODRE.EQ.1)  CALL U2MESK('F','POSTRCCM_7',1,'RCCM')
C
         CALL RCEVOL ( TYPTAB, NOMMAT, SYMAX, NBOPT, KOPT )
C
C     ------------------------------------------------------------------
C
C     ------------------ TYPE_RESU_MECA = TUYAUTERIE ------------------
C
C     ------------------------------------------------------------------
C
      ELSEIF ( TYPMEC .EQ. 'TUYAUTERIE' ) THEN
C
         CALL GETVTX ( ' ', 'OPTION',1,1,1, KOPT, N1 )
C
         IF ( KOPT(1) .EQ. 'FATIGUE' ) THEN
C
            CALL RC3600
C
         ENDIF
C
C     ------------------------------------------------------------------
C
C     ------------------- TYPE_RESU_MECA = UNITAIRE -------------------
C
C     ------------------------------------------------------------------
C
      ELSE
C
         FATIGU = .FALSE.
         PMPB   = .FALSE.
         SN     = .FALSE.
         SNET   = .FALSE.
         LROCHT = .FALSE.
C
         CALL GETFAC ( 'RESU_THER', NBTHER )
         IF ( NBTHER .NE. 0 ) THEN
           SNET   = .TRUE.
           LROCHT = .TRUE.
         ENDIF
C
         CALL GETVTX ( ' ', 'OPTION',1,1,0, K8B, N1 )
         NBOPT = -N1
         CALL GETVTX ( ' ', 'OPTION',1,1,NBOPT, KOPT, N1 )
         DO 30 IOPT = 1, NBOPT
            IF ( KOPT(IOPT) .EQ. 'PM_PB' ) THEN
               PMPB = .TRUE.
            ELSEIF ( KOPT(IOPT) .EQ. 'SN' ) THEN
               SN = .TRUE.
            ELSEIF ( KOPT(IOPT) .EQ. 'FATIGUE' ) THEN
               FATIGU = .TRUE.
               PMPB = .TRUE.
               SN = .TRUE.
            ENDIF
 30      CONTINUE
C
         CALL GETVID ( ' ', 'MATER',  1,1,1, NOMMAT, N1 )
         CALL GETVR8 ( ' ', 'SY_MAX', 1, 1,1,SYMAX,  N1 )
C
         CALL RC3200 ( PMPB, SN, SNET, FATIGU, LROCHT, NOMMAT, SYMAX )
C
      ENDIF
C
C     ------------------------------------------------------------------
C
      CALL TITRE
C
C
      END
