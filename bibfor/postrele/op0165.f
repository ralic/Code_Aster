      SUBROUTINE OP0165 ( IER )
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
      IMPLICIT   NONE
      INTEGER            IER
C     ------------------------------------------------------------------
C MODIF POSTRELE  DATE 21/03/2005   AUTEUR CIBHHLV L.VIVAN 
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
      INTEGER       IBID, N1, NBOPT, IOPT, NBTHER, NSN, I
      LOGICAL       PMPB, SN, SNET, FATIGU
      CHARACTER*2   CODRET
      CHARACTER*8   K8B, NOMMAT
      CHARACTER*16  TYPTAB, TYPMEC, KOPT(4), PHENOM
C DEB ------------------------------------------------------------------
C
      CALL INFMAJ
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
         CALL GETVID ( ' ', 'MATER', 1, 1,1,NOMMAT, N1 )
C
         CALL RCCOME ( NOMMAT, 'RCCM', PHENOM, CODRET )
         IF ( CODRET .EQ. 'NO' ) THEN
            CALL UTMESS('F','POST_RCCM','IL FAUT DEFINIR LE '//
     +                   'COMPORTEMENT "RCCM" DANS "DEFI_MATERIAU"')
         ENDIF
C
         CALL RCEVOL ( TYPTAB, NOMMAT, NBOPT, KOPT )
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
         SNET   = .FALSE.
         CALL GETFAC ( 'RESU_THER', NBTHER )
         IF ( NBTHER .NE. 0 ) THEN
           DO 32 I = 1, NBTHER
             CALL GETVID ('RESU_THER','TABL_RESU_THER',I,1,0,K8B,NSN)
             IF ( NSN .NE. 0 ) SNET = .TRUE.
 32        CONTINUE
         ENDIF
C
         CALL GETVTX ( ' ', 'OPTION',1,1,0, K8B, N1 )
         NBOPT = -N1
         CALL GETVTX ( ' ', 'OPTION',1,1,NBOPT, KOPT, N1 )
         FATIGU = .FALSE.
         PMPB   = .FALSE.
         SN     = .FALSE.
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
         CALL GETVID ( ' ', 'MATER', 1,1,1, NOMMAT, N1 )
C
         CALL RC3200 ( PMPB, SN, SNET, FATIGU, NOMMAT )
C
      ENDIF
C
C     ------------------------------------------------------------------
C
      CALL TITRE
C
C
      END
