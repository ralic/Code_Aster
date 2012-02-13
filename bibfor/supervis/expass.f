      SUBROUTINE EXPASS( JXVRF )
      IMPLICIT REAL*8 (A-H,O-Z)
      LOGICAL            JXVRF
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 14/02/2012   AUTEUR COURTOIS M.COURTOIS 
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
C     EXECUTION D'UNE PASSE SPECIFIQUE D'OPERATEURS
C     ------------------------------------------------------------------
C IN  JXVRF  : LOGICAL : DOIT ON FAIRE JXVERI (INFO DEPUIS MCSIMP JXVERI
C                        SOUS DEBUT, TRANSMIS PAR LE JDC).
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         -
C     ROUTINE(S) FORTRAN     :
C         -
C     ------------------------------------------------------------------
C FIN EXPASS
C     ------------------------------------------------------------------
C
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     --- VARIABLES LOCALES --------------------------------------------
      CHARACTER*8  NOMRES
      CHARACTER*16 CONCEP , NOMCMD
      LOGICAL      LDBG
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      LDBG  = .FALSE.
      LDBG  = JXVRF
C
      CALL GCUOPR( 0 ,ICMDCT )
      CALL EXECOP()
      IF ( LDBG ) THEN
         CALL GETRES(NOMRES,CONCEP,NOMCMD)
         CALL JXVERI(' ')
      ENDIF
C
C     --- EXECUTIONS ---
      CALL GCUOPR( 1 ,ICMDCT)
C
      CALL JEDEMA()
      END
