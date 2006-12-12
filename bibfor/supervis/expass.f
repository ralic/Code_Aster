      SUBROUTINE EXPASS( JXVRF , IPASS , ICMDEB , IERTOT )
      IMPLICIT REAL*8 (A-H,O-Z)
      LOGICAL            JXVRF
      INTEGER                  IPASS , ICMDEB , IERTOT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 13/12/2006   AUTEUR PELLET J.PELLET 
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
C     EXECUTION D'UNE PASSE SPECIFIQUE D'OPERATEURS
C     ------------------------------------------------------------------
C IN  JXVRF  : LOGICAL : DOIT ON FAIRE JXVERI (INFO DEPUIS MCSIMP JXVERI
C                        SOUS DEBUT, TRANSMIS PAR LE JDC).
C IN  IPASS  : IS : INDICATEUR DE LA PASSE
C         = 1  DEMANDE DE VERIFICATION SUPPLEMENTAIRE  --- SANS CALCUL -
C         = 2  EXECUTION DE L'OPERATEUR
C IN  ICMDEB : IS : NUMERO D'ORDRE DE LA PREMIERE COMMANDE A EXCECUTER
C OUT IERTOT : IS : NOMBRE D'ERREURS RENCONTREES DANS LA PASSE
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
      CHARACTER*4   CH4, CI4
      CHARACTER*24 VALK(3)
      CHARACTER*8   NOMRES
      CHARACTER*16  CONCEP , NOMCMD
      LOGICAL       LDBG
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      LDBG  = .FALSE.
      IF ( IPASS .EQ. 1 ) THEN
         ICOND = 1
      ELSEIF ( IPASS .EQ. 2 ) THEN
         ICOND = 0
         LDBG  = JXVRF
      ENDIF
C
      ICMD  =  ICMDEB
      IFIN  =  0
      IERTOT = 0
  10  CONTINUE
         IERCMD = 0
         IF (IPASS .EQ. 2 ) CALL GCUOPR( 0 ,ICMDCT )
         CALL EXECOP( ICMD , ICOND , IERTOT , IERCMD , IFIN )
         IF ( LDBG ) THEN
            CALL GETRES(NOMRES,CONCEP,NOMCMD)
            CALL JXVERI('MESSAGE','CONTROLE DE L''INTEGRITE DE LA '//
     &                            'MEMOIRE APRES '//NOMCMD)
         ENDIF
C
         IF ( IPASS .EQ. 1 ) THEN
C           --- VERIFICATIONS SUPPLEMENTAIRES ---
            CALL GETRES(NOMRES,CONCEP,NOMCMD)
            CALL CODENT( ICMD ,'D0',CI4)
            IF ( IERCMD .NE. 0 ) THEN
               IERTOT = IERTOT + IERCMD
               IERCM2=ABS(IERCMD)
               CALL CODENT(IERCM2,'D',CH4)
                VALK(1) = CI4
                VALK(2) = NOMCMD
                VALK(3) = CH4
                CALL U2MESK('E','SUPERVIS_4', 3 ,VALK)
            ENDIF
C
         ELSEIF( IPASS .EQ. 2 ) THEN
C           --- EXECUTIONS ---
            IF ( IERCMD .NE. 0 ) THEN
               IERTOT = IERCMD
               CALL GETRES(NOMRES,CONCEP,NOMCMD)
               CALL U2MESK('E','SUPERVIS_5',1,NOMCMD)
               IFIN = 1
            ELSE
               CALL GCUOPR( 1 ,ICMDCT)
            ENDIF
C
         ENDIF
      IF ( IFIN .EQ. 0 ) THEN
         ICMD = ICMD + 1
      ELSEIF ( IPASS.EQ.1 .AND. IERTOT.NE.0 ) THEN
         IPASS = 2
         ICOND = 0
      ENDIF
C
      CALL JEDEMA()
      END
