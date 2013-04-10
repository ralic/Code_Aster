      SUBROUTINE UTCONO ( MCFAC, MOCLE, IOCC, NOMAIL, NDIM, COOR, IRET )
      IMPLICIT  NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNOM
      INTEGER             IOCC, NDIM, IRET
      REAL*8              COOR(*)
      CHARACTER*8         NOMAIL
      CHARACTER*(*)       MCFAC, MOCLE(3)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/04/2013   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C
C ----------------------------------------------------------------------
C     ------------------------------------------------------------------
      INTEGER      N1, N2, N3, NUMNO, I, IER, JCOOR
      INTEGER VALI(2)
      REAL*8       R8B
      CHARACTER*8  K8B, NOEUD
      CHARACTER*16 CONCEP, CMD
      CHARACTER*24 COORD, NOMNOE, NOMGRN
      CHARACTER*24 VALK(3)
      INTEGER      IARG
C     ------------------------------------------------------------------
      CALL JEMARQ()
      IRET = 0
C
      CALL GETVR8 ( MCFAC, MOCLE(1), IOCC,IARG,0, R8B, N1 )
      IF ( N1 .NE. 0 ) THEN
         CALL GETVR8 ( MCFAC, MOCLE(1), IOCC,IARG,NDIM, COOR, N1 )
         IF ( N1 .LT. NDIM ) THEN
            CALL GETRES ( K8B, CONCEP, CMD )
            VALK (1) = MCFAC
            VALI (1) = IOCC
            CALL U2MESG('F+','MODELISA9_23',1,VALK,1,VALI,0,0.D0)
            IF ( NDIM .EQ. 2 ) THEN
              CALL U2MESG('F+','MODELISA9_24',0,' ',0,0,0,0.D0)
            ELSE
              CALL U2MESG('F+','MODELISA9_25',0,' ',0,0,0,0.D0)
            ENDIF
            VALI (1) = ABS(N1)
            VALI (2) = NDIM
            VALK (1) = MOCLE(1)
            CALL U2MESG('F','MODELISA9_26',1,VALK,2,VALI,0,0.D0)
         ENDIF
         IRET = 1
         GOTO 9999
      ENDIF
C
      COORD  = NOMAIL//'.COORDO    .VALE'
      NOMNOE = NOMAIL//'.NOMNOE         '
      CALL JEVEUO ( COORD, 'L', JCOOR )
C
      CALL GETVTX ( MCFAC, MOCLE(2), IOCC,IARG,0, K8B, N2 )
      IF ( N2 .NE. 0 ) THEN
         CALL GETVTX ( MCFAC, MOCLE(2), IOCC,IARG,1, NOEUD, N2 )
          CALL JENONU ( JEXNOM(NOMNOE,NOEUD), NUMNO )
          IF ( NUMNO .EQ. 0 ) THEN
             CALL GETRES ( K8B, CONCEP, CMD )
             VALK (1) = MCFAC
             VALK (2) = MOCLE(2)
             VALK (3) = NOEUD
             VALI (1) = IOCC
             CALL U2MESG('F','MODELISA9_27',3,VALK,1,VALI,0,0.D0)
          ENDIF
          DO 10 I = 1 , NDIM
             COOR(I) = ZR(JCOOR+3*(NUMNO-1)+I-1)
 10       CONTINUE
          IRET = 1
          GOTO 9999
       ENDIF
C
       CALL GETVTX ( MCFAC, MOCLE(3), IOCC,IARG,1, K8B, N3 )
       IF ( N3 .NE. 0 ) THEN
          CALL GETVTX ( MCFAC, MOCLE(3), IOCC,IARG,1, NOMGRN, N3 )
          CALL UTNONO ( ' ', NOMAIL, 'NOEUD', NOMGRN, K8B, IER )
          IF ( IER .EQ. 10 ) THEN
             CALL GETRES ( K8B, CONCEP, CMD )
             VALK (1) = MCFAC
             VALK (2) = MOCLE(3)
             VALK (3) = NOMGRN
             VALI (1) = IOCC
             CALL U2MESG('F','MODELISA9_28',3,VALK,1,VALI,0,0.D0)
          ELSEIF ( IER .EQ. 1 ) THEN
             CALL GETRES ( K8B, CONCEP, CMD )
             VALK (1) = MCFAC
             VALK (2) = K8B
             VALI (1) = IOCC
             CALL U2MESG('A','MODELISA9_29',2,VALK,1,VALI,0,0.D0)
          ENDIF
          CALL JENONU ( JEXNOM(NOMNOE,K8B), NUMNO )
          DO 20 I = 1 , NDIM
             COOR(I) = ZR(JCOOR+3*(NUMNO-1)+I-1)
 20       CONTINUE
          IRET = 1
          GOTO 9999
       ENDIF
C
 9999 CONTINUE
      CALL JEDEMA()
      END
