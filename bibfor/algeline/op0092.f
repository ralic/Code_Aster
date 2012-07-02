      SUBROUTINE OP0092()
      IMPLICIT NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     ------------------------------------------------------------------
C
C     OPERATEUR :   CALC_CHAR_SEISME
C
C     ------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'
      CHARACTER*8 MONMOT
      INTEGER      IARG
C     ------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER NBD ,NBDIR ,NBV 
      REAL*8 DEPL 
C-----------------------------------------------------------------------
      CALL INFMAJ()
C
      CALL GETVR8(' ','DIRECTION',0,IARG,0,DEPL,NBD)
      NBDIR = -NBD
      IF ( NBDIR .NE. 3  .AND.  NBDIR .NE. 6 ) THEN
         CALL U2MESS('F','ALGELINE2_76')
      ENDIF
C
C     SEISME ????
C
      MONMOT = ' '
      CALL GETVTX(' ','MONO_APPUI',0,IARG,1,MONMOT,NBV)
      IF (MONMOT(1:3).EQ.'OUI') THEN
C
C        --- SEISME MONO-APPUI ---
         CALL SIMONO
      ELSE
C
C        --- SEISME MULT-APPUI ---
         CALL SIMULT
      ENDIF
C
      CALL TITRE()
C
      END
