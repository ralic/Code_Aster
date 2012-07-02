      SUBROUTINE LECVEC(IAD,LONG,TYPE,UNITE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CATAELEM  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
      IMPLICIT NONE



      INCLUDE 'jeveux.h'
      CHARACTER*3 TYPE
      INTEGER UNITE


C-----------------------------------------------------------------------
      INTEGER IAD ,K ,LONG 
C-----------------------------------------------------------------------
      IF (TYPE.EQ.'R') THEN
        DO 11, K=1,LONG
         READ(UNITE,'(1E12.5)') ZR(IAD-1+K)
 11     CONTINUE

      ELSE IF (TYPE.EQ.'I') THEN
        DO 21, K=1,LONG
         READ(UNITE,'(I12)') ZI(IAD-1+K)
 21     CONTINUE

      ELSE IF (TYPE.EQ.'K8') THEN
        DO 31, K=1,LONG
         READ(UNITE,'(A8)') ZK8(IAD-1+K)
 31     CONTINUE

      ELSE IF (TYPE.EQ.'K16') THEN
        DO 32, K=1,LONG
         READ(UNITE,'(A16)') ZK16(IAD-1+K)
 32     CONTINUE

      ELSE IF (TYPE.EQ.'K24') THEN
        DO 33, K=1,LONG
         READ(UNITE,'(A24)') ZK24(IAD-1+K)
 33     CONTINUE

      ELSE IF (TYPE.EQ.'K32') THEN
        DO 34, K=1,LONG
         READ(UNITE,'(A32)') ZK32(IAD-1+K)
 34     CONTINUE

      ELSE IF (TYPE.EQ.'K80') THEN
        DO 35, K=1,LONG
         READ(UNITE,'(A80)') ZK80(IAD-1+K)
 35     CONTINUE
      ELSE
         CALL ASSERT(.FALSE.)
      END IF


      END
