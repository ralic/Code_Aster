      SUBROUTINE IRMIIM(IFMIS,IFREQ,NFREQ,NBNO,TABRIG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER      IFMIS, NBNO, I1, I2, IFREQ, NFREQ
      CHARACTER*24 TABRIG
C
      CHARACTER*72 TEXTE
      REAL*8 A(3)
C
C-----------------------------------------------------------------------
      INTEGER I ,J ,JRIG ,NBMODE ,NSAUT 
C-----------------------------------------------------------------------
      NBMODE = 3*NBNO
      CALL WKVECT(TABRIG,'V V R',NBMODE*NBMODE,JRIG)
      REWIND IFMIS
      READ(IFMIS,'(A72)') TEXTE
      IF (TEXTE(1:4).EQ.'XXXX') GOTO 4
      DO 1 I1 =1,NBMODE
      DO 1 I2 =1,NBMODE
        NSAUT = NFREQ
        IF (I1.EQ.1.AND.I2.EQ.1) NSAUT = IFREQ
        DO 2 I = 1, NSAUT
          READ(IFMIS,'(A72)') TEXTE
    2   CONTINUE
        READ(IFMIS,*) (A(J),J=1,3)
        ZR(JRIG+(I2-1)*NBMODE+I1-1) = A(2)
    1 CONTINUE
    4 CONTINUE
      END
