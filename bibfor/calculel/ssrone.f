      SUBROUTINE SSRONE(MAG,ISMA,ROTA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C
C     ARGUMENTS:
C     ----------
      INCLUDE 'jeveux.h'
      CHARACTER*8 MAG,ROTA
      INTEGER ISMA
C ----------------------------------------------------------------------
C     IN:  MAG : NOM DU MAILLAGE CONTENANT LA (SUPER)MAILLE ISMA
C          ISMA: NUMERO DE LA (SUPER)MAILLE DANS LE MAILLAGE MAG
C
C     OUT: ROTA: 'OUI' : LA ROTATION EST NECESSAIRE
C                'NON' : LA ROTATION N EST PAS NECESSAIRE
C
C ----------------------------------------------------------------------
C     VARIABLES LOCALES:
C     ------------------
      REAL*8 R1
      INTEGER ROT1,ROT2
C
C
C-----------------------------------------------------------------------
      INTEGER IAPARR ,IRET ,K 
C-----------------------------------------------------------------------
      CALL JEMARQ()
        CALL JEEXIN(MAG//'.PARA_R',IRET)
        IF (IRET.GT.0) THEN
C         -- ROT1= 1 : PEUT-ETRE , 0 : NON , 2 : OUI
          ROT1=1
          CALL JEVEUO(MAG//'.PARA_R','L',IAPARR)
        ELSE
          ROT1=0
        ENDIF
        ROT2=ROT1
        IF (ROT2.EQ.1) THEN
          R1=0.0D0
          DO 3,K=4,6
            R1= R1+ ABS(ZR(IAPARR-1+14*(ISMA-1)+K))
 3        CONTINUE
          ROT1=0
          IF (R1.GT.1.D-6) ROT1=2
        END IF
C
        CALL ASSERT((ROT1.EQ.2).OR.(ROT1.EQ.0))
        IF (ROT1.EQ.2) THEN
          ROTA='OUI'
        ELSE IF (ROT1.EQ.0) THEN
          ROTA='NON'
        END IF
C
      CALL JEDEMA()
      END
