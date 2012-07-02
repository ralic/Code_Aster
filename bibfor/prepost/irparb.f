      SUBROUTINE IRPARB(RESU,NBIN,PARIN,NOMJV,NBOUT)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*)     RESU,     PARIN(*),NOMJV
      INTEGER                NBIN,            NBOUT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     DETERMINATION / VERIFICATION DES PARAMETRES
C     ------------------------------------------------------------------
C IN  RESU   : K8  : NOM DU CONCEPT
C IN  NBIN   : I   : NOMBRE DE PARAMETRES EN ENTREE
C IN  PARIN  : K16 : LISTE DES PARAMETRES EN ENTREE
C IN  NOMJV  : K16 : NOM DE L'OBJET JEVEUX DE STOCKAGE DES
C                       NOMS DE PARAMETRES
C OUT NBOUT  : I   : NOMBRE DE PARAMETRES EN SORTIE
C     ------------------------------------------------------------------
      CHARACTER*24 VALK(2)
C     ------------------------------------------------------------------
      INTEGER NBAC,NBPA
      CHARACTER*8  RESU8
      CHARACTER*16 CBID,NOMCMD
C
C-----------------------------------------------------------------------
      INTEGER I ,IRET ,LPOUT 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      RESU8 = RESU
C
      IF (NBIN .EQ. 0 ) THEN
         NBOUT = 0
         LPOUT = 1
      ELSEIF (NBIN .LT. 0 ) THEN
         CALL RSNOPA(RESU8,2,NOMJV,NBAC,NBPA)
         CALL JEEXIN(NOMJV,IRET)
         IF (IRET.GT.0) CALL JEVEUO(NOMJV,'E',LPOUT)
         NBOUT = NBAC + NBPA
      ELSE
C
C       --- VERIFICATION DE L'EXISTANCE DU PARAMETRE
        NBOUT = 0
        CALL JEEXIN (NOMJV,IRET)
        IF (IRET.NE.0) CALL JEDETR(NOMJV)
        CALL WKVECT(NOMJV,'V V K16',NBIN,LPOUT)
        DO 225 I=1, NBIN
           CALL RSEXPA(RESU8,2,PARIN(I),IRET)
           IF (IRET.EQ.0) THEN
              CALL GETRES(CBID,CBID,NOMCMD)
              VALK (1) = PARIN(I)
              VALK (2) = ' '
              CALL U2MESG('A', 'PREPOST5_41',2,VALK,0,0,0,0.D0)
            ELSE
              NBOUT = NBOUT + 1
              ZK16(LPOUT+NBOUT-1) = PARIN(I)
           ENDIF
 225     CONTINUE
      ENDIF
C
C
      CALL JEDEMA()
      END
