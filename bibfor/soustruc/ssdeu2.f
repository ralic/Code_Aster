      SUBROUTINE SSDEU2(NVAL,ILISTE,NVALAP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C     ARGUMENTS:
C     ----------
      INCLUDE 'jeveux.h'
      INTEGER NVAL,ILISTE(NVAL),NVALAP
C ----------------------------------------------------------------------
C     BUT:
C        - ENLEVER LES DOUBLONS D'UNE LISTE D'ENTIERS.
C          LES ENTIERS SONT RETASSES VERS LE DEBUT DE LA LISTE.
C        - ENLEVER LES "ZERO".
C
C     IN:
C        NVAL  :  NOMBRE D'ENTIERS A PRENDRE EN COMPTE.
C     IN/OUT:
C        ILISTE:  LISTE DES ENTIERS.
C                 EN SORTIE, ELLE EST RETASSEE. LA FIN DE LA LISTE EST
C                 MISE A ZERO.
C     OUT:
C        NVALAP:  NOMBRE D'ENTIERS DIFFERENTS TROUVES DANS LA LISTE.
C
C ----------------------------------------------------------------------
      CHARACTER*8 KBID
C
C
C     -- L'OBJET DE TRAVAIL "&&SSDEU2.WK1" CONTIENDRA DES "1" AU NIVEAU
C        DES ENTIERS A ELIMINER.
C     ---------------------------------------------------------------
      CALL JEMARQ()
      CALL JEEXIN('&&SSDEU2.WK1',IRET)
      IF (IRET.EQ.0) THEN
         NDIM=MAX(1000,2*NVAL)
         CALL WKVECT('&&SSDEU2.WK1','V V I',NDIM,IAWK1)
      ELSE
         CALL JELIRA('&&SSDEU2.WK1','LONMAX',NDIM,KBID)
         IF (NDIM.LT.NVAL) THEN
            CALL JEDETC('V','&&SSDEU2.WK1',1)
            CALL WKVECT('&&SSDEU2.WK1','V V I',2*NVAL,IAWK1)
         ELSE
            CALL JEVEUO('&&SSDEU2.WK1','E',IAWK1)
         END IF
      END IF
C
C     -- MISE A ZERO DE "&&SSDEU2.WK1":
C     ---------------------------------
      DO 10, I=1,NVAL
         ZI(IAWK1-1+I)=0
 10   CONTINUE
C
C     -- MISE A "1" PARTIELLE DE  "&&SSDEU2.WK1":
C     -------------------------------------------
      NVALAP= NVAL
      DO 1 , I=1,NVAL
         IF (ILISTE(I).EQ.0) THEN
            ZI(IAWK1-1+I)=1
            NVALAP= NVALAP-1
            GO TO 1
         END IF
         DO 2 , J=1,I-1
            IF (ILISTE(J).EQ.ILISTE(I)) THEN
               ZI(IAWK1-1+I)=1
               NVALAP= NVALAP-1
               GO TO 1
            END IF
 2       CONTINUE
 1    CONTINUE
C
C
C     -- RETASSAGE DE LA LISTE:
C     -------------------------
      IDECAL=0
      DO 3 , I=1,NVAL
         IF (ZI(IAWK1-1+I).EQ.1) THEN
            IDECAL= IDECAL+1
         ELSE
            ILISTE(I-IDECAL)=ILISTE(I)
         END IF
 3    CONTINUE
C
C     -- ON COMPLETE PAR DES ZERO:
C     ----------------------------
      DO 4 , I=NVALAP+1,NVAL
         ILISTE(I)=0
 4    CONTINUE
C
C
      CALL JEDEMA()
      END
