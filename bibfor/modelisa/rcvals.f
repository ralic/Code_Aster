      SUBROUTINE RCVALS(IARRET, ICODRE, NBRES, NOMRES )
      IMPLICIT NONE
      INTEGER IARRET,NBRES
      INTEGER    ICODRE(NBRES)
      CHARACTER*(*)    NOMRES(NBRES)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)

C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      CHARACTER*24 VALK
      CHARACTER*8  NOMAIL, PARA
      INTEGER IER,IADZI,IAZK24,IRES
C ----------------------------------------------------------------------
C

      IF ( IARRET .GE. 1 ) THEN
         IER = 0
         DO 200 IRES = 1, NBRES
           IF ( ICODRE(IRES) .EQ. 1 ) THEN
              IER = IER + 1
              PARA = NOMRES(IRES)
              VALK = PARA
              CALL U2MESG('E+','MODELISA9_77',1,VALK,0,0,0,0.D0)
              IF ( IARRET .EQ. 1 ) THEN
                 CALL TECAEL(IADZI,IAZK24)
                 NOMAIL = ZK24(IAZK24-1+3)(1:8)
                 VALK = NOMAIL
                 CALL U2MESG('E+','MODELISA9_78',1,VALK,0,0,0,0.D0)
              ENDIF
              CALL U2MESS('E','VIDE_1')
           ENDIF
 200     CONTINUE
         IF ( IER .NE. 0 ) THEN
            CALL U2MESS('F','MODELISA6_4')
         ENDIF
      ENDIF
C
      END
