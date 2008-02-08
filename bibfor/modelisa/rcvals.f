      SUBROUTINE RCVALS( STOP, CODRET, NBRES, NOMRES )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                          NBRES
      CHARACTER*(*)      STOP, CODRET(*),     NOMRES(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/02/2008   AUTEUR MACOCCO K.MACOCCO 
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
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
      CHARACTER*2        ARRET, CODE
      CHARACTER*24 VALK
      CHARACTER*8        NOMAIL, PARA
C ----------------------------------------------------------------------
C

      ARRET = STOP
      IF ( ARRET(1:1) .EQ. 'F' ) THEN
         IER = 0
         DO 200 IRES = 1, NBRES
           CODE = CODRET(IRES)
           IF ( CODE .EQ. 'NO' ) THEN
              IER = IER + 1
              PARA = NOMRES(IRES)
              VALK = PARA
              CALL U2MESG('E+','MODELISA9_77',1,VALK,0,0,0,0.D0)
              IF ( ARRET(1:2) .EQ. 'FM' ) THEN
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
