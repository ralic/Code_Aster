      SUBROUTINE ACEMMT ( NOMA, NMMT )
      IMPLICIT NONE
      CHARACTER*8         NOMA
      INTEGER                   NMMT(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C     AFFE_CARA_ELEM
C     LECTURE DE MODI_METRIQUE POUR LES TUYAUX
C     REMPLISSAGE DU TABLEAU NMMT CONTENANT POUR CHAQUE MAILLE
C        0 : SI MODI_METRIQUE : NON
C        1 : SI MODI_METRIQUE : OUI
C ----------------------------------------------------------------------
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
      INTEGER       NBOCPO, IOCC, IBID, IMMT, NBMA, JMA, I, IMA
      CHARACTER*8   MMT, TYPMCL(2)
      CHARACTER*16  MOTFAC, MOTCLS(2)
      CHARACTER*24  MESMAI
      INTEGER      IARG
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      MOTFAC = 'POUTRE'
      CALL GETFAC ( MOTFAC, NBOCPO )
      IF ( NBOCPO .EQ. 0 ) GOTO 9999
C
      MESMAI = '&&ACEMMT.MES_MAILLES'
      MOTCLS(1) = 'GROUP_MA'
      MOTCLS(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
C
      DO 10 IOCC = 1 , NBOCPO

         CALL GETVTX ( MOTFAC, 'MODI_METRIQUE', IOCC,IARG,1, MMT, IBID )
         IF (MMT.EQ.'NON') THEN
            IMMT = 0
         ELSEIF (MMT.EQ.'OUI') THEN
            IMMT = 1
         ENDIF
C
         CALL RELIEM(' ', NOMA, 'NU_MAILLE', MOTFAC, IOCC, 2,
     +                                   MOTCLS, TYPMCL, MESMAI, NBMA )
         IF ( NBMA .NE. 0 ) THEN
            CALL JEVEUO ( MESMAI, 'L', JMA )
            DO 12 I = 1 , NBMA
               IMA = ZI(JMA+I-1)
               NMMT(IMA) = IMMT
 12         CONTINUE
            CALL JEDETR ( MESMAI )
         ENDIF
C
 10   CONTINUE
C
9999  CONTINUE
      CALL JEDEMA()
      END
