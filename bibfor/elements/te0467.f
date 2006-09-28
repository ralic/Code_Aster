      SUBROUTINE TE0467 ( OPTION , NOMTE )
      IMPLICIT NONE
C     ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C     ----------------------------------------------------------------
C     CALCUL DES OPTIONS DES ELEMENTS D'APPUI
C
C     LINEAIRE     :  RIGI_MECA
C     ----------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER       NNO, NBPAR, JMATE, JGEOM, JMATR, IN(3), I, K
      REAL*8        VALRES(2), VALPAR, AIRE, EN, ET, PGL(3,3)
      REAL*8        MATL(6), MATG(6)
      CHARACTER*2   CODRET(2)
      CHARACTER*8   ELREFE, NOMRES(2), NOMPAR
      CHARACTER*10  PHENOM
      CHARACTER*16  OPTION , NOMTE
C
      CALL ELREF1(ELREFE)
C
      CALL JEVECH ( 'PMATERC' , 'L' , JMATE )
      CALL JEVECH ( 'PGEOMER' , 'L' , JGEOM )
      CALL JEVECH ( 'PMATUUR' , 'E' , JMATR )
C
      IF (ELREFE.EQ.'TR3') THEN
         NNO = 3
         CALL DXTPGL ( ZR(JGEOM) , PGL )
      ELSEIF (ELREFE.EQ.'QU4') THEN
         NNO = 4
         CALL DXQPGL ( ZR(JGEOM) , PGL )
      ELSEIF (ELREFE.EQ.'TR6') THEN
         NNO = 6
         CALL DXTPGL ( ZR(JGEOM) , PGL )
      ELSEIF (ELREFE.EQ.'QU8')THEN
         NNO = 8
         CALL DXQPGL ( ZR(JGEOM) , PGL )
      ELSEIF (ELREFE.EQ.'QU9')THEN
         NNO = 9
         CALL DXQPGL ( ZR(JGEOM) , PGL )
      ELSE
         CALL U2MESS('F','ELEMENTS3_97')
      ENDIF
C
      PHENOM = 'APPUI_ELAS'
      NBPAR  = 0
      NOMPAR = ' '
      VALPAR = 0.D0
      NOMRES(1) = 'E_N'
      NOMRES(2) = 'E_TAN'
      CALL RCVALA(ZI(JMATE),' ', PHENOM, NBPAR, NOMPAR, VALPAR, 1,
     &                         NOMRES, VALRES, CODRET, 'FM' )
      CALL RCVALA(ZI(JMATE),' ', PHENOM, NBPAR, NOMPAR, VALPAR, 1,
     &                         NOMRES(2), VALRES(2), CODRET(2), '  ' )
      IF ( CODRET(2) .NE. 'OK' )  VALRES(2) = 0.D0
C
C     --- CALCUL DE L'AIRE DE LA MAILLE ---
C
      CALL DXAIRE ( ZR(JGEOM), AIRE )
      EN = AIRE * VALRES(1) / NNO
      ET = AIRE * VALRES(2) / NNO
C
C     --- OPTION LINEAIRE ---
C
      IF ( OPTION .EQ. 'RIGI_MECA'  ) THEN
         MATL(1) = ET
         MATL(2) = 0.D0
         MATL(3) = ET
         MATL(4) = 0.D0
         MATL(5) = 0.D0
         MATL(6) = EN
         CALL UTPSLG ( 1 , 3 , PGL , MATL , MATG )
         DO 10 I = 1 , NNO
            K = 3 * ( I - 1 )
            IN(1) =  K    * (K+1) / 2 + 3*(I-1)
            IN(2) = (K+1) * (K+2) / 2 + 3*(I-1)
            IN(3) = (K+2) * (K+3) / 2 + 3*(I-1)
            ZR(JMATR-1+IN(1)+1) = MATG(1)
            ZR(JMATR-1+IN(2)+1) = MATG(2)
            ZR(JMATR-1+IN(2)+2) = MATG(3)
            ZR(JMATR-1+IN(3)+1) = MATG(4)
            ZR(JMATR-1+IN(3)+2) = MATG(5)
            ZR(JMATR-1+IN(3)+3) = MATG(6)
 10      CONTINUE
C
      ELSE
         CALL U2MESS('F','ELEMENTS2_67')
      ENDIF
C
      END
