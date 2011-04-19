      SUBROUTINE TE0220 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
      IMPLICIT NONE
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:
C                         CALCUL DE L'ENERGIE THERMIQUE A L'EQUILIBRE
C                         OPTION : 'EPOT_ELEM_TEMP'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
C ----- DEBUT --- COMMUNS NORMALISES  JEVEUX  --------------------------
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
      INTEGER ICODRE
      CHARACTER*8        NOMPAR
      REAL*8             VALRES, VALPAR
      REAL*8             DFDX(9),DFDY(9),POIDS,FLUX,FLUY,EPOT
      INTEGER            NDIM,NNO,NNOS,NPG,KP,J,ITEMPE,ITEMP,IENER
      INTEGER            IPOIDS,IVF,IDFDE,JGANO,IGEOM,IMATE,IRET,NBPAR
C     ------------------------------------------------------------------
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PTEMPER','L',ITEMPE)
      CALL JEVECH('PENERDR','E',IENER)
C
      CALL TECACH('ONN','PTEMPSR',1,ITEMP,IRET)
      IF ( ITEMP .EQ. 0 ) THEN
         NBPAR  = 0
         NOMPAR = ' '
         VALPAR = 0.D0
      ELSE
         NBPAR  = 1
         NOMPAR = 'INST'
         VALPAR = ZR(ITEMP)
      ENDIF
C
      CALL RCVALA(ZI(IMATE),' ','THER',NBPAR,NOMPAR,VALPAR,1,'LAMBDA',
     &              VALRES,ICODRE,1)
C
      EPOT = 0.D0
      DO 101 KP=1,NPG
         CALL DFDM2D ( NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS )
         FLUX = 0.D0
         FLUY = 0.D0
         DO 110 J=1,NNO
             FLUX = FLUX + ZR(ITEMPE+J-1)*DFDX(J)
             FLUY = FLUY + ZR(ITEMPE+J-1)*DFDY(J)
 110     CONTINUE
C
         EPOT = EPOT - ( FLUX**2 + FLUY**2 )*POIDS
101   CONTINUE
      ZR(IENER) = EPOT * VALRES / 2.D0
C
      END
