      SUBROUTINE TE0264 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          OPTION : 'CHAR_THER_SOUR_F'
C                          ELEMENTS FOURIER
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
C-----------------------------------------------------------------------
      INTEGER ICODE ,NBRES 
      REAL*8 SOUN ,SOUNP1 ,THETA 
C-----------------------------------------------------------------------
      PARAMETER         ( NBRES=3 )
      CHARACTER*8        NOMPAR(NBRES)
      REAL*8             VALPAR(NBRES)
      REAL*8             DFDX(9),DFDY(9),POIDS,R,Z,SOUR
      INTEGER            NNO,KP,NPG1,I,K,ITEMPS,IVECTT,ISOUR,NNOS,JGANO
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM,NDIM
C
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PSOURCF','L',ISOUR)
      CALL JEVECH('PVECTTR','E',IVECTT)
      THETA = ZR(ITEMPS+2)
      NOMPAR(1) = 'X'
      NOMPAR(2) = 'Y'
      NOMPAR(3) = 'INST'
C
      DO 101 KP=1,NPG1
        K=(KP-1)*NNO
        CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
        R = 0.D0
        Z = 0.D0
        DO 102 I=1,NNO
          R = R + ZR(IGEOM+2*(I-1)  )*ZR(IVF+K+I-1)
          Z = Z + ZR(IGEOM+2*(I-1)+1)*ZR(IVF+K+I-1)
102     CONTINUE
        POIDS = POIDS*R
        VALPAR(1) = R
        VALPAR(2) = Z
        VALPAR(3) = ZR(ITEMPS)
        CALL FOINTE('FM',ZK8(ISOUR),3,NOMPAR,VALPAR,SOUNP1,ICODE)
        VALPAR(3) = ZR(ITEMPS)-ZR(ITEMPS+1)
        CALL FOINTE('FM',ZK8(ISOUR),3,NOMPAR,VALPAR,SOUN,ICODE)
        SOUR = THETA*SOUNP1 + (1.0D0-THETA)*SOUN
CCDIR$ IVDEP
        DO 103 I=1,NNO
           ZR(IVECTT+I-1) = ZR(IVECTT+I-1) + POIDS
     &                    * ZR(IVF+K+I-1) * SOUR
103     CONTINUE
101   CONTINUE
      END
