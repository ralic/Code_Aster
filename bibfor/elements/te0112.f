      SUBROUTINE TE0112 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C    - BUT :  CALCUL DES MATRICES DE RAIDEUR GEOMETRIQUE ELEMENTAIRES
C                          POUR LES ELEMENTS DE FOURIER
C                          OPTION : 'RIGI_MECA_GE    '
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      REAL*8             A(3,3,9,9)
      REAL*8             DFDR(9),DFDZ(9),DFDT(9),POIDS,R,XH
      INTEGER            NNO,KP,NPG1,IMATUU,ICONTR,IHARMO,NDIM,NNOS
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM,JGANO
C
C
C-----------------------------------------------------------------------
      INTEGER I ,IC ,IJKL ,IK ,J ,K ,L
      INTEGER NH
C-----------------------------------------------------------------------
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PCONTRR','L',ICONTR)
      CALL JEVECH('PHARMON','L',IHARMO)
      NH = ZI(IHARMO)
      XH = DBLE(NH)
      CALL JEVECH('PMATUUR','E',IMATUU)
C
      DO 113 K=1,3
         DO 113 L=1,3
            DO 113 I=1,NNO
            DO 113 J=1,I
               A(K,L,I,J) = 0.D0
113   CONTINUE
C
C    BOUCLE SUR LES POINTS DE GAUSS
C
      DO 101 KP=1,NPG1
C
        K=(KP-1)*NNO
        IC = ICONTR + (KP-1)*6
C
        CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDR,DFDZ,POIDS)
C
        R   = 0.D0
        DO 102 I=1,NNO
          R   = R + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
102     CONTINUE
        POIDS = POIDS*R
C
        DO 103 I=1,NNO
           DFDR(I) = DFDR(I) + ZR(IVF+K+I-1)/R
           DFDT(I) = - XH * ZR(IVF+K+I-1)/R
103     CONTINUE
C
           DO 106 I=1,NNO
             DO 107 J=1,I
C
               A(1,1,I,J) = A(1,1,I,J) + POIDS *
     &           ( ZR(IC)   * DFDR(I) * DFDR(J)
     &           + ZR(IC+1) * DFDZ(I) * DFDZ(J)
     &           + ZR(IC+2) * DFDT(I) * DFDT(J)
     &           + ZR(IC+3) * (DFDR(I) * DFDZ(J) + DFDZ(I) * DFDR(J))
     &           + ZR(IC+4) * (DFDT(I) * DFDR(J) + DFDR(I) * DFDT(J))
     &           + ZR(IC+5) * (DFDZ(I) * DFDT(J) + DFDT(I) * DFDZ(J)))
C
107            CONTINUE
106        CONTINUE
C
101   CONTINUE
C
           DO 108 I=1,NNO
              DO 109 J=1,I
             A(2,2,I,J) = A(1,1,I,J)
             A(3,3,I,J) = A(1,1,I,J)
109        CONTINUE
108        CONTINUE
C
C PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
C
      DO 111 K=1,3
         DO 111 L=1,3
            DO 111 I=1,NNO
                IK = ((3*I+K-4) * (3*I+K-3)) / 2
            DO 111 J=1,I
                IJKL = IK + 3 * (J-1) + L
                ZR(IMATUU+IJKL-1) = A(K,L,I,J)
111          CONTINUE
C
      END
