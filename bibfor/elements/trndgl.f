      SUBROUTINE TRNDGL (NBX,VECTN,VECTPT,DEPLG,DEPLL,ROTFIC)
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
C
      INCLUDE 'jeveux.h'
      INTEGER NBX
      REAL*8 VECTN(9,3),VECTPT(9,2,3)
      REAL*8 DEPLG(*),DEPLL(*),T(3,3),ROTFIC(*)
      INTEGER I ,I1 ,I2 ,IB ,J
C-----------------------------------------------------------------------
C
      DO 10 IB=1,NBX
C
C     RESTITUTION DE LA MATRICE DE PASSAGE
C
      DO 15  I=1,2
      DO 20  J=1,3
         T(I,J)=VECTPT(IB,I,J)
 20   CONTINUE
 15   CONTINUE
         T(3,1)=VECTN (IB,1)
         T(3,2)=VECTN (IB,2)
         T(3,3)=VECTN (IB,3)
C
         I1=5*(IB-1)
         I2=6*(IB-1)
C
C     LES TERMES DE TRANSLATION
C
         IF (IB.LE.NBX-1) THEN
            DEPLL(I1+1)=DEPLG(I2+1)
            DEPLL(I1+2)=DEPLG(I2+2)
            DEPLL(I1+3)=DEPLG(I2+3)
C
C     LES TERMES DE ROTATION (2 SEULEMENT)
C
            DEPLL(I1+4)=T(1,1)*DEPLG(I2+4)+T(1,2)*DEPLG(I2+5)
     &                                    +T(1,3)*DEPLG(I2+6)
C
            DEPLL(I1+5)=T(2,1)*DEPLG(I2+4)+T(2,2)*DEPLG(I2+5)
     &                                    +T(2,3)*DEPLG(I2+6)
C
             ROTFIC(IB)=T(3,1)*DEPLG(I2+4)+T(3,2)*DEPLG(I2+5)
     &                                    +T(3,3)*DEPLG(I2+6)
         ELSE
            DEPLL(I1+1)=T(1,1)*DEPLG(I2+1)+T(1,2)*DEPLG(I2+2)
     &                                    +T(1,3)*DEPLG(I2+3)
C
            DEPLL(I1+2)=T(2,1)*DEPLG(I2+1)+T(2,2)*DEPLG(I2+2)
     &                                    +T(2,3)*DEPLG(I2+3)
C
             ROTFIC(IB)=T(3,1)*DEPLG(I2+1)+T(3,2)*DEPLG(I2+2)
     &                                    +T(3,3)*DEPLG(I2+3)
         ENDIF
C
 10   CONTINUE
C
      END
