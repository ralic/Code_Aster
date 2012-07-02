      SUBROUTINE TE0316(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C.......................................................................
      IMPLICIT NONE

C     BUT: CALCUL DES VECTEURS ELEMENTAIRES DE FLUX FLUIDE EN MECANIQUE
C          ELEMENTS ISOPARAMETRIQUES 1D

C          OPTION : 'FLUX_FLUI_X '

C     ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................

      INCLUDE 'jeveux.h'
      CHARACTER*16 NOMTE,OPTION
      REAL*8 POIDS,NX,NY,NORM(2)
      INTEGER IPOIDS,IVF,IDFDE,IGEOM
      INTEGER NDI,NNO,KP,NPG
      INTEGER LDEC
      LOGICAL  LTEATT, LAXI

C-----------------------------------------------------------------------
      INTEGER I ,IJ ,IMATTT ,J ,JGANO ,NDIM ,NNOS 

      REAL*8 R 
C-----------------------------------------------------------------------
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
      NDI = NNO* (NNO+1)/2
      LAXI = .FALSE.
      IF (LTEATT(' ','AXIS','OUI')) LAXI = .TRUE.

      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATTTR','E',IMATTT)

      DO 10 I = 1,NDI
        ZR(IMATTT+I-1) = 0.0D0
   10 CONTINUE

C     BOUCLE SUR LES POINTS DE GAUSS

      DO 50 KP = 1,NPG
        LDEC = (KP-1)*NNO
        NX = 0.0D0
        NY = 0.0D0

C ON CALCULE L ACCEL AU POINT DE GAUSS
        CALL VFF2DN(NDIM,NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),NX,NY,POIDS)
        NORM(1) = NX
        NORM(2) = NY

C CAS AXISYMETRIQUE

        IF (LAXI) THEN
          R = 0.D0
          DO 20 I = 1,NNO
            R = R + ZR(IGEOM+2* (I-1))*ZR(IVF+LDEC+I-1)
   20     CONTINUE
          POIDS = POIDS*R
        END IF

CCDIR$ IVDEP
        DO 40 I = 1,NNO
          DO 30 J = 1,I
            IJ = (I-1)*I/2 + J
            ZR(IMATTT+IJ-1) = ZR(IMATTT+IJ-1) +
     &                        POIDS*NORM(1)*ZR(IVF+LDEC+I-1)*
     &                        ZR(IVF+LDEC+J-1)
   30     CONTINUE
   40   CONTINUE
   50 CONTINUE
      END
