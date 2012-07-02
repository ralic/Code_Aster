      SUBROUTINE TE0217(OPTION,NOMTE)
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
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16 OPTION,NOMTE
C.......................................................................

C     BUT: CALCUL DU SECOND MEMBRE ELEMENTAIRE EN THERMIQUE CORRESPON-
C          DANT A UN GRADIENT IMPOSE DE TEMPERATURE
C          ELEMENTS ISOPARAMETRIQUES 3D

C          OPTION : 'CHAR_THER_GRAI_R '

C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................

      INTEGER ICODRE,KPG,SPT
      CHARACTER*8 NOMPAR(4),GRXF,GRYF,GRZF,FAMI,POUM

      REAL*8 VALRES,VALPAR(4),X,Y,Z
      REAL*8 DFDX(27),DFDY(27),DFDZ(27),POIDS,GRX,GRY,GRZ
      INTEGER IPOIDS,IVF,IDFDE,IGEOM
      INTEGER JGANO,NNO,NDIM,KP,NPG1,I,L,IVECTT,IGRAI,IMATE

      LOGICAL FONC


C-----------------------------------------------------------------------
      INTEGER IER ,ITEMPS ,NNOS 
C-----------------------------------------------------------------------
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)

      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PVECTTR','E',IVECTT)
      FAMI='FPG1'
      KPG=1
      SPT=1
      POUM='+'
      CALL RCVALB(FAMI,KPG,SPT,POUM,ZI(IMATE),' ','THER',1,
     &            'INST',0.D0,1,'LAMBDA',VALRES,ICODRE,1)

      IF (OPTION.EQ.'CHAR_THER_GRAI_R') THEN
        FONC = .FALSE.
        CALL JEVECH('PGRAINR','L',IGRAI)
        GRX = ZR(IGRAI)
        GRY = ZR(IGRAI+1)
        GRZ = ZR(IGRAI+2)
      ELSE IF (OPTION.EQ.'CHAR_THER_GRAI_F') THEN
        FONC = .TRUE.
        CALL JEVECH('PTEMPSR','L',ITEMPS)
        CALL JEVECH('PGRAINF','L',IGRAI)
        GRXF = ZK8(IGRAI)
        GRYF = ZK8(IGRAI+1)
        GRZF = ZK8(IGRAI+2)
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'Z'
        NOMPAR(4) = 'INST'
        VALPAR(4) = ZR(ITEMPS)
      END IF

      DO 40 KP = 1,NPG1
        L = (KP-1)*NNO
        CALL DFDM3D ( NNO, KP, IPOIDS, IDFDE,
     &                ZR(IGEOM), DFDX, DFDY, DFDZ, POIDS )

        X = 0.D0
        Y = 0.D0
        Z = 0.D0
        DO 20 I = 1,NNO
          X = X + ZR(IGEOM-1+3* (I-1)+1)*ZR(IVF+L+I-1)
          Y = Y + ZR(IGEOM-1+3* (I-1)+2)*ZR(IVF+L+I-1)
          Z = Z + ZR(IGEOM-1+3* (I-1)+3)*ZR(IVF+L+I-1)
   20   CONTINUE

        POIDS = POIDS*VALRES

        IF (FONC) THEN
          VALPAR(1) = X
          VALPAR(2) = Y
          VALPAR(3) = Z
          CALL FOINTE('FM',GRXF,4,NOMPAR,VALPAR,GRX,IER)
          CALL FOINTE('FM',GRYF,4,NOMPAR,VALPAR,GRY,IER)
          CALL FOINTE('FM',GRZF,4,NOMPAR,VALPAR,GRZ,IER)
        END IF

        DO 30 I = 1,NNO
          ZR(IVECTT+I-1) = ZR(IVECTT+I-1) +
     &                     POIDS* (+GRX*DFDX(I)+GRY*DFDY(I)+GRZ*DFDZ(I))
   30   CONTINUE

   40 CONTINUE

      END
