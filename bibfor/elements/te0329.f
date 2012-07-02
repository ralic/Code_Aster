      SUBROUTINE TE0329(OPTION,NOMTE)
      IMPLICIT NONE
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
C....................................................................
C   CALCUL DES TERMES ELEMENTAIRES DE L'ACCEPTANCE
C     OPTION : ACCEPTANCE
C....................................................................
C
      INCLUDE 'jeveux.h'
      CHARACTER*7        IELEM,IMODE
      CHARACTER*16       NOMTE,OPTION
      CHARACTER*24       VETEL
      REAL*8             SX(9,9),SY(9,9),SZ(9,9),JAC(9)
      REAL*8             NX(9),NY(9),NZ(9),NORM(3,9),ACC(3,9)
      REAL*8             FLUFN(9),ACLOC(3,8)
      REAL*8             X(3,9)
      INTEGER            IPOIDS,IVF,IDFDX,IDFDY,IGEOM
      INTEGER            NDIM,NNO,IPG,NPG1
      INTEGER            IDEC,JDEC,KDEC,LDEC
      INTEGER            NNOS,JGANO
C     ------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,IACCE ,IADZI ,IAZK24 ,IDIM ,IHARM ,INO 
      INTEGER IVECTU ,IVETEL ,J ,JNO ,K 
C-----------------------------------------------------------------------
      IF ( NOMTE(1:5).EQ.'THER_' ) THEN
C          ---------------------
          CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,
     &                IDFDX,JGANO)
          IDFDY  = IDFDX  + 1
C
      CALL JEVECH('PACCELR','L',IACCE)
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PNUMMOD','L',IHARM)
      CALL JEVECH('PVECTUR','E',IVECTU)
C
      DO 1200 I=1,NNO
         ACLOC(1,I)=0.0D0
         ACLOC(2,I)=0.0D0
         ACLOC(3,I)=0.0D0
1200  CONTINUE
C
        K=0
        DO 1201 I=1,NNO
          DO 20 IDIM=1,3
            K=K+1
            ACLOC(IDIM,I) = ZR(IACCE+K-1)
20        CONTINUE
1201    CONTINUE
C
       DO 1052 IPG=1,NPG1
          ACC(1,IPG)=0.0D0
          ACC(2,IPG)=0.0D0
          ACC(3,IPG)=0.0D0
1052   CONTINUE

C
       DO 1051 IPG=1,NPG1
         LDEC=(IPG-1)*NNO
         DO 105 I=1,NNO
           ACC(1,IPG) = ACC(1,IPG) + ACLOC(1,I)*
     &           ZR(IVF+LDEC+I-1)
           ACC(2,IPG) = ACC(2,IPG) + ACLOC(2,I)*
     &           ZR(IVF+LDEC+I-1)
           ACC(3,IPG) = ACC(3,IPG) + ACLOC(3,I)*
     &           ZR(IVF+LDEC+I-1)
105      CONTINUE
1051   CONTINUE
C     CALCUL DES PRODUITS VECTORIELS OMI X OMJ
C
      DO 21 INO = 1,NNO
         I = IGEOM + 3*(INO-1) -1
         DO 22 JNO = 1,NNO
            J = IGEOM + 3*(JNO-1) -1
            SX(INO,JNO) = ZR(I+2) * ZR(J+3) - ZR(I+3) * ZR(J+2)
            SY(INO,JNO) = ZR(I+3) * ZR(J+1) - ZR(I+1) * ZR(J+3)
            SZ(INO,JNO) = ZR(I+1) * ZR(J+2) - ZR(I+2) * ZR(J+1)
22       CONTINUE
21    CONTINUE

C
C     BOUCLE SUR LES POINTS DE GAUSS

      DO 101 IPG=1,NPG1

         KDEC=(IPG-1)*NNO*NDIM
         LDEC=(IPG-1)*NNO

         NX(IPG) = 0.0D0
         NY(IPG) = 0.0D0
         NZ(IPG) = 0.0D0

         DO 102 I=1,NNO
           IDEC = (I-1)*NDIM
            DO 104 J=1,NNO
              JDEC = (J-1)*NDIM

              NX(IPG) = NX(IPG) + ZR(IDFDX+KDEC+IDEC)
     &            * ZR(IDFDY+KDEC+JDEC) * SX(I,J)
              NY(IPG) = NY(IPG) + ZR(IDFDX+KDEC+IDEC)
     &            * ZR(IDFDY+KDEC+JDEC) * SY(I,J)
              NZ(IPG) = NZ(IPG) + ZR(IDFDX+KDEC+IDEC)
     &        * ZR(IDFDY+KDEC+JDEC) * SZ(I,J)

104        CONTINUE
102      CONTINUE

C      CALCUL DU JACOBIEN AU POINT DE GAUSS IPG

         JAC(IPG) = SQRT (NX(IPG)*NX(IPG) + NY(IPG)*NY(IPG)
     &                                    + NZ(IPG)*NZ(IPG))

C       CALCUL DE LA NORMALE UNITAIRE

          NORM(1,IPG) = NX(IPG)/JAC(IPG)
          NORM(2,IPG) = NY(IPG)/JAC(IPG)
          NORM(3,IPG) = NZ(IPG)/JAC(IPG)
101     CONTINUE

C    CALCUL DE COORDONNEES AUX POINTS DE GAUSS

           DO 90 IPG=1,NPG1
             LDEC=(IPG-1)*NNO
             X(1,IPG)=0.0D0
             X(2,IPG)=0.0D0
             X(3,IPG)=0.0D0

              DO 91 J=1,NNO

                X(1,IPG)= X(1,IPG)+ZR(IGEOM + 3*(J-1) -1+1)
     &                *ZR(IVF+LDEC+J-1)
                X(2,IPG)= X(2,IPG)+ZR( IGEOM + 3*(J-1) -1+2)
     &                *ZR(IVF+LDEC+J-1)
                X(3,IPG)= X(3,IPG)+ZR( IGEOM + 3*(J-1) -1+3)
     &                *ZR(IVF+LDEC+J-1)

91            CONTINUE

C CALCUL DU FLUX FLUIDE NORMAL AUX POINTS DE GAUSS

                FLUFN(IPG) = ACC(1,IPG)*NORM(1,IPG)+ACC(2,IPG)*
     &               NORM(2,IPG)+ACC(3,IPG)*NORM(3,IPG)

90        CONTINUE

C STOCKAGE DU FLUX FLUIDE DANS UN VECTEUR INDEXE
C PAR LE MODE ET L'ELEMENT

         IMODE='CHBIDON'
         IELEM ='CHBIDON'
         CALL CODENT(ZI(IHARM),'D0',IMODE)
         CALL TECAEL(IADZI,IAZK24)
         CALL CODENT(ZI(IADZI),'D0',IELEM)
         VETEL = '&&329.M'//IMODE//'.EL'//IELEM
C        ON CONSERVE L'ALLOCATION DYNAMIQUE AU DETRIMENT DE L'ALLOCATION
C        STATIQUE, CAR VETEL EST UTILIE A L'EXTERIEUR DES ROUTINES
C        ELEMENTAIRES
         CALL WKVECT(VETEL,'V V R8',4*NPG1,IVETEL)
         DO 100 IPG=0,NPG1-1
             ZR(IVETEL+4*IPG) = JAC(IPG+1)*ZR(IPOIDS+IPG)*FLUFN(IPG+1)
             ZR(IVETEL+4*IPG+1) = X(1,IPG+1)
             ZR(IVETEL+4*IPG+2) = X(2,IPG+1)
             ZR(IVETEL+4*IPG+3) = X(3,IPG+1)
100       CONTINUE
C
C
      ELSEIF ( NOMTE.EQ.'MEDKQU4') THEN
C              -----------------------
           CALL SHL329

      ENDIF

      END
