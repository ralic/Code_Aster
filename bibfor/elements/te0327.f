      SUBROUTINE TE0327(OPTION,NOMTE)
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
C   CALCUL DES TERMES ELEMENTAIRES D'AMORTISSEMENT AJOUTE
C     OPTION : AMOR_AJOU
C....................................................................
C
      INCLUDE 'jeveux.h'
      CHARACTER*16       NOMTE,OPTION
      REAL*8             SX(9,9),SY(9,9),SZ(9,9),JAC(9)
      REAL*8             NX(9),NY(9),NZ(9),NORM(3,9),ACC(3,9)
      REAL*8             FLUFN(9),ACLOC(3,8),COVA(3,3),CNVA(3,3)
      REAL*8             A(2,2),METR(2,2),E1(3,9),E2(3,9),JC
      INTEGER            IPOIDS,IVF,IDFDX,IDFDY,IGEOM,IMATTT
      INTEGER            NDIM,NNO,IPG,NPG1
      INTEGER            IDEC,JDEC,KDEC,LDEC,NNOS,JGANO
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,IACCE ,IDIM ,IDIR ,II ,IJ ,INO 
      INTEGER J ,JDIR ,JJ ,JNO ,K 
C-----------------------------------------------------------------------
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDX,JGANO)
      IDFDY  = IDFDX  + 1
C
      CALL JEVECH('PACCELR','L',IACCE)
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATTTR','E',IMATTT)
C
      DO 1200 I=1,NNO
         ACLOC(1,I)=0.0D0
         ACLOC(2,I)=0.0D0
         ACLOC(3,I)=0.0D0
1200  CONTINUE
      K=0
      DO 1201 I=1,NNO
         DO 20 IDIM=1,3
            K=K+1
            ACLOC(IDIM,I) = ZR(IACCE+K-1)
20       CONTINUE
1201  CONTINUE

      DO 1052 IPG=1,NPG1
         ACC(1,IPG)=0.0D0
         ACC(2,IPG)=0.0D0
         ACC(3,IPG)=0.0D0
1052  CONTINUE

      DO 1051 IPG=1,NPG1
         LDEC=(IPG-1)*NNO

         DO 105 I=1,NNO
            ACC(1,IPG) = ACC(1,IPG) + ACLOC(1,I)*ZR(IVF+LDEC+I-1)
            ACC(2,IPG) = ACC(2,IPG) + ACLOC(2,I)*ZR(IVF+LDEC+I-1)
            ACC(3,IPG) = ACC(3,IPG) + ACLOC(3,I)*ZR(IVF+LDEC+I-1)
 105     CONTINUE
 1051 CONTINUE
C
C --- CALCUL DES PRODUITS VECTORIELS OMI X OMJ
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
C --- BOUCLE SUR LES POINTS DE GAUSS
C
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
     &                           * ZR(IDFDY+KDEC+JDEC) * SX(I,J)
               NY(IPG) = NY(IPG) + ZR(IDFDX+KDEC+IDEC)
     &                           * ZR(IDFDY+KDEC+JDEC) * SY(I,J)
               NZ(IPG) = NZ(IPG) + ZR(IDFDX+KDEC+IDEC)
     &                           * ZR(IDFDY+KDEC+JDEC) * SZ(I,J)

104         CONTINUE
102      CONTINUE
C
C ------ CALCUL DU JACOBIEN AU POINT DE GAUSS IPG
C
         JAC(IPG) = SQRT (NX(IPG)*NX(IPG) + NY(IPG)*NY(IPG)
     &                                    + NZ(IPG)*NZ(IPG))

C ------ CALCUL DE LA NORMALE UNITAIRE

         NORM(1,IPG) = NX(IPG)/JAC(IPG)
         NORM(2,IPG) = NY(IPG)/JAC(IPG)
         NORM(3,IPG) = NZ(IPG)/JAC(IPG)
101   CONTINUE

C --- CALCUL DES VECTEURS E1, E2 TANGENTS A L'ELEMENT NON NORMALISES

      DO 90 IPG=1,NPG1
         KDEC=(IPG-1)*NNO*NDIM
         E1(1,IPG)=0.0D0
         E1(2,IPG)=0.0D0
         E1(3,IPG)=0.0D0

         E2(1,IPG)=0.0D0
         E2(2,IPG)=0.0D0
         E2(3,IPG)=0.0D0

         DO 91 J=1,NNO
            IDEC=(J-1)*NDIM

            E1(1,IPG)= E1(1,IPG)+ZR(IGEOM + 3*(J-1) -1+1)
     &                          *ZR(IDFDX+KDEC+IDEC)
            E1(2,IPG)= E1(2,IPG)+ZR( IGEOM + 3*(J-1) -1+2)
     &                          *ZR(IDFDX+KDEC+IDEC)
            E1(3,IPG)= E1(3,IPG)+ZR( IGEOM + 3*(J-1) -1+3)
     &                          *ZR(IDFDX+KDEC+IDEC)

            E2(1,IPG)= E2(1,IPG)+ZR( IGEOM + 3*(J-1) -1+1)
     &                          *ZR(IDFDY+KDEC+IDEC)
            E2(2,IPG)= E2(2,IPG)+ZR( IGEOM + 3*(J-1) -1+2)
     &                          *ZR(IDFDY+KDEC+IDEC)
            E2(3,IPG)= E2(3,IPG)+ZR( IGEOM + 3*(J-1) -1+3)
     &                          *ZR(IDFDY+KDEC+IDEC)
91       CONTINUE

C ------ CONSTITUTION DE LA BASE COVARIANTE

         DO 92 I=1,3
            COVA(I,1)=E1(I,IPG)
            COVA(I,2)=E2(I,IPG)
92       CONTINUE

C ------ ON CALCULE LE TENSEUR METRIQUE

         CALL SUMETR(COVA,METR,JC)

C ------ CALCUL DE LA BASE CONTRAVARIANTE

         CALL SUBACV(COVA,METR,JC,CNVA,A)

C ------ CALCUL DU FLUX FLUIDE NORMAL AUX POINTS DE GAUSS

         FLUFN(IPG) = ACC(1,IPG)*NORM(1,IPG)+ACC(2,IPG)*NORM(2,IPG)
     &                                   +ACC(3,IPG)*NORM(3,IPG)

C CALCUL DE LA MATRICE DES PRODUITS SCALAIRES DES GRADIENTS SURFACIQUES
C DES FONCTIONS DE FORMES
         DO 93 I=1,NNO
            IDEC = (I-1)*NDIM
            DO 94 J=1,I
               JDEC = (J-1)*NDIM
               IJ = (I-1)*I/2 +J
               DO 95 II=1,2
                  IDIR=II-1
                  DO 96 JJ=1,2
                     JDIR=JJ-1

                     ZR(IMATTT+IJ-1) = ZR(IMATTT+IJ-1) +
     &                            JAC(IPG)*ZR(IPOIDS+IPG-1)*FLUFN(IPG)
     &                            *ZR(IDFDX+IDIR+KDEC+IDEC)
     &                            *ZR(IDFDX+JDIR+KDEC+JDEC)
     &                            *A(II,JJ)
96                CONTINUE
95             CONTINUE
94          CONTINUE
93       CONTINUE
90    CONTINUE
C
      END
