      SUBROUTINE BOITEQ(CNOEUD,NOEPAN,NPAN,MINMAX,PAN)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 02/04/2002   AUTEUR RATEAU G.RATEAU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C ----------------------------------------------------------------------
C     CORRECTION DE BOITEL PRENANT EN COMPTE LES FACES QUADRATIQUES
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE 
C REAL*8   CNOEUD(3,*)    : COORD. NOEUDS DE LA MAILLE (CF CONOEU)
C INTEGER  NOEPAN(*)      : NOEUDS DEFINISSANT LES PANS (CF NOPAN)
C INTEGER  NPAN           : NOMBRE DE PANS DE LA MAILLE
C
C VARIABLES D'ENTREE/SORTIE
C REAL*8   MINMAX(2,3)    : BOITE ENGLOBANT LA MAILLE SUIVANT X,Y,Z
C REAL*8   PAN(5,*)       : EQUATION DES PANS DU CONVEXE ENGLOBANT
C                           ET INSCRIT DE LA MAILLE (CF BOITE)
C ---------------------------------------------------------------------

      IMPLICIT NONE

C --- PARAMETRES
      INTEGER N
      PARAMETER (N = 8)
      REAL*8  HT,HQ,HT2,HT3,HQ2,HQ3,HQ4,R8MAX
      PARAMETER (HT = 1.D0/N)
      PARAMETER (HQ = 2.D0/N)
      PARAMETER (HT2 = (HT**2)/8.D0)
      PARAMETER (HT3 = (HT**3)/4.D0)
      PARAMETER (HQ2 = (HQ**2)/8.D0)
      PARAMETER (HQ3 = (HQ**3)/8.D0)
      PARAMETER (HQ4 = 3.D0*(HQ**4)/64.D0)
      PARAMETER (R8MAX = 1.D92) 

C --- VARIABLES
      INTEGER NOEPAN(*),NPAN,NNO,P0,I,J,I1,J1,NC
      REAL*8 CNOEUD(3,*),MINMAX(2,*),PAN(5,*)
      REAL*8 XMIN(2,3),XMAX(2,3),MM(2,3),XRMIN(2,6),XRMAX(2),R(6),RM,S
      REAL*8 M0(2),M(3),W0(9),W20(3,9),W2(3),W30(2,9),W3(2),W40(9),W4
      CHARACTER*8 TYPEMA

      P0 = 1

      DO 10 I = 1, NPAN

C ----- INITIALISATION

        DO 20 J = 1, 3
          MM(1,J) = R8MAX
          MM(2,J) = -R8MAX
 20     CONTINUE

        DO 30 J = 1, NPAN
          R(J) = R8MAX
 30     CONTINUE

        RM = -R8MAX

        NNO = NOEPAN(P0) 
        P0 = P0 + 1
    
C ----- FACE TRIANGULAIRE

        IF ((NNO.EQ.-6).OR.(NNO.EQ.-7)) THEN

          IF (NNO.EQ.-6) THEN
            TYPEMA = 'TRIA6'
          ELSEIF (NNO.EQ.-7) THEN
            TYPEMA = 'TRIA7'
          ENDIF

          M0(2) = -0.5D0*HT
          DO 40 J1 = 1, N

            M0(2) = M0(2) + HT
            M0(1) = -0.5D0*HT

            DO 40 I1 = 1, (N + 1 - J1)

C ----------- ECHANTILLONAGE DU TRIANGLE
 
              M0(1) = M0(1) + HT
              CALL FORME0(M0,TYPEMA,W0,NNO)
              CALL MMPROD(CNOEUD,3,0,3,NOEPAN(P0),NNO,W0,NNO,0,0,1,M)   
                
C ----------- EXTREMA APPROCHES POUR MINMAX 

              DO 50 J = 1, 3

                IF (M(J).LT.MM(1,J)) THEN 
                  MM(1,J) = M(J)
                  XMIN(1,J) = M0(1)
                  XMIN(2,J) = M0(2)
                ENDIF

                IF (M(J).GT.MM(2,J)) THEN 
                  MM(2,J) = M(J)
                  XMAX(1,J) = M0(1)
                  XMAX(2,J) = M0(2)
                ENDIF

 50           CONTINUE

C ----------- EXTREMA APPROCHES POUR PAN

              DO 40 J = 1, NPAN

                S = -(PAN(1,J)*M(1) + PAN(2,J)*M(2) + PAN(3,J)*M(3))

                IF (S.LT.R(J)) THEN
                  R(J) = S
                  XRMIN(1,J) = M0(1)
                  XRMIN(2,J) = M0(2)
                ENDIF

                IF ((J.EQ.I).AND.(S.GT.RM)) THEN
                  RM = S
                  XRMAX(1) = M0(1)
                  XRMAX(2) = M0(2)
                ENDIF

 40       CONTINUE

C ------- TRIA6

          IF (NNO.EQ.6) THEN

C --------- MAJORATION EXTREMA POUR MINMAX
 
            DO 60 J = 1, 3

C ----------- MIN

              CALL FORME2(XMIN(1,J),TYPEMA,W20,NNO,NC)
              CALL MTPROD(W20,NC,0,NC,0,NNO,CNOEUD,3,J,1,NOEPAN(P0),W2)
              S = MM(1,J) - HT2*(2.D0*ABS(W2(3))+ABS(W2(1))+ABS(W2(2)))
              IF (S.LT.MINMAX(1,J)) MINMAX(1,J) = S

C ----------- MAX

              CALL FORME2(XMAX(1,J),TYPEMA,W20,NNO,NC)
              CALL MTPROD(W20,NC,0,NC,0,NNO,CNOEUD,3,J,1,NOEPAN(P0),W2)
              S = MM(2,J) + HT2*(2.D0*ABS(W2(3))+ABS(W2(1))+ABS(W2(2)))
              IF (S.GT.MINMAX(2,J)) MINMAX(2,J) = S

 60         CONTINUE

C --------- MAJORATION EXTREMA POUR PAN

            DO 70 J = 1, NPAN
            CALL MMPROD(PAN(1,J),1,0,1,0,3,CNOEUD,3,0,NOEPAN(P0),NNO,W0)
              CALL FORME2(XRMIN(1,J),TYPEMA,W20,NNO,NC)
              CALL MMPROD(W20,NC,0,NC,0,NNO,W0,NNO,0,0,1,W2)
              S = R(J) - HT2*(2.D0*ABS(W2(3))+ABS(W2(1))+ABS(W2(2)))
              IF (S.LT.PAN(4,J)) PAN(4,J) = S
              IF (J.EQ.I) THEN
                CALL FORME2(XRMAX,TYPEMA,W20,NNO,NC)
                CALL MMPROD(W20,NC,0,NC,0,NNO,W0,NNO,0,0,1,W2)
                S = RM + HT2*(2.D0*ABS(W2(3))+ABS(W2(1))+ABS(W2(2)))
                IF (S.GT.PAN(5,J)) PAN(5,J) = S
              ENDIF
 70         CONTINUE

C ------- TRIA7

          ELSE

            DO 80 J = 1, 3

C ----------- MIN

              CALL FORME2(XMIN(1,J),TYPEMA,W20,NNO,NC)
              CALL MTPROD(W20,NC,0,NC,0,NNO,CNOEUD,3,J,1,NOEPAN(P0),W2)
              CALL FORME3(XMIN(1,J),TYPEMA,W30,NNO,NC)
              CALL MTPROD(W30,NC,0,NC,0,NNO,CNOEUD,3,J,1,NOEPAN(P0),W3)
              S = MM(1,J) - HT2*(2.D0*ABS(W2(3))+ABS(W2(1))+ABS(W2(2)))
     &                    - HT3*ABS(W3(1))
              IF (S.LT.MINMAX(1,J)) MINMAX(1,J) = S

C ----------- MAX

              CALL FORME2(XMAX(1,J),TYPEMA,W20,NNO,NC)
              CALL MTPROD(W20,NC,0,NC,0,NNO,CNOEUD,3,J,1,NOEPAN(P0),W2)
              CALL FORME3(XMAX(1,J),TYPEMA,W30,NNO,NC)
              CALL MTPROD(W30,NC,0,NC,0,NNO,CNOEUD,3,J,1,NOEPAN(P0),W3)
              S = MM(2,J) + HT2*(2.D0*ABS(W2(3))+ABS(W2(1))+ABS(W2(2)))
     &                    + HT3*ABS(W3(1))
              IF (S.GT.MINMAX(2,J)) MINMAX(2,J) = S

 80         CONTINUE

C --------- MAJORATION EXTREMA POUR PAN

            DO 90 J = 1, NPAN
            CALL MMPROD(PAN(1,J),1,0,1,0,3,CNOEUD,3,0,NOEPAN(P0),NNO,W0)
              CALL FORME2(XRMIN(1,J),TYPEMA,W20,NNO,NC)
              CALL MMPROD(W20,NC,0,NC,0,NNO,W0,NNO,0,0,1,W2)
              CALL FORME3(XRMIN(1,J),TYPEMA,W30,NNO,NC)
              CALL MMPROD(W30,NC,0,NC,0,NNO,W0,NNO,0,0,1,W3)
              S = R(J) - HT2*(2.D0*ABS(W2(3))+ABS(W2(1))+ABS(W2(2)))
     &                 - HT3*ABS(W3(1))
              IF (S.LT.PAN(4,J)) PAN(4,J) = S
              IF (J.EQ.I) THEN
                CALL FORME2(XRMAX,TYPEMA,W20,NNO,NC)
                CALL MMPROD(W20,NC,0,NC,0,NNO,W0,NNO,0,0,1,W2)
                CALL FORME3(XRMIN(1,J),TYPEMA,W30,NNO,NC)
                CALL MMPROD(W30,NC,0,NC,0,NNO,W0,NNO,0,0,1,W3)
                S = RM + HT2*(2.D0*ABS(W2(3))+ABS(W2(1))+ABS(W2(2)))
     &                 + HT3*ABS(W3(1))
                IF (S.GT.PAN(5,J)) PAN(5,J) = S
              ENDIF
 90         CONTINUE

          ENDIF

C ----- FACE QUADRILATAIRE

        ELSEIF ((NNO.EQ.6).OR.(NNO.EQ.8).OR.(NNO.EQ.9)) THEN

          IF (NNO.EQ.6) THEN
            TYPEMA = 'QUAD6'
          ELSEIF (NNO.EQ.8) THEN
            TYPEMA = 'QUAD8'
          ELSEIF (NNO.EQ.9) THEN
            TYPEMA = 'QUAD9'
          ENDIF

          M0(2) = -1.D0 - 0.5D0*HQ
          DO 100 J1 = 1, N

            M0(2) = M0(2) + HQ
            M0(1) = -1.D0 - 0.5D0*HQ

            DO 100 I1 = 1, N

C ----------- ECHANTILLONAGE DU QUADRANGLE
 
              M0(1) = M0(1) + HQ
              CALL FORME0(M0,TYPEMA,W0,NNO)
              CALL MMPROD(CNOEUD,3,0,3,NOEPAN(P0),NNO,W0,NNO,0,0,1,M)
                
C ----------- EXTREMA APPROCHES POUR MINMAX 

              DO 110 J = 1, 3

                IF (M(J).LT.MM(1,J)) THEN 
                  MM(1,J) = M(J)
                  XMIN(1,J) = M0(1)
                  XMIN(2,J) = M0(2)
                ENDIF

                IF (M(J).GT.MM(2,J)) THEN 
                  MM(2,J) = M(J)
                  XMAX(1,J) = M0(1)
                  XMAX(2,J) = M0(2)
                ENDIF

 110          CONTINUE

C ----------- EXTREMA APPROCHES POUR PAN

              DO 100 J = 1, NPAN

                S = -(PAN(1,J)*M(1) + PAN(2,J)*M(2) + PAN(3,J)*M(3))

                IF (S.LT.R(J)) THEN
                  R(J) = S
                  XRMIN(1,J) = M0(1)
                  XRMIN(2,J) = M0(2)
                ENDIF

                IF ((I.EQ.J).AND.(S.GT.RM)) THEN
                  RM = S
                  XRMAX(1) = M0(1)
                  XRMAX(2) = M0(2)
                ENDIF

 100      CONTINUE

C ------- QUAD6

          IF (NNO.EQ.6) THEN

C --------- MAJORATION EXTREMA POUR MINMAX
 
            DO 120 J = 1, 3

C ----------- MIN

              CALL FORME2(XMIN(1,J),TYPEMA,W20,NNO,NC)
              CALL MTPROD(W20,NC,0,NC,0,NNO,CNOEUD,3,J,1,NOEPAN(P0),W2)
              CALL FORME3(XMIN(1,J),TYPEMA,W30,NNO,NC)
              CALL MTPROD(W30,NC,0,NC,0,NNO,CNOEUD,3,J,1,NOEPAN(P0),W3)
              S = MM(1,J) - HQ2*(2.D0*ABS(W2(2))+ABS(W2(1)))
     &                    - HQ3*ABS(W3(1))
              IF (S.LT.MINMAX(1,J)) MINMAX(1,J) = S

C ----------- MAX

              CALL FORME2(XMAX(1,J),TYPEMA,W20,NNO,NC)
              CALL MTPROD(W20,NC,0,NC,0,NNO,CNOEUD,3,J,1,NOEPAN(P0),W2)
              CALL FORME3(XMAX(1,J),TYPEMA,W30,NNO,NC)
              CALL MTPROD(W30,NC,0,NC,0,NNO,CNOEUD,3,J,1,NOEPAN(P0),W3)
              S = MM(2,J) + HQ2*(2.D0*ABS(W2(2))+ABS(W2(1)))
     &                    + HQ3*ABS(W3(1))
              IF (S.GT.MINMAX(2,J)) MINMAX(2,J) = S

 120        CONTINUE

C --------- MAJORATION EXTREMA POUR PAN

            DO 130 J = 1, NPAN
            CALL MMPROD(PAN(1,J),1,0,1,0,3,CNOEUD,3,0,NOEPAN(P0),NNO,W0)
              CALL FORME2(XRMIN(1,J),TYPEMA,W20,NNO,NC)
              CALL MMPROD(W20,NC,0,NC,0,NNO,W0,NNO,0,0,1,W2)
              CALL FORME3(XRMIN(1,J),TYPEMA,W30,NNO,NC)
              CALL MMPROD(W30,NC,0,NC,0,NNO,W0,NNO,0,0,1,W3)
              S = R(J) - HQ2*(2.D0*ABS(W2(2))+ABS(W2(1)))
     &                 - HQ3*ABS(W3(1))        
              IF (S.LT.PAN(4,J)) PAN(4,J) = S
              IF (I.EQ.J) THEN
                CALL FORME2(XRMAX,TYPEMA,W20,NNO,NC)
                CALL MMPROD(W20,NC,0,NC,0,NNO,W0,NNO,0,0,1,W2)
                CALL FORME3(XRMAX,TYPEMA,W30,NNO,NC)
                CALL MMPROD(W30,NC,0,NC,0,NNO,W0,NNO,0,0,1,W3)
                S = RM + HQ2*(2.D0*ABS(W2(2))+ABS(W2(1)))
     &                 + HQ3*ABS(W3(1))        
                IF (S.GT.PAN(5,J)) PAN(5,J) = S                
              ENDIF

 130        CONTINUE

C ------- QUAD8

          ELSEIF (NNO.EQ.8) THEN

C --------- MAJORATION EXTREMA POUR MINMAX
 
            DO 140 J = 1, 3

C ----------- MIN

              CALL FORME2(XMIN(1,J),TYPEMA,W20,NNO,NC)
              CALL MTPROD(W20,NC,0,NC,0,NNO,CNOEUD,3,J,1,NOEPAN(P0),W2)
              CALL FORME3(XMIN(1,J),TYPEMA,W30,NNO,NC)
              CALL MTPROD(W30,NC,0,NC,0,NNO,CNOEUD,3,J,1,NOEPAN(P0),W3)
              S = MM(1,J) - HQ2*(2.D0*ABS(W2(3))+ABS(W2(1))+ABS(W2(2)))
     &                    - HQ3*(ABS(W3(1))+ABS(W3(2)))
              IF (S.LT.MINMAX(1,J)) MINMAX(1,J) = S

C ----------- MAX

              CALL FORME2(XMAX(1,J),TYPEMA,W20,NNO,NC)
              CALL MTPROD(W20,NC,0,NC,0,NNO,CNOEUD,3,J,1,NOEPAN(P0),W2)
              CALL FORME3(XMAX(1,J),TYPEMA,W30,NNO,NC)
              CALL MTPROD(W30,NC,0,NC,0,NNO,CNOEUD,3,J,1,NOEPAN(P0),W3)
              S = MM(2,J) + HQ2*(2.D0*ABS(W2(3))+ABS(W2(1))+ABS(W2(2)))
     &                    + HQ3*(ABS(W3(1))+ABS(W3(2)))
              IF (S.GT.MINMAX(2,J)) MINMAX(2,J) = S

 140        CONTINUE

C --------- MAJORATION EXTREMA POUR PAN

            DO 150 J = 1, NPAN
            CALL MMPROD(PAN(1,J),1,0,1,0,3,CNOEUD,3,0,NOEPAN(P0),NNO,W0)
              CALL FORME2(XRMIN(1,J),TYPEMA,W20,NNO,NC)
              CALL MMPROD(W20,NC,0,NC,0,NNO,W0,NNO,0,0,1,W2)
              CALL FORME3(XRMIN(1,J),TYPEMA,W30,NNO,NC)
              CALL MMPROD(W30,NC,0,NC,0,NNO,W0,NNO,0,0,1,W3)
              S = R(J) - HQ2*(2.D0*ABS(W2(3))+ABS(W2(1))+ABS(W2(2)))
     &                 - HQ3*(ABS(W3(1))+ABS(W3(2)))        
              IF (S.LT.PAN(4,J)) PAN(4,J) = S
              IF (I.EQ.J) THEN
                CALL FORME2(XRMAX,TYPEMA,W20,NNO,NC)
                CALL MMPROD(W20,NC,0,NC,0,NNO,W0,NNO,0,0,1,W2)
                CALL FORME3(XRMAX,TYPEMA,W30,NNO,NC)
                CALL MMPROD(W30,NC,0,NC,0,NNO,W0,NNO,0,0,1,W3)
                S = RM + HQ2*(2.D0*ABS(W2(3))+ABS(W2(1))+ABS(W2(2)))
     &                 + HQ3*(ABS(W3(1))+ABS(W3(2)))        
                IF (S.GT.PAN(5,J)) PAN(5,J) = S                
              ENDIF

 150        CONTINUE

C ------- QUAD9

          ELSE

C --------- MAJORATION EXTREMA POUR MINMAX
 
            DO 160 J = 1, 3

C ----------- MIN

              CALL FORME2(XMIN(1,J),TYPEMA,W20,NNO,NC)
              CALL MTPROD(W20,NC,0,NC,0,NNO,CNOEUD,3,J,1,NOEPAN(P0),W2)
              CALL FORME3(XMIN(1,J),TYPEMA,W30,NNO,NC)
              CALL MTPROD(W30,NC,0,NC,0,NNO,CNOEUD,3,J,1,NOEPAN(P0),W3)
              CALL FORME4(XMIN(1,J),TYPEMA,W40,NNO,NC)
              CALL MTPROD(W40,1,0,1,0,NNO,CNOEUD,3,J,1,NOEPAN(P0),W4)
              S = MM(1,J) - HQ2*(2.D0*ABS(W2(3))+ABS(W2(1))+ABS(W2(2)))
     &                    - HQ3*(ABS(W3(1))+ABS(W3(2))) - HQ4*ABS(W4)
              IF (S.LT.MINMAX(1,J)) MINMAX(1,J) = S

C ----------- MAX

              CALL FORME2(XMAX(1,J),TYPEMA,W20,NNO,NC)
              CALL MTPROD(W20,NC,0,NC,0,NNO,CNOEUD,3,J,1,NOEPAN(P0),W2)
              CALL FORME3(XMAX(1,J),TYPEMA,W30,NNO,NC)
              CALL MTPROD(W30,NC,0,NC,0,NNO,CNOEUD,3,J,1,NOEPAN(P0),W3)
              CALL FORME4(XMIN(1,J),TYPEMA,W40,NNO,NC)
              CALL MTPROD(W40,1,0,1,0,NNO,CNOEUD,3,J,1,NOEPAN(P0),W4)
              S = MM(2,J) + HQ2*(2.D0*ABS(W2(3))+ABS(W2(1))+ABS(W2(2)))
     &                    + HQ3*(ABS(W3(1))+ABS(W3(2))) + HQ4*ABS(W4)
              IF (S.GT.MINMAX(2,J)) MINMAX(2,J) = S

 160        CONTINUE

C --------- MAJORATION EXTREMA POUR PAN

            DO 170 J = 1, NPAN
            CALL MMPROD(PAN(1,J),1,0,1,0,3,CNOEUD,3,0,NOEPAN(P0),NNO,W0)
              CALL FORME2(XRMIN(1,J),TYPEMA,W20,NNO,NC)
              CALL MMPROD(W20,NC,0,NC,0,NNO,W0,NNO,0,0,1,W2)
              CALL FORME3(XRMIN(1,J),TYPEMA,W30,NNO,NC)
              CALL MMPROD(W30,NC,0,NC,0,NNO,W0,NNO,0,0,1,W3)
              CALL FORME4(XRMIN(1,J),TYPEMA,W40,NNO,NC)
              CALL MMPROD(W40,1,0,1,0,NNO,W0,NNO,0,0,1,W4)
              S = R(J) - HQ2*(2.D0*ABS(W2(3))+ABS(W2(1))+ABS(W2(2)))
     &                 - HQ3*(ABS(W3(1))+ABS(W3(2))) - HQ4*ABS(W4)
              IF (S.LT.PAN(4,J)) PAN(4,J) = S
              IF (I.EQ.J) THEN
                CALL FORME2(XRMAX,TYPEMA,W20,NNO,NC)
                CALL MMPROD(W20,NC,0,NC,0,NNO,W0,NNO,0,0,1,W2)
                CALL FORME3(XRMAX,TYPEMA,W30,NNO,NC)
                CALL MMPROD(W30,NC,0,NC,0,NNO,W0,NNO,0,0,1,W3)
                CALL FORME4(XRMAX,TYPEMA,W40,NNO,NC)
                CALL MMPROD(W40,1,0,1,0,NNO,W0,NNO,0,0,1,W4)
                S = RM + HQ2*(2.D0*ABS(W2(3))+ABS(W2(1))+ABS(W2(2)))
     &                 + HQ3*(ABS(W3(1))+ABS(W3(2))) + HQ4*ABS(W4)
                IF (S.GT.PAN(5,J)) PAN(5,J) = S
              ENDIF  

 170        CONTINUE

          ENDIF

        ENDIF

        P0 = P0 + NNO

 10   CONTINUE

      END
