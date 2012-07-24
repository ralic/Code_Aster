      SUBROUTINE GKMET4(NNOFF,NDIMTE,CHFOND,PAIR,IADRGK,MILIEU,CONNEX,
     &                   IADGKS,IADGKI,ABSCUR,NUM)
      IMPLICIT NONE

      INCLUDE 'jeveux.h'
      INTEGER         NNOFF,NDIMTE,IADRGK,IADGKS,IADGKI,NUM
      CHARACTER*24    CHFOND,ABSCUR
      LOGICAL         PAIR,MILIEU,CONNEX


C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 24/07/2012   AUTEUR PELLET J.PELLET 
C TOLE CRS_1404
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================

C ......................................................................
C      METHODE LAGRANGE_REGU POUR LE CALCUL DE G(S)
C      K1(S) K2(S) ET K3(S)
C
C ENTREE
C
C     NNOFF    --> NOMBRE DE NOEUDS DU FOND DE FISSURE
C     NDIMTE   --> NOMBRE de CHAMPS THETA CHOISIS
C     CHFOND   --> COORDONNEES ET ABSCISSES CURVILIGNES DES NOEUDS
C                  DU FOND DE FISSURE
C     IADRGK   --> ADRESSE DE VALEURS DE GKTHI
C                 (G, K1, K2, K3 POUR LES CHAMPS THETAI)
C     MILIEU   --> .TRUE.  : ELEMENT QUADRATIQUE
C                  .FALSE. : ELEMENT LINEAIRE
C     CONNEX   --> .TRUE.  : SI FOND FERME
C                  .FALSE. : SI FOND OUVERT
C
C  SORTIE
C
C   IADGKS     --> ADRESSE DE VALEURS DE GKS
C                   (VALEUR DE G(S), K1(S), K2(S), K3(S))
C   IADGKI     --> ADRESSE DE VALEURS DE GKTHI
C                  (G, K1, K2, K3 POUR LES CHAMPS THETAI)
C   ABSCUR     --> VALEURS DES ABSCISSES CURVILIGNES S
C      NUM     --> 5 (LAGRANGE_REGU)
C
      INTEGER      IFON,IADABS,IMATR
      INTEGER      I,I1,KK,NUMP,NN,J
      REAL*8       S1,S2,DELTA,S3
      REAL*8       GTHI(NNOFF),K1TH(NNOFF),K2TH(NNOFF),K3TH(NNOFF)
      REAL*8       GS(NDIMTE),K1S(NDIMTE),K2S(NDIMTE),K3S(NDIMTE)
      REAL*8       BETAS(NNOFF),GIS(NNOFF)
      REAL*8       G1TH(NNOFF),G2TH(NNOFF),G3TH(NNOFF)
      REAL*8       G1S(NNOFF),G2S(NNOFF),G3S(NNOFF)
      CHARACTER*24 MATR

C
C

      CALL JEMARQ()


      CALL JEVEUO(CHFOND,'L',IFON)
      CALL JEVEUO(ABSCUR,'E',IADABS)
      DO 10 I=1,NNOFF
        ZR(IADABS-1+(I-1)+1)=ZR(IFON-1+4*(I-1)+4)
10    CONTINUE
      DO 11 I=1,NDIMTE
        ZR(IADABS-1+(I-1)+1)=ZR(IFON-1+4*(I-1)+4)
        GTHI(I)=ZR(IADRGK-1+(I-1)*8+1)
        K1TH(I)=ZR(IADRGK-1+(I-1)*8+5)
        K2TH(I)=ZR(IADRGK-1+(I-1)*8+6)
        K3TH(I)=ZR(IADRGK-1+(I-1)*8+7)
        G1TH(I)=ZR(IADRGK-1+(I-1)*8+2)
        G2TH(I)=ZR(IADRGK-1+(I-1)*8+3)
        G3TH(I)=ZR(IADRGK-1+(I-1)*8+4)
11    CONTINUE

      NUM = 5
C
      MATR = '&&METHO3.MATRI'

      CALL WKVECT(MATR,'V V R8',NDIMTE*NDIMTE,IMATR)

      I1 = 2
      IF (MILIEU) I1 = 4

      DO 40 I = 1,NDIMTE-2
        NUMP = 2*I-1
        IF (MILIEU) NUMP = 4*I-3
        S1 = ZR(IADABS+NUMP-1)
        S2 = ZR(IADABS+NUMP-1+I1)
        DELTA = (S2-S1)/6.D0
C
        KK = IMATR+(I-1  )*NDIMTE+I-1
        ZR(KK )= ZR(KK) +               2.D0*DELTA
        ZR(IMATR+(I-1+1)*NDIMTE+I-1  )=  1.D0*DELTA
C
        ZR(IMATR+(I-1  )*NDIMTE+I-1+1)=  1.D0*DELTA
        ZR(IMATR+(I-1+1)*NDIMTE+I-1+1)=  2.D0*DELTA

 40   CONTINUE
      I = NDIMTE -1
      NUMP = 2*(I-1)
      IF (PAIR) THEN
        S1 = ZR(IADABS+NUMP-1)
        S2 = ZR(IADABS+NUMP-1+I1/2)
        DELTA = (S2-S1)/6.D0
        KK = IMATR+(I-1  )*NDIMTE+I-1
        ZR(KK )= ZR(KK) +         3.5D0*DELTA
        ZR(IMATR+(I-1+1)*NDIMTE+I-1  )=  1.D0*DELTA
        ZR(IMATR+(I-1  )*NDIMTE+I-1+1)=  1.D0*DELTA
        ZR(IMATR+(I-1+1)*NDIMTE+I-1+1)= 0.5D0*DELTA
      ELSE
        S1 = ZR(IADABS+NUMP)
        S2 = ZR(IADABS+NUMP+I1)
        DELTA = (S2-S1)/6.D0
        KK = IMATR+(I-1  )*NDIMTE+I-1
        ZR(KK )= ZR(KK) +               2.D0*DELTA
        ZR(IMATR+(I-1+1)*NDIMTE+I-1  )=  1.D0*DELTA
        ZR(IMATR+(I-1  )*NDIMTE+I-1+1)=  1.D0*DELTA
        ZR(IMATR+(I-1+1)*NDIMTE+I-1+1)=  2.D0*DELTA
      ENDIF

      IF (NNOFF .EQ. 2) THEN
        S1 = ZR(IADABS+1-1)
        S2 = ZR(IADABS+1-1+1)
        DELTA = (S2-S1)/6.D0
        ZR(IMATR + 0)=  3.5D0*DELTA
        ZR(IMATR + 1)=  1.D0*DELTA
        ZR(IMATR + 2)=  1.D0*DELTA
        ZR(IMATR + 3)= 0.5D0*DELTA
      ENDIF

      IF (CONNEX) THEN
        ZR(IMATR) = 2.D0*ZR(IMATR)
        S1 = ZR(IADABS-1+NUMP-I1+1)
        S2 = ZR(IADABS-1+NUMP+1)
        DELTA = (S2-S1)/6.D0
        ZR(IMATR+(1-1)*NDIMTE+NDIMTE-1-1)= 1.D0*DELTA
        KK = IMATR+(NDIMTE-1)*NDIMTE+NDIMTE-1
        ZR(KK) = 2.D0*ZR(KK)
        S1 = ZR(IADABS-1+1)
        S2 = ZR(IADABS-1+I1+1)
        DELTA = (S2-S1)/6.D0
        ZR(IMATR+(NDIMTE-1)*NDIMTE+2-1)= 1.D0*DELTA
      ENDIF

C     SYSTEME LINEAIRE:  MATR*GS = GTHI
      CALL GSYSTE(MATR,NDIMTE,NDIMTE,GTHI,GS)

C     SYSTEME LINEAIRE:  MATR*K1S = K1TH
      CALL GSYSTE(MATR,NDIMTE,NDIMTE,K1TH,K1S)

C     SYSTEME LINEAIRE:  MATR*K2S = K2TH
      CALL GSYSTE(MATR,NDIMTE,NDIMTE,K2TH,K2S)

C     SYSTEME LINEAIRE:  MATR*K3S = K3TH
      CALL GSYSTE(MATR,NDIMTE,NDIMTE,K3TH,K3S)

C       SYSTEMES LINEAIRES POUR GIRWIN
      CALL GSYSTE(MATR,NDIMTE,NDIMTE,G1TH,G1S)
      CALL GSYSTE(MATR,NDIMTE,NDIMTE,G2TH,G2S)
      CALL GSYSTE(MATR,NDIMTE,NDIMTE,G3TH,G3S)
      DO 50 I=1,NDIMTE
        GIS(I)=G1S(I)*G1S(I) + G2S(I)*G2S(I) +G3S(I)*G3S(I)
 50   CONTINUE

C     CALCUL DES ANGLES DE PROPAGATION DE FISSURE LOCAUX BETA
      DO 80 I=1,NDIMTE
        BETAS(I) = 0.0D0
        IF (K2S(I).NE.0.D0) BETAS(I) = 2.0D0*ATAN2(0.25D0*(K1S(I)/K2S(I)
     &    -SIGN(1.0D0,K2S(I))*SQRT((K1S(I)/K2S(I))**2.0D0+8.0D0)),1.0D0)
 80   CONTINUE

      IF (NNOFF .EQ. 2) THEN
        ZR(IADGKS-1+1)=GS(1)
        ZR(IADGKS-1+2)=K1S(1)
        ZR(IADGKS-1+3)=K2S(1)
        ZR(IADGKS-1+4)=K3S(1)
        ZR(IADGKS-1+5)=GIS(1)
        ZR(IADGKS-1+6)=BETAS(1)
        ZR(IADGKS-1+(NNOFF-1)*6+1)=GS(NDIMTE)
        ZR(IADGKS-1+(NNOFF-1)*6+2)=K1S(NDIMTE)
        ZR(IADGKS-1+(NNOFF-1)*6+3)=K2S(NDIMTE)
        ZR(IADGKS-1+(NNOFF-1)*6+4)=K3S(NDIMTE)
        ZR(IADGKS-1+(NNOFF-1)*6+5)=GIS(NDIMTE)
        ZR(IADGKS-1+(NNOFF-1)*6+6)=BETAS(NDIMTE)

      ELSE
       DO 60 I=1,NDIMTE-1
         IF (MILIEU) THEN
           NN = 4*I-3
           ZR(IADGKS-1+(NN-1)*6+1)=GS(I)
           ZR(IADGKS-1+(NN-1)*6+2)=K1S(I)
           ZR(IADGKS-1+(NN-1)*6+3)=K2S(I)
           ZR(IADGKS-1+(NN-1)*6+4)=K3S(I)
           ZR(IADGKS-1+(NN-1)*6+5)=GIS(I)
           ZR(IADGKS-1+(NN-1)*6+6)=BETAS(I)
           S1 = ZR(IADABS+NN-1)
           S3 = ZR(IADABS+NN+4-1)

           ZR(IADGKS-1+(NN-1+1)*6+1)=GS(I)+(ZR(IADABS+NN+1-1)-S1)*
     &                         (GS(I+1)-GS(I))/(S3-S1)
           ZR(IADGKS-1+(NN-1+2)*6+1)=GS(I)+(ZR(IADABS+NN+2-1)-S1)*
     &                         (GS(I+1)-GS(I))/(S3-S1)
           ZR(IADGKS-1+(NN-1+3)*6+1)=GS(I)+(ZR(IADABS+NN+3-1)-S1)*
     &                         (GS(I+1)-GS(I))/(S3-S1)

           ZR(IADGKS-1+(NN-1+1)*6+2)=K1S(I)+(ZR(IADABS+NN+1-1)-S1)*
     &                        (K1S(I+1)-K1S(I))/(S3-S1)
           ZR(IADGKS-1+(NN-1+2)*6+2)=K1S(I)+(ZR(IADABS+NN+2-1)-S1)*
     &                        (K1S(I+1)-K1S(I))/(S3-S1)
           ZR(IADGKS-1+(NN-1+3)*6+2)=K1S(I)+(ZR(IADABS+NN+3-1)-S1)*
     &                        (K1S(I+1)-K1S(I))/(S3-S1)


           ZR(IADGKS-1+(NN-1+1)*6+3)=K2S(I)+(ZR(IADABS+NN+1-1)-S1)*
     &                        (K2S(I+1)-K2S(I))/(S3-S1)
           ZR(IADGKS-1+(NN-1+2)*6+3)=K2S(I)+(ZR(IADABS+NN+2-1)-S1)*
     &                        (K2S(I+1)-K2S(I))/(S3-S1)
           ZR(IADGKS-1+(NN-1+3)*6+3)=K2S(I)+(ZR(IADABS+NN+3-1)-S1)*
     &                        (K2S(I+1)-K2S(I))/(S3-S1)


           ZR(IADGKS-1+(NN-1+1)*6+4)=K3S(I)+(ZR(IADABS+NN+1-1)-S1)*
     &                      (K3S(I+1)-K3S(I))/(S3-S1)
           ZR(IADGKS-1+(NN-1+2)*6+4)=K3S(I)+(ZR(IADABS+NN+2-1)-S1)*
     &                      (K3S(I+1)-K3S(I))/(S3-S1)
           ZR(IADGKS-1+(NN-1+3)*6+4)=K3S(I)+(ZR(IADABS+NN+3-1)-S1)*
     &                      (K3S(I+1)-K3S(I))/(S3-S1)


           ZR(IADGKS-1+(NN-1+1)*6+5)=GIS(I)+(ZR(IADABS+NN+1-1)-S1)*
     &                      (GIS(I+1)-GIS(I))/(S3-S1)
           ZR(IADGKS-1+(NN-1+2)*6+5)=GIS(I)+(ZR(IADABS+NN+2-1)-S1)*
     &                      (GIS(I+1)-GIS(I))/(S3-S1)
           ZR(IADGKS-1+(NN-1+3)*6+5)=GIS(I)+(ZR(IADABS+NN+3-1)-S1)*
     &                      (GIS(I+1)-GIS(I))/(S3-S1)


           ZR(IADGKS-1+(NN-1+1)*6+6)=BETAS(I)+(ZR(IADABS+NN+1-1)-S1)*
     &                      (BETAS(I+1)-BETAS(I))/(S3-S1)           
           ZR(IADGKS-1+(NN-1+2)*6+6)=BETAS(I)+(ZR(IADABS+NN+2-1)-S1)*
     &                      (BETAS(I+1)-BETAS(I))/(S3-S1)
           ZR(IADGKS-1+(NN-1+3)*6+6)=BETAS(I)+(ZR(IADABS+NN+3-1)-S1)*
     &                      (BETAS(I+1)-BETAS(I))/(S3-S1)
         ELSE
           NN = 2*I-1
           ZR(IADGKS-1+(NN-1)*6+1)=GS(I)
           ZR(IADGKS-1+(NN-1)*6+2)=K1S(I)
           ZR(IADGKS-1+(NN-1)*6+3)=K2S(I)
           ZR(IADGKS-1+(NN-1)*6+4)=K3S(I)
           ZR(IADGKS-1+(NN-1)*6+5)=GIS(I)
           ZR(IADGKS-1+(NN-1)*6+6)=BETAS(I)
           S1 = ZR(IADABS+NN-1)
           S2 = ZR(IADABS+NN-1+1)
           S3 = ZR(IADABS+NN-1+2)

           ZR(IADGKS-1+(NN-1+1)*6+1)=GS(I)+(S2-S1)*
     &                         (GS(I+1)-GS(I))/(S3-S1)
           ZR(IADGKS-1+(NN-1+1)*6+2)=K1S(I)+(S2-S1)*
     &                        (K1S(I+1)-K1S(I))/(S3-S1)
           ZR(IADGKS-1+(NN-1+1)*6+3)=K2S(I)+(S2-S1)*
     &                        (K2S(I+1)-K2S(I))/(S3-S1)
           ZR(IADGKS-1+(NN-1+1)*6+4)=K3S(I)+(S2-S1)*
     &                      (K3S(I+1)-K3S(I))/(S3-S1)
           ZR(IADGKS-1+(NN-1+1)*6+5)=GIS(I)+(S2-S1)*
     &                      (GIS(I+1)-GIS(I))/(S3-S1)
           ZR(IADGKS-1+(NN-1+1)*6+6)=BETAS(I)+(S2-S1)*
     &                      (BETAS(I+1)-BETAS(I))/(S3-S1)
         ENDIF
60     CONTINUE

       IF(PAIR)THEN
        NN=2*(NDIMTE-2)
        S1 = ZR(IADABS+NN-1)
        S2 = ZR(IADABS+NN-1+1)
        S3 = ZR(IADABS+NN-1+2)
        ZR(IADGKS-1+(NNOFF-1)*6+1)=ZR(IADGKS-1+(NNOFF-2)*6+1)+ (S3-S2)*
     &   (ZR(IADGKS-1+(NNOFF-3)*6+1)-ZR(IADGKS-1+(NNOFF-2)*6+1))/(S1-S2)
        ZR(IADGKS-1+(NNOFF-1)*6+2)=ZR(IADGKS-1+(NNOFF-2)*6+2)+ (S3-S2)*
     &   (ZR(IADGKS-1+(NNOFF-3)*6+2)-ZR(IADGKS-1+(NNOFF-2)*6+2))/(S1-S2)
        ZR(IADGKS-1+(NNOFF-1)*6+3)=ZR(IADGKS-1+(NNOFF-2)*6+3)+ (S3-S2)*
     &   (ZR(IADGKS-1+(NNOFF-3)*6+3)-ZR(IADGKS-1+(NNOFF-2)*6+3))/(S1-S2)
        ZR(IADGKS-1+(NNOFF-1)*6+4)=ZR(IADGKS-1+(NNOFF-2)*6+4)+ (S3-S2)*
     &   (ZR(IADGKS-1+(NNOFF-3)*6+4)-ZR(IADGKS-1+(NNOFF-2)*6+4))/(S1-S2)
        ZR(IADGKS-1+(NNOFF-1)*6+5)=ZR(IADGKS-1+(NNOFF-2)*6+5)+ (S3-S2)*
     &   (ZR(IADGKS-1+(NNOFF-3)*6+5)-ZR(IADGKS-1+(NNOFF-2)*6+5))/(S1-S2)
        ZR(IADGKS-1+(NNOFF-1)*6+6)=ZR(IADGKS-1+(NNOFF-2)*6+6)+ (S3-S2)*
     &   (ZR(IADGKS-1+(NNOFF-3)*6+6)-ZR(IADGKS-1+(NNOFF-2)*6+6))/(S1-S2)
       ELSE
        ZR(IADGKS-1+(NNOFF-1)*6+1)=GS(NDIMTE)
        ZR(IADGKS-1+(NNOFF-1)*6+2)=K1S(NDIMTE)
        ZR(IADGKS-1+(NNOFF-1)*6+3)=K2S(NDIMTE)
        ZR(IADGKS-1+(NNOFF-1)*6+4)=K3S(NDIMTE)
        ZR(IADGKS-1+(NNOFF-1)*6+5)=GIS(NDIMTE)
        ZR(IADGKS-1+(NNOFF-1)*6+6)=BETAS(NDIMTE)
       ENDIF
      ENDIF

      DO 70 I=1,NDIMTE
        DO 71 J=1,5
          ZR(IADGKI-1+5*(I-1)+J)=ZR(IADRGK-1+8*(I-1)+J)
 71     CONTINUE
 70   CONTINUE



      CALL JEDETR('&&METHO3.MATRI')
      CALL JEDETR('&&METHO3.VECT')

      CALL JEDEMA()
      END
