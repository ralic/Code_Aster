      SUBROUTINE MLTFMJ(N,P,FRONT,FRN,ADPER,TRAV,AD)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 07/01/2002   AUTEUR JFBHHUC C.ROSE 
C RESPONSABLE JFBHHUC C.ROSE
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER N,P,ADPER(*),AD(*)
      REAL*8 FRONT(*),FRN(*)
      INTEGER NBB,DECAL,ADD,IND,NMB,I,J,L,KB,IA,IB,RESTM
      PARAMETER (NBB=32)
      CHARACTER*1 TRANSA, TRANSB
      INTEGER I1,J1,K,M,IT,NB,NUMPRC,MLNUMP
      REAL*8 S,TRAV(P,NBB,*)
      REAL*8  C(NBB, NBB)
      M=N-P
      NMB=M/NBB
      RESTM = M -(NBB*NMB)
      DECAL = ADPER(P+1) - 1
      NB=NBB
C
C$OMP PARALLEL DO DEFAULT(PRIVATE)
C$OMP+SHARED(N,M,P,NMB,NB,RESTM,FRONT,ADPER,DECAL,FRN,TRAV)
C$OMP+SCHEDULE(STATIC,1)
      DO 1000 KB = 1,NMB
      NUMPRC=MLNUMP()
C     K : INDICE DE COLONNE DANS LA MATRICE FRONTALE (ABSOLU DE 1 A N)
         K = NB*(KB-1) + 1 +P
         DO 100 I=1,P
            S = FRONT(ADPER(I))
            ADD= N*(I-1) + K
            DO 50 J=1,NB
               TRAV(I,J,NUMPRC) = FRONT(ADD)*S
               ADD = ADD + 1
 50         CONTINUE
 100     CONTINUE
C     BLOC DIAGONAL

C     SOUS LE BLOC DIAGONAL
C     2EME ESSAI : DES PRODUITS DE LONGUEUR NB
C
         DO 500 IB = KB,NMB
            IA = K + NB*(IB-KB)
            IT=1
            CALL DGEMX( NB,NB,P,FRONT(IA),N, TRAV(IT,1,NUMPRC), P,
     %                   C, NB)
C     RECOPIE

C
            DO 501 I=1,NB
               I1=I-1
C     IND = ADPER(K +I1) - DECAL  + NB*(IB-KB-1) +NB - I1
               IF(IB.EQ.KB) THEN
                  J1= I
                  IND = ADPER(K + I1) - DECAL
               ELSE
                  J1=1
                  IND = ADPER(K + I1) - DECAL + NB*(IB-KB)  - I1
               ENDIF
               DO 502J=J1,NB
                  FRN(IND) = FRN(IND) +C(J,I)
                  IND = IND +1
 502           CONTINUE
 501        CONTINUE
 500     CONTINUE
         IF(RESTM.GT.0) THEN
            IB = NMB + 1
            IA = K + NB*(IB-KB)
            IT=1
            CALL DGEMX( RESTM,NB,P,FRONT(IA),N, TRAV(IT,1,NUMPRC), P,
     %                   C, NB)

C     RECOPIE

C
            DO 801 I=1,NB
               I1=I-1
C     IND = ADPER(K +I1) - DECAL  + NB*(IB-KB-1) +NB - I1
               J1=1
               IND = ADPER(K + I1) - DECAL + NB*(IB-KB)  - I1
               DO 802 J=J1,RESTM
                  FRN(IND) = FRN(IND) +C(J,I)
                  IND = IND +1
 802              CONTINUE
 801           CONTINUE
         ENDIF
 1000 CONTINUE
C$OMP END PARALLEL DO
      IF(RESTM.GT.0 ) THEN
         KB = 1+NMB
C     K : INDICE DE COLONNE DANS LA MATRICE FRONTALE (ABSOLU DE 1 A N)
         K = NB*(KB-1) + 1 +P
         DO 101 I=1,P
            S = FRONT(ADPER(I))
            ADD= N*(I-1) + K
            DO 51 J=1,RESTM
              TRAV(I,J,1) = FRONT(ADD)*S
               ADD = ADD + 1
 51         CONTINUE
 101     CONTINUE
C     BLOC DIAGONAL

         IB = KB
         IA = K + NB*(IB-KB)
         IT=1
           CALL DGEMX( RESTM,RESTM,P,FRONT(IA),N, TRAV(IT,1,1),P,
     %                   C, NB)
C     RECOPIE

C
         DO 902 I=1,RESTM
            I1=I-1
C     IND = ADPER(K +I1) - DECAL  + NB*(IB-KB-1) +NB - I1
            J1= I
            IND = ADPER(K + I1) - DECAL
            DO 901 J=J1,RESTM
               FRN(IND) = FRN(IND) +C(J,I)
               IND = IND +1
 901        CONTINUE
 902     CONTINUE

      ENDIF
      END
