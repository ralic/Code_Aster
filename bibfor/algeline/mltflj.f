      SUBROUTINE MLTFLJ(N,LL,M,IT,P,FRONT,FRN,ADPER,TRAV,AD)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 16/12/2002   AUTEUR ROSE C.ROSE 
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
      INTEGER NBB,DECAL,ADD,ADF,IND,NMB,I,J,L,KB,IA,IB,NLB,LL

      PARAMETER (NBB=96)
      CHARACTER*1 TRANSA, TRANSB
      INTEGER   M,  K, LDC,KI,I1,IT,JB,J1,RESTM,RESTL,NBL,NB
      INTEGER NPROC,NUMPRO,MLNUMP,MLNBPR
      REAL*8     S,TRAV(P,NBB,*)
      REAL*8  C(NBB, NBB)
      NB=NBB
      NBL = P-IT+1
       NMB=M/NB
      NLB = LL/NB
      RESTM = M -(NB*NMB)
      RESTL = LL-(NB*NLB)
      DECAL = ADPER(P+1) -1
      NPROC = MLNBPR()
      IF(NMB.GE.NPROC) THEN
C$OMP PARALLEL DO DEFAULT(PRIVATE)
C$OMP+SHARED(N,M,P,NMB,NBL,NLB,NB,RESTM,RESTL)
C$OMP+SHARED(FRONT,ADPER,DECAL,FRN,TRAV,IT)
C$OMP+SCHEDULE(STATIC,1)
      DO 1000 KB = 1,NMB
      NUMPRO=MLNUMP()
C     K : INDICE DE COLONNE DANS LA MATRICE FRONTALE (ABSOLU DE 1 A N)
         K = NB*(KB-1) + 1 +P
         DO 100 I=IT,P
            S = FRONT(ADPER(I))
            ADD= N*(I-1) + K
            DO 50 J=1,NB
               TRAV(I,J,NUMPRO) = FRONT(ADD)*S
               ADD = ADD + 1
 50         CONTINUE
 100     CONTINUE
C     BLOC DIAGONAL

C     SOUS LE BLOC DIAGONAL
C     2EME ESSAI : DES PRODUITS DE LONGUEUR NB
C

         DO 500 IB = KB,NLB
            IA = N*(IT-1)  + K + NB*(IB-KB)
            CALL DGEMX( NB,NB,NBL,FRONT(IA),N, TRAV(IT,1,NUMPRO), P,
     %                   C, NB)
C     RECOPIE

C
            DO 35 I=1,NB
               I1=I-1
C              IND = ADPER(K +I1) - DECAL  + NB*(IB-KB-1) +NB - I1
               IF(IB.EQ.KB) THEN
                  J1= I
                  IND = ADPER(K + I1) - DECAL
               ELSE
                  J1=1
                  IND = ADPER(K + I1) - DECAL + NB*(IB-KB)  - I1
               ENDIF
               DO 34 J=J1,NB
                  FRN(IND) = FRN(IND) +C(J,I)
                  IND = IND +1
 34            CONTINUE
 35         CONTINUE
 500     CONTINUE
         IF(RESTL.GT.0) THEN
            IB = NLB + 1
            IA =   N*(IT-1)  +K + NB*(IB-KB)
            CALL DGEMX( RESTL,NB,NBL,FRONT(IA),N, TRAV(IT,1,NUMPRO), P,
     %                   C, NB)
C           RECOPIE

C
            DO 45 I=1,NB
               I1=I-1
               J1=1
               IND = ADPER(K + I1) - DECAL + NB*(IB-KB)  - I1
               DO 44 J=J1,RESTL
                  FRN(IND) = FRN(IND) +C(J,I)
                  IND = IND +1
 44            CONTINUE
 45         CONTINUE
         ENDIF
 1000 CONTINUE
      ELSE
      DO 2000 KB = 1,NMB
C     K : INDICE DE COLONNE DANS LA MATRICE FRONTALE (ABSOLU DE 1 A N)
         K = NB*(KB-1) + 1 +P
         DO 2100 I=IT,P
            S = FRONT(ADPER(I))
            ADD= N*(I-1) + K
            DO 250 J=1,NB
               TRAV(I,J,1) = FRONT(ADD)*S
               ADD = ADD + 1
 250         CONTINUE
 2100     CONTINUE
C     BLOC DIAGONAL

C     SOUS LE BLOC DIAGONAL
C     2EME ESSAI : DES PRODUITS DE LONGUEUR NB
C

         DO 2500 IB = KB,NLB
            IA = N*(IT-1)  + K + NB*(IB-KB)
            CALL DGEMX( NB,NB,NBL,FRONT(IA),N, TRAV(IT,1,1), P,
     %                   C, NB)
C     RECOPIE

C
            DO 235 I=1,NB
               I1=I-1
               IF(IB.EQ.KB) THEN
                  J1= I
                  IND = ADPER(K + I1) - DECAL
               ELSE
                  J1=1
                  IND = ADPER(K + I1) - DECAL + NB*(IB-KB)  - I1
               ENDIF
               DO 234 J=J1,NB
                  FRN(IND) = FRN(IND) +C(J,I)
                  IND = IND +1
 234            CONTINUE
 235         CONTINUE
 2500     CONTINUE
         IF(RESTL.GT.0) THEN
            IB = NLB + 1
            IA =   N*(IT-1)  +K + NB*(IB-KB)
            CALL DGEMX( RESTL,NB,NBL,FRONT(IA),N, TRAV(IT,1,1), P,
     %                   C, NB)
C           RECOPIE

C
            DO 245 I=1,NB
               I1=I-1
C              IND = ADPER(K +I1) - DECAL  + NB*(IB-KB-1) +NB - I1
               J1=1
               IND = ADPER(K + I1) - DECAL + NB*(IB-KB)  - I1
               DO 244 J=J1,RESTL
                  FRN(IND) = FRN(IND) +C(J,I)
                  IND = IND +1
 244            CONTINUE
 245         CONTINUE
         ENDIF
 2000 CONTINUE
      ENDIF
      IF(RESTM.GT.0 ) THEN
         KB = 1+NMB
C     K : INDICE DE COLONNE DANS LA MATRICE FRONTALE (ABSOLU DE 1 A N)
         K = NB*(KB-1) + 1 +P
         DO 101 I=IT,P
            S = FRONT(ADPER(I))
            ADD= N*(I-1) + K
            DO 51 J=1,RESTM
               TRAV(I,J,1) = FRONT(ADD)*S
               ADD = ADD + 1
 51         CONTINUE
 101     CONTINUE
C     BLOC DIAGONAL

C     SOUS LE BLOC DIAGONAL
C     2EME ESSAI : DES PRODUITS DE LONGUEUR NB
C
         DO 600 IB = KB,NLB
            IA =   N*(IT-1 ) + K + NB*(IB-KB)
            CALL DGEMX( NB,RESTM,NBL,FRONT(IA),N, TRAV(IT,1,1), P,
     %                   C, NB)
C     RECOPIE

C
            DO 55 I=1,RESTM
               I1=I-1
C     IND = ADPER(K +I1) - DECAL  + NB*(IB-KB-1) +NB - I1
               IF(IB.EQ.KB) THEN
                  J1= I
                  IND = ADPER(K + I1) - DECAL
               ELSE
                  J1=1
                  IND = ADPER(K + I1) - DECAL + NB*(IB-KB)  - I1
               ENDIF
               DO 54 J=J1,NB
                  FRN(IND) = FRN(IND) +C(J,I)
                  IND = IND +1
 54            CONTINUE
 55         CONTINUE
 600     CONTINUE
         IF(RESTL.GT.0) THEN
            IB = NLB + 1
            IA =   N*(IT-1) + K + NB*(IB-KB)
            CALL DGEMX( RESTL,RESTM,NBL,FRONT(IA),N, TRAV(IT,1,1),    P,
     %                   C, NB)
C     RECOPIE

C
            DO 65 I=1,RESTM
               I1=I-1
C     IND = ADPER(K +I1) - DECAL  + NB*(IB-KB-1) +NB - I1
               J1=1
               IND = ADPER(K + I1) - DECAL + NB*(IB-KB)  - I1
               DO 64 J=J1,RESTL
                  FRN(IND) = FRN(IND) +C(J,I)
                  IND = IND +1
 64            CONTINUE
 65         CONTINUE
         ENDIF
      ENDIF
      END
