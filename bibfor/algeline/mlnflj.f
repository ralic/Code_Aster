      SUBROUTINE MLNFLJ(NB,N,LL,M,IT,P,FRONTL,FRONTU,FRNL,FRNU,ADPER,
     %TRAVL,TRAVU,CL,CU)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 04/05/2004   AUTEUR ROSE C.ROSE 
C RESPONSABLE ROSE C.ROSE
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
      INTEGER N,P,ADPER(*)
      REAL*8 FRONTL(*),FRNL(*), FRONTU(*),FRNU(*)
      INTEGER NB,DECAL,ADD,ADF,IND,NMB,I,J,L,KB,IA,IB,NLB,LL
      CHARACTER*1 TRANSA, TRANSB
      INTEGER   M,  K, LDC,KI,I1,IT,JB,J1,RESTM,RESTL,NBL
      INTEGER NPROC,NUMPRO,MLNUMP,MLNBPR
      REAL*8     S,TRAVL(P,NB,*),TRAVU(P,NB,*)
      REAL*8  CL(NB,NB, *),CU(NB,NB, *)
      NBL = P-IT+1
      NMB=M/NB
      NLB = LL/NB
      RESTM = M -(NB*NMB)
      RESTL = LL-(NB*NLB)
      DECAL = ADPER(P+1) -1
      NPROC = MLNBPR()
      IF(NMB.GE.NPROC) THEN
C$OMP PARALLEL DO DEFAULT(PRIVATE)
C$OMP+SHARED(N,M,P,NMB,NBL,NLB,NB,RESTM,RESTL,CL,CU)
C$OMP+SHARED(FRONTL,FRONTU,ADPER,DECAL,FRNL,FRNU,TRAVL,TRAVU,IT)
C$OMP+SCHEDULE(STATIC,1)
      DO 1000 KB = 1,NMB
      NUMPRO=MLNUMP()
C     K : INDICE DE COLONNE DANS LA MATRICE FRONTALE (ABSOLU DE 1 A N)
         K = NB*(KB-1) + 1 +P
         DO 100 I=IT,P
            ADD= N*(I-1) + K
            DO 50 J=1,NB
               TRAVL(I,J,NUMPRO) = FRONTL(ADD)
               TRAVU(I,J,NUMPRO) = FRONTU(ADD)
               ADD = ADD + 1
 50         CONTINUE
 100     CONTINUE
C     BLOC DIAGONAL

C     SOUS LE BLOC DIAGONAL
C     2EME ESSAI : DES PRODUITS DE LONGUEUR NB
C

         DO 500 IB = KB,NLB
            IA = N*(IT-1)  + K + NB*(IB-KB)
            CALL DGEMX( NB,NB,NBL,FRONTL(IA),N, TRAVU(IT,1,NUMPRO), P,
     %                   CL(1,1,NUMPRO), NB)
            CALL DGEMX( NB,NB,NBL,FRONTU(IA),N, TRAVL(IT,1,NUMPRO), P,
     %                   CU(1,1,NUMPRO), NB)
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
                  FRNL(IND) = FRNL(IND) +CL(J,I,NUMPRO)
                  FRNU(IND) = FRNU(IND) +CU(J,I,NUMPRO)
                  IND = IND +1
 34            CONTINUE
 35         CONTINUE
 500     CONTINUE
         IF(RESTL.GT.0) THEN
            IB = NLB + 1
            IA =   N*(IT-1)  +K + NB*(IB-KB)
            CALL DGEMX( RESTL,NB,NBL,FRONTL(IA),N,TRAVU(IT,1,NUMPRO),P,
     %                   CL(1,1,NUMPRO), NB)
            CALL DGEMX( RESTL,NB,NBL,FRONTU(IA),N,TRAVL(IT,1,NUMPRO),P,
     %                   CU(1,1,NUMPRO), NB)
C           RECOPIE

C
            DO 45 I=1,NB
               I1=I-1
               J1=1
               IND = ADPER(K + I1) - DECAL + NB*(IB-KB)  - I1
               DO 44 J=J1,RESTL
                  FRNL(IND) = FRNL(IND) +CL(J,I,NUMPRO)
                  FRNU(IND) = FRNU(IND) +CU(J,I,NUMPRO)
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
            ADD= N*(I-1) + K
            DO 250 J=1,NB
               TRAVL(I,J,1) = FRONTL(ADD)
               TRAVU(I,J,1) = FRONTU(ADD)
               ADD = ADD + 1
 250         CONTINUE
 2100     CONTINUE
C     BLOC DIAGONAL

C     SOUS LE BLOC DIAGONAL
C     2EME ESSAI : DES PRODUITS DE LONGUEUR NB
C

         DO 2500 IB = KB,NLB
            IA = N*(IT-1)  + K + NB*(IB-KB)
            CALL DGEMX( NB,NB,NBL,FRONTL(IA),N, TRAVU(IT,1,1), P,
     %                   CL(1,1,1), NB)
            CALL DGEMX( NB,NB,NBL,FRONTU(IA),N, TRAVL(IT,1,1), P,
     %                   CU(1,1,1), NB)
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
                  FRNL(IND) = FRNL(IND) +CL(J,I,1)
                  FRNU(IND) = FRNU(IND) +CU(J,I,1)
                  IND = IND +1
 234            CONTINUE
 235         CONTINUE
 2500     CONTINUE
         IF(RESTL.GT.0) THEN
            IB = NLB + 1
            IA =   N*(IT-1)  +K + NB*(IB-KB)
            CALL DGEMX( RESTL,NB,NBL,FRONTL(IA),N, TRAVU(IT,1,1), P,
     %                   CL(1,1,1), NB)
            CALL DGEMX( RESTL,NB,NBL,FRONTU(IA),N, TRAVL(IT,1,1), P,
     %                   CU(1,1,1), NB)
C           RECOPIE

C
            DO 245 I=1,NB
               I1=I-1
C              IND = ADPER(K +I1) - DECAL  + NB*(IB-KB-1) +NB - I1
               J1=1
               IND = ADPER(K + I1) - DECAL + NB*(IB-KB)  - I1
               DO 244 J=J1,RESTL
                  FRNL(IND) = FRNL(IND) +CL(J,I,1)
                  FRNU(IND) = FRNU(IND) +CU(J,I,1)
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
            ADD= N*(I-1) + K
            DO 51 J=1,RESTM
               TRAVL(I,J,1) = FRONTL(ADD)
               TRAVU(I,J,1) = FRONTU(ADD)
               ADD = ADD + 1
 51         CONTINUE
 101     CONTINUE
C     BLOC DIAGONAL

C     SOUS LE BLOC DIAGONAL
C     2EME ESSAI : DES PRODUITS DE LONGUEUR NB
C
         DO 600 IB = KB,NLB
            IA =   N*(IT-1 ) + K + NB*(IB-KB)
            CALL DGEMX( NB,RESTM,NBL,FRONTL(IA),N, TRAVU(IT,1,1), P,
     %                   CL(1,1,1), NB)
            CALL DGEMX( NB,RESTM,NBL,FRONTU(IA),N, TRAVL(IT,1,1), P,
     %                   CU(1,1,1), NB)
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
                  FRNL(IND) = FRNL(IND) +CL(J,I,1)
                  FRNU(IND) = FRNU(IND) +CU(J,I,1)
                  IND = IND +1
 54            CONTINUE
 55         CONTINUE
 600     CONTINUE
         IF(RESTL.GT.0) THEN
            IB = NLB + 1
            IA =   N*(IT-1) + K + NB*(IB-KB)
            CALL DGEMX( RESTL,RESTM,NBL,FRONTL(IA),N, TRAVU(IT,1,1), P,
     %                   CL(1,1,1), NB)
            CALL DGEMX( RESTL,RESTM,NBL,FRONTU(IA),N, TRAVL(IT,1,1), P,
     %                   CU(1,1,1), NB)
C     RECOPIE

C
            DO 65 I=1,RESTM
               I1=I-1
C     IND = ADPER(K +I1) - DECAL  + NB*(IB-KB-1) +NB - I1
               J1=1
               IND = ADPER(K + I1) - DECAL + NB*(IB-KB)  - I1
               DO 64 J=J1,RESTL
                  FRNL(IND) = FRNL(IND) +CL(J,I,1)
                  FRNU(IND) = FRNU(IND) +CU(J,I,1)
                  IND = IND +1
 64            CONTINUE
 65         CONTINUE
         ENDIF
      ENDIF
      END
