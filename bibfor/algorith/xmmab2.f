      SUBROUTINE XMMAB2 (NDIM,NNE,NNES,NNC,NNM,NFAES,CFACE,
     &                   HPG,FFPC,FFES,FFMA,JACOBI,IAINES,    
     &                   LAMBDA,COEFFA,COEFFF,TAU1,TAU2,
     &                   RESE,NRESE,MPROJ,ESQ,MMAT)                
      IMPLICIT NONE
      INTEGER  NDIM,NNC,NNE,NNES,NNM,NFAES,IAINES,CFACE(5,3)
      REAL*8   HPG,FFPC(9),FFES(9),FFMA(9),JACOBI  
      REAL*8   LAMBDA,COEFFF,COEFFA   
      REAL*8   TAU1(3),TAU2(3),RESE(3),NRESE,MMAT(81,81),MPROJ(3,3)
      CHARACTER*8  ESQ
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/08/2008   AUTEUR MAZET S.MAZET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_21
C
C ROUTINE APPELLEE PAR : TE0366
C ----------------------------------------------------------------------
C
C CALCUL DE B ET DE BT POUR LE CONTACT METHODE CONTINUE
C SANS ADHERENCE
C
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
C IN  NNES   : NOMBRE DE NOEUDS SOMMETS DE LA MAILLE ESCLAVE
C IN  NNC    : NOMBRE DE NOEUDS DE CONTACT
C IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
C IN  NFAES  : NUMERO DE LA FACETTE DE CONTACT ESCLAVE
C IN  CFACE  : MATRICE DE CONECTIVITE DES FACETTES DE CONTACT
C IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFPC   : FONCTIONS DE FORME DU PT CONTACT DANS ELC
C IN  FFES   : FONCTIONS DE FORME DU PT CONTACT DANS ESC
C IN  FFMA   : FONCTIONS DE FORME DE LA PROJECTION DU PTC DANS MAIT
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  IAINES : POINTEUR VERS LE VECTEUR DES ARRETES ESCLAVES 
C              INTERSECTEES
C IN  LAMBDA : VALEUR DU SEUIL_INIT
C IN  COEFFA : COEF_REGU_FROT
C IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
C IN  TAU1   : PREMIERE TANGENTE
C IN  TAU2   : SECONDE TANGENTE
C IN  RESE   : PROJECTION DE LA BOULE UNITE POUR LE FROTTEMENT
C IN  NRESE  : RACINE DE LA NORME DE RESE
C IN  MPROJ  : MATRICE DE L'OPERATEUR DE PROJECTION
C IN  ESQ    : NOM DE LA MAILLE ESCLAVE D'ORIGINE (QUADRATIQUE)
C I/O MMAT   : MATRICE ELEMENTAIRE DE CONTACT/FROTTEMENT
C ----------------------------------------------------------------------
      INTEGER   I,J,K,L,II,JJ,INI,INJ,PLI,PLJ,XOULA
      REAL*8    C1(3),C2(3),C3(3),D1(3),D2(3),D3(3),H1(3),H2(3)
      REAL*8    G(3,3),D(3,3),H(3,3),B(3,3),R(3,3)
      
C ----------------------------------------------------------------------
C
C --- INITIALISATIONS
C
      DO 300 I = 1,3    
        DO 290 J = 1,3
          G(I,J) = 0.D0
          D(I,J) = 0.D0
          H(I,J) = 0.D0
          B(I,J) = 0.D0
          R(I,J) = 0.D0
  290   CONTINUE
  300 CONTINUE   
      DO 3 K = 1,3
        C1(K) = MPROJ(K,1)
        C2(K) = MPROJ(K,2)
        C3(K) = MPROJ(K,3)
        D1(K) = 0.D0
        D2(K) = 0.D0
        D3(K) = 0.D0
        H1(K) = 0.D0
        H2(K) = 0.D0
3     CONTINUE    
C
C --- CALCUL  DE IK(/\,U)
C 
      CALL MKKVEC(RESE,NRESE,NDIM,C1,D1)
      CALL MKKVEC(RESE,NRESE,NDIM,C2,D2)
      CALL MKKVEC(RESE,NRESE,NDIM,C3,D3)

      DO 12 K = 1,NDIM
        G(K,1) = D1(K)
        G(K,2) = D2(K)
        G(K,3) = D3(K)
12    CONTINUE
C
C --- D:K(-)C.C
C
      DO 13 I = 1,NDIM
        DO 14 J = 1,NDIM
          DO 15 K = 1,NDIM
            D(I,J) = G(K,I)*MPROJ(K,J) + D(I,J)
  15      CONTINUE
  14    CONTINUE
  13  CONTINUE

      CALL MKKVEC(RESE,NRESE,NDIM,TAU1,H1)
      CALL MKKVEC(RESE,NRESE,NDIM,TAU2,H2)

      DO 16 K = 1,3
        H(K,1) = H1(K)
        H(K,2) = H2(K)
 16   CONTINUE
C
C --- B:K(-)T.C
C
      DO  23 I = 1,NDIM-1
        DO 24 J = 1,NDIM
          DO 25  K = 1,NDIM
            B(I,J) = H(K,I)*MPROJ(K,J)+B(I,J)
  25      CONTINUE
  24    CONTINUE
  23  CONTINUE
C
C --------------------- CALCUL DE B ET DE BT--------------------------
C      
C --- PREMIERE PARTIE DE B ET BT : PARTIE CONTACT - ESCLAVE "CLASSIQUE"
C 
      DO 200 I=1,NNC
        DO 201 J=1,NNES
          DO 202 L=1,NDIM-1
            DO 203 K=1,NDIM
              INI=XOULA(CFACE,NFAES,I,IAINES,ESQ)
              CALL XPLMA2(NDIM,NNE,NNES,INI,PLI)
              II = PLI+L
              JJ = (3*NDIM)*(J-1)+K
              MMAT(II,JJ) = 
     &-LAMBDA*COEFFF*HPG*FFPC(I)*FFES(J)*JACOBI*B(L,K)
              MMAT(JJ,II) = MMAT(II,JJ)
 203        CONTINUE
 202      CONTINUE
 201    CONTINUE
 200  CONTINUE
C
C --- DEUXIEME PARTIE DE B ET BT : PARTIE CONTACT - ESCLAVE "ENRICHIE"
C
      DO 204 I=1,NNC
        DO 205 J=1,NNES
          DO 206 L=1,NDIM-1
            DO 207 K=1,NDIM
              INI=XOULA(CFACE,NFAES,I,IAINES,ESQ)
              CALL XPLMA2(NDIM,NNE,NNES,INI,PLI)
              II = PLI+L
              JJ = (3*NDIM)*(J-1)+NDIM+K
              MMAT(II,JJ) = 
     & LAMBDA*COEFFF*HPG*FFPC(I)*FFES(J)*JACOBI*B(L,K)
              MMAT(JJ,II) = MMAT(II,JJ)     
 207        CONTINUE
 206      CONTINUE
 205    CONTINUE
 204  CONTINUE
C
C --- TROISIEME PARTIE DE B ET BT : PARTIE CONTACT - MAITRE "CLASSIQUE"
C 
      DO 208 I = 1,NNC
        DO 209 J = 1,NNM
          DO 210 L=1,NDIM-1
            DO 211 K = 1,NDIM
              INI=XOULA(CFACE,NFAES,I,IAINES,ESQ)
              CALL XPLMA2(NDIM,NNE,NNES,INI,PLI)
              II = PLI+L
              JJ = (3*NDIM)*NNES+NDIM*(NNE-NNES)+2*NDIM*(J-1)+K
              MMAT(II,JJ) =
     & LAMBDA*COEFFF*HPG*FFPC(I)*FFMA(J)*JACOBI*B(L,K)  
              MMAT(JJ,II) = MMAT(II,JJ)
 211        CONTINUE
 210      CONTINUE
 209    CONTINUE
 208  CONTINUE
C
C --- QUATRIEME PARTIE DE B ET BT : PARTIE CONTACT - MAITRE "ENRICHIE"
C
      DO 212 I = 1,NNC
        DO 213 J = 1,NNM
          DO 214 L=1,NDIM-1
            DO 215 K = 1,NDIM
              INI=XOULA(CFACE,NFAES,I,IAINES,ESQ)
              CALL XPLMA2(NDIM,NNE,NNES,INI,PLI)
              II = PLI+L
              JJ = (3*NDIM)*NNES+NDIM*(NNE-NNES)+2*NDIM*(J-1)+NDIM+K
              MMAT(II,JJ) =
     & LAMBDA*COEFFF*HPG*FFPC(I)*FFMA(J)*JACOBI*B(L,K)  
              MMAT(JJ,II) = MMAT(II,JJ)
 215        CONTINUE
 214      CONTINUE
 213    CONTINUE
 212  CONTINUE
C
C --- ON CALCULE LA MATRICE B_U
C
C --- PREMIER BLOC DE LA MATRICE [B_U]: PARTIE ESCLAVE ESCLAVE
C
C------A) ESCLAVE "CLASSIQUE" - ESCLAVE "CLASSIQUE"

      DO 100 I = 1,NNES
        DO 101 J = 1,NNES
          DO 102 K = 1,NDIM
            DO 103 L = 1,NDIM
              II = (3*NDIM)*(I-1)+L
              JJ = (3*NDIM)*(J-1)+K            
              MMAT(II,JJ) =
     &-COEFFA*COEFFF*HPG*LAMBDA*FFES(I)*FFES(J)*JACOBI*D(L,K)
  103       CONTINUE
  102     CONTINUE
  101   CONTINUE
  100 CONTINUE

C------B) ESCLAVE "CLASSIQUE" - ESCLAVE "ENRICHIE"

      DO 104 I = 1,NNES
        DO 105 J = 1,NNES
          DO 106 K = 1,NDIM
            DO 107 L = 1,NDIM
              II = (3*NDIM)*(I-1)+L
              JJ = (3*NDIM)*(J-1)+NDIM+K            
              MMAT(II,JJ) =
     & COEFFA*COEFFF*HPG*LAMBDA*FFES(I)*FFES(J)*JACOBI*D(L,K)
  107       CONTINUE
  106     CONTINUE
  105   CONTINUE
  104 CONTINUE

C------C) ESCLAVE "ENRICHIE" - ESCLAVE "CLASSIQUE"

      DO 108 I = 1,NNES
        DO 109 J = 1,NNES
          DO 110 K = 1,NDIM
            DO 111 L = 1,NDIM
              II = (3*NDIM)*(I-1)+NDIM+L
              JJ = (3*NDIM)*(J-1)+K            
              MMAT(II,JJ) =
     & COEFFA*COEFFF*HPG*LAMBDA*FFES(I)*FFES(J)*JACOBI*D(L,K)
  111       CONTINUE
  110     CONTINUE
  109   CONTINUE
  108 CONTINUE

C------D) ESCLAVE "ENRICHIE" - ESCLAVE "ENRICHIE"

      DO 112 I = 1,NNES
        DO 113 J = 1,NNES
          DO 114 K = 1,NDIM
            DO 115 L = 1,NDIM
              II = (3*NDIM)*(I-1)+NDIM+L
              JJ = (3*NDIM)*(J-1)+NDIM+K            
                MMAT(II,JJ) =
     &-COEFFA*COEFFF*HPG*LAMBDA*FFES(I)*FFES(J)*JACOBI*D(L,K)
  115       CONTINUE
  114     CONTINUE
  113   CONTINUE
  112 CONTINUE     
C
C --- DEUXIEME BLOC DE LA MATRICE [B_U] PARTIE ESCLAVE MAITRE
C ----A) ESCLAVE "CLASSIQUE" - MAITRE "CLASSIQUE"

      DO 116 I = 1,NNES
        DO 117 J = 1,NNM
          DO 118 K = 1,NDIM
            DO 119 L = 1,NDIM
              II = (3*NDIM)*(I-1)+L
              JJ = (3*NDIM)*NNES+NDIM*(NNE-NNES)+2*NDIM*(J-1)+K   
              MMAT(II,JJ) =
     & COEFFA*COEFFF*HPG*LAMBDA*FFES(I)*FFMA(J)*JACOBI*D(L,K)
  119       CONTINUE
  118     CONTINUE
  117   CONTINUE
  116 CONTINUE

C ----B) ESCLAVE "CLASSIQUE" - MAITRE "ENRICHIE"

      DO 120 I = 1,NNES
        DO 121 J = 1,NNM
          DO 122 K = 1,NDIM
            DO 123 L = 1,NDIM
              II = (3*NDIM)*(I-1)+L
              JJ = (3*NDIM)*NNES+NDIM*(NNE-NNES)+
     &              2*NDIM*(J-1)+NDIM+K   
              MMAT(II,JJ) =
     & COEFFA*COEFFF*HPG*LAMBDA*FFES(I)*FFMA(J)*JACOBI*D(L,K)
  123       CONTINUE
  122     CONTINUE
  121   CONTINUE
  120 CONTINUE

C ----C) ESCLAVE "ENRICHIE" - MAITRE "CLASSIQUE"

      DO 124 I = 1,NNES
        DO 125 J = 1,NNM
          DO 126 K = 1,NDIM
            DO 127 L = 1,NDIM
              II = (3*NDIM)*(I-1)+NDIM+L
              JJ = (3*NDIM)*NNES+NDIM*(NNE-NNES)+2*NDIM*(J-1)+K   
              MMAT(II,JJ) =
     &-COEFFA*COEFFF*HPG*LAMBDA*FFES(I)*FFMA(J)*JACOBI*D(L,K)
  127       CONTINUE
  126     CONTINUE
  125   CONTINUE
  124 CONTINUE

C ----D) ESCLAVE "ENRICHIE" - MAITRE "ENRICHIE"

      DO 128 I = 1,NNES
        DO 129 J = 1,NNM
          DO 130 K = 1,NDIM
            DO 131 L = 1,NDIM
              II = (3*NDIM)*(I-1)+NDIM+L
              JJ = (3*NDIM)*NNES+NDIM*(NNE-NNES)+
     &              2*NDIM*(J-1)+NDIM+K   
              MMAT(II,JJ) =
     &-COEFFA*COEFFF*HPG*LAMBDA*FFES(I)*FFMA(J)*JACOBI*D(L,K)
  131       CONTINUE
  130     CONTINUE
  129   CONTINUE
  128 CONTINUE
C
C --- TROISIEME BLOC DE LA MATRICE [B_U] PARTIE MAITRE ESCLAVE
C-----A) MAITRE "CLASSIQUE" - ESCLAVE "CLASSIQUE" 

      DO 132 I = 1,NNM
        DO 133 J = 1,NNES
          DO 134 K = 1,NDIM
            DO 135 L = 1,NDIM
              II = (3*NDIM)*NNES+NDIM*(NNE-NNES)+2*NDIM*(I-1)+L
              JJ = (3*NDIM)*(J-1)+K             
              MMAT(II,JJ) =
     & COEFFA*COEFFF*HPG*LAMBDA*FFMA(I)*FFES(J)*JACOBI*D(L,K)
  135       CONTINUE
  134     CONTINUE
  133   CONTINUE
  132 CONTINUE

C-----B) MAITRE "CLASSIQUE" - ESCLAVE "ENRICHIE" 

      DO 136 I = 1,NNM
        DO 137 J = 1,NNES
          DO 138 K = 1,NDIM
            DO 139 L = 1,NDIM
              II = (3*NDIM)*NNES+NDIM*(NNE-NNES)+2*NDIM*(I-1)+L
              JJ = (3*NDIM)*(J-1)+NDIM+K             
              MMAT(II,JJ) =
     &-COEFFA*COEFFF*HPG*LAMBDA*FFMA(I)*FFES(J)*JACOBI*D(L,K)
  139       CONTINUE
  138     CONTINUE
  137   CONTINUE
  136 CONTINUE

C-----C) MAITRE "ENRICHIE" - ESCLAVE "CLASSIQUE" 

      DO 140 I = 1,NNM
        DO 141 J = 1,NNES
          DO 142 K = 1,NDIM
            DO 143 L = 1,NDIM
              II = (3*NDIM)*NNES+NDIM*(NNE-NNES)+
     &              2*NDIM*(I-1)+NDIM+L
              JJ = (3*NDIM)*(J-1)+K             
              MMAT(II,JJ) =
     & COEFFA*COEFFF*HPG*LAMBDA*FFMA(I)*FFES(J)*JACOBI*D(L,K)
  143       CONTINUE
  142     CONTINUE
  141   CONTINUE
  140 CONTINUE

C-----D) MAITRE "ENRICHIE" - ESCLAVE "ENRICHIE" 

      DO 144 I = 1,NNM
        DO 145 J = 1,NNES
          DO 146 K = 1,NDIM
            DO 147 L = 1,NDIM
              II = (3*NDIM)*NNES+NDIM*(NNE-NNES)+
     &              2*NDIM*(I-1)+NDIM+L
              JJ = (3*NDIM)*(J-1)+NDIM+K             
              MMAT(II,JJ) =
     &-COEFFA*COEFFF*HPG*LAMBDA*FFMA(I)*FFES(J)*JACOBI*D(L,K)
  147       CONTINUE
  146     CONTINUE
  145   CONTINUE
  144 CONTINUE
C
C --- QUATRIEME BLOC DE LA MATRICE [B_U] PARTIE MAITRE MAITRE
C ----A) MAITRE "CLASSIQUE" - MAITRE "CLASSIQUE"

      DO 148 I = 1,NNM
        DO 149 J = 1,NNM
          DO 150 K = 1,NDIM
            DO 151 L = 1,NDIM
              II = (3*NDIM)*NNES+NDIM*(NNE-NNES)+2*NDIM*(I-1)+L
              JJ = (3*NDIM)*NNES+NDIM*(NNE-NNES)+2*NDIM*(J-1)+K
              MMAT(II,JJ) =
     &-COEFFA*COEFFF*HPG*LAMBDA*FFMA(I)*FFMA(J)*JACOBI*D(L,K)
      
  151       CONTINUE
  150     CONTINUE
  149   CONTINUE
  148 CONTINUE

C ----B) MAITRE "CLASSIQUE" - MAITRE "ENRICHIE"      

      DO 152 I = 1,NNM
        DO 153 J = 1,NNM
          DO 154 K = 1,NDIM
            DO 155 L = 1,NDIM
              II = (3*NDIM)*NNES+NDIM*(NNE-NNES)+2*NDIM*(I-1)+L
              JJ = (3*NDIM)*NNES+NDIM*(NNE-NNES)+
     &              2*NDIM*(J-1)+NDIM+K
              MMAT(II,JJ) =
     &-COEFFA*COEFFF*HPG*LAMBDA*FFMA(I)*FFMA(J)*JACOBI*D(L,K)
  155       CONTINUE
  154     CONTINUE
  153   CONTINUE
  152 CONTINUE

C ----C) MAITRE "ENRICHIE" - MAITRE "CLASSIQUE"

      DO 156 I = 1,NNM
        DO 157 J = 1,NNM
          DO 158 K = 1,NDIM
            DO 159 L = 1,NDIM
              II = (3*NDIM)*NNES+NDIM*(NNE-NNES)+
     &              2*NDIM*(I-1)+NDIM+L
              JJ = (3*NDIM)*NNES+NDIM*(NNE-NNES)+2*NDIM*(J-1)+K
              MMAT(II,JJ) =
     &-COEFFA*COEFFF*HPG*LAMBDA*FFMA(I)*FFMA(J)*JACOBI*D(L,K)
  159       CONTINUE
  158     CONTINUE
  157   CONTINUE
  156 CONTINUE

C ----D) MAITRE "ENRICHIE" - MAITRE "ENRICHIE"

      DO 160 I = 1,NNM
        DO 161 J = 1,NNM
          DO 162 K = 1,NDIM
            DO 163 L = 1,NDIM
              II = (3*NDIM)*NNES+NDIM*(NNE-NNES)+
     &              2*NDIM*(I-1)+NDIM+L
              JJ = (3*NDIM)*NNES+NDIM*(NNE-NNES)+
     &              2*NDIM*(J-1)+NDIM+K
              MMAT(II,JJ) =
     &-COEFFA*COEFFF*HPG*LAMBDA*FFMA(I)*FFMA(J)*JACOBI*D(L,K)
  163       CONTINUE
  162     CONTINUE
  161   CONTINUE
  160 CONTINUE 
C
C --- ON CALCULE LA MATRICE F CAR IL Y A GLISSEMENT
C --- R(I,J)= H_I . TAU_J
C
      DO 857 K = 1,NDIM
        R(1,1) = (TAU1(K)-H1(K))*TAU1(K) + R(1,1)
        R(1,2) = (TAU1(K)-H1(K))*TAU2(K) + R(1,2)
        R(2,1) = (TAU2(K)-H2(K))*TAU1(K) + R(2,1)
        R(2,2) = (TAU2(K)-H2(K))*TAU2(K) + R(2,2)
 857  CONTINUE
C
      DO 831 I = 1,NNC
        DO 832 J = 1,NNC
          DO 833 L = 1,NDIM-1
            DO 834 K = 1,NDIM-1
C            
              INI=XOULA(CFACE,NFAES,I,IAINES,ESQ)
              CALL XPLMA2(NDIM,NNE,NNES,INI,PLI)
              INJ=XOULA(CFACE,NFAES,J,IAINES,ESQ)
              CALL XPLMA2(NDIM,NNE,NNES,INJ,PLJ)
              II = PLI+L
              JJ = PLJ+K
C
              MMAT(II,JJ) = COEFFF*LAMBDA*HPG*FFPC(I)*FFPC(J)*
     &                        JACOBI*R(L,K) / COEFFA
 834        CONTINUE
 833      CONTINUE
 832    CONTINUE
 831  CONTINUE 
C
      END
