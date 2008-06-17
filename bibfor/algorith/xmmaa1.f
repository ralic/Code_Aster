      SUBROUTINE XMMAA1(NDIM,NNE,NNES,NNC,NNM,NFAES,CFACE,
     &                  HPG,FFPC,FFES,FFMA,JACOBI,IAINES,
     &                  COEFCA,NORM,ESQ,MMAT)
     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/10/2007   AUTEUR NISTOR I.NISTOR 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INTEGER  NDIM,NNE,NNM,NNES,NNC
      INTEGER  NFAES,IAINES,CFACE(5,3)
      REAL*8   MMAT(81,81),NORM(3)
      REAL*8   HPG,FFPC(9),FFES(9),FFMA(9),JACOBI
      REAL*8   COEFCA
      CHARACTER*8  ESQ
C ----------------------------------------------------------------------
C ROUTINE APPELLEE PAR : TE0366
C ----------------------------------------------------------------------
C ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
C TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
C ----------------------------------------------------------------------
C CALCUL DE A ET DE AT POUR LE CONTACT METHODE CONTINUE (XFEM)
C
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NNE    : NOMBRE TOTAL DE NOEUDS DE LA MAILLE ESCLAVE
C IN  NNES   : NOMBRE DE NOEUDS SOMMETS DE LA MAILLE ESCLAVE
C IN  NNC    : NOMBRE DE NOUEDS DE CONTACT
C IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
C IN  NFAES : NUMERO DE LA FACETTE DE CONTACT ESCLAVE
C IN  CFACE  : MATRICE DE CONECTIVITE DES FACETTES DE CONTACT
C IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFPC   : FONCTIONS DE FORME DU POINT DE CONTACT DANS ELC
C IN  FFES   : FONCTIONS DE FORME DU POINT DE CONTACT DANS ESC
C IN  FFMA   : FONCTIONS DE FORME DE LA PROJECTION DU PTC DANS MAIT
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  IAINES : POINTEUR VERS LE VECTEUR DES ARRETES ESCLAVES 
C              INTERSECTEES
C IN  COEFCA : COEF_REGU_CONT
C IN  NORM   : VALEUR DE LA NORMALE AU POINT DE CONTACT
C IN  ESQ    : NOM DE LA MAILLE ESCLAVE D'ORIGINE (QUADRATIQUE)
C I/O MMAT   : MATRICE ELEMENTAIRE DE CONTACT/FROTTEMENT
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER I,J,K,L,II,JJ,IN,PL,XOULA 
      REAL*8   E
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      

      E=1
C
C --------------------- CALCUL DE A ET DE AT--------------------------

C --- PREMIERE PARTIE DE A ET AT : PARTIE CONTACT - ESCLAVE "CLASSIQUE"
C 
      DO 10 I = 1,NNC
        DO 20 J = 1,NNES
          DO 30 K = 1,NDIM   
            IN=XOULA(CFACE,NFAES,I,IAINES,ESQ)
            CALL XPLMA2(NDIM,NNE,NNES,IN,PL)
            JJ = (3*NDIM)*(J-1)+K
            MMAT(PL,JJ) = -HPG*FFPC(I)*FFES(J)*JACOBI*NORM(K)*E
            MMAT(JJ,PL) = -HPG*FFPC(I)*FFES(J)*JACOBI*NORM(K)*E
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE
C
C --- DEUXIEME PARTIE DE A ET AT : PARTIE CONTACT - ESCLAVE "ENRICHIE"
C 
      DO 40 I = 1,NNC
        DO 50 J = 1,NNES
          DO 60 K = 1,NDIM   
            IN=XOULA(CFACE,NFAES,I,IAINES,ESQ)
            CALL XPLMA2(NDIM,NNE,NNES,IN,PL)
            JJ = (3*NDIM)*(J-1)+NDIM+K
            MMAT(PL,JJ) = HPG*FFPC(I)*FFES(J)*JACOBI*NORM(K)*E
            MMAT(JJ,PL) = HPG*FFPC(I)*FFES(J)*JACOBI*NORM(K)*E
   60     CONTINUE
   50   CONTINUE
   40 CONTINUE
C
C --- TROISIEME PARTIE DE A ET AT : PARTIE CONTACT - MAITRE "CLASSIQUE"
C 
      DO 70 I = 1,NNC
        DO 80 J = 1,NNM
          DO 90 K = 1,NDIM
            IN=XOULA(CFACE,NFAES,I,IAINES,ESQ)
            CALL XPLMA2(NDIM,NNE,NNES,IN,PL)
            JJ = (3*NDIM)*NNES+NDIM*(NNE-NNES)+2*NDIM*(J-1)+K
            MMAT(PL,JJ) = HPG*FFPC(I)*FFMA(J)*JACOBI*NORM(K)*E
            MMAT(JJ,PL) = HPG*FFPC(I)*FFMA(J)*JACOBI*NORM(K)*E
   90     CONTINUE
   80   CONTINUE
   70 CONTINUE
C
C --- QUATRIEME PARTIE DE A ET AT : PARTIE CONTACT - MAITRE "ENRICHIE"
C
      DO 71 I = 1,NNC
        DO 81 J = 1,NNM
          DO 91 K = 1,NDIM
            IN=XOULA(CFACE,NFAES,I,IAINES,ESQ)
            CALL XPLMA2(NDIM,NNE,NNES,IN,PL)
            JJ = (3*NDIM)*NNES+NDIM*(NNE-NNES)+2*NDIM*(J-1)+NDIM+K
            MMAT(PL,JJ) = HPG*FFPC(I)*FFMA(J)*JACOBI*NORM(K)*E
            MMAT(JJ,PL) = HPG*FFPC(I)*FFMA(J)*JACOBI*NORM(K)*E
   91     CONTINUE
   81   CONTINUE
   71 CONTINUE
C
C
C -------------------- CALCUL DE A_U-------------------------

C --- PREMIER BLOC DE LA MATRICE [A_U]: PARTIE ESCLAVE ESCLAVE

C------A) ESCLAVE "CLASSIQUE" - ESCLAVE "CLASSIQUE"

      DO 100 I = 1,NNES
        DO 110 J = 1,NNES
          DO 120 K = 1,NDIM
            DO 130 L = 1,NDIM
              II = (3*NDIM)*(I-1)+L
              JJ = (3*NDIM)*(J-1)+K            
              MMAT(II,JJ) = HPG*COEFCA*
     &                  FFES(I)*NORM(L)*FFES(J)*JACOBI*NORM(K)
  130       CONTINUE
  120     CONTINUE
  110   CONTINUE
  100 CONTINUE

C------B) ESCLAVE "CLASSIQUE" - ESCLAVE "ENRICHIE"

      DO 140 I = 1,NNES
        DO 150 J = 1,NNES
          DO 160 K = 1,NDIM
            DO 170 L = 1,NDIM
              II = (3*NDIM)*(I-1)+L
              JJ = (3*NDIM)*(J-1)+NDIM+K            
              MMAT(II,JJ) = -HPG*COEFCA*
     &                   FFES(I)*NORM(L)*FFES(J)*JACOBI*NORM(K)
  170       CONTINUE
  160     CONTINUE
  150   CONTINUE
  140 CONTINUE

C------C) ESCLAVE "ENRICHIE" - ESCLAVE "CLASSIQUE"

      DO 141 I = 1,NNES
        DO 151 J = 1,NNES
          DO 161 K = 1,NDIM
            DO 171 L = 1,NDIM
              II = (3*NDIM)*(I-1)+NDIM+L
              JJ = (3*NDIM)*(J-1)+K            
              MMAT(II,JJ) = -HPG*COEFCA*
     &                    FFES(I)*NORM(L)*FFES(J)*JACOBI*NORM(K)
  171       CONTINUE
  161     CONTINUE
  151   CONTINUE
  141 CONTINUE

C------D) ESCLAVE "ENRICHIE" - ESCLAVE "ENRICHIE"

      DO 180 I = 1,NNES
        DO 190 J = 1,NNES
          DO 200 K = 1,NDIM
            DO 210 L = 1,NDIM
              II = (3*NDIM)*(I-1)+NDIM+L
              JJ = (3*NDIM)*(J-1)+NDIM+K            
                MMAT(II,JJ) = HPG*COEFCA*
     &                       FFES(I)*NORM(L)*FFES(J)*JACOBI*NORM(K)
  210       CONTINUE
  200     CONTINUE
  190   CONTINUE
  180 CONTINUE     
C
C --- DEUXIEME BLOC DE LA MATRICE [AU] PARTIE ESCLAVE MAITRE
C ----A) ESCLAVE "CLASSIQUE" - MAITRE "CLASSIQUE"

      DO 220 I = 1,NNES
        DO 230 J = 1,NNM
          DO 240 K = 1,NDIM
            DO 250 L = 1,NDIM
              II = (3*NDIM)*(I-1)+L
              JJ = (3*NDIM)*NNES+NDIM*(NNE-NNES)+2*NDIM*(J-1)+K   
              MMAT(II,JJ) = -HPG*COEFCA*
     &                   FFES(I)*NORM(L)*FFMA(J)*JACOBI*NORM(K)
  250       CONTINUE
  240     CONTINUE
  230   CONTINUE
  220 CONTINUE

C ----B) ESCLAVE "CLASSIQUE" - MAITRE "ENRICHIE"

      DO 260 I = 1,NNES
        DO 270 J = 1,NNM
          DO 280 K = 1,NDIM
            DO 290 L = 1,NDIM
              II = (3*NDIM)*(I-1)+L
              JJ = (3*NDIM)*NNES+NDIM*(NNE-NNES)+
     &              2*NDIM*(J-1)+NDIM+K   
              MMAT(II,JJ) = -HPG*COEFCA*
     &                   FFES(I)*NORM(L)*FFMA(J)*JACOBI*NORM(K)
  290       CONTINUE
  280     CONTINUE
  270   CONTINUE
  260 CONTINUE

C ----C) ESCLAVE "ENRICHIE" - MAITRE "CLASSIQUE"

      DO 300 I = 1,NNES
        DO 310 J = 1,NNM
          DO 320 K = 1,NDIM
            DO 330 L = 1,NDIM
              II = (3*NDIM)*(I-1)+NDIM+L
              JJ = (3*NDIM)*NNES+NDIM*(NNE-NNES)+2*NDIM*(J-1)+K   
              MMAT(II,JJ) = HPG*COEFCA*
     &                   FFES(I)*NORM(L)*FFMA(J)*JACOBI*NORM(K)
  330       CONTINUE
  320     CONTINUE
  310   CONTINUE
  300 CONTINUE

C ----D) ESCLAVE "ENRICHIE" - MAITRE "ENRICHIE"

      DO 340 I = 1,NNES
        DO 350 J = 1,NNM
          DO 360 K = 1,NDIM
            DO 370 L = 1,NDIM
              II = (3*NDIM)*(I-1)+NDIM+L
              JJ = (3*NDIM)*NNES+NDIM*(NNE-NNES)+
     &              2*NDIM*(J-1)+NDIM+K   
              MMAT(II,JJ) = HPG*COEFCA*
     &                   FFES(I)*NORM(L)*FFMA(J)*JACOBI*NORM(K)
  370       CONTINUE
  360     CONTINUE
  350   CONTINUE
  340 CONTINUE
C
C --- TROISIEME BLOC DE LA MATRICE AU PARTIE MAITRE ESCLAVE
C-----A) MAITRE "CLASSIQUE" - ESCLAVE "CLASSIQUE" 

      DO 380 I = 1,NNM
        DO 390 J = 1,NNES
          DO 400 K = 1,NDIM
            DO 410 L = 1,NDIM
              II = (3*NDIM)*NNES+NDIM*(NNE-NNES)+2*NDIM*(I-1)+L
              JJ = (3*NDIM)*(J-1)+K             
              MMAT(II,JJ) = -HPG*COEFCA*
     &                  FFMA(I)*NORM(L)*FFES(J)*JACOBI*NORM(K)
  410       CONTINUE
  400     CONTINUE
  390   CONTINUE
  380 CONTINUE

C-----B) MAITRE "CLASSIQUE" - ESCLAVE "ENRICHIE" 

      DO 420 I = 1,NNM
        DO 430 J = 1,NNES
          DO 440 K = 1,NDIM
            DO 450 L = 1,NDIM
              II = (3*NDIM)*NNES+NDIM*(NNE-NNES)+2*NDIM*(I-1)+L
              JJ = (3*NDIM)*(J-1)+NDIM+K             
              MMAT(II,JJ) = HPG*COEFCA*
     &                   FFMA(I)*NORM(L)*FFES(J)*JACOBI*NORM(K)
  450       CONTINUE
  440     CONTINUE
  430   CONTINUE
  420 CONTINUE

C-----C) MAITRE "ENRICHIE" - ESCLAVE "CLASSIQUE" 

      DO 460 I = 1,NNM
        DO 470 J = 1,NNES
          DO 480 K = 1,NDIM
            DO 490 L = 1,NDIM
              II = (3*NDIM)*NNES+NDIM*(NNE-NNES)+
     &              2*NDIM*(I-1)+NDIM+L
              JJ = (3*NDIM)*(J-1)+K             
              MMAT(II,JJ) = -HPG*COEFCA*
     &                   FFMA(I)*NORM(L)*FFES(J)*JACOBI*NORM(K)
  490       CONTINUE
  480     CONTINUE
  470   CONTINUE
  460 CONTINUE

C-----D) MAITRE "ENRICHIE" - ESCLAVE "ENRICHIE" 

      DO 500 I = 1,NNM
        DO 510 J = 1,NNES
          DO 520 K = 1,NDIM
            DO 530 L = 1,NDIM
              II = (3*NDIM)*NNES+NDIM*(NNE-NNES)+
     &              2*NDIM*(I-1)+NDIM+L
              JJ = (3*NDIM)*(J-1)+NDIM+K             
              MMAT(II,JJ) = HPG*COEFCA*
     &                  FFMA(I)*NORM(L)*FFES(J)*JACOBI*NORM(K)
  530       CONTINUE
  520     CONTINUE
  510   CONTINUE
  500 CONTINUE
C
C --- QUATRIEME BLOC DE LA MATRICE AU PARTIE MAITRE MAITRE
C ----A) MAITRE "CLASSIQUE" - MAITRE "CLASSIQUE"

      DO 540 I = 1,NNM
        DO 550 J = 1,NNM
          DO 560 K = 1,NDIM
            DO 570 L = 1,NDIM
              II = (3*NDIM)*NNES+NDIM*(NNE-NNES)+2*NDIM*(I-1)+L
              JJ = (3*NDIM)*NNES+NDIM*(NNE-NNES)+2*NDIM*(J-1)+K
              MMAT(II,JJ) =HPG*COEFCA*
     &                   FFMA(I)*NORM(L)*FFMA(J)*JACOBI*NORM(K)
  570       CONTINUE
  560     CONTINUE
  550   CONTINUE
  540 CONTINUE

C ----B) MAITRE "CLASSIQUE" - MAITRE "ENRICHIE"      

      DO 580 I = 1,NNM
        DO 590 J = 1,NNM
          DO 600 K = 1,NDIM
            DO 610 L = 1,NDIM
              II = (3*NDIM)*NNES+NDIM*(NNE-NNES)+2*NDIM*(I-1)+L
              JJ = (3*NDIM)*NNES+NDIM*(NNE-NNES)+
     &              2*NDIM*(J-1)+NDIM+K
              MMAT(II,JJ) =HPG*COEFCA*
     &                    FFMA(I)*NORM(L)*FFMA(J)*JACOBI*NORM(K)
  610       CONTINUE
  600     CONTINUE
  590   CONTINUE
  580 CONTINUE

C ----A) MAITRE "ENRICHIE" - MAITRE "CLASSIQUE"

      DO 620 I = 1,NNM
        DO 630 J = 1,NNM
          DO 640 K = 1,NDIM
            DO 650 L = 1,NDIM
              II = (3*NDIM)*NNES+NDIM*(NNE-NNES)+
     &              2*NDIM*(I-1)+NDIM+L
              JJ = (3*NDIM)*NNES+NDIM*(NNE-NNES)+2*NDIM*(J-1)+K
              MMAT(II,JJ) =HPG*COEFCA*
     &                   FFMA(I)*NORM(L)*FFMA(J)*JACOBI*NORM(K)
  650       CONTINUE
  640     CONTINUE
  630   CONTINUE
  620 CONTINUE

C ----A) MAITRE "ENRICHIE" - MAITRE "ENRICHIE"

      DO 660 I = 1,NNM
        DO 670 J = 1,NNM
          DO 680 K = 1,NDIM
            DO 690 L = 1,NDIM
              II = (3*NDIM)*NNES+NDIM*(NNE-NNES)+
     &              2*NDIM*(I-1)+NDIM+L
              JJ = (3*NDIM)*NNES+NDIM*(NNE-NNES)+
     &              2*NDIM*(J-1)+NDIM+K
              MMAT(II,JJ) =HPG*COEFCA*
     &                   FFMA(I)*NORM(L)*FFMA(J)*JACOBI*NORM(K)
  690       CONTINUE
  680     CONTINUE
  670   CONTINUE
  660 CONTINUE 
C
      CALL JEDEMA()     
      END
