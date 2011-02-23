      SUBROUTINE LONSEG(NDIM,NNO,NPG,GEOM,DFDE,POIDS,L)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 22/02/2011   AUTEUR TARDIEU N.TARDIEU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INTEGER NDIM,NNO,NPG
      REAL*8  GEOM(NDIM,NNO),DFDE(NNO*NPG),POIDS(NPG)
      REAL*8  DX,DXDS,DYDS,L

C ----------------------------------------------------------------------
C     CALCUL LA LONGUEUR D'UN SEG2 OU SEG3 EN DIM 2 OU 3
C               **            ***
C ----------------------------------------------------------------------
C IN  NDIM   DIMENSION DE L'ESPACE
C IN  NNO    NOMBRE DE NOEUDS
C IN  NPG    NOMBRE DE POINTS DE GAUSS
C IN  GEOM   COORDONNEES DES NOEUDS
C IN  DFDE   DERIVEES DES FONCTIONS DE FORME
C IN  POIDS  POIDS DES POIDS DE GAUSS DE L'ELEMENT DE REFERENCE
C OUT L      LONGUEUR DE L'ELEMENT
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
      INTEGER I,IPG
      REAL*8  U(3),V(3),W(3),MATINV(3,3),TMP(3),NORM,DETM,K1(3)
      REAL*8  K2(3),K3(3),COOR(6),JAC,SQRT,R8MIEM
C ----------------------------------------------------------------------
      CALL JEMARQ()
C
C --- 1 - EXPRESSION DES 3 NOEUDS DANS UN REPERE LOCAL 2D:
C     ===================================================
C
      IF(NDIM.EQ.3)THEN

C         -- CALCUL DU REPERE LOCAL (U,V,W): 
           DO 10 I = 1,NDIM
            U(I)=GEOM(I,2)-GEOM(I,1)
            V(I)=GEOM(I,3)-GEOM(I,1)
 10       CONTINUE
          CALL NORMEV(U,NORM)
          CALL NORMEV(V,NORM)
          W(1)=U(2)*V(3)-U(3)*V(2)
          W(2)=U(3)*V(1)-U(1)*V(3)
          W(3)=U(1)*V(2)-U(2)*V(1)
          V(1)=W(2)*U(3)-W(3)*U(2)
          V(2)=W(3)*U(1)-W(1)*U(3)
          V(3)=W(1)*U(2)-W(2)*U(1)
C                                                (U1 U2 U3)
C         -- CALCUL DU DETERMINANT DE LA MATRICE (V1 V2 V3) 
C                                                (W1 W2 W3)
          DETM= (U(1)*V(2)*W(3)+V(1)*W(2)*U(3)+W(1)*U(2)*V(3))
     &         -(V(1)*U(2)*W(3)+U(1)*W(2)*V(3)+W(1)*V(2)*U(3))
C
C         -- CALCUL DE LA MATRICE INVERSE DE M : MATINV
          MATINV(1,1)=(1.D0/DETM)*(V(2)*W(3)-W(2)*V(3))
          MATINV(1,2)=(1.D0/DETM)*(W(2)*U(3)-U(2)*W(3))
          MATINV(1,3)=(1.D0/DETM)*(U(2)*V(3)-V(2)*U(3))
          MATINV(2,1)=(1.D0/DETM)*(W(1)*V(3)-V(1)*W(3))
          MATINV(2,2)=(1.D0/DETM)*(U(1)*W(3)-W(1)*U(3))
          MATINV(2,3)=(1.D0/DETM)*(V(1)*U(3)-U(1)*V(3))
          MATINV(3,1)=(1.D0/DETM)*(V(1)*W(2)-W(1)*V(2))
          MATINV(3,2)=(1.D0/DETM)*(W(1)*U(2)-U(1)*W(2))
          MATINV(3,3)=(1.D0/DETM)*(U(1)*V(2)-V(1)*U(2))

C         -- EXPRESSION DE K1 (PREMIER NOEUD) DANS LE REPERE LOCAL
          K1(1)=GEOM(1,1)*MATINV(1,1)+GEOM(2,1)*MATINV(2,1)+
     &          GEOM(3,1)*MATINV(3,1)
          K1(2)=GEOM(1,1)*MATINV(1,2)+GEOM(2,1)*MATINV(2,2)+
     &          GEOM(3,1)*MATINV(3,2)
          K1(3)=GEOM(1,1)*MATINV(1,3)+GEOM(2,1)*MATINV(2,3)+
     &          GEOM(3,1)*MATINV(3,3)
C         -- EXPRESSION DE K2 (DEUXIEME NOEUD) DANS LE REPERE LOCAL
          K2(1)=GEOM(1,2)*MATINV(1,1)+GEOM(2,2)*MATINV(2,1)+
     &          GEOM(3,2)*MATINV(3,1)
          K2(2)=GEOM(1,2)*MATINV(1,2)+GEOM(2,2)*MATINV(2,2)+
     &          GEOM(3,2)*MATINV(3,2)
          K2(3)=GEOM(1,2)*MATINV(1,3)+GEOM(2,2)*MATINV(2,3)+
     &          GEOM(3,2)*MATINV(3,3)
C         -- EXPRESSION DE K3 (NOEUD MILIEU) DANS LE REPERE LOCAL
          K3(1)=GEOM(1,3)*MATINV(1,1)+GEOM(2,3)*MATINV(2,1)+
     &          GEOM(3,3)*MATINV(3,1)
          K3(2)=GEOM(1,3)*MATINV(1,2)+GEOM(2,3)*MATINV(2,2)+
     &          GEOM(3,3)*MATINV(3,2)
          K3(3)=GEOM(1,3)*MATINV(1,3)+GEOM(2,3)*MATINV(2,3)+
     &          GEOM(3,3)*MATINV(3,3)

C         -- VERIFICATION QUE L'ON EST BIEN DANS LE PLAN
          TMP(1)=K1(3)
          TMP(2)=K2(3)
          TMP(3)=K3(3)
          CALL NORMEV(TMP,NORM)
          CALL ASSERT(TMP.LE.R8MIEM())
C
C
      ELSEIF(NDIM.EQ.2)THEN             

          K1(1)=GEOM(1,1)
          K1(2)=GEOM(2,1)
          K2(1)=GEOM(1,2)
          K2(2)=GEOM(2,2)
          K3(1)=GEOM(1,3)
          K3(2)=GEOM(2,3)
C
      ELSE
          CALL ASSERT(.FALSE.)
      ENDIF
C
C --- 2 - CALCUL DE LA LONGUEUR DE L'ELEMENT :
C     =======================================
C
      COOR(1)=K1(1)
      COOR(2)=K1(2)
      COOR(3)=K2(1)
      COOR(4)=K2(2)
      COOR(5)=K3(1)
      COOR(6)=K3(2)

      L=0.D0
      DO 30 IPG=1,NPG
         DXDS = 0.D0
         DYDS = 0.D0
         DO 40 I = 1,3
           DX = DFDE(3*(IPG-1)+I)
           DXDS = DXDS + DX * COOR(2*I-1)
           DYDS = DYDS + DX * COOR(2*I)
 40      CONTINUE
         JAC = POIDS(IPG) * SQRT(DXDS**2 + DYDS**2)
         L=L+JAC
 30   CONTINUE

      CALL JEDEMA()

      END
