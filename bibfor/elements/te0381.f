      SUBROUTINE TE0381(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
C.......................................................................
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     BUT: CALCUL DES MATRICES DE RIGIDITE  ELEMENTAIRES EN MECANIQUE
C          ELEMENTS ISOPARAMETRIQUES 3D
C
C          OPTION : 'RIGI_MECA_LAGR'
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C
      PARAMETER         ( NBRES=2 )
      CHARACTER*8        ELREFE,NOMRES(NBRES)
      CHARACTER*2        CODRET(NBRES)
      CHARACTER*16       NOMTE,OPTION
      CHARACTER*24       CHVAL,CHCTE
      REAL*8             DTDM(3,3)
      REAL*8             VALRES(NBRES),UNDEMI,A(3,3,27,27)
      REAL*8             AUXI1,AUXI2,AUXI3,AUXJ1,AUXJ2,AUXJ3
      REAL*8             A11,A12,A13,A21,A22,A23,A31,A32,A33,DER(3)
      REAL*8             DFDX(27),DFDY(27),DFDZ(27),TPG,C1,C2,C3,POIDS
      REAL*8             ALPHA,ALPHD,DETFA
      INTEGER            IFF,IPOIDS,IVF,IDFDE,IDFDN,IDFDK,IGEOM,IMATE
      INTEGER            NNO,KP,NPG1,II,JJ,I,J,IMATUU,ITEMPE
      INTEGER            NBPG(10)
C
C
C
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C------------FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CALL ELREF1(ELREFE)
C
      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,'L',JIN)
      NDIM = ZI(JIN+1-1)
      NNO = ZI(JIN+2-1)
      NBFPG = ZI(JIN+3-1)
      DO 110 I = 1,NBFPG
         NBPG(I) = ZI(JIN+3-1+I)
  110 CONTINUE
      NPG1 = NBPG(1)
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,'L',JVAL)
C
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
      IVF    = IPOIDS + NPG1
      IDFDE  = IVF    + NPG1*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
C
      CALL JEVECH('PGEOMER','L',IGEOM)
C
      CALL JEVECH('PMATERC','L',IMATE)
      MATER = ZI(IMATE)
      NOMRES(1)='E'
      NOMRES(2)='NU'
      CALL JEVECH('PMATUUR','E',IMATUU)
      CALL JEVECH('PTEMPER','L',ITEMPE)
      CALL JEVECH('PTHETAR','L',ITHET)
      CALL JEVECH('PALPHAR','L',IALPH)
C
      ALPHA = ZR(IALPH)

      DO 112 K=1,3
         DO 112 L=1,3
            DO 112 I=1,NNO
            DO 112 J=1,I
                A(K,L,I,J) = 0.D0
112          CONTINUE
C
      UNDEMI = 0.5D0
C
C    BOUCLE SUR LES POINTS DE GAUSS
C
      DO 101 KP=1,NPG1
C
        L = (KP-1)*NNO
        K = L*3
C
        CALL DFDM3D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &   ZR(IDFDK+K),ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS )
C
        TPG = 0.D0
        DO 102 I=1,NNO
          TPG = TPG + ZR(ITEMPE+I-1) *ZR(IVF+L+I-1)
102     CONTINUE
C
C CALCUL DU GRADIENT DE THETA
C
        DO 350 I=1,3
           DO 350 J=1,3
           DTDM(I,J) = 0.D0
350     CONTINUE
C
        DO 400 I=1,NNO
           DER(1) = DFDX(I)
           DER(2) = DFDY(I)
           DER(3) = DFDZ(I)
           DO 450 J=1,3
              DO 450 K=1,3
              DTDM(J,K) = DTDM(J,K) + ZR(ITHET+3*I-4+J)*DER(K)
450      CONTINUE
400    CONTINUE
C
       CALL RCVALA ( MATER,'ELAS',1,'TEMP',TPG,2,NOMRES,
     &                VALRES, CODRET, 'FM' )
       C1 = VALRES(1)/(1.D0 + VALRES(2))
       C2 = (1.D0 - VALRES(2))/(1.D0 - 2.D0*VALRES(2))
       C3 = VALRES(2)/(1.D0 - 2.D0*VALRES(2))
       DC2 = C2 * 2.D0
       DC3 = C3 * 2.D0
       ALPHD= ALPHA*ALPHA
C
C    COEFFICIENTS DE L'INVERSE DE LA MATRICE FALPHA
C
       A11=(1.D0+ALPHA*DTDM(2,2))*(1.D0+ALPHA*DTDM(3,3)) -
     &      ALPHD*DTDM(3,2)*DTDM(2,3)
C
       A21= ALPHD*DTDM(3,1)*DTDM(2,3)
     &      - ALPHA*DTDM(2,1)*(1.D0+ALPHA*DTDM(3,3))
C
       A31= ALPHD*DTDM(2,1)*DTDM(3,2)
     &      - ALPHA*DTDM(3,1)*(1.D0+ALPHA*DTDM(2,2))
C
       A12= ALPHD*DTDM(3,2)*DTDM(1,3)
     &      - ALPHA*DTDM(1,2)*(1.D0+ALPHA*DTDM(3,3))
C
       A22=(1.D0+ALPHA*DTDM(1,1))*(1.D0+ALPHA*DTDM(3,3))
     &      - ALPHD*DTDM(3,1)*DTDM(1,3)
C
       A32= ALPHD*DTDM(3,1)*DTDM(1,2)
     &      - ALPHA*DTDM(3,2)*(1.D0+ALPHA*DTDM(1,1))
C
       A13= ALPHD*DTDM(1,2)*DTDM(2,3)
     &      - ALPHA*DTDM(1,3)*(1.D0+ALPHA*DTDM(2,2))
C
       A23= ALPHD*DTDM(2,1)*DTDM(1,3)
     &       - ALPHA*DTDM(2,3)*(1.D0+ALPHA*DTDM(1,1))
C
       A33=(1.D0+ALPHA*DTDM(1,1))*(1.D0+ALPHA*DTDM(2,2))
     &     - ALPHD*DTDM(2,1)*DTDM(1,2)
C
C    DETERMINANT DE FALPHA
C
       DETFA = (1.D0+ALPHA*DTDM(1,1))*A11
     &        + ALPHA*DTDM(1,2)*A21 + ALPHA*DTDM(1,3)*A31
C
           COEF = POIDS * C1 * UNDEMI * (1.D0/DETFA)
C
           DO 106 I=1,NNO
C
       AUXI1=A11*DFDX(I) + A21*DFDY(I) + A31*DFDZ(I)
       AUXI2=A12*DFDX(I) + A22*DFDY(I) + A32*DFDZ(I)
       AUXI3=A13*DFDX(I) + A23*DFDY(I) + A33*DFDZ(I)
C
             DO 107 J=1,I
C
       AUXJ1=A11*DFDX(J) + A21*DFDY(J) + A31*DFDZ(J)
       AUXJ2=A12*DFDX(J) + A22*DFDY(J) + A32*DFDZ(J)
       AUXJ3=A13*DFDX(J) + A23*DFDY(J) + A33*DFDZ(J)
C
          A(1,1,I,J) = A(1,1,I,J) + COEF * (
     &                              AUXI1*DC2*AUXJ1 +
     &                              AUXI2*AUXJ2 +
     &                              AUXI3*AUXJ3
     &                                     )
C
C
          A(1,2,I,J) = A(1,2,I,J) + COEF * (
     &                              AUXI1*DC3*AUXJ2 +
     &                              AUXI2*AUXJ1
     &                                     )
C
C
          A(1,3,I,J) = A(1,3,I,J) + COEF * (
     &                              AUXI1*DC3*AUXJ3 +
     &                              AUXI3*AUXJ1
     &                                     )
C
C
          A(2,2,I,J) = A(2,2,I,J) + COEF * (
     &                              AUXI2*DC2*AUXJ2 +
     &                              AUXI1*AUXJ1 +
     &                              AUXI3*AUXJ3
     &                                     )
C
C
          A(2,3,I,J) = A(2,3,I,J) + COEF * (
     &                              AUXI2*DC3*AUXJ3 +
     &                              AUXI3*AUXJ2
     &                                     )
C
C
          A(3,3,I,J) = A(3,3,I,J) + COEF * (
     &                              AUXI3*DC2*AUXJ3 +
     &                              AUXI1*AUXJ1 +
     &                              AUXI2*AUXJ2
     &                                     )
C
C
          A(2,1,I,J) = A(2,1,I,J) + COEF * (
     &                              AUXJ1*DC3*AUXI2 +
     &                              AUXJ2*AUXI1
     &                                     )
C
C
          A(3,1,I,J) = A(3,1,I,J) + COEF * (
     &                              AUXJ1*DC3*AUXI3 +
     &                              AUXJ3*AUXI1
     &                                     )
C
C
          A(3,2,I,J) = A(3,2,I,J) + COEF * (
     &                              AUXJ2*DC3*AUXI3 +
     &                              AUXJ3*AUXI2
     &                                     )
C
C
107          CONTINUE
C
106        CONTINUE
C
101   CONTINUE
C
           DO 108 I=1,NNO
             A(1,2,I,I) = 0.D0
             A(1,3,I,I) = 0.D0
             A(2,3,I,I) = 0.D0
108        CONTINUE
C
C PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
C
      DO 111 K=1,3
         DO 111 L=1,3
            DO 111 I=1,NNO
                IK = ((3*I+K-4) * (3*I+K-3)) / 2
            DO 111 J=1,I
                IJKL = IK + 3 * (J-1) + L
                ZR(IMATUU+IJKL-1) = A(K,L,I,J)
111          CONTINUE
C
 9999 CONTINUE
      END
