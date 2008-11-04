      SUBROUTINE MMMAB0 (NBDM,NBCPS,NDIM,NNE,HPG,FFPC,JACOBI,
     &                   TAU1,TAU2, MMAT)     
      IMPLICIT NONE
      INTEGER  NDIM,NNE,NBDM,NBCPS
      REAL*8   HPG,FFPC(9),JACOBI,TAU1(3),TAU2(3),MMAT(81,81)  
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/11/2008   AUTEUR DESOZA T.DESOZA 
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
C ROUTINE APPELLEE PAR : TE0364
C ----------------------------------------------------------------------
C
C CALCUL DE B ET DE BT POUR LE CONTACT METHODE CONTINUE
C CAS SANS CONTACT
C
C IN  NBDM   : NB DE DDL DE LA MAILLE ESCLAVE
C IN  NBCPS  : NB DE DDL DE LAGRANGE
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
C IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFPC   : FONCTIONS DE FORME DU POINT DE CONTACT
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  TAU1   : PREMIER VECTEUR TANGENT
C IN  TAU2   : DEUXIEME VECTEUR TANGENT
C I/O MMAT   : MATRICE ELEMENTAIRE DE CONTACT/FROTTEMENT
C ----------------------------------------------------------------------
      INTEGER   I, J, K, L, II, JJ
      REAL*8    TT(3,3)
C ----------------------------------------------------------------------
C
C --- INITIALISATIONS
C
      DO 300 I = 1,3
        DO 290 J = 1,3
          TT(I,J)  = 0.D0
  290   CONTINUE
  300 CONTINUE         
C
C --- MATRICE 
C
      DO 303 I = 1,NDIM
        DO 302 J = 1,NDIM
          DO 301 K = 1,NDIM
            TT(1,1) = TAU1(K)*TAU1(K) + TT(1,1)
            TT(1,2) = TAU1(K)*TAU2(K) + TT(1,2)
            TT(2,1) = TAU2(K)*TAU1(K) + TT(2,1)
            TT(2,2) = TAU2(K)*TAU2(K) + TT(2,2)
 301      CONTINUE
 302    CONTINUE
 303  CONTINUE
      DO 284 I = 1,NNE
        DO 283 J = 1,NNE
          DO 282 L = 1,NBCPS-1
            DO 281 K = 1,NBCPS-1
              II = NBDM*(I-1)+NDIM+1+L
              JJ = NBDM*(J-1)+NDIM+1+K
              MMAT(II,JJ) = -HPG*FFPC(I)*FFPC(J)*JACOBI*TT(L,K)
 281        CONTINUE
 282      CONTINUE
 283    CONTINUE
 284  CONTINUE
C
      END
