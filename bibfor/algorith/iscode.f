      SUBROUTINE ISCODE (IDEC,ICOD,NDIM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/12/2002   AUTEUR CIBHHAB S.VANDENBERGHE 
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
C***********************************************************************
C    P. RICHARD     DATE 18/02/90
C-----------------------------------------------------------------------
C  BUT: CODER UN ENTIER CODE SUR LES 30 PREMIERES PUISSANCES
C          DE DEUX ( PAS DE PUISSANCE 0)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C
C  IDEC     /I/: VECTEUR DES NDIM PREMIERES CMPS
C  ICOD(*)  /O/: ENTIER CODE :
C                ICOD(1) : 30 1ERES CMPS CODE SUR LES PUISS DE 2:1 A 30
C                ICOD(2) : 30 CMPS SUIV CODE SUR LES PUISS DE 2:1 A 30
C                ...
C  NDIM     /I/: NOMBRE DE CMPS A DECODER
C
C-----------------------------------------------------------------------
C
      INTEGER NDIM, NECMAX
      INTEGER IDEC(NDIM), ICOD(*)
      INTEGER NEC, IEC, I, IPUI, K
      PARAMETER (NECMAX = 10)
      INTEGER IFIN(NECMAX)
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C --- IFIN DONNE POUR CHAQUE ENTIER CODE LE NOMBRE MAX DE CMPS
C --- QUE L'ON PEUT TROUVER SUR CET ENTIER :
C     ------------------------------------
      NEC = (NDIM-1)/30 + 1
      DO 10 IEC = 1, NEC
        ICOD(IEC)=0
        IFIN(IEC)=30
 10   CONTINUE
      IFIN(NEC)=NDIM - 30*(NEC-1)
C
      K = 0
      DO 20 IEC=1,NEC
        IPUI = 1
        DO 30 I=1,IFIN(IEC)
          K = K+1
          IPUI = IPUI*2
          ICOD(IEC)=ICOD(IEC)+IDEC(K)*IPUI
 30     CONTINUE
 20   CONTINUE
C
      END
