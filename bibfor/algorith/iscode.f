      SUBROUTINE ISCODE (IDEC,ICOD,NDIM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/11/98   AUTEUR CIBHHGB G.BERTRAND 
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
C  BUT: CODER UN ENTIER CODER SUR LES 30 PREMIERES PUISSANCES
      IMPLICIT REAL*8 (A-H,O-Z)
C          DE DEUX ( PAS DE PUISSANCE 0)
C-----------------------------------------------------------------------
C
C  IDEC     /O/: VECTEUR DES 30 PREMIERES PUISSANCES DE DEUX
C  ICOD(1)  /I/: ENTIER1 A DECODER
C  NDIM     /I/: NOMBRE DE PUISSANCE A DECODER
C
C-----------------------------------------------------------------------
C
      INTEGER IDEC(NDIM), ICOD(1)
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
      NEC = (NDIM-1)/30 + 1
      DO 10 IEC = 1, NEC
        ICOD(IEC)=0
 10   CONTINUE
      IPUI=1
      DO 20 I=1,NDIM
        IEC = (I-1)/30 + 1
        IPUI=IPUI*2
        ICOD(IEC)=ICOD(IEC)+IDEC(I)*IPUI
 20   CONTINUE
C
 9999 CONTINUE
      END
