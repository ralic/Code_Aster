      SUBROUTINE ISDECO (ICOD,IDEC,NDIM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 01/03/2000   AUTEUR DURAND C.DURAND 
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
C    P. RICHARD     DATE 06/11/90
C-----------------------------------------------------------------------
C  BUT: DECODER UN ENTIER CODER SUR LES 30  PREMIERES PUISSANCES
      IMPLICIT REAL*8 (A-H,O-Z)
C          DE DEUX
C
C***********************************************************************
C  ATTENTION LE TERME I DU VECTEUR IDEC CORRESPOND A LA PUISSANCE I
C   PAS DE DECODAGE SUR PUISSANCE 0
C **********************************************************************
C
C-----------------------------------------------------------------------
C
C NOM----- / /: DEFINITION
C
C  ICOD    /I/: ENTIER A DECODER
C  IDEC    /O/: VECTEUR PREMIERES PUISSANCES DE DEUX
C  NEC     /I/: NOMBRE D'ENTIERS COES SUR LESQUELS TIENT LA GRANDEUR
C  NDIM    /I/: NOMBRE DE PUISSANEC A DECODER
C
C-----------------------------------------------------------------------
C
      INTEGER ICOD(1)
      INTEGER IDEC(NDIM)
      PARAMETER (NBECMX = 10)
      INTEGER IFIN(NBECMX)
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
       NEC = (NDIM-1)/30 + 1
C
C --- IFIN DONNE POUR CHAQUE ENTIER CODE LE NOMBRE MAX DE DDLS
C --- QUE L'ON PEUT TROUVER SUR CET ENTIER :
C     ------------------------------------
      DO 10 IEC = 1, NEC-1
          IFIN(IEC) = 30
 10   CONTINUE
      IFIN(NEC) = NDIM - 30*(NEC-1)
C
      K = 0
      DO 20 IEC = 1, NEC
        XCOU=ICOD(IEC)
        X=XCOU/2.D0
        IE=INT(X)
        V=X-IE
        XCOU=X-V
        DO 30 I=1,IFIN(IEC)
          K = K + 1
          X=XCOU/2.D0
          IE=INT(X)
          V=X-IE
          IDEC(K)=INT(V*2+1.D-2)
          XCOU=X-V
 30     CONTINUE
 20   CONTINUE
C
      END
