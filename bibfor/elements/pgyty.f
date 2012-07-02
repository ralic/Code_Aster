      SUBROUTINE PGYTY (NNO,NPG,DFDE,YTY)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      REAL*8 DFDE(*),YTY(*)
      REAL*8 ENPRIM(3,2)
C ----------------------------------------------------------------------
C     BUT:   POUR LES ELEMENTS DE CABLE, CALCUL DU PRODUIT DE MATRICES
C            YT * Y  Y ETANT LA MATRICES DES DERIVEES DES FONCTIONS DE
C            FORME ENPRIM.
C            LE PRODUIT EST CALCULE AUX POINTS DE GAUSS SUCCESSIFS
C            ET RANGE PAR LIGNES: 1ERE LIGNE, PUIS 2EME LIGNE...
C     IN: NNO  : NOMBRE DE NOEUDS
C         NPG  : NOMBRE DE POINTS DE GAUSS
C         DFDE : DERIVEES DES FONCTIONS DE FORME
C     OUT: YTY
C ----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,II ,J ,JJ ,K ,KI ,NGAUS 
      INTEGER NNO ,NORDRE ,NPG ,NUMERO 
C-----------------------------------------------------------------------
      K = 0
      DO 2 J=1,NPG
        DO 1 I=1,NNO
           K = K + 1
           ENPRIM(I,J) = DFDE(K)
    1   CONTINUE
    2 CONTINUE
C
      NORDRE = 3 * NNO
      NUMERO = -NORDRE
      DO 14 NGAUS=1,NPG
        DO 13 II=1,NNO
          DO 12 KI=1,3
            I = KI + 3*(II-1)
            NUMERO = NUMERO + NORDRE
            DO 11 JJ=1,NNO
              J = KI + 3*(JJ-1)
              YTY(NUMERO + J) = ENPRIM(II,NGAUS) * ENPRIM(JJ,NGAUS)
   11       CONTINUE
   12     CONTINUE
   13   CONTINUE
   14 CONTINUE
      END
