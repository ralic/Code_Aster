      SUBROUTINE EFP2P1(NOMTE, NPG, VFF2, DFF2, VFF1, DFF1, NNO1)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 28/08/2000   AUTEUR GJBHHEL E.LORENTZ 
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
      CHARACTER*16 NOMTE
      INTEGER      NPG, NNO1
      REAL*8       VFF2, DFF2, VFF1, DFF1

C ----------------------------------------------------------------------
C      CALCUL DES FONCTIONS DE FORME P1 A PARTIR DES FONCTIONS P2
C ----------------------------------------------------------------------
C IN  NOMTE  NOM DE L'ELEMENT
C IN  NPG    NOMBRE DE POINTS DE GAUSS
C IN  VFF2   VALEUR  DES FONCTIONS DE FORME P2
C IN  DFF2   DERIVEE DES FONCTIONS DE FORME P2
C OUT VFF1   VALEUR  DES FONCTIONS DE FORME P1
C OUT DFF1   DERIVEE DES FONCTIONS DE FORME P1
C OUT NNO1   NOMBRE DE NOEUDS P1
C ----------------------------------------------------------------------

      INTEGER  A(8,3), NNO2


      IF (NOMTE(5:7) .EQ. 'TR6') THEN
        NNO2   = 6
        NNO1   = 3

        A(1,1) = 4
        A(1,2) = 6
        A(2,1) = 4
        A(2,2) = 5
        A(3,1) = 5
        A(3,2) = 6

        CALL EF212D(NPG,A,NNO2,VFF2,DFF2,NNO1,VFF1,DFF1)


      ELSE IF (NOMTE(5:7) .EQ. 'QS8' .OR.
     &         NOMTE(5:7) .EQ. 'QU8') THEN
        NNO2 = 8
        NNO1 = 4

        A(1,1) = 5
        A(1,2) = 8
        A(2,1) = 6
        A(2,2) = 5
        A(3,1) = 7
        A(3,2) = 6
        A(4,1) = 8
        A(4,2) = 7

        CALL EF212D(NPG,A,NNO2,VFF2,DFF2,NNO1,VFF1,DFF1)


      ELSE IF (NOMTE(5:11) .EQ. '_HEXA20' .OR.
     &         NOMTE(5:11) .EQ. '_HEXS20' ) THEN

        NNO2 = 20
        NNO1 = 8

        A(1,1) = 9
        A(1,2) = 12
        A(1,3) = 13
        A(2,1) = 9
        A(2,2) = 10
        A(2,3) = 14
        A(3,1) = 10
        A(3,2) = 11
        A(3,3) = 15
        A(4,1) = 11
        A(4,2) = 12
        A(4,3) = 16
        A(5,1) = 13
        A(5,2) = 17
        A(5,3) = 20
        A(6,1) = 14
        A(6,2) = 17
        A(6,3) = 18
        A(7,1) = 15
        A(7,2) = 18
        A(7,3) = 19
        A(8,1) = 16
        A(8,2) = 19
        A(8,3) = 20

        CALL EF213D(NPG,A,NNO2,VFF2,DFF2,NNO1,VFF1,DFF1)


      ELSE IF (NOMTE(5:12) .EQ. '_PENTA15') THEN
        NNO2 = 15
        NNO1 = 6

        A(1,1) = 7
        A(1,2) = 9
        A(1,3) = 10
        A(2,1) = 7
        A(2,2) = 8
        A(2,3) = 11
        A(3,1) = 8
        A(3,2) = 9
        A(3,3) = 12
        A(4,1) = 10
        A(4,2) = 13
        A(4,3) = 15
        A(5,1) = 11
        A(5,2) = 13
        A(5,3) = 14
        A(6,1) = 12
        A(6,2) = 14
        A(6,3) = 15

        CALL EF213D(NPG,A,NNO2,VFF2,DFF2,NNO1,VFF1,DFF1)


      ELSE IF (NOMTE(5:12) .EQ. '_TETRA10') THEN
        NNO2 = 10
        NNO1 = 4

        A(1,1) = 5
        A(1,2) = 7
        A(1,3) = 8
        A(2,1) = 5
        A(2,2) = 6
        A(2,3) = 9
        A(3,1) = 6
        A(3,2) = 7
        A(3,3) = 10
        A(4,1) = 8
        A(4,2) = 9
        A(4,3) = 10

        CALL EF213D(NPG,A,NNO2,VFF2,DFF2,NNO1,VFF1,DFF1)


      ELSE
        CALL UTMESS('F','EFP2P1','ELEMENT NON PREVU')
      END IF

      END
