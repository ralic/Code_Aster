      SUBROUTINE PROJVD(TESTC,NP1,NB1,NB2,MAT,U,V)
C
C ********************************************************************
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/05/96   AUTEUR KXBADNG T.FRIOU 
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
C ********************************************************************
C DESCRIPTION : PROJECTION DE LA MATRICE MAT SUR LE VECTEUR U   
C ------------ 
C
C ****************** DECLARATION DES VARIABLES ***********************
C
      IMPLICIT NONE
C
C ARGUMENTS
C ---------
      INTEGER  TESTC, NP1, NB1 ,NB2
      REAL*8 MAT(NP1,*), U(*), V(*)
C
C VARIABLES LOCALES
C -----------------
      INTEGER I, IER
C
C ****************** DEBUT DU CODE EXECUTABLE ************************
C
      IF (TESTC .EQ. 1) THEN
C
        IER = 0
        CALL PRMAVE(0,MAT,NP1,NB1,NB2,U,NB2,V,NB1,IER)
        IF (IER .NE. 0) THEN
          CALL UTMESS('F','PROJVD','TAILLE PRODUIT MATRICE-VECTEUR'//
     &                ' INCOMPATIBLE')
        ENDIF
C
      ELSE
C 
       IF (NB1 .EQ. NB2) THEN
        DO 100 I=1,NB1
          V(I) = U(I)
 100    CONTINUE   
C
       ELSE IF (NB1 .LT. NB2) THEN
        DO 200 I=1,NB1
          V(I) = U(I)
 200    CONTINUE   
        DO 300 I=NB1+1,NB2
          V(I) = 0.0D0
 300    CONTINUE   
C
       ELSE IF (NB2 .LT. NB1) THEN
        DO 400 I=1,NB2
          V(I) = U(I)
 400    CONTINUE   
        DO 500 I=NB2+1,NB1
          V(I) = 0.0D0
 500    CONTINUE   
       ENDIF
C
      ENDIF
C
      END
