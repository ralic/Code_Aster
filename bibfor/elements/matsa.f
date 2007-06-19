      SUBROUTINE MATSA ( DUDX , SA1 , SA2 )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 27/01/99   AUTEUR D6BHHMA M.ALMIKDAD 
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
C
C
C---- MATRICES
C      SA1 ( 6 , 9 ) =  S  + 1 / 2  A ( DUDX )  DEF TOT
C      SA2 ( 6 , 9 ) =  S  +        A ( DUDX )  DEF DIF
C
C
      IMPLICIT NONE
C
      INTEGER I , J
C
      REAL * 8  DUDX ( 9 )
C
      REAL * 8  S ( 6 , 9 )
      REAL * 8  A ( 6 , 9 )
C
      REAL * 8  SA1 ( 6 , 9 )
C
      REAL * 8  SA2 ( 6 , 9 )
C
CDEB
C
C---- INITIALISATION
C
      CALL R8INIR ( 6 * 9 , 0.D0 , S   , 1 )
C
      CALL R8INIR ( 6 * 9 , 0.D0 , A   , 1 )
C
C---- CONSTRUCTION DE  S    ( 6 , 9 ) 
C
C---- LIGNES 1 2 3 
C
      S ( 1 , 1 )  = 1.D0
      S ( 2 , 5 )  = 1.D0 
      S ( 3 , 9 )  = 1.D0 
C
C---- LIGNES 4 5 6 
C
      S ( 4 , 2 )  = 1.D0
      S ( 4 , 4 )  = 1.D0 
C
      S ( 5 , 3 )  = 1.D0
      S ( 5 , 7 )  = 1.D0 
C
      S ( 6 , 6 )  = 1.D0
      S ( 6 , 8 )  = 1.D0 
C
C---- INITIALISATION
C
      CALL R8INIR ( 6 * 9 , 0.D0 , A , 1 )
C
C---- CONSTRUCTION DE  A    ( 6 , 9 ) 
C
C---- LIGNE 1 
C
      A ( 1 , 1 )  = DUDX ( 1 )
      A ( 1 , 4 )  = DUDX ( 4 )
      A ( 1 , 7 )  = DUDX ( 7 )
C
C---- LIGNE 2 
C
      A ( 2 , 2 )  = DUDX ( 2 )
      A ( 2 , 5 )  = DUDX ( 5 )
      A ( 2 , 8 )  = DUDX ( 8 )
C
C---- LIGNE 3 
C
      A ( 3 , 3 )  = DUDX ( 3 )
      A ( 3 , 6 )  = DUDX ( 6 )
      A ( 3 , 9 )  = DUDX ( 9 )
C
C---- LIGNE 4 
C
      A ( 4 , 1 )  = DUDX ( 2 )
      A ( 4 , 2 )  = DUDX ( 1 )
C
      A ( 4 , 4 )  = DUDX ( 5 )
      A ( 4 , 5 )  = DUDX ( 4 )
C
      A ( 4 , 7 )  = DUDX ( 8 )
      A ( 4 , 8 )  = DUDX ( 7 )
C
C---- LIGNE 5
C
      A ( 5 , 1 )  = DUDX ( 3 )
C
      A ( 5 , 3 )  = DUDX ( 1 )
      A ( 5 , 4 )  = DUDX ( 6 )
C
      A ( 5 , 6 )  = DUDX ( 4 )
      A ( 5 , 7 )  = DUDX ( 9 )
C
      A ( 5 , 9 )  = DUDX ( 7 )
C
C---- LIGNE 6
C
      A ( 6 , 2 )  = DUDX ( 3 )
      A ( 6 , 3 )  = DUDX ( 2 )
C
      A ( 6 , 5 )  = DUDX ( 6 )
      A ( 6 , 6 )  = DUDX ( 5 )
C
      A ( 6 , 8 )  = DUDX ( 9 )
      A ( 6 , 9 )  = DUDX ( 8 )
C
C
C
C --- POUR LA DEFORMATION TOTALE           S + 0.5 A
C --- POUR LA DEFORMATION DIFFERENTIELLE   S +     A
C
      DO 100 J = 1 , 9
         DO 110 I = 1 , 6
C
         SA1 ( I , J ) = S ( I , J ) + 0.5D0 * A ( I , J )
C
         SA2 ( I , J ) = S ( I , J ) +         A ( I , J )
C
 110     CONTINUE
 100  CONTINUE
C
CFIN
C
      END
