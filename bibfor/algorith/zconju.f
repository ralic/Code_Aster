      SUBROUTINE ZCONJU (ZIN,PREA,PIMA)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C
C  BUT:  RENDRE LES PARTIES REELLE, IMAGINAIRE D'UN COMPLEXE*16
C   ROUTINE PLUS PRECISE QUE LES REAL ET IMAG FORTRAN
C
C  ATTENTION PREA ET PIMA DOIVENT BIEN ETRE DES REAL*8 DANS LA
C   SUBROUTINE APPELANTE
C
C-----------------------------------------------------------------------
C
C ZIN      /I/: COMPLEXE A DECORTIQUER
C PREA     /O/: PARTIE REELLE CORRESPONDANTE
C PIMA     /O/: PARTIE IMAGINAIRE CORRESPONDANTE
C
C-----------------------------------------------------------------------
      COMPLEX*16  ZIN
      REAL*8     PREA,PIMA
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      PREA =  DBLE(ZIN)
      PIMA = DIMAG(ZIN)
C
      END
