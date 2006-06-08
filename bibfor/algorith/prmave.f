      SUBROUTINE PRMAVE(IPR,AMAT,NA,NA1,NA2,BVEC,NB1,CVEC,NC1,IER)
C
C ********************************************************************
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/09/98   AUTEUR KXBADNG F.BEAUD 
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
C DESCRIPTION : PRODUIT  MATRICE VECTEUR
C ------------      C = A * B  
C
C ****************** DECLARATION DES VARIABLES ***********************
C
      IMPLICIT NONE
C
C ARGUMENTS
C ---------
      INTEGER NA
      INTEGER NA1, NA2, NB1, NC1,IPR, IER
      REAL*8  AMAT(NA,*),BVEC(*),CVEC(*)    
C
C VARIABLES LOCALES
C -----------------
      INTEGER I, J
C
C ****************** DEBUT DU CODE EXECUTABLE ************************
C
      IER=0
C
        IF(NA2.NE.NB1) THEN
          IER = IER + 1
        ENDIF
        IF(NC1.NE.NA1) THEN
          IER = IER + 1
        ENDIF
C
        IF (IER .EQ. 0) THEN
C
         IF (IPR .EQ. 0) THEN
        DO 10 I=1,NA1
            CVEC(I)=0.0D0
 10     CONTINUE
         ENDIF
C
        DO 100 I=1,NA1
          DO 200 J=1,NA2
            CVEC(I)=CVEC(I)+AMAT(I,J)*BVEC(J)
 200      CONTINUE
 100    CONTINUE
C
        ENDIF
C
      END
