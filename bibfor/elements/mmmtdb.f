      SUBROUTINE MMMTDB(VALR  ,TYPMAT,II    ,JJ    )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
      IMPLICIT NONE
      REAL*8      VALR
      CHARACTER*2 TYPMAT
      INTEGER     II,JJ
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C ROUTINE DE DEBUGGAGE POUR LES TE
C
C ----------------------------------------------------------------------
C
C
C IN  VALR   : VALEUR DE LA COMPOSANTE DANS LA MATRICE
C IN  II     : LIGNE DE LA MATRICE
C IN  JJ     : COLONNE DE LA MATRICE
C IN  TYPMAT : TYPE DE MATRICE PONCTUELLE
C                'XY' AVEC
C                E  - ESCLAVE
C                M  - MAITRE
C                C  - CONTACT
C                F  - FROTTEMENT
C                IJ - MATR_ELEM
C
C ----------------------------------------------------------------------
C
      IF (VALR.NE.0.D0) THEN
        WRITE(6,*) 'MATR-ELEM-'//TYPMAT,'(',II,',',JJ,'): ',VALR 
      ENDIF
      
      END
