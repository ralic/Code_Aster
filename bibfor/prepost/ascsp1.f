      SUBROUTINE ASCSP1 ( UN,TYPELE,NIVMAG )
      IMPLICIT   NONE
      INTEGER      UN
      CHARACTER*4  TYPELE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 27/03/2002   AUTEUR F1BHHAJ J.ANGLES 
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
C TOLE  CRP_20
C     MACR_ASCOUF_MAIL
C
C     ECRIT DANS UN FICHIER  LES DONNEES GIBI DE LA PROCEDURE
C                                                "PLAQUE SOUS-EPAISSEUR"
C     PREMIERE PARTIE ( AVANT LES DONNEES UTILISATEUR )
C
C     ------------------------------------------------------------------
C 
      CHARACTER*128 NOMREP
      INTEGER       LNOM,NIVMAG   
C   
      WRITE (UN,*) 'nivmag   = ',NIVMAG,';'
      CALL ASPECR(UN,
     +' option dime 3 elem '//TYPELE//' nive nivmag echo 0;')
      CALL ASPECR(UN,
     +'*')
      CALL ASPECR(UN,
     +'bg = table;')
      CALL ASPECR(UN,
     +'bd = table;')
      CALL ASPECR(UN,
     +'bi = table;')
      CALL ASPECR(UN,
     +'bs = table;')
      CALL ASPECR(UN,
     +'indbg  = table;')
      CALL ASPECR(UN,
     +'indbd  = table;')
      CALL ASPECR(UN,
     +'indbi  = table;')
      CALL ASPECR(UN,
     +'indbs  = table;')
      CALL ASPECR(UN,
     +'axecir = table;')
      CALL ASPECR(UN,
     +'axelon = table;')
      CALL ASPECR(UN,
     +'axelonc = table;')
      CALL ASPECR(UN,
     +'coorzc = table;')
      CALL ASPECR(UN,
     +'prof = table;')
      CALL ASPECR(UN,
     +'posit = table;')
      CALL ASPECR(UN,
     +'coory = table;')
      CALL ASPECR(UN,
     +'coorz = table;')
      CALL ASPECR(UN,
     +'deny = table;')
      CALL ASPECR(UN,
     +'nbely = table;')
      CALL ASPECR(UN,
     +'denz = table;')
      CALL ASPECR(UN,
     +'nbelz = table;')
      CALL ASPECR(UN,
     +'axisym = table;')
      CALL ASPECR(UN,
     +'sousep = table;')
      CALL ASPECR(UN,
     +'*')
C
      CALL REPDEX(1,LNOM,NOMREP)
      WRITE (UN,10)
     +'opti donn '
      WRITE (UN,*)
     + ' '''//NOMREP(1:LNOM)//'ascouf_ssep_mult_v1.datg''; '
C      WRITE (UN,10)
C     +' ''/exterieurs/aubhhmb/datg/ascouf_ssep_mult_v1.datg'';'
C     
 10   FORMAT(T1,A)
C    
      END
