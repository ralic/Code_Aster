      INTEGER FUNCTION SDSOLV(VECT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/10/2012   AUTEUR DESOZA T.DESOZA 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE DESOZA T.DESOZA
C
      IMPLICIT NONE
      CHARACTER*5 VECT
C
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE POUR LES SOLVEURS LINEAIRES
C
C RETOURNE LA LONGUEUR FIXE DES VECTEURS DE LA SD SOLVEUR
C
C ----------------------------------------------------------------------
C
C
C IN  VECT   : NOM DU VECTEUR DONT ON VEUT LA DIMENSION
C
C  .
C /!\ PENSER A MODIFIER SD_SOLVEUR.PY (POUR SD_VERI)
C ---
C
C ----------------------------------------------------------------------
C
      INTEGER    ZSLVK   ,ZSLVR   ,ZSLVI
      PARAMETER (ZSLVK=12,ZSLVR=4 ,ZSLVI=8)
C
C ----------------------------------------------------------------------
C

      IF (VECT.EQ.'ZSLVK') THEN
        SDSOLV = ZSLVK
      ELSE IF (VECT.EQ.'ZSLVR') THEN
        SDSOLV = ZSLVR
      ELSE IF (VECT.EQ.'ZSLVI') THEN
        SDSOLV = ZSLVI
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      END
