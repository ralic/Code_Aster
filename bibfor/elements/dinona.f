      SUBROUTINE DINONA(NOMTE,RAIDE,KLV)
C ----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*16 NOMTE
      REAL*8       KLV(*),RAIDE(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/12/2006   AUTEUR VOLDOIRE F.VOLDOIRE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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

C ======================================================================
C           ACTUALISATION DE LA MATRICE QUASI-TANGENTE
C
C  LES TERMES DIAGONAUX SONT LES SEULS A ETRE ADAPTES
C
C ======================================================================
C
C  IN
C     NOMTE : NOM DE L'ELEMENT
C     RAIDE : RAIDEUR QUASI-TANGENTE
C  OUT
C     KLV   : MATRICE DE RAIDEUR
C
C ======================================================================
      
      IF      (NOMTE.EQ.'MECA_DIS_TR_L') THEN
         KLV(1)  =  RAIDE(1)
         KLV(3)  =  RAIDE(2)
         KLV(6)  =  RAIDE(3)
         KLV(10) =  RAIDE(4)
         KLV(15) =  RAIDE(5)
         KLV(21) =  RAIDE(6)
         KLV(28) =  RAIDE(1)
         KLV(36) =  RAIDE(2)
         KLV(45) =  RAIDE(3)
         KLV(55) =  RAIDE(4)
         KLV(66) =  RAIDE(5)
         KLV(78) =  RAIDE(6)
         KLV(22) = -RAIDE(1)
         KLV(30) = -RAIDE(2)
         KLV(39) = -RAIDE(3)
         KLV(49) = -RAIDE(4)
         KLV(60) = -RAIDE(5)
         KLV(72) = -RAIDE(6)
      ELSE IF (NOMTE.EQ.'MECA_DIS_TR_N') THEN
         KLV(1)  =  RAIDE(1)
         KLV(3)  =  RAIDE(2)
         KLV(6)  =  RAIDE(3)
         KLV(10) =  RAIDE(4)
         KLV(15) =  RAIDE(5)
         KLV(21) =  RAIDE(6)
      ELSE IF (NOMTE.EQ.'MECA_DIS_T_L') THEN
         KLV(1)  =  RAIDE(1)
         KLV(3)  =  RAIDE(2)
         KLV(6)  =  RAIDE(3)
         KLV(10) =  RAIDE(1)
         KLV(15) =  RAIDE(2)
         KLV(21) =  RAIDE(3)
         KLV(7)  = -RAIDE(1)
         KLV(12) = -RAIDE(2)
         KLV(18) = -RAIDE(3)
      ELSE IF (NOMTE.EQ.'MECA_DIS_T_N') THEN
         KLV(1)  =  RAIDE(1)
         KLV(3)  =  RAIDE(2)
         KLV(6)  =  RAIDE(3)
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_T_L') THEN
         KLV(1)  =  RAIDE(1)
         KLV(3)  =  RAIDE(2)
         KLV(6)  =  RAIDE(1)
         KLV(10) =  RAIDE(2)
         KLV(4)  = -RAIDE(1)
         KLV(8)  = -RAIDE(2)
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_T_N') THEN
         KLV(1)  =  RAIDE(1)
         KLV(3)  =  RAIDE(2)
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_TR_L') THEN
         KLV(1)  =  RAIDE(1)
         KLV(3)  =  RAIDE(2)
         KLV(6)  =  RAIDE(3)
         KLV(10) =  RAIDE(1)
         KLV(15) =  RAIDE(2)
         KLV(21) =  RAIDE(3)
         KLV(7)  = -RAIDE(1)
         KLV(12) = -RAIDE(2)
         KLV(18) = -RAIDE(3)
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_TR_N') THEN
         KLV(1)  =  RAIDE(1)
         KLV(3)  =  RAIDE(2)
         KLV(6)  =  RAIDE(3)
      ENDIF

      END
