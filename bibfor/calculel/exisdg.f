      LOGICAL FUNCTION EXISDG(DG,CMP)
      IMPLICIT NONE
      INTEGER DG(*),CMP
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C RESPONSABLE                            VABHHTS J.PELLET
C TOLE CRP_6
C
C     INDIQUE L'EXISTENCE D'1 CMP DANS UN DESCRIPTEUR-GRANDEUR DG
C     ------------------------------------------------------------------
C     EXTERNAL:
C     ---------
      INTEGER IAND

C     VARIABLES LOCALES:
C     ------------------
      INTEGER IEC,RESTE,CODE

C DEB-------------------------------------------------------------------

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      IEC = (CMP-1)/30 + 1
      RESTE = CMP - 30* (IEC-1)
      CODE = LSHIFT(1,RESTE)
      EXISDG = IAND(DG(IEC),CODE) .EQ. CODE

      END
