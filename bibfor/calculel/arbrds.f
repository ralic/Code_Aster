      SUBROUTINE ARBRDS(NOMARB)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*16 NOMARB
C      
C ----------------------------------------------------------------------
C
C CREATION D'UN ARBRE BSP POUR APPARIEMENT DES MAILLES (EN BOITE) 
C
C DESTRUCTION DE LA SD ARBRE
C
C ----------------------------------------------------------------------
C
C
C IN  NOMARB : NOM DE LA SD ARBRE   
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL JEDETR(NOMARB(1:16)//'.CELL')
      CALL JEDETR(NOMARB(1:16)//'.LIMA')
      CALL JEDETR(NOMARB(1:16)//'.INFO')     
C
      CALL JEDEMA()
      END
