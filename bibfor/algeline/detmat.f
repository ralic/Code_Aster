      SUBROUTINE DETMAT()
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 01/04/2005   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT   NONE
C ----------------------------------------------------------
C BUT : DETRUIRE TOUTES LES MATR_ASSE PRESENTES SUR VOLATILE
C       DETRUIT AUSSI LES EVENTUELLES INSTANCES MUMPS
C ----------------------------------------------------------
C RESPONSABLE VABHHTS J.PELLET
      INTEGER      NBMAT,I
      CHARACTER*24 LIREFA(100)
C------------------------------------------------------------------
      CALL JELSTC ('V', '.REFA' , 20 , 100 , LIREFA , NBMAT )
      CALL ASSERT(NBMAT.GE.0)

      DO 1,I=1,NBMAT
         CALL DETRSD('MATR_ASSE',LIREFA(I))
 1    CONTINUE
      END
