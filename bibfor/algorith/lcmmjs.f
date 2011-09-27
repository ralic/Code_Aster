        SUBROUTINE LCMMJS(NBSYS,TBSYS)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/09/2011   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PROIX J-M.PROIX
C      ----------------------------------------------------------------
C     MONOCRISTAL : RECUPERATION DES SYSTEMES DE GLISSEMENT UTILISATEUR
C       ----------------------------------------------------------------
      INTEGER ITBSYS,I,J,NBSYS
      REAL*8 TBSYS(30,6),TBSYSG
      COMMON/TBSYSG/TBSYSG(182)
C     ----------------------------------------------------------------
C
C -   NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
C
      ITBSYS=NINT(TBSYSG(1))
      CALL ASSERT(ITBSYS.EQ.1)
      NBSYS=NINT(TBSYSG(2))
      DO 2 I=1,NBSYS
      DO 2 J=1,6
         TBSYS(I,J)=TBSYSG(2+6*(I-1)+J)
 2    CONTINUE 
 
      END
