      SUBROUTINE DILPEN(IMATE,RPENA)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C ======================================================================
      IMPLICIT  NONE
      INTEGER   IMATE
      REAL*8    RPENA
C ======================================================================
C --- BUT : RECUPERATION DU COEFFICIENT DE PENALISATION ----------------
C ======================================================================
      REAL*8       VAL
      CHARACTER*2  CODRET
      CHARACTER*8  NCRA
C ======================================================================
C --- DEFINITION DES DONNEES INITIALES ---------------------------------
C ======================================================================
      DATA NCRA  / 'PENA_LAG' /
      VAL   = 0.0D0
      CALL RCVALA(IMATE,' ', 'NON_LOCAL', 0, ' ', 0.0D0,
     +                                          1, NCRA,VAL,CODRET,' ')
      RPENA = VAL
C ======================================================================
      END
