      SUBROUTINE ZBINIT(F0,COEF,BEXCLU,DIMMEM,MEM)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/04/2007   AUTEUR ABBAS M.ABBAS 
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
      INTEGER  DIMMEM
      REAL*8   F0,COEF,BEXCLU,MEM(2,DIMMEM)
C      
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (RECH. LINE. - METHODE MIXTE)
C
C INITIALISATION DU COMMON
C      
C ----------------------------------------------------------------------
C       
C   
C IN  F0     : VALEUR DE LA FONCTION EN X0
C IN  COEF   : COEFFICIENT D'AMPLIFICATION POUR LA RECHERCHE DE BORNE
C IN  BEXMIN : DEBUT DE L'INTERVALLE D'EXCLUSION
C IN  BEXMAX : FIN DE L'INTERVALLE D'EXCLUSION
C IN  DIMMEM : NOMBRE MAX DE COUPLES MEMORISES
C IN  MEM    : COUPLES MEMORISES   
C      
C ----------------------------------------------------------------------
C       
      REAL*8  XEXCLU,XNEG  ,XPOS  ,XOPT
      REAL*8  PARMUL,FNEG  ,FPOS  ,FOPT
      INTEGER ITER  ,DIMCPL,NBCPL
      LOGICAL BPOS  ,OPTI      
      COMMON /ZBPAR/ XEXCLU,XNEG  ,XPOS  ,XOPT,
     &               PARMUL,FNEG  ,FPOS  ,FOPT,
     &               ITER  ,DIMCPL,NBCPL ,BPOS,
     &               OPTI     
C       
C ----------------------------------------------------------------------
C 
      IF (F0.GE.0) THEN 
        CALL U2MESS('F','MECANONLINE_81')
      ENDIF
      PARMUL   = COEF
      XEXCLU   = BEXCLU
      DIMCPL   = DIMMEM
      MEM(1,1) = 0.D0
      MEM(2,1) = F0
      NBCPL    = 1
      ITER     = 0
      BPOS     = .FALSE.
      OPTI     = .FALSE.
      FOPT     = 0
      XNEG     = 0.D0
      FNEG     = F0
      
      END 
