      SUBROUTINE NMSUIY(SDIMPR,VALR  ,ISUIV )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/09/2012   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      CHARACTER*24 SDIMPR
      REAL*8       VALR
      INTEGER      ISUIV
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (SUIVI_DDL - UTILITAIRE)
C
C ECRITURE DANS LE TABLEAU DE CONVERGENCE
C
C ----------------------------------------------------------------------
C
C
C IN  SDIMPR : SD AFFICHAGE
C IN  VALR   : VALEUR A ECRIRE DANS LE TABLEAU
C I/O ISUIV  : NUMERO COURANT DU SUIVI_DDL
C
C ----------------------------------------------------------------------
C
      CHARACTER*9  TYPCOL
      CHARACTER*1  INDSUI
C
C ----------------------------------------------------------------------
C

C
C --- AFFICHAGE DANS LE TABLEAU
C
      CALL IMPFOI(0,1,ISUIV,INDSUI)
      TYPCOL = 'SUIV_DDL'//INDSUI
      CALL NMIMCR(SDIMPR,TYPCOL,VALR,.TRUE.)
C
C --- SUIVI_DDL SUIVANT
C
      ISUIV = ISUIV + 1
C
      END
