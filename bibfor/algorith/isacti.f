      SUBROUTINE ISACTI(SDDISC,ACTIOZ,IEVDAC)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/04/2012   AUTEUR ABBAS M.ABBAS 
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
      IMPLICIT      NONE
      CHARACTER*19  SDDISC
      CHARACTER*(*) ACTIOZ
      INTEGER       IEVDAC
C
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE EVENEMENT
C
C DIT SI UN EVENEMENT EST TRAITE
C
C ----------------------------------------------------------------------
C
C
C IN  SDDISC : SD DISCRETISATION TEMPORELLE
C IN  ACTION : ACTION A CHERCHER
C OUT LACTI  : .TRUE. SI TRAITE
C              .FALSE. SINON
C
C ----------------------------------------------------------------------
C
      INTEGER      IBID,IEVEN,NEVEN
      REAL*8       R8BID
      CHARACTER*16 ACTION,ACT,K16BID
C
C ----------------------------------------------------------------------
C
      IEVDAC = 0
      ACTION = ACTIOZ
      CALL UTDIDT('L'   ,SDDISC,'LIST',IBID  ,'NECHEC',
     &            R8BID ,NEVEN,K16BID )

      DO 10 IEVEN = 1,NEVEN
        CALL UTDIDT('L'   ,SDDISC,'ECHE',IEVEN,'ACTION'  ,
     &              R8BID ,IBID  ,ACT   )
        IF (ACT.EQ.ACTION) THEN
          IEVDAC = IEVEN
        ENDIF
  10  CONTINUE
C
      END
