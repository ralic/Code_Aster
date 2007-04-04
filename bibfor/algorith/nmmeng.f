      SUBROUTINE NMMENG(FONACT)
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
      IMPLICIT      NONE
      LOGICAL       FONACT(*)

C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME PRINCIPAL)
C
C NETTOYAGE FIN DE MECA_NON_LINE
C      
C ----------------------------------------------------------------------
C
C
C IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
C                         
C ----------------------------------------------------------------------
C
       CHARACTER*24 OPT,K24BID
       CHARACTER*19 K19BID
       INTEGER      IBID
       REAL*8       R8BID
C      
C ----------------------------------------------------------------------
C
C
C --- MENAGE FINAL
C
       CALL DETMAT()
C
C --- SI FETI, NETTOYAGE DES OBJETS TEMPORAIRES
C --- NETTOYAGE DES SD FETI SI NECESSAIRE (SUCCESSION DE CALCULS
C --- DECOUPLES)
C --- ET INITIALISATION NUMERO D'INCREMENT
C
      IF (FONACT(11)) THEN
        OPT='NETTOYAGE_SDT'
        CALL ALFETI(OPT   ,K19BID,K19BID,K19BID,K19BID,
     &              IBID  ,R8BID ,K24BID,R8BID ,IBID  ,
     &              K24BID,K24BID,K24BID,K24BID,IBID  ,
     &              K24BID,K24BID,IBID)
      ENDIF

C   
      END
