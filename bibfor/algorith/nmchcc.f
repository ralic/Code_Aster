      SUBROUTINE NMCHCC(FONACT,DEFICO,NBMATR,LTYPMA,LOPTME,
     &                  LOPTMA,LASSME,LCALME)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*24 DEFICO
      LOGICAL      FONACT(*)
      INTEGER      NBMATR
      CHARACTER*6  LTYPMA(20)
      CHARACTER*16 LOPTME(20),LOPTMA(20)
      LOGICAL      LASSME(20),LCALME(20)          
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
C
C AJOUTER LES MATR_ELEM CONTACT/XFEM
C      
C ----------------------------------------------------------------------
C
C
C IN  DEFICO : SD DEF. CONTACT
C IN  FONACT : FONCTIONNALITES ACTIVEES
C I/O NBMATR : NOMBRE DE MATR_ELEM DANS LA LISTE
C I/O LTYPMA : LISTE DES NOMS DES MATR_ELEM
C I/O LOPTME : LISTE DES OPTIONS DES MATR_ELEM 
C I/O LOPTMA : LISTE DES OPTIONS DES MATR_ASSE
C I/O LCALME : SI MATR_ELEM A CALCULER
C I/O LASSME : SI MATR_ELEM A ASSEMBLER
C      
C ----------------------------------------------------------------------
C 
      LOGICAL      ISFONC,LCTCC,LXFCM,LTFCM
      CHARACTER*24 K24BLA,K24BID
      REAL*8       R8BID
      INTEGER      IBID
C      
C ----------------------------------------------------------------------
C   
C
C --- INITIALISATIONS
C
      K24BLA = ' ' 
C
C --- FONCTIONNALITES ACTIVEES
C
      LCTCC  = ISFONC(FONACT,'CONT_CONTINU')
      LXFCM  = ISFONC(FONACT,'CONT_XFEM') 
      LTFCM = .FALSE.
      IF (LXFCM) THEN
        CALL MMINFP(0    ,DEFICO,K24BLA,'XFEM_GG',
     &              IBID ,R8BID ,K24BID,LTFCM)
      ENDIF      
C
C --- CONTACT METHODE CONTINU
C
      IF ((LCTCC).AND.(.NOT.LXFCM)) THEN
        CALL NMCMAT('AJOU','MECTCC',' '   ,' '   ,.TRUE.,
     &              .FALSE.,NBMATR    ,LTYPMA,LOPTME,LOPTMA,
     &               LCALME,LASSME)       
        CALL NMCMAT('AJOU','MECTCF',' '   ,' '   ,.TRUE.,
     &              .FALSE.,NBMATR    ,LTYPMA,LOPTME,LOPTMA,
     &               LCALME,LASSME)        
      ENDIF   
C
C --- CONTACT XFEM
C
      IF (LXFCM) THEN            
        IF (LTFCM) THEN
          CALL NMCMAT('AJOU','MEXFTC',' '  ,' '   ,.TRUE.,
     &                .FALSE.,NBMATR    ,LTYPMA,LOPTME,LOPTMA,
     &                 LCALME,LASSME)         
          CALL NMCMAT('AJOU','MEXFTF',' '  ,' '   ,.TRUE.,
     &                .FALSE.,NBMATR    ,LTYPMA,LOPTME,LOPTMA,
     &                 LCALME,LASSME)   
        ELSE
          CALL NMCMAT('AJOU','MEXFEC',' '   ,' '   ,.TRUE.,
     &                .FALSE.,NBMATR    ,LTYPMA,LOPTME,LOPTMA,
     &                 LCALME,LASSME)        
          CALL NMCMAT('AJOU','MEXFEF',' '   ,' '   ,.TRUE.,
     &                .FALSE.,NBMATR    ,LTYPMA,LOPTME,LOPTMA,
     &                 LCALME,LASSME)        
        ENDIF      
      ENDIF       
C
      END
