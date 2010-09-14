      SUBROUTINE APNINV(SDAPPA,POSNOM,QUESTZ,VALI )
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/09/2010   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*19  SDAPPA
      CHARACTER*(*) QUESTZ
      INTEGER       VALI ,POSNOM
C      
C ----------------------------------------------------------------------
C
C ROUTINE APPARIEMENT (UTILITAIRE)
C
C COORDONNEES DU POINT DU NOEUD
C
C ----------------------------------------------------------------------
C
C
C IN  SDAPPA : NOM DE LA SD APPARIEMENT
C IN  POSNOM : POSITION DU NOEUD MAITRE
C IN  QUESTI : QUESTION
C               NMANOM  NOMBRE DE MAILLES ATTACHEES A CE NOEUD
C               JDECIV  DECALAGE POUR TABLEAU DES CONNECTIVITES INVERSES
C OUT VALI   : REPONSE A LA QUESTION
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*24 DEFICO
      INTEGER      NMANOM,IBID,JDECIV
      CHARACTER*24 QUESTI
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()    
C
C --- NOM SD DEFINITION CONTACT
C
      CALL APNOMK(SDAPPA,'DEFICO',DEFICO)
C
C --- INITIALISATIONS
C
      VALI   = 0
      QUESTI = QUESTZ
C      
      IF (QUESTI.EQ.'NMANOM') THEN
        CALL CFNBEN(DEFICO,POSNOM,'CONINV',NMANOM,IBID  )
        VALI   = NMANOM
      ELSEIF (QUESTI.EQ.'JDECIV') THEN
        CALL CFNBEN(DEFICO,POSNOM,'CONINV',IBID  ,JDECIV)
        VALI   = JDECIV                     
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      CALL JEDEMA()
C 
      END
