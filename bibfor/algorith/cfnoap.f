      SUBROUTINE CFNOAP(NOMA  ,DEFICO,TYPAPP,ENTAPP,NOMAPP,
     &                  TYPE2 )
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
      IMPLICIT     NONE
      CHARACTER*24 DEFICO
      CHARACTER*8  NOMA
      INTEGER      ENTAPP,TYPAPP
      CHARACTER*8  NOMAPP
      CHARACTER*4  TYPE2
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE DISCRETE - APPARIEMENT - UTILITAIRE)
C
C NOM DE L'ENTITE APPARIEE
C
C ----------------------------------------------------------------------
C
C
C IN  DEFICO : SD DE DEFINITION DU CONTACT
C IN  NOMA   : NOM DU MAILLAGE
C IN  TYPAPP : TYPE D'ENTITE APAPRIEE
C IN  ENTAPP : POSITION DE L'ENTITE APPARIE DANS SD_CONTACT
C OUT NOMAPP : NOM DE L'ENTITE APPARIEE
C OUT TYPE2  : TYPE D'APPARIEMENT
C                TYPE2  = ' NON'
C                TYPE2  = '/ND '
C                TYPE2  = '/EL '
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
      INTEGER      POSNOM,POSMAM 
      CHARACTER*8  NOMNOM,NOMMAM
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C ----- NOM ET TYPE (MAILLE OU NOEUD) DU MAITRE
C
      IF (TYPAPP.LT.0) THEN
        TYPE2  = ' NON'
        NOMAPP = ' APPARIE'
      ELSEIF (TYPAPP.EQ.1) THEN
        POSNOM = ENTAPP
        CALL CFNOMM(NOMA  ,DEFICO,'NOEU',POSNOM,NOMNOM)  
        TYPE2  = '/ND '
        NOMAPP = NOMNOM
      ELSEIF (TYPAPP.EQ.2) THEN
        POSMAM = ENTAPP
        CALL CFNOMM(NOMA  ,DEFICO,'MAIL',POSMAM,NOMMAM)
        TYPE2  = '/EL '
        NOMAPP = NOMMAM
      ELSE
        CALL ASSERT(.FALSE.)  
      ENDIF
C
      CALL JEDEMA()
C 
      END
