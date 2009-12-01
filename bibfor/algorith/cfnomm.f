      SUBROUTINE CFNOMM(NOMA  ,DEFICO,TYPENT,POSENT,NOMENT,
     &                  CODRET)
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
      IMPLICIT     NONE
      CHARACTER*8  NOMA
      CHARACTER*24 DEFICO
      INTEGER      POSENT
      CHARACTER*8  NOMENT
      CHARACTER*4  TYPENT
      INTEGER      CODRET
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (TOUTES METHODES - UTILITAIRE)
C
C DONNE LE NOM DE L'ENTITE A PARTIR DE SON NUMERO
C
C ----------------------------------------------------------------------
C
C
C IN  NOMA   : NOM DU MAILLAGE
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  POSENT : POSITION DE L'ENTITE DANS LES SD CONTACT
C IN  TYPENT : TYPE D'ENTITE
C                <MAIL>  MAILLE
C                <NOEU>  NOEUD
C OUT NOMENT : NOM DE L'ENTITE
C OUT CODRET : CODE RETOUR
C                -1 ERREUR
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*32 JEXNUM
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      NUMMAI,NUMNOE
      INTEGER      POSMAI,POSNOE
      INTEGER      NBNOEU,NBMAIL
      INTEGER      IRET,CODRE2
      CHARACTER*8  K8BID
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ ()
C
C --- INITIALISATIONS
C
      NOMENT = ' '
      CODRET = 0      
C
C --- DIMENSIONS MAILLAGE
C 
      CALL DISMOI('F'   ,'NB_NO_MAILLA',NOMA ,'MAILLAGE' ,NBNOEU,
     &            K8BID ,IRET)
      CALL DISMOI('F'   ,'NB_MA_MAILLA',NOMA ,'MAILLAGE' ,NBMAIL,
     &            K8BID ,IRET)        
C
C --- PREPARATION DES CHAINES POUR LES NOMS
C
      IF (TYPENT.EQ.'MAIL') THEN  
        POSMAI = POSENT
        CALL CFPOSM(NOMA  ,DEFICO,TYPENT,1     ,POSMAI,
     &              NUMMAI,CODRE2)
        IF (CODRE2.EQ.-1) THEN
          CODRET = -1
          GOTO 999
        ELSE
          IF ((NUMMAI.GT.NBMAIL).OR.(NUMMAI.LE.0)) THEN
            CODRET = -1
            GOTO 999
          ELSE  
            CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMMAI),NOMENT)
          ENDIF 
        ENDIF
            
      ELSE IF (TYPENT.EQ.'NOEU') THEN    
        POSNOE = ABS(POSENT)
        CALL CFPOSM(NOMA  ,DEFICO,TYPENT,1     ,POSNOE,
     &              NUMNOE,CODRE2)
        IF (CODRE2.EQ.-1) THEN
          CODRET = -1
          GOTO 999
        ELSE
          IF ((NUMNOE.GT.NBNOEU).OR.(NUMNOE.LE.0)) THEN
            CODRET = -1
            GOTO 999
          ELSE  
            CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUMNOE),NOMENT)
          ENDIF 
        ENDIF  
      ELSE 
        CODRET = -1
      ENDIF
C
  999 CONTINUE      
C
      CALL JEDEMA()
C
      END
