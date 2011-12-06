      SUBROUTINE NMDIDE(LREUSE,RESULT,NUMDER,INSDER)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/02/2011   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE 
      CHARACTER*8  RESULT
      LOGICAL      LREUSE
      INTEGER      NUMDER
      REAL*8       INSDER
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (STRUCTURES DE DONNES - DISCRETISATION)
C
C DERNIER NUMERO ARCHIVE DANS LA SD SI ETAT_INIT
C
C ----------------------------------------------------------------------
C
C
C IN  RESULT : NOM DE LA SD RESULTAT
C IN  LREUSE : .TRUE. SI CONCEPT REENTRANT
C OUT NUMDER : DERNIER NUMERO ARCHIVE DANS RESULT 
C               (OU 0 SI NON REENTRANT)
C OUT INSDER : DERNIER INSTANT ARCHIVE DANS RESULT 
C               (R8VIDE SI NON REENTRANT)
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER      ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8       ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16   ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL      ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8  ZK8
      CHARACTER*16    ZK16
      CHARACTER*24        ZK24
      CHARACTER*32            ZK32
      CHARACTER*80                ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      IBID,JINST
      CHARACTER*8  K8BID
      COMPLEX*16   C16BID
      REAL*8       R8BID,R8VIDE
C      
C ----------------------------------------------------------------------
C      
      CALL JEMARQ() 
C
C --- INITIALISATIONS
C
      NUMDER = 0
      INSDER = R8VIDE()
C
C --- ACCES SD RESULTAT (OU PAS)
C
      IF (LREUSE) THEN
        CALL RSORAC(RESULT,'DERNIER',IBID   ,R8BID ,K8BID,
     &              C16BID,0.D0    ,'ABSOLU',NUMDER,1    ,
     &              IBID  )
        CALL RSADPA(RESULT,'L',1,'INST',NUMDER,0,JINST,K8BID)
        INSDER = ZR(JINST)
      ELSE
        NUMDER = 0
        INSDER = R8VIDE()   
      ENDIF
C
      CALL JEDEMA()
      END
