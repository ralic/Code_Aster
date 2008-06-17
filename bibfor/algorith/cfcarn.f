      SUBROUTINE CFCARN(NOMA  ,DEFICO,NEWGEO,POSNO ,NBDDL ,
     &                  COORNO,TYPNOE,NOMNO )
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 01/04/2008   AUTEUR ABBAS M.ABBAS 
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
      CHARACTER*24 DEFICO,NEWGEO
      INTEGER      POSNO
      INTEGER      NBDDL
      REAL*8       COORNO(3)
      CHARACTER*4  TYPNOE
      CHARACTER*8  NOMNO
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE DISCRETE - APPARIEMENT - UTILITAIRE)
C
C CARACTERISTIQUES DU NOEUD
C
C ----------------------------------------------------------------------
C
C
C IN  NOMA   : NOM DU MAILLAGE
C IN  DEFICO : SD DE CONTACT (DEFINITION)
C IN  NEWGEO : COORDONNEES REACTUALISEES DES NOEUDS DU MAILLAGE
C IN  POSNO  : INDICE DANS CONTNO DU NOEUD  
C OUT TYPNOE : TYPE DU NOEUD 'MAIT' OU 'ESCL'
C OUT NBDDL  : NOMBRE DE DDL DU NOEUD 
C OUT COORNO : COORDONNEES DU NOEUD
C OUT NOMNOE : NOM DU NOEUD
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*32       JEXNUM
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
      INTEGER      CFMMVD,ZNOES
      CHARACTER*24 CONTNO,PDDL,NOESCL
      INTEGER      JNOCO,JPDDL,JNOESC
      INTEGER      JCOOR
      INTEGER      NUMNO,IRET
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ ()
C
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C
      CONTNO = DEFICO(1:16)//'.NOEUCO'           
      PDDL   = DEFICO(1:16)//'.PDDLCO'  
      NOESCL = DEFICO(1:16)//'.NOESCL' 
      CALL JEVEUO(CONTNO,'L',JNOCO) 
      CALL JEVEUO(NOESCL,'L',JNOESC) 
      ZNOES  = CFMMVD('ZNOES')                   
C      
      CALL JEVEUO(NEWGEO(1:19)//'.VALE','L',JCOOR)     
C
C --- NOMBRE DE DDLS DU NOEUD 
C
      CALL JEEXIN(PDDL,IRET)
      IF (IRET.NE.0) THEN
        CALL JEVEUO(PDDL,  'L',JPDDL)
        NBDDL  = ZI(JPDDL+POSNO ) - ZI(JPDDL+POSNO -1)
        IF ((NBDDL .GT.3).OR.(NBDDL.LT.2)) THEN
          CALL ASSERT(.FALSE.)
        END IF
      ELSE
        NBDDL = 0
      ENDIF
C
C --- NUMERO ABSOLU DU NOEUD
C      
      NUMNO  = ZI(JNOCO+POSNO -1)         
C
C --- COORDONNEES DU NOEUD 
C
      COORNO(1) = ZR(JCOOR+3*(NUMNO -1))
      COORNO(2) = ZR(JCOOR+3*(NUMNO -1)+1)
      COORNO(3) = ZR(JCOOR+3*(NUMNO -1)+2) 
C
C --- TYPE DU NOEUD
C      
      IF (ZR(JNOESC+ZNOES*(POSNO-1)).GT.0.D0) THEN
        TYPNOE = 'MAIT'
      ELSEIF (ZR(JNOESC+ZNOES*(POSNO-1)).LT.0.D0) THEN
        TYPNOE = 'ESCL'
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF      
C
C --- NOM DU NOEUD
C
      CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUMNO),NOMNO)       
C
      CALL JEDEMA()
C 
      END
