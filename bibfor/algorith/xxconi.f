      INTEGER FUNCTION XXCONI(DEFICO,NOMFIS,TYPMAI)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/07/2007   AUTEUR ABBAS M.ABBAS 
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
      CHARACTER*8   NOMFIS
      CHARACTER*4   TYPMAI     
      CHARACTER*24  DEFICO      
C               
C ----------------------------------------------------------------------
C
C ROUTINE XFEM (METHODE XFEM - UTILITAIRE)
C
C RETOURNE LA ZONE DE CONTACT CORRESPONDANT A UNE FISSURE
C      
C ----------------------------------------------------------------------
C
C
C IN  DEFICO : SD DU CONTACT
C IN  NOMFIS : NOM DE LA SD FISS_XFEM
C IN  TYPMAI : TYPE DE LA FISSURE: POUR L'INSTANT 'MAIT'
C 
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
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
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      CHARACTER*24 XFIMAI
      INTEGER      JFIMAI   
      INTEGER      NZOCO,IZONE,IRET
      CHARACTER*8  FISCOU,K8BID
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()   
C
C --- INITIALISATIONS
C     
      IRET   = 0
C
C --- ACCES OBJETS JEVEUX
C 
      XFIMAI = DEFICO(1:16)//'.XFIMAI' 
      CALL JEVEUO(XFIMAI,'L',JFIMAI) 
      CALL JELIRA(XFIMAI,'LONMAX',NZOCO,K8BID)
C
C --- RECHERCHE FISSURE DANS MAITRE
C      
      DO 10 IZONE = 1,NZOCO
        IF (TYPMAI.EQ.'MAIT') THEN
          FISCOU = ZK8(JFIMAI-1+IZONE)
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF  
        IF (FISCOU.EQ.NOMFIS) THEN
          IF (IRET.EQ.0) THEN
            IRET   = IZONE
            GOTO 11
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF  
        ENDIF
  10  CONTINUE
  11  CONTINUE
C    
      IF (IRET.LE.0) THEN
        CALL U2MESS('F','XFEM_4')
      ELSE
        XXCONI = IRET
      ENDIF      
C
      CALL JEDEMA()
C   
      END
