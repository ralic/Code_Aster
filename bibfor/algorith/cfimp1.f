      SUBROUTINE CFIMP1(DEFICO,RESOCO,NOMA  ,NBLIAI,IFM)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*24 RESOCO
      INTEGER      IFM
      INTEGER      NBLIAI
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE DISCRETE - APPARIEMENT - UTILITAIRE)
C
C IMPRESSION DES LIAISONS ESCLAVE/MAITRE
C
C ----------------------------------------------------------------------
C
C
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  NOMA   : NOM DU MAILLAGE
C IN  NBLIAI : NOMBRE TOTAL DE LIAISONS 
C IN  IFM    : UNITE D'IMPRESSION DU MESSAGE
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
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
      INTEGER      CFMMVD,ZAPPA
      INTEGER      POSNOE,POSMAI,ILIAC,ILIAI,ACTIF,IBID
      CHARACTER*8  NOMNOE,NOMMAI,NOMENT
      INTEGER      CODRET
      CHARACTER*14 CHAIAC
      CHARACTER*4  TYPE2
      CHARACTER*2  TYPLIA
      REAL*8       JEU
      CHARACTER*10 TYPLI
      CHARACTER*24 APPARI,APJEU
      INTEGER      JAPPAR,JAPJEU
      CHARACTER*19 LIAC,CONVEC,COCO
      INTEGER      JLIAC,JVECC,JCOCO
      INTEGER      BTOTAL,NBLIAC
      INTEGER      LLF,LLF1,LLF2,NDIM
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ ()
C
C --- ACCES SD CONTACT
C
      APPARI = RESOCO(1:14)//'.APPARI'
      LIAC   = RESOCO(1:14)//'.LIAC'
      CONVEC = RESOCO(1:14)//'.CONVEC'
      APJEU  = RESOCO(1:14)//'.APJEU'
      COCO   = RESOCO(1:14)//'.COCO'
C
      CALL JEVEUO(APPARI,'L',JAPPAR)
      CALL JEVEUO(LIAC  ,'L',JLIAC)
      CALL JEVEUO(APJEU ,'L',JAPJEU)    
      CALL JEVEUO(COCO  ,'L',JCOCO)
      CALL JEEXIN(CONVEC,IBID)
      IF (IBID.NE.0) THEN 
        CALL JEVEUO(CONVEC,'L',JVECC) 
      ELSE
        JVECC = 0
      ENDIF
      ZAPPA  = CFMMVD('ZAPPA')
C
C --- INFORMATIONS SUR CONTACT
C      
      CALL CFDISD(JCOCO ,NDIM  ,NBLIAC,LLF   ,LLF1  ,
     &            LLF2  )

      BTOTAL = NBLIAC+LLF+LLF1+LLF2   
C
C --- BOUCLE SUR LES LIAISONS
C
      DO 500 ILIAI = 1,NBLIAI
C
C --- REPERAGE DE L'ESCLAVE
C
        POSNOE = ZI(JAPPAR+ZAPPA*(ILIAI-1)+1)
C
C --- NOM ET TYPE DU NOEUD ESCLAVE
C
        CALL CFNOMM(NOMA  ,DEFICO,'NOEU',POSNOE,NOMENT,
     &              CODRET)        
        IF (CODRET.EQ.-1) THEN
          NOMNOE = 'ERREUR'
        ELSE  
          NOMNOE = NOMENT
        ENDIF
C
C --- REPERAGE DU MAITRE
C
        POSMAI = ZI(JAPPAR+ZAPPA*(ILIAI-1)+2)
C
C --- JEU
C
        JEU = ZR(JAPJEU-1+ILIAI)
C
C --- ACTIF OU NON ?
C
        ACTIF = 0

        DO 10 ILIAC = 1,BTOTAL
          IF (ZI(JLIAC-1+ILIAC).EQ.ILIAI) THEN 
            ACTIF  = 1
C
C --- TYPE LIAISON
C
            TYPLI = 'CONT.     '
            IF (JVECC.NE.0) THEN
              TYPLIA = ZK8(JVECC-1+ILIAC)
              IF (TYPLIA.EQ.'C0') THEN
                TYPLI = 'CONT.     '
              ELSE IF (TYPLIA.EQ.'F0') THEN
                IF (NDIM.EQ.3) THEN
                  TYPLI = 'FROT. 1&2 '
                ELSE
                  TYPLI = 'FROT.     '
                ENDIF
              ELSE IF (TYPLIA.EQ.'F1') THEN
                TYPLI = 'FROT. 1   '
              ELSE IF (TYPLIA.EQ.'F2') THEN
                TYPLI = 'FROT. 2   '
              ELSE IF (TYPLIA.EQ.'F3') THEN
                TYPLI = 'FROT.     '
              ENDIF
            ENDIF         
          ENDIF
  10    CONTINUE
  20    CONTINUE
C
C --- NOM ET TYPE DU MAITRE
C
      IF (POSMAI.LT.0) THEN
        CALL CFNOMM(NOMA  ,DEFICO,'NOEU',POSMAI,NOMENT,
     &              CODRET)
        IF (CODRET.LT.0) THEN
          TYPE2  = ' '
          NOMMAI = 'ERREUR'          
        ELSE   
          TYPE2  = '/ND '
          NOMMAI = NOMENT   
        ENDIF         
      ELSEIF (POSMAI.GT.0) THEN
        CALL CFNOMM(NOMA  ,DEFICO,'MAIL',POSMAI,NOMENT,
     &              CODRET)
        IF (CODRET.LT.0) THEN
          TYPE2  = ' '
          NOMMAI = 'ERREUR'          
        ELSE
          TYPE2  = '/EL '
          NOMMAI = NOMENT   
        ENDIF    
      ELSE  
        TYPE2  = ' NON'
        NOMMAI = ' APPARIE'               
      ENDIF    
C
C --- IMPRESSION
C
        IF (ACTIF.EQ.1) THEN
          CHAIAC = ' ACTIVE (JEU: '
        
          WRITE (IFM,1000) ILIAI,'(',NOMNOE,TYPE2,NOMMAI,'): ',
     &                     CHAIAC,JEU,',TYPE: ',TYPLI,')'
        ELSE
          CHAIAC = ' LIBRE  (JEU: '
          WRITE (IFM,1010) ILIAI,'(',NOMNOE,TYPE2,NOMMAI,'): ',
     &                     CHAIAC,JEU,')'

        ENDIF
  500 CONTINUE
   
 1000 FORMAT (' <CONTACT> <> LIAISON ',I5,A1,A8,A4,A8,A3,A14,E10.3,
     &         A7,A10,A1)

 1010 FORMAT (' <CONTACT> <> LIAISON ',I5,A1,A8,A4,A8,A3,A14,E10.3,
     &         A1)
C
      CALL JEDEMA()
C
      END
