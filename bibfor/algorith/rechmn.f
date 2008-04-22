      SUBROUTINE RECHMN(NOMA  ,NEWGEO,DEFICO,RESOCO,IZONE,
     &                  IESCL0)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/04/2008   AUTEUR ABBAS M.ABBAS 
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
      INTEGER      IZONE
      CHARACTER*8  NOMA
      CHARACTER*24 DEFICO
      CHARACTER*24 RESOCO
      CHARACTER*24 NEWGEO
      INTEGER      IESCL0 
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT - MAIT/ESCL)
C
C RECHERCHE DU NOEUD MAITRE LE PLUS PROCHE DU NOEUD ESCLAVE
C
C ----------------------------------------------------------------------
C
C
C IN  NOMA   : NOM DU MAILLAGE
C IN  IZONE  : NUMERO DE LA ZONE DE CONTACT
C IN  NEWGEO : GEOMETRIE ACTUALISEE EN TENANT COMPTE DU CHAMP DE
C              DEPLACEMENTS COURANT
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  IESCL0 : PREMIER NOEUD ESCLAVE DE LA ZONE
C OUT NFESCL : NOMBRE DE NOEUDS ESCLAVES DE LA ZONE
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
      INTEGER      CFMMVD,CFDISI,ZAPME,ZAPPA
      CHARACTER*24 NDIMCO,PZONE,PSURNO,CONTNO
      INTEGER      JDIM,JZONE,JSUNO,JNOCO
      CHARACTER*24 APMEMO,APPARI
      INTEGER      JAPMEM,JAPPAR
      REAL*8       COORPT(3)
      CHARACTER*24 K24BLA,K24BID
      INTEGER      NBNOE
      INTEGER      ISURFE,ISURFM,JCOOR,JDECE
      INTEGER      POSNOE,NUMNOE,PROJ
      INTEGER      KE,IESCL,IBID
      INTEGER      POSNOM,SUPPOK
      INTEGER      IFM,NIV
      LOGICAL      DIRAPP,LBID
      REAL*8       DIR(3),TOLEAP
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()      
      CALL INFDBG('CONTACT',IFM,NIV)    
C   
C --- AFFICHAGE
C
      CALL CFIMPE(IFM,NIV,'RECHMN',1)
C
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C
      NDIMCO = DEFICO(1:16)//'.NDIMCO'     
      PZONE  = DEFICO(1:16)//'.PZONECO'
      CONTNO = DEFICO(1:16)//'.NOEUCO'
      PSURNO = DEFICO(1:16)//'.PSUNOCO'
      APPARI = RESOCO(1:14)//'.APPARI'
      APMEMO = RESOCO(1:14)//'.APMEMO'    
C
      CALL JEVEUO(NDIMCO,'E',JDIM  )
      CALL JEVEUO(PZONE, 'L',JZONE )    
      CALL JEVEUO(CONTNO,'L',JNOCO )
      CALL JEVEUO(PSURNO,'L',JSUNO )
      CALL JEVEUO(APPARI,'E',JAPPAR)
      CALL JEVEUO(APMEMO,'E',JAPMEM)
      CALL JEVEUO(NEWGEO(1:19)//'.VALE','L',JCOOR)      
C
      ZAPME  = CFMMVD('ZAPME')
      ZAPPA  = CFMMVD('ZAPPA') 
C
C --- INITIALISATION DE VARIABLES
C        
      K24BLA = ' '
C
C --- OPTIONS SUR LA ZONE DE CONTACT
C            
      CALL MMINFP(IZONE ,DEFICO,K24BLA,'TYPE_APPA',
     &            IBID  ,DIR   ,K24BID,DIRAPP)
      CALL MMINFP(IZONE ,DEFICO,K24BLA,'TOLE_APPA',
     &            IBID  ,TOLEAP,K24BID,LBID  )       
      PROJ   = CFDISI(DEFICO,'PROJECTION',IZONE)          
C
C --- NUMEROS DES SURFACES MAITRE ET ESCLAVE
C
      ISURFE = ZI(JZONE+IZONE)
      ISURFM = ZI(JZONE+IZONE-1) + 1
C
C --- NOMBRE DE NOEUDS DE LA SURFACE ESCLAVE
C
      NBNOE  = ZI(JSUNO+ISURFE) - ZI(JSUNO+ISURFE-1)
C
C --- DECALAGE DANS CONTNO POUR TROUVER LES NOEUDS DE LA SURFACE ESCLAVE
C
      JDECE  = ZI(JSUNO+ISURFE-1)
C
C --- NOMBRE DE NOEUDS ESCLAVES DE LA ZONE A PRIORI
C
      ZI(JDIM+8+IZONE) = NBNOE 
C
C --- PREMIER NOEUD ESCLAVE DE LA ZONE
C
      IESCL    = IESCL0      
C
C --- APPARIEMENT PAR METHODE "BRUTE FORCE"
C --- DOUBLE BOUCLE SUR LES NOEUDS
C
      DO 50 KE = 1,NBNOE
C
C --- POSITION DANS CONTNO DU NOEUD DE LA SURFACE ESCLAVE
C
        POSNOE  = JDECE + KE
C
C --- NUMERO ABSOLU DU NOEUD ESCLAVE
C
        NUMNOE = ZI(JNOCO+POSNOE-1)    
C
C --- COORDONNEES DU NOEUD ESCLAVE
C
        COORPT(1) = ZR(JCOOR+3*(NUMNOE-1))
        COORPT(2) = ZR(JCOOR+3*(NUMNOE-1)+1)
        COORPT(3) = ZR(JCOOR+3*(NUMNOE-1)+2)        
C
C --- ON ELIMINE LE NOEUD SI INTERDIT COMME ESCLAVE
C
        CALL CFELSN(NOMA  ,DEFICO,RESOCO,IZONE,POSNOE,
     &              SUPPOK)
C
C --- SINON ON CHERCHE LE NOEUD MAITRE 
C       
        IF (SUPPOK.EQ.0) THEN     
C
C --- RECHERCHE DU NOEUD MAITRE LE PLUS PROCHE 
C
          CALL MMREND(DEFICO,NEWGEO,ISURFM,COORPT,TOLEAP,
     &                DIRAPP,DIR   ,POSNOM)
C
C --- STOCKAGE DU NOEUD ESCLAVE
C           
          ZI(JAPPAR+ZAPPA*(IESCL-1)+1)  = POSNOE
C
C --- STOCKAGE DU NOEUD MAITRE LE PLUS PROCHE
C        
          ZI(JAPMEM+ZAPME*(POSNOE-1)+1) = POSNOM
C
C --- TYPE DE PROJECTION
C          
          ZI(JAPPAR+ZAPPA*(IESCL-1)+3)  = PROJ 
C
C --- NOUVEAU NOEUD ESCLAVE
C
          IESCL = IESCL + 1
        ENDIF  
C                    
   50 CONTINUE     
C
      CALL JEDEMA()
      END
