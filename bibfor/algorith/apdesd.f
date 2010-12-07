      SUBROUTINE APDESD(SDAPPA)
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
      CHARACTER*19 SDAPPA   
C      
C ----------------------------------------------------------------------
C
C ROUTINE APPARIEMENT 
C
C DESTRUCTION DE LA SD
C
C ----------------------------------------------------------------------
C
C
C IN  SDAPPA : NOM DE LA SD APPARIEMENT
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
      INTEGER      IFM,NIV
      CHARACTER*24 NOMSD,APPAR
      CHARACTER*24 APINZI,APINZR     
      CHARACTER*24 APINFI,APINFR
      CHARACTER*24 APPOIN
      CHARACTER*24 APNOMS,APINFP
      CHARACTER*24 APDIST,APTAU1,APTAU2,APPROJ
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('APPARIEMENT',IFM,NIV)
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<APPARIEMENT> DESTRUCTION DE LA SD APPARIEMENT' 
      ENDIF         
C
C --- SD NOM DES SD
C
      NOMSD  = SDAPPA(1:19)//'.NOSD' 
      CALL JEDETR(NOMSD )         
C
C --- SD APPARIEMENT 
C 
      APPAR  = SDAPPA(1:19)//'.APPA'
      CALL JEDETR(APPAR )
C
C --- SD POUR DISTANCE ET TANGENTES
C      
      APDIST = SDAPPA(1:19)//'.DIST'
      APTAU1 = SDAPPA(1:19)//'.TAU1'
      APTAU2 = SDAPPA(1:19)//'.TAU2'
      CALL JEDETR(APDIST)
      CALL JEDETR(APTAU1)
      CALL JEDETR(APTAU2)
C
C --- COORDONNEES DE LA PROJECTION
C
      APPROJ = SDAPPA(1:19)//'.PROJ'
      CALL JEDETR(APPROJ)
C
C --- COORDONNEES DES POINTS
C
      APPOIN = SDAPPA(1:19)//'.POIN'  
      CALL JEDETR(APPOIN)
C
C --- INFOS DES POINTS
C
      APINFP = SDAPPA(1:19)//'.INFP'
      CALL JEDETR(APINFP)
C
C --- INFORMATIONS GLOBALES
C
      APINFI = SDAPPA(1:19)//'.INFI'
      APINFR = SDAPPA(1:19)//'.INFR'
      CALL JEDETR(APINFI)
      CALL JEDETR(APINFR)
C
C --- INFORMATIONS PAR ZONE
C
      APINZI = SDAPPA(1:19)//'.INZI'
      APINZR = SDAPPA(1:19)//'.INZR'
      CALL JEDETR(APINZI)
      CALL JEDETR(APINZR)   
C
C --- SD NOMS DES POINTS DE CONTACT      
C
      APNOMS = SDAPPA(1:19)//'.NOMS' 
      CALL JEDETR(APNOMS)              
C
      CALL JEDEMA()
C 
      END
