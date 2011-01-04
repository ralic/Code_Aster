      SUBROUTINE APINFR(SDAPPA,QUESTZ,IP    ,VALR  )
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/01/2011   AUTEUR ABBAS M.ABBAS 
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
      CHARACTER*19  SDAPPA
      INTEGER       IP 
      REAL*8        VALR 
      CHARACTER*(*) QUESTZ
C      
C ----------------------------------------------------------------------
C
C ROUTINE APPARIEMENT (UTILITAIRE)
C
C INTERROGATION DE LA SDAPPA - REEL
C
C ----------------------------------------------------------------------
C
C
C IN  SDAPPA : NOM DE LA SD APPARIEMENT
C IN  QUESTI : QUESTION
C              APPARI_PROJ_KSI1 : COORD. PARAM. 1 PROJECTION DU PT
C              APPARI_PROJ_KSI2 : COORD. PARAM. 2 PROJECTION DU PT
C              APPARI_DIST      : DISTANCE PT - PROJECTION
C IN  IP     : INDICE DU POINT
C OUT VALR   : REPONSE A LA QUESTION 
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
      CHARACTER*24 APDIST,APPROJ
      INTEGER      JDIST,JPROJ    
      CHARACTER*16 QUESTI
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('APPARIEMENT',IFM,NIV)     
C
C --- ACCES SDAPPA
C
      APPROJ = SDAPPA(1:19)//'.PROJ'
      APDIST = SDAPPA(1:19)//'.DIST'
C
C --- INITIALISATIONS
C
      VALR   = 0.D0
      QUESTI = QUESTZ
C
C --- QUESTION
C
      IF     (QUESTI.EQ.'APPARI_PROJ_KSI1') THEN
        CALL JEVEUO(APPROJ,'L',JPROJ )
        VALR  = ZR(JPROJ+2*(IP-1)+1-1)
      ELSEIF (QUESTI.EQ.'APPARI_PROJ_KSI2') THEN
        CALL JEVEUO(APPROJ,'L',JPROJ )
        VALR  = ZR(JPROJ+2*(IP-1)+2-1)
C
      ELSEIF (QUESTI.EQ.'APPARI_DIST') THEN
        CALL JEVEUO(APDIST,'L',JDIST )
        VALR  = ZR(JDIST+4*(IP-1)+1-1)                     
C
      ELSE    
        CALL ASSERT(.FALSE.)
      ENDIF             
C
      CALL JEDEMA()
C 
      END
