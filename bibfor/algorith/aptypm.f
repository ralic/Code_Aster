      SUBROUTINE APTYPM(SDAPPA,NUMMA ,NDIM  ,NNOSD ,ALIAS ,
     &                  NOMMA )
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
      CHARACTER*8  ALIAS
      CHARACTER*8  NOMMA 
      INTEGER      NUMMA
      INTEGER      NNOSD,NDIM
C      
C ----------------------------------------------------------------------
C
C ROUTINE APPARIEMENT (UTILITAIRE)
C
C CARACTERISTIQUES DE LA MAILLE
C
C ----------------------------------------------------------------------
C
C
C IN  SDAPPA : NOM DE LA SD APPARIEMENT
C IN  NUMMA  : NUMERO DE LA MAILLE
C IN  NNOSD  : NOMBRE DE NOEUDS DE LA MAILLE SUIVANT LA SD
C OUT NOMMA  : NOM DE LA MAILLE
C OUT ALIAS  : TYPE GEOMETRIQUE DE LA MAILLE
C OUT NDIM   : DIMENSION DE LA MAILLE
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
      CHARACTER*24 RNOMSD
      CHARACTER*8  NOMA      
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()   
C
C --- NOM SD MAILLAGE
C
      CALL APNOMK(SDAPPA,'NOMA'  ,RNOMSD)
      NOMA   = RNOMSD(1:8)          
C
C --- TYPE GEOMETRIQUE DE LA MAILLE 
C
      CALL MMTYPM(NOMA  ,NUMMA ,NNOSD ,ALIAS ,NDIM )
C
C --- NOM DE LA MAILLE
C
      CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMMA ),NOMMA )           
C
      CALL JEDEMA()
C 
      END
