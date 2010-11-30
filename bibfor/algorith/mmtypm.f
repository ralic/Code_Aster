      SUBROUTINE MMTYPM(NOMA  ,NUMMA ,NNOSD  ,ALIAS ,NDIM  )
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
      INTEGER      NUMMA
      INTEGER      NNOSD,NDIM
      CHARACTER*8  NOMA
      CHARACTER*8  ALIAS
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (TOUTES METHODES - UTILITAIRE)
C
C TYPE DE LA MAILLE
C
C ----------------------------------------------------------------------
C
C IN  NOMA   : NOM DU MAILLAGE
C IN  NUMMA  : NUMERO ABSOLU DE LA MAILLE
C IN  NNOSD  : NOMBRE DE NOEUDS DE LA MAILLE SUIVANT LA SD
C OUT NDIM   : DIMENSION DE LA MAILLE 
C OUT ALIAS  : TYPE GEOMETRIQUE DE LA MAILLE
C
C /!\  NNO EST DIFFERENT DE NNOSD
C        NNOSD : NOMBRE DE NOEUDS SUPPORTANT SUIVANT LA SD
C                REPREND UNIQUEMENT LES NOEUDS SUPPORTANT 
C                DES DDLS DE DEPLACEMENTS
C        NNO   : NOMBRE DE NOEUDS TOTAL DE LA MAILLE
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
      INTEGER NNO
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- TYPE GEOMETRIQUE DE LA MAILLE 
C
      CALL MMELTY(NOMA  ,NUMMA ,ALIAS ,NNO  ,NDIM)
C
C --- CAS DES COQUES_3D: PAS DE DX/DY/DZ SUR NOEUD MILIEU
C      
      IF ((NNO .EQ.9).AND.(NNOSD .EQ.8)) THEN
        ALIAS = 'QU8'  
      ENDIF    
      IF ((NNO .EQ.7).AND.(NNOSD .EQ.6)) THEN
        ALIAS = 'TR6'
      ENDIF  
C
C --- CAS DES QUAD8 EN 3D: 4 NOEUDS ET RELATIONS LINEAIRES 
C       
      IF ((NNO .EQ.8).AND.(NNOSD .EQ.4)) THEN
        ALIAS = 'QU4'
      ENDIF  
C
      CALL JEDEMA()
C
      END
