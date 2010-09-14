      SUBROUTINE APCOMA(SDAPPA,NUMMA ,NBNO  ,COORMA)
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
      INTEGER      NUMMA,NBNO
      REAL*8       COORMA(27)
C      
C ----------------------------------------------------------------------
C
C ROUTINE APPARIEMENT (UTILITAIRE)
C
C COORDONNEES D'UNE MAILLE
C
C ----------------------------------------------------------------------
C
C
C IN  SDAPPA : NOM DE LA SD APPARIEMENT
C IN  NUMMA  : NUMERO ABSOLU DE LA MAILLE DANS LE MAILLAGE
C IN  NBNO   : NOMBRE DE NOEUDS DE LA MAILLE
C OUT COORMA : COORDONNEES DE LA MAILLE
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
      CHARACTER*19 NEWGEO
      CHARACTER*8  NOMA
C
      INTEGER      NBNMAX
      PARAMETER   (NBNMAX = 9)
C
      INTEGER      NO(NBNMAX),INO
      INTEGER      JCOOR,I,JDEC
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- NOM SD GEOMETRIE ACTUALISEE
C
      CALL APNOMK(SDAPPA,'NEWGEO',RNOMSD)
      NEWGEO = RNOMSD(1:19)
C
C --- NOM SD MAILLAGE
C
      CALL APNOMK(SDAPPA,'NOMA'  ,RNOMSD)
      NOMA   = RNOMSD(1:8) 
C
C --- INITIALISATIONS
C
      DO 10 I = 1,27
        COORMA(I) = 0.D0
  10  CONTINUE
      CALL ASSERT(NBNO.GT.0)
      CALL ASSERT(NBNO.LE.NBNMAX)
C
C --- NUMEROS ABSOLUS DES NOEUDS DE LA MAILLE 
C
      CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',NUMMA ),'L',JDEC)
      DO 61 INO = 1,NBNO
        NO(INO)     = ZI(JDEC+INO-1)
 61   CONTINUE 
C
C --- COORDONNEES DES NOEUDS DE LA MAILLE
C
      CALL JEVEUO(NEWGEO(1:19)//'.VALE','L',JCOOR)
      DO 70 INO = 1,NBNO
        COORMA(3*(INO-1)+1) = ZR(JCOOR+3*(NO(INO)-1))
        COORMA(3*(INO-1)+2) = ZR(JCOOR+3*(NO(INO)-1)+1)
        COORMA(3*(INO-1)+3) = ZR(JCOOR+3*(NO(INO)-1)+2)      
  70  CONTINUE
C
      CALL JEDEMA()
C 
      END
