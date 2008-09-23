      SUBROUTINE GFARCH(RESULT,NUMARC)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
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
C
      IMPLICIT NONE
      INTEGER      NUMARC
      CHARACTER*8  RESULT
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME)
C
C ARCHIVAGE DE LA SD GRAPPE_FLUIDE
C
C ----------------------------------------------------------------------
C
C
C IN  RESULT : NOM UTILISATEUR DU CONCEPT RESULTAT
C IN  NUMARC : NUMERO DE L'ARCHIVAGE
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
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
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER      JIFL,JFFL,JPARA
      INTEGER      II,I15,I16,I19
      CHARACTER*8  K8BID
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL JEVEUO('&&GFLECT.INDICE'         ,'L',JIFL)
      CALL JEVEUO('&&OP0070.GRAPPE_FLUIDE  ','L',JFFL)
      II     = 5 + ZI(JIFL-1+5)
      I15    = ZI(JIFL-1+II+15)
      I16    = ZI(JIFL-1+II+16)
      I19    = ZI(JIFL-1+II+19)
      CALL RSADPA(RESULT,'E'   ,1     ,'GFUM'        ,NUMARC,
     &            0     ,JPARA ,K8BID )
      ZR(JPARA) = ZR(JFFL-1+I15+1)
      CALL RSADPA(RESULT,'E'   ,1     ,'GFUA'        ,NUMARC,
     &            0     ,JPARA ,K8BID )
      ZR(JPARA) = ZR(JFFL-1+I15+2)
      CALL RSADPA(RESULT,'E'   ,1     ,'GFUML'       ,NUMARC,
     &            0     ,JPARA ,K8BID )
      ZR(JPARA) = ZR(JFFL-1+I15+3)
      CALL RSADPA(RESULT,'E'   ,1     ,'GFUI'        ,NUMARC,
     &            0     ,JPARA ,K8BID )
      ZR(JPARA) = ZR(JFFL-1+I15+4)
      CALL RSADPA(RESULT,'E'   ,1     ,'GFVAG'       ,NUMARC,
     &            0     ,JPARA ,K8BID )
      ZR(JPARA) = ZR(JFFL-1+I16+1)
      CALL RSADPA(RESULT,'E'   ,1     ,'ITER_DASHPOT',NUMARC,
     &            0     ,JPARA ,K8BID )
      ZI(JPARA) = ZI(JIFL-1+4)
      CALL RSADPA(RESULT,'E'   ,1     ,'GFITER'      ,NUMARC,
     &            0     ,JPARA ,K8BID )
      ZI(JPARA) = ZI(JIFL-1+3)
      CALL RSADPA(RESULT,'E'   ,1     ,'GFVFD'       ,NUMARC,
     &            0     ,JPARA ,K8BID )
      ZR(JPARA) = ZR(JFFL-1+I19+1)
      CALL RSADPA(RESULT,'E'   ,1     ,'GFVAD'       ,NUMARC,
     &            0     ,JPARA ,K8BID )
      ZR(JPARA) = ZR(JFFL-1+I19+2)   
C
      CALL JEDEMA()
      END
