      SUBROUTINE TRANMA ( GEOMI , D , BIDIM )
      IMPLICIT   NONE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/09/2002   AUTEUR GREFFET N.GREFFET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     BUT : TRANSLATION D'UN MAILLAGE
C
C     IN :
C            GEOMI  : CHAM_NO(GEOM_R) : CHAMP DE GEOMETRIE A TRANSLATER
C            DIR    : VECTEUR DE TRANSLATION
C     OUT:
C            GEOMI  : CHAM_NO(GEOM_R) : CHAMP DE GEOMETRIE ACTUALISE
C                 
C
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER             N1, I, IADCOO
      LOGICAL             BIDIM
      CHARACTER*8         K8BID
      CHARACTER*19        GEOMI
      CHARACTER*24        COORJV
      REAL*8              D(3)
C
      CALL JEMARQ()
      COORJV=GEOMI(1:19)//'.VALE'
      CALL JEVEUO(COORJV,'E',IADCOO)
      CALL JELIRA(COORJV,'LONMAX',N1,K8BID)
      N1=N1/3
      IADCOO=IADCOO-1
      IF ( BIDIM ) THEN
         DO 10 I=1,N1
            ZR(IADCOO+3*(I-1)+1)=ZR(IADCOO+3*(I-1)+1)+D(1)
            ZR(IADCOO+3*(I-1)+2)=ZR(IADCOO+3*(I-1)+2)+D(2)
 10      CONTINUE
      ELSE
         DO 20 I=1,N1
            ZR(IADCOO+3*(I-1)+1)=ZR(IADCOO+3*(I-1)+1)+D(1)
            ZR(IADCOO+3*(I-1)+2)=ZR(IADCOO+3*(I-1)+2)+D(2)
            ZR(IADCOO+3*(I-1)+3)=ZR(IADCOO+3*(I-1)+3)+D(3)
 20      CONTINUE
      ENDIF
      CALL JEDEMA()
      END
