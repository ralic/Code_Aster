      SUBROUTINE XBASL2(MODELE,NOMA,CHFOND,GRLT,GRLN,CNSBAS)
      IMPLICIT NONE

      CHARACTER*8     MODELE,NOMA
      CHARACTER*19    GRLT,GRLN,CNSBAS
      CHARACTER*24    CHFOND


C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/12/2005   AUTEUR GENIAUT S.GENIAUT 
C RESPONSABLE GENIAUT S.GENIAUT
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
C
C     FONCTION REALISEE : CREATION D'UN CHAM_EL QUI CONTIENT LA BASE
C                         LOCALE AU POINT DU FOND DE FISSURE ASSOCIE
C ----------------------------------------------------------------------
C ENTREE:
C      MODELE  : NOM DE L'OBJET MODELE
C      NOMA    : NOM DE L'OBJET MAILLAGE
C      CHFOND  : NOM DES POINTS DU FOND DE FISSURE
C      GRLT    : GRADIENTS DE LA LEVEL-SET TANGENTE
C      GRLN    : GRADIENTS DE LA LEVEL-SET NORMALE
C
C SORTIE:
C      CNSBAS  : CHAM_NO_S BASE LOCALE DE FONFIS
C ----------------------------------------------------------------------
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
      CHARACTER*1 K1BID
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*24      COORN
      CHARACTER*19      LIGREL
      CHARACTER*8       LICMP(6),K8BID
      INTEGER           IFON,IADRCO,IADRMA,JGSV,JGSL,JGT,JGN
      INTEGER           LONG,LNOFF,NBNO,IRET,INO,J
      REAL*8            D
      REAL*8            XM,YM,DMIN,XN,YN,A(2)
      REAL*8            R8MAEM
C
      CALL JEMARQ()
  
      CALL JEVEUO(CHFOND,'L',IFON) 
      CALL JELIRA(CHFOND,'LONMAX',LONG,K8BID)  
      LNOFF=LONG/4

C     RÉCUPÉRATION DES GRADIENTS DE LST ET LSN
      CALL JEVEUO(GRLT//'.CNSV','L',JGT)
      CALL JEVEUO(GRLN//'.CNSV','L',JGN)

      COORN = NOMA//'.COORDO    .VALE'
      CALL JEVEUO(COORN,'L',IADRCO)

      LICMP(1)  = 'X1'
      LICMP(2)  = 'X2'
      LICMP(3)  = 'X3'
      LICMP(4)  = 'X4'
      LICMP(5)  = 'X5'
      LICMP(6)  = 'X6'
      CALL CNSCRE(NOMA,'NEUT_R',6,LICMP,'V',CNSBAS)
      CALL JEVEUO(CNSBAS//'.CNSV','E',JGSV)
      CALL JEVEUO(CNSBAS//'.CNSL','E',JGSL)
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,K8BID,IRET)
 
C     CALCUL DES PROJETÉS DES NOEUDS SUR LE FOND DE FISSURE 
      DO 100 INO=1,NBNO
C       COORD DU NOEUD M DU MAILLAGE                
        XM = ZR(IADRCO+(INO-1)*3+1-1)
        YM = ZR(IADRCO+(INO-1)*3+2-1)
C       INITIALISATION
        DMIN = R8MAEM()
C       BOUCLE SUR PT DE FONFIS (ALGO VOIR )
        DO 110 J=1,LNOFF
C         COORD PT N
          XN = ZR(IFON-1+4*(J-1)+1) 
          YN = ZR(IFON-1+4*(J-1)+2) 
C         DISTANCE MN
          D = SQRT((XN-XM)*(XN-XM)+(YN-YM)*(YN-YM))
          IF(D.LT.DMIN) THEN
            DMIN = D
            A(1)=XN
            A(2)=YN
          ENDIF
110     CONTINUE
C       STOCKAGE DU PROJETÉ ET DES GRADIENTS         
        DO 120 J=1,2
          ZR(JGSV-1+6*(INO-1)+J)=A(J)
          ZL(JGSL-1+6*(INO-1)+J)=.TRUE.
          ZR(JGSV-1+6*(INO-1)+J+2)=ZR(JGT-1+2*(INO-1)+J)
          ZL(JGSL-1+6*(INO-1)+J+2)=.TRUE.
          ZR(JGSV-1+6*(INO-1)+J+4)=ZR(JGN-1+2*(INO-1)+J)
          ZL(JGSL-1+6*(INO-1)+J+4)=.TRUE.
 120    CONTINUE
 100  CONTINUE
      CALL JXVERI(' ',' ')
      CALL JEDEMA()
      END
