      SUBROUTINE XPOCRF(MODELE,MAXFEM,MFTOT,NFTOT,NFCOMF)
      IMPLICIT NONE
      
      CHARACTER*8  MODELE,MAXFEM
      INTEGER      MFTOT,NFTOT,NFCOMF
      
      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 22/02/2011   AUTEUR MACOCCO K.MACOCCO 
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
C     BUT : GENERER UN MAILLAGE DESTINE UNIQUEMENT AU POST-TRAITEMENT
C           DU FOND DE FISSURE
C   IN
C       MODELE : MODELE FISSURE
C       MFTOT  : NOMBRE TOTAL DE MAILLES DE FONDS DE FISSURES
C       NFTOT  : NOMBRE TOTAL DE NOEUDS DE FONDS DE FISSURES
C       NFCOMF : NOMBRE TOTAL DE CONNEXIONS DANS LES MAILLES
C   IN/OUT
C       MAXFEM : MAILLAGE FISSURE
C     =================================================================
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32 JEXNUM,JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     ------------------------------------------------------------------
      INTEGER      IFISS,IFON,IFON1,IFON2,INO,J,IMA
      INTEGER      NFISS,NFOND,NFON,NTSEG2,NTPOI1
      INTEGER      ICOMPT,NCOMPT,NUFON,IAGMA,IAGNO
      INTEGER      NTAIL,NDIM,NBMAX,IACOO2
      INTEGER      IBID,NNNTOT,IRET,JCONX,IGR
      INTEGER      JVA00,JVA0,JVA1,JVA2,JVA3,JFMULT
      INTEGER      JTYPM2,JNOM
      CHARACTER*2  CHN1,CHN2
      CHARACTER*6  CHN
      CHARACTER*8  K8B,FISS,MO,MALINI,NOGNO,NOGMA
      CHARACTER*19 NOMTAB,COORD2
      CHARACTER*24 NOM
      
      
      
C     INITIALISATION      

C     RECUPERATION DES CARACTERISTIQUES DES FONDS DE FISSURES
      MO = MODELE
      NOM = MO//'.FISS                   '
      CALL JEVEUO(NOM,'L',JNOM)
      CALL JELIRA(NOM,'LONUTI',NFISS,K8B)
            
      CALL DISMOI('F','NB_NO_MAILLA',MAXFEM,'MAILLAGE',NNNTOT,K8B,IRET)
      CALL DISMOI('F','NB_MA_MAILLA',MAXFEM,'MAILLAGE',NBMAX,K8B,IRET)
      CALL DISMOI('F','DIM_GEOM',MO,'MODELE',NDIM,K8B,IRET)
      
      CALL JEVEUO(MAXFEM//'.TYPMAIL','E',JTYPM2)
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','POI1') ,NTPOI1)
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG2') ,NTSEG2)
      
      IF ((NFTOT.GT.0).AND.(MFTOT.GT.0)) THEN               
          
C       ATTRIBUTION DU NOM DES NOEUDS DU FOND DE FISSURE
        DO 10 INO=1,NFTOT
          CALL CODENT(INO,'G',CHN)
          CALL JECROC(JEXNOM(MAXFEM//'.NOMNOE','NF'//CHN))
 10    CONTINUE
C       ATTRIBUTION DU NOM DES MAILLES DU FOND DE FISSURE
        DO 20 IMA=1,MFTOT
          CALL CODENT(IMA,'G',CHN)
          CALL JECROC(JEXNOM(MAXFEM//'.NOMMAI','MF'//CHN))
 20    CONTINUE
 
        NCOMPT = 0
        ICOMPT = 0
        COORD2= MAXFEM//'.COORDO'
        CALL JEVEUO(COORD2//'.VALE','E',IACOO2)                 
        
        DO 30 IFISS=1,NFISS

          FISS = ZK8(JNOM)
          CALL JEEXIN( FISS//'.FONDFISS',IRET)
          IF (IRET.NE.0) THEN
            CALL JEVEUO(FISS//'.FONDMULT','L',JFMULT)
            CALL LTNOTB ( FISS,'FOND_FISS',NOMTAB)
            CALL JEVEUO(NOMTAB//'.0001','L',JVA0)
            CALL JELIRA(NOMTAB//'.0001','LONUTI',NFON,K8B)
            IF (NDIM.EQ.3) THEN
              CALL JEVEUO(NOMTAB//'.0002','L',JVA00)
              CALL JEVEUO(NOMTAB//'.0004','L',JVA1)
              CALL JEVEUO(NOMTAB//'.0005','L',JVA2)
              CALL JEVEUO(NOMTAB//'.0006','L',JVA3)
            ELSE
              CALL JEVEUO(NOMTAB//'.0002','L',JVA1)
              CALL JEVEUO(NOMTAB//'.0003','L',JVA2)
            ENDIF
            NFOND = ZI(JVA0-1+NFON)
            CALL CODENT(IFISS,'D0',CHN1)
            DO 31 IFON=1,NFOND
          
              CALL CODENT(IFON,'D0',CHN2)
              NOGMA = 'MF_'//CHN1//'_'//CHN2
              NOGNO = 'NF_'//CHN1//'_'//CHN2
            
C           VERIFICATION DE L'ABSENCE DE GROUPE AYANT LE MEME NOM
              CALL JENONU(JEXNOM(MAXFEM//'.GROUPEMA',NOGMA),IBID)
              IF (IBID.GT.0) THEN
               CALL U2MESK('F','ALGELINE3_7',1,NOGMA)
              ENDIF
              CALL JENONU(JEXNOM(MAXFEM//'.GROUPENO',NOGNO),IBID)
              IF (IBID.GT.0) THEN
               CALL U2MESK('F','SOUSTRUC_37',1,NOGNO)
              ENDIF

              NTAIL = ZI(JFMULT-1+2*IFON)-ZI(JFMULT-1+2*IFON-1)+1

C           CONSTRUCTION DES GROUPES DE MAILLES DU FOND DE FISSURE
              CALL JECROC(JEXNOM(MAXFEM//'.GROUPEMA',NOGMA))
              CALL JENONU(JEXNOM(MAXFEM//'.GROUPEMA',NOGMA),IGR)
              CALL JEECRA(JEXNUM(MAXFEM//'.GROUPEMA',IGR),'LONMAX',
     &               NFON,K8B)
              CALL JEECRA(JEXNUM(MAXFEM//'.GROUPEMA',IGR),'LONUTI',
     &                    MAX((NTAIL-1),1),K8B)
              CALL JEVEUO(JEXNUM(MAXFEM//'.GROUPEMA',IGR),'E',IAGMA)
            
C           CONSTRUCTION DES GROUPES DE NOEUDS DU FOND DE FISSURE
              CALL JECROC(JEXNOM(MAXFEM//'.GROUPENO',NOGNO))
              CALL JENONU(JEXNOM(MAXFEM//'.GROUPENO',NOGNO),IGR)
              CALL JEECRA(JEXNUM(MAXFEM//'.GROUPENO',IGR),'LONMAX',
     &               NFON,K8B)
              CALL JEECRA(JEXNUM(MAXFEM//'.GROUPENO',IGR),'LONUTI',
     &                    NTAIL,K8B)
              CALL JEVEUO(JEXNUM(MAXFEM//'.GROUPENO',IGR),'E',IAGNO)
            
C           COORDONNEES DES NOEUDS 
              IF (NDIM.EQ.3) THEN          
                DO 311 IFON2=1,NTAIL
                  IFON1 = IFON2+ZI(JFMULT-1+2*IFON-1)-1
                  INO = NNNTOT-NFTOT+IFON1+NCOMPT
                  ZR(IACOO2+3*(INO-1)-1+1) = ZR(JVA1-1+IFON1)
                  ZR(IACOO2+3*(INO-1)-1+2) = ZR(JVA2-1+IFON1)
                  ZR(IACOO2+3*(INO-1)-1+3) = ZR(JVA3-1+IFON1)
  311            CONTINUE
              ELSE
                IFON1 = ZI(JFMULT-1+2*IFON-1)
                INO = NNNTOT-NFTOT+IFON1+NCOMPT
                ZR(IACOO2+3*(INO-1)-1+1) = ZR(JVA1-1+IFON1)
                ZR(IACOO2+3*(INO-1)-1+2) = ZR(JVA2-1+IFON1)
              ENDIF
                          
C           CONNEXITE DES NOEUDS           
              DO 312 IFON2=1,NTAIL
                IFON1 = IFON2+ZI(JFMULT-1+2*IFON-1)-1
                INO = NNNTOT-NFTOT+IFON1+NCOMPT
                IF (NDIM.EQ.3) THEN
                  NUFON = ZI(JVA00-1+IFON1)
                  IF (NUFON.GT.1) THEN
                    IMA = NBMAX-MFTOT+ICOMPT+1
                    ZI(JTYPM2-1+IMA) =  NTSEG2
                    CALL JEECRA(JEXNUM(MAXFEM//'.CONNEX',IMA),
     &                    'LONMAX',2,K8B)
                    CALL JEVEUO(JEXNUM(MAXFEM//'.CONNEX',IMA),'E',JCONX)
                    DO 3121 J=1,2
                      ZI(JCONX-1+J)=INO+J-2
 3121               CONTINUE
                    ICOMPT = ICOMPT + 1
                    ZI(IAGMA-1+IFON2-1) = IMA
                  ENDIF
                ELSEIF (NTAIL.EQ.1) THEN
                  IMA = NBMAX-MFTOT+ICOMPT+1
                  ZI(JTYPM2-1+IMA) =  NTPOI1
                  CALL JEECRA(JEXNUM(MAXFEM//'.CONNEX',IMA),
     &                'LONMAX',1,K8B)
                  CALL JEVEUO(JEXNUM(MAXFEM//'.CONNEX',IMA),'E',JCONX)
                  ZI(JCONX) = INO
                  ICOMPT = ICOMPT + 1
                  ZI(IAGMA-1+IFON2) = IMA
                ENDIF
                ZI(IAGNO-1+IFON2) = INO            
 312          CONTINUE
  
 31         CONTINUE
            NCOMPT = NCOMPT + NFON
          ENDIF

 30     CONTINUE


      ENDIF

      
      END
