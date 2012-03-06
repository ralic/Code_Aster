      SUBROUTINE OP5903(NBOCCI,IFM,NIV,COMPOR)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 05/03/2012   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE FLEJOU J.L.FLEJOU
C ======================================================================
C     COMMANDE:  DEFI_COMPOR MOT-CLE MULTIFIBRE

      CHARACTER*8 COMPOR, MATERI
      CHARACTER*1 K1BID
      INTEGER NBOCCI,IBID,NBG,NBGMAX,IMG,IG,IG1,JNFG,IAFF
      INTEGER NBVF,NBV,ICP,NBKIT,NBNVI(2),NCOMEL,NUMLC
      CHARACTER*8 SDGF,K8BID,KGROUP,MATOR
      CHARACTER*16 NOMREL,ALGO1D,DEFO,NOMKIT(2),LCOMEL(5),COMCOD,MOCLEF
      CHARACTER*16 COMCO2,TEXTE(2)
      CHARACTER*24 VNBFIG,RNOMGF
      INTEGER  IARG,IFM,NIV,IDBOR,IMI,IMK,IOCC,IRETT

C ----- DEBUT --- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON / IVARJE / ZI(1)
      REAL*8 ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16 ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32 JEXNUM,JEXNOM
C------------FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      CALL JEMARQ()
      
C on recupere les renseignements dans la SD_GROUP_FIBRE :
C noms de tous les groupes, nb maxi de groupes, nb de fibres par groupe
        NBVF=0
        MOCLEF='MULTIFIBRE'
        
        CALL GETVID(' ','GEOM_FIBRE',0,IARG,1,SDGF,IBID)
        VNBFIG = SDGF//'.NB_FIBRE_GROUPE'
        RNOMGF = SDGF//'.NOMS_GROUPES'
        CALL JEVEUO(VNBFIG,'L',JNFG)
        CALL JELIRA(VNBFIG,'LONMAX',NBGMAX,K1BID)
        CALL WKVECT(COMPOR//'.CPRK','G V K16',6*NBGMAX+1,IMK)
        CALL WKVECT('&&OP0059.NOMS_GROUPES', 'V V K8',NBGMAX,IMG)
        CALL WKVECT('&&OP0059.VERIF_AFFECT', 'V V I',NBGMAX,IAFF)
        DO 50 IG=1,NBGMAX
           ZI(IAFF-1+IG)=0
 50     CONTINUE
        IDBOR=0
 
        DO 25 IOCC=1,NBOCCI
          CALL GETVTX(MOCLEF,'GROUP_FIBRE',IOCC,IARG,0,K8BID,NBG)
          NBG=-NBG
          CALL GETVTX(MOCLEF,'GROUP_FIBRE',IOCC,IARG,NBG,ZK8(IMG),
     &                   IBID)
          CALL GETVID(MOCLEF,'MATER',IOCC,IARG,1,MATERI,IBID)
          CALL GETVTX(MOCLEF,'RELATION',IOCC,IARG,1,NOMREL,IBID)
          NCOMEL=1
          LCOMEL(NCOMEL)=NOMREL
          
C         AFFECTATION ALGO_1D ANALYTIQUE OU DEBORST
          ALGO1D='ANALYTIQUE'
          CALL LCCREE(NCOMEL, LCOMEL, COMCO2)
          CALL LCTEST(COMCO2,'MODELISATION','1D',IRETT)
          IF (IRETT.EQ.0) THEN
             TEXTE(1)='1D'
             TEXTE(2)=NOMREL
             CALL U2MESG('I','COMPOR1_48',2,TEXTE,0,0,0,0.D0)
             ALGO1D='DEBORST'
             IDBOR=IDBOR+1
          ENDIF
          
C         POUR COMPORTEMENTS KIT_DDI A COMPLETER
          CALL NMDOKI(MOCLEF,' ',NOMREL,IOCC,2,NBKIT,NOMKIT,
     &                NBNVI,NCOMEL,LCOMEL,NUMLC,NBV)
C         SAISIE ET VERIFICATION DU TYPE DE DEFORMATION UTILISEE
          CALL NMDOGD(MOCLEF,NOMREL,IOCC,NCOMEL,LCOMEL,DEFO)

C         APPEL A LCINFO POUR RECUPERER LE NOMBRE DE VARIABLES INTERNES
          CALL LCCREE(NCOMEL, LCOMEL, COMCOD)
          CALL LCINFO(COMCOD, NUMLC, NBV)
          
          DO 27 IG=1,NBG
C numero correspondant au nom
            CALL JENONU(JEXNOM(RNOMGF,ZK8(IMG+IG-1)),IG1)
            IF(IG1.EQ.0)THEN
              CALL U2MESK('F','MODELISA8_8',1,ZK8(IMG+IG-1))
            ENDIF
            ICP=IMK-1+(IG1-1)*6
            ZK16(ICP+1  )=ZK8(IMG+IG-1)
            ZK16(ICP+2)=MATERI
            ZK16(ICP+3)=NOMREL
            ZK16(ICP+4)=ALGO1D
            ZK16(ICP+5)=DEFO
            WRITE(ZK16(ICP+6),'(I16)')ZI(JNFG-1+IG1)
            ZI(IAFF-1+IG1)=1
 27       CONTINUE
 
C on met à jour le nombre de variables internes maxi
          NBVF=MAX(NBVF,NBV)
 25     CONTINUE
 
C verification de l'utilisation de comp_1d en lien avec fiche 15176
        IF (NBOCCI.GT.1) THEN
          IF (IDBOR.GE.1) CALL U2MESS('F','COMPOR1_15')
        ENDIF
        
C Verif tout affecte au moins une fois
C On marque par VIDE les groupes non affectes
        DO 51 IG=1,NBGMAX
          IF(ZI(IAFF-1+IG).EQ.0)THEN
            CALL JENUNO(JEXNUM(RNOMGF,IG),KGROUP)
            ICP=IMK-1+(IG-1)*6
            ZK16(ICP+1  )=KGROUP
            ZK16(ICP+2)='VIDE'
          ENDIF
 51     CONTINUE
 
C On recupere le nom du materiau pour la torsion et on le met à la fin
        CALL GETVID(' ','MATER_SECT',0,IARG,1,MATOR,IBID)
        ZK16(IMK-1+NBGMAX*6+1)=MATOR
        CALL WKVECT(COMPOR//'.CPRI', 'G V I',3,IMI)
C type 3 = multifibre
        ZI(IMI)=3
        ZI(IMI+1)=NBVF
        ZI(IMI+2)=NBGMAX
        
C FIN ------------------------------------------------------------------

      CALL JEDEMA()
      END
