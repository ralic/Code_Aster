      SUBROUTINE ALFETI(SDFETI,MATAS,CHSECM,CHSOL,NITER,EPSI,CRITER,
     &                  TESTCO,NBREOR,TYREOR,PRECO,SCALIN)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 23/08/2004   AUTEUR BOITEAU O.BOITEAU 
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
C TOLE CRP_20
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:  RESOLUTION FETI EN REEL SYMETRIQUE
C                          CF. ALGO. 9 DE LA NOTE HI-23/03/009
C     ------------------------------------------------------------------
C     IN  SDFETI : CH19 : SD DECRIVANT LE PARTIONNEMENT FETI
C     IN  MATAS  : CH19 : NOM DE LA MATR_ASSE GLOBALE
C     IN CHSECM  : CH19 : CHAM_NO SECOND MEMBRE GLOBAL
C     OUT CHSOL  : CH19 : CHAM_NO SOLUTION GLOBAL
C     IN  NITER  :  IN  : NOMBRE D'ITERATIONS MAXIMALES ADMISSIBLES DU
C                         GCPPC DE FETI
C     IN  EPSI   :  R8  : CRITERE D'ARRET RELATIF DU GCPPC
C     IN  CRITER :  K24 : STRUCTURE DE DONNEE STOCKANT INFOS DE CV
C     IN  TESTCO :  R8  : PARAMETRE DE TEST DE LA CONT. A L'INTERFACE
C     IN  NBREOR :  IN  : NBRE DE DD A REORTHOGONALISER
C     IN  TYREOR :  K24 : TYPE DE REORTHOGONALISATION
C     IN  PRECO  :  K24 : TYPE DE PRECONDITIONNEMENT
C     IN  SCALIN :  K24 : PARAMETRE DE SCALING DANS LE PRECOND
C     ------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       26/01/04 (OB): CREATION.
C       03/06/04 (OB): MODIFICATION POUR MODES DE CORPS RIGIDES.
C----------------------------------------------------------------------
C RESPONSABLE BOITEAU O.BOITEAU
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER      NITER,NBREOR
      REAL*8       EPSI,TESTCO
      CHARACTER*19 SDFETI,CHSECM,CHSOL,MATAS
      CHARACTER*24 CRITER,TYREOR,PRECO,SCALIN
      
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      
C DECLARATION VARIABLES LOCALES
      INTEGER      IDIME,NBSD,NBI,IVLAGI,IFETF,IDD,IFETH,NB,I,IRZ,JCRR,
     &             IRR,IRG,IRP,IR1,IR2,ITER,IFM,NIV,IRET,JCRI,NBI1,JCRK,
     &             OPTION,IPSRO,IDDRO,NBREO2,IDDFRO,IAUX1,IAUX2,ITER1,J,
     &             NBREO1,IAUX3,IR3,IRH,IAD,JGI,JGITGI,DIMGI,ITEST,
     &             OPTIOP,IVLAGB
      REAL*8       R8NRM2,ANORM,ANORMK,ANORM0,EPSIK,PARAAF,R8DOT,ALPHA,
     &             ALPHAN,ALPHAD,BETA,BETAD,BETAN,R8MIEM,RMIN,RAUX
      CHARACTER*8  NOMSD,K8BID
      CHARACTER*24 COLAUX,COLAUI,COLAU2,NOMGI,NOMGGT
      CHARACTER*32 JEXNOM,JEXNUM
      LOGICAL      REORTH,IGSMKP,GS,LUMPE,LRIGID
      
C CORPS DU PROGRAMME
      CALL JEMARQ()
C PLUS PETITE VALEUR REELLE DISCERNABLE
      RMIN=R8MIEM()**(2.0D+0/3.0D+0)      
C RECUPERATION DU NIVEAU D'IMPRESSION
      CALL INFNIV(IFM,NIV)
C-----PARAMETRE D'AFFICHAGE DE LA DECROISSANCE DU RESIDU
C (SI ON GAGNE PARAAF * 100%)      
      PARAAF = 0.1D0
                  
C ----------------------------------------------------------------------
C ----  PREPARATION DES DONNEES
C ----------------------------------------------------------------------
      CALL JEVEUO(SDFETI(1:19)//'.DIME','L',IDIME)
C NOMBRE DE SOUS-DOMAINES       
      NBSD=ZI(IDIME)      
C NOMBRE DE DDLS D'INTERFACE
      NBI=ZI(IDIME+3)
      NBI1=NBI-1
C TRAITEMENT DU NOMBRE D'ITERATION SI NULLE      
      IF (NITER.EQ.0) THEN
        NITER = MAX(NBI/2,1)
        WRITE(IFM,1060)NITER
      ENDIF          
C VECTEUR TEMPORAIRE DES INCONNUES D'INTERFACE DE TYPE LAGRANGE      
      CALL WKVECT('&&FETI.LAGR.INTERFACE','V V R',NBI,IVLAGI)
      CALL WKVECT('&&FETI.LAGR.INTERFACB','V V R',NBI,IVLAGB)
      
C VECTEURS TEMPORAIRES           
      CALL WKVECT('&&FETI.RESIDU.R','V V R',NBI,IRR)
      CALL WKVECT('&&FETI.REPROJ.G','V V R',NBI,IRG)
      CALL WKVECT('&&FETI.REPCPJ.H','V V R',NBI,IRH)
      CALL WKVECT('&&FETI.DD.P','V V R',NBI,IRP)
      CALL WKVECT('&&FETI.FIDD.Z','V V R',NBI,IRZ)      
      CALL WKVECT('&&FETI.VECNBI.AUX1','V V R',NBI,IR1)
      CALL WKVECT('&&FETI.VECNBI.AUX2','V V R',NBI,IR2)
      CALL WKVECT('&&FETI.VECNBI.AUX3','V V R',NBI,IR3)
C VECTEURS TEMPORAIRES POUR TEST
      IF (NIV.GE.6)
     &  CALL WKVECT('&&FETI.TEST','V V R',NBI,ITEST)
            
C VECTEUR INDIQUANT SI UN SOUS-DOMAINE EST FLOTTANT                 
      CALL JEVEUO(MATAS//'.FETF','L',IFETF)
C VECTEUR DE LA LISTE DES NOMBRES DE DDLS PAR SOUS-DOMAINE
      CALL JEVEUO(SDFETI(1:19)//'.FETH','L',IFETH)

C PRECONDITIONNEMENT LUMPE OU NON ?
      IF (PRECO(1:5).EQ.'LUMPE') THEN
        LUMPE=.TRUE.
      ELSE
        LUMPE=.FALSE.
      ENDIF
      IF ((PRECO(1:4).EQ.'SANS').AND.(SCALIN(1:4).NE.'SANS'))
     &   CALL UTMESS('F','ALFETI','SCALING LICITE QU''AVEC '//
     &     'PRECONDITIONNEMENT !')                        
C COLLECTIONS TEMPORAIRES DE VECTEURS AUXILAIRES DE TAILLES VARIABLES
      COLAUX='&&FETI.COLLECTIONR'
      COLAUI='&&FETI.COLLECTIONI'      
      CALL JECREC(COLAUX,'V V R','NO','DISPERSE','VARIABLE',NBSD)
      IF (LUMPE) THEN
        COLAU2='&&FETI.COLLECTIONL'            
        CALL JECREC(COLAU2,'V V R','NO','DISPERSE','VARIABLE',NBSD)
      ENDIF      
      DO 10 IDD=1,NBSD
        CALL JENUNO(JEXNUM(SDFETI//'.FETA',IDD),NOMSD)
        CALL JECROC(JEXNOM(COLAUX,NOMSD))
        NB=ZI(IFETH+IDD-1)
        CALL JEECRA(JEXNOM(COLAUX,NOMSD),'LONMAX',NB,K8BID)
        IF (LUMPE) THEN
          CALL JECROC(JEXNOM(COLAU2,NOMSD))
          CALL JEECRA(JEXNOM(COLAU2,NOMSD),'LONMAX',NB,K8BID)   
        ENDIF
   10 CONTINUE
      CALL FETING(NBSD,SDFETI,CHSECM,ZI(IFETH),COLAUI)

C ---------------------------------------------------
C SI REORTHOGONALISATION DES DIRECTIONS DE DESCENTE
C ---------------------------------------------------
      IF (TYREOR(1:4).NE.'SANS') THEN      
        REORTH=.TRUE.
C TRAITEMENT DU NOMBRE DE DD REORTHOGONALISEES SI NULLE      
        IF (NBREOR.EQ.0) THEN
          NBREOR = MAX(NITER/4,1)
          WRITE(IFM,1065)NBREOR   
        ENDIF
        NBREO1=NBREOR+2
        NBREO2=NBREO1*NBI       
        CALL WKVECT('&&FETI.PS.REORTHO.R','V V R',NBREO1,IPSRO)
        CALL WKVECT('&&FETI.DD.REORTHO.R','V V R',NBREO2,IDDRO)
        CALL WKVECT('&&FETI.FIDD.REORTHO.R','V V R',NBREO2,IDDFRO)
        IGSMKP=.FALSE.
        GS=.FALSE.      
        IF (TYREOR.EQ.'IGSM') IGSMKP=.TRUE.
        IF (TYREOR.EQ.'GS') GS=.TRUE.
      ELSE
        REORTH=.FALSE.
      ENDIF
                 
C ----------------------------------------------------------------------
C ----  INITIALISATION DE L'ALGORITHME FETI
C ----------------------------------------------------------------------

C CALCUL DE GI,  (GIT)GI ET DIMGI
C NOMS DES OBJETS JEVEUX STOCKANT GI ET (GI)T*GI SI LRIGID=.TRUE.
      NOMGI='&&FETI.GI.R'
      NOMGGT='&&FETI.GITGI.R'      
      CALL FETGGT(NBSD,MATAS,ZI(IFETF),ZI(IFETH),SDFETI,LRIGID,NBI,
     &            COLAUI,NOMGI,JGI,NOMGGT,JGITGI,DIMGI)
      
C ---------------------------------------------------
C MONITORING
C ---------------------------------------------------
      IF (NIV.GE.3) THEN
        WRITE(IFM,*)
        WRITE(IFM,*)'*****************************************'
        WRITE(IFM,*)'<FETI/ALFETI> PARAM DE RESOLUTION ',NITER,EPSI
        WRITE(IFM,*)'<FETI/ALFETI> PARAM DE PRECONDI ',PRECO(1:5),
     &              ' ',SCALIN(1:4)   
        WRITE(IFM,*)'<FETI/ALFETI> PARAM DE REORTHOGONALISATION ',
     &              REORTH,TYREOR(1:6),NBREOR
        WRITE(IFM,*)'<FETI/ALFETI> PARAM DE TEST DE CONTINUITE ',
     &              TESTCO
        WRITE(IFM,*)'<FETI/ALFETI> NB SOUS-DOMAINES ',NBSD
        WRITE(IFM,*)'<FETI/ALFETI> NB DE MODES RIGIDES ',DIMGI     
        WRITE(IFM,*)'******************************************'     
      ENDIF
               
C ---------------------------------------------------
C CALCUL DU VECTEUR LAGRANGE_FETI INITIAL LANDA0 (ZR(IVLAGI))
C ---------------------------------------------------
      CALL FETINL(NBI,ZR(IVLAGI),ZR(JGI),JGITGI,MATAS,CHSECM,LRIGID,
     &            DIMGI,NBSD,ZI(IFETF),ZI(IFETH))

C ---------------------------------------------------      
C CALCUL DU RESIDU INITIAL (ZR(IRR)): R0=OPFETI*LANDA0 - D
C ---------------------------------------------------
      OPTION=1
      CALL FETRIN(NBSD,NBI,ZR(IRR),ZR(IR1),MATAS,ZI(IFETF),ZI(IFETH),
     &            COLAUX,COLAUI,CHSECM,SDFETI,ZR(IVLAGI),OPTION,CHSOL,
     &            TESTCO,LRIGID,DIMGI,IRR,JGI,JGITGI)

C ---------------------------------------------------     
C CALCUL DU RESIDU PROJETE INITIAL (ZR(IRP)): G0=P*R0
C ---------------------------------------------------
      OPTIOP=1
      CALL FETPRJ(NBI,ZR(IRR),ZR(IRG),ZR(JGI),JGITGI,LRIGID,DIMGI,
     & OPTIOP)
     
C ---------------------------------------------------      
C CALCUL DU RESIDU PRECOND PROJETE P INITIAL (ZR(IRH)): H0=P*M-1*G0
C ---------------------------------------------------
      CALL FETPRC(NBSD,NBI,ZR(IRG),ZR(IR1),ZR(IR2),MATAS,ZI(IFETH),
     &            COLAUX,COLAUI,SDFETI,PRECO,COLAU2)
      OPTIOP=1
      CALL FETPRJ(NBI,ZR(IR2),ZR(IRH),ZR(JGI),JGITGI,LRIGID,DIMGI,
     &            OPTIOP)
     
C ---------------------------------------------------         
C CALCUL DE LA DD INITIALE (ZR(IRP)): P0=H0 (=G0 SI NON PRECOND)
C ---------------------------------------------------
      DO 20 I=0,NBI1      
        ZR(IRP+I)=ZR(IRH+I)
   20 CONTINUE
   
C ---------------------------------------------------
C CALCUL DE ALPHAN0 = G0.P0 (=G0.G0 SI NON PRECOND)
C ---------------------------------------------------      
      ALPHAN=R8DOT(NBI,ZR(IRG),1,ZR(IRP),1)

      DO 25 I=0,NBI1
        ZR(IRR+I)=ZR(IRG+I)
   25 CONTINUE
C ---------------------------------------------------      
C CALCUL DE LA NORME DU RESIDU PROJETE INITIAL (ANORM) POUR TEST
C D'ARRET: EPSIK
C ---------------------------------------------------
      OPTIOP=1
      CALL FETPRJ(NBI,ZR(IRR),ZR(IR1),ZR(JGI),JGITGI,LRIGID,DIMGI,
     &            OPTIOP)
      ANORM=R8NRM2(NBI,ZR(IR1),1)
      EPSIK=EPSI*ANORM 
      IF (NIV.GE.2) WRITE (IFM,1020)ANORM,EPSIK,EPSI

C ---------------------------------------------------
C PREPARATION DE L'OBJET JEVEUX CRITER
C ---------------------------------------------------
      CALL JEEXIN(CRITER(1:19)//'.CRTI',IRET)
      IF (IRET.EQ.0) THEN
        CALL WKVECT(CRITER(1:19)//'.CRTI','V V I',1,JCRI)
        CALL WKVECT(CRITER(1:19)//'.CRTR','V V R8',1,JCRR)
        CALL WKVECT(CRITER(1:19)//'.CRDE','V V K16',2,JCRK)
        ZK16(JCRK)='ITER_GCPC'
        ZK16(JCRK+1)='RESI_GCPC'
      ENDIF
      CALL JEVEUO(CRITER(1:19)//'.CRTI','E',JCRI)
      CALL JEVEUO(CRITER(1:19)//'.CRTR','E',JCRR)
                               

C ---------------------------------------------------
C CONSTANTES POUR AFFICHAGE (ANORMK) ET NORME INITIALE (ANORM0)
C ---------------------------------------------------
      ANORMK=ANORM*PARAAF
      ANORM0=ANORM
      IF (ANORM0.LT.RMIN) THEN
        ANORM0=RMIN
        CALL UTMESS('A','ALFETI','RESIDU INITIAL NUL '//
     &     'SOLUTION LANDAS=LANDA0 !')
     
C -----------------------------
C CALCUL SOLUTION U GLOBALE
C -----------------------------

C RECALCUL DU RESIDU AVEC CE NOUVEAU LANDA_SOL (POUR ALPHA) SI MODES
C DE CORPS RIGIDES
        IF (LRIGID) THEN           
          OPTION=1
          CALL FETRIN(NBSD,NBI,ZR(IRR),ZR(IR1),MATAS,ZI(IFETF),
     &              ZI(IFETH),COLAUX,COLAUI,CHSECM,SDFETI,ZR(IVLAGI),
     &              OPTION,CHSOL,TESTCO,LRIGID,DIMGI,IRR,JGI,JGITGI)
        ENDIF
C CALCUL DE USOL LOCALE PUIS GLOBAL PROPREMENT DIT
C UI_SOL = (KI)+ * (FI - RIT*LANDA_SOL) - BI*ALPHAI_SOL   
        OPTION=2
        CALL FETRIN(NBSD,NBI,ZR(IRR),ZR(IR2),MATAS,ZI(IFETF),
     &      ZI(IFETH),COLAUX,COLAUI,CHSECM,SDFETI,ZR(IVLAGI),OPTION,
     &      CHSOL,TESTCO,LRIGID,DIMGI,IRR,JGI,JGITGI)     
        GOTO 200
      ENDIF

C ---------------------------------------------------
C ATTENTION: ON SAUVEGARDE LA VALEUR INITIALE LANDA0 DANS ZR(IVLAGB)
C L'ANCIENNE VARIABLE ZR(IVALGI) DEVIENT UN LANDA_DE_TRAVAIL DU GCPC
C QUI EST INITIALISEE A ZERO.
C A LA FIN DU GCPC ON RECONSTRUIT LE VRAI LANDA SOLUTION VIA
C     LANDA_SOL = LANDA0 + P * LANDA_DE_TRAVAIL_CONVERGE
C ---------------------------------------------------
      DO 30 I=0,NBI1
        ZR(IVLAGB+I)=ZR(IVLAGI+I)
        ZR(IVLAGI+I)=0.D0
   30 CONTINUE
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
C ----  BOUCLES DU GCPPC DE FETI
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      DO 100 ITER=0,NITER

        ITER1=ITER+1
C STOCKAGE PK SI REORTHO
        IF (REORTH) THEN
          IF (ITER.LT.NBREOR) THEN
            IAUX1=ITER1*NBI
          ELSE
            IAUX1=0
          ENDIF
          DO 50 I=0,NBI1
            ZR(IDDRO+IAUX1+I)=ZR(IRP+I)
   50     CONTINUE
        ENDIF

C ---------------------------------------------------
C ----  CALCUL DU PRODUIT OPERATEUR_FETI * PK = ZK
C ---------------------------------------------------
        CALL FETFIV(NBSD,NBI,ZR(IRP),ZR(IR2),ZR(IRZ),MATAS,ZI(IFETF),
     &    ZI(IFETH),COLAUX,COLAUI,SDFETI)

C STOCKAGE ZK=FI*PK SI REORTHO
        IF (REORTH)THEN
          DO 55 I=0,NBI1
            ZR(IDDFRO+IAUX1+I)=ZR(IRZ+I)
   55     CONTINUE
        ENDIF

C ---------------------------------------------------
C ----  CALCUL DE ALPHAK = GK.PK/ZK.PK = ALPHANK/ALPHADK
C ---------------------------------------------------
        ALPHAD=R8DOT(NBI,ZR(IRP),1,ZR(IRZ),1)
        IF (ALPHAD.LT.RMIN) THEN
          ALPHAD=RMIN
          CALL UTMESS('A','ALFETI','PB DIVISION PAR ZERO'//
     &     'DANS LA CONSTRUCTION DU ALPHA !')     
        ENDIF
        ALPHA=ALPHAN/ALPHAD
        
C STOCKAGE ZK.PK SI REORTHO
        IF (REORTH) THEN
          IF (ITER.LT.NBREOR) THEN
            IAUX1=ITER1
          ELSE
            IAUX1=0
          ENDIF
          ZR(IPSRO+IAUX1)=ALPHAD
        ENDIF

C STOCKAGE ANCIENNE DIRECTION DE DESCENTE SI TEST
        IF (NIV.GE.6) THEN
          OPTIOP=1
          CALL FETPRJ(NBI,ZR(IRR),ZR(IR1),ZR(JGI),JGITGI,LRIGID,DIMGI,
     &                OPTIOP)
          DO 60 I=0,NBI1
            ZR(ITEST+I)=ZR(IR1+I)
   60     CONTINUE
        ENDIF
        
C ---------------------------------------------------
C ----  CALCUL NOUVEAUX VECTEUR D'INTERFACE ET RESIDU
C ---- (ZR(IVLAGI)) LANDAK+1 = LANDAK + ALPHAK * PK  
C ---- (ZR(IRR)))   RK+1     = RK     - ALPHAK * ZK
C ---------------------------------------------------   
        CALL R8AXPY(NBI,ALPHA,ZR(IRP),1,ZR(IVLAGI),1)        
        CALL R8AXPY(NBI,-ALPHA,ZR(IRZ),1,ZR(IRR),1)

C ---------------------------------------------------
C ----  CALCUL DE LA PROJECTION 1 (ZR(IRG)): GK+1 = P * RK+1
C ---------------------------------------------------
        OPTIOP=1
        CALL FETPRJ(NBI,ZR(IRR),ZR(IRG),ZR(JGI),JGITGI,LRIGID,DIMGI,
     &              OPTIOP)
     
C TEST ORTHOGONALITE DU GCPPC     
        IF (NIV.GE.6) THEN
          RAUX=R8DOT(NBI,ZR(IRG),1,ZR(ITEST),1)
          WRITE(IFM,*)'TEST <PRI,PRI-1>',RAUX
          RAUX=R8DOT(NBI,ZR(IRG),1,ZR(IRP),1)
          WRITE(IFM,*)'TEST <PRI,DI-1>',RAUX
        ENDIF
                
C ---------------------------------------------------
C ----  CALCUL TEST D'ARRET ET AFFICHAGE
C ---------------------------------------------------
        ANORM=R8NRM2(NBI,ZR(IRG),1)
        IF (ANORM.LE.ANORMK) THEN
          IF (NIV.EQ.1) WRITE (*,1041)ITER1,ANORM,ANORM/ANORM0
          ANORMK=ANORM*PARAAF
        ENDIF
        IF (NIV.GE.2) THEN
          WRITE(IFM,*)
          WRITE(IFM,*)'******************************************'     
          WRITE(IFM,1041)ITER1,ANORM,ANORM/ANORM0
          WRITE(IFM,*)'******************************************'     
        ENDIF

C -----------------------------
C TEST DE CONVERGENCE
C -----------------------------
        IF (ANORM.LT.EPSIK) THEN
          WRITE(IFM,1040)ANORM0,ANORM,ANORM/ANORM0
          WRITE(IFM,1050)ITER1
          ZI(JCRI)=ITER1
          ZR(JCRR)=ANORM

C -----------------------------
C CALCUL SOLUTION U GLOBALE
C -----------------------------

C UNE FOIS LANDA_DE_TRAVAIL-CV TROUVE ON RECONSTRUIT LE VRAI LANDA
C     LANDA_SOL =  LANDA0 +    P * LANDA_DE_TRAVAIL_CONVERGE
C     ZR(IVLAGI)  ZR(IVLAGB)        ZR(IR1)
          OPTIOP=1
          CALL FETPRJ(NBI,ZR(IVLAGI),ZR(IR1),ZR(JGI),JGITGI,LRIGID,
     &                DIMGI,OPTIOP)
          DO 65 I=0,NBI1
            ZR(IVLAGI+I)=ZR(IVLAGB+I)+ZR(IR1+I)
   65     CONTINUE
C RECALCUL DU RESIDU AVEC CE NOUVEAU LANDA_SOL (POUR ALPHA) SI MODES
C DE CORPS RIGIDES
          IF (LRIGID) THEN           
            OPTION=1
            CALL FETRIN(NBSD,NBI,ZR(IRR),ZR(IR1),MATAS,ZI(IFETF),
     &              ZI(IFETH),COLAUX,COLAUI,CHSECM,SDFETI,ZR(IVLAGI),
     &              OPTION,CHSOL,TESTCO,LRIGID,DIMGI,IRR,JGI,JGITGI)
          ENDIF
C CALCUL DE USOL LOCALE PUIS GLOBAL PROPREMENT DIT
C UI_SOL = (KI)+ * (FI - RIT*LANDA_SOL) - BI*ALPHAI_SOL   
          OPTION=2
          CALL FETRIN(NBSD,NBI,ZR(IRR),ZR(IR2),MATAS,ZI(IFETF),
     &      ZI(IFETH),COLAUX,COLAUI,CHSECM,SDFETI,ZR(IVLAGI),OPTION,
     &      CHSOL,TESTCO,LRIGID,DIMGI,IRR,JGI,JGITGI)
          GOTO 200
        ENDIF
         
C ---------------------------------------------------
C ----  PRECONDITIONNEMENT (ZR(IRH)): HK+1 =P*A*M-1*A*GK+1
C ---------------------------------------------------   
C PHASE DE SCALING 1 (ZR(IR1)): AUX1 = A * GK+1
        CALL FETSCA(NBI,ZR(IRG),ZR(IR1),SCALIN,SDFETI)
C CALCUL DU RESIDU PRECOND PROJETE P INITIAL (ZR(IR2)): AUX2 = M-1*AUX1
        CALL FETPRC(NBSD,NBI,ZR(IR1),ZR(IR3),ZR(IR2),MATAS,ZI(IFETH),
     &              COLAUX,COLAUI,SDFETI,PRECO,COLAU2)
C PHASE DE SCALING 2 (ZR(IR1)): AUX1 = A * AUX2
        CALL FETSCA(NBI,ZR(IR2),ZR(IR3),SCALIN,SDFETI)
C CALCUL DE LA PROJECTION 2 (ZR(IRH)): HK+1 = P * AUX1
        OPTIOP=1
        CALL FETPRJ(NBI,ZR(IR3),ZR(IRH),ZR(JGI),JGITGI,LRIGID,DIMGI,
     &              OPTIOP)

C ---------------------------------------------------
C ----  NEW DIRECTION DE DESCENTE (ZR(IRP)): PK+1=HK+1 + ...
C       AVEC EVENTUELLEMENT UNE PHASE DE REORTHONORMALISATION
C ---------------------------------------------------           
        CALL FETREO(REORTH,ALPHAN,NBI,IRG,ITER,NBREOR,IRP,IDDFRO,
     &              IDDRO,IPSRO,GS,IGSMKP,RMIN,IRH)

C TEST ORTHOGONALITE DU GCPPC     
        IF (NIV.GE.6) THEN
          RAUX=R8DOT(NBI,ZR(IRZ),1,ZR(IRP),1)
          WRITE(IFM,*)'TEST <DI,FI*DI-1>',RAUX
        ENDIF  
          
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
C ----  FIN BOUCLES DU GCPPC DE FETI
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
  100 CONTINUE    

C ----------------------------------------------------------------------
C ----  POST-TRAITEMENTS DES DONNEES
C ----------------------------------------------------------------------
      
C NON CONVERGENCE
      CALL UTDEBM('F','ALFETI','NON CONVERGENCE')
      CALL UTIMPI('L','  NOMBRE D''ITERATIONS: ',1,ITER1)
      CALL UTIMPR('L','  NORME DU RESIDU ABS: ',1,ANORM)
      CALL UTIMPR('L','  NORME DU RESIDU REL: ',1,ANORM/ANORM0)
      CALL UTFINM()
      
C FORMAT AFFICHAGE
 1020 FORMAT(/'   * FETI: NORME DU RESIDU INITIAL =',D11.4,/,
     & '   *        NORME DU RESIDU A ATTEINDRE EN ABS/RELA=',
     & D11.4,D11.4)
 1040 FORMAT('   * FETI: NORME DU RESIDU INITIAL/FINAL/RELATIF=',
     &         D11.4,D11.4,D11.4)
 1041 FORMAT('   * ITERATION',I5,' NORME DU RESIDU EN ABS/RELA =',
     &         D11.4,D11.4)     
 1050 FORMAT(1X,/,2X,32 ('*')/'  * CONVERGENCE EN ',I4,
     &       ' ITERATIONS'/2X,32 ('*'),/)
 1060 FORMAT('! FETI: NMAX_ITER FIXE A NB_NOEUD_INTERFACE/2= ',I6,' !')
 1065 FORMAT('! FETI: NB_REORTHO_DD FIXE A NMAX_ITER/4= ',I6,' !')      

C DESTRUCTION DES OBJETS JEVEUX TEMPORAIRES
  200 CONTINUE
      CALL JEDETR('&&FETI.LAGR.INTERFACE')
      CALL JEDETR('&&FETI.LAGR.INTERFACB')      
      CALL JEDETR('&&FETI.RESIDU.R')
      CALL JEDETR('&&FETI.REPROJ.G')
      CALL JEDETR('&&FETI.REPCPJ.H')
      CALL JEDETR('&&FETI.DD.P')
      CALL JEDETR('&&FETI.FIDD.Z')      
      CALL JEDETR('&&FETI.VECNBI.AUX1')
      CALL JEDETR('&&FETI.VECNBI.AUX2')
      CALL JEDETR('&&FETI.VECNBI.AUX3')                              
      CALL JEDETR('&&FETI.COLLECTIONR')
      CALL JEDETR('&&FETI.COLLECTIONI')
      IF (LUMPE) CALL JEDETR('&&FETI.COLLECTIONL') 
      CALL JEDETR('&&FETI.PS.REORTHO.R')
      CALL JEDETR('&&FETI.DD.REORTHO.R')
      CALL JEDETR('&&FETI.FIDD.REORTHO.R')
      IF (LRIGID) THEN
        CALL JEDETR(NOMGI)
        CALL JEDETR(NOMGGT)
      ENDIF
      IF (NIV.GE.6)
     &  CALL JEDETR('&&FETI.TEST')      
      CALL JEDEMA()
      END
