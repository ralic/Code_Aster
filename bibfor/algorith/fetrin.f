      SUBROUTINE FETRIN(NBSD,NBI,VDO,VD1,MATAS,VSDF,VDDL,COLAUX,COLAUI,
     &                  CHSECM,SDFETI,VLAGI,OPTION,CHSOL,TESTCO)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/06/2004   AUTEUR DURAND C.DURAND 
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
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:  CALCUL, POUR FETI, DU RESIDU INITIAL OU 
C    RECONSTRUCTION VECTEUR DEPLACEMENT GLOBAL SOLUTION SI CONVERGENCE
C
C      IN   NBSD: IN   : NOMBRE DE SOUS-DOMAINES
C      IN    NBI: IN   : NOMBRE DE NOEUDS D'INTERFACE
C      OUT   VDO: VR8  : VECTEUR OUTPUT DE TAILLE NBI SI OPTION=1 
C      IN    VD1: VR8  : VECTEUR AUXILIAIRE DE TAILLE NBI
C      IN  MATAS: CH19 : NOM DE LA MATR_ASSE GLOBALE
C      IN   VSDF: VIN  : VECTEUR MATR_ASSE.FETF INDIQUANT SI 
C                         SD FLOTTANT
C      IN   VDDL: VIN  : VECTEUR DES NBRES DE DDLS DES SOUS-DOMAINES
C      IN COLAUX: COL  : COLLECTION TEMPORAIRE DE REEL
C      IN COLAUI: COL  : COLLECTION TEMPORAIRE D'ENTIER
C      IN CHSECM: K19  : CHAM_NO SECOND MEMBRE GLOBAL
C      IN SDFETI: CH19 : SD DECRIVANT LE PARTIONNEMENT FETI
C      IN VLAGI : VR8  : VECTEUR LAGRANGE INITIAL OU SOLUTION
C      IN OPTION:  IN  : 1 -> RESIDU INIT., 2-> RECONSTRUCTION U SOL.
C      OUT CHSOL: CH19 : CHAM_NO SOLUTION GLOBAL SI OPTION=2
C     IN  TESTCO:  R8  : PARAMETRE DE TEST DE LA CONT. A L'INTERFACE
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       26/01/04 (OB): CREATION.
C----------------------------------------------------------------------
C RESPONSABLE BOITEAU O.BOITEAU
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER      NBSD,NBI,VDDL(NBSD),VSDF(NBSD),OPTION
      REAL*8       VDO(NBI),VD1(NBI),VLAGI(NBI),TESTCO
      CHARACTER*19 MATAS,CHSECM,SDFETI,CHSOL
      CHARACTER*24 COLAUX,COLAUI
      
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
      INTEGER      IDD,IFETM,NBDDL,IDD1,JXSOL,NEQ1,IFETS,IVALE,IVALG,
     &             NBSOL,LMAT,IFETC,NBBLOC,TYPSYM,J,IFM,NIV,NEQ,
     &             OPTIOB,IREFE,IDEEQ,INO,ICMP,NBDDL1,IPRNO,LPRNO,NEC2,
     &             IDIME,NBNO,ILPRN,IVALS,DVALG,IVALCS,IMSG(2)
      CHARACTER*8  K8BID
      CHARACTER*19 MATDD,CHSMDD,PRFCHN,CHAMLS
      CHARACTER*32 JEXNUM
      REAL*8       EPS,RAUX,RAUXL,RMIN,R8MIEM,RAUX1,UMOY
      
C CORPS DU PROGRAMME
      CALL JEMARQ()

C PLUS PETITE VALEUR REELLE
      RMIN=R8MIEM()
      
C RECUPERATION DU NIVEAU D'IMPRESSION
      CALL INFNIV(IFM,NIV)
            
C INIT. NBRE DE SECOND MEMBRES SOLUTION POUR RLTFR8
      NBSOL=1
      
      IF (OPTION.EQ.1) THEN
C INIT. VECTEUR SOLUTION LOCAL ET AUX.
        DO 10 J=1,NBI
          VD1(J)=0.D0
          VDO(J)=0.D0
   10   CONTINUE
      ELSE IF (OPTION.EQ.2) THEN
C OBJETS JEVEUX POINTANT SUR LA LISTE DES CHAM_NOS LOCAUX
        CALL JEVEUO(CHSOL//'.FETC','L',IFETS)
C PROF_CHNO DU DOMAINE GLOBAL   
        CALL JEVEUO(CHSOL//'.REFE','L',IREFE)
        PRFCHN=ZK24(IREFE+1)(1:19)
        CALL JEVEUO(PRFCHN//'.PRNO','L',IPRNO)
        CALL JEVEUO(PRFCHN//'.LPRN','L',ILPRN)
C LONGUEUR DU PREMIER ELEMENT DE LA COLLECTION .PRNO    
        LPRNO=ZI(ILPRN)
C NBRE DE NOEUDS DU MAILLAGE
        CALL JEVEUO(SDFETI//'.DIME','L',IDIME)  
        NBNO=ZI(IDIME+4)
C LONGUEUR DU VECTEUR D'ENTIERS CODES + 2
        NEC2=LPRNO/NBNO
C .VALE DU CHAM8NO SOLUTION GLOBAL
        CALL JEVEUO(CHSOL//'.VALE','E',IVALS)
      ELSE
        CALL UTMESS('F','FETRIN','OPTION DE CALCUL NON PREVUE !')
      ENDIF 
           
C OBJET JEVEUX POINTANT SUR LA LISTE DES MATR_ASSE
      CALL JEVEUO(MATAS//'.FETM','L',IFETM)
C OBJET JEVEUX POINTANT SUR LA LISTE DES CHAM_NO SECOND MEMBRE
      CALL JEVEUO(CHSECM//'.FETC','L',IFETC)
      
C ----------------------------------------------------------------------
C ----  BOUCLE SUR LES SOUS-DOMAINES
C ----------------------------------------------------------------------
      DO 40 IDD=1,NBSD
        IDD1=IDD-1
        
C MATR_ASSE ASSOCIEE AU SOUS-DOMAINE IDD      
        MATDD=ZK24(IFETM+IDD1)(1:19)
C DESCRIPTEUR DE LA MATRICE DU SOUS-DOMAINE     
        CALL JEVEUO(MATDD//'.&INT','L',LMAT)
C NOMBRE DE BLOC DE STOCKAGE DE LA MATRICE KI/ TYPE DE SYMETRIE
        NBBLOC=ZI(LMAT+13)
        TYPSYM=ZI(LMAT+4)                       
C NBRE DE DDL DU SOUS-DOMAINE   
        NBDDL=VDDL(IDD)
        NBDDL1=NBDDL-1
C VECTEUR AUXILIAIRE DE TAILLE NDDL(SOUS_DOMAINE_IDD)     
        CALL JEVEUO(JEXNUM(COLAUX,IDD),'E',JXSOL)       
C SECOND MEMBRE LOCAL AU SOUS-DOMAINE
        CHSMDD=ZK24(IFETC+IDD1)(1:19)
        CALL JEVEUO(CHSMDD//'.VALE','L',IVALE)

C EXTRACTION AU SOUS-DOMAINE IDD: (RIDD)T * LANDA (0 OU SOL)
        OPTIOB=2        
        CALL FETREX(OPTIOB,IDD,NBI,VLAGI,NBDDL,ZR(JXSOL),SDFETI,COLAUI)
        
C RECOPIE DE FIDD - (RIDD)T*LANDA (0 OU SOL) DANS VECTEUR AUX     
        DO 20 J=0,NBDDL1
          ZR(JXSOL+J)=ZR(IVALE+J)-ZR(JXSOL+J)
   20   CONTINUE
C -------------------------------------------------
C ----  SOUS-DOMAINE NON FLOTTANT
C -------------------------------------------------     
        IF (VSDF(IDD).EQ.-1) THEN 

C CALCUL DE (KI)-FI PAR MULT_FRONT   
          CALL RLTFR8(MATDD,NBDDL,ZR(JXSOL),NBSOL,TYPSYM)
          
C MONITORING
        IF (NIV.GE.3) THEN
          WRITE(IFM,*)
          WRITE(IFM,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'
          IF (OPTION.EQ.1) THEN
          WRITE(IFM,*)'<FETI/FETRIN> CALCUL (KI)-*(FI-RIT*LANDA0) I= ',
     &        IDD
          ELSE
          WRITE(IFM,*)'<FETI/FETRIN> CALCUL (KI)-*(FI-RIT*LANDAS) I= ',
     &        IDD           
          ENDIF                                   
          WRITE(IFM,*)'NBDDL/NBBLOC/TYPSYM',NBDDL,NBBLOC,TYPSYM
        ENDIF         

        ELSE
C -------------------------------------------------
C ----  SOUS-DOMAINE FLOTTANT
C -------------------------------------------------

          CALL UTMESS('F','FETRIN','SOUS-STRUCTURE FLOTTANTE'//
     &     ' POUR L''INSTANT PROSCRITE  AVEC FETI !')           
        ENDIF

C -------------------------------------------------
C ----  CALCUL RESIDU INITIAL
C -------------------------------------------------
        IF (OPTION.EQ.1) THEN
C RESTRICTION DU SOUS-DOMAINE IDD SUR L'INTERFACE: (RIDD) * ...
          OPTIOB=1      
          CALL FETREX(OPTIOB,IDD,NBDDL,ZR(JXSOL),NBI,VD1,SDFETI,COLAUI)
          
C CUMUL DANS LE VECTEUR VD1=SOMME(I=1,NBSD)(RI*((KI)+ *(FI-RIT*LANDA0))
          DO 30 J=1,NBI
            VDO(J)=VDO(J)+VD1(J)
   30     CONTINUE

C MONITORING
          IF (NIV.GE.3) THEN
            WRITE(IFM,*)'<FETI/FETRIN> CUMUL  R = R + '//
     &                  'RI*((KI)-*(FI-RIT*LANDA0)) '
            WRITE(IFM,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'
          ENDIF
          
        ELSE

C -------------------------------------------------
C ----  RECONSTRUCTION SOLUTION U GLOBALE
C -------------------------------------------------

C PROF_CHNO DU SOUS-DOMAINE IDD 
          CALL JEVEUO(CHSMDD//'.REFE','L',IREFE)
          PRFCHN=ZK24(IREFE+1)(1:19)
          CALL JEVEUO(PRFCHN//'.DEEQ','L',IDEEQ)

C .VALE DU CHAM_NO LOCAL IDD
          CHAMLS=ZK24(IFETS+IDD1)(1:19)
          CALL JEVEUO(CHAMLS//'.VALE','L',IVALCS)
          
C --------------------------------
C ----  BOUCLE SUR LES DDLS DU CHAM_NO LOCAL
C --------------------------------        
          DO 35 J=0,NBDDL1
          
C NUMERO DU NOEUD DU MAILLAGE (INO) ET DE SA COMPOSANTE (ICMP) CORRES
C PONDANT A L'EQUATION J DU CHAM_NO LOCAL       
            INO=ZI(IDEEQ+2*J)
            ICMP=ZI(IDEEQ+2*J+1)
C TESTS DE COHERENCE   
            CALL ASSERT(INO.GT.0)
            CALL ASSERT(ICMP.GT.0)
C DECALAGE DANS LE .VALE DU CHAM_NO GLOBAL CORRESPONDANT A (INO,ICMP)
            DVALG=ZI(IPRNO+(INO-1)*NEC2) + ICMP-1
C VALEUR UI A TRANSFERRER SUR LE CHAM_NO GLOBAL ET SUR LE LOCAL
            RAUXL=ZR(JXSOL+J)
            ZR(IVALCS+J)=RAUXL      
C TEST POUR VERIFIER LA CONTINUITE AUX INTERFACES
            IVALG=IVALS-1+DVALG
            RAUX=ABS(ZR(IVALG))
            IF (RAUX.GT.RMIN) THEN
              UMOY=(RAUX+RAUXL)*0.5D0       
              RAUX1=ABS((RAUX-RAUXL)/UMOY)
              IF (RAUX1.GT.TESTCO) THEN
                IMSG(1)=INO
                IMSG(2)=ICMP
                RAUX1=100.D0*RAUX1
                CALL UTDEBM('A','FETRIN','PB POTENTIEL DE CONTINUITE ?')
                CALL UTIMPI('L','INTERFACE (INO,ICMP)= ',2,IMSG)
                CALL UTIMPR('L','ERREUR INTERFACE (EN %)= ',1,RAUX1)
                CALL UTFINM()         
              ENDIF
            ELSE
              UMOY=RAUXL           
            ENDIF
             
C AFFECTATION EFFECTIVE DE UI VERS U         
            ZR(IVALG)=UMOY

C MONITORING
            IF (NIV.GE.5) THEN
              WRITE(IFM,*)NEC2
              WRITE(IFM,*)IDD,J,INO,ICMP,RAUX,RAUXL
            ENDIF 
   35     CONTINUE
   
C --------------------------------
C ----  FIN BOUCLE SUR LES DDLS DU CHAM_NO LOCAL
C --------------------------------              

C MONITORING
          IF (NIV.GE.3) THEN
            WRITE(IFM,*)'CALCUL  UI = (KI)-*(FI-RIT*LANDAS) '       
            WRITE(IFM,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'        
          ENDIF
          IF (NIV.GE.4)
     &      CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,CHAMLS(1:19),1,' ')
        ENDIF
                
   40 CONTINUE
C ----------------------------------------------------------------------
C ----  FIN BOUCLE SUR LES SOUS-DOMAINES
C ----------------------------------------------------------------------

      IF (OPTION.EQ.2) THEN
        CALL JEDETR('&&FETI.FETRIN.LOGI')      
      ENDIF                
      CALL JEDEMA()
      END
