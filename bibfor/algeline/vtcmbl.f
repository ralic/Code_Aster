      SUBROUTINE VTCMBL(NBCMB,TYPCST,CONST,TYPECH,NOMCH,TYPRES,CHPRES)
C     ------------------------------------------------------------------
C     COMBINAISON LINEAIRE DE CHAM_NO OU DE CHAM_ELEM
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 20/06/2005   AUTEUR BOITEAU O.BOITEAU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     -----------------------------------------------------------------
C     *  LES CHAM_NOS OU CHAM_ELEMS SONT REELS OU COMPLEXES
C     *  LES SCALAIRES SONT REELS OU COMPLEXES
C     -----------------------------------------------------------------
C IN  : NBCOMB : IS  : NOMBRE DE CHAM_GDS A COMBINER
C IN  : TYPCST : K1  : TYPE DES CONSTANTES (R OU C, OU I )
C IN  : CONST  : R8  : TABLEAU DES COEFFICIENTS
C IN  : TYPECH : K1  : TYPE DES CHAM_GDS   (R OU C)
C IN  : NOMCH  : K19 : NOMS DES CHAM_GDS
C IN  : TYPRES : K1  : TYPE DU CHAMP RESULTAT (R OU C)
C IN  : CHPRES : K19 : NOM DU CHAMP RESULTAT 
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
      
C DECLARATION PARAMETRES D'APPELS
      INTEGER       NBCMB
      REAL*8        CONST(*)
      CHARACTER*(*) TYPCST(*),TYPECH(*),NOMCH(*),TYPRES,CHPRES
      
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------

C DECLARATION VARIABLES LOCALES
      INTEGER      IBID,IREFE,IDIME,NBSD,IFETC,IDD,KFETC,I,IVFETC,IFM,
     &             NIV,ICMB,NBVALE,NBDESC,NBREFE,JDESC,JREFE,IRET,KDESC,
     &             KVALE,KREFE,LVALE,ICONST,JVALE,IVAL,IINF,ILIMPI
      REAL*8       DIMAG
      COMPLEX*16   C8CST
      CHARACTER*4  DOCU,TYPE,KBID
      CHARACTER*5  REFE,DESC,VALE,FETC,FETA
      CHARACTER*8  K8B,NOMSD
      CHARACTER*19 CH19,CH19R
      CHARACTER*24 METHOD,SDFETI,METHO1,SDFET1,K24B,INFOFE,SDFETA
      CHARACTER*32 JEXNUM
      LOGICAL      LFETI,IDDOK
C     ------------------------------------------------------------------

      CALL JEMARQ()
C RECUPERATION ET MAJ DU NIVEAU D'IMPRESSION
      CALL INFNIV(IFM,NIV)
      
C-----------------------------------------------------------------------
C --- PHASE D'INITIALISATION
C-----------------------------------------------------------------------
      TYPE = TYPRES    
      CH19 = NOMCH(1) 

C CHAM_NO OU CHAM_ELEM ?
      K24B=CH19//'.DESC'
      CALL JEEXIN(K24B,IBID)
      IF (IBID.GT.0) THEN
        K24B=CH19//'.DESC'
        CALL JELIRA(K24B,'DOCU',IBID,DOCU)
      ELSE
        K24B=CH19//'.CELD'      
        CALL JELIRA(K24B,'DOCU',IBID,DOCU)
      END IF

C INIT. POUR FETI: NOMBRE DE SOUS-DOMAINES
      NBSD=0
      METHOD='XXXX'
      SDFETI='XXXX'
      
C INIT. DE BASE      
      IF (DOCU.EQ.'CHNO') THEN
         REFE='.REFE'
         DESC='.DESC'
         VALE='.VALE'                    
      ELSEIF (DOCU.EQ.'CHML') THEN
         REFE='.CELK'
         DESC='.CELD'
         VALE='.CELV'
      ELSE
         CALL UTMESS('F','VTCMBL','ON NE TRAITE QUE DES '//      
     &                                  '"CHAM_NO" OU DES "CHAM_ELEM".')
      ENDIF

      IF (DOCU.EQ.'CHNO') THEN
C FETI OR NOT ?
C ON VERIFIE QUE TOUTE LES CHAM_NO DE LA LISTE SONT HOMOGENES
        K24B=CH19//REFE
        CALL JEEXIN(K24B,IRET)
        IF (IRET.NE.0) THEN
          CALL JELIRA(K24B,'LONMAX',NBREFE,K8B)
        ELSE
          IF (NIV.GE.2) 
     &      WRITE(IFM,*)'<FETI/VTCMBL> CHAM_NO SANS REFE ',K24B(1:19)
          NBREFE=0
        ENDIF     
        IF (NBREFE.NE.4) THEN
          IF (NIV.GE.2) 
     &      WRITE(IFM,*)'<FETI/VTCMBL> CHAM_NO NON ETENDU POUR FETI ',
     &      K24B(1:19)
        ELSE
          CALL JEVEUO(K24B,'L',IREFE)      
          METHOD=ZK24(IREFE+2)
          SDFETI=ZK24(IREFE+3)       
        ENDIF
  
        DO 10 ICMB=2,NBCMB
          K24B=NOMCH(ICMB)(1:19)//REFE
          CALL JEEXIN(K24B,IRET)
          IF (IRET.NE.0) THEN
            CALL JELIRA(K24B,'LONMAX',NBREFE,K8B)
          ELSE
            IF (NIV.GE.2) 
     &        WRITE(IFM,*)'<FETI/VTCMBL> CHAM_NO SANS REFE ',K24B(1:19)
            NBREFE=0
          ENDIF   
          IF (NBREFE.NE.4) THEN
            METHO1='XXXX'
            SDFET1='XXXX'
          ELSE
            CALL JEVEUO(K24B,'L',IREFE)
            METHO1=ZK24(IREFE+2)
            SDFET1=ZK24(IREFE+3)                            
            IF ((METHO1.NE.METHOD).OR.(SDFET1.NE.SDFETI))
     &        CALL UTMESS('F','VTCMBL',       
     &          'LISTE DE CHAM_NO A CONCATENER HETEROGENE')
          ENDIF          
   10   CONTINUE
      ENDIF
       
C INIT. POUR METHODE FETI          
      IF (METHOD(1:4).EQ.'FETI') THEN
        FETC='.FETC'
        FETA='.FETA'
        CALL JEVEUO(SDFETI(1:19)//'.FDIM','L',IDIME)
        NBSD=ZI(IDIME)
C PREPARATION POUR LA BOUCLE SUR LES SOUS-DOMAINES. STOCKAGE
C DES ADRESSES DES .FETC DE CHACUN DES CHAM_NOS A CONCATENER
C POUR EVITER (NBCMB-1)*NBSD APPELS A JEVEUO !
        CALL WKVECT('&&VECFETC','V V I',NBCMB,IVFETC)
        DO 20 ICMB=1,NBCMB
          CH19=NOMCH(ICMB)
          CALL JEVEUO(CH19//FETC,'L',IFETC)
          ZI(IVFETC+ICMB-1)=IFETC       
   20   CONTINUE
        CALL JEVEUO('&&'//SDFETI(1:17)//'.FINF','L',IINF)
        INFOFE=ZK24(IINF)
        CALL JEVEUO('&FETI.LISTE.SD.MPI','L',ILIMPI)
        LFETI=.TRUE.
        SDFETA=SDFETI(1:19)//FETA
      ELSE
        LFETI=.FALSE.
        INFOFE='FFFFFFFFFFFFFFFFFFFFFFF'                            
      ENDIF
         
C========================================
C BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
C========================================
C IDD=0 --> DOMAINE GLOBAL/ IDD=I --> IEME SOUS-DOMAINE
      DO 600 IDD=0,NBSD

C TRAVAIL PREALABLE POUR DETERMINER SI ON EFFECTUE LA BOUCLE SUIVANT
C LE SOLVEUR (FETI OU NON), LE TYPE DE RESOLUTION (PARALLELE OU 
C SEQUENTIELLE) ET L'ADEQUATION "RANG DU PROCESSEUR-NUMERO DU SD"
        IF (.NOT.LFETI) THEN
          IDDOK=.TRUE.
        ELSE 
          IF (ZI(ILIMPI+IDD).EQ.1) THEN
            IDDOK=.TRUE.
          ELSE
            IDDOK=.FALSE.
          ENDIF
        ENDIF
        IF (IDDOK) THEN

        IF (IDD.EQ.0) THEN
C PREMIER CHAM_NO GLOBAL A CONCATENER   
          CH19 = NOMCH(1)
        ELSE 
C DETOURS PAR LE .FETC DU PREMIER CHAM_NO GLOBAL A CONCATENER POUR
C OBTENIR LA TAILLE (NBVALE), LE NOM (NOMSD) DU SOUS-DOMAINE IDD ET
C LE NOM DU CHAM_NO LOCAL CORRESPONDANT (CH19)  
          CH19=ZK24(ZI(IVFETC)+IDD-1)(1:19)
          CALL JELIRA(CH19//VALE  ,'LONMAX',NBVALE,K8B)
          CALL JENUNO(JEXNUM(SDFETA,IDD),NOMSD)     
        ENDIF  

C OBTENTION DES ADRESSES ET DES TAILLES DES .DESC, .REFE ET .VALE
C DU PREMIER CHAM_NO (GLOBAL OU LOCAL) A CONCATENER. ON SUPPOSE QUE
C TOUS LES CHAM_NOS DE LA LISTE NOMCH SONT HOMOGENES SUR CE POINT.
        CALL JELIRA(CH19//DESC  ,'LONMAX',NBDESC,K8B)
        CALL JELIRA(CH19//VALE  ,'LONMAX',NBVALE,K8B)
        CALL JELIRA(CH19//REFE,'LONMAX',NBREFE,K8B)
        CALL JEVEUO(CH19//DESC  ,'L',JDESC)
        CALL JEVEUO(CH19//REFE,'L',JREFE)

C CONSTRUCTION D'UN CHAM_GD RESULTAT SUR LE MODELE DE NOMCH(1)
        IF (IDD.EQ.0) THEN
C CHAM_NO GLOBAL RESULTAT       
          CH19R=CHPRES
        ELSE
C CHAM_NO LOCAL RESULTAT STOCKE DANS CHAM_NO GLOBAL.FETC
C NOUVELLE CONVENTION POUR LES CHAM_NOS FILS, GESTTION DE NOMS
C ALEATOIRES
          CALL GCNCON('.',K8B)
          K8B(1:1)='F'    
          CH19R=CHPRES(1:11)//K8B        
          ZK24(KFETC+IDD-1)=CH19R
        ENDIF       
        CALL JEEXIN(CH19R//VALE,IRET)
        IF (IRET.EQ.0) THEN
          CALL WKVECT(CH19R//DESC,'V V I',NBDESC,KDESC)
          CALL WKVECT(CH19R//VALE,'V V '//TYPE,NBVALE,KVALE)
          CALL WKVECT(CH19R//REFE,'V V K24',NBREFE,KREFE)
C SI FETI CONSTITUTION DE L'OBJET JEVEUX CHPRESS.FETC COMPLEMENTAIRE
          IF ((NBSD.GT.0).AND.(IDD.EQ.0))         
     &      CALL WKVECT(CH19R//FETC,'V V K24',NBSD,KFETC)
        ELSE
          CALL JEVEUO(CH19R//DESC,'E',KDESC)
          CALL JELIRA(CH19R//DESC,'LONMAX',NBDESC,KBID)
          CALL JEVEUO(CH19R//REFE,'E',KREFE)
          CALL JELIRA(CH19R//REFE,'LONMAX',NBREFE,KBID)
C SI FETI CONNEXION A L'OBJET JEVEUX CHPRESS.FETC COMPLEMENTAIRE
          IF ((NBSD.GT.0).AND.(IDD.EQ.0))
     &      CALL JEVEUO(CH19R//FETC,'E',KFETC)  
        ENDIF
        CALL JEECRA(CH19R//DESC,'DOCU',IBID,DOCU)
C RECOPIE DU .DESC ET DU .REFE DU PREMIER CHAM_NO DE LA LISTE
C DANS CEUX DU CHAM_NO SOLUTION      
        DO 90 I = 0,NBDESC-1
          ZI(KDESC+I)=ZI(JDESC+I)
   90   CONTINUE
        DO 91 I = 0,NBREFE-1
          ZK24(KREFE+I)=ZK24(JREFE+I)
   91   CONTINUE

C CHANGER LA GRANDEUR
        CALL SDCHGD(CH19R,TYPRES)
                
C VECTEUR RECEPTACLE TEMPORAIRE DE LA COMBINAISON LINEAIRE      
        CALL WKVECT('&&VTCMBL.VALE','V V '//TYPE,NBVALE,LVALE)

C-----------------------------------------------------------------------
C --- BOUCLE SUR LES CHAM_GDS A COMBINER
C-----------------------------------------------------------------------
        ICONST = 1
        DO 100 ICMB =1,NBCMB

C CHAM_NO A CONCATENER                 
          IF (IDD.EQ.0) THEN
C DOMAINE GLOBAL          
            CH19=NOMCH(ICMB)
          ELSE
C SOUS-DOMAINE N°IDD        
            CH19=ZK24(ZI(IVFETC+ICMB-1)+IDD-1)(1:19)
          ENDIF
                            
           CALL JEVEUO(CH19//VALE,'L',JVALE)
           IF (TYPRES(1:1).EQ.'R') THEN
             IF ( TYPECH(ICMB)(1:1) .EQ. 'R' ) THEN
               DO 110 IVAL = 0,NBVALE-1
                 ZR(LVALE+IVAL) = ZR(LVALE+IVAL) +
     +                            CONST(ICONST)*ZR(JVALE+IVAL)
  110          CONTINUE
             ELSE
               IF ( TYPCST(ICMB)(1:1) .EQ. 'R') THEN
                 DO 120 IVAL = 0,NBVALE-1
                   ZR(LVALE+IVAL) = ZR(LVALE+IVAL) +
     +                                CONST(ICONST)*ZC(JVALE+IVAL)
  120            CONTINUE
               ELSEIF ( TYPCST(ICMB)(1:1) .EQ. 'I') THEN
                 DO 130 IVAL = 0,NBVALE-1
                   ZR(LVALE+IVAL) = ZR(LVALE+IVAL) +
     +                              CONST(ICONST)*DIMAG(ZC(JVALE+IVAL))
  130            CONTINUE
               ELSE
                 TYPE = TYPCST(ICMB)(1:1)
                 CALL UTMESS('F','VTCMBL','TYPE INCONNU: '//TYPE)
               ENDIF
             ENDIF
           ELSE
             IF (TYPECH(ICMB) (1:1).EQ.'R') THEN
               IF (TYPCST(ICMB) (1:1).EQ.'R') THEN
                 DO 210 IVAL = 0,NBVALE - 1
                   ZC(LVALE+IVAL) = ZC(LVALE+IVAL) +
     +                              CONST(ICONST)*ZR(JVALE+IVAL)
  210            CONTINUE
              ELSEIF (TYPCST(ICMB) (1:1).EQ.'C') THEN
                C8CST = DCMPLX(CONST(ICONST),CONST(ICONST+1))
                DO 220 IVAL = 0,NBVALE - 1
                  ZC(LVALE+IVAL) = ZC(LVALE+IVAL) +
     +                                C8CST*ZR(JVALE+IVAL)
  220           CONTINUE
              ENDIF
            ELSE
              IF (TYPCST(ICMB) (1:1).EQ.'R') THEN
                DO 310 IVAL = 0,NBVALE - 1
                  ZC(LVALE+IVAL) = ZC(LVALE+IVAL) +
     +                             CONST(ICONST)*ZC(JVALE+IVAL)
  310           CONTINUE
              ELSEIF (TYPCST(ICMB) (:1).EQ.'C') THEN
                C8CST = DCMPLX(CONST(ICONST),CONST(ICONST+1))
                DO 320 IVAL = 0,NBVALE - 1
                   ZC(LVALE+IVAL) = ZC(LVALE+IVAL) +
     +                              C8CST*ZC(JVALE+IVAL)
  320           CONTINUE
              ENDIF
            ENDIF
          ENDIF
          CALL JELIBE(CH19//VALE)
          ICONST = ICONST + 1
          IF (TYPCST(ICMB)(1:1).EQ.'C') ICONST = ICONST + 1
          
C-----------------------------------------------------------------------
C --- FIN BOUCLE CHAM_GD
C-----------------------------------------------------------------------
  100   CONTINUE

C   IL EST NECESSAIRE D'ACTUALISER KVALE SI LE RESULTAT EST DANS NOMCH()
        CALL JEVEUO(CH19R//VALE,'E',KVALE)
        IF (TYPE(1:1).EQ.'R') THEN
          DO 500 IVAL = 0,NBVALE-1
            ZR(KVALE+IVAL) = ZR(LVALE+IVAL)
 500      CONTINUE
        ELSEIF (TYPE(1:1).EQ.'C') THEN
          DO 510 IVAL = 0,NBVALE-1
            ZC(KVALE+IVAL) = ZC(LVALE+IVAL)
 510      CONTINUE
        ENDIF

C DESTRUCTION DU RECEPTACLE TEMPORAIRE, CAR SA TAILLE VA CHANGER A
C L'ITERATION SUIVANTE  
        CALL JEDETR('&&VTCMBL.VALE')

C MONITORING
        IF ((INFOFE(1:1).EQ.'T').AND.(NBSD.GT.0)) THEN
          IF (IDD.EQ.0) THEN
            WRITE(IFM,*)'<FETI/VTCMBL> DOMAINE GLOBAL ',CH19R(1:19)
          ELSE
            WRITE(IFM,*)'<FETI/VTCMBL> SD: ',IDD,' ',CH19R(1:19)
          ENDIF                           
        ENDIF
        IF ((INFOFE(2:2).EQ.'T').AND.(IDD.NE.0)) 
     &    CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,CH19R(1:19),1,' ')
        IF ((INFOFE(2:2).EQ.'T').AND.(IDD.EQ.NBSD))
     &    CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,CHPRES(1:19),1,' ')
        
C LIBERATION DU CHAMP JEVEUX RESULTAT
        CALL JELIBE(CH19R//VALE)

        ENDIF        
C========================================
C FIN BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
C========================================
  600 CONTINUE

C DESTRUCTION OBJET JEVEUX TEMPORAIRE POUR FETI  
      IF (NBSD.GT.0) CALL JEDETR('&&VECFETC')        

      CALL JEDEMA()
      END
