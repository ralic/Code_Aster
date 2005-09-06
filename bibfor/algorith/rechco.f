      subroutine RECHCO(PREMIE,LREAC,NZOCO,NSYME,INST,NOMA,NEWGEO,
     &                  DEFICO,RESOCO)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/09/2005   AUTEUR TORKHANI M.TORKHANI 
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
C
      implicit none
      logical PREMIE
      logical LREAC
      integer NZOCO
      integer NSYME
      real*8 INST
      character*8 NOMA
      character*24 NEWGEO
      character*24 DEFICO
      character*24 RESOCO
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : CFGEOM
C ----------------------------------------------------------------------
C
C LISTE DES NOEUDS ESCLAVES COURANTS. RECHERCHE DE LA MAILLE (OU DU
C NOEUD) MAITRE ASSOCIEE A CHAQUE NOEUD ESCLAVE.
C
C IN  PREMIE : VAUT .TRUE. SI C'EST LE PREMIER CALCUL (PAS DE PASSE)
C IN  LREAC  : VAUT .TRUE. SI REACTUALISATION GEOMETRIQUE
C IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
C IN  NSYME  : NOMBRE DE ZONES DE CONTACT SYMETRIQUES
C IN  INST   : VALEUR DE L'INSTANT DE CALCUL
C IN  NOMA   : NOM DU MAILLAGE
C IN  NEWGEO : GEOMETRIE ACTUALISEE EN TENANT COMPTE DU CHAMP DE
C              DEPLACEMENTS COURANT
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C VAR RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C
C REMARQUE IMPORTANTE
C -------------------
C
C ON DOIT PRENDRE EN COMPTE UN EVENTUEL JEU FICTIF INTRODUIT
C PAR L'UTILISATEUR SOUS LES MOTS-CLES DIST_1 ET DIST_2, ET RANGE
C DANS LE TABLEAU JEUSUP : CE JEU DEVRA ETRE SOUSTRAIT AU VRAI JEU.
C ON MET PREALABLEMENT LE TABLEAU APJEU A ZERO POUR TOUS LES NOEUDS
C ESCLAVES POTENTIELS (NESMAX).
C
C * POUR LES ZONES SANS REACTUALISATION DE L'APPARIEMENT, ON AFFECTE
C   AU TABLEAU APJEU LA VALEUR DE JEUSUP POUR LA ZONE
C   A TOUS LES NOEUDS ESCLAVES (EN NOMBRE CONNU PUISQUE INCHANGE).
C   ON PASSERA ENSUITE DANS LES ROUTINES RECHNO ET CHMANO
C   (APPELEES PAR PROJCO) OU L'ON SOUSTRAIT (DANS RECHNO, SEULEMENT SI
C   PHASE='FINALE', C'EST-A-DIRE APPARIEMENT NODAL) CETTE VALEUR A LA
C   DISTANCE TROUVEE ENTRE LE NOEUD ESCLAVE ET LA MAILLE MAITRE.
C
C * POUR LES ZONES AVEC REACTUALISATION DE L'APPARIEMENT, ON NE CONNAIT
C   PAS A PRIORI LE NOMBRE DE NOEUDS ESCLAVES. DANS RECHNO ET CHMANO,
C   LA PARTIE D'APJEU UTILE VAUT 0, AUQUEL ON RAJOUTE LA DISTANCE
C   NOEUD-MAILLE. ENSUITE (ON CONNAIT DES LORS LE NOMBRE DE NOEUDS
C   ESCLAVES DE LA ZONE), ON SOUSTRAIT A CE JEU REEL LE JEU FICTIF
C   JEUSUP DE LA ZONE. ON NE REPASSERA PAS DANS PROJCO PUISQUE
C   L'APPARIEMENT A CHANGE (LA PROJECTION ET LE CALCUL DU JEU ONT ETE
C   FAITS DANS RECHCO).
C
C
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      integer ZI
      common /IVARJE/ ZI(1)
      real*8 ZR
      common /RVARJE/ ZR(1)
      complex*16 ZC
      common /CVARJE/ ZC(1)
      logical ZL
      common /LVARJE/ ZL(1)
      character*8 ZK8
      character*16 ZK16
      character*24 ZK24
      character*32 ZK32
      character*80 ZK80
      common /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      integer ZMETH
      parameter (ZMETH = 8)
      integer ZREAC
      parameter (ZREAC = 4)
      integer ZAPPAR
      parameter (ZAPPAR = 3)
      integer NESOLD(NZOCO),IOLD(NZOCO)
      integer NDIM,NESMAX,NESCL,NNOCO
      integer IBID,K,IZONE,IESCL,IESCL0
      integer REAAPP,REACTU,APPAR
      real*8 R8BID
      integer POSNOE,TYPALF
      integer JOLDAP,JOLDPT
      integer JCOOR
      character*24 APDDL,APJEU,APMEMO,APPARI,APPOIN,COMAFO,CONTNO
      integer JAPDDL,JAPJEU,JAPMEM,JAPPAR,JAPPTR,JCOMA,JNOCO
      character*24 APREAC,CARACF,FROTE,JEUFO1,JEUFO2,JEUFO3
      integer JREAC,JCMCF,JFRO,JJFO1,JJFO2,JJFO3
      character*24 JEUSUP,METHCO,JEUINI,PENAL
      integer JJSUP,JMETH,JDIM,JPENA
      character*24 NDIMCO,APJEFX,APJEFY,APCOEF
      character*8 K8BID
      integer IFM,NIV
C
C
C ----------------------------------------------------------------------
C
      call INFNIV(IFM,NIV)
      call JEMARQ
C
C --- INFOS SUR LA CHARGE DE CONTACT
C
      call CFDISC(DEFICO,'              ',IBID,TYPALF,IBID,IBID)
C
C ======================================================================
C                      RECUPERATION D'ADRESSES
C ======================================================================
C
C --- LECTURE DES SD POUR LE CONTACT POTENTIEL
C
      APCOEF = RESOCO(1:14) // '.APCOEF'
      APDDL = RESOCO(1:14) // '.APDDL'
      APJEFX = RESOCO(1:14) // '.APJEFX'
      APJEFY = RESOCO(1:14) // '.APJEFY'
      APJEU = RESOCO(1:14) // '.APJEU'
      APMEMO = RESOCO(1:14) // '.APMEMO'
      APPARI = RESOCO(1:14) // '.APPARI'
      APPOIN = RESOCO(1:14) // '.APPOIN'
      APREAC = RESOCO(1:14) // '.APREAC'
      CARACF = DEFICO(1:16) // '.CARACF'
      COMAFO = DEFICO(1:16) // '.COMAFO'
      CONTNO = DEFICO(1:16) // '.NOEUCO'
      FROTE = DEFICO(1:16) // '.FROTE'
      JEUFO1 = DEFICO(1:16) // '.JFO1CO'
      JEUFO2 = DEFICO(1:16) // '.JFO2CO'
      JEUFO3 = DEFICO(1:16) // '.JFO3CO'
      JEUINI = RESOCO(1:14) // '.JEUINI'
      JEUSUP = DEFICO(1:16) // '.JSUPCO'
      METHCO = DEFICO(1:16) // '.METHCO'
      NDIMCO = DEFICO(1:16) // '.NDIMCO'
      PENAL = DEFICO(1:16) // '.PENAL'
C
      call JEVEUO(APDDL,'E',JAPDDL)
      call JEVEUO(APJEU,'E',JAPJEU)
      call JEVEUO(APMEMO,'L',JAPMEM)
      call JEVEUO(APPARI,'E',JAPPAR)
      call JEVEUO(APPOIN,'E',JAPPTR)
      call JEVEUO(APREAC,'E',JREAC)
      call JEVEUO(CARACF,'L',JCMCF)
      call JEVEUO(COMAFO,'E',JCOMA)
      call JEVEUO(CONTNO,'L',JNOCO)
      call JEVEUO(FROTE,'E',JFRO)
      call JEVEUO(JEUFO1,'L',JJFO1)
      call JEVEUO(JEUFO2,'L',JJFO2)
      call JEVEUO(JEUFO3,'L',JJFO3)
      call JEVEUO(JEUSUP,'E',JJSUP)
      call JEVEUO(METHCO,'L',JMETH)
      call JEVEUO(NDIMCO,'E',JDIM)
      call JEVEUO(PENAL,'E',JPENA)
C
C
C
      if (TYPALF .ne. 0) then
C
        
      end if
C
      NDIM = ZI(JDIM)
      NNOCO = ZI(JDIM+4)
      NESMAX = ZI(JDIM+8)
C
C --- COORDONNEES DES NOEUDS DU MAILLAGE
C
      call JEVEUO(NEWGEO(1:19)//'.VALE','L',JCOOR)
C
C --- ACTUALISATION DES NORMALES POUR LES NOEUDS POTENTIELS DE CONTACT
C
      call CFNORM(NDIM,NNOCO,NOMA,NEWGEO,RESOCO,DEFICO)
C
C ----------------------------------------------------------------------
C --- SAUVEGARDE DE RESULTATS DU PASSE SI ON N'EST PAS AU 1ER CALCUL
C ----------------------------------------------------------------------
C
C
C
      if (.not. PREMIE) then
        if (NIV .ge. 2) then
          write (IFM,*) ' <CONTACT> <> APPARIEMENT - SAUVEGARDE DONNEES'
        end if
C
        NESCL = ZI(JAPPAR)
C
C - RECOPIE DANS DES TABLEAUX DE TRAVAIL DE APPARI ET APPOIN
C
        call WKVECT('&&RECHCO.APPARI','V V I',3*NESCL+1,JOLDAP)
        call WKVECT('&&RECHCO.APPOIN','V V I',NESCL+1,JOLDPT)
C
        do 5 K = 1,3*NESCL+1
          ZI(JOLDAP+K-1) = ZI(JAPPAR+K-1)
 5      continue
        do 6 K = 0,NESCL
          ZI(JOLDPT+K) = ZI(JAPPTR+K)
 6      continue
C
C --- STOCKAGE DU NOMBRE DE NOEUDS ESCLAVES ET DU DECALAGE DES ADRESSES
C --- DE DEBUT DANS L'ANCIENNE CONFIGURATION
C
        IOLD(1) = 0
        do 7 IZONE = 1,NZOCO
          NESOLD(IZONE) = ZI(JDIM+8+IZONE)
          if (IZONE .ge. 2) then
            IOLD(IZONE) = IOLD(IZONE-1) + ZI(JDIM+8+IZONE-1)
          end if
 7      continue
C
       end if
C
C
C ======================================================================
C                           APPARIEMENT
C ======================================================================
C
      IESCL = 0
      ZI(JAPPTR) = 0
C
C --- MISE A ZERO DU JEU
C
      do 8 K = 1,NESMAX
        ZR(JAPJEU+K-1) = 0.d0
 8    continue
C
      do 10 IZONE = 1,NZOCO
C
C --- REACTUALISATIONS A FAIRE
C
        if (.not. LREAC) then
          REAAPP = 0
        else
          REAAPP = ZI(JREAC+ZREAC*(IZONE-1))
        end if
C
C ----------------------------------------------------------------------
C     SI PAS DE REACTUALISATION DE L'APPARIEMENT POUR CETTE ZONE
C ----------------------------------------------------------------------
C
        if (REAAPP .eq. 0) then
C
          if (NIV .ge. 2) then
            write (IFM,1000) IZONE, ' - PAS DE REACTUALISATION '
          end if
C
C --- INCREMENTATION DU COMPTEUR D'APPARIEMENT FIXE POUR LA ZONE
C
          ZI(JREAC+ZREAC*(IZONE-1)+1) = ZI(JREAC+ZREAC*(IZONE-1)+1) + 1
C
C --- RECOPIE DES MORCEAUX ENCORE VALABLES DES TABLEAUX 
C
          do 40 K = 1,NESOLD(IZONE)
            ZI(JAPPAR+3*(IESCL+K-1)+1) =
     &        ZI(JOLDAP+3*(IOLD(IZONE)+K-1)+1)
            ZI(JAPPAR+3*(IESCL+K-1)+2) =
     &        ZI(JOLDAP+3*(IOLD(IZONE)+K-1)+2)
            ZI(JAPPAR+3*(IESCL+K-1)+3) = ZI(JREAC+4*(IZONE-1)+2)
            ZI(JAPPTR+IESCL+K) = ZI(JOLDPT+IOLD(IZONE)+K)
 40       continue
C
C --- INCREMENTATION DU NOMBRE DE NOEUDS ESCLAVES
C
          IESCL = IESCL + NESOLD(IZONE)
C
C --- PASSAGE A LA ZONE SUIVANTE
C
          goto 10
C
         end if
C
C ----------------------------------------------------------------------
C         SI REACTUALISATION DE L'APPARIEMENT POUR CETTE ZONE
C     OU PASSAGE INITIAL POUR REMPLIR LES SD (CAS SANS APPARIEMENT)
C ----------------------------------------------------------------------
C
C
C --- DEBUT DU STOCKAGE DES INFOS SUR LES NOEUDS ESCLAVES 
C --- POUR LA ZONE COURANTE
C
        IESCL0 = IESCL
C
C --- COMPTEUR D'APPARIEMENT FIXE NUL POUR LA ZONE
C
        ZI(JREAC+ZREAC*(IZONE-1)+1) = 0
C
C --- DETERMINATION DES CARACTERISTIQUES DE LA REACTUALISATION
C
        REACTU = ZI(JREAC+ZREAC*(IZONE-1)+2)
C
C --- TYPE D'APPARIEMENT
C
        APPAR = ZI(JMETH+ZMETH*(IZONE-1)+1)
C
C --- APPEL DE LA METHODE D'APPARIEMENT
C
C
        if (APPAR .eq. -1) then
C
          call RECHPA(IZONE,REACTU,NEWGEO,DEFICO,RESOCO,IESCL)
C
        elseif ((APPAR.eq.0) .or. (APPAR.eq.4)) then
C
          call RECHNO(IZONE,REAAPP,REACTU,NEWGEO,DEFICO,RESOCO,IESCL)
C
        elseif (APPAR .eq. 1) then
C
C
          call RECHME(IZONE,REAAPP,REACTU,NZOCO,NSYME,NOMA,NEWGEO,
     &                DEFICO,RESOCO,IESCL)
C
        elseif (APPAR .eq. 2) then
C
          call UTMESS('F','RECHCO',
     &               'L''APPARIEMENT PAR LA METHODE'//
     &               ' DES TERRITOIRES N''EST PAS OPERATIONNEL')
C
        elseif (APPAR .eq. 3) then
C
          call UTMESS('F','RECHCO',
     &               'L''APPARIEMENT PAR LA METHODE'//
     &               ' HIERARCHIQUE N''EST PAS OPERATIONNEL')
C
         end if
C
        do 60 K = IESCL0+1,IESCL
C
C --- CALCUL DU JEU FICTIF DE LA ZONE
C
          POSNOE = ZI(JAPPAR+ZAPPAR*(K-1)+1)
          call CFDIST(IZONE,POSNOE,INST,JNOCO,JCOOR,JJFO1,JJFO2,JJFO3,
     &                JJSUP,R8BID)
C
C --- ADDITION DU JEU FICTIF DE LA ZONE DANS APJEU
C
          ZR(JAPJEU+K-1) = ZR(JAPJEU+K-1) - ZR(JJSUP+IZONE-1)
C
C --- CARACTERISTIQUES DU FROTTEMENT POUR METHODES
C --- "PENALISATION" ET "LAGRANGIEN"
C
          ZR(JFRO-1+K) = ZR(JCMCF+10*(IZONE-1)+4)
          ZR(JPENA-1+2*K-1) = ZR(JCMCF+10*(IZONE-1)+2)
          ZR(JPENA-1+2*K) = ZR(JCMCF+10*(IZONE-1)+3)
          ZR(JCOMA-1+K) = ZR(JCMCF+10*(IZONE-1)+6)
 60     continue
C
C --- NOMBRE DE NOEUDS ESCLAVES DANS LA ZONE
C
        ZI(JDIM+8+IZONE) = IESCL - IESCL0
C
 10   continue
C
C --- STOCKAGE DES LONGUEURS EFFECTIVES
C
      NESCL = IESCL
      ZI(JAPPAR) = NESCL
      if (NESCL .gt. NESMAX) then
        call UTMESS('F','RECHCO_04',
     &             'ERREUR DE DIMENSIONNEMENT : '//
     &             'NOMBRE MAXIMAL DE NOEUDS ESCLAVES')
      end if
      if (ZI(JAPPTR+NESCL) .gt. 30*NESMAX) then
        call UTMESS('F','RECHCO_05',
     &             'ERREUR DE DIMENSIONNEMENT '//
     &             'DES TABLEAUX APCOEF ET APDDL')
      end if
      call JEECRA(APPARI,'LONUTI',3*NESCL+1,K8BID)
      call JEECRA(APPOIN,'LONUTI',NESCL+1,K8BID)
      call JEECRA(APJEU,'LONUTI',NESCL,K8BID)
      call JEECRA(JEUINI,'LONUTI',NESCL,K8BID)
      call JEECRA(APCOEF,'LONUTI',ZI(JAPPTR+NESCL),K8BID)
      call JEECRA(APDDL,'LONUTI',ZI(JAPPTR+NESCL),K8BID)
      if (TYPALF .ne. 0) then
        call JEECRA(APJEFX,'LONUTI',NESCL,K8BID)
        call JEECRA(APJEFY,'LONUTI',NESCL,K8BID)
      end if
C
C --- NETTOYAGE DES VECTEURS DE TRAVAIL
C
      call JEDETR('&&RECHCO.APPARI')
      call JEDETR('&&RECHCO.APPOIN')
C
C ----------------------------------------------------------------------
C
 1000 format (' <CONTACT> <> APPARIEMENT - ZONE: ',i6,a16)
C
      call JEDEMA
      end
