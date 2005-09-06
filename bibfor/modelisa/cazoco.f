      subroutine CAZOCO(CHAR,MOTFAC,NOMA,NOMO,NDIM,IREAD,IWRITE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 06/09/2005   AUTEUR TORKHANI M.TORKHANI 
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
C
      implicit none
      character*8 CHAR
      character*16 MOTFAC
      character*8 NOMA
      character*8 NOMO
      integer NDIM
      integer IREAD
      integer IWRITE
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : CARACO
C ----------------------------------------------------------------------
C
C LECTURE DES PRINCIPALES CARACTERISTIQUES DU CONTACT (SURFACE IREAD)
C REMPLISSAGE DE LA SD 'DEFICO' (SURFACE IWRITE)
C
C IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
C IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
C IN  NOMA   : NOM DU MAILLAGE
C IN  NOMO   : NOM DU MODELE
C IN  NDIM   : NOMBRE DE DIMENSIONS DU PROBLEME
C IN  IREAD  : INDICE POUR LIRE LES DONNEES DANS AFFE_CHAR_MECA
C IN  IWRITE : INDICE POUR ECRIRE LES DONNEES DANS LA SD DEFICONT
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
      integer ZTOLE
      parameter (ZTOLE = 4)
      integer ZCONV
      parameter (ZCONV = 4)
C
      character*16 NOMCMD,APPAR,PROJ,RECH,TYPM,TYPN,TYPF,GLIS
      integer NBREAC,LGBLOC,ITER
C
      character*8 K8BID,REAC
      character*16 K16BID,CHAM
      integer NOC,NOCN,NOCC,OLDMET,NEWMET
      character*24 METHCO,TOLECO,CARACF,DIRCO,SANSNQ
      integer JMETH,JTOLE,JCMCF,JDIR,JSANSN
      character*24 JEUSUP,JEUFO1,JEUFO2,JEUFO3
      integer JJSUP,JJFO1,JJFO2,JJFO3
      character*24 NORLIS,TANDEF,CHAMCO,COEFCO,CONVCO
      integer JNORLI,JTGDEF,JCHAM,JCOEF,JCONV
C
      character*8 JEUF1,JEUF2,JEUF3,ISTO
      character*16 INTER
      character*3 NOQU
      real*8 DIST1,DIST2,LAMB,GEOM,ALJEU
      real*8 DIR(3),COEF,SEUIL
      real*8 COEFRO,COEFPN,COEFPT,COEFTE
C
C
C ----------------------------------------------------------------------
C
      call JEMARQ
C
C --- INITIALISATIONS
C
      COEFPT = 0.d0
      COEFPN = 0.d0
      COEFTE = 0.d0
      COEFRO = 0.d0
      COEF = 0.d0
      SEUIL = 0.d0
      DIST1 = 0.d0
      DIST2 = 0.d0
      LAMB = 0.d0
      GEOM = 0.d0
      ALJEU = -1.d0
C
C --- NOM DU CONCEPT RESULTAT D'AFFE_CHAR_MECA
C
      call GETRES(K8BID,K16BID,NOMCMD)
C ======================================================================
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C ======================================================================
      CARACF = CHAR(1:8) // '.CONTACT.CARACF'
      CHAMCO = CHAR(1:8) // '.CONTACT.CHAMCO'
      COEFCO = CHAR(1:8) // '.CONTACT.COEFCO'
      CONVCO = CHAR(1:8) // '.CONTACT.CONVCO'
      DIRCO = CHAR(1:8) // '.CONTACT.DIRCO'
      JEUFO1 = CHAR(1:8) // '.CONTACT.JFO1CO'
      JEUFO2 = CHAR(1:8) // '.CONTACT.JFO2CO'
      JEUFO3 = CHAR(1:8) // '.CONTACT.JFO3CO'
      JEUSUP = CHAR(1:8) // '.CONTACT.JSUPCO'
      METHCO = CHAR(1:8) // '.CONTACT.METHCO'
      NORLIS = CHAR(1:8) // '.CONTACT.NORLIS'
      SANSNQ = CHAR(1:8) // '.CONTACT.SANSNQ'
      TANDEF = CHAR(1:8) // '.CONTACT.TANDEF'
      TOLECO = CHAR(1:8) // '.CONTACT.TOLECO'
C ======================================================================
      call JEVEUO(CARACF,'E',JCMCF)
      call JEVEUO(CHAMCO,'E',JCHAM)
      call JEVEUO(COEFCO,'E',JCOEF)
      call JEVEUO(CONVCO,'E',JCONV)
      call JEVEUO(DIRCO,'E',JDIR)
      call JEVEUO(JEUFO1,'E',JJFO1)
      call JEVEUO(JEUFO2,'E',JJFO2)
      call JEVEUO(JEUFO3,'E',JJFO3)
      call JEVEUO(JEUSUP,'E',JJSUP)
      call JEVEUO(METHCO,'E',JMETH)
      call JEVEUO(NORLIS,'E',JNORLI)
      call JEVEUO(SANSNQ,'E',JSANSN)
      call JEVEUO(TANDEF,'E',JTGDEF)
      call JEVEUO(TOLECO,'E',JTOLE)
C
C ----------------------------------------------------------------------
C
C     RECUPERATION DE LA METHODE DE CONTACT/FROTTEMENT
C
C ----------------------------------------------------------------------
C
      call GETVTX(MOTFAC,'METHODE',IREAD,1,1,TYPM,NOC)
      if (TYPM(1:8) .eq. 'PENALISA') then
        ZI(JMETH+ZMETH*(IWRITE-1)+6) = -1
        call GETVR8(MOTFAC,'E_N',1,1,1,COEFPN,NOCN)
        ZR(JCMCF+10*(IWRITE-1)+2) = COEFPN
      elseif (TYPM(1:8) .eq. 'LAGRANGI') then
        ZI(JMETH+ZMETH*(IWRITE-1)+6) = 1
      elseif (TYPM(1:8) .eq. 'CONTRAIN') then
        call GETVTX(MOTFAC,'GLISSIERE',IREAD,1,1,GLIS,NOC)
        if (GLIS(1:3) .eq. 'NON') then
          ZI(JMETH+ZMETH*(IWRITE-1)+6) = 0
        else
          ZI(JMETH+ZMETH*(IWRITE-1)+6) = 7
          call GETVR8(MOTFAC,'ALARME_JEU',IREAD,1,1,ALJEU,NOC)
          ZR(JTOLE+ZTOLE*(IWRITE-1)+3) = ALJEU
        end if
      elseif (TYPM(1:8) .eq. 'CONTINUE') then
        call GETVTX(MOTFAC,'GLISSIERE',IREAD,1,1,GLIS,NOC)
        if (GLIS(1:3) .eq. 'NON') then
          ZI(JMETH+ZMETH*(IWRITE-1)+6) = 6
        else 
          ZI(JMETH+ZMETH*(IWRITE-1)+6) = 8
          call GETVR8(MOTFAC,'ALARME_JEU',IREAD,1,1,ALJEU,NOC)
          ZR(JTOLE+ZTOLE*(IWRITE-1)+3) = ALJEU
        end if
      elseif (TYPM(1:5) .eq. 'VERIF') then
        ZI(JMETH+ZMETH*(IWRITE-1)+6) = -2
      else
        call UTMESS('F','CAZOCO',
     &             'NE CORRESPOND A AUCUNE METHODE DE CONTACT-FROTTEMENT
     &             ')
      end if
C
C --- TEST POUR LES METHODES SANS FROTTEMENT
C
      if (IWRITE .gt. 1) then
        OLDMET = ZI(JMETH+ZMETH*(IWRITE-2)+6)
        NEWMET = ZI(JMETH+ZMETH*(IWRITE-1)+6)
        if ((TYPM(1:8).eq.'CONTRAIN') .or. (TYPM(1:5).eq.'VERIF') .or.
     &      (TYPM(1:8).eq.'CONTINUE')) then
          if (OLDMET .ne. NEWMET) then
            call UTMESS('F','CAZOCO',
     &                 'METHODE DE CONTACT DIFFERENTES POUR'//
     &                 ' LES ZONES DE CONTACT')
          end if
        end if
      end if
C
C ----------------------------------------------------------------------
C
C     PARAMETRES COMMUNS A TOUTES LES METHODES
C
C ----------------------------------------------------------------------
C
C 
C --- RECUPERATION DU TYPE D'APPARIEMENT 
C 
      call GETVTX(MOTFAC,'APPARIEMENT',IREAD,1,1,APPAR,NOC)
C
      if (APPAR(1:3) .eq. 'NON') then
        ZI(JMETH+ZMETH*(IWRITE-1)+1) = -1
      elseif (APPAR(1:5) .eq. 'NODAL') then
        ZI(JMETH+ZMETH*(IWRITE-1)+1) = 0
      elseif (APPAR(1:9) .eq. 'MAIT_ESCL') then
        ZI(JMETH+ZMETH*(IWRITE-1)+1) = 1
      else
        call UTMESS('F','CAZOCO',
     &             'NE CORRESPOND A AUCUNE METHODE DU MOT CLE 
     &              APPARIEMENT')
      end if
C
      if (APPAR(1:3) .eq. 'NON') then
        if (TYPM(1:8) .eq. 'CONTINUE') then
          call GETVTX(MOTFAC,'GROUP_MA_ESCL',1,1,1,K16BID,NOC)
          if (NOC .ne. 0)
     &      call UTMESS('F','CAZOCO',
     &                 'AVEC APPARIEMENT=NON, IL NE'//
     &                 ' FAUT PAS RENSEIGNER GROUP_MA_ESCL')
          call GETVTX(MOTFAC,'MAILLE_ESCL',1,1,1,K16BID,NOC)
          if (NOC .ne. 0)
     &      call UTMESS('F','CAZOCO',
     &                 'AVEC APPARIEMENT=NON, IL NE FAUT'//
     &                 ' PAS RENSEIGNER MAILLE_ESCL')
        else
          call GETVTX(MOTFAC,'GROUP_MA_MAIT',1,1,1,K16BID,NOC)
          if (NOC .ne. 0)
     &      call UTMESS('F','CAZOCO',
     &                 'AVEC APPARIEMENT=NON, IL NE '//
     &                 'FAUT PAS RENSEIGNER GROUP_MA_MAIT')
          call GETVTX(MOTFAC,'MAILLE_MAIT',1,1,1,K16BID,NOC)
          if (NOC .ne. 0)
     &      call UTMESS('F','CAZOCO',
     &                 'AVEC APPARIEMENT=NON, IL NE FAUT '//
     &                 'PAS RENSEIGNER MAILLE_MAIT')
        end if
      end if
C
C 
C --- APPARIEMENT SYMETRIQUE ?
C
      if (APPAR .eq. 'MAIT_ESCL_SYME') then
        ZI(JMETH+ZMETH*(IWRITE-1)+3) = 1
      else
        ZI(JMETH+ZMETH*(IWRITE-1)+3) = 0
      end if
C 
C --- RECUPERATION DU TYPE DE PROJECTION 
C 
      call GETVTX(MOTFAC,'PROJECTION',IREAD,1,1,PROJ,NOC)
C
      if (PROJ .eq. 'LINEAIRE') then
        ZI(JMETH+ZMETH*(IWRITE-1)+4) = 1
      elseif (PROJ .eq. 'QUADRATIQUE') then
        ZI(JMETH+ZMETH*(IWRITE-1)+4) = 2
      else
        call UTMESS('F','CAZOCO',
     &             'NE CORRESPOND A AUCUNE METHODE DU MOT CLE PROJECTION
     &             ')
      end if
C 
C --- RECUPERATION DE L'ENVIRONNEMENT DE RECHERCHE 
C 
      call GETVTX(MOTFAC,'RECHERCHE',IREAD,1,1,RECH,NOC)
C
      if (APPAR .eq. 'NON') then
        ZI(JMETH+ZMETH*(IWRITE-1)+5) = 0
      elseif (RECH(1:12) .eq. 'NOEUD_BOUCLE') then
        ZI(JMETH+ZMETH*(IWRITE-1)+5) = 1
      elseif (RECH(1:12) .eq. 'NOEUD_VOISIN') then
        ZI(JMETH+ZMETH*(IWRITE-1)+5) = 2
      else
        call UTMESS('F','CAZOCO',
     &             'NE CORRESPOND A AUCUNE METHODE DU MOT CLE RECHERCHE'
     &             )
      end if
C 
C --- PRESENCE DE LISSAGE ? 
C 
      call GETVTX(MOTFAC,'LISSAGE',IREAD,1,1,INTER,NOC)
C
      if (INTER(1:3) .eq. 'OUI') then
        ZI(JNORLI+(IWRITE-1)+1) = 1
      elseif (INTER(1:3) .eq. 'NON') then
        ZI(JNORLI+(IWRITE-1)+1) = 0
      else
        call UTMESS('F','CAZOCO',
     &             'NE CORRESPOND A AUCUNE METHODE DU MOT CLE LISSAGE')
      end if
C 
C --- TYPE DE NORMALE 
C 
      call GETVTX(MOTFAC,'NORMALE',IREAD,1,1,TYPN,NOC)
C
      if (TYPN(1:6) .eq. 'MAIT') then
        ZI(JMETH+ZMETH*(IWRITE-1)+8) = 0
      elseif (TYPN(1:9) .eq. 'MAIT_ESCL') then
        ZI(JMETH+ZMETH*(IWRITE-1)+8) = 1
      else
        call UTMESS('F','CAZOCO',
     &             'NE CORRESPOND A AUCUNE METHODE DU MOT CLE NORMALE')
      end if
C 
C --- RECUPERATION DES GRANDEURS UTILISEES (DEPLACEMENT, PRESSION OU 
C --- TEMPERATURE) ET DES COEFFICIENTS POUR LA LIAISON SANS APPARIEMENT
C --- DANS LA ZONE IOC 
C 
      call GETVTX(MOTFAC,'NOM_CHAM',IREAD,1,1,CHAM,NOC)
      if (CHAM(1:4) .eq. 'DEPL') then
        if (APPAR(1:3) .eq. 'NON') then
          ZI(JCHAM+IWRITE-1) = -1
        else
          ZI(JCHAM+IWRITE-1) = 1
        end if
      elseif (CHAM(1:4) .eq. 'PRES') then
        ZI(JCHAM+IWRITE-1) = -2
      elseif (CHAM(1:4) .eq. 'TEMP') then
        ZI(JCHAM+IWRITE-1) = -3
      elseif (CHAM(1:4) .eq. 'PRE1') then
        ZI(JCHAM+IWRITE-1) = -4
      elseif (CHAM(1:4) .eq. 'PRE2') then
        ZI(JCHAM+IWRITE-1) = -5
      elseif (CHAM(1:4) .eq. 'VITE') then
        call UTMESS('F','CAZOCO','NOM_CHAM = VITE NON DISPONIBLE')
      else
        if (TYPM(1:5) .ne. 'VERIF') then
          call UTMESS('F','CAZOCO',
     &               'NE CORRESPOND A AUCUNE METHODE DU MOT CLE NOM_CHAM
     &               ')
        end if
      end if
C
C --- METHODE CONTINUE: PARAMETRES SPECIFIQUES
C
      if (TYPM(1:8) .eq. 'CONTINUE') then
        call CAZOCC(CHAR,MOTFAC,NOMA,NOMO,NDIM,IREAD,IWRITE)
        goto 999
      end if
C 
C --- MOT-CLE VECT_NORM_ESCL 
C 
      call GETVR8(MOTFAC,'VECT_NORM_ESCL',IREAD,1,3,DIR,NOC)
      if (NOC .ne. 0) then
        if (APPAR(1:5) .ne. 'NODAL') then
          call UTMESS('F','CAZOCO',
     &               ' ON NE PEUT PAS UTILISER '//
     &               'UNE DIRECTION D''APPARIEMENT FIXE SI '//
     &               'L''APPARIEMENT N''EST PAS NODAL')
        end if
        ZI(JMETH+ZMETH*(IWRITE-1)+1) = 4
        ZR(JDIR+3*(IWRITE-1)) = DIR(1)
        ZR(JDIR+3*(IWRITE-1)+1) = DIR(2)
        if (NDIM .eq. 3) then
          ZR(JDIR+3*(IWRITE-1)+2) = DIR(3)
        else
          ZR(JDIR+3*(IWRITE-1)+2) = 0.d0
        end if
      end if
C 
C --- MOT-CLE VECT_Y
C 
      call GETVTX(MOTFAC,'FROTTEMENT',IREAD,1,1,TYPF,NOCC)
      ZI(JMETH+ZMETH*(IWRITE-1)+2) = 0
      if (NOCC .ne. 0) then
        call GETVR8(MOTFAC,'VECT_Y',IREAD,1,3,DIR,NOC)
        if (NOC .ne. 0) then
          if (NDIM .eq. 2) then
            call UTMESS('A','CAZOCO',
     &                 'LA COMMANDE VECT_Y'//' N''INTERVIENT PAS EN 2D.'
     &                 )
          else
            ZI(JMETH+ZMETH*(IWRITE-1)+2) = 1
            ZR(JTGDEF+(IWRITE-1)*3) = DIR(1)
            ZR(JTGDEF+(IWRITE-1)*3+1) = DIR(2)
            ZR(JTGDEF+(IWRITE-1)*3+2) = DIR(3)
          end if
        end if
      end if
C 
C --- RECUPERATION DU JEU SUPPLEMENTAIRE MECANIQUE POUR LA ZONE IOC 
C --- LE JEU TOTAL SERA JEU - JEUSUP (SOIT : D - DIST1 - DIST2) 
C 
      ZR(JJSUP+IWRITE-1) = 0.d0
      ZK8(JJFO1+IWRITE-1) = ' '
      ZK8(JJFO2+IWRITE-1) = ' '
C 
C --- CAS D'UN JEU SUPPLEMENTAIRE REEL (AFFE_CHAR_MECA) 
C
      if (NOMCMD .eq. 'AFFE_CHAR_MECA') then
        call GETVR8(MOTFAC,'DIST_MAIT',IREAD,1,1,DIST1,NOC)
        call GETVR8(MOTFAC,'DIST_ESCL',IREAD,1,1,DIST2,NOC)
        ZR(JJSUP+IWRITE-1) = DIST1 + DIST2
      end if
C 
C --- CAS D'UN JEU SUPPLEMENTAIRE FONCTION (AFFE_CHAR_MECA_F) 
C 
      if (NOMCMD .eq. 'AFFE_CHAR_MECA_F') then
        call GETVID(MOTFAC,'DIST_MAIT',IREAD,1,1,JEUF1,NOC)
        if (NOC .ne. 0) ZK8(JJFO1+IWRITE-1) = JEUF1
        call GETVID(MOTFAC,'DIST_ESCL',IREAD,1,1,JEUF2,NOC)
        if (NOC .ne. 0) ZK8(JJFO2+IWRITE-1) = JEUF2
      end if
C
C
C --- CONTACT SANS CALCUL: PARAMETRES SPECIFIQUES
C
      if (TYPM(1:5) .eq. 'VERIF') then
        call CAZOCV(CHAR,MOTFAC,IREAD,IWRITE)
        goto 999
      end if
C
C --- TOUTES LES AUTRES METHODES (DISCRETES)
C
C
C --- TOLE_PROJ_EXT
C --- TOLE_PROJ_EXT <0: LA PROJECTION HORS DE LA MAILLE EST INTERDITE
C --- TOLE_PROJ_EXT >0: LA PROJECTION HORS DE LA MAILLE EST AUTORISEE
C ---                    MAIS LIMITEE PAR LAMB
C
      call GETVR8(MOTFAC,'TOLE_PROJ_EXT',IREAD,1,1,LAMB,NOC)
      if (LAMB .le. 0.d0) then
        ZR(JTOLE+ZTOLE*(IWRITE-1)) = -1.d0
      else
        ZR(JTOLE+ZTOLE*(IWRITE-1)) = LAMB
      end if
C
C --- TOLE_PROJ_INT
C --- TOLE_PROJ_INT   : LA PROJECTION SUR LES ENTITES GEOMETRIQUES
C ---                   INTERNES (NOEUDS, ARETES, DIAGONALES) EST 
C ---                   DETECTEE DANS LA ZONE LIMITEE PAR LAMB
C
      call GETVR8(MOTFAC,'TOLE_PROJ_INT',IREAD,1,1,LAMB,NOC)
      ZR(JTOLE+ZTOLE*(IWRITE-1)+1) = LAMB
C
C --- TOLE_REAC_GEOM
C
      call GETVR8(MOTFAC,'TOLE_REAC_GEOM',IREAD,1,1,GEOM,NOC)
      ZR(JTOLE+ZTOLE*(IWRITE-1)+2) = GEOM
C 
C --- PARAMETRES DE REACTUALISATION GEOMETRIQUE 
C       
      call GETVTX(MOTFAC,'REAC_GEOM',IREAD,1,1,REAC,NOC)
C
      if (REAC .eq. 'SANS') then
        ZI(JMETH+ZMETH*(IWRITE-1)+7) = 0
      elseif (REAC .eq. 'AUTOMATI') then
        ZI(JMETH+ZMETH*(IWRITE-1)+7) = -1
      elseif (REAC .eq. 'CONTROLE') then
        call GETVIS(MOTFAC,'NB_REAC_GEOM',IREAD,1,1,NBREAC,NOC)
        ZI(JMETH+ZMETH*(IWRITE-1)+7) = NBREAC
      else
        call UTMESS('F','CAZOCO',
     &             'NE CORRESPOND A AUCUNE METHODE DU MOT CLE REAC_GEOM'
     &             )
      end if
C
C --- ARRET OU PAS SI MATRICE DE CONTACT SINGULIERE 
C       
      call GETVTX(MOTFAC,'STOP_SINGULIER',IREAD,1,1,ISTO,NOC)
      ZI(JCONV+ZCONV*(IWRITE-1)) = 0
      if (ISTO .eq. 'OUI') then
        ZI(JCONV+ZCONV*(IWRITE-1)) = 0
      elseif (ISTO .eq. 'NON') then
        ZI(JCONV+ZCONV*(IWRITE-1)) = 1
      else
        call UTMESS('F','CAZOCO',
     &             'NE CORRESPOND A AUCUNE METHODE DU MOT CLE 
     &              STOPSINGULIER')
      end if
C
C --- NOMBRE DE PAQUETS POUR LA RESOLUTION DES SYSTEMES LINEAIRES  
C 
      call GETVIS(MOTFAC,'NB_RESOL',IREAD,1,1,LGBLOC,NOC)
      ZI(JCONV+ZCONV*(IWRITE-1)+1) = LGBLOC
C
C --- NOMBRE D'ITERATIONS DE CONTACT MAX = NBLIAI*ITER_MULT_MAXI  
C 
      if (TYPM(1:10) .ne. 'CONTRAINTE') then
        call GETVIS(MOTFAC,'ITER_MULT_MAXI',IREAD,1,1,ITER,NOC)
        ZI(JCONV+ZCONV*(IWRITE-1)+2) = ITER
      end if
C 
C --- EXCLUSION DE NOEUDS QUADRATIQUES (VOIR CACOOEQ) 
C 
      call GETVTX(MOTFAC,'SANS_NOEUD_QUAD',IREAD,1,1,NOQU,NOC)
      if (NOQU .eq. 'OUI') then
        ZI(JSANSN+(IWRITE-1)) = 1
      elseif (NOQU .eq. 'NON') then
        ZI(JSANSN+(IWRITE-1)) = 0
      else
        call UTMESS('F','CAZOCO',
     &             'NE CORRESPOND A AUCUNE METHODE DU MOT CLE 
     &              SANS_NOEUD_QUAD')
      end if
C
      if (APPAR(1:3) .eq. 'NON') then
        call GETVR8(MOTFAC,'COEF_MULT_ESCL',IREAD,1,1,COEF,NOC)
        ZR(JCOEF+IWRITE-1) = COEF
C
        if (NOMCMD .eq. 'AFFE_CHAR_MECA') then
          call GETVR8(MOTFAC,'COEF_IMPO',IREAD,1,1,SEUIL,NOC)
          ZR(JJSUP+IWRITE-1) = SEUIL
        elseif (NOMCMD .eq. 'AFFE_CHAR_MECA_F') then
          call GETVID(MOTFAC,'COEF_IMPO',IREAD,1,1,JEUF3,NOC)
          if (NOC .ne. 0) then
            ZK8(JJFO3+IWRITE-1) = JEUF3
          end if
        else
          call UTMESS('F','CALICO00','AAAAAAAAAAARRRRGHHHH...')
        end if
      else
        ZR(JCOEF+IWRITE-1) = 1.d0
      end if
C
C --- PARAMETRES DU FROTTEMENT
C
      call GETVTX(MOTFAC,'FROTTEMENT',IREAD,1,1,TYPF,NOCC)
      if (NOCC .ne. 0) then
        if (TYPF .eq. 'COULOMB') then
          ZR(JCMCF+10*(IWRITE-1)+5) = 3.d0
          call GETVR8(MOTFAC,'COULOMB',IREAD,1,1,COEFRO,NOC)
          ZR(JCMCF+10*(IWRITE-1)+4) = COEFRO
          call GETVR8(MOTFAC,'E_T',IREAD,1,1,COEFPT,NOC)
          ZR(JCMCF+10*(IWRITE-1)+3) = COEFPT
          call GETVR8(MOTFAC,'COEF_MATR_FROT',IREAD,1,1,COEFTE,NOC)
          ZR(JCMCF+10*(IWRITE-1)+6) = COEFTE
          if (NDIM .eq. 2) then
            if (TYPM(1:8) .eq. 'PENALISA') then
              ZI(JMETH+ZMETH*(IWRITE-1)+6) = 3
              if (NOCN .ne. 0) ZI(JMETH+ZMETH*(IWRITE-1)+6) = 5
            elseif (TYPM(1:8) .eq. 'LAGRANGI') then
              ZI(JMETH+ZMETH*(IWRITE-1)+6) = 2
            end if
          elseif (NDIM .eq. 3) then
            if (TYPM(1:8) .eq. 'PENALISA') then
              ZI(JMETH+ZMETH*(IWRITE-1)+6) = 3
              if (NOCN .ne. 0) ZI(JMETH+ZMETH*(IWRITE-1)+6) = 5
            elseif (TYPM(1:8) .eq. 'LAGRANGI') then
              ZI(JMETH+ZMETH*(IWRITE-1)+6) = 4
            end if
          end if
        end if
      end if
C
C --- TEST POUR LES METHODES AVEC FROTTEMENT
C
      if (IWRITE .gt. 1) then
        OLDMET = ZI(JMETH+ZMETH*(IWRITE-2)+6)
        NEWMET = ZI(JMETH+ZMETH*(IWRITE-1)+6)
        if ((TYPM(1:8).eq.'PENALISA') .or. (TYPM(1:8).eq.'LAGRANGI'))
     &     then
          if (OLDMET .ne. NEWMET) then
            call UTMESS('F','CAZOCO',
     &                 'METHODE DE CONTACT DIFFERENTES POUR'//
     &                 ' LES ZONES DE CONTACT')
          end if
        end if
      end if
C
 999  continue
C
      call JEDEMA
C
      end
