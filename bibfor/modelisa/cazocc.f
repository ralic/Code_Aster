      subroutine CAZOCC(CHAR,MOTFAC,NOMA,NOMO,NDIM,IREAD,IWRITE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 06/09/2005   AUTEUR TORKHANI M.TORKHANI 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ROUTINE APPELEE PAR : CAZOCO
C ----------------------------------------------------------------------
C
C LECTURE DES PRINCIPALES CARACTERISTIQUES DU CONTACT (SURFACE IREAD)
C REMPLISSAGE DE LA SD 'DEFICO' (SURFACE IWRITE) POUR 
C LA METHODE CONTINUE
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
      integer REACCA,REACBS,REACBG
      INTEGER ZMETH
      PARAMETER (ZMETH = 8)
      character*8 TYMOCL(2),STACO0,COMPLI
      character*16 MOTCLE(2)
      character*24 LISMA
      integer NBMA1
      character*16 MODELI,PHENOM
      integer IER,IBID,NOC,NOCC
      CHARACTER*24 CARACF,ECPDON,DIRCO,METHCO,TANDEF
      INTEGER JCMCF,JECPD,JDIR,JMETH,JTGDEF
      character*16 INTER,MODAX,FORMUL,TYPF
      real*8 DIR1(3),DIR(3),COEFRO,COCAU,COFAU,REACSI
      real*8 ASPER,KAPPAN,KAPPAV
C
C ----------------------------------------------------------------------
C
      call JEMARQ
C
C --- INITIALISATIONS
C
      COCAU = 0.d0
      COFAU = 0.d0
      COEFRO = 0.d0
C ======================================================================
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C ======================================================================
      DIRCO = CHAR(1:8) // '.CONTACT.DIRCO'
      ECPDON = CHAR(1:8) // '.CONTACT.ECPDON'
      CARACF = CHAR(1:8) // '.CONTACT.CARACF'
      METHCO = CHAR(1:8) // '.CONTACT.METHCO'
      TANDEF = CHAR(1:8) // '.CONTACT.TANDEF'
C ======================================================================
      call JEVEUO(CARACF,'E',JCMCF)
      call JEVEUO(DIRCO,'E',JDIR)
      call JEVEUO(ECPDON,'E',JECPD)
      CALL JEVEUO(METHCO,'E',JMETH)
      CALL JEVEUO(TANDEF,'E',JTGDEF)
C 
C --- RECUPERATION DU NOM DU PHENOMENE ET DE LA  MODELISATION          
C 
      call DISMOI('F','PHENOMENE',NOMO,'MODELE',IBID,PHENOM,IER)
      call DISMOI('F','MODELISATION',NOMO,'MODELE',IBID,MODELI,IER)
C
      ZI(JECPD) = 1
C
      call GETVTX(MOTFAC,'FORMULATION',IREAD,1,1,FORMUL,NOC)
      if (FORMUL(1:4) .eq. 'DEPL') then
        ZI(JECPD+6*(IWRITE-1)+6) = 1
      elseif (FORMUL(1:4) .eq. 'VITE') then
        ZI(JECPD+6*(IWRITE-1)+6) = 2
      else
        call UTMESS('F','CAZOCC',
     &            'NE CORRESPOND A AUCUNE METHODE DU MOT CLE 
     &             FORMULATION')
      end if
C
      call GETVTX(MOTFAC,'INTEGRATION',IREAD,1,1,INTER,NOC)
      if (INTER(1:5) .eq. 'NOEUD') then
        ZR(JCMCF+10*(IWRITE-1)+1) = 1.d0
      elseif (INTER(1:5) .eq. 'GAUSS') then
        ZR(JCMCF+10*(IWRITE-1)+1) = 2.d0
      elseif (INTER(1:7) .eq. 'SIMPSON') then
        ZR(JCMCF+10*(IWRITE-1)+1) = 3.d0
        if (INTER(1:8) .eq. 'SIMPSON1') then
          ZR(JCMCF+10*(IWRITE-1)+1) = 4.d0
        end if
        if (INTER(1:8) .eq. 'SIMPSON2') then
          ZR(JCMCF+10*(IWRITE-1)+1) = 5.d0
        end if
      else
        call UTMESS('F','CAZOCC',
     &            'NE CORRESPOND A AUCUNE METHODE DU MOT CLE 
     &             INTEGRATION')
      end if
C
      call GETVR8(MOTFAC,'COEF_REGU_CONT',IREAD,1,1,COCAU,NOC)
      ZR(JCMCF+10*(IWRITE-1)+2) = COCAU
C
      call GETVTX(MOTFAC,'FROTTEMENT',IREAD,1,1,TYPF,NOCC)
      if (TYPF(1:7) .eq. 'COULOMB') then
        ZR(JCMCF+10*(IWRITE-1)+5) = 3.d0
        call GETVR8(MOTFAC,'COULOMB',IREAD,1,1,COEFRO,NOC)
        ZR(JCMCF+10*(IWRITE-1)+4) = COEFRO
        call GETVR8(MOTFAC,'COEF_REGU_FROT',IREAD,1,1,COFAU,NOC)
        ZR(JCMCF+10*(IWRITE-1)+3) = COFAU
        call GETVIS(MOTFAC,'ITER_FROT_MAXI',IREAD,1,1,REACBS,NOC)
        ZI(JECPD+6*(IWRITE-1)+3) = REACBS
        call GETVR8(MOTFAC,'SEUIL_INIT',IREAD,1,1,REACSI,NOC)
        ZR(JCMCF+10*(IWRITE-1)+6) = REACSI
        
        IF (NOCC .NE. 0) THEN
        CALL GETVR8(MOTFAC,'VECT_Y',IREAD,1,3,DIR,NOC)
        ZI(JMETH+ZMETH*(IWRITE-1)+2) = 0
        IF (NOC.NE.0 .AND. NDIM .GE. 2) THEN
        ZI(JMETH+ZMETH*(IWRITE-1)+2) = 1
        ZR(JTGDEF+6*(IWRITE-1)) = DIR(1)
        ZR(JTGDEF+6*(IWRITE-1)+1) = DIR(2)
        ZR(JTGDEF+6*(IWRITE-1)+2) = DIR(3)
        ELSE 
        ZR(JTGDEF+6*(IWRITE-1)) = 0.D0
        ZR(JTGDEF+6*(IWRITE-1)+1) = 0.D0
        ZR(JTGDEF+6*(IWRITE-1)+2) = 0.D0
        END IF

        CALL GETVR8(MOTFAC,'VECT_Z',IREAD,1,3,DIR,NOC)
        IF (NOC.NE.0) THEN
        ZR(JTGDEF+6*(IWRITE-1)+3) = DIR(1)
        ZR(JTGDEF+6*(IWRITE-1)+4) = DIR(2)
        ZR(JTGDEF+6*(IWRITE-1)+5) = DIR(3)
        ELSE 
        ZR(JTGDEF+6*(IWRITE-1)+3) = 0.D0
        ZR(JTGDEF+6*(IWRITE-1)+4) = 0.D0
        ZR(JTGDEF+6*(IWRITE-1)+5) = 0.D0
        END IF
        END IF
        
      elseif (TYPF(1:4) .eq. 'SANS') then
        ZR(JCMCF+10*(IWRITE-1)+5) = 1.d0
      else
        call UTMESS('F','CAZOCC',
     &             'NE CORRESPOND A AUCUNE METHODE DU MOT CLE 
     &              FROTTEMENT')
      end if
C --- LECTURE DES PARAMETRES DE LA COMPLIANCE POUR METHODE CONTINUE
      call GETVTX(MOTFAC,'COMPLIANCE',IREAD,1,1,COMPLI,NOC)
      if (COMPLI .eq. 'OUI') then
        ZR(JCMCF+10*(IWRITE-1)+7) = 1
        call GETVR8(MOTFAC,'ASPERITE',IREAD,1,1,ASPER,NOC)
        ZR(JCMCF+10*(IWRITE-1)+8) = ASPER
        call GETVR8(MOTFAC,'E_N',IREAD,1,1,KAPPAN,NOC)
        ZR(JCMCF+10*(IWRITE-1)+9) = KAPPAN
        call GETVR8(MOTFAC,'E_V',IREAD,1,1,KAPPAV,NOC)
        ZR(JCMCF+10*(IWRITE-1)+10) = KAPPAV
      else
        ZR(JCMCF+10*(IWRITE-1)+7) = 0
        ZR(JCMCF+10*(IWRITE-1)+8) = 0.d0
      end if
C --- FIN LECTURE DES PARAMETRES DE LA COMPLIANCE METHODE CONTINUE
C
C
C
      call GETVTX(MOTFAC,'MODL_AXIS',IREAD,1,1,MODAX,NOC)
      if (MODAX(1:3) .eq. 'OUI') then
        ZI(JECPD+6*(IWRITE-1)+1) = 1
      elseif (MODAX(1:3) .eq. 'NON') then
        ZI(JECPD+6*(IWRITE-1)+1) = 0
      end if
C
      call GETVIS(MOTFAC,'ITER_CONT_MAXI',IREAD,1,1,REACCA,NOC)
      ZI(JECPD+6*(IWRITE-1)+2) = REACCA
C
      call GETVIS(MOTFAC,'ITER_GEOM_MAXI',IREAD,1,1,REACBG,NOC)
      ZI(JECPD+6*(IWRITE-1)+4) = REACBG
C
      call GETVTX(MOTFAC,'CONTACT_INIT',IREAD,1,1,STACO0,NOC)
      if (STACO0 .eq. 'OUI') then
        ZI(JECPD+6*(IWRITE-1)+5) = 1
      else
        ZI(JECPD+6*(IWRITE-1)+5) = 0
      end if
      call GETVR8(MOTFAC,'DIRE_APPA',IREAD,1,3,DIR1,NOC)
      ZR(JDIR+3*(IWRITE-1)) = DIR1(1)
      ZR(JDIR+3*(IWRITE-1)+1) = DIR1(2)
      if (NDIM .eq. 3) then
        ZR(JDIR+3*(IWRITE-1)+2) = DIR1(3)
      else
        ZR(JDIR+3*(IWRITE-1)+2) = 0.d0
      end if
      MOTCLE(1) = 'GROUP_MA_ESCL'
      MOTCLE(2) = 'MAILLE_ESCL'
      TYMOCL(1) = 'GROUP_MA'
      TYMOCL(2) = 'MAILLE'
      LISMA = '&&CARACO.LISTE_MAILLES_1'
      call RELIEM(NOMO,NOMA,'NU_MAILLE',MOTFAC,IREAD,2,MOTCLE,TYMOCL,
     &            LISMA,NBMA1)
C
      if (NDIM .eq. 2) then
        MODELI = 'CONT_DVP_2D'
      else
        MODELI = 'CONT_DVP_3D'
      end if
C
      call AJELLT('&&CALICO.LIGRET',NOMA,NBMA1,LISMA,' ',PHENOM,MODELI,
     &           0,' ')
C
C
      call JEDEMA
      end
