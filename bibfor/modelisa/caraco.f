      subroutine CARACO(CHAR,MOTFAC,NOMA,NOMO,NDIM,NZOCO,NNOQUA)
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
      implicit none
      character*8 CHAR
      character*16 MOTFAC
      character*8 NOMA
      character*8 NOMO
      integer NDIM
      integer NZOCO
      integer NNOQUA
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : CALICO
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
C IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
C IN  NNOQUA : NOMBRE TOTAL DE NOEUDS QUADRATIQUES DES SURFACES DE
C              CONTACT
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
      character*24 METHCO,TOLECO,CARACF,ECPDON,JEUSUP,JEUFO1,JEUFO2
      character*24 JEUFO3
      integer JMETH,JTOLE,JCMCF,JECPD,JJSUP,JJFO1,JJFO2,JJFO3
      character*24 DIRCO,NORLIS,TANDEF,CHAMCO,COEFCO,SANSNQ,CONVCO
      integer JDIR,JNORLI,JTGDEF,JCHAM,JCOEF,JSANSN,JCONV
      integer IOC,ISY,ISYME
      integer IREAD,IWRITE,NZOCP
      character*24 SYMECO
      integer JSYME,NSYME
C
C ----------------------------------------------------------------------
C
      SYMECO = CHAR(1:8) // '.CONTACT.SYMECO'
      call JEVEUO(SYMECO,'L',JSYME)
      NSYME = ZI(JSYME)
C
      NZOCP = NZOCO - NSYME
C 
C --- INITIALISATION
C 
      call JEMARQ
C
C --- PREPARATION DES SD
C
      METHCO = CHAR(1:8) // '.CONTACT.METHCO'
      call WKVECT(METHCO,'G V I',ZMETH*NZOCO+1,JMETH)
C
      SANSNQ = CHAR(1:8) // '.CONTACT.SANSNQ'
      call WKVECT(SANSNQ,'G V I',NZOCO,JSANSN)
      TOLECO = CHAR(1:8) // '.CONTACT.TOLECO'
      call WKVECT(TOLECO,'G V R',ZTOLE*NZOCO,JTOLE)
C
      CONVCO = CHAR(1:8) // '.CONTACT.CONVCO'
      call WKVECT(CONVCO,'G V I',ZCONV*NZOCO,JCONV)
      CARACF = CHAR(1:8) // '.CONTACT.CARACF'
      call WKVECT(CARACF,'G V R',10*NZOCO+1,JCMCF)
      ECPDON = CHAR(1:8) // '.CONTACT.ECPDON'
      call WKVECT(ECPDON,'G V I',6*NZOCO+1,JECPD)
C
      JEUSUP = CHAR(1:8) // '.CONTACT.JSUPCO'
      JEUFO1 = CHAR(1:8) // '.CONTACT.JFO1CO'
      JEUFO2 = CHAR(1:8) // '.CONTACT.JFO2CO'
      JEUFO3 = CHAR(1:8) // '.CONTACT.JFO3CO'
C
      call WKVECT(JEUSUP,'G V R',NZOCO,JJSUP)
      call WKVECT(JEUFO1,'G V K8',NZOCO,JJFO1)
      call WKVECT(JEUFO2,'G V K8',NZOCO,JJFO2)
      call WKVECT(JEUFO3,'G V K8',NZOCO,JJFO3)
C
      DIRCO = CHAR(1:8) // '.CONTACT.DIRCO'
      call WKVECT(DIRCO,'G V R',3*NZOCO,JDIR)
      NORLIS = CHAR(1:8) // '.CONTACT.NORLIS'
      call WKVECT(NORLIS,'G V I',NZOCO+1,JNORLI)
      TANDEF = CHAR(1:8) // '.CONTACT.TANDEF'
      call WKVECT(TANDEF,'G V R',6*NZOCO,JTGDEF)
      CHAMCO = CHAR(1:8) // '.CONTACT.CHAMCO'
      call WKVECT(CHAMCO,'G V I',NZOCO,JCHAM)
      COEFCO = CHAR(1:8) // '.CONTACT.COEFCO'
      call WKVECT(COEFCO,'G V R',NZOCO,JCOEF)
C
C
      ZI(JMETH) = NZOCO
      ZR(JCMCF) = NZOCO
      ZI(JECPD) = 0
C
C          
C --- ON NE BOUCLE QUE SUR LES ZONES PRINCIPALES:
C
      do 8 IOC = 1,NZOCP
        IREAD = IOC
        IWRITE = IOC
        call CAZOCO(CHAR,MOTFAC,NOMA,NOMO,NDIM,IREAD,IWRITE)
 8    continue
C  
C --- ON BOUCLE SUR LES ZONES PRINCIPALES MAIS ON AGIT SUR LES
C --- ZONES SYMETRIQUES
C
      if (NSYME .gt. 0) then
        ISYME = 0
        do 9 IOC = 1,NZOCP
          IREAD = IOC
          do 10 ISY = 1,NSYME
            if (ZI(JSYME+ISY) .eq. IOC) then
              ISYME = ISYME + 1
              IWRITE = NZOCP + ISYME
              call CAZOCO(CHAR,MOTFAC,NOMA,NOMO,NDIM,IREAD,IWRITE)
            end if
 10       continue
 9      continue
        if (ISYME .ne. NSYME)
     &    call UTMESS('F','CARACO','ERREUR SYMETRIQUE')
        if (IWRITE .ne. NZOCO)
     &    call UTMESS('F','CARACO','ERREUR SYMETRIQUE')
      end if
C
C ======================================================================
      call JEDEMA
C
      end
