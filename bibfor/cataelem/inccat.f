      SUBROUTINE INCCAT
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CATAELEM  DATE 12/05/97   AUTEUR JMBHH01 J.M.PROIX 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     INCLUDE($CIMPERR)
C
      COMMON /CIMP/IMP,IULMES,IULIST,IULVIG
C
C     EXCLUDE($CIMPERR)
C
C     INCLUDE($CMOTELE)
      CHARACTER*16 CLELE
      COMMON /CMELEN/NCLELE
      COMMON /CMELEC/CLELE(9)
C     EXCLUDE($CMOTELE)
C     INCLUDE($CMOTOPT)
      CHARACTER*16 CLEOPT
      COMMON /CMTOPN/NCLOPT
      COMMON /CMTOPC/CLEOPT(6)
C     EXCLUDE($CMOTOPT)
C     INCLUDE($CMOTPH)
      CHARACTER*24 CLEPH
      COMMON /CMTPHN/NCLEPH
      COMMON /CMTPHC/CLEPH(5)
C
C     CLEPH(1) = 'PHENOMENES_MODELISATION '
C     CLEPH(2) = 'PHENOMENE               '
C     CLEPH(3) = 'MODELISATION            '
C     CLEPH(4) = 'MAILLE                  '
C     CLEPH(5) = 'ELEMENT                 '
C
C     EXCLUDE($CMOTPH)
C     INCLUDE($CDEBUG)
      CHARACTER*8 CLEDBG
      CHARACTER*24 OBJDMP
      INTEGER PASDMP,TYOBDM
      COMMON /CMODBG/CLEDBG
      COMMON /CDEBUG/ICCDBG
      COMMON /CBDMPC/OBJDMP(30)
      COMMON /CBDMPN/NDMP,PASDMP(30),TYOBDM(30)
C
C       NDMP : NOMBRE D OBJETS A DUMPER
C       PASDMP(IDMP)  : PASSE OU ON DUMPE L OBJET IDMP
C       OBJDMP(IDMP)  : NOM DE L OBJET IDMP
C       TYOBDM(IDMP)  : GENRE DE L OBJET IDMP :  0 OBJET SIMPLE
C                                                1 COLLECTION NUMEROTEE
C                                                2 COLLECTION NOMME
C
C     EXCLUDE($CDEBUG)
C
C     INCLUDE($OCATCO)
C
C     TABLEAUX FORTRAN CORRESPONDANTS AUX OBJETS DU CATALOGUE DES
C     COMMUNICATIONS
C
C-----------------------------------------------------------------------
C  OBJET          ! TYPE JEVEUX         ! TABLEAU FORT! FCTION ACCES   !
C-----------------!---------------------!-------------!-----------------
C &CATA.CO.CHMOD  ! MR(IS)              !  CHMOD      ! IDCHMO(I,J)    !
C                 !                     !             !                !
C-----------------!---------------------!-------------!-----------------
      INTEGER CHMOD
      COMMON /COCACO/CHMOD(2)
C     EXCLUDE($OCATCO)
C     INCLUDE($OCATEL)
C
C     TABLEAUX FORTRAN CORRESPONDANTS AUX OBJETS DU CATALOGUE DES
C     ELEMENTS
C
C-----------------------------------------------------------------------
C  OBJET          ! TYPE JEVEUX         ! TABLEAU FORT! FCTION ACCES   !
C-----------------!---------------------!-------------!-----------------
C &CATA.TE.TYPEMA ! V(K8)               !  TYPEMA     ! IDTMA(IEL)     !
C-----------------!---------------------!-------------!-----------------
C &CATA.TE.NOMTE  ! V(K16) POINTEURS    !             !                !
C-----------------!---------------------!-------------!-----------------
C &CATA.TE.NUMINIT!  C(IS) POINTEE PAR: !  NUMINIT    ! IDNUMI(IEL)    !
C                 !       NOMTE         !             !                !
C-----------------!---------------------!-------------!-----------------
C &CATA.TE.PLMOLOC!  V(IS)              !  PLMOLC     ! IDPMLC(IMOD)   !
C-----------------!---------------------!-------------!-----------------
C &CATA.TE.NOMMOLOC   V(K24) POINTEURS  !             !                !
C-----------------!---------------------!-------------!-----------------
C &CATA.TE.MODELOC!  C(V(IS)) POINTEE PAR: MODELOC    ! IDMODI(IMOD,J) !
C                 ! NOMMOLOC ( NOMS )   !             ! IDMODK(MDLOC,J)!
C                 ! PLMOLOC ( LONGUEURS)!             !                !
C                 !                     !             !                !
C-----------------!---------------------!-------------!-----------------
C &CATA.TE.OPTTE  ! MR(IS)              !  OPTTE      ! IDOPTE(I,J)    !
C-----------------!---------------------!-------------!-----------------
C &CATA.TE.PLOPTMOD   V(IS)             !  PLOPMD     ! IDPOMD         !
C-----------------!---------------------!-------------!-----------------
C &CATA.TE.OPTMOD ! C(V(IS)) POINTEE PAR:  OPTMOD     ! IDOMDI(IOPT,J) !
C                 !          ( NUMEROS) !             !                !
C                 ! PLOPTMOD( LONGUEURS)!             !                !
C-----------------!---------------------!-------------!-----------------
C &CATA.TE.PLOPTNOM   V(IS)             !  PLOPNO     ! IDPONO         !
C-----------------!---------------------!-------------!-----------------
C &CATA.TE.OPTNOM ! C(V(K8)) POINTEE PAR:  OPTNOM     ! IDONOI(IOPT,J) !
C                 !          ( NUMEROS) !             !               )!
C                 ! PLOPTNOM( LONGUEURS)!             !                !
C-----------------!---------------------!-------------!-----------------
C &CATA.TE.PLCONVERS  V(IS)             !  PLCONV     ! IDPCON(IEL)    !
C-----------------!---------------------!-------------!-----------------
C &CATA.TE.CONVERS!  C(V(IS)) POINTEE PAR: CONVER     ! IDCONI(IEL,J)  !
C                 ! NOMTE    ( NOMS )   !             ! IDCONK(TE,J)   !
C                 ! PLCONVERS(LONGUEURS)!             !                !
C                 !                     !             !                !
C-----------------!---------------------!-------------!-----------------
C &CATA.TE.MODEFCA!  MR(IS)             !  MODFCA     ! IDMDFC(I,J)    !
C-----------------!---------------------!-------------!-----------------
C &CATA.TE.MODEFNO!  MR(IS)             !  MODFNO     ! IDMDFN(I,J)    !
C-----------------!---------------------!-------------!-----------------
      INTEGER TYPEMA,OPTNOM
      INTEGER NUMINI,PLMOLC,MODELO,OPTTE
      INTEGER PLOPMD,OPTMOD,PLOPNO,PLCONV,CONVER,MODFCA,MODFNO
C
      COMMON /COCATE/TYPEMA(2),OPTNOM(2),NUMINI(2),PLMOLC(2),MODELO(2),
     +       OPTTE(2),PLOPMD(2),OPTMOD(2),PLOPNO(2),PLCONV(2),CONVER(2),
     +       MODFCA(2),MODFNO(2)
C     EXCLUDE($OCATEL)
C     INCLUDE($OCATGD)
C
C     TABLEAUX FORTRAN CORRESPONDANTS AUX OBJETS DU CATALOGUE DES
C     GRANDEURS
C
C-----------------------------------------------------------------------
C  OBJET          ! TYPE JEVEUX         ! TABLEAU FTN !FONCTION D ACCES!
C-----------------------------------------------------------------------
C                 !                     !             !                !
C &CATA.GD.NOMGD  ! V(K8) REPERTOIRE    ! NOMGD       !                !
C                 !                     !             !                !
C-----------------!---------------------!-------------!----------------!
C                 !                     !             !                !
C &CATA.GD.LNOCMP ! V(IS)               ! LNOCMP      ! IDLNCP(IGD)    !
C                 !                     !             !                !
C-----------------!---------------------!-------------!----------------!
C                 !                     !             ! IDCMPI(IGD,J)  !
C &CATA.GD.NOMCMP ! C(V(K8)) POINTEE PAR! NOMCMP      ! IDCMPK(GD,J)   !
C                 !         NOMGD(NOMS) !             !                !
C                 !         LNOCMP(LONG)!             !                !
C-----------------!---------------------!-------------!----------------!
C                 !                     !             !                !
C &CATA.GD.TYPEGD ! V(K8)               ! TYPEGD      ! IDTGD(I)       !
C                 !                     !             !                !
C-----------------!---------------------!-------------!----------------!
C                 !                     !             ! IDDGDI(IGD,J)  !
C &CATA.GD.DESCRIGD   C(V(IS)) POINTEE PAR! DGD       ! IDDGDK(GD,J)   !
C                 !          NOMGD      !             !                !
C-----------------!---------------------!-------------!----------------!
      INTEGER DGD,LNOCMP
      INTEGER NOMGD,NOMCMP,TYPEGD
C
      COMMON /COCAGD/DGD(2),LNOCMP(2),NOMGD(2),NOMCMP(2),TYPEGD(2)
C
C     EXCLUDE($OCATGD)
C     INCLUDE($OCATOPT)
C
C     TABLEAUX FORTRAN CORRESPONDANTS AUX OBJETS DU CATALOGUE DES
C     OPTIONS
C
C-----------------------------------------------------------------------
C  OBJET          ! TYPE JEVEUX         ! TABLEAU FORT! FCTION ACCES   !
C-----------------!---------------------!-------------!-----------------
C &CATA.OP.NOMOPT ! V(K16) POINTEURS    ! NOMOPT      !                !
C-----------------!---------------------!-------------!-----------------
C &CATA.OP.LDCOPT ! V(IS)               ! LDCOPT      ! IDLCOP(I)      !
C-----------------!---------------------!-------------!-----------------
C &CATA.OP.DESCOPT!  C(V(IS)) POINTE PAR:! DESCOP     ! IDDOPI(IOPT,J) !
C                 ! NOMOPT ( NOMS )     !             ! IDDOPK(OPT,J)  !
C                 ! LDCOPT ( LONG )     !             !                !
C-----------------!---------------------!-------------!-----------------
C &CATA.OP.LOPARA ! V(IS)               ! LOPARA      ! IDLOPA(I)      !
C-----------------!---------------------!-------------!-----------------
C &CATA.OP.OPTPARA!  C(V(K8)) POINTE PAR:! OPTPAR     ! IDOPAI(IOPT,J) !
C                 ! NOMOPT ( NOMS )     !             ! IDOPAK(OPT,J)  !
C                 ! LOPARA ( LONG )     !             !                !
C-----------------!---------------------!-------------!-----------------
      INTEGER NOMOPT
      INTEGER OPTPAR
      INTEGER LDCOPT,DESCOP,LOPARA
C
      COMMON /COCAOP/NOMOPT(2),OPTPAR(2),LDCOPT(2),DESCOP(2),LOPARA(2)
C
C     EXCLUDE($OCATOPT)
C     INCLUDE($OCATTM)
C
C     TABLEAUX FORTRAN CORRESPONDANTS AUX OBJETS DU CATALOGUE DES
C     TYPE DE MAILLE
C
C-----------------------------------------------------------------------
C  OBJET          ! TYPE JEVEUX         ! TABLEAU FORT! FCTION ACCES   !
C-----------------!---------------------!-------------!-----------------
C &CATA.TM.NOMTM  ! V(IS) POINTEUR      !             !                !
C-----------------!---------------------!-------------!-----------------
C &CATA.TM.NBNO   ! C(IS) POINTEE PAR   !  NBNO       ! IDNBNO(ITM)    !
C                 ! &CATA.TM.NOMTM      !             !                !
C                 !     ( NOMS )        !             !                !
C-----------------!---------------------!-------------!-----------------
      INTEGER NBNO
      COMMON /COCATM/NBNO(2)
C     EXCLUDE($OCATTM)
C     INCLUDE($OCATPH)
C
C     TABLEAUX FORTRAN CORRESPONDANTS AUX OBJETS DU CATALOGUE DES
C     PHENOMENES-MODELISATION
C
C-----------------------------------------------------------------------
C  OBJET          ! TYPE JEVEUX    ! TABLEAU FORT! FCTION ACCES        !
C-----------------!----------------!-------------!----------------------
C &CATA.PHENOMENE ! POINTEUR DE NOM!             !                     !
C                 ! K16            !             !                     !
C                 !                !             !                     !
C &CATA.PHENOMENE ! C(V(IS))       !PHMOTE(2,PH)!IDPHMO(PH,IPH,IMD,ITM)!
C                 !                !             !                     !
C-----------------!----------------!-------------!----------------------
      INTEGER PHMOTE
      COMMON /COCAPH/NPHEMX,PHMOTE(2,10)
C
C  PHEMOM EST UN REPERTOIRE CONTENANT LES DIFFERENTS PHENOMENS
C  RENCONTRES DANS LE  CATALOGIE DES   PHENOMENES-MODELISATION
C
C IL Y A AUTANT D OBTETS &CATA.PHENOMENE QUE DE PHENOMES RENCONTRES
C DANS LE CATALOGUE
C
C NPHEMAX : NOMBRE MAX DE PHENOMES ACTUELLEMENT 10
C
C
C     EXCLUDE($OCATPH)
C
      NCLELE = 9
      CLELE(1) = 'ELEMENT         '
      CLELE(2) = 'MAILLE          '
      CLELE(3) = 'CARTE           '
      CLELE(4) = 'CHAMNO          '
      CLELE(5) = 'CHAMELEM        '
      CLELE(6) = 'VECTEUR         '
      CLELE(7) = 'MATRICE         '
      CLELE(8) = 'OPTION          '
      CLELE(9) = 'CONVERT         '
C
      NCLOPT = 6
      CLEOPT(1) = 'OPTION_SIMPLE   '
      CLEOPT(2) = 'OPTION_COMPOSEE '
      CLEOPT(3) = 'IN              '
      CLEOPT(4) = 'OUT             '
      CLEOPT(5) = 'INTER           '
      CLEOPT(6) = 'DECOMPOSITION   '
C
      NCLEPH = 5

      CLEPH(1) = 'PHENOMENES_MODELISATION '
      CLEPH(2) = 'PHENOMENE               '
      CLEPH(3) = 'MODELISATION            '
      CLEPH(4) = 'MAILLE                  '
      CLEPH(5) = 'ELEMENT                 '
C
C
C      MISE A ZERO DES ADRESSES D OBJET ET POINTEURS DE LONGUEUR
C      CUMULEE
C
      CHMOD(1) = 0
      CHMOD(2) = 0
C
      TYPEMA(1) = 0
      OPTNOM(1) = 0
      NUMINI(1) = 0
      PLMOLC(1) = 0
      MODELO(1) = 0
      OPTTE(1) = 0
      PLOPMD(1) = 0
      OPTMOD(1) = 0
      PLOPNO(1) = 0
      PLCONV(1) = 0
      CONVER(1) = 0
      MODFCA(1) = 0
      MODFNO(1) = 0
C
      TYPEMA(2) = 0
      OPTNOM(2) = 0
      NUMINI(2) = 0
      PLMOLC(2) = 0
      MODELO(2) = 0
      OPTTE(2) = 0
      PLOPMD(2) = 0
      OPTMOD(2) = 0
      PLOPNO(2) = 0
      PLCONV(2) = 0
      CONVER(2) = 0
      MODFCA(2) = 0
      MODFNO(2) = 0
C
      DGD(1) = 0
      LNOCMP(1) = 0
      NOMGD(1) = 0
      NOMCMP(1) = 0
      TYPEGD(1) = 0
C
      DGD(2) = 0
      LNOCMP(2) = 0
      NOMGD(2) = 0
      NOMCMP(2) = 0
      TYPEGD(2) = 0
C
      NOMOPT(1) = 0
      OPTPAR(1) = 0
      LDCOPT(1) = 0
      DESCOP(1) = 0
      LOPARA(1) = 0
C
      NOMOPT(2) = 0
      OPTPAR(2) = 0
      LDCOPT(2) = 0
      DESCOP(2) = 0
      LOPARA(2) = 0
C
      NBNO(1) = 0
      NBNO(2) = 0
C
      NPHEMX = 10
      DO 5 I = 1,10
        PHMOTE(1,I) = 0
        PHMOTE(2,I) = 0
    5 CONTINUE
C
      CLEDBG = 'DEBUG   '
      ICCDBG = 0
      NDMP = 0
      DO 10 IDMP = 1,30
        PASDMP(IDMP) = 0
        TYOBDM(IDMP) = 0
   10 CONTINUE
C
      IULMES = IUNIFI('MESSAGE')
      IULIST = IUNIFI('VIGILE ')
      IULVIG = IUNIFI('VIGILE ')
      IMP = 0
C
      GOTO 9999

 9999 CONTINUE
      END
