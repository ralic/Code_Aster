subroutine inccat()
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
!     INCLUDE($CIMPERR)
!
!     EXCLUDE($CIMPERR)
!
!     INCLUDE($CMOTELE)
    character(len=16) :: clele
    common /cmelen/nclele
    common /cmelec/clele(9)
!     EXCLUDE($CMOTELE)
!     INCLUDE($CMOTOPT)
    character(len=16) :: cleopt
    common /cmtopn/nclopt
    common /cmtopc/cleopt(6)
!     EXCLUDE($CMOTOPT)
!     INCLUDE($CMOTPH)
    character(len=24) :: cleph
    common /cmtphn/ncleph
    common /cmtphc/cleph(5)
!
!     CLEPH(1) = 'PHENOMENES_MODELISATION '
!     CLEPH(2) = 'PHENOMENE               '
!     CLEPH(3) = 'MODELISATION            '
!     CLEPH(4) = 'MAILLE                  '
!     CLEPH(5) = 'ELEMENT                 '
!
!     EXCLUDE($CMOTPH)
!     INCLUDE($CDEBUG)
    character(len=8) :: cledbg
    character(len=24) :: objdmp
    integer :: pasdmp, tyobdm
    common /cmodbg/cledbg
    common /cdebug/iccdbg
    common /cbdmpc/objdmp(30)
    common /cbdmpn/ndmp,pasdmp(30),tyobdm(30)
!
!       NDMP : NOMBRE D OBJETS A DUMPER
!       PASDMP(IDMP)  : PASSE OU ON DUMPE L OBJET IDMP
!       OBJDMP(IDMP)  : NOM DE L OBJET IDMP
!       TYOBDM(IDMP)  : GENRE DE L OBJET IDMP :  0 OBJET SIMPLE
!                                                1 COLLECTION NUMEROTEE
!                                                2 COLLECTION NOMME
!
!     EXCLUDE($CDEBUG)
!
!     INCLUDE($OCATCO)
!
!     TABLEAUX FORTRAN CORRESPONDANTS AUX OBJETS DU CATALOGUE DES
!     COMMUNICATIONS
!
!-----------------------------------------------------------------------
!  OBJET          ! TYPE JEVEUX         ! TABLEAU FORT! FCTION ACCES   !
!-----------------!---------------------!-------------!-----------------
! &CATA.CO.CHMOD  ! MR(IS)              !  CHMOD      ! IDCHMO(I,J)    !
!                 !                     !             !                !
!-----------------!---------------------!-------------!-----------------
    integer :: chmod
    common /cocaco/chmod(2)
!     EXCLUDE($OCATCO)
!     INCLUDE($OCATEL)
!
!     TABLEAUX FORTRAN CORRESPONDANTS AUX OBJETS DU CATALOGUE DES
!     ELEMENTS
!
!-----------------------------------------------------------------------
!  OBJET          ! TYPE JEVEUX         ! TABLEAU FORT! FCTION ACCES   !
!-----------------!---------------------!-------------!-----------------
! &CATA.TE.TYPEMA ! V(K8)               !  TYPEMA     ! IDTMA(IEL)     !
!-----------------!---------------------!-------------!-----------------
! &CATA.TE.NOMTE  ! V(K16) POINTEURS    !             !                !
!-----------------!---------------------!-------------!-----------------
! &CATA.TE.NUMINIT!  C(IS) POINTEE PAR: !  NUMINIT    ! IDNUMI(IEL)    !
!                 !       NOMTE         !             !                !
!-----------------!---------------------!-------------!-----------------
! &CATA.TE.PLMOLOC!  V(IS)              !  PLMOLC     ! IDPMLC(IMOD)   !
!-----------------!---------------------!-------------!-----------------
! &CATA.TE.NOMMOLOC   V(K24) POINTEURS  !             !                !
!-----------------!---------------------!-------------!-----------------
! &CATA.TE.MODELOC!  C(V(IS)) POINTEE PAR: MODELOC    ! IDMODI(IMOD,J) !
!                 ! NOMMOLOC ( NOMS )   !             ! IDMODK(MDLOC,J)!
!                 ! PLMOLOC ( LONGUEURS)!             !                !
!                 !                     !             !                !
!-----------------!---------------------!-------------!-----------------
! &CATA.TE.OPTTE  ! MR(IS)              !  OPTTE      ! IDOPTE(I,J)    !
!-----------------!---------------------!-------------!-----------------
! &CATA.TE.PLOPTMOD   V(IS)             !  PLOPMD     ! IDPOMD         !
!-----------------!---------------------!-------------!-----------------
! &CATA.TE.OPTMOD ! C(V(IS)) POINTEE PAR:  OPTMOD     ! IDOMDI(IOPT,J) !
!                 !          ( NUMEROS) !             !                !
!                 ! PLOPTMOD( LONGUEURS)!             !                !
!-----------------!---------------------!-------------!-----------------
! &CATA.TE.PLOPTNOM   V(IS)             !  PLOPNO     ! IDPONO         !
!-----------------!---------------------!-------------!-----------------
! &CATA.TE.OPTNOM ! C(V(K8)) POINTEE PAR:  OPTNOM     ! IDONOI(IOPT,J) !
!                 !          ( NUMEROS) !             !               )!
!                 ! PLOPTNOM( LONGUEURS)!             !                !
!-----------------!---------------------!-------------!-----------------
! &CATA.TE.PLCONVERS  V(IS)             !  PLCONV     ! IDPCON(IEL)    !
!-----------------!---------------------!-------------!-----------------
! &CATA.TE.CONVERS!  C(V(IS)) POINTEE PAR: CONVER     ! IDCONI(IEL,J)  !
!                 ! NOMTE    ( NOMS )   !             ! IDCONK(TE,J)   !
!                 ! PLCONVERS(LONGUEURS)!             !                !
!                 !                     !             !                !
!-----------------!---------------------!-------------!-----------------
! &CATA.TE.MODEFCA!  MR(IS)             !  MODFCA     ! IDMDFC(I,J)    !
!-----------------!---------------------!-------------!-----------------
! &CATA.TE.MODEFNO!  MR(IS)             !  MODFNO     ! IDMDFN(I,J)    !
!-----------------!---------------------!-------------!-----------------
    integer :: typema, optnom
    integer :: numini, plmolc, modelo, optte
    integer :: plopmd, optmod, plopno, plconv, conver, modfca, modfno
!
    common /cocate/typema(2),optnom(2),numini(2),plmolc(2),modelo(2),&
     &       optte(2),plopmd(2),optmod(2),plopno(2),plconv(2),conver(2),&
     &       modfca(2),modfno(2)
!     EXCLUDE($OCATEL)
!     INCLUDE($OCATGD)
!
!     TABLEAUX FORTRAN CORRESPONDANTS AUX OBJETS DU CATALOGUE DES
!     GRANDEURS
!
!-----------------------------------------------------------------------
!  OBJET          ! TYPE JEVEUX         ! TABLEAU FTN !FONCTION D ACCES!
!-----------------------------------------------------------------------
!                 !                     !             !                !
! &CATA.GD.NOMGD  ! V(K8) REPERTOIRE    ! NOMGD       !                !
!                 !                     !             !                !
!-----------------!---------------------!-------------!----------------!
!                 !                     !             !                !
! &CATA.GD.LNOCMP ! V(IS)               ! LNOCMP      ! IDLNCP(IGD)    !
!                 !                     !             !                !
!-----------------!---------------------!-------------!----------------!
!                 !                     !             ! IDCMPI(IGD,J)  !
! &CATA.GD.NOMCMP ! C(V(K8)) POINTEE PAR! NOMCMP      ! IDCMPK(GD,J)   !
!                 !         NOMGD(NOMS) !             !                !
!                 !         LNOCMP(LONG)!             !                !
!-----------------!---------------------!-------------!----------------!
!                 !                     !             !                !
! &CATA.GD.TYPEGD ! V(K8)               ! TYPEGD      ! IDTGD(I)       !
!                 !                     !             !                !
!-----------------!---------------------!-------------!----------------!
!                 !                     !             ! IDDGDI(IGD,J)  !
! &CATA.GD.DESCRIGD   C(V(IS)) POINTEE PAR! DGD       ! IDDGDK(GD,J)   !
!                 !          NOMGD      !             !                !
!-----------------!---------------------!-------------!----------------!
    integer :: dgd, lnocmp
    integer :: nomgd, nomcmp, typegd
!
    common /cocagd/dgd(2),lnocmp(2),nomgd(2),nomcmp(2),typegd(2)
!
!     EXCLUDE($OCATGD)
!     INCLUDE($OCATOPT)
!
!     TABLEAUX FORTRAN CORRESPONDANTS AUX OBJETS DU CATALOGUE DES
!     OPTIONS
!
!-----------------------------------------------------------------------
!  OBJET          ! TYPE JEVEUX         ! TABLEAU FORT! FCTION ACCES   !
!-----------------!---------------------!-------------!-----------------
! &CATA.OP.NOMOPT ! V(K16) POINTEURS    ! NOMOPT      !                !
!-----------------!---------------------!-------------!-----------------
! &CATA.OP.LDCOPT ! V(IS)               ! LDCOPT      ! IDLCOP(I)      !
!-----------------!---------------------!-------------!-----------------
! &CATA.OP.DESCOPT!  C(V(IS)) POINTE PAR:! DESCOP     ! IDDOPI(IOPT,J) !
!                 ! NOMOPT ( NOMS )     !             ! IDDOPK(OPT,J)  !
!                 ! LDCOPT ( LONG )     !             !                !
!-----------------!---------------------!-------------!-----------------
! &CATA.OP.LOPARA ! V(IS)               ! LOPARA      ! IDLOPA(I)      !
!-----------------!---------------------!-------------!-----------------
! &CATA.OP.OPTPARA!  C(V(K8)) POINTE PAR:! OPTPAR     ! IDOPAI(IOPT,J) !
!                 ! NOMOPT ( NOMS )     !             ! IDOPAK(OPT,J)  !
!                 ! LOPARA ( LONG )     !             !                !
!-----------------!---------------------!-------------!-----------------
    integer :: nomopt
    integer :: optpar
    integer :: ldcopt, descop, lopara
!
    common /cocaop/nomopt(2),optpar(2),ldcopt(2),descop(2),lopara(2)
!
!     EXCLUDE($OCATOPT)
!     INCLUDE($OCATTM)
!
!     TABLEAUX FORTRAN CORRESPONDANTS AUX OBJETS DU CATALOGUE DES
!     TYPE DE MAILLE
!
!-----------------------------------------------------------------------
!  OBJET          ! TYPE JEVEUX         ! TABLEAU FORT! FCTION ACCES   !
!-----------------!---------------------!-------------!-----------------
! &CATA.TM.NOMTM  ! V(IS) POINTEUR      !             !                !
!-----------------!---------------------!-------------!-----------------
! &CATA.TM.NBNO   ! C(IS) POINTEE PAR   !  NBNO       ! IDNBNO(ITM)    !
!                 ! &CATA.TM.NOMTM      !             !                !
!                 !     ( NOMS )        !             !                !
!-----------------!---------------------!-------------!-----------------
    integer :: nbno
    common /cocatm/nbno(2)
!     EXCLUDE($OCATTM)
!     INCLUDE($OCATPH)
!
!     TABLEAUX FORTRAN CORRESPONDANTS AUX OBJETS DU CATALOGUE DES
!     PHENOMENES-MODELISATION
!
!-----------------------------------------------------------------------
!  OBJET          ! TYPE JEVEUX    ! TABLEAU FORT! FCTION ACCES        !
!-----------------!----------------!-------------!----------------------
! &CATA.PHENOMENE ! POINTEUR DE NOM!             !                     !
!                 ! K16            !             !                     !
!                 !                !             !                     !
! &CATA.PHENOMENE ! C(V(IS))       !PHMOTE(2,PH)!IDPHMO(PH,IPH,IMD,ITM)!
!                 !                !             !                     !
!-----------------!----------------!-------------!----------------------
    integer :: phmote
    common /cocaph/nphemx,phmote(2,10)
!
!  PHEMOM EST UN REPERTOIRE CONTENANT LES DIFFERENTS PHENOMENS
!  RENCONTRES DANS LE  CATALOGIE DES   PHENOMENES-MODELISATION
!
! IL Y A AUTANT D OBTETS &CATA.PHENOMENE QUE DE PHENOMES RENCONTRES
! DANS LE CATALOGUE
!
! NPHEMAX : NOMBRE MAX DE PHENOMES ACTUELLEMENT 10
!
!
!     EXCLUDE($OCATPH)
!
!-----------------------------------------------------------------------
    integer :: i, iccdbg, idmp, nclele, ncleph, nclopt, ndmp
    integer :: nphemx
!-----------------------------------------------------------------------
    nclele = 9
    clele(1) = 'ELEMENT         '
    clele(2) = 'MAILLE          '
    clele(3) = 'CARTE           '
    clele(4) = 'CHAMNO          '
    clele(5) = 'CHAMELEM        '
    clele(6) = 'VECTEUR         '
    clele(7) = 'MATRICE         '
    clele(8) = 'OPTION          '
    clele(9) = 'CONVERT         '
!
    nclopt = 6
    cleopt(1) = 'OPTION_SIMPLE   '
    cleopt(2) = 'OPTION_COMPOSEE '
    cleopt(3) = 'IN              '
    cleopt(4) = 'OUT             '
    cleopt(5) = 'INTER           '
    cleopt(6) = 'DECOMPOSITION   '
!
    ncleph = 5
!
    cleph(1) = 'PHENOMENES_MODELISATION '
    cleph(2) = 'PHENOMENE               '
    cleph(3) = 'MODELISATION            '
    cleph(4) = 'MAILLE                  '
    cleph(5) = 'ELEMENT                 '
!
!
!      MISE A ZERO DES ADRESSES D OBJET ET POINTEURS DE LONGUEUR
!      CUMULEE
!
    chmod(1) = 0
    chmod(2) = 0
!
    typema(1) = 0
    optnom(1) = 0
    numini(1) = 0
    plmolc(1) = 0
    modelo(1) = 0
    optte(1) = 0
    plopmd(1) = 0
    optmod(1) = 0
    plopno(1) = 0
    plconv(1) = 0
    conver(1) = 0
    modfca(1) = 0
    modfno(1) = 0
!
    typema(2) = 0
    optnom(2) = 0
    numini(2) = 0
    plmolc(2) = 0
    modelo(2) = 0
    optte(2) = 0
    plopmd(2) = 0
    optmod(2) = 0
    plopno(2) = 0
    plconv(2) = 0
    conver(2) = 0
    modfca(2) = 0
    modfno(2) = 0
!
    dgd(1) = 0
    lnocmp(1) = 0
    nomgd(1) = 0
    nomcmp(1) = 0
    typegd(1) = 0
!
    dgd(2) = 0
    lnocmp(2) = 0
    nomgd(2) = 0
    nomcmp(2) = 0
    typegd(2) = 0
!
    nomopt(1) = 0
    optpar(1) = 0
    ldcopt(1) = 0
    descop(1) = 0
    lopara(1) = 0
!
    nomopt(2) = 0
    optpar(2) = 0
    ldcopt(2) = 0
    descop(2) = 0
    lopara(2) = 0
!
    nbno(1) = 0
    nbno(2) = 0
!
    nphemx = 10
    do 5 i = 1, 10
        phmote(1,i) = 0
        phmote(2,i) = 0
 5  end do
!
    cledbg = 'DEBUG   '
    iccdbg = 0
    ndmp = 0
    do 10 idmp = 1, 30
        pasdmp(idmp) = 0
        tyobdm(idmp) = 0
10  end do
!
    goto 9999
!
9999  continue
end subroutine
