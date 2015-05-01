subroutine vpinis(eigsol)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! -------------------------------------------------------------------------------------------------
! LECTURE DES PARAMETRES SOLVEUR MODAL ET CREATION DE LA SD ASSOCIEE
! CF VPINIS, VPLECS, VPLECI, VPECRI.
! RQ1. ON CREE LES OBJETS GLOBAUX LIES A EIGSOL SUR BASE VOLATILE.
!      ILS SONT DETRUITS DANS VPPOST.
! -------------------------------------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
    implicit none

#include "jeveux.h"
#include "asterc/isnnem.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/omega2.h"
#include "asterfort/vecini.h"
#include "asterfort/vecink.h"
#include "asterfort/vecint.h"
#include "asterfort/wkvect.h"

!
! --- INPUT
!
    character(len=19) , intent(in) :: eigsol
!
! --- OUTPUT
! None
!
! --- INPUT/OUTPUT
! None
!
! --- VARIABLES LOCALES
!
    integer           :: eislvi, eislvk, eislvr, iret, ibuff, itemax, nbborn, nborto, nbrss
    integer           :: nbvect, nbvec2, nfreq, nitv, nperm, maxitr, indf
    real(kind=8)      :: alpha, fcorig, omecor, precsh, precdc, prorto, prsudg, seuil, undf
    real(kind=8)      :: tol, toldyn, tolsor
    character(len=1)  :: appr
    character(len=8)  :: arret, method
    character(len=9)  :: typevp
    character(len=14) :: matra, matrb, matrc
    character(len=16) :: optiof, modrig, stoper, sturm, typeqz, typres
    character(len=19) :: amor, k19b, masse, tabmod, raide
    character(len=24) :: kzero

!
! -----------------------
! --- CORPS DE LA ROUTINE
! -----------------------
!

! --- INITS.
    call jemarq()
    undf=r8vide()
    indf=isnnem()
    kzero=' '

! --- CREATION DE LA SD
    call wkvect(eigsol//'.ESVK', 'V V K24', 20, eislvk)
    call wkvect(eigsol//'.ESVR', 'V V R', 15, eislvr)
    call wkvect(eigsol//'.ESVI', 'V V I', 15, eislvi)
    call vecink(20,kzero,zk24(eislvk))
    call vecini(15, undf,zr(eislvr))
    call vecint(15,indf,zi(eislvi))

! --- PARAMETRES COMMUNS A TOUS LES SOLVEURS MODAUX

! --  TYPE_RESU
    typres=''
    call getvtx(' ', 'TYPE_RESU', iocc=1, scal=typres, nbret=iret)
    ASSERT(iret.eq.1)
! --  CATALOGUE DE COMMANDE, DIFFERENT SELON LE TYPE_RESU
!     -> ON STOCKE DANS DES VARIABLES POUR EVITER DE FAIRE DES GETXXX
!     POUR CHAQUE TYPE_RESU.
!     POUR L'INSTANT TYPE_RESU='GENERAL' REVIENT A 'MODE_FLAMB' SAUF LE NOM DES MATRICES
    select case(typres)
    case('DYNAMIQUE')
        matra ='MATR_RIGI'
        matrb ='MATR_MASS'
        matrc ='MATR_AMOR'
        typevp='FREQ'
    case('MODE_FLAMB')
        matra ='MATR_RIGI'
        matrb ='MATR_RIGI_GEOM'
        typevp='CHAR_CRIT'
    case('GENERAL')
        matra ='MATR_A'
        matrb ='MATR_B'
        matrc ='MATR_C'
        typevp='CHAR_CRIT'
        typres='MODE_FLAMB'
    case default
        ASSERT(.false.)
    end select

! --  CALC_FREQ/CHAR_CRIT
    optiof=''
    call getvtx('CALC_'//typevp, 'OPTION', iocc=1, scal=optiof, nbret=iret)
    ASSERT(iret.eq.1)

! --  MATRA/MATRB/MATRC
    raide=''
    call getvid(' ', matra, iocc=1, scal=raide, nbret=iret)
    ASSERT(iret.eq.1)
    masse=''
    call getvid(' ', matrb, iocc=1, scal=masse, nbret=iret)
    ASSERT(iret.eq.1)
    if (typres(1:10).ne.'MODE_FLAMB') then
        k19b=''
        call getvid(' ', matrc, iocc=1, scal=k19b, nbret=iret)
        if (iret.eq.1) then
           amor=trim(k19b)
        else
           amor=''
        endif
    else
        amor=''
    endif

! --  METHODE
    method=''
    call getvtx(' ', 'METHODE', iocc=1, scal=method, nbret=iret)
    ASSERT(iret.eq.1)

! --  DETECTION DES MODES DE CORPS RIGIDE
    modrig=''
    call getvtx(' ', 'OPTION', iocc=1, scal=modrig, nbret=iret)
    ASSERT(iret.eq.1)

! -- NOMBRE DE MODES DEMANDES
    call getvis('CALC_'//typevp, 'NMAX_'//typevp, iocc=1, scal=ibuff, nbret=iret)
    if (iret.eq.1) then
        nfreq=ibuff
    else
        nfreq=0
    endif

! -- LISTES DES FREQUENCES/CHARGES CRITIQUES OU TABLE_ASTER
    call getvr8('CALC_'//typevp, typevp, iocc=1, nbval=0, nbret=iret)
    if (iret.lt.0) then
        nbborn=-iret
        ASSERT((nbborn.eq.1).or.(nbborn.eq.2))
        call getvr8('CALC_'//typevp, typevp, iocc=1, nbval=nbborn, vect=zr(eislvr), nbret=iret)
        ASSERT(iret.eq.nbborn)
    else
        nbborn=0
    endif

    call getvid('CALC_'//typevp, 'TABLE_'//typevp, iocc=1, scal=k19b, nbret=iret)
    if (iret.eq.1) then
        tabmod=trim(k19b)
    else
        tabmod=''
    endif

! --  PARAMETRES ESPACE REDUIT
    call getvis('CALC_'//typevp, 'DIM_SOUS_ESPACE', iocc=1, scal=ibuff, nbret=iret)
    if (iret.eq.1) then
        nbvect=ibuff
    else
        nbvect=0
    endif
    call getvis('CALC_'//typevp, 'COEF_DIM_ESPACE', iocc=1, scal=ibuff, nbret=iret)
    if (iret.eq.1) then
        nbvec2=ibuff
    else
        nbvec2=0
    endif

! --  PARAMETRES SHIFT PRETRAITEMENTS
    call getvis('CALC_'//typevp, 'NMAX_ITER_SHIFT', iocc=1, scal=nbrss, nbret=iret)
    ASSERT(iret.eq.1)
    call getvr8('CALC_'//typevp, 'PREC_SHIFT', iocc=1, scal=precsh, nbret=iret)
    ASSERT(iret.eq.1)
    call getvr8('CALC_'//typevp, 'SEUIL_'//typevp, iocc=1, scal=fcorig, nbret=iret)
    ASSERT(iret.eq.1)
    omecor = 0.d0
    if (typres(1:9) .eq. 'DYNAMIQUE') omecor = omega2(fcorig)
 
! --  PARAMETRES DES SOLVEURS MODAUX
    appr=''
    call getvtx('CALC_'//typevp, 'APPROCHE', iocc=1, scal=appr, nbret=iret)
    ASSERT(iret.eq.1)

! --  PARAM LANCZOS
    select case(method)
    case('TRI_DIAG')
        call getvis(' ', 'NMAX_ITER_ORTHO', iocc=1, scal=nborto, nbret=iret)
        ASSERT(iret.eq.1)
        call getvr8(' ', 'PREC_ORTHO', iocc=1, scal=prorto, nbret=iret)
        ASSERT(iret.eq.1)
        call getvr8(' ', 'PREC_LANCZOS', iocc=1, scal=prsudg, nbret=iret)
        ASSERT(iret.eq.1)
        call getvis(' ', 'NMAX_ITER_QR', iocc=1, scal=nitv, nbret=iret)
        ASSERT(iret.eq.1)
! --  PARAM JACOBI
    case ('JACOBI')
        call getvis(' ', 'NMAX_ITER_BATHE ', iocc=1, scal=itemax, nbret=iret)
        ASSERT(iret.eq.1)
        call getvr8(' ', 'PREC_BATHE', iocc=1, scal=tol, nbret=iret)
        ASSERT(iret.eq.1)
        call getvis(' ', 'NMAX_ITER_JACOBI', iocc=1, scal=nperm, nbret=iret)
        ASSERT(iret.eq.1)
        call getvr8(' ', 'PREC_JACOBI', iocc=1, scal=toldyn, nbret=iret)
        ASSERT(iret.eq.1)
! --  PARAM SORENSEN
    case ('SORENSEN')
        call getvr8(' ', 'PREC_SOREN', iocc=1, scal=tolsor, nbret=iret)
        ASSERT(iret.eq.1)
        call getvis(' ', 'NMAX_ITER_SOREN', iocc=1, scal=maxitr, nbret=iret)
        ASSERT(iret.eq.1)
        call getvr8(' ', 'PARA_ORTHO_SOREN', iocc=1, scal=alpha, nbret=iret)
        ASSERT(iret.eq.1)
! --  PARAM QZ
    case ('QZ')
        typeqz=''
        call getvtx(' ', 'TYPE_QZ', scal=typeqz, nbret=iret)
        ASSERT(iret.eq.1)
    case default
        ASSERT(.false.)
    end select

! --  PARAMETRES POST-TRAITEMENTS
    call getvr8('VERI_MODE', 'PREC_SHIFT', iocc=1, scal=precdc, nbret=iret)
    ASSERT(iret.eq.1)
    stoper=''
    call getvtx('VERI_MODE', 'STOP_ERREUR', iocc=1, scal=stoper, nbret=iret)
    ASSERT(iret.eq.1)
    sturm=''
    call getvtx('VERI_MODE', 'STURM', iocc=1, scal=sturm, nbret=iret)
    ASSERT(iret.eq.1)
    call getvr8('VERI_MODE', 'SEUIL', iocc=1, scal=seuil, nbret=iret)
    ASSERT(iret.eq.1)

! -- DIVERS
    arret=''
    call getvtx(' ', 'STOP_BANDE_VIDE', scal=arret, nbret=iret)
    ASSERT(iret.eq.1)

! --- REMPLISSAGE DE LA SD

! --  PARAMETRES K24 GENERAUX
    zk24(eislvk-1+1)=trim(typres)
    zk24(eislvk-1+2)=trim(raide)
    zk24(eislvk-1+3)=trim(masse)
    zk24(eislvk-1+4)=trim(amor)
    zk24(eislvk-1+5)=trim(optiof)
    zk24(eislvk-1+6)=trim(method)
    zk24(eislvk-1+7)=trim(modrig)
    zk24(eislvk-1+8)=trim(arret)
    zk24(eislvk-1+9)=trim(tabmod)
    zk24(eislvk-1+10)=trim(stoper)
    zk24(eislvk-1+11)=trim(sturm)

! --  PARAMETRES K24 LIES AUX SOLVEURS MODAUX
    zk24(eislvk-1+16)=trim(appr)
    select case(method)
    case('TRI_DIAG')
    case ('JACOBI')
    case ('SORENSEN')
    case ('QZ')
        zk24(eislvk-1+17)=trim(typeqz)
    case default
        ASSERT(.false.)
    end select

! --  PARAMETRES ENTIERS GENERAUX
    zi(eislvi-1+1)=nfreq
    zi(eislvi-1+2)=nbvect
    zi(eislvi-1+3)=nbvec2
    zi(eislvi-1+4)=nbrss
    zi(eislvi-1+5)=nbborn

! -- PARAMETRES ENTIERS LIES AUX SOLVEURS MODAUX
    select case(method)
    case('TRI_DIAG')
        zi(eislvi-1+11)=nborto
        zi(eislvi-1+12)=nitv
    case ('JACOBI')
        zi(eislvi-1+11)=itemax
        zi(eislvi-1+12)=nperm
    case ('SORENSEN')
        zi(eislvi-1+11)=maxitr
    case ('QZ')
    case default
        ASSERT(.false.)
    end select

! --  PARAMETRES REELS GENERAUX
! --  POUR MEMOIRE: UNE OU DEUX PREMIERES VALEURS DEJA AFFECTEES PAR GETVR8 PRECEDENT
!   zr(eislvr-1+1)=freq1
!   zr(eislvr-1+2)=freq2
    zr(eislvr-1+3)=precsh
    zr(eislvr-1+4)=omecor
    zr(eislvr-1+5)=precdc
    zr(eislvr-1+6)=seuil

! --  PARAMETRES REELS LIES AUX SOLVEURS MODAUX
    select case(method)
    case('TRI_DIAG')
        zr(eislvr-1+11)=prorto
        zr(eislvr-1+12)=prsudg
    case ('JACOBI')
        zr(eislvr-1+11)=tol
        zr(eislvr-1+12)=toldyn
    case ('SORENSEN')
        zr(eislvr-1+11)=tolsor
        zr(eislvr-1+12)=alpha
    case ('QZ')
    case default
        ASSERT(.false.)
    end select
    
    call jedema()
!
!     FIN DE VPINIS
!
end subroutine
