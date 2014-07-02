subroutine vplecs(eigsol, itemax, maxitr, nbborn, nitv,&
                  nborto, nbvec2, nbvect, nbrss, nfreq,&
                  nperm, alpha, omecor, freq1, freq2,&
                  precdc, precsh, prorto, prsudg, seuil,&
                  tol, toldyn, tolsor, appr, arret,&
                  method, typevp, matra, matrb, matrc,&
                  modrig, optiof, stoper, sturm, typeqz,&
                  typres, amor, masse, raide, tabmod,&
                  lc, lkr, lns, lpg, lqz)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ROUTINE UTILITAIRE LISANT UNE SD_EIGENSOLVER ET CREEANT QUELQUES VALEURS DE LOGICAL ET CHARACTER
! UTILES POUR LA SUITE D'UN CALCUL MODAL DE MODE_ITER_SIMULT.
! POUR LIRE UNE SEULE VALEUR UTILISER PLUTOT VPLECI.
! CF VPINIS, VPLECS, VPLECI, VPECRI.
! RQ. OPTIOF='PLUS_PETITE' + LPG=.TRUE. SIGNIFIE QUE LE CHAMPS STOCKE CONTIENT EN FAIT LA CHAINE
!     'PLUS_GRANDE'.
! -------------------------------------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/isnnem.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
!
! --- INPUT
!
    character(len=19), intent(in) :: eigsol
!
! --- OUTPUT
!
    integer, intent(out) :: itemax, maxitr, nbborn, nitv, nborto, nbvec2, nbvect, nbrss
    integer, intent(out) :: nfreq, nperm
    real(kind=8), intent(out) :: alpha, omecor, freq1, freq2, precdc, precsh, prorto, prsudg
    real(kind=8), intent(out) :: seuil, tol, toldyn, tolsor
    character(len=1), intent(out) :: appr
    character(len=8), intent(out) :: arret, method
    character(len=9), intent(out) :: typevp
    character(len=14), intent(out) :: matra, matrb, matrc
    character(len=16), intent(out) :: modrig, optiof, stoper, sturm, typeqz, typres
    character(len=19), intent(out) :: amor, masse, raide, tabmod
    aster_logical , intent(out) :: lc, lkr, lns, lpg, lqz
!
! --- INPUT/OUTPUT
! None
!
! --- VARIABLES LOCALES
!
    integer :: eislvi, eislvk, eislvr, jrefa, indf
    real(kind=8) :: undf
    character(len=1) :: ktyp
    aster_logical :: lnsc, lnsk, lnsm
!
! --  BUFFERS DE LECTURE (EN CAS D'APPELS AVEC K*BID, PASSAGE PAR REFERENCE)
    character(len=1) :: app2
    character(len=8) :: arre2, metho2
    character(len=16) :: modri2, optio2, stope2, stur2, typeq2, typre2
    character(len=19) :: amo2, mass2, raid2, tabmo2
!
! -----------------------
! --- CORPS DE LA ROUTINE
! -----------------------
!
!
! --  INITS.
    call jemarq()
    indf=isnnem()
    undf=r8vide()
!
! --  OUVERTURE DE LA SD
    call jeveuo(eigsol//'.ESVK', 'L', eislvk)
    call jeveuo(eigsol//'.ESVI', 'L', eislvi)
    call jeveuo(eigsol//'.ESVR', 'L', eislvr)
!
! --  LECTURE PARAMETRES SOLVEURS MODAUX CHARACTER
    typre2=''
    typre2=trim(zk24(eislvk-1+1))
    raid2=''
    raid2 =trim(zk24(eislvk-1+2))
    mass2=''
    mass2 =trim(zk24(eislvk-1+3))
    amo2=''
    amo2  =trim(zk24(eislvk-1+4))
    optio2=''
    optio2=trim(zk24(eislvk-1+5))
    metho2=''
    metho2=trim(zk24(eislvk-1+6))
    modri2=''
    modri2=trim(zk24(eislvk-1+7))
    arre2=''
    arre2 =trim(zk24(eislvk-1+8))
    tabmo2=''
    tabmo2=trim(zk24(eislvk-1+9))
    stope2=''
    stope2=trim(zk24(eislvk-1+10))
    stur2=''
    stur2 =trim(zk24(eislvk-1+11))
    app2=''
    app2  =trim(zk24(eislvk-1+16))
    typeq2=''
    select case (metho2)
        case('TRI_DIAG')
        case('JACOBI')
        case('SORENSEN')
        case('QZ')
        typeq2=trim(zk24(eislvk-1+17))
    case default
        ASSERT(.false.)
    end select
!
! --  LECTURE PARAMETRES SOLVEURS MODAUX ENTIERS
    nfreq =zi(eislvi-1+1)
    nbvect=zi(eislvi-1+2)
    nbvec2=zi(eislvi-1+3)
    nbrss =zi(eislvi-1+4)
    nbborn=zi(eislvi-1+5)
    nborto=indf
    nitv  =indf
    itemax=indf
    nperm =indf
    maxitr=indf
    select case (metho2)
        case('TRI_DIAG')
        nborto=zi(eislvi-1+11)
        nitv  =zi(eislvi-1+12)
        case('JACOBI')
        itemax=zi(eislvi-1+11)
        nperm =zi(eislvi-1+12)
        case('SORENSEN')
        maxitr=zi(eislvi-1+11)
        case('QZ')
    case default
        ASSERT(.false.)
    end select
!
! --  LECTURE PARAMETRES SOLVEURS MODAUX REELS
    freq1 =zr(eislvr-1+1)
    freq2 =zr(eislvr-1+2)
    precsh=zr(eislvr-1+3)
    omecor=zr(eislvr-1+4)
    precdc=zr(eislvr-1+5)
    seuil =zr(eislvr-1+6)
    prorto=undf
    prsudg=undf
    tol   =undf
    toldyn=undf
    tolsor=undf
    alpha =undf
    select case (metho2)
        case('TRI_DIAG')
        prorto=zr(eislvr-1+11)
        prsudg=zr(eislvr-1+12)
        case('JACOBI')
        tol   =zr(eislvr-1+11)
        toldyn=zr(eislvr-1+12)
        case('SORENSEN')
        tolsor=zr(eislvr-1+11)
        alpha =zr(eislvr-1+12)
        case('QZ')
    case default
        ASSERT(.false.)
    end select
!
! --  INIT. MATRA/B/C ET TYPEVP
    select case (typre2)
        case('DYNAMIQUE')
        matra ='MATR_RIGI'
        matrb ='MATR_MASS'
        matrc ='MATR_AMOR'
        typevp='FREQ'
        case('MODE_FLAMB')
        matra ='MATR_RIGI'
        matrb ='MATR_RIGI_GEOM'
        matrc =''
        typevp='CHAR_CRIT'
        case('GENERAL')
        matra ='MATR_A'
        matrb ='MATR_B'
        matrc ='MATR_C'
        typevp='CHAR_CRIT'
    case default
        ASSERT(.false.)
    end select
!
! --  INIT. LC
    if (amo2 .eq. '') then
        lc=.false.
    else
        lc=.true.
    endif
!
! --  INIT. LPG
    if (optio2(1:11) .eq. 'PLUS_GRANDE') then
        lpg=.true.
        optio2='PLUS_PETITE'
    else
        lpg=.false.
    endif
!
! --  INIT. LQZ
    if (metho2(1:2) .eq. 'QZ') then
        lqz=.true.
    else
        lqz=.false.
    endif
!
! --  INIT. LKR
!
    call jelira(raid2//'.VALM', 'TYPE', cval=ktyp)
    if (ktyp .eq. 'R') then
        lkr=.true.
    else if (ktyp.eq.'C') then
        lkr=.false.
    else
        ASSERT(.false.)
    endif
!
! --  INIT. LNS
    call jeveuo(raid2//'.REFA', 'L', jrefa)
    if (trim(zk24(jrefa-1+9)) .eq. 'MS') then
        lnsk=.false.
    else if (trim(zk24(jrefa-1+9)).eq.'MR') then
        lnsk=.true.
    else
        ASSERT(.false.)
    endif 
    call jeveuo(mass2//'.REFA', 'L', jrefa)
    if (trim(zk24(jrefa-1+9)) .eq. 'MS') then
        lnsm=.false.
    else if (trim(zk24(jrefa-1+9)).eq.'MR') then
        lnsm=.true.
    else
        ASSERT(.false.)
    endif
    if (lc) then
        call jeveuo(amo2//'.REFA', 'L', jrefa)
        if (trim(zk24(jrefa-1+9)) .eq. 'MS') then
            lnsc=.false.
        else if (trim(zk24(jrefa-1+9)).eq.'MR') then
            lnsc=.true.
        else
            ASSERT(.false.)
        endif
    else
        lnsc=.false.
    endif
    if (lnsk .or. lnsm .or. lnsc) then
        lns=.true.
    else
        lns=.false.
    endif
!
! --  CHARGEMENT DES VALEURS OUTPUTS CHARACTER
    appr=trim(app2)
    arret=trim(arre2)
    method=trim(metho2)
    modrig=trim(modri2)
    optiof=trim(optio2)
    stoper=trim(stope2)
    sturm=trim(stur2)
    typeqz=trim(typeq2)
    typres=trim(typre2)
    amor=trim(amo2)
    masse=trim(mass2)
    raide=trim(raid2)
    tabmod=trim(tabmo2)
!
    call jedema()
!
!     FIN DE VPLECS
!
end subroutine
