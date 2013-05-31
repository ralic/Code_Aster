subroutine dilini(option, nomte, ivf, ivf2, idfde,&
                  idfde2, jgano, ndim, ipoids, ipoid2,&
                  icompo, npi, dimdef, nddls, nddlm,&
                  dimcon, typmod, dimuel, nno, nnom,&
                  nnos, regula, axi, interp)
! ======================================================================
! person_in_charge: romeo.fernandes at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! aslint: disable=W1504
    implicit      none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/dimp0.h'
    include 'asterfort/dimp1.h'
    include 'asterfort/dimsl.h'
    include 'asterfort/elref1.h'
    include 'asterfort/elref4.h'
    include 'asterfort/u2mesk.h'
    logical :: axi
    integer :: ivf, ivf2, idfde, idfde2, jgano, ndim, ipoids, npi, nnom
    integer :: ipoid2, dimdef, dimuel, dimcon, nno, nnos, nddls, nddlm
    integer :: regula(6), icompo, nddlc
    character(len=2) :: interp
    character(len=8) :: typmod(2)
    character(len=16) :: option, nomte
! ======================================================================
! --- BUT : INITIALISATION DES GRANDEURS NECESSAIRES POUR LA GESTION ---
! ---       DU CALCUL AVEC REGULARISATION A PARTIR DU MODELE SECOND ----
! ---       GRADIENT A MICRO-DILATATION --------------------------------
! ======================================================================
! =====================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: nno2, nnos2, npi2, ibid, nnoc
    character(len=8) :: elrefe, elrf1, elrf2
! ======================================================================
! --- INITIALISATION ---------------------------------------------------
! ======================================================================
    interp = '  '
    typmod(2) = '        '
    elrf1 = '        '
    elrf2 = '        '
    axi = .false.
    dimdef = 0
    dimcon = 0
! ======================================================================
! --- TYPE D'ELEMENT ---------------------------------------------------
! ======================================================================
    call elref1(elrefe)
    if (elrefe .eq. 'TR7') then
        interp = 'P0'
        elrf1 = 'TR6'
        elrf2 = 'TR3'
    else if (elrefe.eq.'QU9') then
        interp = 'P0'
        elrf1 = 'QU8'
        elrf2 = 'QU4'
    else if (elrefe.eq.'TR6') then
        interp = 'SL'
        elrf1 = 'TR6'
        elrf2 = 'TR3'
    else if (elrefe.eq.'QU8') then
        interp = 'SL'
        elrf1 = 'QU8'
        elrf2 = 'QU4'
    else if (elrefe.eq.'T10') then
        interp = 'P1'
        elrf1 = 'T10'
        elrf2 = 'TE4'
    else if (elrefe.eq.'P15') then
        interp = 'P1'
        elrf1 = 'P15'
        elrf2 = 'PE6'
    else if (elrefe.eq.'H20') then
        interp = 'P1'
        elrf1 = 'H20'
        elrf2 = 'HE8'
    else
        call u2mesk('F', 'DVP_4', 1, elrefe)
    endif
! ======================================================================
! --- FONCTIONS DE FORME P2 --------------------------------------------
! ======================================================================
    call elref4(elrf1, 'RIGI', ndim, nno, nnos,&
                npi, ipoids, ivf, idfde, jgano)
! ======================================================================
! --- FONCTIONS DE FORME P1 --------------------------------------------
! ======================================================================
    call elref4(elrf2, 'RIGI', ndim, nno2, nnos2,&
                npi2, ipoid2, ivf2, idfde2, ibid)
! ======================================================================
! --- RECUPERATION DU TYPE DE LA MODELISATION --------------------------
! ======================================================================
    if (nomte(5:6) .eq. 'DP') then
        typmod(1) = 'D_PLAN  '
    else if (nomte(5:6).eq.'3D') then
        typmod(1) = '3D  '
    else
!       NOM D'ELEMENT ILLICITE
        call assert(nomte(5:6).eq.'DP' .or. nomte(5:6).eq.'3D')
    endif
! ======================================================================
    if (interp .eq. 'P0') then
        call dimp0(ndim, nno, nnos, dimdef, dimcon,&
                   nnom, nnoc, nddls, nddlm, nddlc,&
                   dimuel, regula)
    else if (interp.eq.'SL') then
        call dimsl(ndim, nno, nnos, dimdef, dimcon,&
                   nnom, nnoc, nddls, nddlm, nddlc,&
                   dimuel, regula)
    else if (interp.eq.'P1') then
        call dimp1(ndim, nno, nnos, dimdef, dimcon,&
                   nnom, nnoc, nddls, nddlm, nddlc,&
                   dimuel, regula)
    endif
! ======================================================================
end subroutine
