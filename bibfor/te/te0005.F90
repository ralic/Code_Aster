subroutine te0005(option, nomte)
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    implicit      none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/dilcar.h'
    include 'asterfort/dilele.h'
    include 'asterfort/dilini.h'
    include 'asterfort/epsdil.h'
    include 'asterfort/fnodil.h'
    character(len=16) :: option, nomte
! ======================================================================
! --- BUT : ROUTINE ELEMENTAIRE DE CALCUL DU MODELE --------------------
! ---       SECOND GRADIENT MICRO-DILATATION ---------------------------
! ======================================================================
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    logical :: axi
    integer :: i, ivf, ivf2, idfde, idfde2, jgano, ndim, ipoids, npi
    integer :: ipoid2, dimdef, icompo, ichg, ichn, regula(6), idefo
    integer :: icontm, ideplm, ideplp, igeom, imate, jcret, nddls, nddlm
    integer :: imatuu, ivectu, icontp, nno, nnos, nnom, dimuel, dimcon
    integer :: ivarip, codret
    character(len=2) :: interp
    character(len=8) :: typmod(2)
! ======================================================================
! --- INITIALISATION DU CODE RETOUR ------------------------------------
! ======================================================================
    codret = 0
! ======================================================================
! --- RECUPERATION DES ADRESSES DES CHAMPS DE LA CARTE DE L'ELEMENT ----
! ======================================================================
    call dilcar(option, icompo, icontm, ideplm, ideplp,&
                igeom, imate, imatuu, ivectu, icontp,&
                ivarip, ichg, ichn, jcret, idefo)
! ======================================================================
! --- INITIALISATION DES VARIABLES DE L'ELEMENT ------------------------
! ======================================================================
    call dilini(option, nomte, ivf, ivf2, idfde,&
                idfde2, jgano, ndim, ipoids, ipoid2,&
                icompo, npi, dimdef, nddls, nddlm,&
                dimcon, typmod, dimuel, nno, nnom,&
                nnos, regula, axi, interp)
! ======================================================================
! --- CALCUL ELEMENTAIRE -----------------------------------------------
! ======================================================================
    if (option(1:9) .eq. 'RIGI_MECA') then
        call dilele(option, typmod, npi, ndim, dimuel,&
                    nddls, nddlm, nno, nnos, nnom,&
                    axi, regula, dimcon, ipoids, ipoid2,&
                    ivf, ivf2, interp, idfde, idfde2,&
                    zk16(icompo), zr(igeom), zr(ideplm), zr(icontm), zi( imate),&
                    dimdef, zr(imatuu), zr(ivectu))
        else if (option(1:9).eq.'RAPH_MECA' .or. option(1:9)&
    .eq.'FULL_MECA' ) then
        do 10 i = 1, dimuel
            zr(ideplp-1+i)=zr(ideplm-1+i)+zr(ideplp-1+i)
10      continue
        call dilele(option, typmod, npi, ndim, dimuel,&
                    nddls, nddlm, nno, nnos, nnom,&
                    axi, regula, dimcon, ipoids, ipoid2,&
                    ivf, ivf2, interp, idfde, idfde2,&
                    zk16(icompo), zr(igeom), zr(ideplp), zr(icontp), zi( imate),&
                    dimdef, zr(imatuu), zr(ivectu))
        zi(jcret) = codret
! ======================================================================
! --- PHASE D'INITIALISATION DU PAS DE TEMPS A PARTIR DE L'INSTANT - ---
! ======================================================================
    else if (option.eq.'FORC_NODA') then
        call fnodil(dimuel, dimdef, nno, nnos, nnom,&
                    ndim, npi, dimcon, zr(igeom), ipoids,&
                    ipoid2, ivf, ivf2, interp, idfde,&
                    idfde2, nddls, nddlm, axi, regula,&
                    zr(ideplm), zr(icontm), zi(imate), zr(ivectu))
! ======================================================================
! --- OPTION : EPSI_ELGA ------------------------------------------
! ======================================================================
    else if (option.eq.'EPSI_ELGA') then
        call epsdil(npi, ipoids, ipoid2, ivf, ivf2,&
                    idfde, idfde2, zr(igeom), dimdef, dimuel,&
                    ndim, nddls, nddlm, nno, nnos,&
                    nnom, interp, axi, regula, zr(ideplp),&
                    zr(idefo))
    else
        call assert(.false.)
    endif
! ======================================================================
end subroutine
