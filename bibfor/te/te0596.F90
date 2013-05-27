subroutine te0596(option, nomte)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: sebastien.fayolle at edf.fr
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/elref2.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/lteatt.h'
    include 'asterfort/niinit.h'
    include 'asterfort/nofnpd.h'
    include 'asterfort/nufnlg.h'
    include 'asterfort/nufnpd.h'
    include 'asterfort/teattr.h'
    include 'asterfort/u2mesk.h'
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
! FONCTION REALISEE:  CALCUL DE L'OPTION FORC_NODA POUR LES ELEMENTS
!                     INCOMPRESSIBLES A 2 CHAMPS UP
!                     EN 3D/D_PLAN/AXI
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ----------------------------------------------------------------------
!
    logical :: mini
    integer :: ndim, nno1, nno2, nnos, npg, jgn, ntrou
    integer :: iw, ivf1, ivf2, idf1, idf2
    integer :: vu(3, 27), vg(27), vp(27), vpi(3, 27)
    integer :: igeom, icontm, iddlm, icompo, imate, ivectu
    integer :: ibid
    character(len=8) :: lielrf(10), typmod(2), alias8
    character(len=24) :: valk
! ----------------------------------------------------------------------
!
! - FONCTIONS DE FORMES ET POINTS DE GAUSS
    call elref2(nomte, 10, lielrf, ntrou)
    call assert(ntrou.ge.2)
    call elref4(lielrf(2), 'RIGI', ndim, nno2, nnos,&
                npg, iw, ivf2, idf2, jgn)
    call elref4(lielrf(1), 'RIGI', ndim, nno1, nnos,&
                npg, iw, ivf1, idf1, jgn)
!
! - TYPE DE MODELISATION
    if (ndim .eq. 2 .and. lteatt(' ','AXIS','OUI')) then
        typmod(1) = 'AXIS  '
    else if (ndim.eq.2 .and. lteatt(' ','D_PLAN','OUI')) then
        typmod(1) = 'D_PLAN  '
    else if (ndim .eq. 3) then
        typmod(1) = '3D'
    else
        call u2mesk('F', 'ELEMENTS_34', 1, nomte)
    endif
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PCONTMR', 'L', icontm)
    call jevech('PDEPLMR', 'L', iddlm)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PVECTUR', 'E', ivectu)
!
! - CALCUL DES FORCES INTERIEURES
    if (zk16(icompo+2) (1:6) .eq. 'PETIT ') then
        if (lteatt(' ','INCO','C2PD ')) then
!
! - MINI ELEMENT ?
            call teattr(' ', 'S', 'ALIAS8', alias8, ibid)
            if (alias8(6:8) .eq. 'TR3' .or. alias8(6:8) .eq. 'TE4') then
                mini = .true.
            else
                mini = .false.
            endif
!
! - ACCES AUX COMPOSANTES DU VECTEUR DDL
            call niinit(nomte, typmod, ndim, nno1, 0,&
                        nno2, 0, vu, vg, vp,&
                        vpi)
!
            call nufnpd(ndim, nno1, nno2, npg, iw,&
                        zr(ivf1), zr(ivf2), idf1, vu, vp,&
                        typmod, zi(imate), zk16(icompo), zr(igeom), zr(icontm),&
                        zr(iddlm), mini, zr(ivectu))
        else if (lteatt(' ','INCO','C2PDO')) then
! - ACCES AUX COMPOSANTES DU VECTEUR DDL
            call niinit(nomte, typmod, ndim, nno1, 0,&
                        nno2, nno2, vu, vg, vp,&
                        vpi)
!
            call nofnpd(ndim, nno1, nno2, nno2, npg,&
                        iw, zr(ivf1), zr(ivf2), zr(ivf2), idf1,&
                        vu, vp, vpi, typmod, zi(imate),&
                        zk16(icompo), zr( igeom), nomte, zr(icontm), zr(iddlm),&
                        zr(ivectu))
        else
            valk = zk16(icompo+2)
            call u2mesk('F', 'MODELISA10_17', 1, valk)
        endif
    else if (zk16(icompo+2) (1:8).eq.'GDEF_LOG') then
        if (lteatt(' ','INCO','C2LG ')) then
!
! - ACCES AUX COMPOSANTES DU VECTEUR DDL
            call niinit(nomte, typmod, ndim, nno1, 0,&
                        nno2, 0, vu, vg, vp,&
                        vpi)
!
            call nufnlg(ndim, nno1, nno2, npg, iw,&
                        zr(ivf1), zr(ivf2), idf1, vu, vp,&
                        typmod, zi(imate), zk16(icompo), zr(igeom), zr(icontm),&
                        zr(iddlm), zr(ivectu))
        else
            valk = zk16(icompo+2)
            call u2mesk('F', 'MODELISA10_17', 1, valk)
        endif
    else
        call u2mesk('F', 'ELEMENTS3_16', 1, zk16(icompo+2))
    endif
!
end subroutine
