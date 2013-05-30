subroutine te0561(option, nomte)
!
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
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/elrefv.h'
    include 'asterfort/jevech.h'
    include 'asterfort/lteatt.h'
    include 'asterfort/nmfogn.h'
    include 'asterfort/nmforn.h'
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES FORCES NODALES
!                          POUR ELEMENTS NON LOCAUX  GVNO
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    character(len=8) :: typmod(2)
    integer :: nno, nnob, npg
    integer :: iw, ivf, idfde, igeom, imate
    integer :: ivfb, idfdeb, nnos, jgano, jganob
    integer :: icontm
    integer :: idplgm
    integer :: ivectu
    integer :: ndim
!
    character(len=16) :: nomelt
    common /ffauto/ nomelt
!
!
!
!
!
    nomelt = nomte
!
    call elrefv(nomelt, 'RIGI', ndim, nno, nnob,&
                nnos, npg, iw, ivf, ivfb,&
                idfde, idfdeb, jgano, jganob)
!
!
! - TYPE DE MODELISATION
!
    if (lteatt(' ','AXIS','OUI')) then
        typmod(1) = 'AXIS    '
    else if (lteatt(' ','D_PLAN','OUI')) then
        typmod(1) = 'D_PLAN  '
    else if (nomte(1:4).eq.'MNVG') then
        typmod(1) = '3D      '
    else
!       NOM D'ELEMENT ILLICITE
        call assert(nomte(1:4).eq.'MNVG')
    endif
!
    typmod(2) = 'GDVARINO'
!
!      CALL JEVECH('PDEPLMR','L',IDPLGM)
!      CALL JEVECH('PDEPLPR','L',IDDPLG)
!
    if (option .eq. 'FORC_NODA') then
!
        call jevech('PGEOMER', 'L', igeom)
        call jevech('PCONTMR', 'L', icontm)
        call jevech('PMATERC', 'L', imate)
        call jevech('PDEPLMR', 'L', idplgm)
        call jevech('PVECTUR', 'E', ivectu)
!
        call nmfogn(ndim, nno, nnob, npg, iw,&
                    zr(ivf), zr(ivfb), idfde, idfdeb, zr(igeom),&
                    typmod, zi(imate), zr(idplgm), zr(icontm), zr( ivectu))
!
    endif
!
!
    if (option .eq. 'REFE_FORC_NODA') then
!
        call jevech('PMATERC', 'L', imate)
        call jevech('PGEOMER', 'L', igeom)
        call jevech('PVECTUR', 'E', ivectu)
!
        call nmforn(ndim, nno, nnob, npg, iw,&
                    zr(ivf), zr(ivfb), idfde, zr( igeom), zr(ivectu))
    endif
!
!
!
end subroutine
