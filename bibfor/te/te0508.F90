subroutine te0508(option, nomte)
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
#include "jeveux.h"
!
#include "asterfort/elrefv.h"
#include "asterfort/jevech.h"
#include "asterfort/ngforc.h"
#include "asterfort/ngfore.h"
#include "asterfort/nmgvmb.h"
#include "asterfort/teattr.h"
#include "asterfort/terefe.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  OPTIONS FORC_NODA ET REFE_FORC_NODA
!                          POUR LA MODELISATION GRAD_VARI
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
    integer :: nnomax, npgmax, epsmax, ddlmax
    parameter (nnomax=27,npgmax=27,epsmax=20,ddlmax=15*nnomax)
! ......................................................................
    character(len=8) :: typmod(2)
    logical :: axi
    integer :: nno, nnob, npg, ndim, nddl, neps
    integer :: iret, nnos, jgano, ipoids, ivf, idfde, ivfb, idfdeb, jganob
    integer :: igeom, icontm, ivectu
    real(kind=8) :: sigref, varref, lagref, sref(11)
    real(kind=8) :: b(epsmax, npgmax, ddlmax), w(npgmax), ni2ldc(epsmax)
    character(len=16) :: nomelt
    common /ffauto/ nomelt
!
!
!
! - INITIALISATION
!
    nomelt = nomte
    call teattr('S', 'TYPMOD', typmod(1), iret)
    typmod(2) = 'GRADVARI'
    axi = typmod(1).eq.'AXIS'
!
    call elrefv(nomte, 'RIGI', ndim, nno, nnob,&
                nnos, npg, ipoids, ivf, ivfb,&
                idfde, idfdeb, jgano, jganob)
!
!
!
! - CALCUL DES ELEMENTS CINEMATIQUES
!
    call jevech('PGEOMER', 'L', igeom)
    call nmgvmb(ndim, nno, nnob, npg, axi,&
                zr(igeom), zr(ivf), zr(ivfb), idfde, idfdeb,&
                ipoids, nddl, neps, b, w,&
                ni2ldc)
!
! - OPTION FORC_NODA
!
    if (option .eq. 'FORC_NODA') then
        call jevech('PCONTMR', 'L', icontm)
        call jevech('PVECTUR', 'E', ivectu)
        call ngforc(nddl, neps, npg, w, b,&
                    ni2ldc, zr(icontm), zr(ivectu))
    endif
!
!
!
! - OPTION REFE_FORC_NODA
!
    if (option .eq. 'REFE_FORC_NODA') then
        call jevech('PVECTUR', 'E', ivectu)
!
!      LECTURE DES COMPOSANTES DE REFERENCE
        call terefe('SIGM_REFE', 'MECA_GRADVARI', sigref)
        call terefe('VARI_REFE', 'MECA_GRADVARI', varref)
        call terefe('LAGR_REFE', 'MECA_GRADVARI', lagref)
!
!      AFFECTATION DES CONTRAINTES GENERALISEES DE REFERENCE
        if (ndim .eq. 2) then
            sref(1) = sigref
            sref(2) = sigref
            sref(3) = sigref
            sref(4) = sigref
            sref(5) = lagref
            sref(6) = varref
            sref(7) = 0
            sref(8) = 0
        else if (ndim.eq.3) then
            sref(1) = sigref
            sref(2) = sigref
            sref(3) = sigref
            sref(4) = sigref
            sref(5) = sigref
            sref(6) = sigref
            sref(7) = lagref
            sref(8) = varref
            sref(9) = 0
            sref(10) = 0
            sref(11) = 0
        endif
!
        call ngfore(nddl, neps, npg, w, b,&
                    ni2ldc, sref, zr(ivectu))
    endif
!
end subroutine
