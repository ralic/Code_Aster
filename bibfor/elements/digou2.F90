subroutine digou2(option, nomte, ndim, nbt, nno,&
                  nc, ulm, dul, pgl, iret)
    implicit none
#include "jeveux.h"
#include "asterfort/digouj.h"
#include "asterfort/infdis.h"
#include "asterfort/jevech.h"
#include "asterfort/ut2mgl.h"
#include "asterfort/ut2mlg.h"
#include "asterfort/utpsgl.h"
#include "asterfort/utpslg.h"
#include "blas/dcopy.h"
!
    character(len=*) :: option, nomte
    integer :: ndim, nbt, nno, nc, iret
    real(kind=8) :: ulm(12), dul(12), pgl(3, 3)
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
! person_in_charge: jean-luc.flejou at edf.fr
! --------------------------------------------------------------------------------------------------
!
!  IN
!     option   : option de calcul
!     nomte    : nom terme élémentaire
!     ndim     : dimension du problème
!     nbt      : nombre de terme dans la matrice de raideur
!     nno      : nombre de noeuds de l'élément
!     nc       : nombre de composante par noeud
!     ulm      : déplacement moins
!     dul      : incrément de déplacement
!     pgl      : matrice de passage de global a local
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jdc, irep, imat, ivarim, ifono, icontp, ivarip, icompo, icontm, neq
    real(kind=8) :: r8bid, klv(78), klv2(78)
    character(len=8) :: k8bid
!
!   paramètres en entrée
    call jevech('PCADISK', 'L', jdc)
!   matrice de rigidité en repère local
    call infdis('REPK', irep, r8bid, k8bid)
    if (irep .eq. 1) then
        if (ndim .eq. 3) then
            call utpsgl(nno, nc, pgl, zr(jdc), klv)
        elseif (ndim.eq.2) then
            call ut2mgl(nno, nc, pgl, zr(jdc), klv)
        endif
    else
        call dcopy(nbt, zr(jdc), 1, klv, 1)
    endif
!
    call jevech('PCOMPOR', 'L', icompo)
!
    if (option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
        call jevech('PMATERC', 'L', imat)
        call jevech('PCONTMR', 'L', icontm)
        call jevech('PVARIMR', 'L', ivarim)
        call jevech('PVECTUR', 'E', ifono)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
    elseif (option.eq.'RIGI_MECA_TANG') then
        call jevech('PMATERC', 'L', imat)
        call jevech('PCONTMR', 'L', icontm)
        call jevech('PVARIMR', 'L', ivarim)
        ifono = 1
        icontp= 1
        ivarip= 1
    endif
!   relation de comportement : élastique partout
!   sauf suivant Y local : élasto-plastique VMIS_ISOT_TRAC
    neq = nno*nc
    call digouj(option, zk16(icompo), nno, nbt, neq,&
                nc, zi(imat), dul, zr(icontm), zr(ivarim),&
                pgl, klv, klv2, zr(ivarip), zr(ifono),&
                zr(icontp), nomte)
!
    if (option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
        call jevech('PMATUUR', 'E', imat)
        if (ndim .eq. 3) then
            call utpslg(nno, nc, pgl, klv, zr(imat))
        elseif (ndim.eq.2) then
            call ut2mlg(nno, nc, pgl, klv, zr(imat))
        endif
    endif
!
end subroutine
