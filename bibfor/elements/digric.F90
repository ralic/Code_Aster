subroutine digric(option, nomte, ndim, nbt, nno,&
                  nc, ulm, dul, pgl, iret)
    implicit none
#include "jeveux.h"
#include "asterfort/dicrgr.h"
#include "asterfort/jevech.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
!
    character(len=*) :: option, nomte
    integer :: ndim, nbt, nno, nc, iret
    real(kind=8) :: ulm(12), dul(12), pgl(3, 3)
!
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
    integer :: iadzi, iazk24, imat, ivarim, jtp, ifono, icontp, ivarip, neq, icompo, icontm
    real(kind=8) :: klv(78)
    character(len=24) :: messak(5)
!
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PCONTMR', 'L', icontm)
!
    if (nomte .ne. 'MECA_DIS_TR_L') then
        messak(1) = nomte
        messak(2) = option
        messak(3) = zk16(icompo+3)
        messak(4) = zk16(icompo)
        call tecael(iadzi, iazk24)
        messak(5) = zk24(iazk24-1+3)
        call utmess('F', 'DISCRETS_11', nk=5, valk=messak)
    endif
!   paramètres en entrée
    call jevech('PMATERC', 'L', imat)
    call jevech('PVARIMR', 'L', ivarim)
    call jevech('PINSTPR', 'L', jtp)
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        call jevech('PVECTUR', 'E', ifono)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
    else
        ifono = 1
        icontp = 1
        ivarip = 1
    endif
    neq = nno*nc
!
    call dicrgr('RIGI', option, neq, nc, zi(imat),&
                ulm, dul, zr(icontm), zr(ivarim), pgl,&
                klv, zr(ivarip), zr(ifono), zr(icontp))
!
    if (option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
        call jevech('PMATUUR', 'E', imat)
        call utpslg(nno, nc, pgl, klv, zr(imat))
    endif
end subroutine
