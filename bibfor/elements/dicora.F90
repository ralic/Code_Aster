subroutine dicora(option, nomte, ndim, nbt, nno,&
                  nc, ulm, dul, pgl, iret)
    implicit none
#include "jeveux.h"
#include "asterfort/dicorn.h"
#include "asterfort/jevech.h"
#include "asterfort/pmavec.h"
#include "asterfort/ut2vlg.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvlg.h"
#include "asterfort/vecma.h"
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
    integer :: iiter, imat, ivarim, neq, iterat, ii, ifono, icontp, ivarip, icontm, irmetg
    real(kind=8) :: ulp(12), klv(78), klv2(78), varipc(7), klc(144), fl(12)
!
!   paramètres en entrée
    call jevech('PITERAT', 'L', iiter)
    call jevech('PMATERC', 'L', imat)
    call jevech('PVARIMR', 'L', ivarim)
    call jevech('PCONTMR', 'L', icontm)
!
    neq = nno*nc
    ulp(1:12) = ulm(1:12) + dul(1:12)
!   relation de comportement de la cornière
    irmetg = 0
    if (option .eq. 'RIGI_MECA_TANG') irmetg = 1
    iterat = nint(zr(iiter))
    call dicorn(irmetg, nbt, neq, iterat, zi(imat),&
                ulm, dul, ulp, zr(icontm), zr(ivarim),&
                klv, klv2, varipc)
!   actualisation de la matrice tangente
    if (option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
        call jevech('PMATUUR', 'E', imat)
        call utpslg(nno, nc, pgl, klv2, zr(imat))
    endif
!
!   calcul des efforts généralisés, des forces nodales et des variables internes
    if (option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
        call jevech('PVECTUR', 'E', ifono)
        call jevech('PCONTPR', 'E', icontp)
!       demi-matrice klv transformée en matrice pleine klc
        call vecma(klv, nbt, klc, neq)
!       calcul de fl = klc.dul (incrément d'effort)
        call pmavec('ZERO', neq, klc, dul, fl)
!       efforts généralisés aux noeuds 1 et 2 (repère local)
!       on change le signe des efforts sur le premier noeud pour les MECA_DIS_TR_L et MECA_DIS_T_L
        if (nno .eq. 1) then
            do ii = 1, neq
                zr(icontp-1+ii) = fl(ii) + zr(icontm-1+ii)
                fl(ii) = fl(ii) + zr(icontm-1+ii)
            enddo
        elseif (nno.eq.2) then
            do ii = 1, nc
                zr(icontp-1+ii) = -fl(ii) + zr(icontm-1+ii)
                zr(icontp-1+ii+nc) = fl(ii+nc) + zr(icontm-1+ii+nc)
                fl(ii) = fl(ii) - zr(icontm-1+ii)
                fl(ii+nc) = fl(ii+nc) + zr(icontm-1+ii+nc)
            enddo
        endif
!       forces nodales aux noeuds 1 et 2 (repère global)
        if (nc .ne. 2) then
            call utpvlg(nno, nc, pgl, fl, zr(ifono))
        else
            call ut2vlg(nno, nc, pgl, fl, zr(ifono))
        endif
!       mise a jour des variables internes
        call jevech('PVARIPR', 'E', ivarip)
        do ii = 1, 7
            zr(ivarip+ii-1) = varipc(ii)
            zr(ivarip+ii+6) = varipc(ii)
        enddo
    endif
end subroutine
