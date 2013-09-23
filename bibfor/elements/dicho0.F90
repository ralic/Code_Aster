subroutine dicho0(option, nomte, ndim, nbt, nno,&
                  nc, ulm, dul, pgl, iret)
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/dichoc.h"
#include "asterfort/infdis.h"
#include "asterfort/jevech.h"
#include "asterfort/pmavec.h"
#include "asterfort/tecach.h"
#include "asterfort/ut2mgl.h"
#include "asterfort/ut2mlg.h"
#include "asterfort/ut2vgl.h"
#include "asterfort/ut2vlg.h"
#include "asterfort/utpsgl.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvgl.h"
#include "asterfort/utpvlg.h"
#include "asterfort/vecma.h"
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
    integer :: jdc, irep, imat, ivarim, ii, jinst, ivitp, idepen, iviten, neq, igeom, ivarip
    integer :: iretlc, ifono
    integer :: icontm, icontp
    real(kind=8) :: r8bid, klv(78), varmo(8), varpl(8), dvl(12), dpe(12), dve(12), ulp(12), duly
    real(kind=8) :: force(3)
    real(kind=8) :: klc(144), fl(12)
    character(len=8) :: k8bid
!
!   paramètres en entrée
    call jevech('PCADISK', 'L', jdc)
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCONTMR', 'L', icontm)
!
    call infdis('REPK', irep, r8bid, k8bid)
!   absolu vers local ? ---
!   irep = 1 = matrice en repère global ==> passer en local ---
    if (irep .eq. 1) then
        if (ndim .eq. 3) then
            call utpsgl(nno, nc, pgl, zr(jdc), klv)
        else if (ndim.eq.2) then
            call ut2mgl(nno, nc, pgl, zr(jdc), klv)
        endif
    else
        call dcopy(nbt, zr(jdc), 1, klv, 1)
    endif
    call jevech('PMATERC', 'L', imat)
    call jevech('PVARIMR', 'L', ivarim)
!
    do ii = 1, 8
        varmo(ii) = zr(ivarim+ii-1)
    enddo
!
    call jevech('PINSTPR', 'L', jinst)
!
    call tecach('ONN', 'PVITPLU', 'L', iretlc, iad=ivitp)
    if (iretlc .eq. 0) then
        if (ndim .eq. 3) then
            call utpvgl(nno, nc, pgl, zr(ivitp), dvl)
        else if (ndim.eq.2) then
            call ut2vgl(nno, nc, pgl, zr(ivitp), dvl)
        endif
    else
        dvl(:) = 0.d0
    endif
!
    call tecach('ONN', 'PDEPENT', 'L', iretlc, iad=idepen)
    if (iretlc .eq. 0) then
        if (ndim .eq. 3) then
            call utpvgl(nno, nc, pgl, zr(idepen), dpe)
        else if (ndim.eq.2) then
            call ut2vgl(nno, nc, pgl, zr(idepen), dpe)
        endif
    else
        dpe(:) = 0.0d0
    endif
!
    call tecach('ONN', 'PVITENT', 'L', iretlc, iad=iviten)
    if (iretlc .eq. 0) then
        if (ndim .eq. 3) then
            call utpvgl(nno, nc, pgl, zr(iviten), dve)
        else if (ndim.eq.2) then
            call ut2vgl(nno, nc, pgl, zr(iviten), dve)
        endif
    else
        dve(:) = 0.d0
    endif
!
    neq = nno*nc
    ulp(:) = ulm(:) + dul(:)
!   relation de comportement de choc
    call dichoc(nbt, neq, nno, nc, zi(imat),&
                dul, ulp, zr(igeom), pgl, klv,&
                duly, dvl, dpe, dve, force,&
                varmo, varpl, ndim)
!   actualisation de la matrice tangente
    if (option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
        call jevech('PMATUUR', 'E', imat)
        if (ndim .eq. 3) then
            call utpslg(nno, nc, pgl, klv, zr(imat))
        else if (ndim.eq.2) then
            call ut2mlg(nno, nc, pgl, klv, zr(imat))
        endif
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
        else if (nno.eq.2) then
            do ii = 1, nc
                zr(icontp-1+ii) = -fl(ii) + zr(icontm-1+ii)
                zr(icontp-1+ii+nc) = fl(ii+nc) + zr(icontm-1+ii+nc)
                fl(ii) = fl(ii) - zr(icontm-1+ii)
                fl(ii+nc) = fl(ii+nc) + zr(icontm-1+ii+nc)
            enddo
        endif
        if (nno .eq. 1) then
            zr(icontp-1+1) = force(1)
            zr(icontp-1+2) = force(2)
            fl(1) = force(1)
            fl(2) = force(2)
            if (ndim .eq. 3) then
                fl(3) = force(3)
                zr(icontp-1+3) = force(3)
            endif
        else if (nno.eq.2) then
            zr(icontp-1+1) = force(1)
            zr(icontp-1+1+nc) = force(1)
            zr(icontp-1+2) = force(2)
            zr(icontp-1+2+nc) = force(2)
            fl(1) = -force(1)
            fl(1+nc) = force(1)
            fl(2) = -force(2)
            fl(2+nc) = force(2)
            if (ndim .eq. 3) then
                zr(icontp-1+3) = force(3)
                zr(icontp-1+3+nc) = force(3)
                fl(3) = -force(3)
                fl(3+nc) = force(3)
            endif
        endif
        if (abs(force(1)) .lt. r8prem()) then
            do ii = 1, neq
                fl(ii) = 0.0d0
                zr(icontp-1+ii) = 0.0d0
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
        do ii = 1, 8
            zr(ivarip+ii-1) = varpl(ii)
            if (nno .eq. 2) zr(ivarip+ii+7) = varpl(ii)
        enddo
    endif
end subroutine
