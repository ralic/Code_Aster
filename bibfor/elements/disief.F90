subroutine disief(nbt, neq, nno, nc, pgl,&
                  klv, dul, sim, ilogic, duly,&
                  sip, fono, force, dimele)
    implicit none
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/pmavec.h"
#include "asterfort/ut2vlg.h"
#include "asterfort/utpvlg.h"
#include "asterfort/vecma.h"
    integer :: nbt, neq, ilogic, nno, nc, dimele
    real(kind=8) :: pgl(3, 3), klv(nbt), dul(neq), sim(neq), duly
    real(kind=8) :: sip(neq), fono(neq), force(3)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-luc.flejou at edf.fr
! --------------------------------------------------------------------------------------------------
!
!     CALCUL DES EFFORTS GÉNÉRALISÉS (REPÈRE LOCAL)
!     ET DES FORCES NODALES (REPÈRE GLOBAL). COMME ON TRAITE DES
!     ÉLÉMENTS DISCRETS, CES QUANTITÉS SONT ÉGALES, AU REPÈRE PRÈS.
!
! --------------------------------------------------------------------------------------------------
!
! IN
!       nbt    : nombre de valeurs pour la demi-matrice
!       neq    : nombre de ddl de l'élément
!       nno    : nombre de noeuds de l'élément (1 ou 2)
!       nc     : nombre de ddl par noeud
!       pgl    : matrice de passage repère global -> local
!       klv    : matrice de "raideur tangente"
!       dul    : incrément de déplacement local
!       sim    : efforts généralisés a l'instant précédent
!       ilogic :
!       duly   :
!
! OUT
!       sip    : efforts generalises actualises
!       fono   : forces nodales
!
! --------------------------------------------------------------------------------------------------
    integer :: n, i
    real(kind=8) :: klc(144), fl(12), zero
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT( (ilogic.eq.0) .or. (ilogic.eq.2) )
    zero = 0.d0
!   demi-matrice klv transformée en matrice pleine klc
    call vecma(klv, nbt, klc, neq)
!   calcul de fl = klc.dul (incrément d'effort)
    call pmavec('ZERO', neq, klc, dul, fl)
!   efforts généralisés aux noeuds 1 et 2 (repère local)
!   on change le signe des efforts sur le premier noeud pour les MECA_DIS_TR_L et MECA_DIS_T_L
    if (nno .eq. 1) then
        do i = 1, neq
            sip(i) = fl(i) + sim(i)
            fl(i)  = fl(i) + sim(i)
        enddo
    elseif (nno.eq.2) then
        do i = 1, nc
            sip(i)    = -fl(i)    + sim(i)
            sip(i+nc) =  fl(i+nc) + sim(i+nc)
            fl(i)     =  fl(i)    - sim(i)
            fl(i+nc)  =  fl(i+nc) + sim(i+nc)
        enddo
    endif
!
    if (ilogic .eq. 2) then
        if (nno .eq. 1) then
            sip(1)   = force(1)
            sip(2)   = force(2)
            fl(1)    = force(1)
            fl(2)    = force(2)
            if (dimele .eq. 3) then
                fl(3)  = force(3)
                sip(3) = force(3)
            endif
        elseif (nno.eq.2) then
            sip(1)      =  force(1)
            sip(2)      =  force(2)
            sip(1+nc)   =  force(1)
            sip(2+nc)   =  force(2)
            fl(1)       = -force(1)
            fl(2)       = -force(2)
            fl(1+nc)    =  force(1)
            fl(2+nc)    =  force(2)
            if (dimele .eq. 3) then
                sip(3)    =  force(3)
                sip(3+nc) =  force(3)
                fl(3)     = -force(3)
                fl(3+nc)  =  force(3)
            endif
        endif
        if (abs(force(1)) .lt. r8prem()) then
            do n = 1, neq
                fl(n)  = zero
                sip(n) = zero
            enddo
        endif
    endif
!
!   forces nodales aux noeuds 1 et 2 (repère global)
    if (nc .ne. 2) then
        call utpvlg(nno, nc, pgl, fl, fono)
    else
        call ut2vlg(nno, nc, pgl, fl, fono)
    endif
end subroutine
