subroutine te0442(option, nomte)
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
!
    implicit none
#include "jeveux.h"
!
#include "asterc/r8dgrd.h"
#include "asterfort/assert.h"
#include "asterfort/coqrep.h"
#include "asterfort/dxefro.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxsiro.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
#include "asterfort/u2mesi.h"
    character(len=16) :: option, nomte
!......................................................................
!
!    - FONCTION REALISEE: CHANGEMENT DE REPERE POUR LES PLAQUES
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                        'REPE_TENS'  :  TENSEURS
!                        'REPE_GENE'  :  QUANTITES GENERALISEES
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfdx, jgano, iret(4)
    integer :: jgeom, jin, jout, jang, np, itab(7), iret1, iret2, nbsp
    integer :: nptmax, ncpmax, nspmax
    integer :: jcara, ncmp, vali(2)
    parameter    (nptmax=9,ncpmax=8,nspmax=162)
    real(kind=8) :: pgl(3, 3), alpha, beta, c, s
    real(kind=8) :: t2ev1(2, 2), t2ve1(2, 2)
    real(kind=8) :: t2ev2(2, 2), t2ve2(2, 2)
    real(kind=8) :: conin(nptmax*ncpmax*nspmax)
    real(kind=8) :: rep
    character(len=4) :: fami
    character(len=8) :: pain, paout
!
    if (option .ne. 'REPE_TENS' .and. option .ne. 'REPE_GENE') then
!C OPTION DE CALCUL INVALIDE
        ASSERT(.false.)
    endif
!
    if (option .eq. 'REPE_TENS') then
        ncmp=6
        call tecach('ONO', 'PCOGAIN', 'L', 7, itab,&
                    iret(1))
        call tecach('ONO', 'PCONOIN', 'L', 7, itab,&
                    iret(2))
        call tecach('ONO', 'PDEGAIN', 'L', 7, itab,&
                    iret(3))
        call tecach('ONO', 'PDENOIN', 'L', 7, itab,&
                    iret(4))
        iret1 = iret(1) + iret(2) + iret(3) + iret(4)
        ASSERT(iret1.eq.6)
!
        if (iret(1) .eq. 0) then
            pain = 'PCOGAIN'
            paout = 'PCOGAOUT'
        else if (iret(2).eq.0) then
            pain = 'PCONOIN'
            paout = 'PCONOOUT'
        else if (iret(3).eq.0) then
            pain = 'PDEGAIN'
            paout = 'PDEGAOUT'
        else if (iret(4).eq.0) then
            pain = 'PDENOIN'
            paout = 'PDENOOUT'
        endif
!
    else if (option.eq.'REPE_GENE') then
        ncmp=8
        call tecach('ONO', 'PEFGAIN', 'L', 7, itab,&
                    iret(1))
        call tecach('ONO', 'PEFNOIN', 'L', 7, itab,&
                    iret(2))
        call tecach('ONO', 'PDGGAIN', 'L', 7, itab,&
                    iret(3))
        call tecach('ONO', 'PDGNOIN', 'L', 7, itab,&
                    iret(4))
        iret1 = iret(1) + iret(2) + iret(3) + iret(4)
        ASSERT(iret1.eq.6)
!
        if (iret(1) .eq. 0) then
            pain = 'PEFGAIN'
            paout = 'PEFGAOUT'
        else if (iret(2).eq.0) then
            pain = 'PEFNOIN'
            paout = 'PEFNOOUT'
        else if (iret(3).eq.0) then
            pain = 'PDGGAIN'
            paout = 'PDGGAOUT'
        else if (iret(4).eq.0) then
            pain = 'PDGNOIN'
            paout = 'PDGNOOUT'
        endif
    endif
!
    if (pain(4:5) .eq. 'NO') then
        fami = 'NOEU'
    else if (pain(4:5).eq.'GA') then
        fami = 'RIGI'
    endif
    call elref4(' ', fami, ndim, nno, nnos,&
                npg, ipoids, ivf, idfdx, jgano)
    if (pain(4:5) .eq. 'NO') then
        np = nno
    else if (pain(4:5).eq.'GA') then
        np = npg
    endif
!
    call jevech('PGEOMER', 'L', jgeom)
    call jevech('PCACOQU', 'L', jcara)
    call jevech('PANGREP', 'L', jang)
    call jevech(pain, 'L', jin)
    call jevech(paout, 'E', jout)
    call tecach('OOO', pain, 'L', 7, itab,&
                iret2)
    nbsp = itab(7)
    if ((nbsp.ne.1) .and. (mod(nbsp,3).ne.0)) then
        call u2mesi('F', 'ELEMENTS5_54', 1, nbsp)
    endif
!
    if (nno .eq. 3) then
        call dxtpgl(zr(jgeom), pgl)
    else if (nno.eq.4) then
        call dxqpgl(zr(jgeom), pgl, 'S', iret1)
    endif
    ASSERT(ncmp.le.ncpmax)
    ASSERT(np.le.nptmax)
    vali(1)=nspmax
    vali(2)=nbsp
    if (nbsp .gt. nspmax) call u2mesi('F', 'ELEMENTS5_4', 2, vali)
!
!     LE TABLEAU CONIN A ETE ALLOUE DE FACON STATIQUE POUR
!     OPTIMISER LE CPU CAR LES APPELS A WKVECT DANS LES TE SONT COUTEUX.
!
    alpha = zr(jcara+1) * r8dgrd()
    beta = zr(jcara+2) * r8dgrd()
    call coqrep(pgl, alpha, beta, t2ev1, t2ve1,&
                c, s)
!
    rep = zr(jang+2)
    if (rep .eq. 0.d0) then
!
! --- PASSAGE DES CONTRAINTES DU REPERE LOCAL 1
! --- A L'ELEMENT AU REPERE INTRINSEQUE DE LA COQUE
!     ---------------------------------------
        if (option .eq. 'REPE_TENS') then
            call dxsiro(np*nbsp, t2ev1, zr(jin), conin)
        else if (option.eq.'REPE_GENE') then
            call dxefro(np, t2ev1, zr(jin), conin)
        endif
!
! --- CALCUL DES MATRICES DE PASSAGE DU CHGT DE REPERE
        alpha = zr(jang) * r8dgrd()
        beta = zr(jang+1) * r8dgrd()
        call coqrep(pgl, alpha, beta, t2ev2, t2ve2,&
                    c, s)
!
! ---   PASSAGE DES QUANTITES DU REPERE INTRINSEQUE
! ---   A L'ELEMENT AU REPERE LOCAL 2 DE LA COQUE
        if (option .eq. 'REPE_TENS') then
            call dxsiro(np*nbsp, t2ve2, conin, zr(jout))
        else if (option.eq.'REPE_GENE') then
            call dxefro(np, t2ve2, conin, zr(jout))
        endif
!
! --- PASSAGE DES CONTRAINTES DU REPERE INTRINSEQUE
! --- A L'ELEMENT AU REPERE LOCAL 1 DE LA COQUE
!     REPERE = 'COQUE_INTR_UTIL'
!     ---------------------------------------
    else if (rep.eq.1.d0) then
        if (option .eq. 'REPE_TENS') then
            call dxsiro(np*nbsp, t2ve1, zr(jin), zr(jout))
        else if (option.eq.'REPE_GENE') then
            call dxefro(np, t2ve1, zr(jin), zr(jout))
        endif
!
! --- PASSAGE DES CONTRAINTES DU REPERE LOCAL 1
! --- A L'ELEMENT AU REPERE INTRINSEQUE DE LA COQUE
!     REPERE = 'COQUE_UTIL_INTR'
!     ---------------------------------------
    else if (rep.eq.2.d0) then
        if (option .eq. 'REPE_TENS') then
            call dxsiro(np*nbsp, t2ev1, zr(jin), zr(jout))
        else if (option.eq.'REPE_GENE') then
            call dxefro(np, t2ev1, zr(jin), zr(jout))
        endif
!
    endif
end subroutine
