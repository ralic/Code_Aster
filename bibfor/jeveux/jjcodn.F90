function jjcodn(icre, nomrep, nomec, irep, crep,&
                nmax, nuti)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: j-pierre.lefebvre at edf.fr
    implicit none
#include "asterfort/jxhcod.h"
#include "asterfort/utmess.h"
    character(len=*) :: nomrep, nomec, crep(*)
    integer :: icre, irep(*), nmax, nuti
! ----------------------------------------------------------------------
    integer :: ilorep, ideno, ilnom, iluti, idehc
!-----------------------------------------------------------------------
    integer :: i, idehco, idenom, iin, in, j, jin
    integer :: jjcodn, k, ll, lnom, lorep, ne
!-----------------------------------------------------------------------
    parameter      ( ilorep=1,ideno=2,ilnom=3,iluti=5,idehc=6)
    integer :: iret
    character(len=32) :: cle, nom, valk(2)
    logical(kind=1) :: rinser
! DEB ------------------------------------------------------------------
    jjcodn = 0
    rinser = .false.
    iret = 0
    lorep = irep(ilorep)
    idenom = irep(ideno )
    lnom = irep(ilnom)
    idehco = irep(idehc )
    ll = min ( lnom , len(nomec) )
    nom = nomec(1:ll)
    i = jxhcod (nom,lorep)
    ne = 1
    valk(1) = nom
    valk(2) = nomrep
!
 5  continue
    if (irep(idehco+i) .eq. 0 .and. .not. rinser) then
        if (icre .eq. 3) then
            if (nuti .ge. nmax) then
                call utmess('F', 'JEVEUX1_33', sk=valk(2))
            else
                j = nuti + 1
                do 12 k = 1, ll
                    crep(idenom+lnom*(j-1)+k) = nomec(k:k)
12              continue
                nuti = nuti + 1
                irep(iluti) = nuti
                irep(idehco+i) = j
                iret = j
            endif
        else
            if (icre .eq. 0) then
                iret = 0
            else
                call utmess('F', 'JEVEUX1_34', nk=2, valk=valk)
            endif
        endif
    else
        j = irep(idehco+i)
        do 15 k = 1, ll
            cle(k:k) = crep(idenom+lnom*(abs(j)-1)+k)
15      continue
        do 16 k = ll+1, 32
            cle(k:k) = ' '
16      continue
        if (cle .eq. nom) then
            if (icre .eq. 3) then
                call utmess('F', 'JEVEUX1_35', nk=2, valk=valk)
            else if (icre .eq. 0) then
                iret = j
            else if (icre .eq. -3) then
                irep(idehco+i) = -j
                crep(idenom+lnom*(j-1)+1) = '?'
                iret = -j
            endif
        else
            if (j .lt. 0 .and. .not. rinser) then
                if (icre .eq. 3) then
                    rinser = .true.
                    jin = j
                    iin = i
                endif
            endif
            if (ne .eq. 1) in = jxhcod (nom,lorep-2)
            ne = ne + 1
            i = 1 + mod (i+in,lorep)
            if (ne .le. lorep) then
                j = irep ( idehco + i )
                if (j .eq. 0 .and. rinser) goto 10
                goto 5
            else
                if (icre .eq. 3) then
                    call utmess('F', 'JEVEUX1_36', nk=2, valk=valk)
                else if (icre .eq. 0) then
                    iret = 0
                endif
            endif
        endif
    endif
10  continue
    if (rinser) then
        irep(idehco+iin) = -jin
        do 25 k = 1, ll
            crep(idenom+lnom*(-jin-1)+k) = nomec(k:k)
25      continue
        iret = -jin
    endif
    jjcodn = iret
! FIN ------------------------------------------------------------------
end function
