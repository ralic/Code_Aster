subroutine prmadj(nbnd, neq, n2, adjncy, xadj,&
                  xadjd, liste, q, noeud)
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: olivier.boiteau at edf.fr
    implicit none
    include 'asterfort/prmade.h'
    include 'asterfort/prmadl.h'
    integer :: nbnd, neq, n2
    integer :: adjncy(*), xadj(neq+1), xadjd(*), liste(neq), q(n2)
    integer :: noeud(*), nbnoeu, ndi, ndj, deb, fin, deblis, i, j, ndsuiv
    logical :: vider
    vider = .false.
    nbnoeu = 0
    deblis=0
    do 50 i = 1, nbnd
        xadjd(i)=1
50  continue
    do 100 i = 1, n2
        ndi = noeud(q(i))
        deb = xadj(i)
        fin = xadj(i+1)-1
        do 120 j = deb, fin
            ndj = noeud(q(adjncy(j)))
            if (ndi .ne. ndj) then
!     ON MET  NDJ DANS  LA LISTE
                call prmadl(ndj, deblis, liste)
            endif
120      continue
!     ON ECRIT LA LISTE DANS ADJNCY,XADJD ET ON LA REMET A ZERO
        if (i .eq. n2) then
            vider = .true.
        else
            ndsuiv = noeud(q(i+1))
            if (ndsuiv .ne. ndi) then
                nbnoeu = nbnoeu + 1
                vider = .true.
            endif
        endif
        if (vider) then
            call prmade(deblis, liste, adjncy, xadjd, ndi)
            vider = .false.
        endif
100  continue
end subroutine
