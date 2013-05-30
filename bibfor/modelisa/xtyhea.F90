subroutine xtyhea(nfiss, ifiss, ima, nno, jconx1,&
                  jconx2, jstnl, jstnv, nbheav)
! TOLE CRS_1404
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
! person_in_charge: patrick.massin at edf.fr
    implicit none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    integer :: nfiss, ifiss, ima, nno, nbheav
    integer :: jconx1, jconx2, jstnl(nfiss), jstnv(nfiss)
! ----------------------------------------------------------------------
!
! --- ROUTINE XFEM
!
! --- TYPE D'ELEMENT D'INTERSECTION XFEM
!
! ----------------------------------------------------------------------
!
! OUT NBHEAV   : NOMBRE D'ENRICHISSEMENTS HEAVISIDES DANS L'ÉLÉMENT
!
!
!
!
    integer :: ino, stno(nno), ifis
    integer :: nngl
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATION
    nbheav = 0
    do 10 ino = 1, nno
        stno(ino) = 0
10  end do
!
! --- BOUCLE SUR LES NOEUDS DE LA MAILLE
!
    do 20 ino = 1, nno
        nngl=zi(jconx1-1+zi(jconx2+ima-1)+ino-1)
!
! --- BOUCLE SUR LES FISSURES PRECEDENTES
!
        do 30 ifis = 1, ifiss
! --- RECUPERATION DES STATUTS DES NOEUDS
            if (zl(jstnl(ifis)-1+nngl) .and. zi(jstnv(ifis)-1+nngl) .gt. 0) then
                stno(ino) = stno(ino)+1
            endif
            nbheav = max(nbheav,stno(ino))
30      continue
20  end do
    call jedema()
end subroutine
