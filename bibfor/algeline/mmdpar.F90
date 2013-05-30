subroutine mmdpar(nd, nbsn, nbsn1, supnd, nouv,&
                  parent, prov, invsup)
! person_in_charge: olivier.boiteau at edf.fr
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
    implicit none
    integer :: nd, nbsn, nbsn1, supnd(nbsn1), nouv(nd)
    integer :: parent(nd), prov(nd), invsup(nd)
    integer :: i, j, snjp, snj
!     CALCUL DE PARENT EN FONCTION DES SUPERNOEUDS
!
!     SAUVEGARDE DE PARENT NODAL
    do 110 i = 1, nd
        prov(i) = parent(i)
110  end do
!     CALCUL DE INVSUP
    do 130 i = 1, nbsn
        do 120 j = supnd(i), supnd(i+1) - 1
            invsup(j) = i
120      continue
130  end do
    do 140 i = 1, nd
        parent(i) = 0
140  end do
    do 150 i = 1, nd
        if (prov(i) .ne. 0) then
            snjp = invsup(nouv(prov(i)))
            snj = invsup(nouv(i))
            parent(snj) = snjp
        endif
150  end do
end subroutine
