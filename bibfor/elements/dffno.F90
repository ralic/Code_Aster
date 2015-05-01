subroutine dffno(elrefe, ndim, nno, nnos, dff)
    implicit none
#include "asterfort/elraca.h"
#include "asterfort/elrfdf.h"
    character(len=*) :: elrefe
    integer :: ndim, nno, nnos
    real(kind=8) :: dff(*)
! ----------------------------------------------------------------------
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
! BUT:   CALCUL DES DERIVEES DES FONCTIONS DE FORMES
!        AUX NOEUDS D'UN ELREFE
!
    integer :: nbnomx, nbfamx
    parameter    ( nbnomx=27, nbfamx=20)
    real(kind=8) :: x(nbnomx*3), vol, tab(3, nbnomx)
    integer :: dimd, nbfpg, nbpg(nbfamx), ino, ideri, ifonc, ibi1, ibi2
    character(len=8) :: fapg(nbfamx)
! ----------------------------------------------------------------------
!
    call elraca(elrefe, ndim, nno, nnos, nbfpg,&
                fapg, nbpg, x, vol)
!
    dimd = ndim*nno
!
    do 10 ino = 1, nno
!
        call elrfdf(elrefe, x(ndim*(ino-1)+1), dimd, tab, ibi1,&
                    ibi2)
!
        do 20 ideri = 1, ndim
            do 30 ifonc = 1, nno
                dff((ino-1)*nno*ndim+(ideri-1)*nno+ifonc)=tab(ideri,&
                ifonc)
30          continue
20      continue
!
10  end do
!
end subroutine
