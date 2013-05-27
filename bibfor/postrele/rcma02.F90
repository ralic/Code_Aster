subroutine rcma02(etat, iocc, vale)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/jeveuo.h'
    integer :: iocc
    real(kind=8) :: vale(*)
    character(len=1) :: etat
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     ------------------------------------------------------------------
!     RECUPERATION DES CARACTERISTIQUES MATERIAU POUR UN ETAT
!
! IN  : ETAT   : ETAT STABILISE "A" OU "B"
! IN  : IOCC   : NUMERO D'OCCURRENCE DE LA SITUATION
! OUT : VALE   : CARACTERISTIQUES MATERIAU
!                VALE(1) = E
!                VALE(2) = NU
!                VALE(3) = ALPHA
!                VALE(4) = EC
!                VALE(5) = SM
!                VALE(6) = M
!                VALE(7) = N
!     ------------------------------------------------------------------
!
    integer :: nbcmp, i, jvale
    parameter   ( nbcmp = 8 )
! DEB ------------------------------------------------------------------
!
! --- LES CARACTERISTIQUES MATERIAU
!
    call jeveuo('&&RC3200.MATERIAU_'//etat, 'L', jvale)
!
    do 10 i = 1, nbcmp
        vale(i) = zr(jvale-1+nbcmp*(iocc-1)+i)
10  end do
!
end subroutine
