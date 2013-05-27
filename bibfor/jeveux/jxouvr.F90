subroutine jxouvr(iclas, idn)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE CRP_6
    implicit none
    include 'asterc/opendr.h'
    include 'asterfort/codent.h'
    include 'asterfort/u2mesg.h'
    integer :: iclas, idn
!     ==================================================================
    character(len=2) :: dn2
    character(len=5) :: classe
    integer :: n
!-----------------------------------------------------------------------
    integer :: ierr
    real(kind=8) :: r8bid
!-----------------------------------------------------------------------
    parameter      ( n = 5 )
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
    character(len=8) :: nombas
    common /kbasje/  nombas(n)
    character(len=128) :: repglo, repvol
    common /banvje/  repglo,repvol
    integer :: lrepgl, lrepvo
    common /balvje/  lrepgl,lrepvo
!     ------------------------------------------------------------------
    character(len=8) :: nom
    character(len=128) :: nom128
    integer :: indx(1), nbl
! DEB ------------------------------------------------------------------
    nbl = 1
    if (kstini(iclas) .ne. 'DUMMY   ') then
        ierr = 0
        nom = nomfic(iclas)(1:4)//'.   '
        call codent(idn, 'G', nom(6:7))
        if (nom(1:4) .eq. 'glob') then
            nom128=repglo(1:lrepgl)//'/'//nom
        else if (nom(1:4) .eq. 'vola') then
            nom128=repvol(1:lrepvo)//'/'//nom
        else
            nom128='./'//nom
        endif
        call opendr(nom128, indx, nbl, 0, ierr)
        if (ierr .ne. 0) then
            call u2mesg('F', 'JEVEUX_43', 1, nombas(iclas), 1,&
                        ierr, 0, r8bid)
        endif
    endif
end subroutine
