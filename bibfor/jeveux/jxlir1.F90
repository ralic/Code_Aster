subroutine jxlir1(ic, caralu)
! person_in_charge: j-pierre.lefebvre at edf.fr
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
    include 'asterc/closdr.h'
    include 'asterc/opendr.h'
    include 'asterc/readdr.h'
    include 'asterfort/codent.h'
    include 'asterfort/u2mesk.h'
    integer :: ic, caralu(*)
! ----------------------------------------------------------------------
! RELECTURE DU PREMIER ENREGISTREMENT D UNE BASE JEVEUX
!
! IN  IC    : CLASSE ASSOCIEE
! OUT CARALU: CARACTERISTIQUES DE LA BASE
! ----------------------------------------------------------------------
    integer :: n
!-----------------------------------------------------------------------
    integer :: ierr, k, np2
!-----------------------------------------------------------------------
    parameter      ( n = 5 )
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
    character(len=8) :: nombas
    common /kbasje/  nombas(n)
    character(len=128) :: repglo, repvol
    common /banvje/  repglo,repvol
    integer :: lrepgl, lrepvo
    common /balvje/  lrepgl,lrepvo
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
!     ------------------------------------------------------------------
    integer :: lindex, npar
    parameter      ( lindex = 11, npar = 11, np2 = npar+3 )
    integer :: index(lindex), tampon(np2)
    logical :: lexist
    character(len=8) :: nom
    character(len=128) :: nom128
! DEB ------------------------------------------------------------------
    ierr = 0
    nom = nomfic(ic)(1:4)//'.   '
    call codent(1, 'G', nom(6:7))
    if (nom(1:4) .eq. 'glob') then
        nom128=repglo(1:lrepgl)//'/'//nom
    else if (nom(1:4) .eq. 'vola') then
        nom128=repvol(1:lrepvo)//'/'//nom
    else
        nom128='./'//nom
    endif
    inquire (file=nom128,exist=lexist)
    if (.not. lexist) then
        call u2mesk('F', 'JEVEUX_12', 1, nombas(ic))
    endif
    call opendr(nom128, index, lindex, 0, ierr)
!
!   SUR CRAY L'APPEL A READDR EST EFFECTUE AVEC UNE LONGUEUR EN
!   ENTIER, A MODIFIER LORSQUE L'ON PASSERA AUX ROUTINES C
!
    call readdr(nom128, tampon, np2*lois, 1, ierr)
    if (ierr .ne. 0) then
        call u2mesk('F', 'JEVEUX_13', 1, nombas(ic))
    endif
    call closdr(nom128, ierr)
    if (ierr .ne. 0) then
        call u2mesk('F', 'JEVEUX_14', 1, nombas(ic))
    endif
    do 1 k = 1, npar
        caralu(k) = tampon(k+3)
 1  end do
! FIN ------------------------------------------------------------------
end subroutine
