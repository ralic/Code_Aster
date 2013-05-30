subroutine dimvoi(nvtot, nvoima, nscoma, touvoi, dimvlo)
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    MAILLE M0 DE CONNECTIVITE ADCOM0
!    CALCULE LA TAILLE DES DONNEES DU VOISINAGE LOCAL D UN ELEMENT
!    A PARTIR DE TOUVOI
!    IN : DIM,M0,ADCOM0,IATYMA,
!         NVTOT,NVOIMA,NSCOMA,TOUVOI
!    OUT : DIMVLO
!
    implicit none
    integer :: nvtot, nvoima, nscoma
    integer :: touvoi(1:nvoima, 1:nscoma+2)
    integer :: dimvlo
    integer :: iv, nsco
!
    dimvlo=1+5*nvtot
    if (nvtot .ge. 1) then
        do 10 iv = 1, nvtot
            nsco=touvoi(iv,2)
            dimvlo=dimvlo+2*nsco
10      continue
    endif
!
end subroutine
