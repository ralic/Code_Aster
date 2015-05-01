subroutine nbsomm(typema, nbso)
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
    implicit none
    character(len=8) :: typema
    integer :: nbso
!
!     DONNE LE NOMBRE DE SOMMETS POUR UN TYPE DE MAILLES
!
    if (typema(1:4) .eq. 'HEXA') then
        nbso=8
    else if (typema(1:4).eq.'PENT') then
        nbso=6
    else if (typema(1:4).eq.'TETR') then
        nbso=4
    else if (typema(1:4).eq.'QUAD') then
        nbso=4
    else if (typema(1:4).eq.'TRIA') then
        nbso=3
    else if (typema(1:3).eq.'SEG') then
        nbso=2
    else
! CONDITIONS AUX LIMITES PONCTUELLES (POUR MODELE ENDO_HETEROGENE)
        nbso=1
!
    endif
end subroutine
