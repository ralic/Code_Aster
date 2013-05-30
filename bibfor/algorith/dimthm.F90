subroutine dimthm(nomte, ndlno, ndlnm, ndim)
    implicit none
    include 'asterfort/lxlgut.h'
    include 'asterfort/u2mess.h'
    integer :: ndlno, ndlnm, ndim
    integer :: iaux
    character(len=16) :: nomte
    logical :: elsufm
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! --- BUT: CALCUL DU NOMBRE DE DDL SUR CHAQUE TYPE DE NOEUDS POUR LES --
! --- ELEMENTS DE BORD EN THM ------------------------------------------
! ======================================================================
!     ELSUFM VRAI SI ELEMENT SUSHI OU EFMH
!     NDDLNO NOMBRE DE DDL DES NOEUDS EXTREMITE DE SEGMENTS
!     NDDLM  NOMBRE DE DDL DES NOEUDS MILIEU DE SEGMENTS OU FACE
    iaux = lxlgut(nomte)
    elsufm= (( nomte(iaux-4:iaux).eq.'_SUDM').or.&
     &        ( nomte(iaux-4:iaux).eq.'_SUDA' ).or.&
     &        ( nomte(iaux-3:iaux).eq.'_SUC' ))
!
    if (elsufm) then
! ======================================================================
! --- SI MODELISATION = SUSHI   ----------------------------------------
! ======================================================================
        ndlno = 0
        ndlnm = 2
    else if (nomte(1:5).eq.'THHM_') then
! ======================================================================
! --- SI MODELISATION = THHM -------------------------------------------
! ======================================================================
        ndlno = ndim+3
        ndlnm = ndim
    else if (nomte(1:5).eq.'THH2M') then
! ======================================================================
! --- SI MODELISATION = THH2M ------------------------------------------
! ======================================================================
        ndlno = ndim +3
        ndlnm = ndim
    else if (nomte(1:2).eq.'HM') then
! ======================================================================
! --- SI MODELISATION = HM ---------------------------------------------
! ======================================================================
        ndlno = ndim+1
        ndlnm = ndim
    else if (nomte(1:3).eq.'HHM') then
! ======================================================================
! --- SI MODELISATION = HHM --------------------------------------------
! ======================================================================
        ndlno = ndim+2
        ndlnm = ndim
    else if (nomte(1:4).eq.'HH2M') then
! ======================================================================
! --- SI MODELISATION = HH2M -------------------------------------------
! ======================================================================
        ndlno = ndim+2
        ndlnm = ndim
    else if (nomte(1:4).eq.'THH_') then
! ======================================================================
! --- SI MODELISATION = THH --------------------------------------------
! ======================================================================
        ndlno = 3
        ndlnm = 0
    else if (nomte(1:5).eq.'THH2_') then
! ======================================================================
! --- SI MODELISATION = THH2 -------------------------------------------
! ======================================================================
        ndlno = 3
        ndlnm = 0
    else if (nomte(1:3).eq.'HH_') then
! ======================================================================
! --- SI MODELISATION = HH_ -------------------------------------------
! ======================================================================
        ndlno = 2
        ndlnm = 0
    else if (nomte(1:2).eq.'H_') then
! ======================================================================
! --- SI MODELISATION = H_ --------------------------------------------
! ======================================================================
        ndlno = 1
        ndlnm = 0
    else if (nomte(1:4).eq.'HH2_') then
! ======================================================================
! --- SI MODELISATION = HH2 -------------------------------------------
! ======================================================================
        ndlno = 2
        ndlnm = 0
    else if (nomte(1:4).eq.'THV_') then
! ======================================================================
! --- SI MODELISATION = THV --------------------------------------------
! ======================================================================
        ndlno = 2
        ndlnm = 0
    else if (nomte(1:4).eq.'THM_') then
! ======================================================================
! --- SI MODELISATION = THM --------------------------------------------
! ======================================================================
        ndlno = ndim+2
        ndlnm = ndim
    else
        call u2mess('F', 'ALGORITH3_8')
    endif
! =====================================================================
end subroutine
