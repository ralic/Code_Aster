subroutine xelrex(elrefp, nno, xref, ndime)
    implicit none
#include "asterf_types.h"
#include "asterfort/elraca.h"
    character(len=8) :: elrefp
    integer :: nno
    integer, optional :: ndime
    real(kind=8) :: xref(81)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!   BUT: INTERFACE VERS ELRACA : 
!         RETOURNE LES COORDONNEES DE REFERENCE DE 
!             L ELEMENT PARENT COMPLET
    integer :: nbfamx
    parameter    ( nbfamx=20)
    integer :: nnos, nbfpg, nbpg(nbfamx), ndim
    real(kind=8) :: vol
    character(len=8) :: fapg(nbfamx), elp
    aster_logical :: transfert
!=======================================================================
!
    transfert=.false.
    if ((elrefp .eq. 'H20')) then
        elp='H27'
        transfert=.true.
    else if ((elrefp .eq. 'P15')) then
        elp='P18'
        transfert=.true.
    else if ((elrefp .eq. 'QU8')) then
        elp='QU9'
        transfert=.true.       
    else
        elp=elrefp
    endif
    call elraca(elp, ndim, nno, nnos, nbfpg,&
                fapg, nbpg, xref, vol)
!   LE TRANSFERT VERS L ELMENT COMPLET EST AMBIGU
!     ON STOCKE LES COORDONNES DE REFERENCE DE L ELEMENT COMPLET
!     ON INTERPOLE SUR LE L ELEMENT PARENT => NNO (L ELMENT INCOMPLET)
    if (transfert) then
        if ((elrefp .eq. 'H20')) then
            nno=20
        else if ((elrefp .eq. 'P15')) then
            nno=15
        else if ((elrefp .eq. 'QU8')) then
            nno=8   
        endif
    endif
!
!   Cas particulier de la pyramide quadratique : le noeud au centre de
!   la base doit être ajouté à la main, car la pyramide quadratique à
!   14 noeuds n'existe pas en tant qu'élémént de référence dans Aster
    if (elrefp.eq.'P13') then
       xref(ndim*(14-1)+1)=0.
       xref(ndim*(14-1)+2)=0.
       xref(ndim*(14-1)+3)=0.
    endif
!
    if (present(ndime)) ndime=ndim
!
end
