subroutine surfco(char, noma)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/cfdisi.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/surfc1.h'
    include 'asterfort/surfc2.h'
    include 'asterfort/surfc3.h'
    include 'asterfort/surfcl.h'
    include 'asterfort/surfcp.h'
    character(len=8) :: char
    character(len=8) :: noma
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - AFFICHAGE DONNEES)
!
! AFFICHAGE LES INFOS CONTENUES DANS LA SD CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMA   : NOM DU MAILLAGE
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    logical :: lmail
    character(len=24) :: defico
    integer :: iform
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
    if (niv .le. 1) then
        goto 999
    endif
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
!
! --- TYPE DE FORMULATION (DISCRETE/CONTINUE/XFEM)
!
    iform = cfdisi(defico,'FORMULATION')
    lmail = (iform.eq.1).or.(iform.eq.2)
!
    call surfcp(char, ifm)
!
    if (lmail) then
        call surfcl(char, noma, ifm)
    endif
!
    if (iform .eq. 1) then
        call surfc1(char, ifm)
    else if (iform.eq.2) then
        call surfc2(char, noma, ifm)
    else if (iform.eq.3) then
        call surfc3(char, noma, ifm)
    else
        call assert(.false.)
    endif
!
999  continue
!
    call jedema()
end subroutine
