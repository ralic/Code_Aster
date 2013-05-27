subroutine ulisog(unit, fname, etat)
    implicit none
    include 'asterfort/u2mesk.h'
    include 'asterfort/ulinit.h'
    integer :: unit
    character(len=255) :: fname
    character(len=1) :: etat
!     ------------------------------------------------------------------
!     RETOURNE LE NOM ET L'ETAT D'UN FICHIER ASSOCIE A UNE UNITE LOGIQUE
!     (INVERSE DE ULNOMF)
!     ------------------------------------------------------------------
!
! IN  UNIT   : NUMERO D'UNITE LOGIQUE ASSOCIE A "FNAME"
! OUT FNAME  : NOM "LONG"  ASSOCIE AU NUMERO D'UNITE LOGIQUE FORTRAN
!     ETAT   : (O)UVERT, (F)ERME, (R)ESERVE
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
    integer :: mxf
    parameter       (mxf=100)
    character(len=1) :: typefi(mxf), accefi(mxf), etatfi(mxf), modifi(mxf)
    character(len=16) :: ddname(mxf)
    character(len=255) :: namefi(mxf)
    integer :: first, unitfi(mxf), nbfile
    common/ asgfi1 / first, unitfi      , nbfile
    common/ asgfi2 / namefi,ddname,typefi,accefi,etatfi,modifi
!
    character(len=8) :: k8bid
    integer :: i
!
    if (first .ne. 17111990) call ulinit()
!
    if (unit .lt. 0) then
        write(k8bid,'(I4)') -unit
        call u2mesk('F', 'UTILITAI5_9', 1, k8bid)
    endif
    fname = ' '
    etat = 'F'
    do 1 i = 1, nbfile
        if (unitfi(i) .eq. unit) then
            fname = namefi(i)
            etat = etatfi(i)
            goto 999
        endif
 1  end do
999  continue
end subroutine
