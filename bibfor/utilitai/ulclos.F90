subroutine ulclos()
    implicit   none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: j-pierre.lefebvre at edf.fr
!     ------------------------------------------------------------------
!     FERMETURE DE TOUS LES FICHIERS OUVERTS REFERENCES PAR ULDEFI
!     SAUF UNITES 6 ET 8 POUR POUVOIR ENCORE LES UTILISER EN PYTHON
!     ELLES SERONT FERMEES PAR APPEL PYTHON A ULOPEN(-6/-8)
!
!     ------------------------------------------------------------------
    integer :: mxf
    parameter       (mxf=100)
    character(len=1) :: typefi(mxf), accefi(mxf), etatfi(mxf), modifi(mxf)
    character(len=16) :: ddname(mxf)
    character(len=255) :: namefi(mxf)
    integer :: first, unitfi(mxf), nbfile
    common/ asgfi1 / first, unitfi      , nbfile
    common/ asgfi2 / namefi,ddname,typefi,accefi,etatfi,modifi
!     ------------------------------------------------------------------
    integer :: i, unit
!
    do 10 i = 1, nbfile
        unit = unitfi(i)
        if ((unit.gt.0) .and. (unit.ne.6) .and. (unit.ne.8)) then
            if (etatfi(i) .eq. 'O') then
                close ( unit=unit )
            endif
            namefi(i) = ' '
            ddname(i) = ' '
            unitfi(i) = -1
            typefi(i) = '?'
            accefi(i) = '?'
            etatfi(i) = 'F'
            modifi(i) = ' '
        endif
10  end do
!
end subroutine
