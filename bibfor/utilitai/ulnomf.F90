function ulnomf(nomfic, kacc, typef)
    implicit none
    integer :: ulnomf
    character(len=*) :: nomfic, kacc, typef
!     ------------------------------------------------------------------
!     RETOURNE LE NUMERO D'UNITE LOGIQUE ASSOCIE AU NOM SYSTEME
!              -1 SI AUCUN DE DISPONIBLE
!     RENVOIE LE TYPE D'ACCES AU FICHIER ASSOCIE DANS L'ARGUMENT KACC
!     RENVOIE LE TYPE DE FICHIER ASSOCIE DANS L'ARGUMENT TYPEF
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: mxf
    parameter       (mxf=100)
    character(len=1) :: typefi(mxf), accefi(mxf), etatfi(mxf), modifi(mxf)
    character(len=16) :: ddname(mxf)
    character(len=255) :: namefi(mxf)
    integer :: first, unitfi(mxf), nbfile
    common/ asgfi1 / first, unitfi      , nbfile
    common/ asgfi2 / namefi,ddname,typefi,accefi,etatfi,modifi
!
    integer :: ival, k
!
    ival = -1
    kacc = '?'
    typef = '?'
    do 1 k = 1, mxf-1
        if (typefi(k) .ne. '?') then
            if (namefi(k) .eq. nomfic) then
                ival = unitfi(k)
                typef = typefi(k)
                kacc = accefi(k)
                goto 2
            endif
        endif
 1  end do
 2  continue
    ulnomf = ival
end function
