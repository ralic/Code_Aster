function iunifi(name)
    implicit none
    integer :: iunifi
    include 'asterfort/ulinit.h'
    character(len=*) :: name
!
!     ------------------------------------------------------------------
!     RETOURNE L'UNITE LOGIQUE ATTACHEE AU NOM LOCAL NAME (DDNAME)
!     ------------------------------------------------------------------
!
! IN  NAME   : CH*16 : NOM "LOCALE" DONT ON RECHERCHE LE NUMERO D'UNITE
!                      LOGIQUE ASSOCIEE
! OUT IUNIFI : IS    : NUMERO D'UNITE LOGIQUE ASSOCIE A "NAME"
!                      RENVOI 0 SI LE NOM N'EST PAS DANS LES TABLES
!
!     ------------------------------------------------------------------
!     REMARQUE : SUPPOSE QUE LA DEFINITION DU COUPLE (UNITE LOGIQUE,NOM)
!                EST DEJA FAITE (CF ULDEFI)
!     REMARQUE : SI L'INITIALISATION N'A PAS ETE FAITE LA ROUTINE S'EN
!                CHARGERA (APPEL A ULINIT)
!     ------------------------------------------------------------------
! person_in_charge: j-pierre.lefebvre at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=16) :: name16
    integer :: i
!     ------------------------------------------------------------------
!     CONSERVER LA COHERENCE AVEC IBIMPR
    integer :: mximpr
    parameter   ( mximpr = 3)
    character(len=16) :: nompr (mximpr)
    integer :: unitpr (mximpr)
    data          nompr  /'MESSAGE'  , 'RESULTAT', 'ERREUR'/
    data          unitpr /    6      ,     8     ,      9  /
!     ------------------------------------------------------------------
!
    if (first .ne. 17111990) call ulinit()
!
    name16 = name
    iunifi = 0
!
!     VALEUR PAR DEFAUT POUR LES NOMS INTERNES
    do 10 i = 1, mximpr
        if (name16 .eq. nompr(i)) then
            iunifi = unitpr(i)
            goto 99
        endif
10  end do
!
    do 20 i = 1, nbfile
        if (name16 .eq. ddname(i)) then
            iunifi = unitfi(i)
            goto 99
        endif
20  end do
!
99  continue
end function
