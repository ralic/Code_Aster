subroutine mepres(nomo, chpres, fonc)
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
    implicit none
!
    include 'jeveux.h'
    include 'asterfort/fozerv.h'
    include 'asterfort/mecact.h'
    character(len=8) :: nomo
    character(len=*) :: chpres
!
    logical :: fonc
!
!    CETTE ROUTINE GENERE UN CHAMP DE PRESSION NUL (CARTE CONSTANTE)
!     FONC = .TRUE.  FORCE FONCTION
!     FONC = .FALSE. FORCE REELLE
!
!
    real(kind=8) :: rcmp(2), rbid
!
    character(len=8) :: licmp(2), nomf(2), zero
    character(len=19) :: ligrmo
!
    complex(kind=8) :: cbid
!
!     -----------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: ibid
!-----------------------------------------------------------------------
    zero = '&&MEPRES'
    ligrmo = nomo//'.MODELE    '
    licmp(1) = 'PRES'
    licmp(2) = 'CISA'
    rcmp(1) = 0.d0
    rcmp(2) = 0.d0
    if (fonc) then
        call fozerv(zero)
        nomf(1) = zero
        nomf(2) = zero
        call mecact('V', chpres, 'MODELE', ligrmo, 'PRES_F',&
                    2, licmp, ibid, rbid, cbid,&
                    nomf)
    else
        call mecact('V', chpres, 'MODELE', ligrmo, 'PRES_R',&
                    2, licmp, ibid, rcmp, cbid,&
                    ' ')
    endif
!
end subroutine
