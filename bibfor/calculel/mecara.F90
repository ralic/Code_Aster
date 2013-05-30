subroutine mecara(cara, exicar, chcara)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ARGUMENTS:
!     ----------
    character(len=*) :: cara
    character(len=*) :: chcara(18)
    logical :: exicar
! ----------------------------------------------------------------------
!
!     ON CHERCHE LES NOMS DES CHAMPS DE CARAC_ELEM DANS 1 CARTE CARA
!
!
!     ENTREES:
!        CARA : NOM DE LA CARTE
!
!     SORTIES:
!        EXICAR : VRAI SI ON TROUVE 1 CHAMP DE CARAC_ELEM
!        CHCARA : NOMS DES CHAMPS DE CARAC_ELEM TROUVES.
!
! ----------------------------------------------------------------------
!
    integer :: ii
!
!   1   PCAORIE         CARORIEN
!   2   PCADISK         CARDISCK
!   3   PCADISM         CARDISCM
!   4   PCADISA         CARDISCA
!   5   PCAGEPO         CARGEOPO
!   6   PCAGNPO         CARGENPO
!   7   PCACOQU         CARCOQUE
!   8   PCASECT         CARSECTI
!   9   PCAARPO         CARARCPO
!  10   PCACABL         CARCABLE
!  11   PCAGNBA         CARGENBA
!  12   PCAMASS         CARMASSI
!  13   PCAPOUF         CARPOUFL
!  14   PVENTCX         CVENTCXF
!  15   PCINFDI         CARDINFO
!  16   PNBSP_I         CANBSP
!  17   PFIBRES         CAFIBR
!
    exicar = .false.
    if (cara(1:8) .ne. '        ') then
!        CHAINE DE 19 CARACTERES AU MAXIMUM
!                    12345678    90123456789
        chcara(1) = cara(1:8)//'.CARORIEN'
        chcara(2) = cara(1:8)//'.CARDISCK'
        chcara(3) = cara(1:8)//'.CARDISCM'
        chcara(4) = cara(1:8)//'.CARDISCA'
        chcara(5) = cara(1:8)//'.CARGEOPO'
        chcara(6) = cara(1:8)//'.CARGENPO'
        chcara(7) = cara(1:8)//'.CARCOQUE'
        chcara(8) = cara(1:8)//'.CARSECTI'
        chcara(9) = cara(1:8)//'.CARARCPO'
        chcara(10)= cara(1:8)//'.CARCABLE'
        chcara(11)= cara(1:8)//'.CARGENBA'
        chcara(12)= cara(1:8)//'.CARMASSI'
        chcara(13)= cara(1:8)//'.CARPOUFL'
        chcara(14)= cara(1:8)//'.CVENTCXF'
        chcara(15)= cara(1:8)//'.CARDINFO'
        chcara(16)= cara(1:8)//'.CANBSP'
        chcara(17)= cara(1:8)//'.CAFIBR'
        chcara(18)= ' '
        exicar = .true.
    else
        do 1 ii = 1, 18
            chcara(ii) = ' '
 1      continue
    endif
!
end subroutine
