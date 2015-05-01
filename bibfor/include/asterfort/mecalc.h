! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
interface
    subroutine mecalc(option, modele, chdepl, chgeom, chmate,&
                      chcara, chtemp, chtref, chtime, chnumc,&
                      chharm, chsig, cheps, chfreq, chmass,&
                      chmeta, charge, typcoe, alpha, calpha,&
                      chdynr, suropt, chelem, chelex, ligrel,&
                      base, ch1, ch2, chvari, compor,&
                      chtese, chdese, nopase, typese, chacse,&
                      chstrx, codret)
        character(len=*) :: option
        character(len=*) :: modele
        character(len=*) :: chdepl
        character(len=*) :: chgeom
        character(len=*) :: chmate
        character(len=*) :: chcara(*)
        character(len=*) :: chtemp
        character(len=*) :: chtref
        character(len=*) :: chtime
        character(len=*) :: chnumc
        character(len=*) :: chharm
        character(len=*) :: chsig
        character(len=*) :: cheps
        character(len=*) :: chfreq
        character(len=*) :: chmass
        character(len=*) :: chmeta
        character(len=*) :: charge
        character(len=*) :: typcoe
        real(kind=8) :: alpha
        complex(kind=8) :: calpha
        character(len=*) :: chdynr
        character(len=*) :: suropt
        character(len=*) :: chelem
        character(len=*) :: chelex
        character(len=*) :: ligrel
        character(len=*) :: base
        character(len=*) :: ch1
        character(len=*) :: ch2
        character(len=*) :: chvari
        character(len=*) :: compor
        character(len=*) :: chtese
        character(len=*) :: chdese
        character(len=*) :: nopase
        integer :: typese
        character(len=*) :: chacse
        character(len=*) :: chstrx
        integer :: codret
    end subroutine mecalc
end interface
