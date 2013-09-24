!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
                      codret)
        character(*) :: option
        character(*) :: modele
        character(*) :: chdepl
        character(*) :: chgeom
        character(*) :: chmate
        character(*) :: chcara(*)
        character(*) :: chtemp
        character(*) :: chtref
        character(*) :: chtime
        character(*) :: chnumc
        character(*) :: chharm
        character(*) :: chsig
        character(*) :: cheps
        character(*) :: chfreq
        character(*) :: chmass
        character(*) :: chmeta
        character(*) :: charge
        character(*) :: typcoe
        real(kind=8) :: alpha
        complex(kind=8) :: calpha
        character(*) :: chdynr
        character(*) :: suropt
        character(*) :: chelem
        character(*) :: chelex
        character(*) :: ligrel
        character(*) :: base
        character(*) :: ch1
        character(*) :: ch2
        character(*) :: chvari
        character(*) :: compor
        character(*) :: chtese
        character(*) :: chdese
        character(*) :: nopase
        integer :: typese
        character(*) :: chacse
        integer :: codret
    end subroutine mecalc
end interface
