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
    subroutine nttcmv(modele, mate, carele, fomult, charge,&
                      infcha, infoch, numedd, solveu, time,&
                      chlapm, tpsthe, tpsnp1, reasvt, reasmt,&
                      creas, vtemp, vtempm, vec2nd, matass,&
                      maprec, cndirp, cnchci, cnchtp)
        character(len=24) :: modele
        character(len=24) :: mate
        character(len=24) :: carele
        character(len=24) :: fomult
        character(len=24) :: charge
        character(len=19) :: infcha
        character(len=24) :: infoch
        character(len=24) :: numedd
        character(len=19) :: solveu
        character(len=24) :: time
        character(len=24) :: chlapm
        real(kind=8) :: tpsthe(6)
        real(kind=8) :: tpsnp1
        logical :: reasvt
        logical :: reasmt
        character(len=1) :: creas
        character(len=24) :: vtemp
        character(len=24) :: vtempm
        character(len=24) :: vec2nd
        character(len=24) :: matass
        character(len=19) :: maprec
        character(len=24) :: cndirp
        character(len=24) :: cnchci
        character(len=24) :: cnchtp
    end subroutine nttcmv
end interface
