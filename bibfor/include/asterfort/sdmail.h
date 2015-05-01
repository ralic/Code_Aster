!
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
    subroutine sdmail(nomu, nommai, nomnoe, cooval, coodsc,&
                      cooref, grpnoe, gpptnn, grpmai, gpptnm,&
                      connex, titre, typmai, adapma)
        character(len=8) :: nomu
        character(len=24) :: nommai
        character(len=24) :: nomnoe
        character(len=24) :: cooval
        character(len=24) :: coodsc
        character(len=24) :: cooref
        character(len=24) :: grpnoe
        character(len=24) :: gpptnn
        character(len=24) :: grpmai
        character(len=24) :: gpptnm
        character(len=24) :: connex
        character(len=24) :: titre
        character(len=24) :: typmai
        character(len=24) :: adapma
    end subroutine sdmail
end interface
