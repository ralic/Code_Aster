subroutine rcvals(iarret, icodre, nbres, nomres)
    implicit none
    include 'jeveux.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    integer :: iarret, nbres
    integer :: icodre(nbres)
    character(len=*) :: nomres(nbres)
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! ----------------------------------------------------------------------
!
!
!
!
    character(len=24) :: valk
    character(len=8) :: nomail, para
    integer :: ier, iadzi, iazk24, ires
! ----------------------------------------------------------------------
!
!
    if (iarret .ge. 1) then
        ier = 0
        do 200 ires = 1, nbres
            if (icodre(ires) .eq. 1) then
                ier = ier + 1
                para = nomres(ires)
                valk = para
                call u2mesg('E+', 'MODELISA9_77', 1, valk, 0,&
                            0, 0, 0.d0)
                if (iarret .eq. 1) then
                    call tecael(iadzi, iazk24)
                    nomail = zk24(iazk24-1+3)(1:8)
                    valk = nomail
                    call u2mesg('E+', 'MODELISA9_78', 1, valk, 0,&
                                0, 0, 0.d0)
                endif
                call u2mess('E', 'VIDE_1')
            endif
200      continue
        if (ier .ne. 0) then
            call u2mess('F', 'MODELISA6_4')
        endif
    endif
!
end subroutine
