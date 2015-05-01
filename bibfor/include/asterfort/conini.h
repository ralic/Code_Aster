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
    subroutine conini(ma, noecon, maicon, marcon, nbmar,&
                      nbnoe, nbmarc, nommar, jmicor, mbcor,&
                      nomtyr, nbgco, io8gco)
        integer :: nbnoe
        integer :: nbmar
        character(len=8) :: ma
        integer :: noecon(nbnoe)
        integer :: maicon(nbmar)
        integer :: marcon(nbmar)
        integer :: nbmarc
        character(len=8) :: nommar(nbmar)
        integer :: jmicor(nbmar)
        integer :: mbcor(nbmar)
        character(len=8) :: nomtyr(nbmar)
        integer :: nbgco
        integer :: io8gco
    end subroutine conini
end interface
