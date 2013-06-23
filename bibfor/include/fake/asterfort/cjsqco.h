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
    subroutine cjsqco(gamma, sig, x, pref, epssig,&
                      i1, s, sii, siirel, cos3ts,&
                      hts, dets, q, qii, qiirel,&
                      cos3tq, htq, detq)
        real(kind=8) :: gamma
        real(kind=8) :: sig(6)
        real(kind=8) :: x(6)
        real(kind=8) :: pref
        real(kind=8) :: epssig
        real(kind=8) :: i1
        real(kind=8) :: s(6)
        real(kind=8) :: sii
        real(kind=8) :: siirel
        real(kind=8) :: cos3ts
        real(kind=8) :: hts
        real(kind=8) :: dets
        real(kind=8) :: q(6)
        real(kind=8) :: qii
        real(kind=8) :: qiirel
        real(kind=8) :: cos3tq
        real(kind=8) :: htq
        real(kind=8) :: detq
    end subroutine cjsqco
end interface
