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
    subroutine xpoajc(nnm, inm, inmtot, nbmac, ise,&
                      npg, jcesd1, jcesd2, jcvid1, jcvid2,&
                      ima, ndim, ndime, iadc, iadv,&
                      jcesv1, jcesl2, jcesv2, jcviv1, jcvil2,&
                      jcviv2)
        integer :: nnm
        integer :: inm
        integer :: inmtot
        integer :: nbmac
        integer :: ise
        integer :: npg
        integer :: jcesd1
        integer :: jcesd2
        integer :: jcvid1
        integer :: jcvid2
        integer :: ima
        integer :: ndim
        integer :: ndime
        integer :: iadc
        integer :: iadv
        integer :: jcesv1
        integer :: jcesl2
        integer :: jcesv2
        integer :: jcviv1
        integer :: jcvil2
        integer :: jcviv2
    end subroutine xpoajc
end interface
