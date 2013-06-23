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
    subroutine transi(np1, np2, np3, np4, nbm,&
                      nbnl, npfmax, npfts, dttr, ttrans,&
                      eps, fext, text, fextts, textts,&
                      fexttr, fextt0, masgi, amori, pulsi,&
                      phii, typch, nbseg, rc, alpha,&
                      beta, gamma, orig, theta, vitg,&
                      depg, amor, pulsd, omegaf, aa,&
                      bb, old, s0, z0, sr0,&
                      za1, za2, za3, za4, za5,&
                      zitr, zin, mtrans, amor00, puls00,&
                      accg0, vitg0, depg0, iconfb, tconf1,&
                      ftest0, ier)
        integer :: np4
        integer :: np3
        integer :: np2
        integer :: np1
        integer :: nbm
        integer :: nbnl
        integer :: npfmax
        integer :: npfts
        real(kind=8) :: dttr
        real(kind=8) :: ttrans
        real(kind=8) :: eps
        real(kind=8) :: fext(np4, *)
        real(kind=8) :: text(*)
        real(kind=8) :: fextts(np4, *)
        real(kind=8) :: textts(*)
        real(kind=8) :: fexttr(*)
        real(kind=8) :: fextt0(*)
        real(kind=8) :: masgi(*)
        real(kind=8) :: amori(*)
        real(kind=8) :: pulsi(*)
        real(kind=8) :: phii(np2, np1, *)
        integer :: typch(*)
        integer :: nbseg(*)
        real(kind=8) :: rc(np3, *)
        real(kind=8) :: alpha(2, *)
        real(kind=8) :: beta(2, *)
        real(kind=8) :: gamma(2, *)
        real(kind=8) :: orig(3, *)
        real(kind=8) :: theta(np3, *)
        real(kind=8) :: vitg(*)
        real(kind=8) :: depg(*)
        real(kind=8) :: amor(*)
        real(kind=8) :: pulsd(*)
        real(kind=8) :: omegaf(*)
        real(kind=8) :: aa(*)
        real(kind=8) :: bb(*)
        real(kind=8) :: old(9, *)
        complex(kind=8) :: s0(*)
        complex(kind=8) :: z0(*)
        complex(kind=8) :: sr0(*)
        complex(kind=8) :: za1(*)
        complex(kind=8) :: za2(*)
        complex(kind=8) :: za3(*)
        complex(kind=8) :: za4(np4, *)
        complex(kind=8) :: za5(np4, *)
        complex(kind=8) :: zitr(*)
        complex(kind=8) :: zin(*)
        real(kind=8) :: mtrans(2, 2, *)
        real(kind=8) :: amor00(*)
        real(kind=8) :: puls00(*)
        real(kind=8) :: accg0(*)
        real(kind=8) :: vitg0(*)
        real(kind=8) :: depg0(*)
        integer :: iconfb(*)
        real(kind=8) :: tconf1(4, *)
        real(kind=8) :: ftest0
        integer :: ier
    end subroutine transi
end interface
