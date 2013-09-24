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
    subroutine newton(np1, np2, np3, np4, nbm,&
                      n2, nbmcd, icoupl, tc, dt,&
                      dtc, vecdt, nbnl, typch, nbseg,&
                      phii, choc, alpha, beta, gamma,&
                      orig, rc, theta, vgap, vecr4,&
                      indic, tpfl, veci1, vecr1, vecr2,&
                      vecr5, vecr3, masgi, amori, pulsi,&
                      amor, amor0, puls, puls0, xsi0,&
                      vitg, depg, accg0, vitg0, depg0,&
                      vitgc, depgc, vitgt, depgt, cmod,&
                      kmod, cmod0, kmod0, cmodca, kmodca,&
                      amflu0, amfluc, locflc, cmodfa, npfts,&
                      textts, fextts, ndef, indt, fexmod,&
                      fnlmod, fmoda, fmres, fmod0, fmod00,&
                      fmodt, fmod0t, vitg0t, depg0t, ftmp,&
                      mtmp1, mtmp2, mtmp6, ttr, u,&
                      w, dd, loc, intge1, intge2,&
                      indx, indxf, vvg, vg, vg0,&
                      vd, vd0, rr, rr0, ri,&
                      premac, prerel, trans, pulsd, s0,&
                      z0, sr0, za1, za2, za3,&
                      zin, old, oldia, iconfe, iconfa,&
                      nbcha, nbchea, ftest, iconfb, tconf1,&
                      tconf2, toln, tolc, tolv, testc,&
                      itforn)
        integer :: n2
        integer :: np4
        integer :: np3
        integer :: np2
        integer :: np1
        integer :: nbm
        integer :: nbmcd
        integer :: icoupl
        real(kind=8) :: tc
        real(kind=8) :: dt
        real(kind=8) :: dtc
        real(kind=8) :: vecdt(*)
        integer :: nbnl
        integer :: typch(*)
        integer :: nbseg(*)
        real(kind=8) :: phii(np2, np1, *)
        real(kind=8) :: choc(6, *)
        real(kind=8) :: alpha(2, *)
        real(kind=8) :: beta(2, *)
        real(kind=8) :: gamma(2, *)
        real(kind=8) :: orig(6, *)
        real(kind=8) :: rc(np3, *)
        real(kind=8) :: theta(np3, *)
        real(kind=8) :: vgap
        real(kind=8) :: vecr4(*)
        integer :: indic
        character(len=8) :: tpfl
        integer :: veci1(*)
        real(kind=8) :: vecr1(*)
        real(kind=8) :: vecr2(*)
        real(kind=8) :: vecr5(*)
        real(kind=8) :: vecr3(*)
        real(kind=8) :: masgi(*)
        real(kind=8) :: amori(*)
        real(kind=8) :: pulsi(*)
        real(kind=8) :: amor(*)
        real(kind=8) :: amor0(*)
        real(kind=8) :: puls(*)
        real(kind=8) :: puls0(*)
        real(kind=8) :: xsi0(*)
        real(kind=8) :: vitg(*)
        real(kind=8) :: depg(*)
        real(kind=8) :: accg0(*)
        real(kind=8) :: vitg0(*)
        real(kind=8) :: depg0(*)
        real(kind=8) :: vitgc(*)
        real(kind=8) :: depgc(*)
        real(kind=8) :: vitgt(*)
        real(kind=8) :: depgt(*)
        real(kind=8) :: cmod(np1, *)
        real(kind=8) :: kmod(np1, *)
        real(kind=8) :: cmod0(np1, *)
        real(kind=8) :: kmod0(np1, *)
        real(kind=8) :: cmodca(np1, *)
        real(kind=8) :: kmodca(np1, *)
        real(kind=8) :: amflu0(np1, *)
        real(kind=8) :: amfluc(np1, *)
        logical :: locflc(*)
        real(kind=8) :: cmodfa(np1, *)
        integer :: npfts
        real(kind=8) :: textts(*)
        real(kind=8) :: fextts(np4, *)
        integer :: ndef
        integer :: indt
        real(kind=8) :: fexmod(*)
        real(kind=8) :: fnlmod(*)
        real(kind=8) :: fmoda(*)
        real(kind=8) :: fmres(*)
        real(kind=8) :: fmod0(*)
        real(kind=8) :: fmod00(*)
        real(kind=8) :: fmodt(*)
        real(kind=8) :: fmod0t(*)
        real(kind=8) :: vitg0t(*)
        real(kind=8) :: depg0t(*)
        real(kind=8) :: ftmp(*)
        real(kind=8) :: mtmp1(np1, *)
        real(kind=8) :: mtmp2(np1, *)
        real(kind=8) :: mtmp6(3, *)
        real(kind=8) :: ttr(n2, *)
        real(kind=8) :: u(*)
        real(kind=8) :: w(*)
        real(kind=8) :: dd(*)
        logical :: loc(*)
        integer :: intge1(*)
        integer :: intge2(*)
        integer :: indx(*)
        integer :: indxf(*)
        real(kind=8) :: vvg(np1, *)
        real(kind=8) :: vg(np1, *)
        real(kind=8) :: vg0(np1, *)
        real(kind=8) :: vd(np1, *)
        real(kind=8) :: vd0(np1, *)
        real(kind=8) :: rr(*)
        real(kind=8) :: rr0(*)
        real(kind=8) :: ri(*)
        real(kind=8) :: premac
        real(kind=8) :: prerel
        real(kind=8) :: trans(2, 2, *)
        real(kind=8) :: pulsd(*)
        complex(kind=8) :: s0(*)
        complex(kind=8) :: z0(*)
        complex(kind=8) :: sr0(*)
        complex(kind=8) :: za1(*)
        complex(kind=8) :: za2(*)
        complex(kind=8) :: za3(*)
        complex(kind=8) :: zin(*)
        real(kind=8) :: old(9, *)
        integer :: oldia(*)
        integer :: iconfe
        integer :: iconfa
        integer :: nbcha
        integer :: nbchea
        real(kind=8) :: ftest
        integer :: iconfb(*)
        real(kind=8) :: tconf1(4, *)
        real(kind=8) :: tconf2(4, *)
        real(kind=8) :: toln
        real(kind=8) :: tolc
        real(kind=8) :: tolv
        integer :: testc
        integer :: itforn(*)
    end subroutine newton
end interface
