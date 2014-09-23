subroutine b3d_pm(vg, vp0, dv0, depst6, ssg6,&
                  dg3, epspg6, epspfg6, etag, bg,&
                  xmg, pg, dt, epg0, vw0,&
                  poro0, biot0, xwsat, pw1, bw1,&
                  epsvpw, vplg33, vplg33t, e0, rt0,&
                  ept0, erreur, xwns, mfr, pw0,&
                  dpw, vw1)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: etienne grimal at edf.fr
!=====================================================================
!=====================================================================
!     calcul des pressions, des deformations plastiques
!     associees aux fissurations diffuses dues au gel
!=====================================================================
    implicit none

#include "asterfort/b3d_bgpg.h"
#include "asterfort/b3d_bwpw.h"
#include "asterfort/b3d_sdif.h"
#include "asterfort/b3d_viscoplast.h"

        real(kind=8) :: vg
        real(kind=8) :: vp0
        real(kind=8) :: dv0
        real(kind=8) :: depst6(6)
        real(kind=8) :: ssg6(6)
        real(kind=8) :: dg3(3)
        real(kind=8) :: epspg6(6)
        real(kind=8) :: epspfg6(6)
        real(kind=8) :: etag
        real(kind=8) :: bg
        real(kind=8) :: xmg
        real(kind=8) :: pg
        real(kind=8) :: dt
        real(kind=8) :: epg0
        real(kind=8) :: vw0
        real(kind=8) :: poro0
        real(kind=8) :: biot0
        real(kind=8) :: xwsat
        real(kind=8) :: pw1
        real(kind=8) :: bw1
        real(kind=8) :: epsvpw
        real(kind=8) :: vplg33(3, 3)
        real(kind=8) :: vplg33t(3, 3)
        real(kind=8) :: e0
        real(kind=8) :: rt0
        real(kind=8) :: ept0
        integer :: erreur
        real(kind=8) :: xwns
        integer :: mfr
        real(kind=8) :: pw0
        real(kind=8) :: dpw
        real(kind=8) :: vw1
!
!     declaration externe
    integer :: i,j
!
!     declarations locales
    real(kind=8) :: depsv, epsvve, vpt
    real(kind=8) :: ssd33(3, 3), ssd3(3), vssg33(3, 3), vssg33t(3, 3)
    real(kind=8) :: x33(3, 3), epsp33(3, 3), epspl3(3), epl3(3), dff3(3), st3(3)
    real(kind=8) :: rapp3(3),epsvpg,epsvt
!
!**********************************************************************
!     calcul du volume poreux accessible au gel en debut de pas
!
    depsv=0.d0
    epsvpg=0.d0
    do i = 1, 3
!      increment de deformation volumique total
        depsv=depsv+depst6(i)
!      deformation volumique plastique cumulee
        epsvpg=epsvpg+epspg6(i)
    end do
!     nouvelle deformation visco elastique totale
    epsvt=dv0+depsv
!      print*,depst6
!      print*,epsvt ,dv0,depsv
!**********************************************************************
!     calcul des pressions avant ecoulement plastique
!
!     pression du gel
    call b3d_bgpg(vg, biot0, poro0, xmg, vp0,&
                  bg, epsvt, epsvpg, 0.d0, pg)
!
!     pression hydrique avant ecoulement plastique
!       print*,'ds b3d_pm',biot0,vw0,xwsat,poro0,epsvt,epsvpw,epsvpg,vg,
!    # pw1,bw1,xwns,mfr,pw0,dpw,vw1
    call b3d_bwpw(biot0, vw0, xwsat, poro0, epsvt,&
                  epsvpw, epsvpg, vg, pw1, bw1,&
                  xwns, mfr, pw0, dpw, vw1)
!
!      print*,'Press av ecoul ds b3d_pm bg pg:',bg,pg
!
!**********************************************************************
!     ecoulement plastique du au gel
    if ((dt.gt.0.) .and. (xmg.ne.0.)) then
!       pression du gel
        call b3d_bgpg(vg, biot0, poro0, xmg, vp0,&
                      bg, epsvt, epsvpg, 0.d0, pg)
!       endommagement a l infini du au gel
!       i.e l endo si la pression de gel etait maintenue
!        print*,'ds b3d_pm',dt,xmg,E0,ept0
        call b3d_sdif(ssg6, e0, rt0, ept0, erreur,&
                      dff3, st3, vssg33, vssg33t, rapp3)
!       print*,'ds b3d_rgi ssd',ssg6
!       ecoulement plastique pour le gel  et endo du gel
        call b3d_viscoplast(pg, etag, dt, epg0, dff3,&
                            xmg, bg, epsvpg, epspg6, epspfg6,&
                            vssg33, vssg33t, vplg33, vplg33t, dg3)
!       pressions a la fin du pas
        epsvpg=0.d0
        do i = 1, 3
            epsvpg=epsvpg+epspfg6(i)
!          print*,'ds b3d_pm epspfg(',i,')=',epspfg6(i)
        end do
!       pression du gel
        call b3d_bgpg(vg, biot0, poro0, xmg, vp0,&
                      bg, epsvt, epsvpg, 0.d0, pg)
!       pression hydrique avant ecoulement plastique
        call b3d_bwpw(biot0, vw0, xwsat, poro0, epsvt,&
                      epsvpw, epsvpg, vg, pw1, bw1,&
                      xwns, mfr, pw0, dpw, vw1)
    else
        do i = 1, 6
            epspfg6(i)=epspg6(i)
        end do
        do i = 1, 3
!         print*,'mise a 0 de dg ds b3d_pm'
!         dg3(i)=0.d0
            do j = 1, 3
                if (i .ne. j) then
                    vplg33(i,j)=0.d0
                    vplg33t(i,j)=0.d0
                else
                    vplg33(i,j)=1.d0
                    vplg33t(i,j)=1.d0
                    end if
                    end do
                    end do
                    end if
!     print*,'Press ap ecoul ds b3d_pm bg,pg:',bg,pg
end subroutine
