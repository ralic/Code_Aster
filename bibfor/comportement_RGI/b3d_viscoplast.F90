subroutine b3d_viscoplast(pg, etag, dt, epg0, dff3,&
                          xmg, bg, epsvpg, epspg6, epspfg6,&
                          vssd33, vssd33t, vplg33, vplg33t, dg3)
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
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!=====================================================================
!     ecoulement viscoplastique en base principale des seuils
!     de fissuration diffuse
!     epl=epg0*d/(1-d)<epl max // (pg=0)
!=====================================================================
    implicit none
#include "asterfort/x6x33.h"
#include "asterfort/b3d_chrep.h"
#include "asterfort/b3d_valp33.h"
#include "asterfort/x33x6.h"
#include "asterfort/transpos1.h"
!     variables externes
    real(kind=8) :: pg
    real(kind=8) :: etag
    real(kind=8) :: dt
    real(kind=8) :: epg0
    real(kind=8) :: dff3(3)
    real(kind=8) :: xmg
    real(kind=8) :: bg
    real(kind=8) :: epsvpg
    real(kind=8) :: epspg6(6)
    real(kind=8) :: epspfg6(6)
    real(kind=8) :: vssd33(3, 3)
    real(kind=8) :: vssd33t(3, 3)
    real(kind=8) :: vplg33(3, 3)
    real(kind=8) :: vplg33t(3, 3)
    real(kind=8) :: dg3(3)
!
!     variables locales
    real(kind=8) :: aux0, aux1, aux2, vpl00, dvgeff, vgeff, xlambda, epg1
    integer :: i
    real(kind=8) :: epspl3(3), x33(3, 3), epsp33(3, 3), eplg3(3)
!
!     prise en compte du signe de la pression motrice pour
!     le signe de l'écoulement viscoplastique
!
    if (pg .ge. 0.d0) then
        epg1=epg0
    else
        epg1=-epg0
    end if
!
!     calcul des deformations limites associees a lendo micro
!     et ecoulement viscoplastique
    aux1=abs(pg)/etag
    aux2=aux1*dt
!     deformation volumique limite a l infini si pression maintenue
    vpl00=0.d0
    do i = 1, 3
!       print*,'ds b3d_viscoplast dff',dff3(i)
        epspl3(i)=epg1*dff3(i)/(1.d0-dff3(i))
        vpl00=vpl00+epspl3(i)
    end do
!     deformation volumique limite pour annuler la pression de gel
    if (xmg .gt. 0.) then
        if (bg .lt. 0.99) then
            dvgeff=pg/xmg/(1.d0-bg)
        else
            print*,'bg>0.9999 dans b3d_viscoplast !'
            dvgeff=pg/xmg/0.0001d0
!        read*
        end if
    else
        dvgeff=0.d0
    end if
    vgeff=epsvpg+dvgeff
!
!     on maintient la pression positive ou nulle lors de l ecoulement
!     viscoplastique
    if (pg .gt. 0.d0) then
        if (vpl00 .gt. vgeff) then
!       ecoulement asymptotique trop grand / volume de gel, on le limite
!       au volume de gel : risque de pression negative a annuler
            xlambda=vgeff/vpl00
            do i = 1, 3
                epspl3(i)=epspl3(i)*xlambda
            end do
!       print*,'vgeff',vgeff,'vp0',vp0,'lambda',xlambda
        end if
    else
!      la pression de gel est negative : on la fait tendre vers 0
        if (vpl00 .lt. vgeff) then
!       ecoulement trop grand / volume de gel, on le limite
!       au volume de gel
            xlambda=vgeff/vpl00
            do i = 1, 3
                epspl3(i)=epspl3(i)*xlambda
            end do
!       print*,'vgeff',vgeff,'vp0',vp0,'lambda',xlambda
        end if
    end if
!
!     ecoulement viscoplastique
!     passage des def plast ds les dir prin d endo
    call x6x33(epspg6, x33)
    call b3d_chrep(epsp33, x33, vssd33)
    call x33x6(epsp33, epspg6)
    do i = 1, 3
        if (pg .gt. 0.d0) then
!       ecoulement dilatant  car pression positive
            if (epspl3(i) .gt. epspg6(i)) then
                epspfg6(i)=epspl3(i)+(epspg6(i)-epspl3(i))*exp(-aux2)
            else
                epspfg6(i)=epspg6(i)
            end if
        else
!       ecoulement contractant car pression negative
            if (epspl3(i) .lt. epspg6(i)) then
                epspfg6(i)=epspl3(i)+(epspg6(i)-epspl3(i))*exp(-aux2)
            else
                epspfg6(i)=epspg6(i)
            end if
        end if
    end do
    do i = 4, 6
        epspfg6(i)=epspg6(i)
    end do
!      do i=1,6
!        print*,'b3d_viscoplast epsp(',i,')=',epspfg6(i)
!      end do
!     retour des deformations plastiques initiales en base fixe
!     retour en base fixe
    call x6x33(epspg6, x33)
    call b3d_chrep(epsp33, x33, vssd33t)
    call x33x6(epsp33, epspg6)
!     retour des deformations plastiques finales en base fixe
!     retour en base fixe
    call x6x33(epspfg6, x33)
    call b3d_chrep(epsp33, x33, vssd33t)
    call x33x6(epsp33, epspfg6)
!     calcul des endo reel de gel en fonction des deformation plastiques
!     call affiche33(epsp33)
    call b3d_valp33(epsp33, eplg3, vplg33)
!      call affiche33(vplg33)
!      print*,eplg3
!      print*,epg1
!      call affiche33(vplg33t)
    call transpos1(vplg33t, vplg33, 3)
!
!     mise a jour des endommagement reels associes a la def visco plasti
    do i = 1, 3
        aux0=(epg0+eplg3(i))
        dg3(i)=eplg3(i)/aux0
!       print*,'d diffus (',i,')=',dg3(i)
    end do
end subroutine
