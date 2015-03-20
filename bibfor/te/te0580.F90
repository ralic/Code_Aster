subroutine te0580(nomopt, nomte)
    implicit none
! aslint: disable=C1505
#include "jeveux.h"
#include "asterfort/utmess.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
!     ------------------------------------------------------------------
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
    character(len=16) :: nomte, nomopt

!-----------------------------------------------------------------------

    character(len=1) :: code
    integer :: jad, itab(8), nbv, iret, k, kpara, mater, icodre(2)
    integer :: imate,idimge,npara,nno,igeom,ndim,ino,ier,ipt,nbpt
    real(kind=8) :: valres(2), valpar(3),vxyz, pr
    character(len=8) :: nompar(3)
    character(len=16) :: nomres(2)
    character(len=24) :: valk(2)
    character(len=32) :: phenom
    character(len=8) :: param
    character(len=8) :: lparam1(2)=(/ 'PPRESSR', 'PPRESSF' /)
    character(len=8) :: lparam2(6)
    character(len=8) :: lparam3(2)=(/ 'PFRCO3D', 'PFFCO3D' /)

    data  lparam2 / 'PPRESSR', 'PPRESSF', 'PFR2D3D', &
                    'PFF2D3D', 'PFR1D2D', 'PFF1D2D' /

!-----------------------------------------------------------------------
! Cette routine realise les calculs elementaires "triviaux" qui ne sont pas
! encore programmes par les elements.
! Par exemple les chargements de Neumann nuls.
!-----------------------------------------------------------------------


    if (nomopt(1:15).eq.'CHAR_MECA_PRES_' .or. nomopt(1:15).eq.'CHAR_MECA_PRSU_'  &
       .or. nomopt(1:15).eq.'RIGI_MECA_PRSU_'  ) then
!   ===================================================================================
        do kpara=1,2
            param=lparam1(kpara)
            call tecach('NNN', param, 'L', iret, nval=8, itab=itab)
            if (iret.eq.0) then
                jad=itab(1)
                nbv=itab(2)
                ASSERT(itab(5).eq.1 .or. itab(5).eq.4)
                if (itab(5).eq.1) then
                    do k=1,nbv
                        if (zr(jad-1+k).ne.0.d0) goto 998
                    enddo
                else
                    do k=1,nbv
                        if (zk8(jad-1+k).ne.'&FOZERO') then
                            call fointe(' ', zk8(jad-1+k), 0, ' ', [0.d0], pr, ier)
                            if (ier.eq.0 .and. pr.eq.0.d0) then
                                ! tout va bien ...
                            else
                                goto 998
                            endif
                        endif
                    enddo
                endif
            endif
        enddo


    elseif ( nomopt.eq.'CHAR_MECA_SFCO3D' .or. nomopt.eq.'CHAR_MECA_SRCO3D'  &
        .or. nomopt.eq.'RIGI_MECA_SFCO3D' .or. nomopt.eq.'RIGI_MECA_SRCO3D' ) then
!   ===================================================================================
        do kpara=1,2
            param=lparam3(kpara)
            call tecach('NNN', param, 'L', iret, nval=8, itab=itab)
            if (iret.eq.0) then
                jad=itab(1)
                nbv=itab(2)
                ASSERT(itab(5).eq.1 .or. itab(5).eq.4)
                ASSERT(mod(nbv,8).eq.0)
                nbpt=nbv/8
                ! on ne conserve que les 6 CMPS FX, FY, ..., MZ
                nbv=6
                if (itab(5).eq.1) then
                    do ipt=1,nbpt
                        do k=1,nbv
                            if (zr(jad-1+8*(ipt-1)+k).ne.0.d0) goto 998
                        enddo
                    enddo
                else
                    do ipt=1,nbpt
                        do k=1,nbv
                            if (zk8(jad-1+k).ne.'&FOZERO') then
                                call fointe(' ', zk8(jad-1+8*(ipt-1)+k), 0, ' ', [0.d0], pr, ier)
                                if (ier.eq.0 .and. pr.eq.0.d0) then
                                    ! tout va bien ...
                                else
                                    goto 998
                                endif
                            endif
                        enddo
                    enddo
                endif
            endif
        enddo


    elseif (nomopt.eq.'CALC_G' .or. nomopt.eq.'CALC_G_F' &
            .or. nomopt.eq.'CALC_K_G' .or. nomopt.eq.'CALC_K_G_F') then
!   =======================================================================

!       -- le resultat est nul si les forces de bord sont nulles ou
!          si le champ theta est nul.

!       -- on regarde d'abord theta :
        call tecach('OOO', 'PTHETAR', 'L', iret, nval=8, itab=itab)
        ASSERT(iret.eq.0)
        jad=itab(1)
        nbv=itab(2)
        do k=1,nbv
            if (zr(jad-1+k).ne.0.d0) goto 2
        enddo
        goto 999

!       -- on regarde toutes les forces :
2       continue
        do kpara=1,6
            param=lparam2(kpara)
            call tecach('NNN', param, 'L', iret, nval=8, itab=itab)
            if (iret.eq.0) then
                jad=itab(1)
                nbv=itab(2)
                ASSERT(itab(5).eq.1 .or. itab(5).eq.4)
                if (itab(5).eq.1) then
                    do k=1,nbv
                        if (zr(jad-1+k).ne.0.d0) goto 998
                    enddo
                else
                    do k=1,nbv
                        if (zk8(jad-1+k).ne.'&FOZERO') goto 998
                    enddo
                endif
            endif
        enddo


    elseif (nomopt.eq.'AMOR_MECA' ) then
!   ==========================================================================

!       -- le resultat est nul si les coefficients d'amortissement sont nuls ou absents.
        call jevech('PMATERC', 'L', imate)
        mater=zi(imate)
        call rccoma(mater, 'ELAS', 0, phenom, icodre(1))
        if (icodre(1).ne.0) goto 999

        nompar(1)='X'
        nompar(2)='Y'
        nompar(3)='Z'

        call elrefe_info(fami='RIGI',ndim=ndim,nno=nno)
        call tecach('ONN', 'PGEOMER', 'L', iret, nval=5, itab=itab)
        igeom=itab(1)
        idimge=itab(2)/nno

        ASSERT(idimge.eq.2 .or. idimge.eq.3)

        npara=idimge
        do k = 1, npara
            vxyz = 0.d0
            do ino = 1, nno
                vxyz = vxyz+zr(igeom + idimge*(ino-1) +k -1)
            end do
            valpar(k) = vxyz/nno
        end do

        nomres(1)='AMOR_ALPHA'
        nomres(2)='AMOR_BETA'
        call rcvalb('RIGI', 1, 1, '+', mater, ' ', phenom, npara, nompar, valpar, 2,&
                    nomres, valres, icodre, 0)
        if (icodre(1).ne.0 .and. icodre(2).ne.0) goto 999
        if (valres(1).eq.0.d0 .and. valres(2).eq.0.d0) goto 999

    else
        ASSERT(.false.)
    endif
    goto 999


!   -- erreur :
998 continue
    valk(1)=nomte
    valk(2)=nomopt
    code='F'

!   -- le bloc if suivant sera a retirer apres la correction de issue23503
    if (nomopt(1:14).eq.'CHAR_MECA_PRES') then
        if (nomte.eq.'HM_J_AXSE3'.or.nomte.eq.'HM_J_DPSE3') code='F'
    endif

!   -- le bloc if suivant sera a retirer apres la correction de issue23504
    if (nomopt(1:8).eq.'CALC_K_G') then
        if (nomte.eq.'MECA_XH_FACE4' .or. nomte.eq.'MECA_XHT_FACE4' &
            .or. nomte.eq.'MECA_XT_FACE4') code='A'
    endif
    call utmess(code, 'CALCULEL_44',2,valk=valk)


!   -- sortie normale :
999 continue

end subroutine

