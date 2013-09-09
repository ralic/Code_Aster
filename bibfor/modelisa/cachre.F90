subroutine cachre(char, ligrmo, noma, ndim, fonree,&
                  param, motcl)
    implicit none
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterc/getfac.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/char_affe_neum.h"
#include "asterfort/getvc8.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
#include "asterfort/tecart.h"
#include "asterfort/vetyma.h"
    integer :: ndim
    character(len=4) :: fonree
    character(len=5) :: param
    character(len=8) :: char, noma
    character(len=*) :: ligrmo, motcl
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! BUT : STOCKAGE DES CHARGES REPARTIES DANS UNE CARTE ALLOUEE SUR LE
!       LIGREL DU MODELE
!
! ARGUMENTS D'ENTREE:
!      CHAR   : NOM UTILISATEUR DU RESULTAT DE CHARGE
!      LIGRMO : NOM DU LIGREL DE MODELE
!      NOMA   : NOM DU MAILLAGE
!      NDIM   : DIMENSION DU PROBLEME (2D OU 3D)
!      FONREE : FONC OU REEL
!      PARAM  : NOM DU TROISIEME "CHAMP" DE LA CARTE (F3D3D F2D3D ...)
!      MOTCL  : MOT-CLE FACTEUR
! ----------------------------------------------------------------------
    integer :: i, n, nchre, nrep, ncmp, jvalv, jncmp, iocc, nfx, nfy, nfz
    integer :: nmx, nmy, nmz, nplan
    real(kind=8) :: fx, fy, fz, mx, my, mz, vpre
    complex(kind=8) :: cfx, cfy, cfz, cmx, cmy, cmz, cvpre
    character(len=8) :: kfx, kfy, kfz, kmx, kmy, kmz, typch, plan
    character(len=16) :: motclf
    character(len=19) :: carte
    character(len=19) :: cartes(1)
    integer :: ncmps(1)
!     ------------------------------------------------------------------
!
    call jemarq()
!
    motclf = motcl
    call getfac(motclf, nchre)
!
    carte = char(1:8)//'.CHME.'//param(1:5)
!
    if (fonree .eq. 'REEL') then
        call alcart('G', carte, noma, 'FORC_R')
    else if (fonree.eq.'FONC') then
        call alcart('G', carte, noma, 'FORC_F')
    else if (fonree.eq.'COMP') then
        call alcart('G', carte, noma, 'FORC_C')
    else
        ASSERT(.false.)
    endif
!
    call jeveuo(carte//'.NCMP', 'E', jncmp)
    call jeveuo(carte//'.VALV', 'E', jvalv)
!
! --- STOCKAGE DE FORCES NULLES SUR TOUT LE MAILLAGE
!     ET REPERE = 0.(SI 'REEL'),REPERE = 'GLOBAL' (SI FONC) ---
!
    zk8(jncmp-1+1) = 'FX'
    zk8(jncmp-1+2) = 'FY'
    zk8(jncmp-1+3) = 'FZ'
    zk8(jncmp-1+4) = 'MX'
    zk8(jncmp-1+5) = 'MY'
    zk8(jncmp-1+6) = 'MZ'
    zk8(jncmp-1+7) = 'REP'
    zk8(jncmp-1+8) = 'PLAN'
    if (fonree(1:4) .eq. 'REEL') then
        do i = 1, 8
            zr(jvalv-1+i) = 0.d0
        enddo
    else if (fonree(1:4).eq.'COMP') then
        do i = 1, 8
            zc(jvalv-1+i) = dcmplx( 0.d0 , 0.d0 )
        enddo
    else if (fonree.eq.'FONC') then
        do i = 1, 6
            zk8(jvalv-1+i) = '&FOZERO'
        enddo
        zk8(jvalv-1+7) = 'GLOBAL'
        zk8(jvalv-1+8) = '&FOZERO'
    else
        ASSERT(.false.)
    endif
    call nocart(carte, 1, 8)
!
! --- STOCKAGE DANS LA CARTE ---
!
    do iocc = 1, nchre
        nrep = 0
        ncmp = 0
        if (motclf .eq. 'FORCE_POUTRE') then
            call getvtx(motclf, 'TYPE_CHARGE', iocc=iocc, scal=typch, nbret=n)
            if (typch .eq. 'VENT') nrep = 2
        endif
        if (fonree .eq. 'COMP') then
            call getvc8(motclf, 'FX', iocc=iocc, scal=cfx, nbret=nfx)
            call getvc8(motclf, 'FY', iocc=iocc, scal=cfy, nbret=nfy)
            call getvc8(motclf, 'FZ', iocc=iocc, scal=cfz, nbret=nfz)
            if (motclf .ne. 'FORCE_INTERNE' .and. motclf .ne. 'FORCE_POUTRE' .and. motclf&
                .ne. 'FORCE_FACE') then
                call getvc8(motclf, 'MX', iocc=iocc, scal=cmx, nbret=nmx)
                call getvc8(motclf, 'MY', iocc=iocc, scal=cmy, nbret=nmy)
                call getvc8(motclf, 'MZ', iocc=iocc, scal=cmz, nbret=nmz)
            else
                nmx = 0
                nmy = 0
                nmz = 0
            endif
            if (nfx+nfy+nfz+nmx+nmy+nmz .eq. 0) then
                if (motclf .eq. 'FORCE_POUTRE') then
                    nrep = 1
                    call getvc8(motclf, 'N', iocc=iocc, scal=cfx, nbret=nfx)
                    call getvc8(motclf, 'VY', iocc=iocc, scal=cfy, nbret=nfy)
                    call getvc8(motclf, 'VZ', iocc=iocc, scal=cfz, nbret=nfz)
!                 CALL GETVC8 ( MOTCLF, 'MT' , IOCC,IARG, 1, CMX, NMX )
!                 CALL GETVC8 ( MOTCLF, 'MFY', IOCC,IARG, 1, CMY, NMY )
!                 CALL GETVC8 ( MOTCLF, 'MFZ', IOCC,IARG, 1, CMZ, NMZ )
                else if (motclf .eq. 'FORCE_COQUE') then
                    nrep = 1
                    call getvc8(motclf, 'PRES', iocc=iocc, scal=cvpre, nbret=nfz)
                    if (nfz .eq. 0) then
                        call getvc8(motclf, 'F1', iocc=iocc, scal=cfx, nbret=nfx)
                        call getvc8(motclf, 'F2', iocc=iocc, scal=cfy, nbret=nfy)
                        call getvc8(motclf, 'F3', iocc=iocc, scal=cfz, nbret=nfz)
                        call getvc8(motclf, 'MF1', iocc=iocc, scal=cmx, nbret=nmx)
                        call getvc8(motclf, 'MF2', iocc=iocc, scal=cmy, nbret=nmy)
                        nmz = 0
                    else
                        cfz = -cvpre
                        nfx = 0
                        nfy = 0
                        nmx = 0
                        nmy = 0
                        nmz = 0
                    endif
                endif
            endif
            if (nfx .ne. 0) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'FX'
                zc(jvalv-1+ncmp) = cfx
            endif
            if (nfy .ne. 0) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'FY'
                zc(jvalv-1+ncmp) = cfy
            endif
            if (nfz .ne. 0) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'FZ'
                zc(jvalv-1+ncmp) = cfz
            endif
            if (nmx .ne. 0) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'MX'
                zc(jvalv-1+ncmp) = cmx
            endif
            if (nmy .ne. 0) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'MY'
                zc(jvalv-1+ncmp) = cmy
            endif
            if (nmz .ne. 0) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'MZ'
                zc(jvalv-1+ncmp) = cmz
            endif
            if (nrep .ge. 1) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'REP'
                if (nrep .eq. 1) zc(jvalv-1+ncmp) = dcmplx(1.d0,1.d0)
                if (nrep .eq. 2) zc(jvalv-1+ncmp) = dcmplx(2.d0,2.d0)
                if (nrep .eq. 1) zc(jvalv-1+ncmp) = 1.d0
                if (nrep .eq. 2) zc(jvalv-1+ncmp) = 2.d0
            endif
        else if (fonree.eq.'REEL') then
            call getvr8(motclf, 'FX', iocc=iocc, scal=fx, nbret=nfx)
            call getvr8(motclf, 'FY', iocc=iocc, scal=fy, nbret=nfy)
            call getvr8(motclf, 'FZ', iocc=iocc, scal=fz, nbret=nfz)
            if (motclf .ne. 'FORCE_INTERNE' .and. motclf .ne. 'FORCE_FACE') then
                call getvr8(motclf, 'MX', iocc=iocc, scal=mx, nbret=nmx)
                call getvr8(motclf, 'MY', iocc=iocc, scal=my, nbret=nmy)
                call getvr8(motclf, 'MZ', iocc=iocc, scal=mz, nbret=nmz)
            else
                nmx = 0
                nmy = 0
                nmz = 0
            endif
            if (nfx+nfy+nfz+nmx+nmy+nmz .eq. 0) then
                if (motclf .eq. 'FORCE_POUTRE') then
                    nrep = 1
                    call getvr8(motclf, 'N', iocc=iocc, scal=fx, nbret=nfx)
                    call getvr8(motclf, 'VY', iocc=iocc, scal=fy, nbret=nfy)
                    call getvr8(motclf, 'VZ', iocc=iocc, scal=fz, nbret=nfz)
                    call getvr8(motclf, 'MT', iocc=iocc, scal=mx, nbret=nmx)
                    call getvr8(motclf, 'MFY', iocc=iocc, scal=my, nbret=nmy)
                    call getvr8(motclf, 'MFZ', iocc=iocc, scal=mz, nbret=nmz)
                else if (motclf .eq. 'FORCE_COQUE') then
                    nrep = 1
                    call getvr8(motclf, 'PRES', iocc=iocc, scal=vpre, nbret=nfz)
                    if (nfz .eq. 0) then
                        call getvr8(motclf, 'F1', iocc=iocc, scal=fx, nbret=nfx)
                        call getvr8(motclf, 'F2', iocc=iocc, scal=fy, nbret=nfy)
                        call getvr8(motclf, 'F3', iocc=iocc, scal=fz, nbret=nfz)
                        call getvr8(motclf, 'MF1', iocc=iocc, scal=mx, nbret=nmx)
                        call getvr8(motclf, 'MF2', iocc=iocc, scal=my, nbret=nmy)
                        nmz = 0
                    else
                        fz = -vpre
                        nfx = 0
                        nfy = 0
                        nmx = 0
                        nmy = 0
                        nmz = 0
                    endif
                endif
            endif
            if (nfx .ne. 0) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'FX'
                zr(jvalv-1+ncmp) = fx
            endif
            if (nfy .ne. 0) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'FY'
                zr(jvalv-1+ncmp) = fy
            endif
            if (nfz .ne. 0) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'FZ'
                zr(jvalv-1+ncmp) = fz
            endif
            if (nmx .ne. 0) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'MX'
                zr(jvalv-1+ncmp) = mx
            endif
            if (nmy .ne. 0) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'MY'
                zr(jvalv-1+ncmp) = my
            endif
            if (nmz .ne. 0) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'MZ'
                zr(jvalv-1+ncmp) = mz
            endif
            if (nrep .ge. 1) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'REP'
                if (nrep .eq. 1) zr(jvalv-1+ncmp) = 1.d0
                if (nrep .eq. 2) zr(jvalv-1+ncmp) = 2.d0
            endif
        else
            call getvid(motclf, 'FX', iocc=iocc, scal=kfx, nbret=nfx)
            call getvid(motclf, 'FY', iocc=iocc, scal=kfy, nbret=nfy)
            call getvid(motclf, 'FZ', iocc=iocc, scal=kfz, nbret=nfz)
            if (motclf .ne. 'FORCE_INTERNE' .and. motclf .ne. 'FORCE_FACE') then
                call getvid(motclf, 'MX', iocc=iocc, scal=kmx, nbret=nmx)
                call getvid(motclf, 'MY', iocc=iocc, scal=kmy, nbret=nmy)
                call getvid(motclf, 'MZ', iocc=iocc, scal=kmz, nbret=nmz)
            else
                nmx = 0
                nmy = 0
                nmz = 0
            endif
            if (nfx+nfy+nfz+nmx+nmy+nmz .eq. 0) then
                if (motclf .eq. 'FORCE_POUTRE') then
                    nrep = 1
                    call getvid(motclf, 'N', iocc=iocc, scal=kfx, nbret=nfx)
                    call getvid(motclf, 'VY', iocc=iocc, scal=kfy, nbret=nfy)
                    call getvid(motclf, 'VZ', iocc=iocc, scal=kfz, nbret=nfz)
                    call getvid(motclf, 'MT', iocc=iocc, scal=kmx, nbret=nmx)
                    call getvid(motclf, 'MFY', iocc=iocc, scal=kmy, nbret=nmy)
                    call getvid(motclf, 'MFZ', iocc=iocc, scal=kmz, nbret=nmz)
                else if (motclf .eq. 'FORCE_COQUE') then
                    nrep = 1
                    call getvid(motclf, 'PRES', iocc=iocc, scal=kfz, nbret=nfz)
                    if (nfz .eq. 0) then
                        call getvid(motclf, 'F1', iocc=iocc, scal=kfx, nbret=nfx)
                        call getvid(motclf, 'F2', iocc=iocc, scal=kfy, nbret=nfy)
                        call getvid(motclf, 'F3', iocc=iocc, scal=kfz, nbret=nfz)
                        call getvid(motclf, 'MF1', iocc=iocc, scal=kmx, nbret=nmx)
                        call getvid(motclf, 'MF2', iocc=iocc, scal=kmy, nbret=nmy)
                        nmz = 0
                    else
                        nfx = 0
                        nfy = 0
                        nmx = 0
                        nmy = 0
                        nmz = 0
                        nrep = 3
                    endif
                endif
            endif
            if (nfx .ne. 0) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'FX'
                zk8(jvalv-1+ncmp) = kfx
            endif
            if (nfy .ne. 0) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'FY'
                zk8(jvalv-1+ncmp) = kfy
            endif
            if (nfz .ne. 0) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'FZ'
                zk8(jvalv-1+ncmp) = kfz
            endif
            if (nmx .ne. 0) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'MX'
                zk8(jvalv-1+ncmp) = kmx
            endif
            if (nmy .ne. 0) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'MY'
                zk8(jvalv-1+ncmp) = kmy
            endif
            if (nmz .ne. 0) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'MZ'
                zk8(jvalv-1+ncmp) = kmz
            endif
            if (nrep .ge. 1) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'REP'
                if (nrep .eq. 1) zk8(jvalv-1+ncmp) = 'LOCAL'
                if (nrep .eq. 2) zk8(jvalv-1+ncmp) = 'VENT'
                if (nrep .eq. 3) zk8(jvalv-1+ncmp) = 'LOCAL_PR'
! --           (NREP=3) CAS D UNE PRESSION --> ON PREND L OPPOSE DE
! --           LA VALEUR LUE DANS LE TE
            endif
        endif
        if (ncmp .eq. 0) goto 20
!
        if (motclf .eq. 'FORCE_COQUE') then
            call getvtx(motclf, 'PLAN', iocc=iocc, scal=plan, nbret=nplan)
            if (nplan .ne. 0) then
                ncmp = ncmp + 1
                zk8(jncmp-1+ncmp) = 'PLAN'
                if (fonree .eq. 'REEL') then
                    if (plan .eq. 'MAIL') then
                        zr(jvalv-1+ncmp) = dble(0)
                    else if (plan.eq.'INF') then
                        zr(jvalv-1+ncmp) = dble(-1)
                    else if (plan.eq.'SUP') then
                        zr(jvalv-1+ncmp) = dble(1)
                    else if (plan.eq.'MOY') then
                        zr(jvalv-1+ncmp) = dble(2)
                    endif
                else if (fonree.eq.'FONC') then
                    zk8(jvalv-1+ncmp) = plan
                endif
            endif
        endif
!
        cartes(1) = carte
        ncmps(1) = ncmp
        call char_affe_neum(noma, ndim, motclf, iocc, 1,&
                            cartes, ncmps)
!
20      continue
    end do
!
    call tecart(carte)
    call jedema()
end subroutine
