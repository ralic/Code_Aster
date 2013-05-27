subroutine cachre(char, ligrmo, noma, ndim, fonree,&
                  param, motcl)
    implicit   none
    include 'jeveux.h'
    include 'asterc/getexm.h'
    include 'asterc/getfac.h'
    include 'asterc/getvc8.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/alcart.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/nocart.h'
    include 'asterfort/reliem.h'
    include 'asterfort/tecart.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/vetyma.h'
    integer :: ndim
    character(len=4) :: fonree
    character(len=5) :: param
    character(len=8) :: char, noma
    character(len=*) :: ligrmo, motcl
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    integer :: ibid, i, n, nchre, nrep, ncmp, jvalv, jncmp, iocc, nfx, nfy, nfz
    integer :: nmx, nmy, nmz, nplan, nbtou, ier, nbma, jma
    real(kind=8) :: fx, fy, fz, mx, my, mz, vpre
    complex(kind=8) :: cfx, cfy, cfz, cmx, cmy, cmz, cvpre
    character(len=8) :: k8b, kfx, kfy, kfz, kmx, kmy, kmz, typch, plan
    character(len=8) :: typmcl(2)
    character(len=16) :: motclf, motcle(2)
    character(len=19) :: carte
    character(len=24) :: mesmai
    integer :: xtout
    integer :: iarg
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
        call u2mesk('F', 'MODELISA2_37', 1, fonree(1:4))
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
        do 10 i = 1, 8
            zr(jvalv-1+i) = 0.d0
10      continue
    else if (fonree(1:4).eq.'COMP') then
        do 12 i = 1, 8
            zc(jvalv-1+i) = dcmplx( 0.d0 , 0.d0 )
12      continue
    else if (fonree.eq.'FONC') then
        do 14 i = 1, 6
            zk8(jvalv-1+i) = '&FOZERO'
14      continue
        zk8(jvalv-1+7) = 'GLOBAL'
        zk8(jvalv-1+8) = '&FOZERO'
    else
        call u2mesk('F', 'MODELISA2_37', 1, fonree)
    endif
    call nocart(carte, 1, ' ', 'NOM', 0,&
                ' ', 0, ligrmo, 8)
!
    mesmai = '&&CACHRE.MES_MAILLES'
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
! --- STOCKAGE DANS LA CARTE ---
!
    do 20 iocc = 1, nchre
        nrep = 0
        ncmp = 0
        if (motclf .eq. 'FORCE_POUTRE') then
            call getvtx(motclf, 'TYPE_CHARGE', iocc, iarg, 1,&
                        typch, n)
            if (typch .eq. 'VENT') nrep = 2
        endif
        if (fonree .eq. 'COMP') then
            call getvc8(motclf, 'FX', iocc, iarg, 1,&
                        cfx, nfx)
            call getvc8(motclf, 'FY', iocc, iarg, 1,&
                        cfy, nfy)
            call getvc8(motclf, 'FZ', iocc, iarg, 1,&
                        cfz, nfz)
            if (motclf .ne. 'FORCE_INTERNE' .and. motclf .ne. 'FORCE_POUTRE' .and. motclf&
                .ne. 'FORCE_FACE') then
                call getvc8(motclf, 'MX', iocc, iarg, 1,&
                            cmx, nmx)
                call getvc8(motclf, 'MY', iocc, iarg, 1,&
                            cmy, nmy)
                call getvc8(motclf, 'MZ', iocc, iarg, 1,&
                            cmz, nmz)
            else
                nmx = 0
                nmy = 0
                nmz = 0
            endif
            if (nfx+nfy+nfz+nmx+nmy+nmz .eq. 0) then
                if (motclf .eq. 'FORCE_POUTRE') then
                    nrep = 1
                    call getvc8(motclf, 'N', iocc, iarg, 1,&
                                cfx, nfx)
                    call getvc8(motclf, 'VY', iocc, iarg, 1,&
                                cfy, nfy)
                    call getvc8(motclf, 'VZ', iocc, iarg, 1,&
                                cfz, nfz)
!                 CALL GETVC8 ( MOTCLF, 'MT' , IOCC,IARG, 1, CMX, NMX )
!                 CALL GETVC8 ( MOTCLF, 'MFY', IOCC,IARG, 1, CMY, NMY )
!                 CALL GETVC8 ( MOTCLF, 'MFZ', IOCC,IARG, 1, CMZ, NMZ )
                else if (motclf .eq. 'FORCE_COQUE') then
                    nrep = 1
                    call getvc8(motclf, 'PRES', iocc, iarg, 1,&
                                cvpre, nfz)
                    if (nfz .eq. 0) then
                        call getvc8(motclf, 'F1', iocc, iarg, 1,&
                                    cfx, nfx)
                        call getvc8(motclf, 'F2', iocc, iarg, 1,&
                                    cfy, nfy)
                        call getvc8(motclf, 'F3', iocc, iarg, 1,&
                                    cfz, nfz)
                        call getvc8(motclf, 'MF1', iocc, iarg, 1,&
                                    cmx, nmx)
                        call getvc8(motclf, 'MF2', iocc, iarg, 1,&
                                    cmy, nmy)
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
            call getvr8(motclf, 'FX', iocc, iarg, 1,&
                        fx, nfx)
            call getvr8(motclf, 'FY', iocc, iarg, 1,&
                        fy, nfy)
            call getvr8(motclf, 'FZ', iocc, iarg, 1,&
                        fz, nfz)
            if (motclf .ne. 'FORCE_INTERNE' .and. motclf .ne. 'FORCE_FACE') then
                call getvr8(motclf, 'MX', iocc, iarg, 1,&
                            mx, nmx)
                call getvr8(motclf, 'MY', iocc, iarg, 1,&
                            my, nmy)
                call getvr8(motclf, 'MZ', iocc, iarg, 1,&
                            mz, nmz)
            else
                nmx = 0
                nmy = 0
                nmz = 0
            endif
            if (nfx+nfy+nfz+nmx+nmy+nmz .eq. 0) then
                if (motclf .eq. 'FORCE_POUTRE') then
                    nrep = 1
                    call getvr8(motclf, 'N', iocc, iarg, 1,&
                                fx, nfx)
                    call getvr8(motclf, 'VY', iocc, iarg, 1,&
                                fy, nfy)
                    call getvr8(motclf, 'VZ', iocc, iarg, 1,&
                                fz, nfz)
                    call getvr8(motclf, 'MT', iocc, iarg, 1,&
                                mx, nmx)
                    call getvr8(motclf, 'MFY', iocc, iarg, 1,&
                                my, nmy)
                    call getvr8(motclf, 'MFZ', iocc, iarg, 1,&
                                mz, nmz)
                else if (motclf .eq. 'FORCE_COQUE') then
                    nrep = 1
                    call getvr8(motclf, 'PRES', iocc, iarg, 1,&
                                vpre, nfz)
                    if (nfz .eq. 0) then
                        call getvr8(motclf, 'F1', iocc, iarg, 1,&
                                    fx, nfx)
                        call getvr8(motclf, 'F2', iocc, iarg, 1,&
                                    fy, nfy)
                        call getvr8(motclf, 'F3', iocc, iarg, 1,&
                                    fz, nfz)
                        call getvr8(motclf, 'MF1', iocc, iarg, 1,&
                                    mx, nmx)
                        call getvr8(motclf, 'MF2', iocc, iarg, 1,&
                                    my, nmy)
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
            call getvid(motclf, 'FX', iocc, iarg, 1,&
                        kfx, nfx)
            call getvid(motclf, 'FY', iocc, iarg, 1,&
                        kfy, nfy)
            call getvid(motclf, 'FZ', iocc, iarg, 1,&
                        kfz, nfz)
            if (motclf .ne. 'FORCE_INTERNE' .and. motclf .ne. 'FORCE_FACE') then
                call getvid(motclf, 'MX', iocc, iarg, 1,&
                            kmx, nmx)
                call getvid(motclf, 'MY', iocc, iarg, 1,&
                            kmy, nmy)
                call getvid(motclf, 'MZ', iocc, iarg, 1,&
                            kmz, nmz)
            else
                nmx = 0
                nmy = 0
                nmz = 0
            endif
            if (nfx+nfy+nfz+nmx+nmy+nmz .eq. 0) then
                if (motclf .eq. 'FORCE_POUTRE') then
                    nrep = 1
                    call getvid(motclf, 'N', iocc, iarg, 1,&
                                kfx, nfx)
                    call getvid(motclf, 'VY', iocc, iarg, 1,&
                                kfy, nfy)
                    call getvid(motclf, 'VZ', iocc, iarg, 1,&
                                kfz, nfz)
                    call getvid(motclf, 'MT', iocc, iarg, 1,&
                                kmx, nmx)
                    call getvid(motclf, 'MFY', iocc, iarg, 1,&
                                kmy, nmy)
                    call getvid(motclf, 'MFZ', iocc, iarg, 1,&
                                kmz, nmz)
                else if (motclf .eq. 'FORCE_COQUE') then
                    nrep = 1
                    call getvid(motclf, 'PRES', iocc, iarg, 1,&
                                kfz, nfz)
                    if (nfz .eq. 0) then
                        call getvid(motclf, 'F1', iocc, iarg, 1,&
                                    kfx, nfx)
                        call getvid(motclf, 'F2', iocc, iarg, 1,&
                                    kfy, nfy)
                        call getvid(motclf, 'F3', iocc, iarg, 1,&
                                    kfz, nfz)
                        call getvid(motclf, 'MF1', iocc, iarg, 1,&
                                    kmx, nmx)
                        call getvid(motclf, 'MF2', iocc, iarg, 1,&
                                    kmy, nmy)
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
            call getvtx(motclf, 'PLAN', iocc, iarg, 1,&
                        plan, nplan)
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
        xtout=getexm( motclf, 'TOUT')
        nbtou=0
        if (xtout .eq. 1) then
            call getvtx(motclf, 'TOUT', iocc, iarg, 1,&
                        k8b, nbtou)
        endif
!
        if (nbtou .ne. 0) then
!
            call nocart(carte, 1, ' ', 'NOM', 0,&
                        ' ', 0, ligrmo, ncmp)
        else
            call reliem(ligrmo, noma, 'NO_MAILLE', motclf, iocc,&
                        2, motcle, typmcl, mesmai, nbma)
            if (nbma .eq. 0) goto 20
            call jeveuo(mesmai, 'L', jma)
            call vetyma(noma, zk8(jma), nbma, k8b, 0,&
                        motclf, ndim, ier)
            call nocart(carte, 3, k8b, 'NOM', nbma,&
                        zk8(jma), ibid, ' ', ncmp)
            call jedetr(mesmai)
        endif
!
20  end do
!
    call tecart(carte)
    call jedema()
end subroutine
