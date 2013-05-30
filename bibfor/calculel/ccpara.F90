subroutine ccpara(option, modele, resuin, resuou, numord,&
                  nordm1, exitim, mateco, carael)
    implicit none
!     --- ARGUMENTS ---
    include 'jeveux.h'
!
    include 'asterc/getvtx.h'
    include 'asterc/r8nnem.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mearcc.h'
    include 'asterfort/mecact.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/vrcins.h'
    include 'asterfort/vrcref.h'
    integer :: numord, nordm1
    character(len=8) :: modele, resuin, resuou, mateco, carael
    character(len=16) :: option
    logical :: exitim
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.sellenet at edf.fr
    integer :: nparin, iret, ibid, jainst, jfreq, iaopds, iaoplo, ipara
    integer :: jnmo, opt, inume, iarg, jains2
!
    real(kind=8) :: rbid, tps(6), rundf, omega2, freq, time, zero
    parameter   (zero = 0.0d0)
!
    complex(kind=8) :: cbid
!
    character(len=2) :: chdret
    character(len=8) :: mailla, k8b, nomcmp(6)
    character(len=16) :: vari
    character(len=24) :: curcha, chtime, chfreq, chome2, chharm, chvref
    character(len=24) :: chmass, chnova, chsigf, chsig, chvarc, chvac2
    parameter   (chtime = '&&CCPARA.CH_INST_R')
    parameter   (chfreq = '&&CCPARA.FREQ')
    parameter   (chome2 = '&&CCPARA.OMEGA2')
    parameter   (chharm = '&&CCPARA.NUME_MODE')
    parameter   (chvarc = '&&CCPARA.VARI_INT_N')
    parameter   (chvref = '&&CCPARA.VARI_INT_REF')
    parameter   (chvac2 = '&&CCPARA.VARI_INT_NM1')
    parameter   (chmass = '&&CCPARA.MASS_MECA_D')
    parameter   (chnova = '&&CCPARA.NOM_VARI')
    parameter   (chsigf = '&&CCPARA.CHAM_SI2D')
!
    data nomcmp  /'INST    ','DELTAT  ','THETA   ','KHI     ',&
     &              'R       ','RHO     '/
!
    call jenonu(jexnom('&CATA.OP.NOMOPT', option), opt)
    call jeveuo(jexnum('&CATA.OP.DESCOPT', opt), 'L', iaopds)
    call jeveuo(jexnum('&CATA.OP.LOCALIS', opt), 'L', iaoplo)
    nparin = zi(iaopds-1+2)
!
    rundf = r8nnem()
!
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                mailla, iret)
!
    if (exitim) then
        call rsadpa(resuin, 'L', 1, 'INST', numord,&
                    0, jainst, k8b)
        time = zr(jainst)
    else
        time = zero
    endif
    call vrcref(modele, mateco, carael, chvref(1:19))
    call vrcins(modele, mateco, carael, time, chvarc(1:19),&
                chdret)
!
    do 10 ipara = 1, nparin
        curcha = zk24(iaoplo+3*ipara-2)
        if (curcha .eq. chtime) then
            call jenonu(jexnom(resuin//'           .NOVA', 'INST'), iret)
            if (iret .ne. 0) then
                call rsadpa(resuin, 'L', 1, 'INST', numord,&
                            0, jainst, k8b)
!
                tps(1) = zr(jainst)
                tps(2) = rundf
                tps(3) = rundf
                tps(4) = rundf
                tps(5) = rundf
                tps(6) = rundf
!
                call mecact('V', chtime, 'MAILLA', mailla, 'INST_R',&
                            6, nomcmp, ibid, tps, cbid,&
                            k8b)
            endif
!
        else if (curcha.eq.chfreq) then
            call jenonu(jexnom(resuin//'           .NOVA', 'FREQ'), iret)
            if (iret .ne. 0) then
                call rsadpa(resuin, 'L', 1, 'FREQ', numord,&
                            0, jfreq, k8b)
                freq = zr(jfreq)
            else
                freq = 1.d0
            endif
            call mecact('V', chfreq, 'MAILLA', mailla, 'FREQ_R',&
                        1, 'FREQ', ibid, freq, cbid,&
                        k8b)
!
        else if (curcha.eq.chome2) then
            call jenonu(jexnom(resuin//'           .NOVA', 'OMEGA2'), iret)
            if (iret .ne. 0) then
                call rsadpa(resuin, 'L', 1, 'OMEGA2', numord,&
                            0, jfreq, k8b)
                omega2 = zr(jfreq)
            else
                omega2 = 1.0d0
            endif
            call mecact('V', chome2, 'MAILLA', mailla, 'OME2_R',&
                        1, 'OMEG2', ibid, omega2, cbid,&
                        k8b)
!
        else if (curcha.eq.chharm) then
            call jenonu(jexnom(resuin//'           .NOVA', 'NUME_MODE'), iret)
            if (iret .ne. 0) then
                call rsadpa(resuin, 'L', 1, 'NUME_MODE', numord,&
                            0, jnmo, k8b)
                call mecact('V', chharm, 'MAILLA', mailla, 'HARMON',&
                            1, 'NH', zi(jnmo), rbid, cbid,&
                            ' ')
            endif
!
        else if (curcha.eq.chmass) then
            inume=1
            call mecact('V', chmass, 'MAILLA', mailla, 'POSI',&
                        1, 'POS', inume, rbid, cbid,&
                        k8b)
!
        else if (curcha.eq.chvac2) then
            if (exitim) then
                call rsadpa(resuin, 'L', 1, 'INST', nordm1,&
                            0, jains2, k8b)
                time = zr(jains2)
            else
                time = zero
            endif
            call vrcins(modele, mateco, carael, time, chvac2(1:19),&
                        chdret)
!
        else if (curcha.eq.chnova) then
            call getvtx(' ', 'NOM_VARI', 0, iarg, 1,&
                        vari, ibid)
            call mecact('V', chnova, 'MAILLA', mailla, 'NEUT_K24',&
                        1, 'Z1', ibid, rbid, cbid,&
                        vari)
!
        else if (curcha.eq.chsigf) then
            call rsexch(' ', resuin, 'SIGM_ELNO', numord, chsig,&
                        iret)
            if (iret .ne. 0) then
                call rsexch('F', resuou, 'SIGM_ELNO', numord, chsig,&
                            iret)
            endif
            call mearcc(option, modele, chsig, chsigf)
!
        endif
10  end do
!
end subroutine
