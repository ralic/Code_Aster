subroutine asexc1(motfac, nbocc, nbmode, parmod, amort,&
                  corfre, ndir, valspe, asyspe)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8depi.h"
#include "asterfort/fointe.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    integer :: nbocc, nbmode, ndir(*)
    real(kind=8) :: parmod(nbmode, *), amort(*), valspe(3, *), asyspe(*)
    character(len=*) :: motfac
    aster_logical :: corfre
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     COMMANDE : COMB_SISM_MODAL
!                TRAITEMENT DU MOT-CLE "EXCIT" POUR LE MONO-APPUI
!     ------------------------------------------------------------------
! IN  : MOTFAC : MOT CLE FACTEUR
! IN  : NBOCC  : NOMBRE D'OCCURENCE DU MOT CLE FACTEUR
! IN  : NBMODE : NOMBRE DE MODES
! IN  : AMORT  : AMORTISSEMENTS MODAUX
! IN  : PARMOD : PARAMETRES MODAUX
! IN  : CORFRE : CORRECTION FREQUENCE SI .TRUE.
! OUT : NDIR   : DIRECTION DU SEISME A ETUDIER
! OUT : VALSPE : VALEURS DU SPECTRE
! OUT : ASYSPE : VALEURS ASYMPTOTIQUES DU SPECTRE
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: nature(3), id, ier, ifm, ii, im, inat, ioc, n1
    integer :: nimpr, jvar1
    real(kind=8) :: amor, coef, deuxpi, echel, epsi, freq, dirspe(3), echspe(3)
    real(kind=8) :: valpu(2), omega, omega2, resu, un, uns2pi, xnorm, zero
    real(kind=8) :: fcoup
    character(len=1) :: dir(3)
    character(len=4) :: knat
    character(len=8) :: spect, nomspe(3), nompu(2)
    character(len=9) :: niveau
!     ------------------------------------------------------------------
    data  nompu / 'AMOR' , 'FREQ'    /
    data   dir  / 'X' , 'Y' , 'Z' /
!     ------------------------------------------------------------------
!
    call jemarq()
    ier = 0
    ifm = iunifi('RESULTAT')
    epsi = 1.d-03
    zero = 0.d0
    un = 1.d0
    deuxpi = r8depi()
    uns2pi = un / deuxpi
!
!     --- LECTURE MOT-CLE FACTEUR IMPRESSION ---
!
    call getvtx('IMPRESSION', 'NIVEAU', iocc=1, scal=niveau, nbret=nimpr)
    if (nimpr .eq. 0) niveau='TOUT     '
!
    call getvr8(' ', 'FREQ_COUP', iocc=1, scal=fcoup, nbret=n1)
    if (n1 .eq. 0) then
        fcoup = uns2pi * sqrt(parmod(nbmode,2))
    endif
!
    do 10 ioc = 1, nbocc
!
        echspe(1) = un
        echspe(2) = un
        echspe(3) = un
        dirspe(1) = un
        dirspe(2) = un
        dirspe(3) = un
        xnorm = un
!
!        --- RECUPERATION DE LA DIRECTION DU SPECTRE ---
        call getvr8(motfac, 'AXE', iocc=ioc, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            call getvr8(motfac, 'AXE', iocc=ioc, nbval=3, vect=dirspe,&
                        nbret=n1)
            xnorm = zero
            do 12 id = 1, 3
                xnorm = xnorm + dirspe(id) * dirspe(id)
 12         continue
            if (xnorm .lt. epsi) then
                ier = ier + 1
                call utmess('E', 'SEISME_4')
                goto 10
            endif
            xnorm = un / sqrt(xnorm)
            call getvid(motfac, 'SPEC_OSCI', iocc=ioc, scal=spect, nbret=n1)
            nomspe(1) = spect
            nomspe(2) = spect
            nomspe(3) = spect
            call getvr8(motfac, 'ECHELLE', iocc=ioc, scal=echel, nbret=n1)
            if (n1 .ne. 0) then
                echspe(1) = echel
                echspe(2) = echel
                echspe(3) = echel
            endif
!
        else
            call getvr8(motfac, 'TRI_AXE', iocc=ioc, nbval=0, nbret=n1)
            if (n1 .ne. 0) then
                call getvr8(motfac, 'TRI_AXE', iocc=ioc, nbval=3, vect=dirspe,&
                            nbret=n1)
                call getvid(motfac, 'SPEC_OSCI', iocc=ioc, scal=spect, nbret=n1)
                nomspe(1) = spect
                nomspe(2) = spect
                nomspe(3) = spect
                call getvr8(motfac, 'ECHELLE', iocc=ioc, scal=echel, nbret=n1)
                if (n1 .ne. 0) then
                    echspe(1) = echel
                    echspe(2) = echel
                    echspe(3) = echel
                endif
!
            else
!
                call getvid(motfac, 'SPEC_OSCI', iocc=ioc, nbval=3, vect=nomspe,&
                            nbret=n1)
                call getvr8(motfac, 'ECHELLE', iocc=ioc, nbval=3, vect=echspe,&
                            nbret=n1)
            endif
        endif
!
        call getvtx(motfac, 'NATURE', iocc=ioc, scal=knat, nbret=n1)
        if (knat .eq. 'ACCE') inat = 1
        if (knat .eq. 'VITE') inat = 2
        if (knat .eq. 'DEPL') inat = 3
!
        do 14 id = 1, 3
            dirspe(id) = xnorm * dirspe(id)
            if (abs(dirspe(id)) .gt. epsi) then
                if (ndir(id) .ne. 0) then
                    ier = ier + 1
                    call utmess('E', 'SEISME_5')
                    goto 10
                else
                    ndir(id) = 1
                endif
                nature(id) = inat
            endif
 14     continue
!
 10 end do
!
    if (ier .ne. 0) then
        call utmess('F', 'SEISME_6')
    endif
!
!     --- INTERPOLATION DES SPECTRES ---
    if (niveau .eq. 'TOUT     ' .or. niveau .eq. 'SPEC_OSCI') then
        write(ifm,1000)
        write(ifm,1010)
    endif
    do 20 im = 1, nbmode
        ii = 0
        amor = amort(im)
        omega2 = parmod(im,2)
        omega = sqrt( omega2 )
        freq = uns2pi * omega
        valpu(1) = amor
        valpu(2) = freq
        if (corfre) valpu(2) = valpu(2) * sqrt( un - amor*amor )
        do 22 id = 1, 3
            if (ndir(id) .eq. 1) then
                call fointe('F ', nomspe(id), 2, nompu, valpu,&
                            resu, ier)
                coef = dirspe(id)*echspe(id)
                if (nature(id) .eq. 1) then
                    valspe(id,im) = resu * coef
                else if (nature(id).eq.2) then
                    valspe(id,im) = resu * coef * omega
                else
                    valspe(id,im) = resu * coef * omega2
                endif
                if (niveau .eq. 'TOUT     ' .or. niveau .eq. 'SPEC_OSCI') then
                    if (ii .eq. 0) then
                        ii = 1
                        write(ifm,1100)im,freq,amor,dir(id),valspe(id,&
                        im)
                    else
                        write(ifm,1110)dir(id),valspe(id,im)
                    endif
                endif
            endif
 22     continue
 20 end do
!
!     --- VALEURS ASYMPTOTIQUES DES SPECTRES ---
    if (niveau .eq. 'TOUT     ' .or. niveau .eq. 'SPEC_OSCI') then
        write(ifm,1300)
        write(ifm,1310)
    endif
    do 30 id = 1, 3
        if (ndir(id) .eq. 1) then
            amor=amort(nbmode)
            valpu(1) = amor
            valpu(2) = fcoup
            omega = deuxpi * fcoup
            if (corfre) valpu(2) = valpu(2) * sqrt( un - amor*amor )
            call fointe('F ', nomspe(id), 2, nompu, valpu,&
                        resu, ier)
            coef = dirspe(id)*echspe(id)
            if (nature(id) .eq. 1) then
                asyspe(id) = resu * coef
            else if (nature(id).eq.2) then
                asyspe(id) = resu * coef * omega
            else
                asyspe(id) = resu * coef * omega * omega
            endif
            if (niveau .eq. 'TOUT     ' .or. niveau .eq. 'SPEC_OSCI') write(ifm, 1410)dir(id),&
                                                                      asyspe(id)
        endif
 30 end do
!
    1000 format(/,1x,'--- VALEURS DU SPECTRE ---')
    1010 format(1x,&
     &'MODE      FREQUENCE    AMORTISSEMENT    DIR         SPECTRE')
    1100 format(1p,1x,i4,3x,d12.5,5x,d12.5,6x,a1,4x,d12.5)
    1110 format(1p,43x,a1,4x,d12.5)
    1300 format(/,1x,'--- VALEURS CORRECTION STATIQUE ---')
    1310 format(1x,'DIRECTION                ')
    1410 format(1p,9x,a1,4x,d12.5)
!
    call jedema()
end subroutine
