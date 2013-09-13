subroutine op0147()
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!   CALCUL DES INTERSPECTRES DE REPONSE MODALE (DYNA_SPEC_MODAL)
!      LE CONCEPT PRODUIT EST UN INTERSPECTRE
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/calcsp.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ordis.h"
#include "asterfort/titre.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!-----------------------------------------------------------------------
    integer :: i, ifreq, im, imasg, imod1, inumo
    integer :: inuor, ivite, jnuor, nbm, nbmr
    integer :: nnn, npv, i1, i3, ivitef
!-----------------------------------------------------------------------
    logical :: casint
    character(len=8) :: table, nomu, option
    character(len=16) :: concep, cmd
    character(len=19) :: base
    character(len=24) :: freq, masg, vite, numo, nomobj, chnumi
    integer ::  lnumi, lrefe, lrefes
    real(kind=8) :: epsi, val, vitef
!
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
!
    call getres(nomu, concep, cmd)
!
! --- 1.RECUPERATION DES OBJETS DE LA BASE MODALE PERTURBEE ---
!
    call getvid(' ', 'BASE_ELAS_FLUI', scal=base)
!
    masg = base//'.MASG'
    vite = base//'.VITE'
    freq = base//'.FREQ'
    numo = base//'.NUMO'
!
    call jeveuo(masg, 'L', imasg)
!
    call jeveuo(vite, 'L', ivite)
    call jelira(vite, 'LONUTI', npv)
    call getvr8(' ', 'VITE_FLUI', scal=vitef)
    call getvr8(' ', 'PRECISION', scal=epsi)
!
    ivitef = 1
    do 300 i3 = 1, npv
        val = zr(ivite-1+i3)-vitef
        if (abs(val) .lt. epsi) then
            ivitef = i3
        endif
300  end do
!
    call jeveuo(freq, 'L', ifreq)
    call jelira(freq, 'LONUTI', nbm)
    nbm = nbm / ( 2 * npv )
!
    call jeveuo(numo, 'L', inumo)
!
! --- 2.RECUPERATION DU NOM DE LA TABLE ---
!
    call getvid('EXCIT ', 'INTE_SPEC_GENE', iocc=1, scal=table)
!
!     VERIFICATION DES PARAMETRES DE LA TABLE
!
    chnumi = table//'.NUMI'
    call jeveuo(chnumi, 'L', lnumi)
    call jelira(chnumi, 'LONMAX', nbmr)
!
    nomobj = '&&OP0147.TEMP.NUOR'
    call wkvect(nomobj, 'V V I', nbmr, jnuor)
    do 150 i1 = 1, nbmr
        zi(jnuor-1+i1) = zi(lnumi-1+i1)
150  end do
    call ordis(zi(jnuor), nbmr)
    call wkvect('&&OP0147.MODE', 'V V I', nbmr, inuor)
    nnn = 1
    zi(inuor) = zi(jnuor)
    do 20 i = 2, nbmr
        if (zi(jnuor+i-1) .eq. zi(inuor+nnn-1)) goto 20
        nnn = nnn + 1
        zi(inuor+nnn-1) = zi(jnuor+i-1)
20  end do
    nbmr = nnn
    do 30 im = 1, nbm
        if (zi(inumo+im-1) .eq. zi(inuor)) then
            imod1 = im
            goto 31
        endif
30  end do
    call u2mess('F', 'MODELISA5_78')
31  continue
!
! --- 3.RECUPERATION DE L'OPTION DE CALCUL ---
!
    casint = .true.
    call getvtx(' ', 'OPTION', scal=option)
    if (option(1:4) .eq. 'DIAG') casint = .false.
!
    call jeveuo(table//'.REFE', 'L', lrefe)
    if (zk16(lrefe+1)(1:4) .eq. 'DIAG' .and. casint) then
        call u2mess('F', 'MODELISA5_79')
    endif
!
! --- 4.CREATION DE LA STRUCTURE RESULTAT ET CALCUL DE LA REPONSE ---
! ---   PAR CALCSP                                                ---
!
    call wkvect(nomu//'.REFE', 'G V K16', 2, lrefes)
    zk16(lrefes) = 'DEPL_GENE'
    zk16(lrefes+1) = option
!
    call calcsp(casint, nomu, table, zr(ifreq), zr(imasg),&
                nbm, nbmr, imod1, zi(inuor), ivitef)
!
    call titre()
!
!
    call jedema()
end subroutine
