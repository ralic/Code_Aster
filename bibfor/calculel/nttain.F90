subroutine nttain(modele, mate, carele, charge, infoch,&
                  numedd, solveu, time, epsr, lonch,&
                  matass, maprec, cnchci, cnresi, vtemp,&
                  vtempm, vtempp, vec2nd, chlapm, chlapp,&
                  ci1, ci2, testi)
!
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
!
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
#include "asterfort/asasve.h"
#include "asterfort/ascova.h"
#include "asterfort/copisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/resoud.h"
#include "asterfort/vetrth.h"
    integer :: lonch
    real(kind=8) :: epsr, testi
    character(len=1) :: ci1, ci2
    character(len=19) :: solveu, maprec
    character(len=24) :: modele, mate, carele, charge, infoch, numedd, time
    character(len=24) :: matass, cnchci, cnresi
    character(len=24) :: vtemp, vtempm, vtempp, vec2nd, chlapm, chlapp
!
! ----------------------------------------------------------------------
!
! COMMANDE THER_MOBI_NLINE : ITERATIONS
!
! ----------------------------------------------------------------------
!
!     IN  VTEMP  : CHAMP DE TEMPERATURE A L'INSTANT PRECEDENT
!     VAR VTEMPM : ITERE PRECEDENT DU CHAMP DE TEMPERATURE
!     OUT VTEMPP : ITERE COURANT   DU CHAMP DE TEMPERATURE
!
!
    complex(kind=8) :: cbid
!
!
    integer :: k, jvare, j2nd, jtempm, jtempp, jtemp
    real(kind=8) :: r8bid, testn, rbid
    character(len=1) :: typres
    character(len=19) :: chsol
    character(len=24) :: bidon, veresi, varesi, criter
!
!-----------------------------------------------------------------------
    integer :: iret
!-----------------------------------------------------------------------
    data typres        /'R'/
    data chsol         /'&&NTTAIN.SOLUTION'/
    data bidon         /'&&FOMULT.BIDON'/
    data veresi        /'&&VERESI           .RELR'/
! ----------------------------------------------------------------------
!
    call jemarq()
    varesi = '&&VARESI'
    criter = '&&RESGRA_GCPC'
!
    ci1 = ' '
    ci2 = ' '
!
! --- RECUPERATION D'ADRESSES
!
    call jeveuo(vec2nd(1:19)//'.VALE', 'L', j2nd)
!
! ======================================================================
!
! --- VECTEURS ELEMENTAIRES DU SEGOND MEMBRE
!
    call vetrth(modele, charge, infoch, carele, mate,&
                time, vtemp, vtempm, chlapm, chlapp,&
                veresi)
!
! --- ASSEMBLAGE DU SEGOND MEMBRE
!
    call asasve(veresi, numedd, typres, varesi)
    call ascova('D', varesi, bidon, 'INST', r8bid,&
                typres, cnresi)
    call jeveuo(cnresi(1:19)//'.VALE', 'L', jvare)
!
! --- RESOLUTION (VTEMPP CONTIENT LE SECOND MEMBRE, CHSOL LA SOLUTION)
!
    call jeveuo(vtempp(1:19)//'.VALE', 'E', jtempp)
    do 130 k = 1, lonch
        zr(jtempp+k-1) = zr(j2nd+k-1) + zr(jvare+k-1)
130  continue
!
    call resoud(matass, maprec, solveu, cnchci, 0,&
                vtempp, chsol, 'V', [0.d0], [cbid],&
                criter, .true., 0, iret)
!
! --- RECOPIE DANS VTEMPP DU CHAMP SOLUTION CHSOL
!
    call copisd('CHAMP_GD', 'V', chsol, vtempp(1:19))
    call jeveuo(vtempm(1:19)//'.VALE', 'E', jtempm)
    call jeveuo(vtemp(1:19)//'.VALE', 'E', jtemp)
    call jeveuo(vtempp(1:19)//'.VALE', 'L', jtempp)
!
! --- EVALUATION DE !!T(I+1)-T(I)!! ET ACTUALISATION DE LA TEMPERATURE
!
    testi = 0.d0
    testn = 0.d0
    do 131 k = 1, lonch
        testi = testi + (zr(jtempp+k-1)-zr(jtempm+k-1))**2
        testn = testn + (zr(jtempp+k-1))**2
        zr(jtemp +k-1) = zr(jtempm+k-1)
        zr(jtempm+k-1) = zr(jtempp+k-1)
131  continue
    if (testn .gt. 0) testi = testi/testn
!
! --- A-T-ON CONVERGE ?
!
    testi = sqrt(testi)
    if (testi .le. epsr) then
        ci1 = '*'
    else
        ci1 = ' '
    endif
!-----------------------------------------------------------------------
    call jeexin(criter(1:19)//'.CRTI', iret)
    if (iret .ne. 0) then
        call jedetr(criter(1:19)//'.CRTI')
        call jedetr(criter(1:19)//'.CRTR')
        call jedetr(criter(1:19)//'.CRDE')
    endif
    call jedema()
end subroutine
