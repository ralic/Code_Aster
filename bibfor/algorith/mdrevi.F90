subroutine mdrevi(numddl, nbrevi, nbmode, bmodal, neq,&
                  dplrev, fonrev, ier)
    implicit none
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/mgutdm.h"
#include "asterfort/posddl.h"
#include "asterfort/resmod.h"
#include "asterfort/utmess.h"
#include "asterfort/utnono.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "asterfort/nlget.h"
#include "asterfort/nlsav.h"
!
    integer :: nbrevi, nbmode, neq, ier
    real(kind=8) :: dplrev(nbrevi, nbmode, *), bmodal(neq, *)
    character(len=8) :: fonrev(nbrevi, *)
    character(len=14) :: numddl
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     STOCKAGE DES INFORMATIONS DE REV DANS DES TABLEAUX
!     ------------------------------------------------------------------
! IN  : NUMDDL : NOM DU CONCEPT NUMDDL
! IN  : NBREVI : NOMBRE DE RELATION EFFORT VITESSE (REV)
! IN  : NBMODE : NOMBRE DE MODES DE LA BASE DE PROJECTION
! IN  : BMODAL : VECTEURS MODAUX
! IN  : NEQ    : NOMBRE D'EQUATIONS
! OUT : DPLREV : TABLEAU DES DEPLACEMENTS MODAUX AUX NOEUDS DE REV
! OUT : FONREV : TABLEAU DES FONCTIONS AUX NOEUDS DE REV
! OUT : IER    : CODE RETOUR
! ----------------------------------------------------------------------
!
!
!
!
!
    integer :: i, nunoe, nuddl, icomp
    character(len=8) :: noeu, comp, fonc, sst, noecho(3)
    character(len=8) :: sd_nl
    character(len=14) :: nume
    character(len=16) :: typnum
    character(len=24) :: mdgene, mdssno, numero
    character(len=24) :: valk
!
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: ibid, iret, j, ns
    real(kind=8), pointer :: dplcho(:) => null()
    character(len=24), pointer :: refe(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    ier = 0
    sd_nl = '&&OP29NL'
    call gettco(numddl, typnum)
!
    if (typnum(1:13) .eq. 'NUME_DDL_GENE') then
        call jeveuo(numddl//'.NUME.REFE', 'L', vk24=refe)
        mdgene = refe(1)
        mdssno = mdgene(1:14)//'.MODG.SSNO'
        numero(1:14) = numddl
    endif
!
    do 10 i = 1, nbrevi
!
        call nlget(sd_nl, _NO1_NAME, iocc=i, kscal=noeu)
        call nlget(sd_nl, _CMP_NAME, iocc=i, kscal=comp)
        call nlget(sd_nl, _FX_FONCT, iocc=i, kscal=fonc)
        call nlget(sd_nl, _SS1_NAME, iocc=i, kscal=sst)
        if (sst .eq. ' ') then
            ns = 0
        else
            ns = 1
        endif
!
        if (comp(1:2) .eq. 'DX') icomp = 1
        if (comp(1:2) .eq. 'DY') icomp = 2
        if (comp(1:2) .eq. 'DZ') icomp = 3
        if (comp(1:3) .eq. 'DRX') icomp = 4
        if (comp(1:3) .eq. 'DRY') icomp = 5
        if (comp(1:3) .eq. 'DRZ') icomp = 6
!
! ----- CALCUL DIRECT
        if (typnum .eq. 'NUME_DDL_SDASTER') then
            call posddl('NUME_DDL', numddl, noeu, comp, nunoe,&
                        nuddl)
!
! ----- CALCUL PAR SOUS-STRUCTURATION
        else if (typnum(1:13).eq.'NUME_DDL_GENE') then
            if (ns .eq. 0) then
                call utmess('F', 'ALGORITH5_63')
            endif
            call jenonu(jexnom(mdssno, sst), iret)
            if (iret .eq. 0) then
                call utmess('F', 'ALGORITH5_64')
            endif
            call mgutdm(mdgene, sst, ibid, 'NOM_NUME_DDL', ibid,&
                        nume)
            call posddl('NUME_DDL', nume(1:8), noeu, comp, nunoe,&
                        nuddl)
        endif
!
        if (nuddl .eq. 0) then
            valk = noeu
            call utmess('E+', 'ALGORITH15_16', sk=valk)
            if (typnum(1:13) .eq. 'NUME_DDL_GENE') then
                valk = sst
                call utmess('E+', 'ALGORITH15_17', sk=valk)
            endif
            valk = comp
            call utmess('E', 'ALGORITH15_18', sk=valk)
            ier = ier + 1
            goto 10
        endif
!
        do j = 1, nbmode
            dplrev(i,j,1) = 0.d0
            dplrev(i,j,2) = 0.d0
            dplrev(i,j,3) = 0.d0
            dplrev(i,j,4) = 0.d0
            dplrev(i,j,5) = 0.d0
            dplrev(i,j,6) = 0.d0
        end do
!
! ----- CALCUL DIRECT
        if (typnum .eq. 'NUME_DDL_SDASTER') then
            do j = 1, nbmode
                dplrev(i,j,icomp) = bmodal(nuddl,j)
            end do
!
! ----- CALCUL PAR SOUS-STRUCTURATION
        else if (typnum(1:13).eq.'NUME_DDL_GENE') then
            AS_ALLOCATE(vr=dplcho, size=nbmode*6)
            noecho(1) = noeu
            noecho(2) = sst
            noecho(3) = nume(1:8)
            call resmod(bmodal, nbmode, neq, numero, mdgene,&
                        noecho,dplcho)
            do j = 1, nbmode
                dplrev(i,j,icomp) = dplcho(j+(icomp-1)*nbmode)
            end do
            AS_DEALLOCATE(vr=dplcho)
        endif
!
        fonrev(i,1) = noeu
        fonrev(i,2) = comp
        fonrev(i,3) = fonc
!
10  continue
!
    call jedema()
end subroutine
