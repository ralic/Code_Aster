subroutine mdrede(numddl, nbrede, nbmode, bmodal, neq,&
                  dplred, fonred, ier)
    implicit none
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/mgutdm.h"
#include "asterfort/posddl.h"
#include "asterfort/resmod.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbrede, nbmode, neq, ier
    real(kind=8) :: dplred(nbrede, nbmode, *), bmodal(neq, *)
    character(len=8) :: fonred(nbrede, *)
    character(len=14) :: numddl
! ----------------------------------------------------------------------
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
!     STOCKAGE DES INFORMATIONS DE RED DANS DES TABLEAUX
!     ------------------------------------------------------------------
! IN  : NUMDDL       : NOM DU CONCEPT NUMDDL
! IN  : NBREDE       : NOMBRE DE RELATION EFFORT DEPLACEMENT (RED)
! IN  : NBMODE       : NOMBRE DE MODES DE LA BASE DE PROJECTION
! IN  : BMODAL       : VECTEURS MODAUX
! IN  : NEQ          : NOMBRE D'EQUATIONS
! OUT : DPLRED       : TABLEAU DES DEPLACEMENTS MODAUX AUX NOEUDS DE RED
! OUT : FONRED( ,1-3): TABLEAU DES FONCTIONS AUX NOEUDS DE RED
! OUT : IER          : CODE RETOUR
! ----------------------------------------------------------------------
!
!
!
    integer :: i, nunoe, nuddl, icomp
    character(len=8) :: noeu, comp, fonc, sst, noecho(3)
    character(len=14) :: nume
    character(len=16) :: typnum
    character(len=24) :: mdgene, mdssno, numero
    character(len=24) :: valk
!
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: ibid, iret, j, jdpl, llrefe
    integer :: nc, nf, nn, ns
!-----------------------------------------------------------------------
    call jemarq()
    ier = 0
    call gettco(numddl, typnum)
!
    if (typnum(1:13) .eq. 'NUME_DDL_GENE') then
        call jeveuo(numddl//'.NUME.REFE', 'L', llrefe)
        mdgene = zk24(llrefe)
        mdssno = mdgene(1:14)//'.MODG.SSNO'
        numero(1:14) = numddl
    endif
!
    do 10 i = 1, nbrede
!
        call getvtx('RELA_EFFO_DEPL', 'NOEUD', iocc=i, scal=noeu, nbret=nn)
        call getvtx('RELA_EFFO_DEPL', 'NOM_CMP', iocc=i, scal=comp, nbret=nc)
        call getvid('RELA_EFFO_DEPL', 'RELATION', iocc=i, scal=fonc, nbret=nf)
        call getvtx('RELA_EFFO_DEPL', 'SOUS_STRUC', iocc=i, scal=sst, nbret=ns)
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
                call u2mess('F', 'ALGORITH5_63')
            endif
            call jenonu(jexnom(mdssno, sst), iret)
            if (iret .eq. 0) then
                call u2mess('F', 'ALGORITH5_64')
            endif
            call mgutdm(mdgene, sst, ibid, 'NOM_NUME_DDL', ibid,&
                        nume)
            call posddl('NUME_DDL', nume(1:8), noeu, comp, nunoe,&
                        nuddl)
        endif
!
        if (nuddl .eq. 0) then
            valk = noeu
            call u2mesg('E+', 'ALGORITH15_16', 1, valk, 0,&
                        0, 0, 0.d0)
            if (typnum(1:13) .eq. 'NUME_DDL_GENE') then
                valk = sst
                call u2mesg('E+', 'ALGORITH15_17', 1, valk, 0,&
                            0, 0, 0.d0)
            endif
            valk = comp
            call u2mesg('E', 'ALGORITH15_18', 1, valk, 0,&
                        0, 0, 0.d0)
            ier = ier + 1
            goto 10
        endif
!
        do 11 j = 1, nbmode
            dplred(i,j,1) = 0.d0
            dplred(i,j,2) = 0.d0
            dplred(i,j,3) = 0.d0
            dplred(i,j,4) = 0.d0
            dplred(i,j,5) = 0.d0
            dplred(i,j,6) = 0.d0
11      continue
!
! ----- CALCUL DIRECT
        if (typnum .eq. 'NUME_DDL_SDASTER') then
            do 13 j = 1, nbmode
                dplred(i,j,icomp) = bmodal(nuddl,j)
13          continue
!
! ----- CALCUL PAR SOUS-STRUCTURATION
        else if (typnum(1:13).eq.'NUME_DDL_GENE') then
            call wkvect('&&MDREDE.DPLCHO', 'V V R8', nbmode*6, jdpl)
            noecho(1) = noeu
            noecho(2) = sst
            noecho(3) = nume
            call resmod(bmodal, nbmode, neq, numero, mdgene,&
                        noecho, zr( jdpl))
            do 12 j = 1, nbmode
                dplred(i,j,icomp) = zr(jdpl-1+j+(icomp-1)*nbmode)
12          continue
            call jedetr('&&MDREDE.DPLCHO')
        endif
!
        fonred(i,1) = noeu
        fonred(i,2) = comp
        fonred(i,3) = fonc
!
10  end do
!
    call jedema()
end subroutine
