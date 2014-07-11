subroutine ascavc(lchar, infcha, fomult, numedd, inst, vci)
    implicit none
!
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
! person_in_charge: jacques.pellet at edf.fr
!
#include "jeveux.h"
#include "asterfort/ascova.h"
#include "asterfort/assert.h"
#include "asterfort/calvci.h"
#include "asterfort/corich.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcnco2.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rgndas.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
!
    character(len=24) :: lchar, infcha, fomult
    character(len=*) :: vci, numedd
    real(kind=8) :: inst
! ----------------------------------------------------------------------
! BUT  :  CALCUL DU CHAM_NO CONTENANT LE VECTEUR LE CINEMATIQUE
! ---     ASSOCIE A LA LISTE DE CHAR_CINE_* LCHAR A UN INSTANT INST
!         AVEC LES FONCTIONS MULTIPLICATIVES FOMULT.
! ----------------------------------------------------------------------
! IN  K*24 LCHAR : NOM DE L'OJB S V K24 CONTENANT LA LISTE DES CHARGES
! IN  K*19 INFCHA : NOM DE L'OJB S V I CONTENANT LA LISTE DES INFO.
! IN  K*24 FOMULT : NOM DE L'OJB S V K24 CONTENANT LA LISTE DES FONC.
! IN  K*14 NUMEDD  : NOM DE LA NUMEROTATION SUPPORTANT LE CHAM_NO
! IN  R*8  INST   : VALE DU PARAMETRE INST.
! VAR/JXOUT  K*19 VCI    :  CHAM_NO RESULTAT
!   -------------------------------------------------------------------
!-----------------------------------------------------------------------
!----------------------------------------------------------------------
!     VARIABLES LOCALES
!----------------------------------------------------------------------
    integer :: idchar, jinfc, idfomu, nchtot, nchci, ichar, icine, ilchno
    integer :: ichci, ibid, ifm, niv, neq, ieq, jdlci2,  ieqmul
    character(len=8) :: newnom
    character(len=1) :: tyddl
    character(len=19) :: charci, chamno, vci2
    character(len=24) :: vachci
    character(len=8) :: charge
    integer, pointer :: dlci(:) => null()
    data chamno/'&&ASCAVC.???????'/
    data vachci/'&&ASCAVC.LISTE_CI'/
    data charci/'&&ASCAVC.LISTE_CHI'/
!----------------------------------------------------------------------
!
!
    call jemarq()
    if (vci .eq. ' ') vci='&&ASCAVC.VCI'
    vci2=vci
!
    call infniv(ifm, niv)
!
!
    newnom='.0000000'
!
    call jedetr(vachci)
    call jedetr(vci2//'.DLCI')
    call jeveuo(lchar, 'L', idchar)
    call jeveuo(infcha, 'L', jinfc)
    call jeveuo(fomult, 'L', idfomu)
!
    nchtot = zi(jinfc)
    nchci = 0
    ieqmul=0
!
    do ichar = 1, nchtot
        icine = zi(jinfc+ichar)
        if (icine .lt. 0) nchci=nchci+1
!       -- UNE CHARGE NON "CINEMATIQUE" PEUT EN CONTENIR UNE :
        charge=zk24(idchar-1+ichar)(1:8)
    end do
!
!
    call wkvect(vachci, 'V V K24', max(nchci, 1), ilchno)
!
!     -- S'IL N'Y A PAS DE CHARGES CINEMATIQUES, ON CREE UN CHAMP NUL:
    if (nchci .eq. 0) then
        call gcnco2(newnom)
        chamno(10:16) = newnom(2:8)
        call corich('E', chamno, -2, ibid)
        call vtcreb(chamno, 'V', 'R',&
                    nume_ddlz = numedd,&
                    nb_equa_outz = neq)
        zk24(ilchno-1+1) = chamno
!
!
!     -- S'IL Y A DES CHARGES CINEMATIQUES :
    else
!
        ichci = 0
        call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
        call wkvect(vci2//'.DLCI', 'V V I', neq, jdlci2)
        do ichar = 1, nchtot
            charge=zk24(idchar-1+ichar)(1:8)
            icine = zi(jinfc+ichar)
            if (icine .lt. 0) then
                ichci = ichci + 1
                call gcnco2(newnom)
                chamno(10:16) = newnom(2:8)
                call corich('E', chamno, ichar, ibid)
                zk24(ilchno-1+ichci) = chamno
                call calvci(chamno, numedd, 1, charge, inst, 'V')
                call jeveuo(chamno//'.DLCI', 'L', vi=dlci)
!           --- COMBINAISON DES DLCI (OBJET CONTENANT DES 0 OU DES 1),
!           --- LES 1 ETANT POUR LES DDL CONTRAINT
!           --- LE RESTE DE L OBJECT VCI2 EST CREE PAR ASCOVA
                do ieq = 1, neq
!             -- ON REGARDE SI UN DDL N'EST PAS ELIMINE PLUSIEURS FOIS:
                    if (zi(jdlci2-1+ieq) .gt. 0 .and. dlci(ieq) .gt. 0) ieqmul=ieq
!
                    zi(jdlci2-1+ieq)=max(zi(jdlci2-1+ieq),dlci(&
                    ieq))
                end do
            endif
        end do
        call jedetr(chamno//'.DLCI')
    endif
!
!     -- SI UN DDL A ETE ELIMINE PLUSIEURS FOIS :
    if (ieqmul .gt. 0) then
        call utmess('A', 'CALCULEL3_37')
        call rgndas(numedd, ieqmul, l_print = .true.,&
                    type_equaz = tyddl)
        ASSERT(tyddl.eq.'A')
    endif
!
!
!
!     -- ON COMBINE LES CHAMPS CALCULES :
    call ascova('D', vachci, fomult, 'INST', inst,&
                'R', vci2)
!
!     --SI ON A PAS DE CHARGE CINEMATIQUE, IL FAUT QUAND MEME
!        FAIRE LE MENAGE
    if (nchci .eq. 0) call detrsd('CHAMP_GD', chamno(1:19))
    call jedetr(charci)
!
    call jedema()
end subroutine
