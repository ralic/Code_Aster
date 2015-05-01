subroutine alrslt(iopt, ligrel, nout, lchout, lpaout,&
                  base, ldist)
use module_calcul, only : ca_evfini_, ca_iachoi_, ca_iachok_, ca_iaobtr_, ca_nbobtr_
implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ARGUMENTS:
!     ----------
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/alchml.h"
#include "asterfort/alresl.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/grdeur.h"
#include "asterfort/jeexin.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
!
    integer :: iopt, nout
    character(len=19) :: ligrel
    character(len=*) :: base, lchout(*)
    character(len=8) :: lpaout(*)
    aster_logical :: ldist
! ----------------------------------------------------------------------
!     ENTREES:
!      IOPT : OPTION
!     LIGREL : NOM DE LIGREL
!     LCHOUT : LISTE DES NOMS DES CHAMPS DE SORTIE
!     LPAOUT : LISTE DES PARAMETRES ASSOCIES AUX CHAMPS DE SORTIE
!       NOUT : NOMBRE DE CHAMPS DE SORTIE
!       BASE : 'G', 'V' OU 'L'
!     LDIST : CALCUL DISTRIBUE
!     SORTIES:
!      CREATION DES CHAMPS GLOBAUX RESULTATS
!
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: gd, descgd, code, i, iret1, iret2, iret
    character(len=19) :: nochou, dcel
    character(len=8) :: nompar
    character(len=8) :: nomgd, tsca, tych
    character(len=16) :: nomopt
    character(len=24), pointer :: noli(:) => null()
    character(len=24), pointer :: celk(:) => null()
!
!
    call jenuno(jexnum('&CATA.OP.NOMOPT', iopt), nomopt)
!
!
!     -- ALLOCATION DES CHAMPS RESULTATS :
    do i = 1, nout
        nompar = lpaout(i)
        nochou = lchout(i)
        gd = grdeur(nompar)
        call jeveuo(jexnum('&CATA.GD.DESCRIGD', gd), 'L', descgd)
        code = zi(descgd-1+1)
!
!        -- SI GD EST 1 GRANDEUR_SIMPLE --> CHAM_ELEM
        if (code .eq. 1) then
            call detrsd('CHAM_ELEM', nochou)
            call exisd('CHAM_ELEM_S', nochou, iret)
            if (iret .gt. 0) then
                dcel = nochou
            else
                dcel = ' '
            endif
            call alchml(ligrel, nomopt, nompar, base, nochou,&
                        iret, dcel)
!           -- les cham_elems sont incomplets si ldist
            if (ldist) then
                call jeveuo(nochou//'.CELK', 'E', vk24=celk)
                celk(7)='MPI_INCOMPLET'
            endif
!
        else
!        -- SINON --> RESUELEM
            call detrsd('RESUELEM', nochou)
            ASSERT((code.ge.3).and.(code.le.5))
            call alresl(iopt, ligrel, nochou, nompar, base)
!        -- LES RESU_ELEMS SONT INCOMPLETS EN LDIST
            if (ldist) then
                call jeveuo(nochou//'.NOLI', 'E', vk24=noli)
                noli(3)='MPI_INCOMPLET'
            endif
        endif
    end do
!
!
    call wkvect('&&CALCUL.LCHOU_I', 'V V I', max(3*nout, 3), ca_iachoi_)
    ca_nbobtr_ = ca_nbobtr_ + 1
    zk24(ca_iaobtr_-1+ca_nbobtr_) = '&&CALCUL.LCHOU_I'
    call wkvect('&&CALCUL.LCHOU_K8', 'V V K8', max(2*nout, 2), ca_iachok_)
    ca_nbobtr_ = ca_nbobtr_ + 1
    zk24(ca_iaobtr_-1+ca_nbobtr_) = '&&CALCUL.LCHOU_K8'
!
    do i = 1, nout
        nompar = lpaout(i)
        nochou = lchout(i)
        gd = grdeur(nompar)
        call jeveuo(jexnum('&CATA.GD.DESCRIGD', gd), 'L', descgd)
        code = zi(descgd-1+1)
        call jeexin(nochou//'.DESC', iret1)
        call jeexin(nochou//'.CELD', iret2)
        if ((iret1+iret2) .eq. 0) goto 30
!
        call dismoi('NOM_GD', nochou, 'CHAMP', repk=nomgd)
        call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
        zk8(ca_iachok_-1+2* (i-1)+2) = tsca
        call dismoi('TYPE_CHAMP', nochou, 'CHAMP', repk=tych)
!        -- si c'est un cham_elem:
!           on doit faire un jeveuo en ecriture pour recuperer
!           l'adresse du .celv
        if (tych(1:2) .eq. 'EL') then
            call jeveuo(nochou//'.CELD', 'E', zi(ca_iachoi_-1+3* (i-1)+1))
            call jeveuo(nochou//'.CELV', 'E', zi(ca_iachoi_-1+3* (i-1)+2))
            zk8(ca_iachok_-1+2* (i-1)+1) = 'CHML'
        else
            call jeveuo(nochou//'.DESC', 'E', zi(ca_iachoi_-1+3* (i-1)+1))
            if (ca_evfini_ .eq. 1 .and. code .gt. 3) then
                ASSERT(code.eq.5)
                call jeveuo(nochou//'.RSVI', 'L', zi(ca_iachoi_-1+3* (i-1)+ 2))
                call jeveuo(jexatr(nochou//'.RSVI', 'LONCUM'), 'L', zi(ca_iachoi_-1+3* (i-1)+3))
            endif
            zk8(ca_iachok_-1+2* (i-1)+1) = 'RESL'
        endif
 30     continue
    end do
!
end subroutine
