subroutine alchlo(opt, ligrel, nin, lpain, nout, lpaout)
use calcul_module, only : ca_iaobtr_, ca_iaoppa_, ca_iawlo2_, ca_iawloc_,&
                          ca_iawtyp_, ca_igr_, ca_nbgr_, ca_nbobtr_,&
                          ca_npario_
implicit none

! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!     arguments:
!     ----------
#include "jeveux.h"

#include "asterc/indik8.h"
#include "asterfort/dchlmx.h"
#include "asterfort/grdeur.h"
#include "asterfort/mecoe1.h"
#include "asterfort/scalai.h"
#include "asterfort/typele.h"
#include "asterfort/wkvect.h"
    integer :: opt, nin, nout
    character(len=8) :: lpain(nin), lpaout(nout)
    character(len=19) :: ligrel
! ----------------------------------------------------------------------
!     entrees:
!      opt : option
!     ligrel : nom de ligrel

!     sorties:
!     creation des champs locaux de noms &&calcul.nompar(opt)
!     le champ local est une zone memoire temporaire a la routine calcul
!     qui contiendra les  valeurs des champs "bien rangees"
!     (pour les te00ij) de tous les elements d'un grel.
!-----------------------------------------------------------------------
    integer :: iparg, taille, gd
    integer ::  iparin, iparou, nute
    character(len=24) :: nochl, nochl2
    character(len=8) :: nompar
    character(len=8) :: scal
!-----------------------------------------------------------------------

    call wkvect('&&CALCUL.IA_CHLOC', 'V V I', 3*ca_npario_, ca_iawloc_)
    call wkvect('&&CALCUL.IA_CHLO2', 'V V I', 5*ca_npario_*ca_nbgr_, ca_iawlo2_)
    call wkvect('&&CALCUL.TYPE_SCA', 'V V K8', ca_npario_, ca_iawtyp_)
    ca_nbobtr_ = ca_nbobtr_ + 1
    zk24(ca_iaobtr_-1+ca_nbobtr_) = '&&CALCUL.IA_CHLOC'
    ca_nbobtr_ = ca_nbobtr_ + 1
    zk24(ca_iaobtr_-1+ca_nbobtr_) = '&&CALCUL.IA_CHLO2'
    ca_nbobtr_ = ca_nbobtr_ + 1
    zk24(ca_iaobtr_-1+ca_nbobtr_) = '&&CALCUL.TYPE_SCA'


!   -- initialisation de '&&CALCUL.IA_CHLO2':
    do ca_igr_ = 1, ca_nbgr_
        nute=typele(ligrel,ca_igr_,1)
        call mecoe1(opt, nute)
    enddo


    do iparg = 1, ca_npario_
        nompar = zk8(ca_iaoppa_-1+iparg)
        nochl = '&&CALCUL.'//nompar
        nochl2= '&&CALCUL.'//nompar//'.EXIS'
        zi(ca_iawloc_-1+3*(iparg-1)+1)=-1
        zi(ca_iawloc_-1+3*(iparg-1)+2)=-1

!       Si le parametre n'est associe a aucun champ, on passe :
!       --------------------------------------------------------
        iparin = indik8(lpain,nompar,1,nin)
        iparou = indik8(lpaout,nompar,1,nout)
        zi(ca_iawloc_-1+3*(iparg-1)+3)=iparin+iparou
        if ((iparin+iparou) .eq. 0) cycle

        gd = grdeur(nompar)
        scal = scalai(gd)
        zk8(ca_iawtyp_-1+iparg) = scal
        call dchlmx(opt, ligrel, iparg, nin, lpain,&
                    nout, lpaout, taille)
        if (taille .ne. 0) then
            call wkvect(nochl, 'V V '//scal(1:4), taille, zi(ca_iawloc_-1+3* (iparg-1)+1))
            ca_nbobtr_ = ca_nbobtr_ + 1
            zk24(ca_iaobtr_-1+ca_nbobtr_) = nochl
            if (iparin .gt. 0) then
                call wkvect(nochl2, 'V V L', taille, zi(ca_iawloc_-1+3*( iparg-1)+2))
                ca_nbobtr_ = ca_nbobtr_ + 1
                zk24(ca_iaobtr_-1+ca_nbobtr_) = nochl2
            endif
        else
            zi(ca_iawloc_-1+3*(iparg-1)+1)=-2
        endif
    end do

end subroutine
