subroutine nbptca(ligrel, option, param, obnbpt, obnbno)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "jeveux.h"
#include "asterfort/alchml.h"
#include "asterfort/celces.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: ligrel, option, param, obnbpt, obnbno
! ------------------------------------------------------------------
! But: Creer l'objet obnbpt qui contiendra le nombre de points
!      de discretisation (pour les mailles d'un ligrel)
!      pour le cham_elem associe a un parametre d'une option.
!      Calcule egalement un objet contenant le nombre de noeuds des
!      mailles du ligrel.
! ------------------------------------------------------------------
!     arguments:
! ligrel  in/jxin  k19 : ligrel
! option  in       k16 : nom d'une option de calcul
! param   in       k8  : nom d'un parametre de option
! obnbpt  in/jxout k24 : objet qui contiendra les nombres de points
! obnbno  in/jxout k24 : objet qui contiendra les nombres de noeuds
! ------------------------------------------------------------------
! remarques :
!  cette routine peut etre utilisee par exemple pour determiner les
!  nombre de points de gauss d'un modele mecanique non-lineaire:
!  option = 'raph_meca' + param='pcontmr'
!
!  l'objet cree est un vecteur d'entiers dimensionne au nombre de
!  mailles du maillage : v(ima) : nbpt(maille_ima)
!  les mailles tardives sont ignorees.
!-----------------------------------------------------------------------
    integer :: iret, nbma, ima,  jnbpt, jnbno, iacnx1, ilcnx1, nbno
    character(len=8) :: ma
    character(len=19) :: cel, ces
    integer, pointer :: cesd(:) => null()
!------------------------------------------------------------------
    call jemarq()
    cel = '&&NBPTCA.CEL'
    ces = '&&NBPTCA.CES'

    call dismoi('NOM_MAILLA', ligrel, 'LIGREL', repk=ma)
    call dismoi('NB_MA_MAILLA', ma, 'MAILLAGE', repi=nbma)
    call wkvect(obnbpt, 'V V I', nbma, jnbpt)
    call wkvect(obnbno, 'V V I', nbma, jnbno)
    call jeveuo(ma//'.CONNEX', 'L', iacnx1)
    call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', ilcnx1)

    call alchml(ligrel, option, param, 'V', cel,&
                iret, ' ')
    if (iret .ne. 0) then
!       - IL N'Y A RIEN A FAIRE : NBPT(IMA)=0
    else
        call celces(cel, 'V', ces)
        call jeveuo(ces//'.CESD', 'L', vi=cesd)
        do ima = 1, nbma
            zi(jnbpt-1+ima) = cesd(5+4* (ima-1)+1)
            nbno = zi(ilcnx1+ima) - zi(ilcnx1-1+ima)
            zi(jnbno-1+ima) = nbno
        end do

    endif


    call detrsd('CHAM_ELEM', cel)
    call detrsd('CHAM_ELEM_S', ces)

    call jedema()
end subroutine
