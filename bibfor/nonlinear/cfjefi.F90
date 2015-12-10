subroutine cfjefi(noma, ds_contact, ddepla)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/caladu.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfimp1.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: noma
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: ddepla
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - ALGORITHME)
!
! CALCUL DES JEUX FINAUX
!
! ----------------------------------------------------------------------
!
! IN  NOMA   : NOM DU MAILLAGE
! In  ds_contact       : datastructure for contact management
! IN  DDEPLA : INCREMENT DE DEPLACEMENT DEPUIS L'ITERATION
!              DE NEWTON PRECEDENTE CORRIGEE PAR LE CONTACT
!
    integer :: ifm, niv
    integer :: iliai, jdecal, nbddl
    real(kind=8) :: jeuini, jeuold, jeuinc
    real(kind=8) :: jexnew, jexold, jexinc
    aster_logical :: lpenac, llagrf, lctfd
    character(len=24) :: apcoef, apddl, appoin
    integer :: japcoe, japddl, japptr
    character(len=24) :: apcofr
    integer :: japcof
    character(len=24) :: jeuite, jeux
    integer :: jjeuit, jjeux
    integer :: nbliai, neq, ndimg
    real(kind=8), pointer :: vale(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... CALCUL DES JEUX FINAUX'
    endif
!
! --- PARAMETRES
!
    nbliai = cfdisd(ds_contact%sdcont_solv,'NBLIAI' )
    neq = cfdisd(ds_contact%sdcont_solv,'NEQ' )
    ndimg = cfdisd(ds_contact%sdcont_solv,'NDIM' )
    lpenac = cfdisl(ds_contact%sdcont_defi,'CONT_PENA' )
    llagrf = cfdisl(ds_contact%sdcont_defi,'FROT_LAGR' )
    lctfd = cfdisl(ds_contact%sdcont_defi,'FROT_DISCRET')
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    appoin = ds_contact%sdcont_solv(1:14)//'.APPOIN'
    apddl = ds_contact%sdcont_solv(1:14)//'.APDDL'
    apcoef = ds_contact%sdcont_solv(1:14)//'.APCOEF'
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(apcoef, 'L', japcoe)
!
    if (lctfd) then
        apcofr = ds_contact%sdcont_solv(1:14)//'.APCOFR'
        call jeveuo(apcofr, 'L', japcof)
    endif
!
    jeuite = ds_contact%sdcont_solv(1:14)//'.JEUITE'
    jeux = ds_contact%sdcont_solv(1:14)//'.JEUX'
    call jeveuo(jeux, 'L', jjeux)
    call jeveuo(jeuite, 'E', jjeuit)
!
! --- ACCES VECTEUR DEPLACEMENTS
!
    call jeveuo(ddepla(1:19)//'.VALE', 'L', vr=vale)
!
! --- MISE A JOUR DES JEUX
!
    do iliai = 1, nbliai
        jeuini = zr(jjeux+3*(iliai-1)+1-1)
        if (lpenac) then
            zr(jjeuit+3*(iliai-1)+1-1) = jeuini
        else
            jdecal = zi(japptr+iliai-1)
            nbddl = zi(japptr+iliai) - zi(japptr+iliai-1)
            call caladu(neq, nbddl, zr(japcoe+jdecal), zi(japddl+jdecal), vale,&
                        jeuinc)
            jeuold = zr(jjeuit+3*(iliai-1)+1-1)
            zr(jjeuit+3*(iliai-1)+1-1) = jeuold - jeuinc
            if (llagrf .and. ndimg .eq. 2) then
                jexold = zr(jjeuit+3*(iliai-1)+2-1)
                call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+ jdecal), vale,&
                            jexinc)
                jexnew = jexold + jexinc
                zr(jjeuit+3*(iliai-1)+2-1) = jexnew
            endif
        endif
    end do
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        call cfimp1('FIN', noma, ds_contact%sdcont_defi, ds_contact%sdcont_solv, ifm)
    endif
!
    call jedema()
!
end subroutine
