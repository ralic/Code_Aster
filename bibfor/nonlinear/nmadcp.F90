subroutine nmadcp(sddisc, ds_contact, i_event_acti, retpen)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmco.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utdidt.h"
#include "asterfort/utmess.h"
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
    integer, intent(in) :: i_event_acti
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: sddisc
    integer, intent(out) :: retpen
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! GESTION DE L'ACTION ADAPTATION COEF. PENALISATION
!
! ----------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! In  sddisc           : datastructure for time discretization
! IN  i_event_acti     : INDICE DE L'EVENEMENT ACTIF
! OUT RETPEN : CODE RETOUR ADAPTATION PENALISATION
!               0 ON N'A PAS ADAPTE
!               1 ON A ADAPTE
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: pene_maxi, coefpn, newcoe
    real(kind=8) :: coef_maxi
    real(kind=8) :: jeumin, jeumax, jeufin
    integer :: nbliai, nb_cont_zone
    integer :: iliai, i_zone
    character(len=24) :: jeuite, numlia
    integer :: jjeuit, jnumli
    character(len=24) :: ctevpe
    integer :: jctevp
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    retpen = 1
    call utdidt('L', sddisc, 'ECHE', 'PENE_MAXI', index_ = i_event_acti,&
                valr_ = pene_maxi)
    call utdidt('L', sddisc, 'ECHE', 'COEF_MAXI', index_ = i_event_acti,&
                valr_ = coef_maxi)
!
! - Get contact parameters
!
    nbliai       = cfdisd(ds_contact%sdcont_solv,'NBLIAI')
    nb_cont_zone = cfdisi(ds_contact%sdcont_defi,'NZOCO' )
!
! --- ACCES OBJETS DU CONTACT
!
    jeuite = ds_contact%sdcont_solv(1:14)//'.JEUITE'
    numlia = ds_contact%sdcont_solv(1:14)//'.NUMLIA'
    call jeveuo(jeuite, 'L', jjeuit)
    call jeveuo(numlia, 'L', jnumli)
    ctevpe = ds_contact%sdcont_solv(1:14)//'.EVENPE'
    call jeveuo(ctevpe, 'E', jctevp)
!
! --- DETECTION PENETRATION MAXIMUM/MINIMUM
!
    do iliai = 1, nbliai
        jeufin = zr(jjeuit+3*(iliai-1)+1-1)
        i_zone = zi(jnumli+4*(iliai-1)+4-1)
        jeumin = zr(jctevp+3*(i_zone-1)+1-1)
        jeumax = zr(jctevp+3*(i_zone-1)+2-1)
        if (jeufin .le. 0.d0) then
            jeufin = abs(jeufin)
            jeumax = max(jeumax,jeufin)
        else
            jeumin = max(jeumin,jeufin)
        endif
        zr(jctevp+3*(i_zone-1)+1-1) = jeumin
        zr(jctevp+3*(i_zone-1)+2-1) = jeumax
        zr(jctevp+3*(i_zone-1)+3-1) = jeufin
    end do
!
! --- DETECTION PENETRATION MAXIMUM
!
    do i_zone = 1, nb_cont_zone
        call cfmmco(ds_contact, i_zone, 'E_N', 'L', coefpn)
        if (jeumax .gt. pene_maxi) then
            newcoe = coefpn*2.d0
            if (newcoe .gt. coef_maxi) then
                newcoe = coef_maxi
                retpen = 0
            endif
            call cfmmco(ds_contact, i_zone, 'E_N', 'E', newcoe)
        endif
        if (retpen .eq. 1) then
            call utmess('I', 'MECANONLINE10_46', si=i_zone, sr=newcoe)
        endif
    end do
!
! --- AFFICHAGE
!
    if (retpen .eq. 0) then
        call utmess('I', 'MECANONLINE10_44')
    else if (retpen.eq.1) then
        call utmess('I', 'MECANONLINE10_45')
    else
        ASSERT(.false.)
    endif
!
    call jedema()
end subroutine
