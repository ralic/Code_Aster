subroutine nmadev(sddisc, sderro, iterat)
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
    implicit none
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmerge.h"
#include "asterfort/nmlecv.h"
#include "asterfort/nmltev.h"
#include "asterfort/utdidt.h"
    character(len=19) :: sddisc
    character(len=24) :: sderro
    integer :: iterat
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! MISE A JOUR DE L'INDICATEUR DE SUCCES DES ITERATIONS DE NEWTON
!
! ----------------------------------------------------------------------
!
! In  sddisc           : datastructure for time discretization
! IN  SDERRO : GESTION DES ERREURS
! IN  ITERAT : NUMERO DE L'ITERATION DE NEWTON
!
! ----------------------------------------------------------------------
!
    integer :: vali, nb_event_ok
    integer :: i_adapt, nb_adapt
    real(kind=8) :: vale
    character(len=8) :: cricom, metlis
    character(len=16) :: nopara
    character(len=19) :: event_name
    aster_logical :: itemax, lerrit, divres, cvnewt
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LA MISE A JOUR DE L'INDICATEUR DE SUCCES DES ITERATIONS DE NEWTON
! --- N'EST FAITE QU'EN GESTION AUTO DU PAS DE TEMPS
!
    call utdidt('L', sddisc, 'LIST', 'METHODE',&
                valk_ = metlis)
    if (metlis .ne. 'AUTO') goto 999
!
! --- EVENEMENTS
!
    call nmerge(sderro, 'ITER_MAXI', itemax)
    call nmltev(sderro, 'ERRI', 'NEWT', lerrit)
    call nmerge(sderro, 'DIVE_RESI', divres)
!
! --- NEWTON A CONVERGE ?
!
    call nmlecv(sderro, 'NEWT', cvnewt)
!
! --- BOUCLE SUR LES OCCURENCES D'ADAPTATION
!
    call utdidt('L', sddisc, 'LIST', 'NADAPT',&
                vali_ = nb_adapt)
!
    do i_adapt = 1, nb_adapt
!
! ----- NOM DE L'EVENEMENT
!
        call utdidt('L', sddisc, 'ADAP', 'NOM_EVEN', index_ = i_adapt, &
                    valk_ = event_name)
!
        if (event_name .eq. 'SEUIL_SANS_FORMU') then
!
! ------- PARAMETRES DU SEUIL
!
            call utdidt('L', sddisc, 'ADAP', 'NOM_PARA', index_ = i_adapt, &
                        valk_ = nopara)
            call utdidt('L', sddisc, 'ADAP', 'CRIT_COMP', index_ = i_adapt, &
                        valk_ = cricom)
            call utdidt('L', sddisc, 'ADAP', 'VALE', index_ = i_adapt, &
                        valr_ = vale, vali_ = vali)
!
            ASSERT(nopara.eq.'NB_ITER_NEWT')
!
! ------- RECUP DU NB DE SUCCES CONSECUTIFS : NBOK
!
            call utdidt('L', sddisc, 'ADAP', 'NB_EVEN_OK', index_ = i_adapt, &
                        vali_ = nb_event_ok)
!
! ------- EN CAS DE NOUVEAU SUCCES A CONVERGENCE
!
            if (cvnewt) then
                if (cricom .eq. 'LT' .and. iterat .lt. vali .or. cricom .eq. 'GT' .and.&
                    iterat .gt. vali .or. cricom .eq. 'LE' .and. iterat .le. vali .or.&
                    cricom .eq. 'GE' .and. iterat .ge. vali) then
                    nb_event_ok = nb_event_ok+1
                endif
            endif
!
! ------- EN CAS D'ECHEC: ON REMET A ZERO
!
            if (lerrit .or. itemax .or. divres) nb_event_ok=0
!
! ------- ENREGISTREMENT DU NB DE SUCCES CONSECUTIFS
!
            call utdidt('E', sddisc, 'ADAP', 'NB_EVEN_OK', index_ = i_adapt, &
                        vali_ = nb_event_ok)
!
        endif
    end do
!
999 continue
!
    call jedema()
end subroutine
