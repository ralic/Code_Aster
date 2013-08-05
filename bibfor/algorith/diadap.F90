function diadap(sddisc, iadapt)
!
! person_in_charge: samuel.geniaut at edf.fr
!
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
    implicit none
    logical :: diadap
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utdidt.h"
    integer :: iadapt
    character(len=19) :: sddisc
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! ACCES AU DECLENCHEUR DE L'ADAPTATION DU PAS DE TEMPS
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION
! IN  IADAPT : NUMERO DE LA METHODE D ADAPTATION TRAITEE
! OUT DIADAP : .TRUE. SI ON DOIT ADAPTER LE PAS DE TEMPS
!
!
!
!
!
    real(kind=8) :: r8bid
    integer :: ibid, nbinse, nbok
    character(len=8) :: k8bid
    character(len=19) :: even
    character(len=24) :: tpsite
    integer :: jiter
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD
!
    tpsite = sddisc(1:19)//'.ITER'
    call jeveuo(tpsite, 'L', jiter)
!
! --- NOM DE L'EVENEMENT
!
    call utdidt('L', sddisc, 'ADAP', iadapt, 'NOM_EVEN',&
                r8bid, ibid, even)
!
    if (even .eq. 'AUCUN') then
!
        diadap = .false.
!
    else if (even.eq.'TOUT_INST') then
!
        diadap = .true.
!
    else if (even.eq.'SEUIL_SANS_FORMULE') then
!
! ----- RECUP DU SEUIL SUR LE NB DE SUCCES CONSECUTIFS
!
        call utdidt('L', sddisc, 'ADAP', iadapt, 'NB_INCR_SEUIL',&
                    r8bid, nbinse, k8bid)
!
! ----- RECUP DU NB DE SUCCES CONSECUTIFS
!
        call utdidt('L', sddisc, 'ADAP', iadapt, 'NB_EVEN_OK',&
                    r8bid, nbok, k8bid)
!
        if (nbok .lt. nbinse) then
            diadap = .false.
        else
!         ICI NBOK EST NORMALEMENT EGAL A NBINSE
!         MAIS NBOK PEUT ETRE AUSSI SUPERIEUR A NBINSE SI ON UTILISE
!         LA METHODE CONTINUE
            diadap = .true.
!         REMISE A ZERO DE NBOK
            nbok = 0
            call utdidt('E', sddisc, 'ADAP', iadapt, 'NB_EVEN_OK',&
                        r8bid, nbok, k8bid)
        endif
!
    else if (even.eq.'SEUIL_AVEC_FORMULE') then
!
        ASSERT(.false.)
!
    endif
!
    call jedema()
end function
