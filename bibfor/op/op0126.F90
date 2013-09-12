subroutine op0126()
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
!***********************************************************************
    implicit none
!  P. RICHARD     DATE 13/07/90
!-----------------------------------------------------------------------
!  BUT: TRAITER LA DEFINITION DU MODELE GENERALISE DONNE PAR
!       L'UTILISATEUR ET TRAITER L'ORIENTATION DES MATRICES DE LIAISON
!       PROCEDER AUX VERIFICATIONS SUR LA COHERENCE DE LA DEFINITION
!       DES LIAISONS ET SUR LA COMPATIBILITE DES MACR_ELEM MIS EN JEU
!
!  CONCEPT CREE: MODE_GENE
!
!-----------------------------------------------------------------------
!
!
!
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/arg126.h"
#include "asterfort/callis.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/vecomo.h"
#include "asterfort/versst.h"
    integer :: ival, ibid, nblia, i, iinc, irep11, irep12, irep21, irep22, iopt
    integer :: iret
    character(len=3) :: rep
    character(len=8) :: nomres, sst1, sst2, intf1, intf2, k8bid, option
    character(len=16) :: nomcon, nomope
    integer :: iarg
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
    call infmaj()
    call getres(nomres, nomcon, nomope)
!
!-----TRAITEMENT DES DONNEES UTILISATEUR
!
    call arg126(nomres)
!
!-----VERIFICATION COHERENCE DES SOUS-STRUCTURES ET CREATION DU .DESC
!
    call versst(nomres)
!
!
!-----VERIFICATION DE LA COHERENCE DU MODELE GENERALISE
!
    call getfac('VERIF', ival)
    if (ival .ne. 0) then
        call getvtx('VERIF', 'STOP_ERREUR', iocc=1, scal=rep, nbret=ibid)
        if (rep .eq. 'NON') goto 20
    endif
!
    call getfac('LIAISON', nblia)
!
    do 10 i = 1, nblia
        call getvtx('LIAISON', 'OPTION', iocc=i, scal=option, nbret=iopt)
        call getvtx('LIAISON', 'SOUS_STRUC_1', iocc=i, scal=sst1, nbret=ibid)
        call getvtx('LIAISON', 'SOUS_STRUC_2', iocc=i, scal=sst2, nbret=ibid)
        call getvtx('LIAISON', 'INTERFACE_1', iocc=i, scal=intf1, nbret=ibid)
        call getvtx('LIAISON', 'INTERFACE_2', iocc=i, scal=intf2, nbret=ibid)
        iinc=0
!     ON TESTE SI LA LIAISON EST INCOMPATIBLE
        call getvtx('LIAISON', 'GROUP_MA_MAIT_1', iocc=i, scal=k8bid, nbret=irep11)
        call getvtx('LIAISON', 'MAILLE_MAIT_1', iocc=i, scal=k8bid, nbret=irep12)
        call getvtx('LIAISON', 'GROUP_MA_MAIT_2', iocc=i, scal=k8bid, nbret=irep21)
        call getvtx('LIAISON', 'MAILLE_MAIT_2', iocc=i, scal=k8bid, nbret=irep22)
        if ((irep11.ne.0) .or. (irep12.ne.0)) then
            iinc=1
        else if ((irep21.ne.0).or.(irep22.ne.0)) then
            iinc=2
        endif
!
!       SI ELLE EST COMPATIBLE ON VERIFIE LA COINCIDENCE DES NOEUDS
!       D'INTERFACE, SINON ON FAIT RIEN
        if ((iinc.eq.0) .and. (option.eq.'CLASSIQU')) then
            iret=i
            call vecomo(nomres, sst1, sst2, intf1, intf2,&
                        iret, option)
        endif
10  end do
20  continue
!
!
!-----ORIENTATION DES MATRICES DE LIAISON
!
    call callis(nomres)
!
!-----VERIFICATION DU MODELE GENERALISE
!
!
end subroutine
