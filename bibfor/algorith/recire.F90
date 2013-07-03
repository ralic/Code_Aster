subroutine recire(typopt, iderre, frexci, fremin, fremax,&
                  pas, nbptmd)
    implicit   none
#include "asterc/getfac.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
    integer :: iderre, nbptmd
    real(kind=8) :: fremin, fremax, pas
    character(len=4) :: typopt, frexci
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!  BUT: RECUPERER LES INFORMATIONS DU MOT CLE FACTEUR REPONSE POUR
!        LE CALCUL DYNAMIQUE ALEATOIRE
!
! OUT : TYPOPT : OPTION 'DIAG' OU 'TOUT'
! OUT : IDERRE : ORDRE DE DERIVATION
! OUT : FREXCI : FREQUENCE DE L EXCITATION: AVEC OU SANS
! OUT : FREMIN : FREQ MIN DE LA DISCRETISATION
! OUT : FREMAX : FREQ MAX DE LA DISCRETISATION
! OUT : PAS    : PAS DE LA DISCRETISATION
! OUT : NBPTMD : NOMBRE DE POINTS PAR MODES
!
!-----------------------------------------------------------------------
    integer :: ibid, nbocc
    integer :: iarg
!-----------------------------------------------------------------------
!
    typopt = 'TOUT'
    iderre = 0
    frexci = 'AVEC'
    fremin = -1.d0
    fremax = -1.d0
    pas = -1.d0
    nbptmd = 50
!
    call getfac('REPONSE', nbocc)
!
    if (nbocc .ne. 0) then
!
!----TYPE DE REPONSE ET RELATIF/ABSOLU ET MONO/INTER
!
        call getvtx('REPONSE', 'OPTION', 1, iarg, 1,&
                    typopt, ibid)
        call getvis('REPONSE', 'DERIVATION', 1, iarg, 1,&
                    iderre, ibid)
!
!----INCLUSION DES FREQUENCES DEXCITATION DANS LA DISCRETISATION REPONSE
!
        call getvtx('REPONSE', 'FREQ_EXCIT', 1, iarg, 1,&
                    frexci, ibid)
!-
!----FREQUENCE INITIALE
!
        call getvr8('REPONSE', 'FREQ_MIN', 1, iarg, 1,&
                    fremin, ibid)
        if (ibid .ne. 0) frexci = 'SANS'
!
!----FREQUENCE FINALE
!
        call getvr8('REPONSE', 'FREQ_MAX', 1, iarg, 1,&
                    fremax, ibid)
!
!----PAS DE LA DISCRETISATION
!
        call getvr8('REPONSE', 'PAS', 1, iarg, 1,&
                    pas, ibid)
!
!----NOMBRE DE POINTS PAR MODES
!
        call getvis('REPONSE', 'NB_POIN_MODE', 1, iarg, 1,&
                    nbptmd, ibid)
!
    endif
!
end subroutine
