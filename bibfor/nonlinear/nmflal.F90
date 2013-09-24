subroutine nmflal(option, compor, sdpost, mod45, defo,&
                  nfreq, cdsp, typmat, optmod, bande,&
                  nddle, ddlexc, nsta, ddlsta, modrig)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmlesd.h"
#include "asterfort/utmess.h"
    character(len=24) :: compor, ddlexc, ddlsta
    character(len=16) :: optmod, option
    character(len=4) :: mod45
    integer :: nfreq, defo, nddle, nsta, cdsp
    character(len=16) :: typmat, modrig
    real(kind=8) :: bande(2)
    character(len=19) :: sdpost
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - CALCUL DE MODES)
!
! LECTURE DES OPTIONS DANS MECA_NON_LINE
!
! ----------------------------------------------------------------------
!
!
! IN  OPTION : TYPE DE CALCUL
!              'FLAMBSTA' MODES DE FLAMBEMENT EN STATIQUE
!              'FLAMBDYN' MODES DE FLAMBEMENT EN DYNAMIQUE
!              'VIBRDYNA' MODES VIBRATOIRES
! IN  COMPOR : CARTE COMPORTEMENT
! IN  SDPOST : SD POUR POST-TRAITEMENTS (CRIT_STAB ET MODE_VIBR)
! OUT MOD45  : TYPE DE CALCUL DE MODES PROPRES
!              'VIBR'     MODES VIBRATOIRES
!              'FLAM'     MODES DE FLAMBEMENT
!              'STAB'     MODE DE STABILITE
! OUT NFREQ  : NOMBRE DE FREQUENCES A CALCULER
! OUT CDSP   : COEFFICIENT MULTIPLICATEUR DE NFREQ -> DIM_SPACE
! OUT TYPMAT : TYPE DE MATRICE A UTILISER
!                'ELASTIQUE/TANGENTE/SECANTE'
! OUT OPTMOD : OPTION DE RECHERCHE DE MODES
!               'PLUS_PETITE' LA PLUS PETITE FREQUENCE
!               'BANDE'       DANS UNE BANDE DE FREQUENCE DONNEE
! OUT DEFO   : TYPE DE DEFORMATIONS
!                0            PETITES DEFORMATIONS (MATR. GEOM.)
!                1            GRANDES DEFORMATIONS (PAS DE MATR. GEOM.)
! OUT BANDE  : BANDE DE FREQUENCE SI OPTMOD='BANDE'
! OUT NDDLE  : NOMBRE DE DDL EXCLUS
! OUT DDLEXC : NOM DE L'OBJET JEVEUX CONTENANT LE NOM DES DDLS EXCLUS
! OUT NSTA   : NOMBRE DE DDL STAB
! OUT DDLSTA : NOM DE L'OBJET JEVEUX CONTENANT LE NOM DES DDLS STAB
!
!
!
!
    integer :: nbv, i, ibid
    real(kind=8) :: r8bid
    character(len=24) :: k24bid
    integer :: init, ides
    character(len=16) :: optrig
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    bande(1) = 1.d-5
    bande(2) = 1.d5
    nfreq = 0
    cdsp = 0
    nddle = 0
    defo = 0
    mod45 = ' '
    optmod = ' '
    optrig = ' '
    typmat = ' '
    nsta = 0
!
! --- TYPE DE DEFORMATIONS
!
    call jeveuo(compor(1:19)//'.VALE', 'L', init)
    call jeveuo(compor(1:19)//'.DESC', 'L', ides)
    nbv = zi(ides-1+3)
!     RIGIDITE GEOMETRIQUE INTEGREE A LA MATRICE TANGENTE
    do 10 i = 1, nbv
        if ((zk16(init+2+20*(i-1)).eq.'GROT_GDEP') .or.&
            (zk16(init+2+20*(i-1)).eq.'GDEF_HYPO_ELAS') .or.&
            (zk16(init+2+20*(i-1)).eq.'SIMO_MIEHE') .or.&
            (zk16(init+2+20*(i-1)).eq.'GDEF_LOG')) then
            defo = 1
        endif
10  enddo
!
! --- RECUPERATION DES OPTIONS
!
    if (option(1:7) .eq. 'VIBRDYN') then
        call nmlesd('POST_TRAITEMENT', sdpost, 'NB_FREQ_VIBR', nfreq, r8bid,&
                    k24bid)
        call nmlesd('POST_TRAITEMENT', sdpost, 'COEF_DIM_VIBR', cdsp, r8bid,&
                    k24bid)
        call nmlesd('POST_TRAITEMENT', sdpost, 'TYPE_MATR_VIBR', ibid, r8bid,&
                    typmat)
        call nmlesd('POST_TRAITEMENT', sdpost, 'OPTION_EXTR_VIBR', ibid, r8bid,&
                    optmod)
        call nmlesd('POST_TRAITEMENT', sdpost, 'BANDE_VIBR_1', ibid, bande(1),&
                    k24bid)
        call nmlesd('POST_TRAITEMENT', sdpost, 'BANDE_VIBR_2', ibid, bande(2),&
                    k24bid)
        mod45 = 'VIBR'
    else if (option(1:5) .eq. 'FLAMB') then
        call nmlesd('POST_TRAITEMENT', sdpost, 'NB_FREQ_FLAMB', nfreq, r8bid,&
                    k24bid)
        call nmlesd('POST_TRAITEMENT', sdpost, 'COEF_DIM_FLAMB', cdsp, r8bid,&
                    k24bid)
        call nmlesd('POST_TRAITEMENT', sdpost, 'TYPE_MATR_FLAMB', ibid, r8bid,&
                    typmat)
        mod45 = 'FLAM'
        call nmlesd('POST_TRAITEMENT', sdpost, 'RIGI_GEOM_FLAMB', ibid, r8bid,&
                    optrig)
        if (optrig .eq. 'RIGI_GEOM_NON') then
            defo = 1
            call utmess('I', 'MECANONLINE4_3')
        endif
!
        if (defo .eq. 0) then
            optmod = 'BANDE'
            call nmlesd('POST_TRAITEMENT', sdpost, 'OPTION_EXTR_FLAM', ibid, r8bid,&
                        optmod)
        else if (defo.eq.1) then
            optmod = 'PLUS_PETITE'
            call nmlesd('POST_TRAITEMENT', sdpost, 'OPTION_EXTR_FLAM', ibid, r8bid,&
                        optmod)
        else
            ASSERT(.false.)
        endif
!
        call nmlesd('POST_TRAITEMENT', sdpost, 'BANDE_FLAMB_1', ibid, bande(1),&
                    k24bid)
        call nmlesd('POST_TRAITEMENT', sdpost, 'BANDE_FLAMB_2', ibid, bande(2),&
                    k24bid)
        call nmlesd('POST_TRAITEMENT', sdpost, 'NB_DDL_EXCLUS', nddle, r8bid,&
                    k24bid)
        call nmlesd('POST_TRAITEMENT', sdpost, 'NOM_DDL_EXCLUS', ibid, r8bid,&
                    ddlexc)
        call nmlesd('POST_TRAITEMENT', sdpost, 'NB_DDL_STAB', nsta, r8bid,&
                    k24bid)
        call nmlesd('POST_TRAITEMENT', sdpost, 'NOM_DDL_STAB', ibid, r8bid,&
                    ddlsta)
        call nmlesd('POST_TRAITEMENT', sdpost, 'MODI_RIGI', ibid, r8bid,&
                    modrig)
    else
        ASSERT(.false.)
    endif
!
    call jedema()
!
end subroutine
