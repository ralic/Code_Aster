subroutine nmflam(option, modele, numedd, numfix     , carele,&
                  compor, numins, mate       , comref,&
                  lischa, ds_contact, ds_algopara, fonact,&
                  carcri, ds_measure, sddisc, sddyna,&
                  sdpost, valinc, solalg, meelem     , measse,&
                  veelem, sderro)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmcrel.h"
#include "asterfort/nmecsd.h"
#include "asterfort/nmflal.h"
#include "asterfort/nmflin.h"
#include "asterfort/nmflma.h"
#include "asterfort/nmlesd.h"
#include "asterfort/nmop45.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
! aslint: disable=W1504
!
    integer :: numins
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
    character(len=16) :: option
    character(len=19) :: meelem(*)
    type(NL_DS_Contact), intent(in) :: ds_contact
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=24) :: sderro
    character(len=19) :: lischa, sddisc, sddyna, sdpost
    character(len=24) :: modele, numedd, numfix, carele, compor
    character(len=19) :: veelem(*), measse(*)
    character(len=19) :: solalg(*), valinc(*)
    character(len=24) :: mate
    character(len=24) :: carcri, comref
    integer :: fonact(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CALCUL DE MODES
!
! ----------------------------------------------------------------------
!
! IN  OPTION : TYPE DE CALCUL
!              'FLAMBSTA' MODES DE FLAMBEMENT EN STATIQUE
!              'FLAMBDYN' MODES DE FLAMBEMENT EN DYNAMIQUE
!              'VIBRDYNA' MODES VIBRATOIRES
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARI_COM DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  LISCHA : LISTE DES CHARGES
! In  ds_contact       : datastructure for contact management
! IO  ds_measure       : datastructure for measure and statistics management
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! In  ds_algopara      : datastructure for algorithm parameters
! IN  SDPOST : SD POUR POST-TRAITEMENTS (CRIT_STAB ET MODE_VIBR)
! IN  CARCRI : PARAMETRES METHODES D'INTEGRATION LOCALES (VOIR NMLECT)
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO D'INSTANT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  MEELEM : MATRICES ELEMENTAIRES (POUR NMFLMA)
! IN  MEASSE : MATRICE ASSEMBLEE (POUR NMFLMA)
! IN  VEELEM : VECTEUR ELEMENTAIRE (POUR NMFLMA)
! IN  SDERRO : SD ERREUR
!
! ----------------------------------------------------------------------
!
    aster_logical :: linsta
    integer :: nfreq, nfreqc
    integer :: i, ljeveu, ibid, iret
    integer :: defo, ldccvg, numord
    integer :: nddle, nsta, ljeve2, cdsp
    real(kind=8) :: bande(2), r8bid
    real(kind=8) :: freqm, freqv, freqa, freqr
    real(kind=8) :: csta
    character(len=4) :: mod45
    character(len=8) :: sdmode, sdstab
    character(len=16) :: optmod, varacc, typmat, modrig
    character(len=19) :: matgeo, matas2, vecmod, champ
    character(len=19) :: champ2, vecmo2
    character(len=24) :: k24bid, ddlexc, ddlsta
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    matgeo = '&&NMFLAM.MAGEOM'
    matas2 = '&&NMFLAM.MATASS'
    linsta = .false.
!
! --- NOM DE LA SD DE STOCKAGE DES MODES
!
    sdmode = '&&NM45BI'
    sdstab = '&&NM45SI'
!
! --- RECUPERATION DES OPTIONS
!
    call nmflal(option, compor, sdpost, mod45, defo,&
                nfreq, cdsp, typmat, optmod, bande,&
                nddle, ddlexc, nsta, ddlsta, modrig)
!
! --- CALCUL DE LA MATRICE TANGENTE ASSEMBLEE ET DE LA MATRICE GEOM.
!
    call nmflma(typmat, mod45 , defo  , ds_algopara, modele,&
                mate  , carele, sddisc, sddyna     , fonact,&
                numins, valinc, solalg, lischa     , comref,&
                ds_contact, numedd     , numfix,&
                compor, carcri, ds_measure, meelem,&
                measse, veelem, nddle , ddlexc     , modrig,&
                ldccvg, matas2, matgeo)
    ASSERT(ldccvg.eq.0)
!
! --- CALCUL DES MODES PROPRES
!
!  ON DIFFERENCIE NFREQ (DONNEE UTILISATEUR) DE NFREQC
!  QUI EST LE NB DE FREQ TROUVEES PAR L'ALGO DANS NMOP45
!
    nfreqc = nfreq
    call nmop45(matas2, matgeo, defo, optmod, nfreqc,&
                cdsp, bande, mod45, ddlexc, nddle,&
                sdmode, sdstab, ddlsta, nsta)
    if (nfreqc .eq. 0) then
        freqr = r8vide()
        numord = -1
        goto 999
    endif
!
! --- SELECTION DU MODE DE PLUS PETITE FREQUENCE
!
    if (mod45 .eq. 'VIBR') then
        varacc = 'FREQ'
    else if (mod45 .eq. 'FLAM') then
        varacc = 'CHAR_CRIT'
    else
        ASSERT(.false.)
    endif
    freqm = r8maem()
    numord = 0
    do i = 1, nfreqc
        call rsadpa(sdmode, 'L', 1, varacc, i,&
                    0, sjv=ljeveu)
        freqv = zr(ljeveu)
        freqa = abs(freqv)
        if (freqa .lt. freqm) then
            numord = i
            freqm = freqa
            freqr = freqv
        endif
        if (mod45 .eq. 'VIBR') then
            call utmess('I', 'MECANONLINE6_10', si=i, sr=freqv)
        else if (mod45 .eq. 'FLAM') then
            call utmess('I', 'MECANONLINE6_11', si=i, sr=freqv)
        else
            ASSERT(.false.)
        endif
    end do
    if (nsta .ne. 0) then
        call rsadpa(sdstab, 'L', 1, 'CHAR_STAB', 1,&
                    0, sjv=ljeve2)
        csta = zr(ljeve2)
        call utmess('I', 'MECANONLINE6_12', si=1, sr=csta)
    endif
!
! --- NOM DU MODE
!
    if (mod45 .eq. 'VIBR') then
        call nmlesd('POST_TRAITEMENT', sdpost, 'SOLU_MODE_VIBR', ibid, r8bid,&
                    vecmod)
    else if (mod45 .eq. 'FLAM') then
        call nmlesd('POST_TRAITEMENT', sdpost, 'SOLU_MODE_FLAM', ibid, r8bid,&
                    vecmod)
        if (nsta .ne. 0) then
            call nmlesd('POST_TRAITEMENT', sdpost, 'SOLU_MODE_STAB', ibid, r8bid,&
                        vecmo2)
        endif
    else
        ASSERT(.false.)
    endif
!
! --- RECUPERATION DES MODES DANS LA SD MODE
!
    call rsexch('F', sdmode, 'DEPL', numord, champ,&
                iret)
    call copisd('CHAMP_GD', 'V', champ, vecmod)
    if (nsta .ne. 0) then
        call rsexch('F', sdstab, 'DEPL', 1, champ2,&
                    iret)
        call copisd('CHAMP_GD', 'V', champ2, vecmo2)
    endif
!
! --- AFFICHAGE DES MODES
!
    if (mod45 .eq. 'VIBR') then
        call utmess('I', 'MECANONLINE6_14', sr=freqr)
    else if (mod45 .eq. 'FLAM') then
        call utmess('I', 'MECANONLINE6_15', sr=freqr)
        if (nsta .ne. 0) then
            call utmess('I', 'MECANONLINE6_16', sr=csta)
        endif
    else
        ASSERT(.false.)
    endif
!
! --- DETECTION INSTABILITE SI DEMANDE
!
    if (mod45 .eq. 'FLAM') then
        call nmflin(sdpost, matas2, freqr, linsta)
        call nmcrel(sderro, 'CRIT_STAB', linsta)
    endif
!
! --- ARRET
!
999 continue
!
! --- MODE SELECTIONNE ECRIT DANS SDPOST
!
    if (mod45 .eq. 'VIBR') then
        call nmecsd('POST_TRAITEMENT', sdpost, 'SOLU_FREQ_VIBR', ibid, freqr,&
                    k24bid)
        call nmecsd('POST_TRAITEMENT', sdpost, 'SOLU_NUME_VIBR', numord, r8bid,&
                    k24bid)
    else if (mod45 .eq. 'FLAM') then
        call nmecsd('POST_TRAITEMENT', sdpost, 'SOLU_FREQ_FLAM', ibid, freqr,&
                    k24bid)
        call nmecsd('POST_TRAITEMENT', sdpost, 'SOLU_NUME_FLAM', numord, r8bid,&
                    k24bid)
        if (nsta .ne. 0) then
            call nmecsd('POST_TRAITEMENT', sdpost, 'SOLU_FREQ_STAB', ibid, csta,&
                        k24bid)
            call nmecsd('POST_TRAITEMENT', sdpost, 'SOLU_NUME_STAB', 1, r8bid,&
                        k24bid)
        endif
    else
        ASSERT(.false.)
    endif
!
! --- DESTRUCTION DE LA SD DE STOCKAGE DES MODES
!
    call jedetc('G', sdmode, 1)
    call jedetc('G', sdstab, 1)
!
    call jedema()
end subroutine
