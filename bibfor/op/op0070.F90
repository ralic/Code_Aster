subroutine op0070()
!
use NonLin_Datastructure_type
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/infmaj.h"
#include "asterfort/inidbg.h"
#include "asterfort/jerecu.h"
#include "asterfort/ndexpl.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmactp.h"
#include "asterfort/nmaffi.h"
#include "asterfort/nmarch.h"
#include "asterfort/nmcvgc.h"
#include "asterfort/nmcvgp.h"
#include "asterfort/nmdata.h"
#include "asterfort/nmeceb.h"
#include "asterfort/nmerro.h"
#include "asterfort/nmevdt.h"
#include "asterfort/nmfpas.h"
#include "asterfort/nmini0.h"
#include "asterfort/nminit.h"
#include "asterfort/nmleeb.h"
#include "asterfort/nmlost.h"
#include "asterfort/nmmeng.h"
#include "asterfort/nmnewt.h"
#include "asterfort/nmpost.h"
#include "asterfort/nmrinc.h"
#include "asterfort/nmstat.h"
#include "asterfort/nmtime.h"
#include "asterfort/onerrf.h"
#include "asterfort/titre.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!

!
! --------------------------------------------------------------------------------------------------
!
! STAT_NON_LINE
! DYNA_NON_LINE
!
! --------------------------------------------------------------------------------------------------
!
    integer :: fonact(100)
    integer :: zmeelm, zmeass, zveelm, zveass
    parameter    (zmeelm=9 ,zmeass=4 ,zveelm=21,zveass=32)
    integer :: zsolal, zvalin
    parameter    (zsolal=17,zvalin=28)
!
! --- GESTION BOUCLES
!
    integer :: numins, nbiter
    character(len=4) :: etfixe, etinst, etcalc
!
! --- GESTION ERREUR
!
    integer :: lenout
    character(len=16) :: compex
!
    integer :: ibid
!
    real(kind=8) :: eta
!
    character(len=8) :: result, mesh
!
    character(len=16) :: k16bid
    character(len=19) :: lischa
    character(len=19) :: solveu, maprec, matass
    character(len=24) :: modele, mate, carele, compor
    character(len=24) :: numedd, numfix
    character(len=24) :: carcri, comref, codere
!
! --- FONCTIONNALITES ACTIVEES
!
    aster_logical :: lexpl, limpl, lstat
!
! --- STRUCTURES DE DONNEES
!
    character(len=24) :: sderro
    character(len=24) :: sd_suiv, sdcriq
    character(len=19) :: sdpilo, sdnume, sddyna, sddisc, sdcrit
    character(len=19) :: sd_obsv, sdpost
    type(NL_DS_Print)     :: ds_print
    type(NL_DS_Conv)      :: ds_conv
    type(NL_DS_AlgoPara)  :: ds_algopara
    type(NL_DS_InOut)     :: ds_inout
    type(NL_DS_Contact)   :: ds_contact
    type(NL_DS_Measure)   :: ds_measure
    type(NL_DS_Energy)    :: ds_energy
    type(ROM_DS_AlgoPara) :: ds_algorom
!
! --- VARIABLES CHAPEAUX
!
    character(len=19) :: valinc(zvalin), solalg(zsolal)
!
! --- MATR_ELEM, VECT_ELEM ET MATR_ASSE
!
    character(len=19) :: meelem(zmeelm), veelem(zveelm)
    character(len=19) :: measse(zmeass), veasse(zveass)
!
! ----------------------------------------------------------------------
!
    data sdpilo            /'&&OP0070.PILO.'/
    data sdpost, sdcriq    /'&&OP0070.POST.','&&OP0070.CRIQ.'/
    data sderro            /'&&OP0070.ERRE.'/
    data sdnume            /'&&OP0070.NUME.ROTAT'/
    data sddisc            /'&&OP0070.DISC.'/
    data sdcrit            /'&&OP0070.CRIT.'/
    data lischa            /'&&OP0070.LISCHA'/
    data carcri            /'&&OP0070.PARA_LDC'/
    data solveu            /'&&OP0070.SOLVEUR'/
    data comref            /'&&OP0070.COREF'/
    data maprec            /'&&OP0070.MAPREC'/
    data codere            /'&&OP0070.CODERE'/
!
! ----------------------------------------------------------------------
!
    call titre()
    call infmaj()
    call inidbg()
!
! ======================================================================
!     RECUPERATION DES OPERANDES ET INITIALISATION
! ======================================================================
!
! --- ON STOCKE LE COMPORTEMENT EN CAS D'ERREUR AVANT MNL : COMPEX
! --- PUIS ON PASSE DANS LE MODE "VALIDATION DU CONCEPT EN CAS D'ERREUR"
!
    call onerrf(' ', compex, lenout)
    call onerrf('EXCEPTION+VALID', k16bid, ibid)
!
! --- NOM DE LA SD RESULTAT
!
    call getres(result, k16bid, k16bid)
!
! - Creation of datastructures
!
    call nmini0(fonact    , eta      , numins     , matass  , zmeelm    ,&
                zmeass    , zveelm   , zveass     , zsolal  , zvalin    ,&
                ds_print  , ds_conv  , ds_algopara, ds_inout, ds_contact,&
                ds_measure, ds_energy, ds_algorom)
!
! - Read parameters
!
    call nmdata(modele    , mesh      , mate      , carele     , lischa  ,&
                solveu    , ds_conv   , sddyna    , sdpost     , sderro  ,&
                ds_energy , sdcriq    , ds_print  , ds_algopara, ds_inout,&
                ds_contact, ds_measure, ds_algorom, compor     , carcri)
!
! - Initializations of datastructures
!
    call nminit(result, modele  , numedd    , numfix     , mate      ,&
                compor, carele  , lischa    , ds_algopara, maprec    ,&
                solveu, carcri  , numins    , sddisc     , sdnume    ,&
                sdcrit, comref  , fonact    , mesh       , sdpilo    ,&
                sddyna, ds_print, sd_suiv   , sd_obsv    , sderro    ,&
                sdpost, ds_inout, ds_energy , ds_conv    , sdcriq    ,&
                valinc, solalg  , measse    , veelem     , meelem    ,&
                veasse, codere  , ds_contact, ds_measure , ds_algorom)
!
! - Launch timer for total time
!
    call nmtime(ds_measure, 'Launch', 'Compute')
!
! --- PREMIER INSTANT
!
    numins = 1
!
! --- QUELQUES FONCTIONNALITES ACTIVEES
!
    limpl = ndynlo(sddyna,'IMPLICITE')
    lexpl = ndynlo(sddyna,'EXPLICITE')
    lstat = ndynlo(sddyna,'STATIQUE' )
!
! ======================================================================
!  DEBUT DU PAS DE TEMPS
! ======================================================================
!
200 continue
!
! --- AUCUNE BOUCLE N'EST CONVERGE
!
    call nmeceb(sderro, 'RESI', 'CONT')
    call nmeceb(sderro, 'NEWT', 'CONT')
    call nmeceb(sderro, 'FIXE', 'CONT')
    call nmeceb(sderro, 'INST', 'CONT')
!
    call jerecu('V')
!
! - Launch timer for current step time
!
    call nmtime(ds_measure, 'Launch', 'Time_Step')
!
! --- REALISATION DU PAS DE TEMPS
!
    if (lexpl) then
        call ndexpl(modele, numedd  , numfix, mate       , carele  ,&
                    comref, compor  , lischa, ds_algopara, fonact  ,&
                    carcri, ds_print, ds_measure, sdnume     , sddyna  ,&
                    sddisc, sderro, valinc     , numins  ,&
                    solalg, solveu  , matass, maprec     , ds_inout,&
                    meelem, measse  , veelem, veasse     , nbiter)
    else if (lstat.or.limpl) then
        call nmnewt(mesh       , modele  , numins , numedd    , numfix     ,&
                    mate       , carele  , comref , compor    , lischa     ,&
                    ds_algopara, fonact  , carcri , ds_measure, sderro     ,&
                    ds_print   , sdnume  , sddyna , sddisc    , sdcrit     ,&
                    sd_suiv    , sdpilo  , ds_conv, solveu    , maprec     ,&
                    matass     , ds_inout, valinc , solalg    , meelem     ,&
                    measse     , veelem  , veasse , ds_contact, ds_algorom ,&
                    eta        , nbiter  )
    else
        ASSERT(.false.)
    endif
!
! - End of timer for current step time
!
    call nmtime(ds_measure, 'Stop', 'Time_Step')
    call nmrinc(ds_measure, 'Time_Step')
!
! ======================================================================
!  FIN DU PAS DE TEMPS
! ======================================================================
!
!
! - Time lost (step was cut)
!
    call nmleeb(sderro, 'FIXE', etfixe)
    if (etfixe .eq. 'ERRE') then
        call nmlost(ds_measure)
    endif
!
! - Post-treatment
!
    call nmpost(modele , mesh    , numedd, numfix     , carele,&
                compor , numins  , mate  , comref     , ds_inout,&
                ds_contact, ds_algopara, fonact,&
                carcri , ds_print, ds_measure, sddisc     , &
                sd_obsv, sderro  , sddyna, sdpost     , valinc,&
                solalg , meelem  , measse, veelem     , veasse,&
                ds_energy , sdcriq  , eta   , lischa)
!
! --- ETAT DE LA CONVERGENCE DU PAS DE TEMPS
!
    call nmcvgp(sddisc, numins, sderro, valinc, fonact,&
                ds_contact)
!
! --- AFFICHAGES PENDANT LA BOUCLE DES PAS DE TEMPS
!
    call nmaffi(fonact, ds_conv, ds_print, sderro, sddisc,&
                'INST')
!
! --- STATISTIQUES SUR PAS DE TEMPS
!
    if (.not.lexpl) then
        call nmstat('P', ds_measure, ds_print, sddisc, numins, sderro)
    endif
!
! --- GESTION DES ACTIONS A LA FIN D'UN PAS DE TEMPS
!
    call nmactp(ds_print, sddisc, sderro, ds_contact,&
                ds_conv , nbiter, numins)
!
! --- INSTANT SUIVANT
!
    call nmleeb(sderro, 'INST', etinst)
    if (etinst .eq. 'ERRE') then
        goto 200
    else if (etinst.eq.'STOP') then
        goto 800
    endif
!
! --- VERIFICATION DU DECLENCHEMENT DES ERREURS FATALES
!
    call nmevdt(ds_measure, sderro, 'PAS')
!
! --- EVALUATION DE LA CONVERGENCE DU CALCUL
!
    call nmcvgc(sddisc, sderro, numins, fonact)
!
! --- ARCHIVAGE DES RESULTATS
!
    call onerrf(compex, k16bid, ibid)
    call nmarch(numins    , modele  , mate  , carele, fonact,&
                carcri    , ds_print, sddisc, sdpost, sdcrit,&
                ds_measure, sderro  , sddyna, sdpilo, ds_energy,&
                ds_inout  , sdcriq  )
    call onerrf('EXCEPTION+VALID', k16bid, ibid)
!
! --- ETAT DU CALCUL
!
    call nmleeb(sderro, 'CALC', etcalc)
    if ((etcalc.eq.'ERRE') .or. (etcalc.eq.'STOP')) then
        goto 800
    else if (etcalc.eq.'CONV') then
        goto 900
    endif
!
! --- MISE A JOUR DES INFORMATIONS POUR UN NOUVEAU PAS DE TEMPS
!
    ASSERT(etcalc.eq.'CONT')
    call nmfpas(fonact, sddyna, sdpilo, sddisc, nbiter,&
                numins, eta   , valinc, solalg, veasse)
    numins = numins + 1
!
    goto 200
!
! ======================================================================
!     GESTION DES ERREURS
! ======================================================================
!
800 continue
!
! --- ON COMMENCE PAR ARCHIVER LE PAS DE TEMPS PRECEDENT
!
    if (numins .ne. 1) then
        call nmarch(numins-1, modele  , mate  , carele, fonact,&
                    carcri    , ds_print, sddisc, sdpost, sdcrit,&
                    ds_measure, sderro  , sddyna, sdpilo, ds_energy,&
                    ds_inout  , sdcriq  )
    endif
!
! - Write messages for errors
!
    call nmerro(sderro, ds_measure, numins)
!
! ======================================================================
!     SORTIE
! ======================================================================
!
900 continue
!
! - End of timer for total time
!
    call nmtime(ds_measure, 'Stop', 'Compute')
!
! --- IMPRESSION STATISTIQUES FINALES
!
    if (.not.lexpl) then
        call nmstat('T', ds_measure, ds_print, sddisc, numins, sderro)
    endif
!
! --- ON REMET LE MECANISME D'EXCEPTION A SA VALEUR INITIALE
!
    call onerrf(compex, k16bid, ibid)
!
! --- MENAGE
!
    call nmmeng(fonact, ds_algorom)
!
end subroutine
