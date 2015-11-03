subroutine nmconv(noma    , modele, mate      , numedd  , sdnume     ,&
                  fonact  , sddyna, ds_conv   , ds_print, sdstat     ,&
                  sddisc  , sdtime, sdcrit    , sderro  , ds_algopara,&
                  ds_inout, comref, matass    , solveu  , numins     ,&
                  iterat  , eta   , ds_contact, valinc  , solalg     ,&
                  measse  , veasse)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/cfmmcv.h"
#include "asterfort/dierre.h"
#include "asterfort/diinst.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmadev.h"
#include "asterfort/nmcore.h"
#include "asterfort/nmcrel.h"
#include "asterfort/nmdivr.h"
#include "asterfort/nmeceb.h"
#include "asterfort/nmerge.h"
#include "asterfort/nmevcv.h"
#include "asterfort/nmimr0.h"
#include "asterfort/nmimrv.h"
#include "asterfort/nmlecv.h"
#include "asterfort/nmlerr.h"
#include "asterfort/nmltev.h"
#include "asterfort/nmnkft.h"
#include "asterfort/nmresi.h"
#include "asterfort/nmrvai.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
! aslint: disable=W1504
!
    integer :: fonact(*)
    integer :: iterat, numins
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
    real(kind=8) :: eta, instan
    character(len=19) :: sdcrit, sddisc, sddyna, sdnume
    character(len=19) :: matass, solveu
    character(len=19) :: measse(*), veasse(*)
    character(len=19) :: solalg(*), valinc(*)
    character(len=24) :: comref, mate
    character(len=8) :: noma
    character(len=24) :: numedd, modele
    type(NL_DS_Contact), intent(inout) :: ds_contact
    character(len=24) :: sderro, sdstat, sdtime
    type(NL_DS_InOut), intent(in) :: ds_inout
    type(NL_DS_Print), intent(inout) :: ds_print
    type(NL_DS_Conv), intent(inout) :: ds_conv
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Convergence management
!
! Global convergence of current Newton iteration
!
! --------------------------------------------------------------------------------------------------
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  MODELE : NOM DU MODELE
! IO  ds_contact       : datastructure for contact management
! IO  ds_conv          : datastructure for convergence management
! IN  SDTIME : SD TIMER
! In  ds_inout         : datastructure for input/output management
! IO  ds_print         : datastructure for printing parameters
! IN  NUMEDD : NUMEROTATION NUME_DDL
! IN  SDNUME : NOM DE LA SD NUMEROTATION
! IN  COMREF : VARI_COM REFE
! IN  MATASS : MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  SOLVEU : SOLVEUR
! IN  ITERAT : NUMERO D'ITERATION
! IN  NUMINS : NUMERO D'INSTANT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! IN  ETA    : COEFFICIENT DE PILOTAGE
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDERRO : GESTION DES ERREURS
! In  ds_algopara      : datastructure for algorithm parameters
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  SDSTAT : SD STATISTIQUES
! IN  SDCRIT : SYNTHESE DES RESULTATS DE CONVERGENCE POUR ARCHIVAGE
! IN  COMREF : VARI_COM REFE
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: lreli, lnkry, limpex, lcont
    real(kind=8) :: r8bid
    real(kind=8) :: pasmin
    real(kind=8) :: instam, instap
    real(kind=8) :: vresi, vchar
    aster_logical :: lerror, itemax, dvdebo
    aster_logical :: cvnewt, cvresi
    integer :: nbiter, itesup
    integer :: ifm, niv
    real(kind=8) :: line_sear_coef
    integer :: line_sear_iter
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> EVALUATION DE LA CONVERGENCE'
    endif
!
! --- INITIALISATIONS
!
    itemax = .false.
    lerror = .false.
    cvnewt = .false.
    pasmin = ds_algopara%pas_mini_elas
    line_sear_coef = r8vide()
    line_sear_iter = -1
!
! --- FONCTIONNALITES ACTIVEES
!
    lreli = isfonc(fonact,'RECH_LINE')
    lnkry = isfonc(fonact,'NEWTON_KRYLOV')
    limpex = isfonc(fonact,'IMPLEX')
    lcont = isfonc(fonact,'CONTACT')
!
! --- INSTANTS
!
    instan = diinst(sddisc,numins)
    instam = diinst(sddisc,numins-1)
    instap = diinst(sddisc,numins )
!
! - Set values are not affected on rows for residuals loop
!
    call nmimr0(ds_print, 'RESI')
!
! --- EVENEMENT ERREUR ACTIVE ?
!
    call nmltev(sderro, 'ERRI', 'NEWT', lerror)
    if (lerror) goto 999
!
! --- EXAMEN DU NOMBRE D'ITERATIONS
!
    call nmlerr(sddisc, 'L', 'ITERSUP', r8bid, itesup)
    if (itesup .eq. 0) then
        if (abs(instap-instam) .lt. pasmin) then
            nbiter = ds_conv%iter_glob_elas
        else
            nbiter = ds_conv%iter_glob_maxi
        endif
    else
        call nmlerr(sddisc, 'L', 'NBITER', r8bid, nbiter)
    endif
    itemax = (iterat+1) .ge. nbiter
!
! --- STATISTIQUES POUR RECHERCHE LINEAIRE
!
    if (lreli) then
        line_sear_coef = ds_conv%line_sear_coef
        line_sear_iter = ds_conv%line_sear_iter
    endif
    call nmrvai(sdstat, 'RECH_LINE_ITER', 'E', line_sear_iter)
!
! - Compute residuals
!
    call nmresi(noma  , mate   , numedd  , sdnume  , fonact,&
                sddyna, ds_conv, ds_print, ds_contact,&
                matass, numins , eta     , comref  , valinc,&
                solalg, veasse , measse  , ds_inout, vresi ,&
                vchar)
!
! - Evaluate convergence of residuals
!
    call nmcore(sdcrit        , sderro, fonact, numins, iterat ,&
                line_sear_iter, eta   , vresi , vchar , ds_conv)
!
! --- METHODE IMPLEX: CONVERGENCE FORCEE
!
    if (limpex) call nmeceb(sderro, 'RESI', 'CONV')
!
! --- CONVERGENCE ADAPTEE AU CONTACT
!
    if (lcont) then
        call cfmmcv(noma    , modele, numedd    , fonact, sddyna,&
                    ds_print, sdstat, sddisc    , sdtime, sderro,&
                    numins  , iterat, ds_contact, valinc, solalg,&
                    instan)
    endif
!
! - Set value of informations in convergence table (residuals are in nmimre)
!
    call nmimrv(ds_print, fonact, iterat, line_sear_coef, line_sear_iter,&
                eta)
!
! --- CAPTURE ERREUR EVENTUELLE
!
    call nmltev(sderro, 'ERRI', 'NEWT', lerror)
    if (lerror) goto 999
!
! --- INFORMATION POUR DEBORST
!
    call nmlecv(sderro, 'RESI', cvresi)
    call nmerge(sderro, 'DIVE_DEBO', dvdebo)
    if (cvresi .and. dvdebo) then
        call utmess('I', 'MECANONLINE2_3')
    endif
!
! --- EVALUATION DE LA CONVERGENCE DE L'ITERATION DE NEWTON
!
    call nmevcv(sderro, fonact, 'NEWT')
    call nmlecv(sderro, 'NEWT', cvnewt)
!
! --- ENREGISTRE LES RESIDUS A CETTE ITERATION
!
    call dierre(sddisc, sdcrit, iterat)
!
! --- EVALUATION DE LA DIVERGENCE DU RESIDU
!
    call nmdivr(sddisc, sderro, iterat)
!
! --- SI ON A CONVERGE: ON A PAS ATTEINT LE NB D'ITERATIONS MAXIMUM
!
    if (cvnewt) itemax = .false.
!
! --- ENREGISTREMENT EVENEMENT MAX ITERATION DE NEWTON
!
    call nmcrel(sderro, 'ITER_MAXI', itemax)
!
! --- CALCUL CRITERE DE CONVERGENCE POUR NEWTON-KRYLOV (FORCING-TERM)
!
    if (lnkry) then
        call nmnkft(solveu, sddisc, iterat)
    endif
!
999 continue
!
! --- MISE A JOUR DE L'INDICATEUR DE SUCCES SUR LES ITERATIONS DE NEWTON
!
    call nmadev(sddisc, sderro, iterat)
!
end subroutine
