subroutine nmconv(noma, modele, mate, numedd, sdnume,&
                  fonact, sddyna, sdconv, sdimpr, sdstat,&
                  sddisc, sdtime, sdcrit, sderro, parmet,&
                  comref, matass, solveu, numins, iterat,&
                  conv, eta, parcri, defico, resoco,&
                  valinc, solalg, measse, veasse)
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/cfmmcv.h"
#include "asterfort/dierre.h"
#include "asterfort/diinst.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
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
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    real(kind=8) :: eta, conv(*), parcri(*), parmet(*), instan
    character(len=19) :: sdcrit, sddisc, sddyna, sdnume
    character(len=19) :: matass, solveu
    character(len=19) :: measse(*), veasse(*)
    character(len=19) :: solalg(*), valinc(*)
    character(len=24) :: comref, mate
    character(len=8) :: noma
    character(len=24) :: numedd, modele
    character(len=24) :: defico, resoco
    character(len=24) :: sdimpr, sderro, sdstat, sdconv, sdtime
!
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! VERIFICATION DES CRITERES D'ARRET
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  MODELE : NOM DU MODELE
! IN  DEFICO : SD POUR LA DEFINITION DU CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DU CONTACT
! IN  SDIMPR : SD AFFICHAGE
! IN  SDCONV : SD GESTION DE LA CONVERGENCE
! IN  SDTIME : SD TIMER
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
! I/O CONV   : INFORMATIONS SUR LA CONVERGENCE DU CALCUL
! IN  PARCRI : CRITERES DE CONVERGENCE
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDERRO : GESTION DES ERREURS
! IN  PARMET : PARAMETRES DE LA METHODE DE RESOLUTION
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  SDIMPR : SD AFFICHAGE
! IN  SDSTAT : SD STATISTIQUES
! IN  SDCRIT : SYNTHESE DES RESULTATS DE CONVERGENCE POUR ARCHIVAGE
! IN  COMREF : VARI_COM REFE
!
! ----------------------------------------------------------------------
!
    aster_logical :: lreli, lnkry, limpex, lcont
    real(kind=8) :: r8bid
    real(kind=8) :: resi_glob_rela, resi_glob_maxi, pasmin
    real(kind=8) :: instam, instap
    real(kind=8) :: vrela, vmaxi, vrefe, vresi, vchar, vinit, vcomp, vfrot
    real(kind=8) :: vgeom
    aster_logical :: lerror, itemax, dvdebo
    aster_logical :: cvnewt, cvresi
    integer :: nbiter, itesup
    integer :: ifm, niv
    real(kind=8) :: relcoe
    integer :: relite
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> EVALUATION DE LA CONVERGENCE'
    endif
!
! --- INITIALISATIONS
!
    itemax = .false.
    lerror = .false.
    cvnewt = .false.
    resi_glob_maxi = parcri(1)
    resi_glob_rela = parcri(2)
    pasmin = parmet(3)
    vrela = r8vide()
    vmaxi = r8vide()
    vrefe = r8vide()
    vresi = r8vide()
    vchar = r8vide()
    vinit = r8vide()
    vcomp = r8vide()
    vfrot = r8vide()
    vgeom = r8vide()
    relcoe = r8vide()
    relite = -1
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
! --- INITIALISATION AFFECTATION DES COLONNES
!
    call nmimr0(sdimpr, 'RESI')
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
            nbiter = nint(parcri(5))
        else
            nbiter = nint(parcri(1))
        endif
    else
        call nmlerr(sddisc, 'L', 'NBITER', r8bid, nbiter)
    endif
    itemax = (iterat+1) .ge. nbiter
!
! --- STATISTIQUES POUR RECHERCHE LINEAIRE
!
    if (lreli) then
        relite = nint(conv(1))
        relcoe = conv(2)
    endif
    call nmrvai(sdstat, 'RECH_LINE_ITER', 'E', relite)
!
! --- CALCUL DES RESIDUS
!
    call nmresi(noma, mate, numedd, sdnume, fonact,&
                sddyna, sdconv, sdimpr, defico, resoco,&
                matass, numins, conv, resi_glob_rela, resi_glob_maxi,&
                eta, comref, valinc, solalg, veasse,&
                measse, vrela, vmaxi, vchar, vresi,&
                vrefe, vinit, vcomp, vfrot, vgeom)
!
! --- VERIFICATION DES CRITERES D'ARRET SUR RESIDUS
!
    call nmcore(sdcrit, sderro, sdconv, defico, numins,&
                iterat, fonact, relite, eta, parcri,&
                vresi, vrela, vmaxi, vchar, vrefe,&
                vcomp, vfrot, vgeom)
!
! --- METHODE IMPLEX: CONVERGENCE FORCEE
!
    if (limpex) call nmeceb(sderro, 'RESI', 'CONV')
!
! --- CONVERGENCE ADAPTEE AU CONTACT
!
    if (lcont) then
        call cfmmcv(noma, modele, numedd, fonact, sddyna,&
                    sdimpr, sdstat, sddisc, sdtime, sderro,&
                    numins, iterat, defico, resoco, valinc,&
                    solalg, instan)
    endif
!
! --- ENREGISTRE LES DONNEES POUR AFFICHAGE DANS LA SDIMPR
!
    call nmimrv(sdimpr, fonact, iterat, relcoe, relite,&
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
    call jedema()
end subroutine
