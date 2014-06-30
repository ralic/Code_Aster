subroutine nmdesc(modele, numedd, numfix, mate, carele,&
                  comref, compor, lischa, resoco, method,&
                  solveu, parmet, carcri, fonact, numins,&
                  iterat, sddisc, sdimpr, sdstat, sdtime,&
                  sddyna, sdnume, sderro, matass, maprec,&
                  defico, valinc, solalg, meelem, measse,&
                  veasse, veelem, lerrit)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
#include "asterfort/copisd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmacin.h"
#include "asterfort/nmassc.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmcoma.h"
#include "asterfort/nmcret.h"
#include "asterfort/nmltev.h"
#include "asterfort/nmresd.h"
#include "asterfort/vtzero.h"
    integer :: numins, iterat
    real(kind=8) :: parmet(*)
    character(len=16) :: method(*)
    character(len=19) :: matass, maprec
    character(len=24) :: sdimpr, sdtime, sdstat
    character(len=19) :: lischa, solveu, sddisc, sddyna, sdnume
    character(len=24) :: numedd, numfix
    character(len=24) :: modele, mate, carele, comref, compor, carcri
    character(len=24) :: defico, resoco, sderro
    integer :: fonact(*)
    character(len=19) :: meelem(*), veelem(*)
    character(len=19) :: solalg(*), valinc(*)
    character(len=19) :: measse(*), veasse(*)
    logical(kind=1) :: lerrit
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CALCUL DE LA DIRECTION DE DESCENTE
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARI_COM DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  LISCHA : L_CHARGES
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDIMPR : SD AFFICHAGE
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  SDERRO : SD GESTION DES ERREURS
! IN  SDNUME : SD NUMEROTATION
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! IN  NUMINS : NUMERO D'INSTANT
! IN  RESOCO : SD RESOLUTION CONTACT
! IN  METHOD : INFORMATIONS SUR LES METHODES DE RESOLUTION
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLE
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! OUT LERRIT : .TRUE. SI ERREUR PENDANT L'ITERATION
!
! ----------------------------------------------------------------------
!
    character(len=24) :: codere
    character(len=19) :: cncine, depdel, cndonn, cnpilo, cncind
    integer :: faccvg, rescvg, ldccvg
    real(kind=8) :: r8bid
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- INITIALISATIONS
!
    cndonn = '&&CNCHAR.DONN'
    cnpilo = '&&CNCHAR.PILO'
    cncind = '&&CNCHAR.CINE'
    call vtzero(cndonn)
    call vtzero(cnpilo)
    call vtzero(cncind)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> CALCUL DIRECTION DE DESCENTE...'
    endif
!
! --- INITIALISATIONS CODES RETOURS
!
    ldccvg = -1
    faccvg = -1
    rescvg = -1
    codere = '&&NMDESC.CODERE'
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(veasse, 'VEASSE', 'CNCINE', cncine)
    call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
!
! --- CALCUL DE LA MATRICE GLOBALE
!
    call nmcoma(modele, mate, carele, compor, carcri,&
                parmet, method, lischa, numedd, numfix,&
                solveu, comref, sddisc, sddyna, sdimpr,&
                sdstat, sdtime, numins, iterat, fonact,&
                defico, resoco, valinc, solalg, veelem,&
                meelem, measse, veasse, maprec, matass,&
                codere, faccvg, ldccvg, sdnume)
!
! --- ERREUR SANS POSSIBILITE DE CONTINUER
!
    if ((faccvg.eq.1) .or. (faccvg.eq.2)) goto 9999
    if (ldccvg .eq. 1) goto 9999
!
! --- PREPARATION DU SECOND MEMBRE
!
    call nmassc(fonact, sddyna, sdtime, veasse, cnpilo,&
                cndonn)
!
! --- ACTUALISATION DES CL CINEMATIQUES
!
    call copisd('CHAMP_GD', 'V', cncine, cncind)
    call nmacin(fonact, matass, depdel, cncind)
!
! --- RESOLUTION
!
    call nmresd(fonact, sddyna, sdstat, sdtime, solveu,&
                numedd, r8bid, maprec, matass, cndonn,&
                cnpilo, cncind, solalg, rescvg)
!
9999  continue
!
! --- TRANSFORMATION DES CODES RETOURS EN EVENEMENTS
!
    call nmcret(sderro, 'LDC', ldccvg)
    call nmcret(sderro, 'FAC', faccvg)
    call nmcret(sderro, 'RES', rescvg)
!
! --- EVENEMENT ERREUR ACTIVE ?
!
    call nmltev(sderro, 'ERRI', 'NEWT', lerrit)
!
    call jedema()
end subroutine
