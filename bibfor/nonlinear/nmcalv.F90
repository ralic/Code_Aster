subroutine nmcalv(typvec, modelz, lischa, mate  , carele,&
                  compor, carcri, numedd, comref, sdtime,&
                  instam, instap, valinc, solalg, sddyna,&
                  option, vecele)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdebg.h"
#include "asterfort/nmdep0.h"
#include "asterfort/nmdidi.h"
#include "asterfort/nmsssv.h"
#include "asterfort/nmtime.h"
#include "asterfort/nmvcex.h"
#include "asterfort/nmvcfo.h"
#include "asterfort/vecgme.h"
#include "asterfort/vechme.h"
#include "asterfort/vedime.h"
#include "asterfort/vedpme.h"
#include "asterfort/vefnme.h"
#include "asterfort/vefpme.h"
#include "asterfort/veimpd.h"
#include "asterfort/velame.h"
#include "asterfort/veondp.h"
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
    character(len=*) :: modelz
    character(len=24) :: mate, carele, compor, carcri, numedd
    character(len=24) :: comref, sdtime
    real(kind=8) :: instam, instap
    character(len=19) :: lischa, sddyna
    character(len=19) :: solalg(*), valinc(*)
    character(len=6) :: typvec
    character(len=19) :: vecele
    character(len=16) :: option
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CALCUL DES VECTEURS ELEMENTAIRES
!
! ----------------------------------------------------------------------
!
!
! IN  TYPVEC : TYPE DE CALCUL VECT_ELEM
! IN  MODELE : MODELE
! IN  LISCHA : LISTE DES CHARGES
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMPOR : CARTE DECRIVANT LE TYPE DE COMPORTEMENT
! IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
! IN  NUMEDD : NUME_DDL
! IN  SDTIME : SD TIMER
! IN  COMREF : VARI_COM DE REFERENCE
! IN  INSTAM : INSTANT MOINS
! IN  INSTAP : INSTANT PLUS
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  SDDYNA : SD DYNAMIQUE
! IN  OPTION : OPTION DE CALCUL DE VECT_ELEM
! OUT VECELE : VECT_ELEM CALCULE
!
!
!
!
    character(len=24) :: modele
    character(len=19) :: depmoi, sigmoi, commoi, depdel, vitmoi, sigext
    character(len=19) :: complu, vitplu, strmoi
    character(len=24) :: vrcmoi, vrcplu, varc
    character(len=19) :: depl, sigm, strx
    integer :: neq
    real(kind=8) :: partps(2), inst(3)
    character(len=24) :: charge, infoch
    character(len=8) :: noma
    character(len=16) :: optio2
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE><VECT> CALCUL DES VECT_ELEM DE TYPE <',typvec,'>'
    endif
!
! --- INITIALISATIONS
!
    modele = modelz
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=noma)
    call dismoi('NB_EQUA   ', numedd, 'NUME_DDL', repi=neq)
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    if (valinc(1)(1:1) .ne. ' ') then
        call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
        call nmchex(valinc, 'VALINC', 'VITMOI', vitmoi)
        call nmchex(valinc, 'VALINC', 'SIGMOI', sigmoi)
        call nmchex(valinc, 'VALINC', 'COMMOI', commoi)
        call nmchex(valinc, 'VALINC', 'STRMOI', strmoi)
        call nmchex(valinc, 'VALINC', 'COMPLU', complu)
        call nmchex(valinc, 'VALINC', 'VITPLU', vitplu)
        call nmchex(valinc, 'VALINC', 'SIGEXT', sigext)
    endif
    if (solalg(1)(1:1) .ne. ' ') then
        if (typvec .eq. 'CNFNOD') then
            call nmdep0('ON ', solalg)
        endif
        call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
    endif
!
! --- VARIABLES TEMPS
!
    partps(1) = instam
    partps(2) = instap
!
    inst(1) = instap
    inst(2) = instap-instam
    inst(3) = 0.d0
!
    charge = lischa(1:19)//'.LCHA'
    infoch = lischa(1:19)//'.INFC'
!
! --- MESURES
!
    call nmtime(sdtime, 'INI', 'SECO_MEMB')
    call nmtime(sdtime, 'RUN', 'SECO_MEMB')
!
! --- FORCES NODALES
!
    if (typvec .eq. 'CNFNOD') then
        optio2 = 'FORC_NODA'
!
        if (option .eq. 'SIGMOI') then
            call nmvcex('TOUT', commoi, vrcmoi)
            depl = depmoi
            varc = vrcmoi
            sigm = sigmoi
            strx = strmoi
        else if (option.eq.'SIGEXT') then
            call nmvcex('TOUT', commoi, vrcmoi)
            depl = depmoi
            varc = vrcmoi
            sigm = sigext
            strx = ' '
        else
            ASSERT(.false.)
        endif
!
        call vefnme(optio2, 'V', modele, mate, carele,&
                    compor, partps, 0, ' ', varc,&
                    sigm, strx, depl, depdel, vecele)
        call nmdep0('OFF', solalg)
!
! --- DEPLACEMENTS DIRICHLET FIXE
!
    else if (typvec.eq.'CNDIDO') then
        call vedime(modele, charge, infoch, instap, 'R',&
                    vecele)
!
! --- DEPLACEMENTS DIRICHLET DIFFERENTIEL
!
    else if (typvec.eq.'CNDIDI') then
        call nmdidi(modele, lischa, depmoi, vecele)
!
! --- DEPLACEMENTS DIRICHLET PILOTE
!
    else if (typvec.eq.'CNDIPI') then
        call vedpme(modele, charge, infoch, instap, vecele)
!
! --- FORCES DE LAPLACE
!
    else if (typvec.eq.'CNLAPL') then
        call velame(modele, charge, infoch, depmoi, vecele)
!
! --- FORCES ONDES PLANES
!
    else if (typvec.eq.'CNONDP') then
        call veondp(modele, mate, sddyna, instap, vecele)
!
! --- FORCES IMPEDANCE
!
    else if (typvec.eq.'CNIMPC') then
        call veimpd(modele, mate, vitplu, sddyna, vecele)
    else if (typvec.eq.'CNIMPP') then
        call veimpd(modele, mate, vitmoi, sddyna, vecele)
!
! --- FORCES FIXES MECANIQUES DONNEES
!
    else if (typvec.eq.'CNFEDO') then
        call nmvcex('TOUT', complu, vrcplu)
        call vechme('S', modele, charge, infoch, inst,&
                    carele, mate, vecele, varc_currz = vrcplu)
!
! --- FORCES PILOTEES
!
    else if (typvec.eq.'CNFEPI') then
        call nmvcex('TOUT', complu, vrcplu)
        call vefpme(modele, carele, mate, charge, infoch,&
                    inst  , vrcplu, vecele, ' ')
!
! --- FORCES ISSUES DU CALCUL PAR SOUS-STRUCTURATION
!
    else if (typvec.eq.'CNSSTF') then
        call nmsssv(modele, mate, carele, lischa, vecele)
!
! --- FORCES SUIVEUSES
!
    else if (typvec.eq.'CNFSDO') then
        call vecgme(modele, carele, mate, charge, infoch,&
                    instap, depmoi, depdel, vecele, instam,&
                    compor, carcri, ' ', vitplu, strmoi)
!
! --- FORCE DE REFERENCE POUR VARIABLES DE COMMANDE INITIALES
!
    else if (typvec.eq.'CNVCF1') then
        call nmvcfo('-'   , modele   , mate     , carele, compor,&
                    comref, valinc, vecele)
!
! --- FORCE DE REFERENCE POUR VARIABLES DE COMMANDE COURANTES
!
    else if (typvec.eq.'CNVCF0') then
        call nmvcfo('+'   , modele   , mate     , carele, compor,&
                    comref, valinc, vecele)
    else
        ASSERT(.false.)
    endif
!
! --- DEBUG
!
    if (niv .eq. 2) then
        call nmdebg('VECT', vecele, ifm)
    endif
!
    call nmtime(sdtime, 'END', 'SECO_MEMB')
!
end subroutine
