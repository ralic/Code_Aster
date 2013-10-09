subroutine w039ca(ifi, form)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lgphmo.h"
#include "asterfort/w039c1.h"
#include "asterfort/w039c3.h"
    integer :: ifi
    character(len=*) :: form
! ----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
!     BUT:
!       IMPRIMER LES "CARTES" DE DONNEES DES CHAM_MATER, CARA_ELE, ...
! ----------------------------------------------------------------------
!
!
!
!
    integer :: nocc, iocc, n1, ibid
    character(len=3) :: rplo
    character(len=8) :: chmat, carele, mailla, charge, modele
    character(len=80) :: titre
    character(len=19) :: ligrel
    logical :: lexi
! ----------------------------------------------------------------------
!
    call jemarq()
    lexi=.false.
    ligrel='&&W039CA.LIGREL'
!
    if (.not.(form.eq.'MED'.or.form.eq.'RESULTAT')) goto 20
!
!
!
    call getfac('CONCEPT', nocc)
    do iocc = 1, nocc
!
!       -- CHAM_MATER :
!       ----------------
        call getvid('CONCEPT', 'CHAM_MATER', iocc=iocc, scal=chmat, nbret=n1)
        if (n1 .eq. 1) then
            if (.not.lexi) then
                call dismoi('NOM_MAILLA', chmat, 'CHAM_MATER', repk=mailla)
                call lgphmo(mailla, ligrel, 'PRESENTATION', 'TOUT')
                lexi=.true.
            endif
!
!
            titre='Champ de MATERIAUX'
            call w039c1(chmat//'.CHAMP_MAT', ifi, form, ligrel, titre)
        endif
!
!       -- CARA_ELEM :
!       ----------------
        call getvid('CONCEPT', 'CARA_ELEM', iocc=iocc, scal=carele, nbret=n1)
        if (n1 .eq. 1) then
            if (.not.lexi) then
                call dismoi('NOM_MAILLA', carele, 'CARA_ELEM', repk=mailla)
                call lgphmo(mailla, ligrel, 'PRESENTATION', 'TOUT')
                lexi=.true.
            endif
!
!
            titre='Caracteristiques generales des barres'
            call w039c1(carele//'.CARGENBA', ifi, form, ligrel, titre)
            titre='Caracteristiques geom. des barres'
            call w039c1(carele//'.CARGEOBA', ifi, form, ligrel, titre)
!
            titre='Caracteristiques generales des poutres'
            call w039c1(carele//'.CARGENPO', ifi, form, ligrel, titre)
            titre='Caracteristiques geom. des poutres'
            call w039c1(carele//'.CARGEOPO', ifi, form, ligrel, titre)
!
            titre='Caracteristiques des cables'
            call w039c1(carele//'.CARCABLE', ifi, form, ligrel, titre)
!
            titre='Caracteristiques des poutres courbes'
            call w039c1(carele//'.CARARCPO', ifi, form, ligrel, titre)
!
            titre='Caracteristiques des poutres "fluides"'
            call w039c1(carele//'.CARPOUFL', ifi, form, ligrel, titre)
!
            titre='Caracteristiques des elements discrets K_*'
            call w039c1(carele//'.CARDISCK', ifi, form, ligrel, titre)
            call w039c1(carele//'.CARDNSCK', ifi, form, ligrel, titre)
!
            titre='Caracteristiques des elements discrets M_*'
            call w039c1(carele//'.CARDISCM', ifi, form, ligrel, titre)
            call w039c1(carele//'.CARDNSCM', ifi, form, ligrel, titre)
!
            titre='Caracteristiques des elements discrets A_*'
            call w039c1(carele//'.CARDISCA', ifi, form, ligrel, titre)
            call w039c1(carele//'.CARDNSCA', ifi, form, ligrel, titre)
!
            titre='Caracteristiques geom. des coques'
            call w039c1(carele//'.CARCOQUE', ifi, form, ligrel, titre)
!
            titre='Orientation des elements 2D et 3D'
            call w039c1(carele//'.CARMASSI', ifi, form, ligrel, titre)
!
            titre='Orientation des coques et des poutres'
            call w039c1(carele//'.CARORIEN', ifi, form, ligrel, titre)
!
            call getvtx('CONCEPT', 'REPERE_LOCAL', iocc=iocc, scal=rplo, nbret=ibid)
            ASSERT(ibid.eq.1)
            if (rplo .eq. 'OUI') then
                call getvid('CONCEPT', 'MODELE', iocc=iocc, scal=modele, nbret=ibid)
                ASSERT(ibid.eq.1)
!
                titre = 'vecteur du repere local'
                call w039c3(carele, modele, ifi, form, titre)
!
!
            endif
!
        endif
!
!       -- CHARGE :
!       ----------------
        call getvid('CONCEPT', 'CHARGE', iocc=iocc, scal=charge, nbret=n1)
        if (n1 .eq. 1) then
            if (.not.lexi) then
                call dismoi('NOM_MAILLA', charge, 'CHARGE', repk=mailla)
                call lgphmo(mailla, ligrel, 'PRESENTATION', 'TOUT')
                lexi=.true.
            endif
!
!
            titre='Chargement de PESANTEUR'
            call w039c1(charge//'.CHME.PESAN', ifi, form, ligrel, titre)
!
            titre='Chargement de ROTATION'
            call w039c1(charge//'.CHME.ROTAT', ifi, form, ligrel, titre)
!
            titre='Chargement de PRES_REP'
            call w039c1(charge//'.CHME.PRESS', ifi, form, ligrel, titre)
!
            titre='Chargement de forces volumiques en 3D'
            call w039c1(charge//'.CHME.F3D3D', ifi, form, ligrel, titre)
!
            titre='Chargement de forces surfaciques en 3D'
            call w039c1(charge//'.CHME.F2D3D', ifi, form, ligrel, titre)
!
            titre='Chargement de forces lineiques en 3D'
            call w039c1(charge//'.CHME.F1D3D', ifi, form, ligrel, titre)
!
            titre='Chargement de forces surfaciques en 2D'
            call w039c1(charge//'.CHME.F2D2D', ifi, form, ligrel, titre)
!
            titre='Chargement de forces lineiques en 2D'
            call w039c1(charge//'.CHME.F1D2D', ifi, form, ligrel, titre)
!
            titre='Chargement de forces reparties pour les coques'
            call w039c1(charge//'.CHME.FCO3D', ifi, form, ligrel, titre)
            call w039c1(charge//'.CHME.FCO2D', ifi, form, ligrel, titre)
!
            titre='Chargement de EPSI_INIT'
            call w039c1(charge//'.CHME.EPSIN', ifi, form, ligrel, titre)
!
            titre='Chargement de SIGM_INTERNE'
            call w039c1(charge//'.CHME.SIINT', ifi, form, ligrel, titre)
!
            titre='Chargement de FORCE_ELEC'
            call w039c1(charge//'.CHME.FELEC', ifi, form, ligrel, titre)
!
            titre='Chargement de FLUX_THM_REP'
            call w039c1(charge//'.CHME.FLUX', ifi, form, ligrel, titre)
!
            titre='Chargement d''IMPE_FACE'
            call w039c1(charge//'.CHME.IMPE', ifi, form, ligrel, titre)
!
            titre='Chargement d''ONDE_FLUI'
            call w039c1(charge//'.CHME.ONDE', ifi, form, ligrel, titre)
!
        endif
    end do
!
!
!
 20 continue
    call detrsd('LIGREL', ligrel)
    call jedema()
end subroutine
