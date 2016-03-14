subroutine cfapre(noma, ds_contact, newgeo, sdappa, instan)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/apinfi.h"
#include "asterfort/apinfr.h"
#include "asterfort/apvect.h"
#include "asterfort/assert.h"
#include "asterfort/cfapma.h"
#include "asterfort/cfapno.h"
#include "asterfort/cfappi.h"
#include "asterfort/cfcorn.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfdist.h"
#include "asterfort/cfecrd.h"
#include "asterfort/cfmmco.h"
#include "asterfort/cfnumn.h"
#include "asterfort/cfparz.h"
#include "asterfort/infdbg.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mminfr.h"
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
!
    character(len=8) :: noma
    character(len=19) :: sdappa
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19) :: newgeo
    real(kind=8) :: instan
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT)
!
! RECOPIE DE LA SD APPARIEMENT DEDIEE POUR LE CONTACT
!
! ----------------------------------------------------------------------
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  NEWGEO : NOUVELLE GEOMETRIE (AVEC DEPLACEMENT GEOMETRIQUE)
! In  ds_contact       : datastructure for contact management
!
! ----------------------------------------------------------------------
!
    integer :: izone, i, iliai, ip, ifm, niv
    integer :: jdecne
    integer :: inoe
    integer :: posmae, posnoe(1), posmam, posnom(1)
    integer :: numnoe(1)
    integer :: entapp, typapp
    aster_logical :: lctfd
    integer :: nzoco, ndimg, nbpt, nbliai
    integer :: nesmax
    aster_logical :: lveri
    character(len=8) :: nomnoe
    real(kind=8) :: ksipr1, ksipr2, tau1m(3), tau2m(3)
    real(kind=8) :: coorne(3), gap_user
    real(kind=8) :: coefff, coefpn, coefpt, coefte
!
! ----------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ......... RECOPIE DE L''APPARIEMENT'
    endif
!
! --- INFOS SUR LA CHARGE DE CONTACT
!
    lctfd = cfdisl(ds_contact%sdcont_defi,'FROT_DISCRET')
!
! --- NOMBRE TOTAL DE NOEUDS ESCLAVES ET DIMENSION DU PROBLEME
!
    nzoco = cfdisi(ds_contact%sdcont_defi,'NZOCO' )
    ndimg = cfdisi(ds_contact%sdcont_defi,'NDIM' )
!
! --- INITIALISATIONS
!
    ip = 1
    iliai = 0
    posmae = 0
!
! --- BOUCLE SUR LES ZONES
!
    do izone = 1, nzoco
!
! ----- INFORMATION SUR LA ZONE
!
        nbpt = mminfi(ds_contact%sdcont_defi,'NBPT' ,izone )
        jdecne = mminfi(ds_contact%sdcont_defi,'JDECNE',izone )
!
! ----- MODE VERIF: ON SAUTE LES POINTS
!
        lveri = mminfl(ds_contact%sdcont_defi,'VERIF',izone )
        if (lveri) then
            ip = ip + nbpt
            goto 25
        endif
!
! ----- COEFFICIENTS
!
        call cfmmco(ds_contact, izone, 'E_N', 'L', coefpn)
        call cfmmco(ds_contact, izone, 'E_T', 'L', coefpt)
        coefff = mminfr(ds_contact%sdcont_defi,'COEF_COULOMB' ,izone )
        coefte = mminfr(ds_contact%sdcont_defi,'COEF_MATR_FROT' ,izone )
!
! ----- BOUCLE SUR LES NOEUDS DE CONTACT
!
        do i = 1, nbpt
!
! ------- NOEUD ESCLAVE COURANT
!
            inoe = i
            posnoe = jdecne + inoe
!
! ------- INDICE ABSOLU DANS LE MAILLAGE DU NOEUD
!
            call cfnumn(ds_contact%sdcont_defi, 1, posnoe(1), numnoe(1))
!
! ------- COORDONNEES DU NOEUD
!
            call cfcorn(newgeo, numnoe(1), coorne)
!
! ------- NOM DU NOEUD
!
            call jenuno(jexnum(noma//'.NOMNOE', numnoe(1)), nomnoe)
!
! ------- INFOS APPARIEMENT
!
            call apinfi(sdappa, 'APPARI_TYPE', ip, typapp)
            call apinfi(sdappa, 'APPARI_ENTITE', ip, entapp)
            call apinfr(sdappa, 'APPARI_PROJ_KSI1', ip, ksipr1)
            call apinfr(sdappa, 'APPARI_PROJ_KSI2', ip, ksipr2)
            call apvect(sdappa, 'APPARI_TAU1', ip, tau1m)
            call apvect(sdappa, 'APPARI_TAU2', ip, tau2m)
!
! ------- RECOPIE APPARIEMENT
!
            if (typapp .lt. 0) then
                if (niv .ge. 2) then
                    call cfappi(noma, ds_contact%sdcont_defi, nomnoe, typapp, entapp)
                endif
                goto 35
            else if (typapp.eq.1) then
! --------- CARAC. MAITRE
                posnom = entapp
! --------- LIAISON DE CONTACT EFFECTIVE
                iliai = iliai + 1
! --------- CALCUL LIAISON
                call cfapno(noma, newgeo, ds_contact, lctfd,&
                            ndimg, izone, posnoe(1), numnoe(1),&
                            coorne, posnom(1), tau1m, tau2m, iliai)
!
            else if (typapp.eq.2) then
! --------- CARAC. MAITRE
                posmam = entapp
! --------- LIAISON DE CONTACT EFFECTIVE
                iliai = iliai + 1
! --------- CALCUL LIAISON
                call cfapma(noma, newgeo, ds_contact, lctfd,&
                            ndimg, izone, posnoe(1), numnoe(1),&
                            coorne, posmam, ksipr1, ksipr2, tau1m,&
                            tau2m, iliai)
            else
                ASSERT(.false.)
            endif
!
! ------- CALCUL DU JEU FICTIF DE LA ZONE
!
            call cfdist(ds_contact, izone, posmae, coorne, instan, &
                        gap_user, node_slav_indx_ = posnoe(1))
!
! ------- CARACTERISTIQUES DE LA LIAISON POUR LA ZONE
!
            call cfparz(ds_contact, iliai, coefff, coefpn, coefpt,&
                        coefte, gap_user, izone, ip, numnoe(1),&
                        posnoe(1))
!
 35         continue
!
! ------- POINT SUIVANT
!
            ip = ip + 1
            ASSERT(iliai.le.ip)
!
        end do
 25     continue
    end do
!
! --- NOMBRE DE LIAISONS EFFECTIVES
!
    nbliai = iliai
    call cfecrd(ds_contact%sdcont_solv, 'NBLIAI', nbliai)
    nesmax = cfdisd(ds_contact%sdcont_solv,'NESMAX')
    ASSERT(nbliai.le.nesmax)
!
end subroutine
