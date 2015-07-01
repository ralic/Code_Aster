subroutine cftanr(noma, ndimg, defico, resoco, izone,&
                  posnoe, typenm, posenm, numenm, ksipr1,&
                  ksipr2, tau1m, tau2m, tau1, tau2)
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
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/apvect.h"
#include "asterfort/assert.h"
#include "asterfort/cfchno.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfinvm.h"
#include "asterfort/cfnben.h"
#include "asterfort/cfnomm.h"
#include "asterfort/cfnors.h"
#include "asterfort/cfnumm.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mmelty.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mminfr.h"
#include "asterfort/utmess.h"
    character(len=8) :: noma
    integer :: posenm, posnoe, numenm
    integer :: izone
    integer :: ndimg
    real(kind=8) :: ksipr1, ksipr2
    character(len=4) :: typenm
    character(len=24) :: defico, resoco
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: tau1m(3), tau2m(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - APPARIEMENT)
!
! MOD. LES VECTEURS TANGENTS RESULTANTS SUIVANT OPTIONS
!
! ----------------------------------------------------------------------
!
!  NB: LE REPERE EST ORTHONORME ET TEL QUE LA NORMALE POINTE VERS
!  L'INTERIEUR DE LA MAILLE
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NDIMG  : DIMENSION DU MODELE
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  IZONE  : ZONE DE CONTACT ACTIVE
! IN  TYPENM : TYPE DE L'ENTITE MAITRE RECEVANT LA PROJECTION
!               'MAIL' UNE MAILLE
!               'NOEU' UN NOEUD
! IN  POSENM : NUMERO ENTITE MAITRE QUI RECOIT LA PROJECTION
! IN  NUMENM : NUMERO ABSOLU ENTITE MAITRE QUI RECOIT LA PROJECTION
! IN  POSNOE : NOEUD ESCLAVE
! IN  KSIPR1 : COORDONNEE PARAMETRIQUE SUR MAITRE DU POINT ESCLAVE
!              PROJETE
! IN  KSIPR2 : COORDONNEE PARAMETRIQUE SUR MAITRE DU POINT ESCLAVE
!              PROJETE
! IN  TAU1M  : PREMIERE TANGENTE SUR LA MAILLE MAITRE AU POINT ESCLAVE
!              PROJETE
! IN  TAU2M  : SECONDE TANGENTE SUR LA MAILLE MAITRE AU POINT ESCLAVE
!              PROJETE
! OUT TAU1   : PREMIER VECTEUR TANGENT LOCAL AU POINT ESCLAVE PROJETE
! OUT TAU2   : SECOND VECTEUR TANGENT LOCAL AU POINT ESCLAVE PROJETE
!
!
!
!
    aster_logical :: lliss, lmfixe, lefixe, lmait, lescl
    aster_logical :: lpoutr, lpoint
    integer :: ima
    integer :: posmam, posmae, nummae, nummam
    integer :: itypem, itypee
    character(len=8) :: aliase, aliasm, nommam, nommae
    real(kind=8) :: r8bid, vector(3)
    real(kind=8) :: tau1e(3), tau2e(3)
    character(len=19) :: sdappa
    integer :: nbma, jdeciv
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE APPARIEMENT
!
    sdappa = resoco(1:14)//'.APPA'
!
! --- INITIALISATIONS
!
    lmfixe = .false.
    lefixe = .false.
    lmait = .false.
    lescl = .false.
    vector(1) = 0.d0
    vector(2) = 0.d0
    vector(3) = 0.d0
!
! --- LISSAGE OU PAS ?
!
    lliss = cfdisl(defico,'LISSAGE')
!
! --- NORMALES A MODIFIER
!
    if (mminfl(defico,'MAIT',izone)) then
        lmait = .true.
        lescl = .false.
    else if (mminfl(defico,'MAIT_ESCL',izone)) then
        lmait = .true.
        lescl = .true.
    else if (mminfl(defico,'ESCL',izone)) then
        lmait = .false.
        lescl = .true.
    else
        ASSERT(.false.)
    endif
!
! --- INCOMPATIBILITE SCHEMA INTEGRATION GAUSS AVEC OPTION ESCLAVE
!
    if (lescl) then
        if (posnoe .eq. 0) then
            call utmess('F', 'CONTACT_98')
        else if (posnoe.eq.-1) then
            call utmess('F', 'CONTACT_99')
        endif
    endif
!
! --- DIMENSION MAILLE ESCLAVE: ON PREND LA PREMIERE MAILLE ATTACHEE
! --- AU NOEUD ESCLAVE
!
    if (lescl) then
        call cfnben(defico, posnoe, 'CONINV', nbma, jdeciv)
        ima = 1
        call cfinvm(defico, jdeciv, ima, posmae)
        call cfnumm(defico, posmae, nummae)
        call cfnomm(noma, defico, 'MAIL', posmae, nommae)
        call mmelty(noma, nummae, aliase)
    endif
!
! --- DIMENSION MAILLE MAITRE
!
    if (lmait) then
!
! --- RECUP. MAILLE SI APPARIEMENT NODAL
!
        if (typenm .eq. 'NOEU') then
            call cfnben(defico, posenm, 'CONINV', nbma, jdeciv)
            ima = 1
            call cfinvm(defico, jdeciv, ima, posmam)
        else
            posmam = posenm
        endif
        call cfnumm(defico, posmam, nummam)
        call cfnomm(noma, defico, 'MAIL', posmam, nommam)
        call mmelty(noma, nummam, aliasm)
    endif
!
! --- RECUPERATION TANGENTES ESCLAVES SI NECESSSAIRE
!
    if (lescl) then
        call apvect(sdappa, 'APPARI_NOEUD_TAU1', posnoe, tau1e)
        call apvect(sdappa, 'APPARI_NOEUD_TAU2', posnoe, tau2e)
    endif
!
! --- MODIFICATIONS DES NORMALES MAITRES
!
    if (lmait) then
        itypem = mminfi(defico,'VECT_MAIT',izone)
        if (itypem .ne. 0) then
            vector(1) = mminfr(defico,'VECT_MAIT_DIRX',izone)
            vector(2) = mminfr(defico,'VECT_MAIT_DIRY',izone)
            vector(3) = mminfr(defico,'VECT_MAIT_DIRZ',izone)
        endif
        if ((ndimg.eq.2) .and. (itypem.eq.2)) then
            call utmess('F', 'CONTACT3_43', sk=nommam)
        endif
        lpoutr = (ndimg.eq.3).and.(aliasm(1:2).eq.'SE')
        lpoint = aliasm.eq.'POI1'
        if (lpoint) then
            call utmess('F', 'CONTACT3_75', sk=nommam)
        endif
        call cfnors(noma, defico, resoco, posmam, typenm,&
                    numenm, lpoutr, lpoint, ksipr1, ksipr2,&
                    lliss, itypem, vector, tau1m, tau2m,&
                    lmfixe)
    endif
!
! --- MODIFICATIONS DES NORMALES ESCLAVES
!
    if (lescl) then
        itypee = mminfi(defico,'VECT_ESCL',izone)
        if (itypee .ne. 0) then
            vector(1) = mminfr(defico,'VECT_ESCL_DIRX',izone)
            vector(2) = mminfr(defico,'VECT_ESCL_DIRY',izone)
            vector(3) = mminfr(defico,'VECT_ESCL_DIRZ',izone)
        endif
        if ((ndimg.eq.2) .and. (itypee.eq.2)) then
            call utmess('F', 'CONTACT3_43', sk=nommae)
        endif
        lpoutr = (ndimg.eq.3).and.(aliase(1:2).eq.'SE')
        lpoint = aliase.eq.'POI1'
        call cfnors(noma, defico, resoco, posmae, typenm,&
                    numenm, lpoutr, lpoint, r8bid, r8bid,&
                    .false._1, itypee, vector, tau1e, tau2e,&
                    lefixe)
    endif
!
! --- CHOIX DE LA NORMALE -> CALCUL DES TANGENTES
!
    call cfchno(noma, defico, ndimg, posnoe, typenm,&
                numenm, lmait, lescl, lmfixe, lefixe,&
                tau1m, tau2m, tau1e, tau2e, tau1,&
                tau2)
!
    call jedema()
!
end subroutine
