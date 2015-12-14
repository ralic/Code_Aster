subroutine cfinal(ds_contact, l_first_geom, l_pair, nbliac,&
                  llf, llf1, llf2)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cftabl.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer :: nbliac, llf, llf1, llf2
    aster_logical, intent(in) :: l_first_geom, l_pair
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - ALGORITHME)
!
! ACTIVATION DES LIAISONS INITIALES
!
! ----------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! IN  REAPRE : .TRUE. SI PREMIERE ACTUALISATION
! IN  REAGEO : .TRUE. SI ON VIENT DE FAIRE UN NOUVEL APPARIEMENT
! I/O NBLIAC : NOMBRE DE LIAISONS ACTIVES
! I/O LLF    : NOMBRE DE LIAISON DE FROTTEMENT (DEUX DIRECTIONS)
! I/O LLF1   : NOMBRE DE LIAISON DE FROTTEMENT (1ERE DIRECTION )
! I/O LLF2   : NOMBRE DE LIAISON DE FROTTEMENT (2EME DIRECTION )
!
!
!
!
    aster_logical :: liaact, liaexi
    real(kind=8) :: jeuini, jeuold, jeumin
    integer :: posit, ajliai, spliai, indic, btotin
    integer :: nbliai
    integer :: iliai, iliac
    aster_logical :: lgcp, llagrc, llagrf, lgliss
    character(len=1) :: typeaj
    character(len=2) :: typeli, typec0
    character(len=19) :: liac, typl
    integer :: jliac, jtypl
    character(len=24) :: jeuite, jeux
    integer :: jjeuit, jjeux
    character(len=24) :: numlia
    integer :: jnumli
    character(len=19) :: statfr
    integer :: jstfr
    integer :: posnoe
    character(len=2) :: typlia
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    liac = ds_contact%sdcont_solv(1:14)//'.LIAC'
    typl = ds_contact%sdcont_solv(1:14)//'.TYPL'
    call jeveuo(liac, 'L', jliac)
    call jeveuo(typl, 'L', jtypl)
    jeuite = ds_contact%sdcont_solv(1:14)//'.JEUITE'
    jeux = ds_contact%sdcont_solv(1:14)//'.JEUX'
    call jeveuo(jeuite, 'L', jjeuit)
    call jeveuo(jeux, 'L', jjeux)
!
    numlia = ds_contact%sdcont_solv(1:14)//'.NUMLIA'
    call jeveuo(numlia, 'L', jnumli)
!
    if (l_first_geom) then
        statfr = ds_contact%sdcont_solv(1:14)//'.STF0'
    else
        statfr = ds_contact%sdcont_solv(1:14)//'.STFR'
    endif
!
! --- INITIALISATIONS
!
    jeumin = r8prem()
    posit = 0
    btotin = nbliac + llf + llf1 + llf2
    typeaj = 'A'
    typec0 = 'C0'
    spliai = 0
    ajliai = 0
!
! --- PARAMETRES
!
    nbliai = cfdisd(ds_contact%sdcont_solv,'NBLIAI' )
    lgcp = cfdisl(ds_contact%sdcont_defi,'CONT_GCP')
    llagrc = cfdisl(ds_contact%sdcont_defi,'CONT_LAGR')
    llagrf = cfdisl(ds_contact%sdcont_defi,'FROT_LAGR')
    lgliss = cfdisl(ds_contact%sdcont_defi,'CONT_DISC_GLIS')
!
! --- DETECTION DES COUPLES DE NOEUDS INTERPENETRES
!
    do iliai = 1, nbliai
!
! ----- JEU SANS CORRECTION DU CONTACT
!
        jeuini = zr(jjeux+3*(iliai-1)+1-1)
!
! ----- JEU AVANT L'ITERATION DE NEWTON
!
        jeuold = zr(jjeuit+3*(iliai-1)+1-1)
!
! ----- LIAISON ACTIVEE ?
!
        liaact = .false.
        if (lgcp) then
            liaact = .true.
        else if (llagrc) then
            if (jeuold .lt. jeumin) then
                liaact = .true.
            else
                liaact = .false.
            endif
        else
            if (jeuini .lt. jeumin) then
                liaact = .true.
            else
                liaact = .false.
            endif
        endif
!
! ----- LIAISON GLISSIERE -> TOUTES LES LIAISONS SONT ACTIVEES
!
        if (lgliss) then
            liaact = .true.
        endif
!
! ----- LA LIAISON EXISTE-T-ELLE DEJA ?
!
        liaexi = .false.
        do iliac = 1, btotin
            if (zi(jliac-1+iliac) .eq. iliai) then
                typeli = zk8(jtypl-1+iliac)(1:2)
                if (typeli .eq. typec0) liaexi = .true.
            endif
        end do
!
! ----- SI LAGRANGIEN: ON ACTIVE UNE LIAISON QUE SI ON EST APRES
! ----- UN NOUVEL APPARIEMENT
!
        if (llagrc .and. liaact) then
            if (l_pair) then
! --------- LA LIAISON N'EXISTE PAS ENCORE, FORCEMENT
                if (liaexi) then
                    ASSERT(.false.)
                endif
            else
                liaact = .false.
            endif
        endif
!
! ----- INDICE DE LA NOUVELLE LIAISON ACTIVE
!
        if (liaact) then
            if (lgcp) then
                posit = iliai
            else
                posit = nbliac + llf + llf1 + llf2 + 1
            endif
        endif
!
! ----- ACTIVATION DE LA LIAISON DE CONTACT
!
        if (liaact) then
            call cftabl(indic, nbliac, ajliai, spliai, llf,&
                        llf1, llf2, ds_contact%sdcont_solv, typeaj, posit,&
                        iliai, typec0)
        endif
!
    end do
!
! --- EN LAGRANGIEN
! --- L'ETAT DES LIAISONS DE FROTTEMENT EST CONSERVE
! --- APRES UN APPARIEMENT ON LE TRANSFERE
!
! --- ATTENTION IL FAUDRA GERER LE PB DU REDECOUPAGE
!
    if (llagrf) then
        call jeveuo(statfr, 'E', jstfr)
        if (l_pair) then
            do iliac = 1, nbliac
                ASSERT(zk8(jtypl-1+iliac).eq.'C0')
                iliai = zi(jliac -1+iliac)
                posnoe = zi(jnumli-1+4*(iliai-1)+2)
                typlia = zk8(jstfr -1+posnoe)(1:2)
                if (typlia .ne. ' ') then
                    ASSERT(typlia .eq. 'F0' .or. typlia .eq. 'F1' .or. typlia .eq. 'F2')
                    posit = nbliac + llf + llf1 + llf2 + 1
                    call cftabl(indic, nbliac, ajliai, spliai, llf,&
                                llf1, llf2, ds_contact%sdcont_solv, typeaj, posit,&
                                iliai, typlia)
                endif
            end do
        endif
    endif
!
    call jedema()
!
end subroutine
