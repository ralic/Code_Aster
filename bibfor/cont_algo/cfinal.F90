subroutine cfinal(ds_contact, nbliac)
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
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer :: nbliac
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
! I/O NBLIAC : NOMBRE DE LIAISONS ACTIVES
!
!
    aster_logical :: liaact, liaexi
    real(kind=8) :: jeuini, jeumin
    integer :: posit, ajliai, spliai, indic, nbliac_init
    integer :: nbliai
    integer :: iliai, iliac
    aster_logical :: lgcp, lgliss
    character(len=1) :: typeaj
    character(len=19) :: liac
    integer :: jliac
    character(len=24) :: jeuite, jeux
    integer :: jjeuit, jjeux
    character(len=24) :: numlia
    integer :: jnumli
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    liac = ds_contact%sdcont_solv(1:14)//'.LIAC'
    call jeveuo(liac, 'L', jliac)
    jeuite = ds_contact%sdcont_solv(1:14)//'.JEUITE'
    jeux = ds_contact%sdcont_solv(1:14)//'.JEUX'
    call jeveuo(jeuite, 'L', jjeuit)
    call jeveuo(jeux, 'L', jjeux)
    numlia = ds_contact%sdcont_solv(1:14)//'.NUMLIA'
    call jeveuo(numlia, 'L', jnumli)
!
! --- INITIALISATIONS
!
    jeumin = r8prem()
    posit = 0
    typeaj = 'A'
    spliai = 0
    ajliai = 0
    nbliac_init = nbliac
!
! --- PARAMETRES
!
    nbliai = cfdisd(ds_contact%sdcont_solv,'NBLIAI' )
    lgcp = cfdisl(ds_contact%sdcont_defi,'CONT_GCP')
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
! ----- LIAISON ACTIVEE ?
!
        liaact = .false.
        if (lgcp) then
            liaact = .true.
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
        do iliac = 1, nbliac_init
            if (zi(jliac-1+iliac) .eq. iliai) then
                liaexi = .true.
            endif
        end do
!
! ----- INDICE DE LA NOUVELLE LIAISON ACTIVE
!
        if (liaact) then
            if (lgcp) then
                posit = iliai
            else
                posit = nbliac + 1
            endif
        endif
!
! ----- ACTIVATION DE LA LIAISON DE CONTACT
!
        if (liaact) then
            call cftabl(indic, nbliac, ajliai, spliai, &
                        ds_contact%sdcont_solv, typeaj, posit,&
                        iliai)
        endif
!
    end do
!
    call jedema()
!
end subroutine
