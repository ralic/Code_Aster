subroutine merith(model_, nb_load, list_name_, mate, cara_elem_,&
                  time_, matr_elem_, nh, base_)
    implicit none
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
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/load_neut_excl.h"
#include "asterfort/memare.h"
#include "asterfort/merit1.h"
#include "asterfort/merit2.h"
#include "asterfort/merit3.h"
#include "asterfort/reajre.h"
    character(len=*) :: list_name_(*), mate
    character(len=*) :: model_, cara_elem_, matr_elem_, base_, time_
    integer :: nb_load
! ----------------------------------------------------------------------
!
!     CALCUL DES MATRICES ELEMENTAIRES DE RIGIDITE THERMIQUE
!      MATEL:
!            ( ISO     , 'RIGIDI_TH'  )
!            ( CAL_TI  , 'DDLMUR_THER')
!            ( ISO_FACE, 'RIGITH_COEFR/F' )
!
!     ENTREES:
!
!     LES NOMS QUI SUIVENT SONT LES PREFIXES UTILISATEUR K8:
!        MODELZ : NOM DU MODELE
!        NCHAR  : NOMBRE DE CHARGES
!        LCHAR  : LISTE DES CHARGES
!        MATE   : CHAMP DE MATERIAUX
!        CARAZ  : CHAMP DE CARAC_ELEM
!        TIMEZ  : CHAMPS DE TEMPSR
!        MATELZ : NOM  DU  MATELE (N RESUELEM) PRODUIT
!                  ( ISO      , 'RIGIDI_TH'             )
!                  ( CAL_TI   , 'DDLMUR_THER'           )
!                  ( ISO_FACE , 'RIGIDI_TH_COEFHR/F'    )
!        NH     : NUMERO DE L'HARMONIQUE DE FOURIER(SI PAS FOURIER NH=0)
!
!     SORTIES:
!        MATELZ   : LE MATELE EST REMPLI.
!
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
!-----------------------------------------------------------------------
    integer :: i, iret,    long1, long2
    integer :: long3, nh, numor3
    character(len=8) :: model, cara_elem, k8_dummy
    character(len=19) :: matr_elem
    character(len=1) :: base
    character(len=24) :: time
    character(len=16) :: command
    character(len=24), pointer :: lire1(:) => null()
    character(len=24), pointer :: lire2(:) => null()
    character(len=24), pointer :: lire3(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    model     = model_
    cara_elem = cara_elem_
    time      = time_
    base      = base_
    matr_elem = matr_elem_
!
! - Current command
!
    call getres(k8_dummy, command, k8_dummy)
!
! - EVOL_CHAR is prohibden
!
    call load_neut_excl(command, list_nbload_ = nb_load, list_name_ = list_name_)
!
!     -- RIGIDITE CORRESPONDANT AUX ELEMENTS ISO ET AUX ELEMENTS CAL_TI:
    call merit1(model, nb_load, list_name_, mate, cara_elem,&
                time, '&MERITH1           ', nh, matr_elem, 0,&
                base)
    call jeexin('&MERITH1           .RELR', iret)
    long1=0
    if (iret .ne. 0) then
        call jelira('&MERITH1           .RELR', 'LONUTI', long1)
        call jeveuo('&MERITH1           .RELR', 'L', vk24=lire1)
    endif
!
!     -- RIGIDITE CORRESPONDANT AUX ELEMENTS D'ECHANGE:
    call merit2(model, nb_load, list_name_, cara_elem, time,&
                '&MERITH2           ', matr_elem, long1, base)
    call jeexin('&MERITH2           .RELR', iret)
    long2=0
    if (iret .ne. 0) then
        call jelira('&MERITH2           .RELR', 'LONUTI', long2)
        call jeveuo('&MERITH2           .RELR', 'L', vk24=lire2)
    endif
!
!     -- OPERATEUR ELEMENTAIRE DE CONVECTION NATURELLE:
    numor3 = long1 + long2
    call merit3(model, nb_load, list_name_, mate, cara_elem,&
                time, '&MERITH3           ', matr_elem, numor3, base)
    call jeexin('&MERITH3           .RELR', iret)
    long3=0
    if (iret .ne. 0) then
        call jelira('&MERITH3           .RELR', 'LONUTI', long3)
        call jeveuo('&MERITH3           .RELR', 'L', vk24=lire3)
    endif
!
!
!     -- ON RECOPIE LES .RELR DE &MERITH1, &MERITH2 ET
!     -- &MERITH3 DANS MATEL.
!
    call jedetr(matr_elem//'.RERR')
    call jedetr(matr_elem//'.RELR')
!
    call memare(base, matr_elem, model, mate, cara_elem,&
                'RIGI_THER')
!
    do i = 1,long1
        call reajre(matr_elem, lire1(i), base)
    end do
    do i = 1,long2
        call reajre(matr_elem, lire2(i), base)
    end do
    do i = 1,long3
        call reajre(matr_elem, lire3(i), base)
    end do
!
! --- MENAGE
    call jedetr('&MERITH1           .RELR')
    call jedetr('&MERITH2           .RELR')
    call jedetr('&MERITH3           .RELR')
    call jedetr('&MERITH1           .RERR')
    call jedetr('&MERITH2           .RERR')
    call jedetr('&MERITH3           .RERR')
!
    matr_elem_ = matr_elem
    call jedema()
end subroutine
