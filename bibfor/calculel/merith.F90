subroutine merith(modelz, nchar, lchar, mate, caraz,&
                  timez, matelz, nh, basez)
    implicit none
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
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/memare.h"
#include "asterfort/merit1.h"
#include "asterfort/merit2.h"
#include "asterfort/merit3.h"
#include "asterfort/reajre.h"
    character(len=*) :: lchar(*), mate
    character(len=*) :: modelz, caraz, matelz, basez, timez
    character(len=8) :: modele, cara
    character(len=19) :: matel
    character(len=1) :: base
    character(len=24) :: time
    integer :: nchar
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
    character(len=8) :: k8bid
!-----------------------------------------------------------------------
    integer :: i, iret, jlire1, jlire2, jlire3, long1, long2
    integer :: long3, nh, numor3
!-----------------------------------------------------------------------
    call jemarq()
    modele = modelz
    cara = caraz
    time = timez
    base = basez
    matel = matelz
!
!     -- RIGIDITE CORRESPONDANT AUX ELEMENTS ISO ET AUX ELEMENTS CAL_TI:
    call merit1(modele, nchar, lchar, mate, cara,&
                time, '&MERITH1           ', nh, matel, 0,&
                base)
    call jeexin('&MERITH1           .RELR', iret)
    long1=0
    if (iret .ne. 0) then
        call jelira('&MERITH1           .RELR', 'LONUTI', long1, k8bid)
        call jeveuo('&MERITH1           .RELR', 'L', jlire1)
    endif
!
!     -- RIGIDITE CORRESPONDANT AUX ELEMENTS D'ECHANGE:
    call merit2(modele, nchar, lchar, cara, time,&
                '&MERITH2           ', matel, long1, base)
    call jeexin('&MERITH2           .RELR', iret)
    long2=0
    if (iret .ne. 0) then
        call jelira('&MERITH2           .RELR', 'LONUTI', long2, k8bid)
        call jeveuo('&MERITH2           .RELR', 'L', jlire2)
    endif
!
!     -- OPERATEUR ELEMENTAIRE DE CONVECTION NATURELLE:
    numor3 = long1 + long2
    call merit3(modele, nchar, lchar, mate, cara,&
                time, '&MERITH3           ', matel, numor3, base)
    call jeexin('&MERITH3           .RELR', iret)
    long3=0
    if (iret .ne. 0) then
        call jelira('&MERITH3           .RELR', 'LONUTI', long3, k8bid)
        call jeveuo('&MERITH3           .RELR', 'L', jlire3)
    endif
!
!
!     -- ON RECOPIE LES .RELR DE &MERITH1, &MERITH2 ET
!     -- &MERITH3 DANS MATEL.
!
    call jedetr(matel//'.RERR')
    call jedetr(matel//'.RELR')
!
    call memare(base, matel, modele, mate, cara,&
                'RIGI_THER')
!
    do 1,i = 1,long1
    call reajre(matel, zk24(jlire1-1+i), base)
    1 end do
    do 2,i = 1,long2
    call reajre(matel, zk24(jlire2-1+i), base)
    2 end do
    do 3,i = 1,long3
    call reajre(matel, zk24(jlire3-1+i), base)
    3 end do
!
! --- MENAGE
    call jedetr('&MERITH1           .RELR')
    call jedetr('&MERITH2           .RELR')
    call jedetr('&MERITH3           .RELR')
    call jedetr('&MERITH1           .RERR')
    call jedetr('&MERITH2           .RERR')
    call jedetr('&MERITH3           .RERR')
!
    matelz = matel
    call jedema()
end subroutine
