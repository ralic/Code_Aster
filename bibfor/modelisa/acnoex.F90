subroutine acnoex(noma, type, liste, nb, no1,&
                  no2)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: noma
    character(len=24) :: liste(*)
    character(len=4) :: type
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     RECHERCHE DES NOEUDS EXTREMITES D'UNE LISTE DE MAILLES
!     UTILISE PAR DEFI_ARC
! ----------------------------------------------------------------------
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : TYPE   : 'TOUT', 'GRMA', 'LIMA'
! IN  : LISTE  : VECTEUR DE K24( NB) : LISTE DES MAILLES OU GROUPES
! IN  : NB     : DIMENSION DE LISTE
! OUT : NO1    : NOEUD EXTREMITE DE L'ENSEMBLE DES MAILLES
! OUT : NO2    : NOEUD AUTRE EXTREMITE DE L'ENSEMBLE DES MAILLES
! ----------------------------------------------------------------------
    character(len=24) :: mlggma, mlgnma, mlgcnx
    character(len=24) :: valk
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ig, im, img, in, ir, jdgm, jdno
    integer :: jnbn, nb, nbm, nbn, nm, nn, nn1
    integer :: nn2, no1, no2, nummai
!-----------------------------------------------------------------------
    call jemarq()
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbn, arret='C',&
                ier=ir)
    call wkvect('&&ACNOEX', 'V V I', nbn, jnbn)
    mlggma = noma//'.GROUPEMA'
    mlgnma = noma//'.NOMMAI'
    mlgcnx = noma//'.CONNEX'
    if (type .eq. 'TOUT') then
        call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbm, arret='C',&
                    ier=ir)
        do im = 1, nbm
            call jeveuo(jexnum(mlgcnx, im), 'L', jdno)
            nn1 = zi(jdno)
            nn2 = zi(jdno+1)
            zi(jnbn+nn1-1)=zi(jnbn+nn1-1)+1
            zi(jnbn+nn2-1)=zi(jnbn+nn2-1)+1
        end do
    else if (type.eq.'GRMA') then
        do ig = 1, nb
            call jeveuo(jexnom(mlggma, liste(ig)), 'L', jdgm)
            call jelira(jexnom(mlggma, liste(ig)), 'LONUTI', nm)
            do im = 1, nm
                img = zi(jdgm+im-1)
                call jeveuo(jexnum(mlgcnx, img), 'L', jdno)
                nn1 = zi(jdno)
                nn2 = zi(jdno+1)
                zi(jnbn+nn1-1)=zi(jnbn+nn1-1)+1
                zi(jnbn+nn2-1)=zi(jnbn+nn2-1)+1
            end do
        end do
    else if (type.eq.'LIMA') then
        do im = 1, nb
            call jenonu(jexnom(mlgnma, liste(im)), nummai)
            call jeveuo(jexnum(mlgcnx, nummai), 'L', jdno)
            nn1 = zi(jdno)
            nn2 = zi(jdno+1)
            zi(jnbn+nn1-1)=zi(jnbn+nn1-1)+1
            zi(jnbn+nn2-1)=zi(jnbn+nn2-1)+1
        end do
    endif
!
!     RECHERCHE DES EXTREMITES DE L'ARC
!
    no1 = 0
    no2 = 0
    do in = 1, nbn
        nn = zi(jnbn+in-1)
        if (nn .eq. 1) then
            if (no1 .eq. 0) then
                no1 = in
            else if (no2.eq.0) then
                no2 = in
            else
                valk = ' '
                call utmess('E', 'MODELISA8_25', sk=valk)
            endif
        endif
    end do
!
!     CAS OU LES EXTREMITES DE L'ARC SONT IDENTIQUES
!     ARC FERME
!
    if (no1 .eq. no2) then
        valk = ' '
        call utmess('E', 'MODELISA8_26', sk=valk)
    endif
    call jedetr('&&ACNOEX')
    call jedema()
end subroutine
