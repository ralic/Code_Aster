subroutine cfdism(defico, ldpou, ldcoq, posnoe, distst)
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
#include "asterfort/cfinvm.h"
#include "asterfort/cfnben.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: defico
    integer :: posnoe
    real(kind=8) :: distst
    aster_logical :: ldpou, ldcoq
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - APPARIEMENT)
!
! CALCUL DU JEU SUPPLEMENTAIRE POUR POUTRE/COQUE
! SUR UN NOEUD ESCLAVE DONNE
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  POSNOE : INDICE DU NOEUD ESCLAVE DANS CONTNO
! IN  LDPOU  : SI DIST_POUTRE
! IN  LDCOQ  : SI DIST_COQUE
! OUT DISTST : JEU SUPPLEMENTAIRE DU AUX POUTRES/COQUES
!
!
!
!
!
    character(len=24) :: jeupou, jeucoq
    integer :: jjpou, jjcoq
    integer :: ntmae, jdeciv, imae, posmae
    real(kind=8) :: distma
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE DES SD POUR LE CONTACT POTENTIEL
!
    jeucoq = defico(1:16)//'.JEUCOQ'
    jeupou = defico(1:16)//'.JEUPOU'
!
    call jeveuo(jeupou, 'L', jjpou)
    call jeveuo(jeucoq, 'L', jjcoq)
!
! --- INITIALISATIONS
!
    distma = 0.d0
!
! --- NOMBRE DE MAILLES ATTACHEES AU NOEUD
! --- DECALAGE POUR ACCES AU TABLEAU
!
    call cfnben(defico, posnoe, 'CONINV', ntmae, jdeciv)
!
! --- BOUCLE SUR LES MAILLES MAITRES
!
    do 50 imae = 1, ntmae
        call cfinvm(defico, jdeciv, imae, posmae)
        if (ldpou) then
            distma = distma+zr(jjpou-1+posmae)
        endif
        if (ldcoq) then
            distma = distma+zr(jjcoq-1+posmae)
        endif
 50 end do
!
! --- MOYENNE SIMPLE
!
    distst = distma/ntmae
!
    call jedema()
end subroutine
