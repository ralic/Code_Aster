subroutine cfdist(defico, method, izone, posnoe, posmae,&
                  coord, dist)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdism.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfl.h"
    character(len=24) :: defico
    character(len=8) :: method
    integer :: izone
    integer :: posnoe, posmae
    real(kind=8) :: dist, coord(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - APPARIEMENT)
!
! CALCUL DU JEU SUPPLEMENTAIRE
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  METHOD : METHODE DE CONTACT
!               'CONTINUE'
!               'DISCRETE'
! IN  IZONE  : ZONE DE CONTACT
! IN  POSNOE : INDICE DU NOEUD ESCLAVE DANS CONTNO
! IN  POSMAE : INDICE DE LA MAILLE ESCLAVE DANS CONTMA
! IN  COORD  : VALEUR DES COORDONNEES DU NOEUD COURANT
! OUT DIST   : JEU SUPPLEMENTAIRE
!
!
!
!
!
    integer :: ier
    character(len=24) :: jeupou, jeucoq
    integer :: jjpou, jjcoq
    character(len=24) :: jeufo1, jeufo2
    integer :: jjfo1, jjfo2
    character(len=8) :: jeuf1, jeuf2
    character(len=8) :: nompar(3)
    real(kind=8) :: valpar(3)
    real(kind=8) :: dist1, dist2, distst
    logical :: ldpou, ldcoq, ldescl, ldmait
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE DES SD POUR LE CONTACT POTENTIEL
!
    jeucoq = defico(1:16)//'.JEUCOQ'
    jeupou = defico(1:16)//'.JEUPOU'
    jeufo1 = defico(1:16)//'.JFO1CO'
    jeufo2 = defico(1:16)//'.JFO2CO'
!
    call jeveuo(jeupou, 'L', jjpou)
    call jeveuo(jeucoq, 'L', jjcoq)
    call jeveuo(jeufo1, 'L', jjfo1)
    call jeveuo(jeufo2, 'L', jjfo2)
!
! --- INITIALISATIONS
!
    dist1 = 0.d0
    dist2 = 0.d0
    distst = 0.d0
!
! --- EN VUE DE L'INTERPOLATION DU JEU PAR DES VARIABLES D'ESPACE
!
    nompar(1) = 'X'
    nompar(2) = 'Y'
    nompar(3) = 'Z'
    valpar(1) = coord(1)
    valpar(2) = coord(2)
    valpar(3) = coord(3)
!
! --- TYPES DE JEUX SUPS
!
    ldpou = mminfl(defico,'DIST_POUTRE',izone)
    ldcoq = mminfl(defico,'DIST_COQUE' ,izone)
    ldmait = mminfl(defico,'DIST_MAIT' ,izone)
    ldescl = mminfl(defico,'DIST_ESCL' ,izone)
!
! --- VALEUR DU JEU SUPPLEMENTAIRE SI C'EST UNE FONCTION DE L'ESPACE
!
    if (ldmait) then
        jeuf1 = zk8(jjfo1+izone-1)
        call fointe('F', jeuf1, 3, nompar, valpar,&
                    dist1, ier)
    endif
!
! --- VALEUR DU JEU SUPPLEMENTAIRE SI C'EST UNE FONCTION DE L'ESPACE
!
    if (ldescl) then
        jeuf2 = zk8(jjfo2+izone-1)
        call fointe('F', jeuf2, 3, nompar, valpar,&
                    dist2, ier)
    endif
!
! --- VALEUR DU JEU SUPPLEMENTAIRE SI DIST_POUTRE/DIST_COQUE
!
    if (ldcoq .or. ldpou) then
        if (method .eq. 'DISCRETE') then
            call cfdism(defico, ldpou, ldcoq, posnoe, distst)
        else if (method.eq.'CONTINUE') then
            call assert(posnoe.eq.0)
            if (ldpou) then
                distst = distst+zr(jjpou-1+posmae)
            endif
            if (ldcoq) then
                distst = distst+zr(jjcoq-1+posmae)
            endif
        else
            call assert(.false.)
        endif
    endif
!
! --- TOTAL JEU SUPPLEMENTAIRE
!
    dist = dist1 + dist2 + distst
!
    call jedema()
end subroutine
