subroutine obclcr(subccn, typcol, larcol, tithau, titli1,&
                  titli2, titli3, typval, cnoval, sdcolo)
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
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/obcrea.h"
#include "asterfort/oblgen.h"
#include "asterfort/obsetb.h"
#include "asterfort/obseti.h"
#include "asterfort/obsetk.h"
    character(len=6) :: subccn
    character(len=15) :: typcol
    integer :: larcol, tithau
    character(len=16) :: titli1, titli2, titli3
    character(len=1) :: typval
    character(len=4) :: cnoval
    character(len=24) :: sdcolo
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (GESTION STRUCTS - TABLEAU_COLONNE)
!
! GENERATION D'UNE COLONNE
!
! ----------------------------------------------------------------------
!
!
! IN  SUBCCN : ROUTINE DE CREATION
! IN  TYPCOL : CODE POUR TYPE DE COLONNE
! IN  LARCOL : LARGEUR DE LA COLONNE
! IN  TITHAU : HAUTEUR DU TITRE
! IN  TITLI1 : LIGNE 1 DU TITRE DE LA COLONNE
! IN  TITLI2 : LIGNE 2 DU TITRE DE LA COLONNE
! IN  TITLI3 : LIGNE 3 DU TITRE DE LA COLONNE
! IN  TYPVAL : TYPE DE LA VALEUR DANS LA COLONNE (I/R/K)
! IN  CNOVAL : COMPORTEMENT EN CAS DE VALEUR NON AFFECTEE AU MOMENT DE
!              L'IMPRESSION DE LA COLONNE
!              'VIDE' - ON IMPRIMERA RIEN (BLANC)
!              'ERRE' - PROVOQUERA UNE ERREUR A U
!              'SANS' - ON IMPRIMERA 'SANS OBJET'
! OUT SDCOLO : SD COLONNE D'UN TABLEAU
!
! ----------------------------------------------------------------------
!
    logical :: linte, lreel, lchai
    logical :: lnvvid, lnverr, lnvsan
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- GENERATION DU STRUCT AVEC NOM AUTOMATIQUE
!
    call oblgen(subccn, typcol, sdcolo)
    call obcrea('TABLEAU_COLONNE', sdcolo)
!
! --- LARGEUR DE LA COLONNE
!
    call obseti(sdcolo, 'LARGEUR', larcol)
!
! --- TITRE DE LA COLONNE
!
    call obseti(sdcolo, 'HAUTEUR_TITRE', tithau)
    call assert((tithau.gt.0).and.(tithau.le.3))
    call obsetk(sdcolo, 'TITRE_LIGN_1', titli1)
    if (tithau .ge. 2) call obsetk(sdcolo, 'TITRE_LIGN_2', titli2)
    if (tithau .eq. 3) call obsetk(sdcolo, 'TITRE_LIGN_3', titli3)
!
! --- TYPE DE LA COLONNE
!
    call obsetk(sdcolo, 'TYPE_COLONNE', typcol)
!
! --- TYPE DE LA VALEUR DANS LA COLONNE
!
    linte = .false.
    lreel = .false.
    lchai = .false.
    if (typval .eq. 'I') then
        linte = .true.
    else if (typval.eq.'R') then
        lreel = .true.
    else if (typval.eq.'K') then
        lchai = .true.
    else
        call assert(.false.)
    endif
    call obsetb(sdcolo, 'ENTIER', linte)
    call obsetb(sdcolo, 'REEL', lreel)
    call obsetb(sdcolo, 'CHAINE', lchai)
    call obsetb(sdcolo, 'VALE_AFFE', .false.)
    lnvvid = .false.
    lnverr = .false.
    lnvsan = .false.
!
! --- COMPORTEMENT SI VALEUR NON AFFECTE
!
    if (cnoval .eq. 'VIDE') then
        lnvvid = .true.
    else if (cnoval.eq.'ERRE') then
        lnverr = .true.
    else if (cnoval.eq.'SANS') then
        lnvsan = .true.
    else
        call assert(.false.)
    endif
    call obsetb(sdcolo, 'NON_AFFE_ERREUR', lnverr)
    call obsetb(sdcolo, 'NON_AFFE_VIDE', lnvvid)
    call obsetb(sdcolo, 'NON_AFFE_SANSOBJ', lnvsan)
!
    call jedema()
end subroutine
