subroutine gcsele(motcle, chvolu, ch1d2d, ch2d3d, chpres,&
                  chepsi, chpesa, chrota, lvolu , l1d2d ,&
                  l2d3d , lpres , lepsi , lpesa , lrota ,&
                  lfvolu, lf1d2d, lf2d3d, lfpres, lfepsi,&
                  lfpesa, lfrota, carte0, lformu, lpchar,&
                  lccomb)
!
    implicit none
!
#include "asterfort/assert.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W1504
!
    character(len=16) :: motcle
    character(len=19) :: carte0
    logical :: lformu, lpchar, lccomb
    character(len=19) :: chvolu, ch1d2d, ch2d3d, chpres
    character(len=19) :: chepsi, chpesa, chrota
    logical :: lvolu, l1d2d, l2d3d, lpres
    logical :: lepsi, lpesa, lrota
    logical :: lfvolu, lf1d2d, lf2d3d, lfpres
    logical :: lfepsi, lfpesa, lfrota
!
! ----------------------------------------------------------------------
!
! ROUTINE CALC_G
!
! SELECTION DES VARIABLES CORRESPONDANT AU MOT-CLEF ACTIF
!
! ----------------------------------------------------------------------
!
!
! IN  MOTCLE : MOT-CLEF DE LA CHARGE (VOIR LISDEF)
! I/O lvolu  : .TRUE.  ON A UNE CHARGE FORCE_INTERNE
! I/O l1d2d  : .TRUE.  SI ON AU MOINS UNE CHARGE FORCE_CONTOUR
! I/O l2d3d  : .TRUE.  SI ON AU MOINS UNE CHARGE FORCE_FACE
! I/O lpres  : .TRUE.  SI ON AU MOINS UNE CHARGE PRES_REP
! I/O lepsi  : .TRUE.  SI ON AU MOINS UNE CHARGE EPSI_INIT
! I/O lpesa  : .TRUE.  SI ON AU MOINS UNE CHARGE PESANTEUR
! I/O lrota  : .TRUE.  SI ON AU MOINS UNE CHARGE ROTATION
! IN  lfvolu : .TRUE.  SI CHARGE FORCE_INTERNE DE TYPE 'FONCTION'
! IN  lf1d2d : .TRUE.  SI CHARGE FORCE_CONTOUR DE TYPE 'FONCTION'
! IN  lf2d3d : .TRUE.  SI CHARGE FORCE_FACE DE TYPE 'FONCTION'
! IN  lfpres : .TRUE.  SI CHARGE PRES_REP DE TYPE 'FONCTION'
! IN  lfepsi : .TRUE.  SI CHARGE EPSI_INIT DE TYPE 'FONCTION'
! IN  lfpesa : .TRUE.  SI CHARGE PESANTEUR DE TYPE 'FONCTION'
! IN  lfrota : .TRUE.  SI CHARGE ROTATION DE TYPE 'FONCTION'
! IN  CHVOLU : CARTE POUR FORCE_INTERNE
! IN  ch1d2d : CARTE POUR FORCE_CONTOUR
! IN  ch2d3d : CARTE POUR FORCE_FACE
! IN  chpres : CARTE POUR PRES_REP
! IN  chepsi : CARTE POUR EPSI_INIT
! IN  chpesa : CARTE POUR PESANTEUR
! IN  chrota : CARTE POUR ROTATION
! OUT LPCHAR : .TRUE.  SI C'EST LA PREMIERE FOIS QU'ON A UNE CHARGE DU STYLE COURANT
! OUT CARTE0 : CARTE DE LA CHARGE DU STYLE COURANT
! OUT LFORMU : .TRUE.  SI LE CHARGEMENT 'FONCTION' UTILISE UNE FORMULE
! OUT LCCOMB : .TRUE. SI LE CHARGEMENT EST COMBINABLE
!
! ----------------------------------------------------------------------
!
    lpchar = .false.
    if (motcle .eq. 'FORCE_INTERNE#2D' .or. motcle .eq. 'FORCE_INTERNE#3D') then
        carte0 = chvolu
        if (.not.lvolu) lpchar = .true.
        lvolu  = .true.
        lformu = lfvolu
        lccomb = .true.
    else if (motcle.eq.'FORCE_CONTOUR') then
        carte0 = ch1d2d
        if (.not.l1d2d) lpchar = .true.
        l1d2d  = .true.
        lformu = lf1d2d
        lccomb = .true.
    else if (motcle.eq.'FORCE_FACE') then
        carte0 = ch2d3d
        if (.not.l2d3d) lpchar = .true.
        l2d3d  = .true.
        lformu = lf2d3d
        lccomb = .true.
    else if (motcle.eq.'PRES_REP') then
        carte0 = chpres
        if (.not.lpres) lpchar = .true.
        lpres  = .true.
        lformu = lfpres
        lccomb = .true.
    else if (motcle.eq.'EPSI_INIT') then
        carte0 = chepsi
        if (.not.lepsi) lpchar = .true.
        lepsi  = .true.
        lformu = lfepsi
        lccomb = .false.
    else if (motcle.eq.'PESANTEUR') then
        carte0 = chpesa
        if (.not.lpesa) lpchar = .true.
        lpesa  = .true.
        lformu = lfpesa
        lccomb = .false.
    else if (motcle.eq.'ROTATION') then
        carte0 = chrota
        if (.not.lrota) lpchar = .true.
        lrota  = .true.
        lformu = lfrota
        lccomb = .false.
    else
        write(6,*) 'MOT-CLEF:',motcle
        ASSERT(.false.)
    endif
!
end subroutine
