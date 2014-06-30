subroutine jelstc(clas, souch, ipos, maxval, klst,&
                  nbval)
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
! person_in_charge: j-pierre.lefebvre at edf.fr
    implicit none
#include "jeveux_private.h"
#include "asterfort/utmess.h"
    character(len=*) :: clas, souch, klst(*)
    integer :: ipos, maxval, nbval
! ----------------------------------------------------------------------
!  BUT : RETROUVER LES NOMS DES OBJETS DONT LE NOM CONTIENT UNE CHAINE
!        DE CARATERES DONNEE, PRESENTS SUR UNE BASE JEVEUX.
!
!  IN  : CLAS : NOM DE LA BASE : 'G', 'V', ..( ' ' -> TOUTES LES BASES )
!  IN  : SOUCH: CHAINE DE CARACTERES A CHERCHER
!  IN  : IPOS : POSITION DU DEBUT DE LA CHAINE
!               SI IPOS=0 ON REND TOUS LES NOMS
!  IN  : MAXVAL: DIMENSION DU TABLEAU KLST
!  OUT : KLST  : TABLEAU DE K24 CONTENANT LES NOMS TROUVES
!  OUT : NBVAL : NOMBRE DE NOMS TROUVES (NBVAL = -NBVAL SI < MAXVAL)
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: jdocu, jgenr, jorig, jrnom, jtype, l, n
    integer :: nbl
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
    logical(kind=1) :: trouve
    character(len=6) :: pgma
    common /kappje/  pgma
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
    integer :: nrhcod, nremax, nreuti
    common /icodje/  nrhcod(n) , nremax(n) , nreuti(n)
!     ==================================================================
    integer :: ncla1, ncla2, ic, j
    character(len=32) :: crnom, k32val
    character(len=1) :: kclas
!     ==================================================================
    pgma = 'JELSTC'
    l = len ( souch )
    if (ipos + l .gt. 25 .or. ipos .lt. 0 .or. l .eq. 0) then
        k32val=souch
        call utmess('F', 'JEVEUX1_11', sk=k32val)
    endif
    kclas = clas (1:min(1,len(clas)))
    if (kclas .eq. ' ') then
        ncla1 = 1
        ncla2 = index ( classe , '$' ) - 1
        if (ncla2 .lt. 0) ncla2 = n
    else
        ncla1 = index ( classe , kclas)
        ncla2 = ncla1
    endif
    nbl = 0
    do 100 ic = ncla1, ncla2
        do 150 j = 1, nremax(ic)
            crnom = rnom(jrnom(ic)+j)
            if (crnom(1:1) .eq. '?' .or. crnom(25:32) .ne. '        ') goto 150
            if (ipos .eq. 0) then
                trouve=.true.
            else if (souch .eq. crnom(ipos:ipos+l-1)) then
                trouve=.true.
            else
                trouve=.false.
            endif
            if (trouve) then
                nbl = nbl + 1
                if (nbl .le. maxval) then
                    klst(nbl) = crnom(1:24)
                endif
            endif
150      continue
100  end do
    if (nbl .gt. maxval) then
        nbval = -nbl
    else
        nbval = nbl
    endif
!
end subroutine
