subroutine jedupc(clain, schin, ipos, claout, schout,&
                  dupcol)
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
    implicit none
    include 'jeveux_private.h'
    include 'asterfort/jedupo.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: clain, schin, claout, schout
    integer :: ipos
    logical :: dupcol
! ----------------------------------------------------------------------
!     RECOPIE LES OBJETS DE LA CLASSE CLAIN POSSEDANT LA SOUS-CHAINE
!     SCHIN EN POSITION IPOS DANS LA CLASSE CLAOUT AVEC LA SOUS-CHAINE
!     DISTINCTE SCHOUT
!
! IN  CLAIN  : NOM DE LA CLASSE EN ENTREE (' ' POUR TOUTES LES BASES)
! IN  SCHIN  : SOUS-CHAINE EN ENTREE
! IN  IPOS   : POSITION DE LA SOUS-CHAINE
! IN  CLAOUT : NOM DE LA CLASSE EN SORTIE
! IN  SCHOUT : SOUS-CHAINE EN SORTIE
! IN  DUPCOL : .TRUE. DUPLIQUE LES OBJETS PARTAGEABLES D'UNE COLLECTION
!              .FALSE. S'ARRETE SUR ERREUR
!
! ----------------------------------------------------------------------
    character(len=6) :: pgma
    common /kappje/  pgma
!-----------------------------------------------------------------------
    integer :: jdocu, jgenr, jorig, jrnom, jtype, n, ncla1
    integer :: ncla2
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
    integer :: nrhcod, nremax, nreuti
    common /icodje/  nrhcod(n) , nremax(n) , nreuti(n)
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
    integer :: l1, l2, j, icin
    character(len=32) :: nomin, nomout, schin2, schou2
    character(len=1) :: kclas
!
! DEB ------------------------------------------------------------------
    pgma = 'JEDUPC'
!
    l1 = len ( schin )
    if (ipos + l1 .gt. 25 .or. ipos .lt. 0 .or. l1 .eq. 0) then
        call u2mesk('F', 'JEVEUX_92', 1, schin)
    endif
    schin2=schin
    l2 = len ( schout)
    schou2=schout
!
    if (l1 .ne. l2) then
        call u2mesk('F', 'JEVEUX_93', 1, schou2//' '//schin2)
    endif
!
    if (ipos + l2 .gt. 25 .or. ipos .lt. 0 .or. l2 .eq. 0) then
        call u2mesk('F', 'JEVEUX_92', 1, schout)
    endif
    if (schin(1:l1) .eq. schout(1:l2)) then
        call u2mesk('F', 'JEVEUX_94', 1, schin2//' : '//schou2)
    endif
!
    kclas = clain (1:min(1,len(clain)))
    if (kclas .eq. ' ') then
        ncla1 = 1
        ncla2 = index ( classe , '$' ) - 1
        if (ncla2 .lt. 0) ncla2 = n
    else
        ncla1 = index ( classe , kclas)
        ncla2 = ncla1
    endif
    do 100 icin = ncla1, ncla2
        kclas = classe(icin:icin)
        do 150 j = 1, nremax(icin)
            nomin = rnom(jrnom(icin)+j)
            if (nomin(1:1) .eq. '?' .or. nomin(25:32) .ne. '        ') goto 150
            if (schin .eq. nomin(ipos:ipos+l1-1)) then
                nomout = nomin
                nomout = nomout(1:ipos-1)//schou2(1:l2)//nomout(ipos+ l1:32)
                call jedupo(nomin, claout, nomout, dupcol)
            endif
150      continue
100  end do
! FIN ------------------------------------------------------------------
!
!
end subroutine
