subroutine ini002(nomte, nmax, itabl, k24tab, nval)
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: jacques.pellet at edf.fr
!-----------------------------------------------------------------------
!
! BUT :  ROUTINE D'INITIALISATION DES ELEMENTS AYANT DES ELREFE
!
! IN  NOMTE : NOM DU TYPE ELEMENT
! IN  NMAX  : DIMENSION DES TABLEAUX ITABL ET K24TAB
! OUT NVAL  : NVAL EST LE NOMBRE D'OBJETS CREES PAR INI0IJ
!              (SI NVAL > NMAX : NVAL = -NVAL ET ITABL EST VIDE)
! OUT K24TAB: CE TABLEAU CONTIENT LES NOMS DES OBJETS
!             QUE L'ON PEUT RECUPERER DANS LES TE000I
!             VIA LA ROUTINE JEVETE.
! OUT ITABL : CE TABLEAU CONTIENT LES ADRESSES
!             DES OBJETS DE K24TAB DANS ZR, ZI, ...
!   -------------------------------------------------------------------
!-----------------------------------------------------------------------
    implicit none
!
!
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/elref2.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jni001.h'
    include 'asterfort/jni002.h'
    include 'asterfort/jni015.h'
    include 'asterfort/jni080.h'
    include 'asterfort/jni091.h'
    include 'asterfort/jni092.h'
    include 'asterfort/nuelrf.h'
    character(len=16) :: nomte
    character(len=8) :: elrefe, lirefe(10)
    integer :: nmax, nval, itabl(nmax), nujni
    character(len=24) :: k24tab(nmax)
    character(len=24) :: liobj(10)
    integer :: nbelr, ii, kk, iret, nbobj, k
! DEB ------------------------------------------------------------------
!
! --- RECUPERATION DE LA LISTE DES ELREFE CORRESPONDANTS AU NOMTE
    call elref2(nomte, 10, lirefe, nbelr)
    call assert(nbelr.gt.0)
!
!
!     --BOUCLE SUR LES ELREFE :
!     -------------------------
    nval = 0
    do 20 ii = 1, nbelr
        elrefe = lirefe(ii)
        call nuelrf(elrefe, nujni)
!
!
!       -- CAS DES ELREFA :
!       -------------------
        if (nujni .eq. 2) then
            call jni002(elrefe, 10, liobj, nbobj)
!
        else if (nujni.eq.1) then
            call jni001(elrefe, 10, liobj, nbobj)
!
        else if (nujni.eq.15) then
            call jni015(elrefe, 10, liobj, nbobj)
!
        else if (nujni.eq.80) then
            call jni080(elrefe, 10, liobj, nbobj)
!
        else if (nujni.eq.91) then
            call jni091(elrefe, 10, liobj, nbobj)
!
        else if (nujni.eq.92) then
            call jni092(elrefe, 10, liobj, nbobj)
!
        else
            call assert(.false.)
        endif
!
        nval = nval + nbobj
        call assert(nval.le.nmax)
        do 10,k = 1,nbobj
        k24tab(nval-nbobj+k) = liobj(k)
10      continue
20  end do
!
!
!     RECUPERATION DES ADRESSES DES OBJETS CREES :
!     ---------------------------------------------
    do 30,kk = 1,nval
    call jeexin(k24tab(kk), iret)
    if (iret .gt. 0) call jeveuo(k24tab(kk), 'L', itabl(kk))
    30 end do
!
end subroutine
