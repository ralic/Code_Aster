subroutine oriem0(kdim, type, coor, lino1, nbno1,&
                  lino2, nbno2, lino3, nbno3, ier,&
                  imai)
    implicit   none
! person_in_charge: jacques.pellet at edf.fr
    include 'asterfort/assert.h'
    include 'asterfort/indiis.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/provec.h'
    include 'blas/ddot.h'
    integer :: lino1(*), nbno1, lino2(*), nbno2, lino3(*), nbno3
    integer :: ier, imai
    real(kind=8) :: coor(*)
    character(len=2) :: kdim
    character(len=8) :: type
!.======================================================================
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
! BUT :  DETERMINER LA POSITION RELATIVE DE 2 MAILLES "VOLUMIQUES"
!        (LINO1 ET LINO2) PAR RAPPORT A UNE MAILLE DE "PEAU" (LINO3)
!        ON SUPPOSE QUE LES NOEUDS DE LA MAILLE DE PEAU APPARTIENNENT
!        AUX 2 MAILLES VOLUMIQUES.
! ======================================================================
! IN  : KDIM   : '3D' / '2D'
! IN  : TYPE   : TYPE DE LA MAILLE DE PEAU (TRIA3, QUAD4, SEG2, ...)
! IN  : LINO1  : LISTE DES NOEUDS DE LA MAILLE 1 (3D OU 2D)
! IN  : NBNO1  : NB DE NOEUDS DE LINO1
! IN  : LINO2  : LISTE DES NOEUDS DE LA MAILLE 2  (3D OU 2D)
! IN  : NBNO2  : NB DE NOEUDS DE LINO2
! IN  : LINO3  : LISTE DES NOEUDS DE LA MAILLE DE PEAU (2.5D OU 1.5D)
! IN  : NBNO3  : NB DE NOEUDS DE DE LA MAILLE DE PEAU
! OUT : IER    : = 0  MAILLE 1 ET MAILLES 2 DU MEME COTE (PAR RAPPORT
!                     A LA MAILLE DE PEAU)
!                = 1  SINON
! OUT : IMAI   : = /0 /1 /2
!          SI IER =0 :  IMAI = 0
!          SINON:  IMAI = 1 OU 2
!              IMAI EST LE NUMERO DE LA MAILLE QUI A LA MEME NORMALE
!              SORTANTE QUE LA MAILLE DE PEAU.
!.========================= DEBUT DES DECLARATIONS ====================
!
    integer :: ino, n1, n2, n3, ic, indi
    real(kind=8) :: nor1(3), n1n2(3), n1n3(3), ps1, ps2
!
! ========================= DEBUT DU CODE EXECUTABLE ==================
!
    call jemarq()
!
    if (kdim .eq. '3D') then
        call assert(type(1:4).eq.'TRIA' .or. type(1:4).eq.'QUAD')
    else if (kdim.eq.'2D') then
        call assert(type(1:3).eq.'SEG')
    else
        call assert(.false.)
    endif
!
! --- INITIALISATIONS :
!     ---------------
!
!
! --- VERIFICATION DE LA POSITION DES MAILLES 1 ET 2
!     PAR RAPPORT A LA MAILLE DE PEAU :
!     -----------------------------------------------
!
!     DEFINITION DE LA NORMALE DE LA MAILLE DE PEAU: NOR1
    n1 = lino3(1)
    n2 = lino3(2)
    if (type(1:3) .ne. 'SEG') then
        n3 = lino3(3)
        do 19 ic = 1, 3
            n1n2(ic)=coor(3*(n2-1)+ic)-coor(3*(n1-1)+ic)
            n1n3(ic)=coor(3*(n3-1)+ic)-coor(3*(n1-1)+ic)
19      continue
        call provec(n1n2, n1n3, nor1)
    else
        do 21 ic = 1, 3
            n1n2(ic)=coor(3*(n2-1)+ic)-coor(3*(n1-1)+ic)
21      continue
        nor1(1)=-n1n2(2)
        nor1(2)=n1n2(1)
        nor1(3)=n1n2(3)
    endif
!
!
!
!     POSITION DE LA MAILLE 1 PAR RAPPORT A LA MAILLE DE PEAU
    do 30 ino = 1, nbno1
        indi=indiis(lino3(1),lino1(ino),1,nbno3)
!        -- ON CHERCHE UN NOEUD DE LINO1 (N2) QUI NE SOIT PAS UN NOEUD
!           DE LA PEAU :
        if (indi .eq. 0) then
            n2=lino1(ino)
            n1=lino3(1)
            do 20 ic = 1, 3
                n1n2(ic)=coor(3*(n2-1)+ic)-coor(3*(n1-1)+ic)
20          continue
!          -- PS1 > 0 <=> LA NORMALE DE LA PEAU EST ORIENTEE COMME LA
!                         LA NORMALE EXTERIEURE DE LA MAILLE 1
            ps1=ddot(3,n1n2,1,nor1,1)
            goto 40
!
        endif
30  end do
    call assert(.false.)
40  continue
!
!
!     POSITION DE LA MAILLE 2 PAR RAPPORT A LA MAILLE DE PEAU
    do 60 ino = 1, nbno2
        indi=indiis(lino3(1),lino2(ino),1,nbno2)
        if (indi .eq. 0) then
            n2=lino2(ino)
            n1=lino3(1)
            do 50 ic = 1, 3
                n1n2(ic)=coor(3*(n2-1)+ic)-coor(3*(n1-1)+ic)
50          continue
            ps2=ddot(3,n1n2,1,nor1,1)
            goto 70
!
        endif
60  end do
    call assert(.false.)
70  continue
!
!
!     LES MAILLES 1 ET 2 SONT ELLES DU MEME COTE PAR RAPPORT
!     A LA MAILLE DE PEAU ?
!     -------------------------------------------------------
    call assert(ps1.ne.0.d0 .and. ps2.ne.0.d0)
    ier=1
    if ((ps1.gt.0.and.ps2.gt.0) .or. (ps1.lt.0.and.ps2.lt.0)) ier=0
!
    imai=0
    if (ier .ne. 0) then
        if (ps1 .lt. 0) then
            imai=1
        else
            imai=2
        endif
    endif
!
    call jedema()
!
end subroutine
