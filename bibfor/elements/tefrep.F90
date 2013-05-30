subroutine tefrep(option, nomte, param, iforc)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/tecach.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    character(len=16) :: option, nomte
    character(len=*) :: param
!
! ......................................................................
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!    - BUT:
!     RECUPERER L'ADRESSE DU CHAMP LOCAL CORRESPONDANT AUX EFFORTS
!     REPARTIS (CHAR_MECA_FR.D.D)
! ......................................................................
!
    integer :: iforc, itab(8), k, iret, ino, ico
    integer :: iadzi, iazk24, nbval, jad, nbno, nbcmp
    character(len=24) :: valk(4)
    character(len=8) :: nommai
!     ------------------------------------------------------------------
!
    call tecach('OON', param, 'L', 8, itab,&
                iret)
    call assert(iret.eq.0.or.iret.eq.3)
!
    if (iret .eq. 0) then
        iforc=itab(1)
!
    else if (iret.eq.3) then
        iforc=itab(1)
!       -- APRES CONCERTATION (JP+JLF) ON DECIDE :
!       1) SI UN NOEUD NE PORTE PAS TOUTES LES CMPS => <F>
!       2) SI UN NOEUD NE PORTE AUCUNE COMPOSANTE, ON LE MET
!          A ZERO.
        nbval=itab(2)
        nbno=itab(3)
        jad=itab(8)
        nbcmp=nbval/nbno
        call assert(nbval.eq.nbno*nbcmp)
!
        do 3,ino=1,nbno
        ico=0
        do 1, k=1,nbcmp
        if (zl(jad-1+(ino-1)*nbcmp+k)) ico=ico+1
 1      continue
        if (ico .ne. 0 .and. ico .ne. nbcmp) goto 12
        call assert(iforc.ne.0)
        if (ico .eq. 0) then
            do 2, k=1,nbcmp
            zr(iforc-1+(ino-1)*nbcmp+k)=0.d0
 2          continue
        endif
 3      continue
        goto 9999
!
!
12      continue
        call tecael(iadzi, iazk24)
        nommai=zk24(iazk24-1+3)
        valk(1) = param
        valk(2) = option
        valk(3) = nomte
        valk(4) = nommai
        call u2mesk('F', 'CALCULEL2_72', 4, valk)
    endif
!
9999  continue
end subroutine
